{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

module JavaScript.WebSockets.Internal (
    -- * Types
    Connection(..)
  , ConnectionProcess(..)
  , TypeQueue
  , Socket
  , Waiter
  , ConnectionQueue
  , ConnectionWaiters
    -- * FFI
  , ws_newSocket
  , ws_socketSend
  , ws_awaitConn
  , ws_closeSocket
    -- * Utility
  , withConn
  , openConnection
  , openTaggedConnection
  , closeConnection
  , selfConn
  , connOrigin
  , connTagged
  , popQueueFp
  , queueUpFp
  , awaitMessage
  ) where

import Control.Applicative       (Applicative, (<*>), pure, (<$>))
import Control.Monad             (ap)
import Control.Concurrent.MVar   (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.Binary.Tagged
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.Map.Strict           (Map)
import Data.Sequence             as S
import Data.Text                 (Text)
import Data.Text.Encoding        (encodeUtf8, decodeUtf8)
import GHCJS.Foreign             (fromJSString, toJSString, newArray)
import GHCJS.Types               (JSRef, JSArray, JSString)
import JavaScript.Blob           (isBlob, readBlob)
import Unsafe.Coerce             (unsafeCoerce)
import qualified Data.Map.Strict as M

type TypeQueue = Map TagFingerprint (Seq ByteString)

-- | Encapsulates a (reference to a) Websocket connection.  Ideally, you
-- should not have to deal with this yourself and should only be
-- interfacing with sockets within the 'ConnectionProcess' monad and
-- 'withUrl'.  However, if you want to (for example, if you want to
-- easily interface with multiple sockets at a time), you can get one with
-- 'openConnection' and run commands on it with 'withConn'.
--
-- Care must be taken to close the connection once you are done, or else
-- incoming messages will continue to queue.
data Connection = Connection { _connSocket     :: Socket
                             , _connQueue      :: ConnectionQueue
                             , _connWaiters    :: ConnectionWaiters
                             , _connTypeQueues :: MVar TypeQueue
                             , _connOrigin     :: Text
                             , _connTagged     :: Bool
                             }

-- | Represents a process that can be executed with a 'Connection'.
-- 'ConnectionProcess' is a 'Monad', so you can chain together complex
-- processes with do notation and other monadic operations.  However, you
-- can also use these as one-shot commands inside 'IO' as well.
--
-- 'ConnectionProcess' is also a 'MonadIO', so you can execute arbitrary
-- 'IO a' commands using 'liftIO'.
data ConnectionProcess a = ProcessExpect (ByteString -> ConnectionProcess a)
                         | ProcessSend ByteString (ConnectionProcess a)
                         | ProcessRead (Connection -> ConnectionProcess a)
                         | ProcessIO (IO (ConnectionProcess a))
                         | ProcessPure a

instance Monad ConnectionProcess where
    return                    = ProcessPure
    (ProcessExpect e)   >>= f = ProcessExpect $ \t -> e t >>= f
    (ProcessSend s p)   >>= f = ProcessSend s $ p >>= f
    (ProcessRead p)     >>= f = ProcessRead   $ \c -> p c >>= f
    (ProcessIO io)      >>= f = ProcessIO     $ fmap (>>= f) io
    (ProcessPure a)     >>= f = f a

instance Applicative ConnectionProcess where
    pure = return
    (<*>) = ap

instance Functor ConnectionProcess where
    fmap f s = pure f <*> s

instance MonadIO ConnectionProcess where
    liftIO io = ProcessIO (return <$> io)

data Socket_
type Socket = JSRef Socket_

data Waiter_
type Waiter = JSRef Waiter_

type ConnectionQueue = JSArray Text
type ConnectionWaiters = JSArray Waiter

foreign import javascript unsafe "$1.close();" ws_closeSocket :: Socket -> IO ()
foreign import javascript unsafe "$1.send($2)" ws_socketSend :: Socket -> JSString -> IO ()

foreign import javascript interruptible  "var ws = new WebSocket($1);\
                                          ws.onmessage = function(e) {\
                                            if (!(typeof e === 'undefined')) {\
                                              $2.push(e.data);\
                                              if ($3.length > 0) {\
                                                var w0 = $3.shift();\
                                                var e0 = $2.shift();\
                                                w0(e0);\
                                              }\
                                            }\
                                          };\
                                          ws.onopen = function() {\
                                            $c(ws);\
                                          };"
  ws_newSocket :: JSString -> ConnectionQueue -> ConnectionWaiters -> IO Socket

foreign import javascript interruptible  "if ($1.length > 0) {\
                                            var d = $1.shift();\
                                            $c(d);\
                                          } else {\
                                            $2.push(function(d) {\
                                              $c(d);\
                                            });\
                                          }"
  ws_awaitConn :: ConnectionQueue -> ConnectionWaiters -> IO (JSRef ())

-- Low-level API

-- | Execute/run a 'ConnectionProcess' process/computation inside an 'IO'
-- monad with a given 'Connection'.
--
-- Remember that ideally you would not have to use this directly, and
-- instead use 'withUrl' to handle opening and closing the connection for
-- you.  However, this is here for you to be able to deal with explicit
-- connection managing and swapping.
withConn :: Connection -> ConnectionProcess a -> IO a
withConn conn (ProcessExpect p)  = do
    msg <- awaitMessage conn
    withConn conn (p msg)
withConn conn (ProcessSend s p)  = do
    ws_socketSend (_connSocket conn) (toJSString . decodeUtf8 . toStrict $ s)
    withConn conn p
withConn conn (ProcessRead p)    = withConn conn (p conn)
withConn conn (ProcessIO io)     = io >>= withConn conn
withConn _    (ProcessPure x)    = return x

-- | Opens a connection to the websocket server at the given URL.  Returns
-- a 'Connection' object.
--
-- This opens a /non-tagged/ communcation channel.  All uses of 'expect' or
-- attempts to get non-tagged typed data from this channel will throw away
-- non-decodable data.  You can still use 'expectTagged' to get tagged
-- data, and it'll still be queued, but other 'expect' functions won't
-- queue anything.
--
-- If you don't ever expect to receive 'Tagged' data, this is for you.
--
-- Unless you want to use multiple sockets simultaneously, consider using
-- 'withUrl', which handles opening and closing connections for you.
openConnection :: Text -> IO Connection
openConnection = openConnection_ False

-- | Opens a connection to the websocket server at the given URL.  Returns
-- a 'Connection' object.
--
-- This opens a /tagged/ communication channel.  All attempts to get typed
-- data will pass over data of the wrong type and queue it for later
-- access with 'expectTagged'.
--
-- If you expect to use 'Tagged' data, even mixed with untagged data, this
-- is for you.
--
-- Unless you want to use multiple sockets simultaneously, consider using
-- 'withUrlTagged', which handles opening and closing connections for you.
openTaggedConnection :: Text -> IO Connection
openTaggedConnection = openConnection_ True

openConnection_ :: Bool -> Text -> IO Connection
openConnection_ t url = do
  queue <- newArray
  waiters <- newArray
  socket <- ws_newSocket (toJSString url) queue waiters
  tqs <- newMVar M.empty
  return $ Connection socket queue waiters tqs url t

-- | Closes the given 'Connection'.  Closed connections really aren't
-- distinguishable from open connections at this point, so be aware that
-- doing anything with a closed connection fails silently.
--
-- Unless you want to use multiple sockets simultaneously, consider using
-- 'withUrl', which handles opening and closing connections for you.
closeConnection :: Connection -> IO ()
closeConnection (Connection s _ _ _ _ _) = ws_closeSocket s

-- | Returns the 'Connection' that the 'ConnectionProcess' is being run
-- with.
selfConn :: ConnectionProcess Connection
selfConn = ProcessRead return

-- | Returns the origin (url) of the given 'Connection'.
connOrigin :: Connection -> Text
connOrigin = _connOrigin

-- | Returns if the given 'Connection' is a tagged channel.
connTagged :: Connection -> Bool
connTagged = _connTagged

-- Internal stuff

popQueueFp :: TagFingerprint -> ConnectionProcess (Maybe ByteString)
popQueueFp fp = do
  tqsref <- _connTypeQueues <$> selfConn
  tq <- M.lookup fp <$> liftIO (readMVar tqsref)
  case tq of
    Nothing  -> return Nothing
    Just tqseq -> do
      case viewl tqseq of
        EmptyL -> return Nothing
        a :< rest -> do
          liftIO $ modifyMVar_ tqsref (return . M.insert fp rest)
          return (Just a)

queueUpFp :: TagFingerprint -> ByteString -> ConnectionProcess ()
queueUpFp fp bs = do
    tqsref <- _connTypeQueues <$> selfConn
    liftIO $ modifyMVar_ tqsref (return . M.insertWith f fp (S.singleton bs))
  where
    f = flip (><)

awaitMessage :: Connection -> IO ByteString
awaitMessage (Connection _ q w _ _ _) = do
    msg <- ws_awaitConn q w
    blb <- isBlob msg
    if blb
      then do
        let blob = unsafeCoerce msg
        fromStrict <$> readBlob blob
      else do
        let blob = unsafeCoerce msg :: JSString
        return (fromStrict . encodeUtf8 . fromJSString $ blob)
