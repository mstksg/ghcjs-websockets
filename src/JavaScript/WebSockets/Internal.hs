{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module JavaScript.WebSockets.Internal (
  -- * Types
  -- ** Data types
    Connection(..)
  , SocketMsg(..)
  , ConnClosing(..)
  -- ** Typeclasses
  , WSSendable(..)
  , WSReceivable(..)
  -- ** Exceptions
  , ConnectionException(..)
  -- * Manipulating and inspecting 'Connection's
  , openConnection
  , closeConnection
  , closeConnectionLeftovers
  , clearConnectionQueue
  , dumpConnectionQueue
  , connectionClosed
  , connectionCloseReason
  , connectionOrigin
  -- * Sending data
  -- ** With feedback
  , sendMessage
  , sendMessage_
  , send
  , send_
  -- * Receiving data
  , receiveMessage
  , receiveMessageMaybe
  , receiveEither
  , receiveEitherMaybe
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad             (void, join, when)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe                (isJust, catMaybes)
import Data.Text as T            (Text, unpack, append)
import Data.Text.Encoding        (decodeUtf8', encodeUtf8, decodeUtf8)
import Data.Traversable          (mapM)
import Data.Typeable             (Typeable)
import GHCJS.Foreign
import GHCJS.Marshal             (fromJSRef)
import GHCJS.Types               (JSRef, JSString, isNull)
import JavaScript.Blob           (Blob, isBlob, readBlob)
import JavaScript.WebSockets.FFI
import Prelude hiding            (mapM)
import Unsafe.Coerce             (unsafeCoerce)

import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Base64.Lazy as B64L

-- | Encapsulates a (reference to a) Javascript Websocket connection.  Can
-- be created/accessed with either 'openConnection' or (preferably)
-- 'withUrl'.
--
-- Care must be taken to close the connection once you are done if using
-- 'openConnection', or unprocessed messages and callbacks will continue to
-- queue up.
--
data Connection = Connection { -- | JSRef to JS Websocket object
                               _connSocket     :: Socket
                               -- | JSRef to JSArray of queued incoming
                               -- messages, managed directly in FFI
                             , _connQueue      :: ConnectionQueue
                               -- | JSRef to JSArray of queue of waiting
                               -- receivers, managed directly in FFI
                             , _connWaiters    :: ConnectionWaiters
                               -- | Text of server socket was originally
                               -- opened with
                             , _connOrigin     :: Text
                               -- | IORef with Nothing if the connection is
                               -- still open and @Just reason@ if it's
                               -- closed, with the reason
                             , _connClosed     :: IORef (Maybe ConnClosing)
                               -- | Mutex for thread-safe manipulation
                             , _connBlock      :: MVar ()
                             }

-- | Sum type over the data that can be sent or received through
-- a JavaScript websocket.
--
-- What an incoming message is classified as depends on the Javascript
-- Websockets API <http://www.w3.org/TR/websockets/>, which provides
-- a "typed" input channel of either text or binary blob.
--
-- There are several convenience functions to help you never have to deal
-- with this explicitly; its main purpose is if you want to explicitly
-- branch on a 'receiveMessage' depending on what kind of message you
-- receive and do separate things. 'receiveText' and 'receiveData' will
-- both allow you to "filter" incoming messages by their type.
data SocketMsg = SocketMsgText Text
               | SocketMsgData ByteString
               deriving (Show, Eq, Typeable)

-- | Data type containing information on 'Connection' closes.
--
-- *  'ManualClose': Closed by the Haskell 'JavaScript.WebSockets'
--    interface, using 'closeConnection' or variants.
--
-- *  'JSClose': Closed on the Javascript end, either by a connection error
--    or server request, or what have you.  Contains information from the
--    Javascript Websockets API
--    <http://www.w3.org/TR/websockets/#event-definitions>.
--
--    The first field is whether or not it was a clean close; the second
--    field is the closing reason code; the third field is a 'Text' with
--    the reason given by the Websockets API.
data ConnClosing = ManualClose
                 | JSClose (Maybe Bool) (Maybe Int) (Maybe Text)
                 | OpenInterruptedClose
                 | UnexpectedClose Text
                 deriving (Show, Eq)

-- | An exception that may be thrown when using the various 'Connection'
-- operations.  Right now, only includes 'ConnectionClosed', which is
-- thrown when using an "unsafe" @receive@ on a closed 'Connection', or if
-- a 'Connection' closes while an unsafe @receive@ is waiting.
data ConnectionException = ConnectionClosed { _socketOrigin :: Text }
                         deriving (Eq, Typeable)

instance Show ConnectionException where
    show (ConnectionClosed o) = T.unpack $ T.append "Error: Waiting on closed connection " o
instance Exception ConnectionException

-- | A typeclass offering gratuitous and unnecessary abstraction over what
-- can be sent through a 'Connection'.  Allows you to wrap things in
-- a 'SocketMsg' automatically.  The only instances that should really ever
-- exist are 'Text' and instances of 'Binary'.
class WSSendable s where
    wrapSendable :: s -> SocketMsg

-- | A typeclass offering gratuitous and unnecessary abstraction over what
-- can be received through a 'Connection'.  Allows you to unwrap things in
-- a 'SocketMsg' automatically.  The only instances that should really ever
-- exist are 'Text' and instances of 'Binary'.
class WSReceivable s where
    unwrapReceivable :: SocketMsg -> Either SocketMsg s

instance WSSendable Text where
    wrapSendable = SocketMsgText

instance Binary a => WSSendable a where
    wrapSendable = SocketMsgData . encode

instance WSReceivable Text where
    unwrapReceivable (SocketMsgText t) = Right t
    unwrapReceivable i@(SocketMsgData d) = f . decodeUtf8' . toStrict $ d
      where
        f (Right t) = Right t
        f _         = Left i

instance Binary a => WSReceivable a where
    unwrapReceivable inc =
        case decodeOrFail bs of
          Right (_,_,x) -> Right x
          Left   _      -> Left inc
      where
        bs = case inc of
               SocketMsgText t -> fromStrict (encodeUtf8 t)
               SocketMsgData d -> d

-- | Opens a websocket connection to the given url, and returns the
-- 'Connection' after the handshake is complete.  Care should be taken to
-- ensure that the 'Connection' is later closed with 'closeConnection'.
--
-- Consider using 'withUrl', which handles closing with bracketing and
-- error handling so you don't have to worry about closing the connection
-- yourself.
--
-- Blocks until the connection has been established and opened.
--
-- If an async exception happens while this is waiting, the socket will be
-- closed as the exception bubbles up.
openConnection :: Text -> IO Connection
openConnection url = readMVar =<< openConnectionImmediate url

openConnectionImmediate :: Text -> IO (MVar Connection)
openConnectionImmediate url = do
    queue   <- newArray
    waiters <- newArray
    socket  <- ws_newSocket (toJSString url) queue waiters
    closed  <- newIORef Nothing
    block   <- newMVar ()
    let conn = Connection socket queue waiters url closed block
    outp    <- newEmptyMVar
    _       <- forkIO $ handleClose conn
    _       <- forkIO $ handleOpen conn outp
    return outp

_dudConnection :: Text -> ConnClosing -> IO Connection
_dudConnection url closing = Connection jsNull
                         <$> newArray
                         <*> newArray
                         <*> pure url
                         <*> newIORef (Just closing)
                         <*> newMVar ()

handleOpen :: Connection -> MVar Connection -> IO ()
handleOpen conn connMVar =
    bracketOnError (return ())
                   (\_ -> _closeConnection OpenInterruptedClose False conn)
                  $ \_ -> do
                      _ <- ws_handleOpen (_connSocket conn)
                      putMVar connMVar conn

handleClose :: Connection -> IO ()
handleClose conn = do
    connState <- connectionState conn
    when (connState < 3) . handle handler $ do
      closeEvent <- ws_handleClose (_connSocket conn)
      wasClean   <- fmap fromJSBool <$> getPropMaybe ("wasClean" :: Text) closeEvent
      code       <- fmap join . mapM fromJSRef =<< getPropMaybe ("code" :: Text) closeEvent
      reason     <- fmap fromJSString <$> getPropMaybe ("reason" :: Text) closeEvent
      let jsClose = JSClose wasClean code reason
      _ <- _closeConnection jsClose False conn
      return ()
  where
    -- TODO: any way to reasonably restore the handler?
    handler :: SomeAsyncException -> IO ()
    handler _ = void $ _closeConnection (UnexpectedClose reason) False conn
      where
        reason = "Close handler interrupted with Asynchronous Exception."

-- | Manually closes the given 'Connection'.  It un-blocks all threads
-- currently waiting on the connection and disables all sending and
-- receiving in the future.
--
-- The result is a list of all messages received by the connection but not
-- yet retrieved by 'receive', etc. on the Haskell end.
--
-- If you want to throw away all of the extra messages, use
-- 'closeConnection_'.
--
closeConnectionLeftovers :: Connection -> IO [SocketMsg]
closeConnectionLeftovers = _closeConnection ManualClose True

-- | Manually closes the given 'Connection'.  Will un-block all threads
-- currently waiting on the 'Connection' for messages (releasing their
-- callbacks) and disable sending and receiving in the future.  Also clears
-- the message queue on that 'Connection' so no future retrievals will
-- return anything.
closeConnection :: Connection -> IO ()
closeConnection = void . _closeConnection ManualClose False

_closeConnection :: ConnClosing -> Bool -> Connection -> IO [SocketMsg]
_closeConnection cclsing dump conn = withConnBlockMasked conn $ do
    closed <- readIORef (_connClosed conn)
    connState <- connectionState conn
    if isJust closed || connState < 3
      then
        return []
      else do
        writeIORef (_connClosed conn) (Just cclsing)
        ws_closeSocket (_connSocket conn)
        ws_clearWaiters (_connWaiters conn)
        outp <- if dump
          then _dumpConnectionQueue conn
          else [] <$ ws_clearQueue (_connQueue conn)
        return outp

-- | Clears the message queue (messages waiting to be 'receive'd) on the
-- given 'Connection'.  Works on closed 'Connection's.
clearConnectionQueue :: Connection -> IO ()
clearConnectionQueue conn = withConnBlockMasked conn $
    ws_clearQueue (_connQueue conn)

dumpConnectionQueue :: Connection -> IO [SocketMsg]
dumpConnectionQueue conn = withConnBlockMasked conn $
    _dumpConnectionQueue conn

withConnBlock :: Connection -> IO a -> IO a
withConnBlock conn f = withMVar (_connBlock conn) (const f)

withConnBlockMasked :: Connection -> IO a -> IO a
withConnBlockMasked conn f = withMVarMasked (_connBlock conn) (const f)


_dumpConnectionQueue :: Connection -> IO [SocketMsg]
_dumpConnectionQueue conn = do
    msgsRefs <- fromArray (_connQueue conn)
    results  <- catMaybes <$> mapM _loadJSMessage msgsRefs
    ws_clearQueue (_connQueue conn)
    return results


-- | Check if the given 'Connection' is closed.  Returns a 'Bool'.  To
-- check *why* it was closed, see 'connectionCloseReason'.
connectionClosed :: Connection -> IO Bool
connectionClosed = fmap isJust . connectionCloseReason

-- | Returns @Nothing@ if the given 'Connection' is still open, or @Just
-- closing@ containing a 'ConnClosing' with information on why the
-- connection was closed.
--
-- For just a 'Bool' saying whether or not the connection is closed, try
-- 'connectionClosed'.
connectionCloseReason :: Connection -> IO (Maybe ConnClosing)
connectionCloseReason conn = withConnBlock conn $
    readIORef (_connClosed conn)

connectionState :: Connection -> IO Int
connectionState conn = withConnBlock conn $
    ws_readyState (_connSocket conn)

-- | Returns the origin url of the given 'Connection'.
connectionOrigin :: Connection -> Text
connectionOrigin = _connOrigin

-- | Sends the given 'SocketMsg' through the given 'Connection'.
-- A 'SocketMsg' is a sum type of either 'SocketMsgText t', containing
-- (strict) 'Text', or 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Returns 'True' if the connection is open, and 'False' if it
-- is closed.
sendMessage :: Connection -> SocketMsg -> IO Bool
sendMessage conn msg = do
  closed <- connectionClosed conn
  if closed
    then
      return False
    else do
      -- TODO: Validate send here
      ws_socketSend (_connSocket conn) (outgoingData msg)
      return True
  where
    outgoingData (SocketMsgText t) = toJSString . decodeUtf8 . B64.encode . encodeUtf8 $ t
    outgoingData (SocketMsgData d) = toJSString . decodeUtf8 . toStrict . B64L.encode $ d


-- | Sends the given 'SocketMsg' through the given 'Connection'.
-- A 'SocketMsg' is a sum type of either 'SocketMsgText t', containing
-- (strict) 'Text', or 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Fails silently if the connection is closed.  Use 'sendMessage' to get
-- feedback on the result of the send.
sendMessage_ :: Connection -> SocketMsg -> IO ()
sendMessage_ conn = void . sendMessage conn

-- | Send the given item through the given 'Connection'.
--
-- You can 'send' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'sendText' or 'sendData'.
--
-- Returns 'True' if the connection is open, and 'False' if it
-- is closed.
send :: WSSendable a => Connection -> a -> IO Bool
send conn = sendMessage conn . wrapSendable

-- | Send the given item through the given 'Connection'.
--
-- You can 'send_' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'sendText_' or 'sendData_'.
--
-- Fails silently if the connection is closed.  Use 'send' to get feedback
-- on the result of the send.
send_ :: WSSendable a => Connection -> a -> IO ()
send_ conn = void . send conn

-- | Block and wait until the 'Connection' receives any message, and
-- returns the message wrapped in a 'SocketMsg'.  A 'SocketMsg' is a sum
-- type of either 'SocketMsgText t', containing (strict) 'Text', or
-- 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Will return 'Just msg' as soon as any message is received, or 'Nothing'
-- if the 'Connection' closes first.  Returns 'Nothing' immediately if the
-- 'Connection' is already closed.
receiveMessageMaybe :: Connection -> IO (Maybe SocketMsg)
receiveMessageMaybe conn = do
  closed <- connectionClosed conn
  if closed
    then return Nothing
    else do
      waiterKilled <- newObj
              -- set to ignore waiter if thread has died
      msg <- ws_awaitConn (_connQueue conn) (_connWaiters conn) waiterKilled
              `onException` setProp ("k" :: JSString) jsTrue waiterKilled
      _loadJSMessage msg

_loadJSMessage :: JSRef a -> IO (Maybe SocketMsg)
_loadJSMessage msg | isNull msg = return Nothing
                   | otherwise  = do
    blb <- isBlob msg
    if blb
      then do
        let blob = unsafeCoerce msg :: Blob
        readed <- fromStrict <$> readBlob blob
        return (Just (SocketMsgData readed))
      else do
        let blob = unsafeCoerce msg :: JSString
        return . Just . SocketMsgText . fromJSString $ blob


-- | Block and wait until the 'Connection' receives any message, and
-- returns the message wrapped in a 'SocketMsg'.  A 'SocketMsg' is a sum
-- type of either 'SocketMsgText t', containing (strict) 'Text', or
-- 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Will return the message as soon as any is received, or throw
-- a 'ConnectionException' if the connection is closed while waiting.
-- Throws an exception immediately if the connection is already closed.
--
-- To handle closed sockets with 'Maybe', use 'receiveMessageMaybe'.
receiveMessage :: Connection -> IO SocketMsg
receiveMessage conn = unjust <$> receiveMessageMaybe conn
  where
    unjust (Just i ) = i
    unjust Nothing   = throw $ ConnectionClosed (_connOrigin conn)

-- | Block and wait until the 'Connection' receives any message, and
-- attempts to decode it depending on the desired type.  If 'Text' is
-- requested, assumes Utf8-encoded text or just a plain Javascript string.
-- If an instance of 'Binary' is requested, attempts to decode it into that
-- instance.  Successful parses return 'Right x', and failed parses return
-- 'Left SocketMsg' (A sum type between 'SocketMsgText' containing (strict)
-- 'Text' and 'SocketMsgData' containing a (lazy) 'ByteString').
--
-- Returns @Just result@ on the first message received, or @Nothing@ if the
-- 'Connection' closes while waiting.  Returns @Nothing@ if the connection
-- is already closed and there are no queued messages left.
receiveEitherMaybe :: WSReceivable a => Connection -> IO (Maybe (Either SocketMsg a))
receiveEitherMaybe = (fmap . fmap) unwrapReceivable . receiveMessageMaybe

-- | Block and wait until the 'Connection' receives any message, and
-- attempts to decode it depending on the desired type.  If 'Text' is
-- requested, assumes Utf8-encoded text or just a plain Javascript string.
-- If an instance of 'Binary' is requested, attempts to decode it into that
-- instance.  Successful parses return 'Right x', and failed parses return
-- 'Left SocketMsg' (A sum type between 'SocketMsgText' containing (strict)
-- 'Text' and 'SocketMsgData' containing a (lazy) 'ByteString').
--
-- Will return the message as soon as any is received, or throw
-- a 'ConnectionException' if the connection is closed while waiting.
-- Throws an exception immediately if the connection is already closed and
-- there are no queued messages left.
--
-- To handle closed sockets with 'Maybe', use 'receiveEitherMaybe'.
--
receiveEither :: WSReceivable a => Connection -> IO (Either SocketMsg a)
receiveEither = fmap unwrapReceivable . receiveMessage
