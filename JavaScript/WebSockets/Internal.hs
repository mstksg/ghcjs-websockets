{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.WebSockets.Internal (
  -- * Types
  -- ** Data types
    Connection(..)
  , SocketMsg(..)
  -- ** Typeclasses
  , WSSendable(..)
  , WSReceivable(..)
  -- ** Exceptions
  , ConnectionException(..)
  -- * Manipulating and inspecting 'Connection's
  , openConnection
  , closeConnection
  , closeConnection'
  , clearConnectionQueue
  , connectionClosed
  , connectionOrigin
  -- * Sending data
  -- ** With feedback
  , sendMessage
  , send
  -- ** Without feedback
  , sendMessage_
  , send_
  -- * Receiving data
  -- ** Safe
  , receiveMessage
  , receiveEither
  -- ** Unsafe
  , receiveMessage_
  , receiveEither_
  ) where

import Control.Applicative       ((<$>))
import Control.Exception         (Exception, throw)
import Control.Monad             (unless, void, when)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent.MVar   (MVar, newMVar, withMVar)
import Data.Text as T            (Text, unpack, append)
import Data.Text.Encoding        (decodeUtf8', encodeUtf8, decodeUtf8)
import Data.Typeable             (Typeable)
import GHCJS.Foreign             (fromJSString, toJSString, newArray)
import GHCJS.Types               (JSString, isNull)
import JavaScript.Blob           (isBlob, readBlob)
import JavaScript.WebSockets.FFI
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
data Connection = Connection { _connSocket     :: Socket
                             , _connQueue      :: ConnectionQueue
                             , _connWaiters    :: ConnectionWaiters
                             , _connOrigin     :: Text
                             , _connClosed     :: IORef Bool
                             , _connBlock      :: MVar ()
                             }

-- | Sum type over the data that can be sent or received through
-- a JavaScript websocket.
--
-- What an incoming message is classified as depends on the Javascript
-- Websockets API <http://www.w3.org/TR/2011/WD-websockets-20110419/>,
-- which provides a "typed" input channel of either text or binary blob.
--
-- There are several convenience functions to help you never have to deal
-- with this explicitly; its main purpose is if you want to explicitly
-- branch on a 'receiveMessage' depending on what kind of message you
-- receive and do separate things. 'receiveText' and 'receiveData' will
-- both allow you to "filter" incoming messages by their type.
data SocketMsg = SocketMsgText Text
               | SocketMsgData ByteString
               deriving (Show, Eq, Typeable)

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
openConnection :: Text -> IO Connection
openConnection url = do
  queue <- newArray
  waiters <- newArray
  socket <- ws_newSocket (toJSString url) queue waiters
  closed <- newIORef False
  block <- newMVar ()
  return $ Connection socket queue waiters url closed block

-- | Manually closes the given 'Connection'.  Will un-block all threads
-- currently waiting on the 'Connection' for messages (releasing their
-- callbacks) and disable sending and receiving in the future.
--
-- Will not clear the message queue, so unprocessed messages will be left
-- for future retrievals to access.  This might not be desired, so either
-- manually clear the queue with 'clearConnectionQueue' or use
-- 'closeConnection''.
closeConnection :: Connection -> IO ()
closeConnection = closeConnection_ False

-- | Manually closes the given 'Connection'.  Will un-block all threads
-- currently waiting on the 'Connection' for messages (releasing their
-- callbacks) and disable sending and receiving in the future.  Also clears
-- the message queue on that 'Connection' so no future retrievals will
-- return anything.
closeConnection' :: Connection -> IO ()
closeConnection' = closeConnection_ True

closeConnection_ :: Bool -> Connection -> IO ()
closeConnection_ clr conn = do
  withMVar (_connBlock conn) . const $ do
    closed <- readIORef (_connClosed conn)
    unless closed $ do
      writeIORef (_connClosed conn) True
      ws_closeSocket (_connSocket conn)
      ws_clearWaiters (_connWaiters conn)
      when clr $ ws_clearQueue (_connQueue conn)

-- | Clears the message queue (messages waiting to be 'receive'd) on the
-- given 'Connection'.  Works on closed 'Connection's.
clearConnectionQueue :: Connection -> IO ()
clearConnectionQueue = ws_clearQueue . _connQueue

-- | Check if the given 'Connection' is closed.
connectionClosed :: Connection -> IO Bool
connectionClosed conn = withMVar (_connBlock conn)
                                 (const (readIORef (_connClosed conn)))

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
      ws_socketSend (_connSocket conn) (outgoingData msg)
      return True
  where
    outgoingData (SocketMsgText t) = toJSString . decodeUtf8 . B64.encode . encodeUtf8 $ t
    outgoingData (SocketMsgData d) = toJSString . decodeUtf8 . toStrict . B64L.encode $ d

-- | Sends the given 'SocketMsg' through the given 'Connection'.
-- A 'SocketMsg' is a sum type of either 'SocketMsgText t', containing
-- (strict) 'Text', or 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Fails silently if the connection is closed.
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
-- Fails silently if the connection is closed.
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
receiveMessage :: Connection -> IO (Maybe SocketMsg)
receiveMessage conn = do
  closed <- connectionClosed conn
  msg <- if closed
           then ws_awaitConnClosed (_connQueue conn)
           else ws_awaitConn (_connQueue conn) (_connWaiters conn)
  if isNull msg
    then
      return Nothing
    else do
      blb <- isBlob msg
      if blb
        then do
          let blob = unsafeCoerce msg
          readed <- fmap fromStrict <$> readBlob blob
          case readed of
            Just b  -> return (Just (SocketMsgData b))
            Nothing -> receiveMessage conn
        else do
          let blob = unsafeCoerce msg :: JSString
          return (Just . SocketMsgText . fromJSString $ blob)

-- | Block and wait until the 'Connection' receives any message, and
-- returns the message wrapped in a 'SocketMsg'.  A 'SocketMsg' is a sum
-- type of either 'SocketMsgText t', containing (strict) 'Text', or
-- 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Will return the message as soon as any is received, or throw
-- a 'ConnectionException' if the connection is closed while waiting.
-- Throws an exception immediately if the connection is already closed.
--
-- For a "safe" version, see 'receiveMessage'.
receiveMessage_ :: Connection -> IO SocketMsg
receiveMessage_ conn = unjust <$> receiveMessage conn
  where
    unjust (Just i ) = i
    unjust Nothing  = throw $ ConnectionClosed (_connOrigin conn)

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
receiveEither :: WSReceivable a => Connection -> IO (Maybe (Either SocketMsg a))
receiveEither = (fmap . fmap) unwrapReceivable . receiveMessage

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
-- For a "safe" version, see 'receiveEither'.
receiveEither_ :: WSReceivable a => Connection -> IO (Either SocketMsg a)
receiveEither_ = fmap unwrapReceivable . receiveMessage_
