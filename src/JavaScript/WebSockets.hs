-- |
-- Module      : JavaScript.WebSockets
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : ghcjs
--
-- Contains functions and operations for working with Javascript Websocket
-- connections, which are encapsulated in the 'Connection' object.
--
-- It includes operations for opening, closing, inspecting connections and
-- operations for sending and receiving text and serializable data
-- (instances of 'Binary') through them.
--
-- Most of the necessary functionality is in hopefully in
-- "JavaScript.WebSockets"; more of the low-level API is exposed in
-- "JavaScript.WebSockets.Internal" if you need it for library
-- construction.
--

module JavaScript.WebSockets (
  -- * Usage
  -- $usage
  -- * Types
    Connection
  , WSSendable
  , WSReceivable
  , SocketMsg(..)
  , ConnClosing(..)
  , ConnectionException(..)
  -- * Opening, closing, and working with connections
  , withUrl
  , withUrlLeftovers
  , openConnection
  , closeConnection
  , closeConnectionLeftovers
  , clearConnectionQueue
  , dumpConnectionQueue
  , connectionClosed
  , connectionCloseReason
  , connectionOrigin
  -- * Sending data
  -- $sending
  , sendData
  , sendData_
  , sendText
  , sendText_
  , sendMessage
  , sendMessage_
  , send
  , send_
  -- * Receiving data
  -- $receiving
  , receive
  , receiveMaybe
  , receiveText
  , receiveTextMaybe
  , receiveData
  , receiveDataMaybe
  , receiveMessage
  , receiveMessageMaybe
  , receiveEither
  , receiveEitherMaybe
  ) where

import Control.Applicative
import Control.Concurrent             (readMVar)
import Control.Exception              (bracket, throw)
import Control.Monad                  (void)
import Data.Binary                    (Binary)
import Data.Text                      (Text)
import JavaScript.WebSockets.Internal


-- $usage
--
-- > import Data.Text (unpack)
-- >
-- > -- A simple echo client, echoing all incoming text data
-- > main :: IO ()
-- > main = withUrl "ws://my-server.com" $ \conn ->
-- >     forever $ do
-- >         t <- receiveText conn
-- >         putStrLn (unpack t)
-- >         sendText conn t
--
-- The above code will attempt to interpret all incoming data as
-- UTF8-encoded Text, and throw away data that does not.
--
-- @conn@ is a 'Connection', which encapsulates a websocket channel.
--
-- You can also do the same thing to interpret all incoming data as any
-- instance of 'Binary' --- say, 'Int's:
--
-- > -- A simple client waiting for connections and outputting the running sum
-- > main :: IO ()
-- > main = withUrl "ws://my-server.com" (runningSum 0)
-- >
-- > runningSum :: Int -> Connection -> IO ()
-- > runningSum n conn = do
-- >     i <- receiveData conn
-- >     print (n + i)
-- >     runningSum (n + i) conn
--
-- 'receiveData' will block until the 'Connection' receives data that is
-- decodable as whatever type you expect, and will throw away all
-- nondecodable data (including 'Text' data).
--
-- The 'receive' function is provided as a over-indulgent layer of
-- abstraction where you can receive both 'Text' and instances of 'Binary'
-- with the same function using typeclass magic --- for the examples above,
-- you could use 'receive' in place of both 'receiveText' and
-- 'receiveData'.
--
-- 'send' works the same way for 'sendText' and 'sendData'.
--
-- If you want to, you can access the incoming data directly using the
-- 'SocketMsg' sum type, which exposes either a 'Text' or a lazy
-- 'ByteString':
--
-- > import Data.Text (unpack, append)
-- > import qualified Data.ByteString.Base64.Lazy as B64
-- >
-- > main :: IO ()
-- > main = withUrl "ws://my-server.com" $ \conn ->
-- >     forever $ do
-- >         msg <- receiveMessage
-- >         putStrLn $ case msg of
-- >             SocketMsgText t ->
-- >                 unpack $ append "Received text: " t
-- >             SocketMsgData d ->
-- >                 "Received data: " ++ show (B64.encode d)
--
-- You can talk to multiple connections by nesting 'withUrl':
--
-- > -- Act as a relay between two servers
-- > main :: IO ()
-- > main =  withUrl "ws://server-1.com" $ \conn1 ->
-- >         withUrl "ws://server-2.com" $ \conn2 ->
-- >             forever $ do
-- >                 msg <- receiveMessage conn1
-- >                 sendMessage conn2 msg
--
-- And also alternatively, you can manually open and close connections:
--
-- > -- Act as a relay between two servers
-- > main :: IO ()
-- > main = do
-- >     conn1 <- openConnection "ws://server-1.com"
-- >     conn2 <- openConnection "ws://server-2.com"
-- >     forever $ do
-- >         msg <- receiveMessage conn1
-- >         sendMessage conn2 msg
-- >     closeConnection conn2
-- >     closeConnection conn1
--
-- 'receiveMessage' and its varieties will all throw an exception if the
-- connection closes while they're waiting or if you attempt to receive on
-- a closed connection.  You can handle these with mechanisms from
-- "Control.Exception", or you can use their "maybe"-family counterparts,
-- 'receiveMessageMaybe', etc., who will return results in 'Just' on
-- a success, or return a 'Nothing' if the connection is closed or if
-- receiving on a closed connection.
--
-- You can use also @'connectionClosed' :: 'Connection' -> 'IO' 'Bool'@ to
-- check if the given 'Connection' object is closed (or
-- 'connectionCloseReason' to see *why*).
--
-- When closing connections, there might be some messages that were
-- received by the socket but never processed on the Haskell side with
-- a 'receive' method.  These will normally be deleted; however, you can
-- use 'closeConnectionLeftovers' or 'withUrlLeftovers' to grab a list of
-- the raw 'SocketMsg's remaining after closing.

-- | Like 'withUrl', except returns also the "leftover messages" that were
-- received by the socket but never processed on the Haskell end with
-- 'receive'.
--
withUrlLeftovers :: Text                  -- ^ Websocket address to connect to
                 -> (Connection -> IO a)  -- ^ Process to run on connection
                 -> IO (a, [SocketMsg])   -- ^ Result of process, with leftovers
withUrlLeftovers url process = withUrl url $ \conn ->
    liftA2 (,) (process conn) (dumpConnectionQueue conn)

-- | Performs the given @Connection -> IO a@ process attached to the given
-- server url.  Handles opening and closing the 'Connection' for you (and
-- clearing the message queue afterwards), and cleans up on errors.
--
-- If any messages were received by the socket but never processed/received
-- on the Haskell end, this will delete and drop them.  Use
-- 'withUrlLeftovers' to get a hold of them.
--
withUrl :: Text                   -- ^ Websocket address to connect to
        -> (Connection -> IO a)   -- ^ Process to run on connection
        -> IO a
withUrl url = bracket (openConnection url) closeConnection

-- | Opens a websocket connection to the given url, and returns the
-- 'Connection' after connection is completed and opened.  Care should be
-- taken to ensure that the 'Connection' is later closed with
-- 'closeConnection'.
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

-- | Send the given serializable (instance of 'Binary') data on the given
-- connection.
--
-- Returns 'True' if the connection is open, and 'False' if it is closed.
-- In the future will return more feedback about whether or not the send
-- was completed succesfully.
sendData :: Binary a => Connection -> a -> IO Bool
sendData = send

-- | Send the given (strict) 'Text' on the given connection.
--
-- Returns 'True' if the connection is open, and 'False' if it is closed.
-- In the future will return more feedback about whether or not the send
-- was completed succesfully.
sendText :: Connection -> Text -> IO Bool
sendText = send

-- | Send the given serializable (instance of 'Binary') data on the given
-- connection.
--
-- Fails silently if the connection is closed or otherwise was not
-- succesful.  Use 'sendData' to get feedback on the result of the send.
sendData_ :: Binary a => Connection -> a -> IO ()
sendData_ conn = void . sendData conn

-- | Send the given (strict) 'Text' on the given connection.
--
-- Fails silently if the connection is closed or otherwise was not
-- succesful.  Use 'sendText' to get feedback on the result of the send.
sendText_ :: Connection -> Text -> IO ()
sendText_ conn = void . sendText conn

-- | Send the given item through the given 'Connection'.
--
-- You can 'send' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'sendText' or 'sendData'.
--
-- Returns 'True' if the connection is open, and 'False' if it is closed.
-- In the future will return more feedback about whether or not the send
-- was completed succesfully.
send :: WSSendable a => Connection -> a -> IO Bool
send conn = sendMessage conn . wrapSendable

-- | Send the given item through the given 'Connection'.
--
-- You can 'send_' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'sendText_' or 'sendData_'.
--
-- Fails silently if the connection is closed or otherwise was not
-- succesful.  Use 'send' to get feedback on the result of the send.
send_ :: WSSendable a => Connection -> a -> IO ()
send_ conn = void . send conn

-- | Sends the given 'SocketMsg' through the given 'Connection'.
-- A 'SocketMsg' is a sum type of either 'SocketMsgText t', containing
-- (strict) 'Text', or 'SocketMsgData d', containing a (lazy) 'ByteString'.
--
-- Fails silently if the connection is closed or otherwise was not
-- succesful.  Use 'sendMessage' to get feedback on the result of the send.
sendMessage_ :: Connection -> SocketMsg -> IO ()
sendMessage_ conn = void . sendMessage conn

-- | Block and wait until the 'Connection' receives a "typed" 'Text'.  This
-- is determined by Javascript's own "typed" Websockets API
-- <http://www.w3.org/TR/websockets/>, which receives data typed either as
-- text or as a binary blob.  Returns @Just t@ on the first encountered
-- text.  Returns @Nothing@ if the 'Connection' closes while it is waiting,
-- or immediately if the connection is already closed and there are no
-- queued messages left.
--
-- All "binary blobs" encountered are discarded.
receiveTextMaybe :: Connection -> IO (Maybe Text)
receiveTextMaybe = receiveMaybe

-- | Block and wait until the 'Connection' receives a "binary blob"
-- decodable as the desired instance of 'Binary'.  Returns @Just x@ as soon
-- as it is able to decode a blob, and @Nothing@ if the 'Connection' closes
-- while it is waiting.  Returns @Nothing@ immediately if the 'Connection'
-- is already closed and there are no queued messages left.
--
-- All incoming messages received that cannot be decoded as the data type
-- (or are text) will be discarded.
--
-- This is polymorphic on its return type, so remember to let the type
-- inference system know what you want at some point or just give an
-- explicit type signature --- @receiveData conn :: IO (Maybe Int)@, for
-- example.
receiveDataMaybe :: Binary a => Connection -> IO (Maybe a)
receiveDataMaybe = receiveMaybe

-- | Block and wait until either something decodable as the desired type is
-- received (returning @Just x@), or the 'Connection' closes (returning
-- @Nothing@).  Returns @Nothing@ immediately if the 'Connection' is
-- already closed and there are no queued messages left.
--
-- This is polymorphic on its return type, so remember to let the type
-- inference system know what you want at some point or just give an
-- explicit type signature --- @receiveData conn :: IO (Maybe Int)@, for
-- example.
--
-- All non-decodable or non-matching data that comes along is discarded.
--
-- You can 'receive' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'receiveText' or 'receiveData'.
receiveMaybe :: WSReceivable a => Connection -> IO (Maybe a)
receiveMaybe conn = do
  d <- receiveEitherMaybe conn
  case d of
    Nothing         -> return Nothing
    Just (Right d') -> return (Just d')
    Just _          -> receiveMaybe conn

-- | Block and wait until the 'Connection' receives a "typed" 'Text'.  This
-- is determined by Javascript's own "typed" Websockets API
-- <http://www.w3.org/TR/websockets/>, which receives data
-- typed either as text or as a binary blob.  Returns the first encountered
-- text.  Throws a 'ConnectionException' if the 'Connection' closes first,
-- and throws one immediately if the connection is already closed and there
-- are no queued messages left.
--
-- All "binary blobs" encountered are discarded.
--
-- To handle closed sockets with 'Maybe', use 'receiveTextMaybe'.
receiveText :: Connection -> IO Text
receiveText = receive

-- | Block and wait until the 'Connection' receives a "binary blob"
-- decodable as the desired instance of 'Binary'.  Returns the first
-- succesfully decoded data, and throws a 'ConnectionException' if the
-- 'Connection' closes first.  Throws the exception immediately if the
-- 'Connection' is already closed and there are no queued messages left.
--
-- This is polymorphic on its return type, so remember to let the type
-- inference system know what you want at some point or just give an
-- explicit type signature --- @receiveData conn :: IO (Maybe Int)@, for
-- example.
--
-- All incoming messages received that cannot be decoded as the data type
-- (or are text) will be discarded.
--
-- To handle closed sockets with 'Maybe', use 'receiveDataMaybe'.
receiveData :: Binary a => Connection -> IO a
receiveData = receive

-- | Block and wait until either something decodable as the desired type is
-- received (returning it), or the 'Connection' closes (throwing
-- a 'ConnectionException').  Throws the exception immediately if the
-- 'Connection' is already closed and there are no queued messages left.
--
-- This is polymorphic on its return type, so remember to let the type
-- inference system know what you want at some point or just give an
-- explicit type signature --- @receiveData conn :: IO (Maybe Int)@, for
-- example.
--
-- All non-decodable or non-matching data that comes along is discarded.
--
-- You can 'receive' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'receiveText' or 'receiveData'.
--
-- To handle closed sockets with 'Maybe', use 'receiveMaybe'.
receive :: WSReceivable a => Connection -> IO a
receive conn = do
  d <- receiveEither conn
  case d of
    Right d' -> return d'
    _        -> receive conn

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
-- 'Text' and 'SocketMsgData' containing a (lazy) 'ByteString').  Nothing
-- is ever discarded.
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
-- 'Text' and 'SocketMsgData' containing a (lazy) 'ByteString').  Nothing
-- is ever discarded.
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

-- | Returns the origin url of the given 'Connection'.
connectionOrigin :: Connection -> Text
connectionOrigin = _connOrigin

