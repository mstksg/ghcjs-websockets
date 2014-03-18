-- |
-- Module      : JavaScript.WebSockets
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
--
-- 'JavaScript.WebSockets' contains functions and operations for working
-- with Javascript Websocket connections, which are encapsulated in the
-- 'Connection' object.
--
-- It includes operations for opening, closing, inspecting connections and
-- operations for sending and receiving text and serializable data
-- (instances of 'Binary') through them.
--
-- Most of the necessary functionality is in hopefully in
-- 'JavaScript.WebSockets'; more of the low-level API is exposed in
-- 'JavaScript.WebSockets.Internal' if you need it for library
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
  -- * Opening, closing, and working with connections
  , withUrl
  , openConnection
  , closeConnection
  , closeConnection'
  , clearConnectionQueue
  , connectionClosed
  , connectionClosed'
  , connectionOrigin
  -- * Sending data
  -- $sending
  -- ** With feedback
  , sendData
  , sendText
  , sendMessage
  , send
  -- ** Without feedback
  , sendData_
  , sendText_
  , sendMessage_
  , send_
  -- * Receiving data
  -- $receiving
  -- ** Safe
  , receiveText
  , receiveData
  , receiveMessage
  , receiveEither
  , receive
  -- ** Unsafe
  , receiveText_
  , receiveData_
  , receiveMessage_
  , receiveEither_
  , receive_
  -- * Exceptions
  , ConnectionException(..)
  ) where

import Control.Exception              (bracket)
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
-- >         t <- receiveText_ conn
-- >         putStrLn (unpack t)
-- >         sendText_ conn t
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
-- >     i <- receiveData_ conn
-- >     print (n + i)
-- >     runningSum (n + i) conn
--
-- 'receiveData_' will block until the 'Connection' receives data that is
-- decodable as whatever type you expect, and will throw away all
-- nondecodable data (including 'Text' data).
--
-- The 'receive_' function is provided as a disgustingly over-indulgent and
-- unnecessary layer of abstraction where you can receive both 'Text' and
-- instances of 'Binary' with the same function using typeclass magic ---
-- for the examples above, you could use 'receive_' in place of both
-- 'receiveText_' and 'receiveData_'.
--
-- 'send_' works the same way for 'sendText_' and 'sendData_'.
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
-- >         msg <- receiveMessage_
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
-- >                 msg <- receiveMessage_ conn1
-- >                 sendMessage_ conn2 msg
--
-- And also alternatively, you can manually open and close connections:
--
-- > -- Act as a relay between two servers
-- > main :: IO ()
-- > main = do
-- >     conn1 <- openConnection "ws://server-1.com"
-- >     conn2 <- openConnection "ws://server-2.com"
-- >     forever $ do
-- >         msg <- receiveMessage_ conn1
-- >         sendMessage_ conn2 msg
-- >     closeConnection conn2
-- >     closeConnection conn1
--
-- If you're manually working with connections like that, then the "safe",
-- non-underscore versions of 'receive_', 'send_' are available.
--
-- forall @X@, @receiveX@ behaves exactly like @receiveX_@, except it
-- returns @Maybe a@ instead of @a@, and returns @Nothing@ if the
-- connection is closed or if it closes while it's blocking/waiting.  If
-- you attempt to @receiveX_@ on a closed connection or if the connection
-- closes while you are waiting, a 'ConnectionException' will be thrown.
--
-- forall @X@, @sendX@ behaves exactly like @sendX_@, except it returns
-- 'Bool' instead of '()', where the 'Bool' indicates if connection you are
-- trying to send is open or not.  Trying to send message through a closed
-- connection returns 'False' (and nothing is ever sent) and trying on an
-- open connection returns 'True'.  This is different, technically, then
-- checking if the message was sent at all, as other things might go wrong
-- further down the line, or with the FFI API.  Hopefully stronger
-- guarantees will be implemented in due time.
--
-- You can use also 'connectionClosed' @:: Connection -> IO Bool@ to check
-- if the given 'Connection' object is closed.
--

-- | Performs the given @Connection -> IO a@ process attached to the given
-- server url.  Handles opening and closing the 'Connection' for you (and
-- clearing the message queue afterwards), and cleans up on errors.
withUrl :: Text -> (Connection -> IO a) -> IO a
withUrl url process = do
    bracket
      (openConnection url)
      closeConnection'
      process

-- $sending
--
-- All @sendX@ functions return @IO Bool@, which indicates if the
-- connection you just tried to send on is closed.  They all have
-- @sendX_@ counterparts, which just return @IO ()@.

-- | Send the given serializable (instance of 'Binary') data on the given
-- connection.
--
-- Returns 'True' if the connection is open, and 'False' if it
-- is closed.
sendData :: Binary a => Connection -> a -> IO Bool
sendData = send

-- | Send the given (strict) 'Text' on the given connection.
--
-- Returns 'True' if the connection is open, and 'False' if it
-- is closed.
sendText :: Connection -> Text -> IO Bool
sendText = send

-- | Send the given serializable (instance of 'Binary') data on the given
-- connection.
--
-- Fails silently if the connection is closed.
sendData_ :: Binary a => Connection -> a -> IO ()
sendData_ conn = void . sendData conn

-- | Send the given (strict) 'Text' on the given connection.
--
-- Fails silently if the connection is closed.
sendText_ :: Connection -> Text -> IO ()
sendText_ conn = void . sendText conn

-- $receiving
--
-- All @receiveX@ functions block until either the desired data is received
-- or the 'Connection' is closed.  They return immediately if the
-- 'Connection' is already closed.  They return @Just x@ on a successful
-- retrieval on an open 'Connection' and @Nothing@ if the 'Connection'
-- closes while waiting, or if it is called on a closed 'Connection' with
-- nothing left in its queue.
--
-- They also come with @receiveX_@ counterparts, which all return @x@ on
-- successful retrievals on an open 'Connection' and raise
-- a 'ConnectionException' on a closed 'Connection'
--
-- For most @receiveX@ functions (the exceptions being 'receiveMessage',
-- which matches everything, and 'receiveEither', which returns @Left
-- socketMsg@ on an unsuccessful parse), incoming data that does not match
-- what you are looking for is discarded.

-- | Block and wait until the 'Connection' receives a "typed" 'Text'.  This
-- is determined by Javascript's own "typed" Websockets API
-- <http://www.w3.org/TR/websockets/>, which receives data typed either as
-- text or as a binary blob.  Returns @Just t@ on the first encountered
-- text.  Returns @Nothing@ if the 'Connection' closes while it is waiting,
-- or immediately if the connection is already closed and there are no
-- queued messages left.
--
-- All "binary blobs" encountered are discarded.
receiveText :: Connection -> IO (Maybe Text)
receiveText = receive

-- | Block and wait until the 'Connection' receives a "binary blob"
-- decodable as the desired instance of 'Binary'.  Returns @Just x@ as soon
-- as it is able to decode a blob, and @Nothing@ if the 'Connection' closes
-- while it is waiting.  Returns @Nothing@ immediately if the 'Connection'
-- is already closed and there are no queued messages left.
--
-- This is polymorphic on its return type, so remember to let the type
-- inference system know what you want at some point or just give an
-- explicit type signature --- @receiveData conn :: IO (Maybe Int)@, for
-- example.
receiveData :: Binary a => Connection -> IO (Maybe a)
receiveData = receive

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
-- All non-decodable data that comes along is discarded.
--
-- You can 'receive' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'receiveText' or 'receiveData'.
receive :: WSReceivable a => Connection -> IO (Maybe a)
receive conn = do
  d <- receiveEither conn
  case d of
    Nothing         -> return Nothing
    Just (Right d') -> return (Just d')
    Just _          -> receive conn

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
-- For a "safe" version, see 'receive'.
receiveText_ :: Connection -> IO Text
receiveText_ = receive_

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
-- For a "safe" version, see 'receive'.
receiveData_ :: Binary a => Connection -> IO a
receiveData_ = receive_

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
-- All non-decodable data that comes along is discarded.
--
-- You can 'receive_' either (strict) 'Text' or any instance of 'Binary',
-- due to over-indulgent typeclass magic; this is basically a function that
-- works everywhere you would use 'receiveText_' or 'receiveData_'.
--
-- For a "safe" version, see 'receive'.
receive_ :: WSReceivable a => Connection -> IO a
receive_ conn = do
  d <- receiveEither_ conn
  case d of
    Right d' -> return d'
    _        -> receive_ conn

