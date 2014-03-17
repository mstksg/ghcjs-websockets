-- |
-- Module      : JavaScript.WebSockets
-- Copyright   : (c) Justin Le 2014
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- 'JavaScript.WebSockets' aims to provide a clean, idiomatic, efficient,
-- low-level, out-of-your-way, bare bones, concurrency-aware interface with
-- minimal abstractions over the Javascript Websockets API
-- <http://www.w3.org/TR/2011/WD-websockets-20110419/>, inspired by common
-- Haskell idioms found in libraries like @io-stream@
-- <http://hackage.haskell.org/package/io-streams> and the server-side
-- @websockets@ <http://hackage.haskell.org/package/websockets> library,
-- targeting compilation to Javascript with @ghcjs@.
--
-- The interface asbtracts websockets as simple IO/file handles, with
-- additional access to the natively "typed" (text vs binary) nature of the
-- Javascript Websockets API.  There are also convenience functions to
-- directly decode serialized data (serialized with @binary@
-- <http://hackage.haskell.org/package/binary>) sent through channels.
--
-- The library is mostly intended to be a low-level FFI library, with the
-- hopes that other, more advanced libraries maybe build on the low-level
-- FFI bindings in order to provide more advanced and powerful
-- abstractions.  Most design decisions were made with the intent of
-- keeping things as simple as possible in order for future libraries to
-- abstract over it.
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
  -- * Opening and closing connections
  , openConnection
  , closeConnection
  , withUrl
  -- * Sending data
  -- $sending
  -- ** With feedback
  , send
  , sendData
  , sendText
  , sendMessage
  -- ** Without feedback
  , send_
  , sendData_
  , sendText_
  , sendMessage_
  -- * Receiving data
  -- $receiving
  -- ** Safe
  , receive
  , receiveText
  , receiveData
  , receiveMessage
  , receiveEither
  -- ** Unsafe
  , receive_
  , receiveText_
  , receiveData_
  , receiveMessage_
  , receiveEither_
  -- * Exceptions
  , ConnectionException(..)
  ) where

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
-- 'SocketMsg' sum type:
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

import Control.Exception              (bracket)
import Control.Monad                  (void)
import Data.Binary                    (Binary)
import Data.Text                      (Text)
import JavaScript.WebSockets.Internal

withUrl :: Text -> (Connection -> IO a) -> IO a
withUrl url process = do
    bracket
      (openConnection url)
      closeConnection
      process

-- $sending
--
-- All @sendX@ functions return @IO Bool@, which indicates if the
-- connection you just tried to send on is closed.  They all have
-- @sendX_@ counterparts, which just return @IO ()@.

sendData :: Binary a => Connection -> a -> IO Bool
sendData = send

sendText :: Connection -> Text -> IO Bool
sendText = send

sendData_ :: Binary a => Connection -> a -> IO ()
sendData_ conn = void . sendData conn

sendText_ :: Connection -> Text -> IO ()
sendText_ conn = void . sendText conn

-- $receiving
--
-- All @receiveX@ functions block until either the desired data is received
-- or the connection is closed.  They return immediately if the connection
-- is already closed.  They return @Just x@ on a successful retrieval on an
-- open connection and @Nothing@ on a closed connection.  They also come
-- with @receiveX_@ counterparts, which all return @x@ on successful
-- retrievals on an open connection and raise a 'ConnectionException' on
-- a closed connection.
--
-- For most @receiveX@ functions (the exceptions being 'receiveMessage',
-- which matches everything, and 'receiveEither', which returns @Left
-- socketMsg@ on an unsuccessful parse), incoming data that does not match
-- what you are looking for is discarded.

receive :: WSReceivable a => Connection -> IO (Maybe a)
receive conn = do
  d <- receiveEither conn
  case d of
    Nothing         -> return Nothing
    Just (Right d') -> return (Just d')
    Just _          -> receive conn

receiveText :: Connection -> IO (Maybe Text)
receiveText = receive

receiveData :: Binary a => Connection -> IO (Maybe a)
receiveData = receive

receive_ :: WSReceivable a => Connection -> IO a
receive_ conn = do
  d <- receiveEither_ conn
  case d of
    Right d' -> return d'
    _        -> receive_ conn

receiveText_ :: Connection -> IO Text
receiveText_ = receive_

receiveData_ :: Binary a => Connection -> IO a
receiveData_ = receive_

