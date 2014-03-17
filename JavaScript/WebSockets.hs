module JavaScript.WebSockets (
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

sendData :: Binary a => Connection -> a -> IO Bool
sendData = send

sendText :: Connection -> Text -> IO Bool
sendText = send

sendData_ :: Binary a => Connection -> a -> IO ()
sendData_ conn = void . sendData conn

sendText_ :: Connection -> Text -> IO ()
sendText_ conn = void . sendText conn

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

