module JavaScript.WebSockets (
    Connection
  , Sendable
  , Incoming(..)
  , withUrl
  , openConnection
  , closeConnection
  , send
  , sendData
  , sendText
  , receiveMessage
  , receiveEither
  , receive
  , receiveText
  , receiveData
  , receiveMessage_
  , receiveEither_
  , receive_
  , receiveText_
  , receiveData_
  ) where

import Control.Exception              (bracket)
import Data.Binary                    (Binary)
import Data.Text                      (Text)
import JavaScript.WebSockets.Internal

withUrl :: Text -> (Connection -> IO a) -> IO a
withUrl url process = do
    bracket
      (openConnection url)
      (closeConnection)
      process

sendData :: Binary a => Connection -> a -> IO Bool
sendData = send

sendText :: Connection -> Text -> IO Bool
sendText = send

receive :: Receivable a => Connection -> IO (Maybe a)
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

receive_ :: Receivable a => Connection -> IO a
receive_ conn = do
  d <- receiveEither_ conn
  case d of
    Right d' -> return d'
    _        -> receive_ conn

receiveText_ :: Connection -> IO Text
receiveText_ = receive_

receiveData_ :: Binary a => Connection -> IO a
receiveData_ = receive_

