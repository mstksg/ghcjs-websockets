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
  , receiveEither
  , receiveMaybe
  , receive
  , receiveText
  , receiveData
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

sendData :: Binary a => Connection -> a -> IO ()
sendData = send

sendText :: Connection -> Text -> IO ()
sendText = send

receive :: Receivable a => Connection -> IO a
receive conn = do
  md <- receiveMaybe conn
  case md of
    Just d  -> return d
    Nothing -> receive conn

receiveMaybe :: Receivable a => Connection -> IO (Maybe a)
receiveMaybe = fmap (either (const Nothing) Just) . receiveEither

receiveText :: Connection -> IO Text
receiveText = receive

receiveData :: Binary a => Connection -> IO a
receiveData = receive
