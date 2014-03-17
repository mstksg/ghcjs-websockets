module JavaScript.WebSockets (
    Connection
  , Sendable
  , withUrl
  , openConnection
  , closeConnection
  , send
  , sendData
  , sendText
  , receiveText
  , receiveMaybe
  , receive
  , receiveByteString
  , receiveDataEither
  , receiveDataMaybe
  , receiveData
  , clearTextQueue
  , clearDataQueue
  , clearQueues
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

receiveMaybe :: Receivable a => Connection -> IO (Maybe a)
receiveMaybe = fmap (either (const Nothing) Just) . receiveEither

receive :: Receivable a => Connection -> IO a
receive conn = do
  md <- receiveMaybe conn
  case md of
    Just d  -> return d
    Nothing -> receive conn

receiveDataMaybe :: Binary a => Connection -> IO (Maybe a)
receiveDataMaybe = fmap (either (const Nothing) Just) . receiveDataEither

receiveData :: Binary a => Connection -> IO a
receiveData conn = do
  md <- receiveDataMaybe conn
  case md of
    Just d  -> return d
    Nothing -> receiveData conn
