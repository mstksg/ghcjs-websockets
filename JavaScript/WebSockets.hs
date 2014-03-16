module JavaScript.WebSockets (
    Connection
  , Sendable
  , withUrl
  , openConnection
  , closeConnection
  , send
  , receiveText
  , receiveDataMaybe
  , receiveData
  , receiveByteString
  , clearTextQueue
  , clearDataQueue
  , clearQueues
  ) where

import Control.Exception              (bracket)
import Data.Text                      (Text)
import JavaScript.WebSockets.Internal

withUrl :: Text -> (Connection -> IO a) -> IO a
withUrl url process = do
    bracket
      (openConnection url)
      (closeConnection)
      process

receiveDataMaybe :: Receivable a => Connection -> IO (Maybe a)
receiveDataMaybe = fmap (either (const Nothing) Just) . receiveDataEither

receiveData :: Receivable a => Connection -> IO a
receiveData conn = do
  md <- receiveDataMaybe conn
  case md of
    Just d  -> return d
    Nothing -> receiveData conn
