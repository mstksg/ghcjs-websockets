module JavaScript.WebSockets (
    Connection
  , Sendable
  , withUrl
  , openConnection
  , closeConnection
  , send
  , receiveText
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

