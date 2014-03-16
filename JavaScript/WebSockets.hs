module JavaScript.WebSockets (
    Connection
  , Sendable
  , withUrl
  , awaitText
  , awaitText'
  , awaitData
  , awaitData'
  , awaitByteString
  , awaitByteString'
  , clearTextQueue
  , clearDataQueue
  , clearQueues
  ) where

import Control.Exception              (bracket)
import Data.Binary                    (Binary)
import Data.ByteString.Lazy           (ByteString)
import Data.Text                      (Text)
import JavaScript.WebSockets.Internal

withUrl :: Text -> (Connection -> IO a) -> IO a
withUrl url process = do
    bracket
      (openConnection url)
      (closeConnection)
      process

awaitText :: Connection -> IO Text
awaitText = awaitText_ True

awaitText' :: Connection -> IO Text
awaitText' = awaitText_ False

awaitByteString :: Connection -> IO ByteString
awaitByteString = awaitByteString_ True

awaitByteString' :: Connection -> IO ByteString
awaitByteString' = awaitByteString_ False

awaitData :: Binary a => Connection -> IO a
awaitData = awaitData_ True

awaitData' :: Binary a => Connection -> IO a
awaitData' = awaitData_ False

