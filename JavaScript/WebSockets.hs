module JavaScript.WebSockets (
    Connection
  , Sendable
  , withUrl
  , awaitText
  , awaitText'
  , awaitData
  , awaitData'
  ) where

import Control.Exception              (bracket)
import Control.Spoon                  (teaspoon)
import Data.Binary                    (Binary, encode, decode)
import Data.ByteString.Lazy           (ByteString, fromStrict)
import Data.Text                      (Text)
import Data.Text.Encoding             (encodeUtf8, decodeUtf8)
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

awaitData_ :: Binary a => Bool -> Connection -> IO a
awaitData_ queue conn = do
  bs <- awaitByteString_ queue conn
  case teaspoon (decode bs) of
    Just d  -> return d
    Nothing -> awaitData_ queue conn

awaitData :: Binary a => Connection -> IO a
awaitData = awaitData_ True

awaitData' :: Binary a => Connection -> IO a
awaitData' = awaitData_ False


