{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JavaScript.WebSockets.Internal (
    Connection
  , Sendable
  , Receivable
  , Incoming(..)
  , Outgoing(..)
  , openConnection
  , closeConnection
  , sendMessage
  , send
  , receiveMessage
  , receiveEither
  ) where

import Control.Applicative       ((<$>))
import Control.Concurrent.MVar   (MVar, newMVar, readMVar, modifyMVar_, putMVar, swapMVar, withMVar, takeMVar, newEmptyMVar)
import Control.Monad             (when)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.Text as T            (Text, append, unpack)
import Data.Text.Encoding        (decodeUtf8', encodeUtf8, decodeUtf8)
import GHCJS.Foreign             (fromJSString, toJSString, newArray)
import GHCJS.Types               (JSString)
import JavaScript.Blob           (isBlob, readBlob)
import JavaScript.WebSockets.FFI
import Unsafe.Coerce             (unsafeCoerce)

import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Base64.Lazy as B64L

data Connection = Connection { _connSocket     :: Socket
                             , _connQueue      :: ConnectionQueue
                             , _connWaiters    :: ConnectionWaiters
                             , _connOrigin     :: Text
                             , _connClosed     :: MVar Bool
                             }

data Incoming = IncomingText Text
              | IncomingData ByteString
              deriving (Show, Eq)

data Outgoing = OutgoingText Text
              | OutgoingData ByteString
              deriving (Show, Eq)

class Sendable s where
    wrapSendable :: s -> Outgoing

class Receivable s where
    unwrapReceivable :: Incoming -> Either Incoming s

instance Sendable Text where
    wrapSendable = OutgoingText

instance Binary a => Sendable a where
    wrapSendable = OutgoingData . encode

instance Receivable Text where
    unwrapReceivable (IncomingText t) = Right t
    unwrapReceivable i@(IncomingData d) = f . decodeUtf8' . toStrict $ d
      where
        f (Right t) = Right t
        f _         = Left i

instance Binary a => Receivable a where
    unwrapReceivable inc =
        case decodeOrFail bs of
          Right (_,_,x) -> Right x
          Left   _      -> Left inc
      where
        bs = case inc of
               IncomingText t -> fromStrict (encodeUtf8 t)
               IncomingData d -> d

openConnection :: Text -> IO Connection
openConnection url = do
  queue <- newArray
  waiters <- newArray
  socket <- ws_newSocket (toJSString url) queue waiters
  closed <- newMVar False
  return $ Connection socket queue waiters url closed

closeConnection :: Connection -> IO ()
closeConnection conn = do
  closed <- readMVar (_connClosed conn)
  when closed $ do
    ws_closeSocket (_connSocket conn)
    putMVar (_connClosed conn) True
    -- kill waiters

sendMessage :: Connection -> Outgoing -> IO ()
sendMessage conn msg = do
  closed <- readMVar (_connClosed conn)
  if closed
    then error . T.unpack $
      "Attempting to send from closed websocket " `T.append` _connOrigin conn
    else
      ws_socketSend (_connSocket conn) (outgoingData msg)
  where
    outgoingData (OutgoingText t) = toJSString . decodeUtf8 . B64.encode . encodeUtf8 $ t
    outgoingData (OutgoingData d) = toJSString . decodeUtf8 . toStrict . B64L.encode $ d

send :: Sendable a => Connection -> a -> IO ()
send conn = sendMessage conn . wrapSendable

receiveMessage :: Connection -> IO Incoming
receiveMessage conn = do
  msg <- ws_awaitConn (_connQueue conn) (_connWaiters conn)
  blb <- isBlob msg
  if blb
    then do
      let blob = unsafeCoerce msg
      readed <- fmap fromStrict <$> readBlob blob
      case readed of
        Just b -> return (IncomingData b)
        Nothing -> receiveMessage conn
    else do
      let blob = unsafeCoerce msg :: JSString
      return (IncomingText . fromJSString $ blob)


receiveEither :: Receivable a => Connection -> IO (Either Incoming a)
receiveEither = fmap unwrapReceivable . receiveMessage
