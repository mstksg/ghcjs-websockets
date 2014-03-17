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
  , receiveMessage_
  , receiveEither
  , receiveEither_
  ) where

import Control.Applicative       ((<$>))
import Control.Monad             (when)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Data.Text as T            (Text, unpack, append)
import Data.Text.Encoding        (decodeUtf8', encodeUtf8, decodeUtf8)
import GHCJS.Foreign             (fromJSString, toJSString, newArray)
import GHCJS.Types               (JSString, isNull)
import JavaScript.Blob           (isBlob, readBlob)
import JavaScript.WebSockets.FFI
import Unsafe.Coerce             (unsafeCoerce)

import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Base64.Lazy as B64L

data Connection = Connection { _connSocket     :: Socket
                             , _connQueue      :: ConnectionQueue
                             , _connWaiters    :: ConnectionWaiters
                             , _connOrigin     :: Text
                             , _connClosed     :: IORef Bool
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
  closed <- newIORef False
  return $ Connection socket queue waiters url closed

closeConnection :: Connection -> IO ()
closeConnection conn = do
  closed <- readIORef (_connClosed conn)
  when closed $ do
    ws_closeSocket (_connSocket conn)
    writeIORef (_connClosed conn) True
    ws_clearWaiters (_connWaiters conn)

sendMessage :: Connection -> Outgoing -> IO Bool
sendMessage conn msg = do
  closed <- readIORef (_connClosed conn)
  if closed
    then
      return False
    else do
      ws_socketSend (_connSocket conn) (outgoingData msg)
      return True
  where
    outgoingData (OutgoingText t) = toJSString . decodeUtf8 . B64.encode . encodeUtf8 $ t
    outgoingData (OutgoingData d) = toJSString . decodeUtf8 . toStrict . B64L.encode $ d

send :: Sendable a => Connection -> a -> IO Bool
send conn = sendMessage conn . wrapSendable

receiveMessage :: Connection -> IO (Maybe Incoming)
receiveMessage conn = do
  closed <- readIORef (_connClosed conn)
  if closed
    then
      return Nothing
    else do
      msg <- ws_awaitConn (_connQueue conn) (_connWaiters conn)
      if isNull msg
        then
          return Nothing
        else do
          blb <- isBlob msg
          if blb
            then do
              let blob = unsafeCoerce msg
              readed <- fmap fromStrict <$> readBlob blob
              case readed of
                Just b  -> return (Just (IncomingData b))
                Nothing -> receiveMessage conn
            else do
              let blob = unsafeCoerce msg :: JSString
              return (Just . IncomingText . fromJSString $ blob)

receiveMessage_ :: Connection -> IO Incoming
receiveMessage_ conn = unjust <$> receiveMessage conn
  where
    unjust (Just i ) = i
    unjust Nothing  = error . T.unpack $ T.append "Waiting on closed connection " (_connOrigin conn)

receiveEither :: Receivable a => Connection -> IO (Maybe (Either Incoming a))
receiveEither = (fmap . fmap) unwrapReceivable . receiveMessage

receiveEither_ :: Receivable a => Connection -> IO (Either Incoming a)
receiveEither_ = fmap unwrapReceivable . receiveMessage_
