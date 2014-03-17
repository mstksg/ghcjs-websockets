{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.WebSockets.Internal (
    Connection
  , WSSendable
  , WSReceivable
  , SocketMsg(..)
  , ConnectionException(..)
  , openConnection
  , closeConnection
  , connectionClosed
  , sendMessage
  , sendMessage_
  , send
  , send_
  , receiveMessage
  , receiveMessage_
  , receiveEither
  , receiveEither_
  ) where

import Control.Applicative       ((<$>))
import Control.Exception         (Exception, throw)
import Control.Monad             (unless, void)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Data.Text as T            (Text, unpack, append)
import Data.Text.Encoding        (decodeUtf8', encodeUtf8, decodeUtf8)
import Data.Typeable             (Typeable)
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

data SocketMsg = SocketMsgText Text
               | SocketMsgData ByteString
               deriving (Show, Eq)

data ConnectionException = ConnectionClosed { _socketOrigin :: Text }
                         deriving (Eq, Typeable)

instance Show ConnectionException where
    show (ConnectionClosed o) = T.unpack $ T.append "Error: Waiting on closed connection " o
instance Exception ConnectionException

class WSSendable s where
    wrapSendable :: s -> SocketMsg

class WSReceivable s where
    unwrapReceivable :: SocketMsg -> Either SocketMsg s

instance WSSendable Text where
    wrapSendable = SocketMsgText

instance Binary a => WSSendable a where
    wrapSendable = SocketMsgData . encode

instance WSReceivable Text where
    unwrapReceivable (SocketMsgText t) = Right t
    unwrapReceivable i@(SocketMsgData d) = f . decodeUtf8' . toStrict $ d
      where
        f (Right t) = Right t
        f _         = Left i

instance Binary a => WSReceivable a where
    unwrapReceivable inc =
        case decodeOrFail bs of
          Right (_,_,x) -> Right x
          Left   _      -> Left inc
      where
        bs = case inc of
               SocketMsgText t -> fromStrict (encodeUtf8 t)
               SocketMsgData d -> d

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
  unless closed $ do
    ws_closeSocket (_connSocket conn)
    writeIORef (_connClosed conn) True
    ws_clearWaiters (_connWaiters conn)

connectionClosed :: Connection -> IO Bool
connectionClosed = readIORef . _connClosed

sendMessage :: Connection -> SocketMsg -> IO Bool
sendMessage conn msg = do
  closed <- readIORef (_connClosed conn)
  if closed
    then
      return False
    else do
      ws_socketSend (_connSocket conn) (outgoingData msg)
      return True
  where
    outgoingData (SocketMsgText t) = toJSString . decodeUtf8 . B64.encode . encodeUtf8 $ t
    outgoingData (SocketMsgData d) = toJSString . decodeUtf8 . toStrict . B64L.encode $ d

sendMessage_ :: Connection -> SocketMsg -> IO ()
sendMessage_ conn = void . sendMessage conn

send :: WSSendable a => Connection -> a -> IO Bool
send conn = sendMessage conn . wrapSendable

send_ :: WSSendable a => Connection -> a -> IO ()
send_ conn = void . send conn

receiveMessage :: Connection -> IO (Maybe SocketMsg)
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
                Just b  -> return (Just (SocketMsgData b))
                Nothing -> receiveMessage conn
            else do
              let blob = unsafeCoerce msg :: JSString
              return (Just . SocketMsgText . fromJSString $ blob)

receiveMessage_ :: Connection -> IO SocketMsg
receiveMessage_ conn = unjust <$> receiveMessage conn
  where
    unjust (Just i ) = i
    unjust Nothing  = throw $ ConnectionClosed (_connOrigin conn)

receiveEither :: WSReceivable a => Connection -> IO (Maybe (Either SocketMsg a))
receiveEither = (fmap . fmap) unwrapReceivable . receiveMessage

receiveEither_ :: WSReceivable a => Connection -> IO (Either SocketMsg a)
receiveEither_ = fmap unwrapReceivable . receiveMessage_
