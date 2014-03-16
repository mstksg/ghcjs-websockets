{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , awaitMessage
  , receiveText
  , receiveByteString
  , receiveData
  , clearTextQueue
  , clearDataQueue
  , clearQueues
  ) where

import Control.Applicative       ((<$>))
import Control.Concurrent.MVar   (MVar, newMVar, readMVar, modifyMVar_, putMVar, takeMVar, swapMVar)
import Control.Monad             (when, void)
import Control.Spoon             (teaspoon)
import Data.Binary               (Binary, encode, decode)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.Sequence             as S
import Data.Text as T            (Text, append, unpack)
import Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import GHCJS.Foreign             (fromJSString, toJSString, newArray)
import GHCJS.Types               (JSString)
import JavaScript.Blob           (isBlob, readBlob)
import JavaScript.WebSockets.FFI
import Unsafe.Coerce             (unsafeCoerce)

data Connection = Connection { _connSocket     :: Socket
                             , _connQueue      :: ConnectionQueue
                             , _connWaiters    :: ConnectionWaiters
                             , _connTextQueue  :: MVar (Seq Text)
                             , _connDataQueue  :: MVar (Seq ByteString)
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
    unwrapReceivable :: Incoming -> Maybe s

instance Sendable Text where
    wrapSendable = OutgoingText

instance Binary a => Sendable a where
    wrapSendable = OutgoingData . encode

instance Receivable Text where
    unwrapReceivable (IncomingText t) = Just t
    unwrapReceivable (IncomingData d) = teaspoon . decodeUtf8 . toStrict $ d

instance Binary a => Receivable a where
    unwrapReceivable (IncomingText t) = teaspoon . decode . fromStrict . encodeUtf8 $ t
    unwrapReceivable (IncomingData d) = teaspoon . decode $ d


openConnection :: Text -> IO Connection
openConnection url = do
  queue <- newArray
  waiters <- newArray
  socket <- ws_newSocket (toJSString url) queue waiters
  tq <- newMVar empty
  bq <- newMVar empty
  closed <- newMVar False
  return $ Connection socket queue waiters tq bq url closed

closeConnection :: Connection -> IO ()
closeConnection conn = do
  closed <- readMVar (_connClosed conn)
  when closed $ do
    ws_closeSocket (_connSocket conn)
    putMVar (_connClosed conn) True
    -- kill waiters

sendMessage :: Connection -> Outgoing -> IO ()
sendMessage conn = ws_socketSend (_connSocket conn) . outgoingData
  where
    outgoingData (OutgoingText t) = toJSString t
    outgoingData (OutgoingData d) = toJSString (decodeUtf8 (toStrict d))

send :: Sendable a => Connection -> a -> IO ()
send conn = sendMessage conn . wrapSendable

awaitMessage :: Connection -> IO Incoming
awaitMessage conn = do
  msg <- ws_awaitConn (_connQueue conn) (_connWaiters conn)
  blb <- isBlob msg
  if blb
    then do
      let blob = unsafeCoerce msg
      readed <- fmap fromStrict <$> readBlob blob
      case readed of
        Just b -> return (IncomingData b)
        Nothing -> awaitMessage conn
    else do
      let blob = unsafeCoerce msg :: JSString
      return (IncomingText . fromJSString $ blob)

receiveText :: Connection -> IO Text
receiveText = receiveQueue fi _connTextQueue _connDataQueue
  where
    fi (IncomingText t) = Right t
    fi (IncomingData d) = Left d

receiveByteString :: Connection -> IO ByteString
receiveByteString = receiveQueue fi _connDataQueue _connTextQueue
  where
    fi (IncomingData d) = Right d
    fi (IncomingText t) = Left t

receiveData :: Receivable a => Connection -> IO a
receiveData conn = do
  msg <- awaitMessage conn
  case unwrapReceivable msg of
    Just d  -> return d
    Nothing -> receiveData conn

receiveQueue ::
       (Incoming -> Either b a)
    -> (Connection -> MVar (Seq a))
    -> (Connection -> MVar (Seq b))
    -> (Connection -> IO a)
receiveQueue fi refa refb conn = do
  closed <- readMVar (_connClosed conn)
  if closed
    then error . T.unpack $
      "Attempting to receive from closed websocket " `T.append` _connOrigin conn
    else do
      queued <- popQueue (refa conn)
      case queued of
        Just x ->
          return x
        Nothing -> do
          msg <- fi <$> awaitMessage conn
          case msg of
            Right x ->
              return x
            Left y  -> do
              pushQueue (refb conn) y
              receiveQueue fi refa refb conn

clearQueues :: Connection -> IO ()
clearQueues conn = do
  clearTextQueue conn
  clearDataQueue conn

clearTextQueue :: Connection -> IO ()
clearTextQueue = clearQueue . _connTextQueue

clearDataQueue :: Connection -> IO ()
clearDataQueue = clearQueue . _connDataQueue

clearQueue :: MVar (Seq a) -> IO ()
clearQueue = void . flip swapMVar S.empty

pushQueue :: MVar (Seq a) -> a -> IO ()
pushQueue qref x = modifyMVar_ qref (return . (|> x))

popQueue :: MVar (Seq a) -> IO (Maybe a)
popQueue qref = do
  q <- takeMVar qref
  case viewl q of
    EmptyL  ->
      return Nothing
    x :< xs -> do
      putMVar qref xs
      return (Just x)
