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
  , awaitMessage
  , receiveText
  , receiveByteString
  , receiveDataEither
  , receiveEither
  , clearTextQueue
  , clearDataQueue
  , clearQueues
  , viewQueues
  ) where

import Control.Applicative       ((<$>))
import Control.Concurrent        (forkIO, ThreadId)
import Control.Concurrent.MVar   (MVar, newMVar, readMVar, modifyMVar_, putMVar, swapMVar, withMVar, takeMVar, newEmptyMVar)
import Control.Monad             (when, void)
import Data.Binary               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy      (ByteString, fromStrict, toStrict)
import Data.Sequence as S        (Seq, viewl, (|>), ViewL(..), empty)
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
                             , _connAnyQueue   :: MVar (Seq (Incoming, MVar Bool))
                             , _connTextQueue  :: MVar (Seq (Text, MVar Bool))
                             , _connDataQueue  :: MVar (Seq (ByteString, MVar Bool))
                             , _connWaitersAny  :: MVar (Seq (MVar Incoming))
                             , _connWaitersText :: MVar (Seq (MVar Text))
                             , _connWaitersData :: MVar (Seq (MVar ByteString))
                             , _connWorker     :: Maybe ThreadId
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
    unwrapReceivable (IncomingData d) = either (const Nothing) Just . decodeUtf8' . toStrict $ d

instance Binary a => Receivable a where
    unwrapReceivable inc =
        case decodeOrFail bs of
          Right (_,_,x) -> Just x
          Left   _      -> Nothing
      where
        bs = case inc of
               IncomingText t -> fromStrict (encodeUtf8 t)
               IncomingData d -> d

openConnection :: Text -> IO Connection
openConnection url = do
  queue <- newArray
  waiters <- newArray
  socket <- ws_newSocket (toJSString url) queue waiters
  tq <- newMVar empty
  bq <- newMVar empty
  wa <- newMVar empty
  ta <- newMVar empty
  da <- newMVar empty
  closed <- newMVar False
  let conn = Connection socket queue waiters tq bq wa ta da Nothing url closed
  worker <- forkIO (workerThread conn)
  return $ conn { _connWorker = Just worker }

closeConnection :: Connection -> IO ()
closeConnection conn = do
  closed <- readMVar (_connClosed conn)
  when closed $ do
    ws_closeSocket (_connSocket conn)
    putMVar (_connClosed conn) True
    -- kill waiters

workerThread :: Connection -> IO ()
workerThread conn = do
  msg <- awaitMessage conn
  maw <- popQueue (_connWaitersAny conn)
  case maw of
    Just aw -> putMVar aw msg
    Nothing ->
      case msg of
        IncomingText t -> do
          mtw <- popQueue (_connWaitersText conn)
          case mtw of
            Just tw -> putMVar tw t
            Nothing -> pushQueue (_connTextQueue conn) t
        IncomingData d -> do
          mdw <- popQueue (_connWaitersData conn)
          case mdw of
            Just dw -> putMVar dw d
            Nothing -> pushQueue (_connDataQueue conn) d

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
    -- outgoingData (OutgoingData d) = toJSString . decodeUtf8 . toStrict $ d

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
receiveText = receiveQueue _connWaitersText _connTextQueue

receiveByteString :: Connection -> IO ByteString
receiveByteString = receiveQueue _connWaitersData _connDataQueue

receiveDataEither :: Binary a => Connection -> IO (Either ByteString a)
receiveDataEither = fmap unwrap . receiveByteString
  where
    unwrap bs = case decodeOrFail bs of
                  Right (_,_,x) -> Right x
                  Left   _      -> Left bs

receiveEither :: forall a. Receivable a => Connection -> IO (Either Incoming a)
receiveEither = fmap unwrap . awaitMessage
  where
    unwrap :: Incoming -> Either Incoming a
    unwrap msg = maybe (Left msg) Right (unwrapReceivable msg)

receiveQueue ::
       (Connection -> MVar (Seq (MVar a)))
    -> (Connection -> MVar (Seq a))
    -> (Connection -> IO a)
receiveQueue wref qref conn = do
  closed <- readMVar (_connClosed conn)
  if closed
    then error . T.unpack $
      "Attempting to receive from closed websocket " `T.append` _connOrigin conn
    else do
      queued <- popQueue (qref conn)
      case queued of
        Just x ->
          return x
        Nothing -> do
          waiter <- newEmptyMVar
          pushQueue (wref conn) waiter
          takeMVar waiter

clearQueues :: Connection -> IO ()
clearQueues conn = do
  clearTextQueue conn
  clearDataQueue conn

clearTextQueue :: Connection -> IO ()
clearTextQueue = clearQueue . _connTextQueue

clearDataQueue :: Connection -> IO ()
clearDataQueue = clearQueue . _connDataQueue

clearQueue :: MVar (Seq a) -> IO ()
clearQueue = void . flip swapMVar empty

pushQueue :: MVar (Seq a) -> a -> IO ()
pushQueue qref x = modifyMVar_ qref (return . (|> x))

pushQueueMarked :: MVar (Seq (a, MVar Bool)) -> a -> IO ()
pushQueueMarked qref x = do
  used <- newMVar False
  pushQueue qref (x,used)

popQueue :: MVar (Seq a) -> IO (Maybe a)
popQueue qref = do
  withMVar qref $ \q -> do
    case viewl q of
      EmptyL  ->
        return Nothing
      x :< xs -> do
        putMVar qref xs
        return (Just x)

popQueueMarked :: MVar (Seq (a, MVar Bool)) -> IO (Maybe a)
popQueueMarked qref = do
  popped <- popQueue qref
  case popped of
    Nothing -> return Nothing
    Just (x, uref) -> do
      used <- readMVar uref
      if used
        then
          Just x <$ swapMVar used True
        else
          popQueueMarked qref

viewQueues :: Connection -> IO (Seq Text, Seq ByteString)
viewQueues conn = do
  tq <- readMVar (_connTextQueue conn)
  dq <- readMVar (_connDataQueue conn)
  return (tq, dq)
