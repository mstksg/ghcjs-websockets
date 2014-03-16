{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module JavaScript.WebSockets.Internal (
    Connection
  , Sendable
  , openConnection
  , closeConnection
  , sendMessage
  , send
  , awaitMessage
  , awaitText_
  , awaitByteString_
  , awaitData_
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
import Data.Text                 (Text)
import Data.Text.Encoding        (encodeUtf8, decodeUtf8)
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

instance Sendable Text where
    wrapSendable = OutgoingText

instance Binary a => Sendable a where
    wrapSendable = OutgoingData . encode

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

awaitText_ :: Bool -> Connection -> IO Text
awaitText_ = awaitQueue fi _connTextQueue _connDataQueue
  where
    fi (IncomingText t) = Right t
    fi (IncomingData d) = Left d

awaitByteString_ :: Bool -> Connection -> IO ByteString
awaitByteString_ = awaitQueue fi _connDataQueue _connTextQueue
  where
    fi (IncomingData d) = Right d
    fi (IncomingText t) = Left t

awaitData_ :: Binary a => Bool -> Connection -> IO a
awaitData_ queue conn = do
  bs <- awaitByteString_ queue conn
  case teaspoon (decode bs) of
    Just d  -> return d
    Nothing -> awaitData_ queue conn


awaitQueue ::
       (Incoming -> Either b a)
    -> (Connection -> MVar (Seq a))
    -> (Connection -> MVar (Seq b))
    -> (Bool -> Connection -> IO a)
awaitQueue fi refa refb queue conn = do
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
          when queue $ pushQueue (refb conn) y
          awaitQueue fi refa refb queue conn

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
