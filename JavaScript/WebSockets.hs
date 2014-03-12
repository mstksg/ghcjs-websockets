{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JavaScript.WebSockets where

import Control.Applicative
import Control.Concurrent
import Data.Default
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.IO.Class
import Control.Spoon
import Data.Binary
import Data.Binary.Tagged
import Data.ByteString.Lazy
import Data.IORef
import Data.Map.Strict                (Map)
import Data.Sequence                  as S
import Data.Text                      (Text)
import Data.Text.Encoding
import Data.Typeable                  as DT
import Debug.Trace
import GHCJS.Foreign
import GHCJS.Types
import JavaScript.Blob
import JavaScript.WebSockets.Internal
import Prelude                        as P
import Unsafe.Coerce
import qualified Data.Map.Strict      as M

data Connection = Connection { connSocket     :: Socket
                             , connQueue      :: ConnectionQueue
                             , connWaiters    :: ConnectionWaiters
                             , connTypeQueues :: IORef (Map TagFingerprint (Seq ByteString))
                             }


data ConnectionProcess a = ProcessExpect (ByteString -> ConnectionProcess a)
                         | ProcessSend Text (ConnectionProcess a)
                         | ProcessRead (Connection -> ConnectionProcess a)
                         | ProcessIO (IO (ConnectionProcess a))
                         | ProcessPure a

instance Monad ConnectionProcess where
    return    = ProcessPure
    (ProcessExpect e)   >>= f = ProcessExpect $ \t -> e t >>= f
    (ProcessSend s p)   >>= f = ProcessSend s $ p >>= f
    (ProcessRead p)     >>= f = ProcessRead   $ \c -> p c >>= f
    (ProcessIO io)      >>= f = ProcessIO     $ fmap (>>= f) io
    (ProcessPure a)     >>= f = f a

instance Applicative ConnectionProcess where
    pure = return
    (<*>) = ap

instance Functor ConnectionProcess where
    fmap f s = pure f <*> s

instance MonadIO ConnectionProcess where
    liftIO io = ProcessIO (return <$> io)

withConn :: Connection -> ConnectionProcess a -> IO a
withConn conn (ProcessExpect p)  = do
    msg <- awaitMessage conn
    withConn conn (p msg)
withConn conn (ProcessSend s p)  = do
    ws_socketSend (connSocket conn) (toJSString s)
    withConn conn p
withConn conn (ProcessRead p)    = withConn conn (p conn)
withConn conn (ProcessIO io)     = io >>= withConn conn
withConn conn (ProcessPure x)    = return x

expectBS :: ConnectionProcess ByteString
expectBS = ProcessExpect return

expectEither :: Binary a => ConnectionProcess (Either ByteString a)
expectEither = do
  bs <- expectBS
  return $ maybe (Left bs) Right . teaspoon . decode $ bs

expectMaybe :: Binary a => ConnectionProcess (Maybe a)
expectMaybe = do
  expected <- expectEither
  case expected of
    Right x -> return (Just x)
    Left bs -> do
      let fpIn = fromMaybe def (bsFingerprint bs)
      queueUpFp fpIn bs
      return Nothing

expect :: Binary a => ConnectionProcess a
expect = do
  res <- expectMaybe
  case res of
    Just res' -> return res'
    Nothing   -> expect

expectTagged :: forall a. (Binary a, Typeable a) => ConnectionProcess a
expectTagged = do
  -- check queue first
  let fp = typeFingerprint (undefined :: a)
  queued <- popQueue fp
  case queued of
    -- something is there!
    Just q  ->
      case decodeTagged q of
        Just a -> return a
        Nothing -> error "Unable to decode tagged ByteString"
    -- otherwise...
    Nothing -> do
      bs <- expectBS
      let fpIn = fromMaybe def (bsFingerprint bs)
      if fpIn == fp
        then
          case decodeTagged bs of
            Just a  -> return a
            Nothing -> error "Unable to decode tagged ByteString."
        else do
          queueUpFp fpIn bs
          expectTagged

send :: Text -> ConnectionProcess ()
send t = ProcessSend t (return ())

selfConn :: ConnectionProcess Connection
selfConn = ProcessRead return

popQueue :: TagFingerprint -> ConnectionProcess (Maybe ByteString)
popQueue fp = do
  tqsref <- connTypeQueues <$> selfConn
  tq <- M.lookup fp <$> liftIO (readIORef tqsref)
  case tq of
    Nothing  -> return Nothing
    Just tqseq -> do
      case viewl tqseq of
        EmptyL -> return Nothing
        a :< rest -> do
          liftIO $ modifyIORef' tqsref (M.insert fp rest)
          return (Just a)

queueUpFp :: TagFingerprint -> ByteString -> ConnectionProcess ()
queueUpFp fp bs = do
    tqsref <- connTypeQueues <$> selfConn
    liftIO $ modifyIORef' tqsref (M.insertWith f fp (S.singleton bs))
  where
    f = flip (><)


awaitMessage :: Connection -> IO ByteString
awaitMessage (Connection _ q w _) = do
    msg <- ws_awaitConn q w
    blb <- isBlob msg
    if blb
      then do
        let blob = unsafeCoerce msg
        fromStrict <$> readBlob blob
      else do
        let blob = unsafeCoerce msg :: JSString
        return (fromStrict . encodeUtf8 . fromJSString $ blob)

closeConnection :: Connection -> IO ()
closeConnection (Connection s _ _ _) = ws_closeSocket s

openConnection :: Text -> IO Connection
openConnection url = do
    queue <- newArray
    waiters <- newArray
    socket <- ws_newSocket (toJSString url) queue waiters
    tqs <- newIORef M.empty
    return $ Connection socket queue waiters tqs

withUrl :: Text -> ConnectionProcess a -> IO a
withUrl url process = do
    conn <- openConnection url

    res <- withConn conn process

    closeConnection conn
    return res
