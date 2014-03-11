{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module JavaScript.WebSockets where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Spoon
import Data.Binary
import Data.ByteString.Lazy
import Data.Text                      (Text)
import Data.Text.Encoding
import Data.Typeable                  as DT
import Data.Typeable.Internal         as DT
import GHCJS.Foreign
import GHCJS.Types
-- import Data.Typeable.FingerprintRep
import Data.Binary.Tagged
import JavaScript.Blob
import JavaScript.WebSockets.Internal
import Unsafe.Coerce

data NodeClient_

type NodeClient = JSRef NodeClient_

data Connection = Connection { connSocket :: Socket
                             , connQueue      :: ConnectionQueue
                             , connWaiters    :: ConnectionWaiters
                             }


data ConnectionProcess a = ProcessExpect (ByteString -> ConnectionProcess a)
                         | ProcessSend Text (ConnectionProcess a)
                         | ProcessIO (IO (ConnectionProcess a))
                         | ProcessPure a

instance Monad ConnectionProcess where
    return    = ProcessPure
    (ProcessExpect e) >>= f = ProcessExpect $ \t -> e t >>= f
    (ProcessSend s p) >>= f = ProcessSend s $ p >>= f
    (ProcessIO io)    >>= f = ProcessIO     $ fmap (>>= f) io
    (ProcessPure a)   >>= f = f a

instance Applicative ConnectionProcess where
    pure = return
    (<*>) = ap

instance Functor ConnectionProcess where
    fmap f s = pure f <*> s

instance MonadIO ConnectionProcess where
    liftIO io = ProcessIO (return <$> io)

expectBS :: ConnectionProcess ByteString
expectBS = ProcessExpect return

expectEither :: Binary a => ConnectionProcess (Either ByteString a)
expectEither = do
  bs <- expectBS
  return $ maybe (Left bs) Right . teaspoon . decode $ bs

expectMaybe :: Binary a => ConnectionProcess (Maybe a)
expectMaybe = either (const Nothing) (Just) <$> expectEither

expect :: Binary a => ConnectionProcess a
expect = do
  res <- expectMaybe
  case res of
    Just res' -> return res'
    Nothing   -> expect

expectTagged :: (Binary a, Typeable a) => ConnectionProcess a
expectTagged = do
  tagged <- expect
  case getTagged tagged of
    Just x -> return x
    Nothing -> expectTagged

-- expectAsType :: (Binary a, Typeable a) => ConnectionProcess a
-- expectAsType = do
--   (fp,res) <- expect
--   if fp == fingerprintRep res
--     then return res
--     else expectAsType

send :: Text -> ConnectionProcess ()
send t = ProcessSend t (return ())

awaitMessage :: Connection -> IO ByteString
awaitMessage (Connection _ q w) = do
    msg <- ws_awaitConn q w
    blb <- isBlob msg
    if blb
      then do
        let blob = unsafeCoerce msg
        fromStrict <$> readBlob blob
      else do
        let blob = unsafeCoerce msg :: JSString
        return (fromStrict . encodeUtf8 . fromJSString $ blob)

withConn :: Connection -> ConnectionProcess a -> IO a
withConn conn (ProcessExpect f) = do
    msg <- awaitMessage conn
    withConn conn (f msg)
withConn conn (ProcessSend s p) = do
    ws_socketSend (connSocket conn) (toJSString s)
    withConn conn p
withConn conn (ProcessIO io)    = io >>= withConn conn
withConn conn (ProcessPure x)   = return x

closeConnection :: Connection -> IO ()
closeConnection (Connection s _ _) = ws_closeSocket s

openConnection :: Text -> IO Connection
openConnection url = do
    queue <- newArray
    waiters <- newArray
    socket <- ws_newSocket (toJSString url) queue waiters
    return $ Connection socket queue waiters

withUrl :: Text -> ConnectionProcess a -> IO a
withUrl url process = do
    conn <- openConnection url

    res <- withConn conn process

    closeConnection conn
    return res
