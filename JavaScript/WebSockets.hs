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
import Data.Typeable.FingerprintRep
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

expectMaybe :: Binary a => ConnectionProcess (Maybe a)
expectMaybe = do
  bs <- expectBS
  return (teaspoon $ decode bs)

expect :: Binary a => ConnectionProcess a
expect = do
  res <- expectMaybe
  case res of
    Just res' -> return res'
    Nothing   -> expect

expectAsType :: (Binary a, Typeable a) => ConnectionProcess a
expectAsType = do
  (fp,res) <- expect
  if fp == fingerprintRep res
    then return res
    else expectAsType

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


runConnectionProcess :: Connection -> ConnectionProcess a -> IO a
runConnectionProcess conn (ProcessExpect f) = do
    msg <- awaitMessage conn
    runConnectionProcess conn (f msg)
runConnectionProcess conn (ProcessSend s p) = do
    ws_socketSend (connSocket conn) (toJSString s)
    runConnectionProcess conn p
runConnectionProcess conn (ProcessIO io)    = io >>= runConnectionProcess conn
runConnectionProcess conn (ProcessPure x)   = return x

closeConnection :: Connection -> IO ()
closeConnection (Connection s _ _) = ws_closeSocket s

withConn :: Text -> ConnectionProcess () -> IO ()
withConn url process = do
    sockobj <- ws_newSocket (toJSString url)

    socket <- getProp ("conn"::Text) sockobj
    queue <- getProp ("queue"::Text) sockobj
    waiters <- getProp ("waiters"::Text) sockobj
    let conn = Connection socket queue waiters

    runConnectionProcess conn process

    closeConnection conn
    return ()
