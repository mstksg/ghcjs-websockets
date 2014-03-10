{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module JavaScript.WebSockets where

import Control.Applicative
import Unsafe.Coerce
import Control.Concurrent
import Control.Monad
import Data.Text.Encoding
import Control.Monad.IO.Class
import Data.ByteString.Lazy
import Data.Binary
import Data.Text                      (Text)
import GHCJS.Foreign
import GHCJS.Types
import JavaScript.Blob
import JavaScript.WebSockets.Internal

data NodeClient_

type NodeClient = JSRef NodeClient_

data Socket = Socket { socketConnection :: Connection
                     , socketQueue      :: SocketQueue
                     , socketWaiters    :: SocketWaiters
                     }


data SocketProcess a = ProcessExpect (ByteString -> SocketProcess a)
                     | ProcessSend Text (SocketProcess a)
                     | ProcessIO (IO (SocketProcess a))
                     | ProcessPure a

instance Monad SocketProcess where
    return    = ProcessPure
    (ProcessExpect e) >>= f = ProcessExpect $ \t -> e t >>= f
    (ProcessSend s p) >>= f = ProcessSend s $ p >>= f
    (ProcessIO io)    >>= f = ProcessIO     $ fmap (>>= f) io
    (ProcessPure a)   >>= f = f a

instance Applicative SocketProcess where
    pure = return
    (<*>) = ap

instance Functor SocketProcess where
    fmap f s = pure f <*> s

instance MonadIO SocketProcess where
    liftIO io = ProcessIO (return <$> io)

expectBS :: SocketProcess ByteString
expectBS = ProcessExpect return

expect :: Binary a => SocketProcess a
expect = ProcessExpect (return . decode)

send :: Text -> SocketProcess ()
send t = ProcessSend t (return ())

awaitMessage :: Socket -> IO ByteString
awaitMessage (Socket _ q w) = do
    msg <- js_await q w
    blb <- isBlob msg
    if blb
      then do
        let blob = unsafeCoerce msg
        fromStrict <$> readBlob blob
      else do
        let blob = unsafeCoerce msg :: JSString
        return (fromStrict . encodeUtf8 . fromJSString $ blob)


runSocketProcess :: Socket -> SocketProcess a -> IO a
runSocketProcess sock (ProcessExpect f) = do
    msg <- awaitMessage sock
    runSocketProcess sock (f msg)
runSocketProcess sock (ProcessSend s p) = do
    socketSend (socketConnection sock) (toJSString s)
    runSocketProcess sock p
runSocketProcess sock (ProcessIO io)    = io >>= runSocketProcess sock
runSocketProcess sock (ProcessPure x)   = return x

withConn :: Text -> SocketProcess () -> IO ()
withConn url process = do
    sockobj <- newSocket (toJSString url)

    conn <- getProp ("conn"::Text) sockobj
    queue <- getProp ("queue"::Text) sockobj
    waiters <- getProp ("waiters"::Text) sockobj
    let sock = Socket conn queue waiters

    runSocketProcess sock process

    closeSocket conn
    return ()
