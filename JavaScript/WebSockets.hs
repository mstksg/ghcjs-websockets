{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module JavaScript.WebSockets where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text               (Text)
import GHCJS.Foreign
import GHCJS.Types
import qualified GHCJS.Foreign as F

data NodeClient_

type NodeClient = JSRef NodeClient_

data Socket = Socket { socketConnection :: Connection
                     , socketQueue      :: SocketQueue
                     , socketWaiters    :: SocketWaiters
                     }

data Connection_
type Connection = JSRef Connection_

data Waiter_
type Waiter = JSRef Waiter_

type SocketQueue = JSArray Text
type SocketWaiters = JSArray Waiter

foreign import javascript unsafe "var ws = new WebSocket($1); ws.onmessage = function (e) { console.log(e); };" testSocket :: JSString -> IO ()

foreign import javascript unsafe "$1.close();" closeSocket :: Connection -> IO ()
foreign import javascript unsafe "$1.send($2)" socketSend :: Connection -> JSString -> IO ()
foreign import javascript interruptible "$1.onopen = function() { $c(); };" socketWait :: Connection -> IO ()

foreign import javascript interruptible  "var q = [];\
                                          var w = [];\
                                          var ws = new WebSocket($1);\
                                          ws.onmessage = function(e) {\
                                            console.log(e);\
                                            if (!(typeof e === 'undefined')) {\
                                              if (w.length > 0) {\
                                                var w0 = w.shift();\
                                                w0(e.data);\
                                              } else {\
                                                q.push(e.data);\
                                              }\
                                            }\
                                          };\
                                          ws.onopen = function() {\
                                            $c({ conn: ws, queue: q, waiters: w });\
                                          };"
  newSocket :: JSString -> IO (JSRef qw)

foreign import javascript interruptible  "if ($1.length > 0) {\
                                            var d = $1.shift();\
                                            $c(d);\
                                          } else {\
                                            $2.push( function(d) {\
                                              $c(d);\
                                            });\
                                          }"
  awaitMessage :: SocketQueue -> SocketWaiters -> IO JSString

data SocketProcess a = ProcessExpect (Text -> SocketProcess a)
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

expect :: SocketProcess Text
expect = ProcessExpect ProcessPure

send :: Text -> SocketProcess ()
send t = ProcessSend t (return ())

runSocketProcess :: Socket -> SocketProcess a -> IO a
runSocketProcess sock (ProcessExpect f) = do
    msg <- fromJSString <$> awaitMessage (socketQueue sock) (socketWaiters sock)
    -- threadDelay 1000000
    runSocketProcess sock (f msg)
runSocketProcess sock (ProcessSend s p) = do
    socketSend (socketConnection sock) (toJSString s)
    runSocketProcess sock p
runSocketProcess sock (ProcessIO io)    = io >>= runSocketProcess sock
runSocketProcess sock (ProcessPure x)   = return x

withConn :: Text -> SocketProcess () -> IO ()
withConn url process = do
    sockobj <- newSocket (toJSString url)

    conn <- F.getProp ("conn"::Text) sockobj
    queue <- F.getProp ("queue"::Text) sockobj
    waiters <- F.getProp ("waiters"::Text) sockobj
    let sock = Socket conn queue waiters

    runSocketProcess sock process

    closeSocket conn
    return ()
