{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

module JavaScript.WebSockets.Internal where

import Data.Text       (Text)
import GHCJS.Types
import JavaScript.Blob

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
  js_await :: SocketQueue -> SocketWaiters -> IO (JSRef ())


