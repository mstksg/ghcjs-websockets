{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

module JavaScript.WebSockets.Internal where

import Data.Text       (Text)
import GHCJS.Types
import JavaScript.Blob

data Socket_
type Socket = JSRef Socket_

data Waiter_
type Waiter = JSRef Waiter_

type ConnectionQueue = JSArray Text
type ConnectionWaiters = JSArray Waiter

-- foreign import javascript unsafe "var ws = new WebSocket($1); ws.onmessage = function (e) { console.log(e); };" testSocket :: JSString -> IO ()

foreign import javascript unsafe "$1.close();" ws_closeSocket :: Socket -> IO ()
foreign import javascript unsafe "$1.send($2)" ws_socketSend :: Socket -> JSString -> IO ()

-- foreign import javascript interruptible "$1.onopen = function() { $c(); };" ws_socketWait :: Connection -> IO ()

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
  ws_newSocket :: JSString -> IO (JSRef qw)

foreign import javascript interruptible  "if ($1.length > 0) {\
                                            var d = $1.shift();\
                                            $c(d);\
                                          } else {\
                                            $2.push( function(d) {\
                                              $c(d);\
                                            });\
                                          }"
  ws_awaitConn :: ConnectionQueue -> ConnectionWaiters -> IO (JSRef ())


