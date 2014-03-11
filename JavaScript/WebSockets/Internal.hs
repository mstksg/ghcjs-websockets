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

foreign import javascript interruptible  "var ws = new WebSocket($1);\
                                          ws.onmessage = function(e) {\
                                            console.log(e);\
                                            if (!(typeof e === 'undefined')) {\
                                              $2.push(e.data);\
                                              if ($3.length > 0) {\
                                                var w0 = $3.shift();\
                                                var e0 = $2.shift();\
                                                w0(e0);\
                                              }\
                                            }\
                                          };\
                                          ws.onopen = function() {\
                                            $c(ws);\
                                          };"
  ws_newSocket :: JSString -> ConnectionQueue -> ConnectionWaiters -> IO Socket

foreign import javascript interruptible  "if ($1.length > 0) {\
                                            var d = $1.shift();\
                                            $c(d);\
                                          } else {\
                                            $2.push(function(d) {\
                                              $c(d);\
                                            });\
                                          }"
  ws_awaitConn :: ConnectionQueue -> ConnectionWaiters -> IO (JSRef ())

foreign import javascript unsafe "if ($2.length > 0) {\
                                    var w0 = w.shift();\
                                    w0($3);\
                                  } else {\
                                    $1.unshift($3);\
                                  }"
  ws_requeue :: ConnectionQueue -> ConnectionWaiters -> JSString -> IO ()

-- foreign import javascript unsafe "$1.shift();"
--   js_shift :: JSArray a -> IO (JSRef a)

-- foreign import javascript interruptible "function(x) { $c(x); };"
--   js_await :: IO (JSRef a)
