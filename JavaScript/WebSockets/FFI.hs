{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

module JavaScript.WebSockets.FFI (
  -- * Types
    Socket
  , Waiter
  , ConnectionQueue
  , ConnectionWaiters
    -- * FFI
  , ws_newSocket
  , ws_closeSocket
  , ws_socketSend
  , ws_awaitConn
  , ws_clearWaiters
  ) where

import GHCJS.Types               (JSRef, JSArray, JSString)
import Data.Text                 (Text)

data Socket_
type Socket = JSRef Socket_

data Waiter_
type Waiter = JSRef Waiter_

type ConnectionQueue = JSArray Text
type ConnectionWaiters = JSArray Waiter

foreign import javascript unsafe "$1.close();" ws_closeSocket :: Socket -> IO ()
foreign import javascript unsafe "$1.send(atob($2))" ws_socketSend :: Socket -> JSString -> IO ()

foreign import javascript interruptible  "var ws = new WebSocket($1);\
                                          ws.onmessage = function(e) {\
                                            if (!(typeof e === 'undefined')) {\
                                              if (window.ws_debug) {\
                                                console.log(e);\
                                              }\
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

foreign import javascript unsafe "for (var i = $1.length-1; i >= 0; i--) {\
                                    $1[i](null);\
                                  }"
  ws_clearWaiters :: ConnectionWaiters -> IO ()
