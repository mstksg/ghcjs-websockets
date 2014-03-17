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
                                              if (window.ws_debug || true) {\
                                                console.log(e);\
                                              }\
                                              $2.push(e.data);\
                                              console.log($3.length);\
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

foreign import javascript interruptible  "console.log('awaiting');\
                                          if ($1.length > 0) {\
                                            console.log('taking');\
                                            var d = $1.shift();\
                                            $c(d);\
                                          } else {\
                                            console.log('pushing');\
                                            $2.push(function(d) {\
                                              console.log('calling');\
                                              $c(d);\
                                            });\
                                          }"
  ws_awaitConn :: ConnectionQueue -> ConnectionWaiters -> IO (JSRef ())

