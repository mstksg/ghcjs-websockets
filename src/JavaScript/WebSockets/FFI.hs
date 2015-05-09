{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_HADDOCK hide #-}

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
  , ws_awaitConnClosed
  , ws_clearWaiters
  , ws_clearQueue
  , ws_handleOpen
  , ws_handleClose
  , ws_readyState
  , js_consolelog
  ) where

import GHCJS.Types               (JSRef, JSArray, JSString, JSObject)
import Data.Text                 (Text)

data Socket_
type Socket = JSRef Socket_

data Waiter_
type Waiter = JSRef Waiter_

type ConnectionQueue = JSArray Text
type ConnectionWaiters = JSArray Waiter

type WSCloseEvent = JSObject ()

foreign import javascript unsafe "$1.close();" ws_closeSocket :: Socket -> IO ()
foreign import javascript unsafe "$1.send(atob($2));" ws_socketSend :: Socket -> JSString -> IO ()

foreign import javascript interruptible "$1.onopen = function() { $c($1); };"
  ws_handleOpen :: Socket -> IO Socket

foreign import javascript unsafe  "var ws = new WebSocket($1);\
                                   ws.onmessage = function(e) {\
                                     if (!(typeof e === 'undefined')) {\
                                       if (window.ws_debug) {\
                                         console.log(e);\
                                       };\
                                       $2.push(e.data);\
                                       if ($3.length > 0) {\
                                         var w0 = $3.shift();\
                                         var e0 = $2.shift();\
                                         w0(e0);\
                                       }\
                                     }\
                                   };\
                                   $r = ws;"
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


foreign import javascript interruptible  "if ($1.length > 0) {\
                                            $c($1.shift());\
                                          } else {\
                                            $c(null);\
                                          }"
  ws_awaitConnClosed :: ConnectionQueue -> IO (JSRef ())

foreign import javascript unsafe "while ($1.length > 0) {\
                                    var w0 = $1.shift();\
                                    w0(null);\
                                  };"
  ws_clearWaiters :: ConnectionWaiters -> IO ()

foreign import javascript unsafe "while ($1.length > 0) { $1.shift(); };"
  ws_clearQueue :: ConnectionQueue -> IO ()

foreign import javascript interruptible  "$1.onclose = function (e) { $c(e); };"
  ws_handleClose :: Socket -> IO WSCloseEvent

foreign import javascript unsafe "$1.readyState"
  ws_readyState :: Socket -> IO Int

foreign import javascript unsafe "console.log($1)"
  js_consolelog :: JSRef a -> IO ()
