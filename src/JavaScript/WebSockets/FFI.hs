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
  , WaiterKilled
    -- * FFI
  , ws_newSocket
  , ws_closeSocket
  , ws_socketSend
  , ws_awaitConn
  , ws_clearWaiters
  , ws_clearQueue
  , ws_handleOpen
  , ws_handleClose
  , ws_readyState
  ) where

import Data.Text   (Text)
import GHCJS.Types (JSRef, JSArray, JSString, JSObject, JSBool)

data Socket_
type Socket = JSRef Socket_

data Waiter_
type Waiter = JSRef Waiter_

type WaiterKilled = JSObject JSBool

type ConnectionQueue = JSArray Text
type ConnectionWaiters = JSArray Waiter

type WSCloseEvent = JSObject ()

foreign import javascript unsafe "$1.close();"
  ws_closeSocket :: Socket -> IO ()

foreign import javascript unsafe "$1.send(atob($2));"
  ws_socketSend :: Socket -> JSString -> IO ()

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
                                         var e0 = $2.shift();\
                                         var found = false;\
                                         while ($3.length > 0 && !found) {\
                                           var w0 = $3.shift();\
                                           found = w0(e0);\
                                         };\
                                         if (!found) {\
                                           $2.unshift(e0);\
                                         };\
                                       };\
                                     }\
                                   };\
                                   $r = ws;"
  ws_newSocket :: JSString -> ConnectionQueue -> ConnectionWaiters -> IO Socket

foreign import javascript interruptible  "if ($1.length > 0) {\
                                            var d = $1.shift();\
                                            $c(d);\
                                          } else {\
                                            $2.push(function(d) {\
                                              if ($3.k) {\
                                                return false;\
                                              } else {\
                                                $c(d);\
                                                return true;\
                                              };\
                                            });\
                                          }"
  ws_awaitConn :: ConnectionQueue -> ConnectionWaiters -> WaiterKilled -> IO (JSRef ())

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
