{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_HADDOCK hide #-}

module JavaScript.Blob (
    Blob
  , readBlob
  , isBlob
  ) where

import Control.Exception (mask_)
import Data.ByteString   (ByteString)

#ifdef ghcjs_HOST_OS
import GHCJS.Foreign     (bufferByteString)
import GHCJS.Types       (JSRef)
#else
import JavaScript.NoGHCJS
#endif

data Blob_
type Blob = JSRef Blob_

#ifdef ghcjs_HOST_OS
foreign import javascript interruptible  "var reader = new FileReader();\
                                          reader.addEventListener('loadend', function() {\
                                            $c(reader.result);\
                                          });\
                                          reader.readAsArrayBuffer($1);"
  ffi_readBlob :: Blob -> IO (JSRef a)

foreign import javascript unsafe "$1 instanceof Blob"
  ffi_blobCheck :: JSRef a -> IO Bool
#else
ffi_readBlob :: Blob -> IO (JSRef a)
ffi_blobCheck :: JSRef a -> IO Bool

ffi_readBlob = error "ffi_readBlob: only available in JavaScript"
ffi_blobCheck = error "ffi_blobCheck: only available in JavaScript"
#endif

readBlob :: Blob -> IO ByteString
readBlob b = bufferByteString 0 0 =<< mask_ (ffi_readBlob b)

isBlob :: JSRef a -> IO Bool
isBlob = ffi_blobCheck
