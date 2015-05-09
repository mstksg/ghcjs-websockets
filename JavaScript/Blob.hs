{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_HADDOCK hide #-}

module JavaScript.Blob (
    Blob
  , readBlob
  , isBlob
  ) where

import Data.ByteString
import GHCJS.Foreign
import GHCJS.Types


data Blob_
type Blob = JSRef Blob_

foreign import javascript interruptible  "var reader = new FileReader();\
                                          reader.addEventListener('loadend', function() {\
                                            $c(reader.result);\
                                          });\
                                          reader.readAsArrayBuffer($1);"
    ffi_readBlob :: Blob -> IO (JSRef a)

foreign import javascript unsafe "$1 instanceof Blob" ffi_blobCheck :: JSRef a -> IO Bool

readBlob :: Blob -> IO ByteString
readBlob b = bufferByteString 0 0 =<< ffi_readBlob b

isBlob :: JSRef a -> IO Bool
isBlob ref = ffi_blobCheck ref



