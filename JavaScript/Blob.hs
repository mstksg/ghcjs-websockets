{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}

module JavaScript.Blob where

import GHCJS.Types
import Control.Applicative ((<$>))
import GHCJS.Foreign
import Data.ByteString
import Data.Text.Encoding
import qualified Data.ByteString.Base64 as B64


data Blob_
type Blob = JSRef Blob_

data FileReader_
type FileReader = JSRef FileReader_


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



