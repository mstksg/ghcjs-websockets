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
                                            $c(btoa(reader.result));\
                                          });\
                                          reader.readAsBinaryString($1);"
    ffi_readBlob :: Blob -> IO JSString

foreign import javascript unsafe "$1 instanceof Blob" ffi_blobCheck :: JSRef a -> IO Bool

readBlob :: Blob -> IO (Maybe ByteString)
readBlob b = either (const Nothing) Just . B64.decode . encodeUtf8 . fromJSString <$> ffi_readBlob b

isBlob :: JSRef a -> IO Bool
isBlob ref = ffi_blobCheck ref



