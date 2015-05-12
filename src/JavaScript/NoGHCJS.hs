{-# OPTIONS_HADDOCK hide #-}

module JavaScript.NoGHCJS where

data JSArray a
data JSBool
data JSObject a
data JSRef a

type JSString = String

jsTrue :: a
jsTrue = error "jsTrue: only available in JavaScript"

setProp :: a
setProp = error "setProp: only available in JavaScript"

newObj :: a
newObj = error "newObj: only available in JavaScript"

toJSString :: a
toJSString = error "toJSString: only available in JavaScript"

fromArray :: a
fromArray = error "fromArray: only available in JavaScript"

getPropMaybe :: a
getPropMaybe = error "getPropMaybe: only available in JavaScript"

fromJSBool :: a
fromJSBool = error "fromJSBool: only available in JavaScript"

newArray :: a
newArray = error "newArray: only available in JavaScript"

jsNull :: a
jsNull = error "jsNull: only available in JavaScript"

fromJSString :: a
fromJSString = error "fromJSString: only available in JavaScript"

fromJSRef :: a
fromJSRef = error "fromJSRef: only available in JavaScript"

isNull :: a
isNull = error "isNull: only available in JavaScript"

bufferByteString :: Int -> Int -> a
bufferByteString = error "bufferByteString: only available in JavaScript"

