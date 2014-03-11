{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Binary.Tagged (Tagged, tag, encodeTagged, getTagged, tagMatched) where

import Data.Binary
import Data.Typeable.Internal
import Data.ByteString.Lazy
import GHC.Generics
import Data.Maybe (isJust)

data Tagged a = Tagged Word64 Word64 a
                deriving (Show, Eq, Generic, Typeable)

instance Binary a => Binary (Tagged a)

tag :: (Binary a, Typeable a) => a -> Tagged a
tag x = Tagged fp1 fp2 x
  where
    TypeRep (Fingerprint fp1 fp2) _ _ = typeOf x

encodeTagged :: (Binary a, Typeable a) => a -> ByteString
encodeTagged = encode . tag

getTagged :: (Binary a, Typeable a) => Tagged a -> Maybe a
getTagged (Tagged tfp1 tfp2 x) | matched   = Just x
                               | otherwise = Nothing
  where
    matched = tfp1 == xfp1 && tfp2 == xfp2
    TypeRep (Fingerprint xfp1 xfp2) _ _ = typeOf x

tagMatched :: (Binary a, Typeable a) => Tagged a -> Bool
tagMatched = isJust . getTagged
