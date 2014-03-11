{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Typeable.FingerprintRep (fingerprintRep, FingerprintRep) where

import Data.Word
import Data.Binary
import Data.Typeable.Internal
import GHC.Generics

data FingerprintRep = FingerprintRep !Word64 !Word64 deriving (Show, Typeable, Generic, Eq)

instance Binary FingerprintRep

fingerprintRep :: Typeable a => a -> FingerprintRep
fingerprintRep a = FingerprintRep fp1 fp2
  where
    TypeRep (Fingerprint fp1 fp2) _ _ = typeOf a
