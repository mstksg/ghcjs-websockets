{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Tagged
import Data.Char
import Data.Typeable
import GHC.Generics
import JavaScript.WebSockets
import Numeric
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Typeable, Generic, Functor)

toList :: BTree a -> [a]
toList Empty = []
toList (Node x t1 t2) = toList t1 ++ [x] ++ toList t2

instance Binary a => Binary (BTree a)

pBin :: L.ByteString -> IO ()
pBin = putStrLn . concatMap (flip (showIntAtBase 2 intToDigit) "") . L.unpack


main :: IO ()
main = do
  -- let x = encodeTagged (Empty :: BTree Int)
  -- print (typeFingerprint (Empty :: BTree Int))
  -- pBin x
  -- print (bsFingerprint x)
  -- print (decodeTagged x :: Maybe (BTree Int))
  -- pBin (encodeTagged (Empty :: BTree String))


  -- print (typeFingerprint (Empty :: BTree Int))

  c <- openTaggedConnection "ws://home.jle0.com:4270"
  -- withConn c ptest
  block <- newEmptyMVar
  withConn c $ do
    forkProcess . forever $ do
      msg <- expectText
      liftIO $ putStrLn "saw text"
      liftIO $ T.putStrLn msg
    -- forkProcess . forever $ do
      -- bs <- expectBS
      -- liftIO . pBin $ bs
      -- liftIO . print $ bsFingerprint bs
    forkProcess . forever $ do
      strs <- replicateM 2 expectTagged :: ConnectionProcess [BTree String]
      let msg = "String trees sum to " ++ show (concat (concatMap toList strs))
      liftIO $ putStrLn msg
      sendText (T.pack msg)
    forkProcess . forever $ do
      ints <- replicateM 3 expectTagged :: ConnectionProcess [BTree Int]
      let msg = "Integer trees concat to " ++ show (sum (concatMap toList ints))
      liftIO $ putStrLn msg
      sendText (T.pack msg)
  takeMVar block
  return ()


ptest :: ConnectionProcess ()
ptest = do
  k <- (*2) <$> return 14
  liftIO $ print k
  e <- expect
  liftIO $ T.putStrLn (T.decodeUtf8 e)
  sendText "hello again"
  sendText "how are you"
  sendText "does this work?"
  sendText "yes!"
  -- forever $ do
  --   liftIO $ putStrLn "waiting for input:"
  --   e  <- expect :: ConnectionProcess (BTree Int)
  --   t1 <- expectTagged
  --   liftIO $ print (t1 :: BTree Int)
  --   t2 <- expectTagged
  --   liftIO $ print (t2 :: BTree Int)
  --   liftIO $ putStrLn "waiting showed"
  --   t3 <- expectTagged
  --   liftIO $ print (t3 :: BTree String)
  return ()


