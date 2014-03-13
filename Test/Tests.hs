{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Binary.Tagged
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Binary
import Data.Typeable
import Control.Concurrent.MVar
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import JavaScript.WebSockets
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Typeable, Generic, Functor)

toList :: BTree a -> [a]
toList Empty = []
toList (Node x t1 t2) = toList t1 ++ [x] ++ toList t2

instance Binary a => Binary (BTree a)

main :: IO ()
main = do
  c <- openTaggedConnection "my-server"
  withConn c ptest
  ts@[t1,t2,t3] <- replicateM 3 newEmptyMVar
  forkIO . withConn c . forever $ do
    strs <- replicateM 2 expectTagged :: ConnectionProcess [BTree String]
    let msg = "String trees sum to " ++ show (concat (concatMap toList strs))
    liftIO $ putStrLn msg
    sendText (T.pack msg)
  forkIO . withConn c . forever $ do
    ints <- replicateM 3 expectTagged :: ConnectionProcess [BTree Int]
    let msg = "Integer trees sum to " ++ show (sum (concatMap toList ints))
    liftIO $ putStrLn msg
    sendText (T.pack msg)
  -- forkIO . withConn c . forever $ do
  --   t <- expectText
  --   liftIO $ T.putStrLn t
  mapM_ takeMVar ts
  return ()


ptest :: ConnectionProcess ()
ptest = do
  liftIO $ print (typeFingerprint (Empty :: BTree Int))
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


