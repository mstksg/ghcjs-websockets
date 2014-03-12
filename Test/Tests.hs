{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Binary.Tagged
import Control.Monad.IO.Class
import Data.Binary
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import JavaScript.WebSockets
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Typeable, Generic)

instance Binary a => Binary (BTree a)

main :: IO ()
main = withUrl "your-server-here" ptest

ptest :: ConnectionProcess ()
ptest = do
  liftIO $ print (typeFingerprint (Empty :: BTree Int))
  k <- (*2) <$> return 14
  liftIO $ print k
  e <- expect
  liftIO $ T.putStrLn (T.decodeUtf8 e)
  send "hello again"
  send "how are you"
  send "does this work?"
  send "yes!"
  forever $ do
    liftIO $ putStrLn "waiting for input:"
    e  <- expect :: ConnectionProcess (BTree Int)
    t1 <- expectTagged
    liftIO $ print (t1 :: BTree Int)
    t2 <- expectTagged
    liftIO $ print (t2 :: BTree Int)
    liftIO $ putStrLn "waiting showed"
    t3 <- expectTagged
    liftIO $ print (t3 :: BTree String)
  return ()


