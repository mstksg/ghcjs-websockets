{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
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
main = withConn "my-server" ptest

ptest :: ConnectionProcess ()
ptest = do
  k <- (*2) <$> return 14
  liftIO $ print k
  -- e <- expect
  -- liftIO $ T.putStrLn (T.decodeUtf8 e)
  send "hello again"
  send "how are you"
  send "does this work?"
  send "yes!"
  forever $ do
    liftIO $ putStrLn "waiting for input:"
    e <- expect :: ConnectionProcess (BTree Int)
    -- e <- expectBS
    liftIO $ print e
    -- liftIO $ print (L.length e)
    -- liftIO $ T.putStrLn (T.decodeUtf8 e)
  return ()


