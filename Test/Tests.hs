{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar          (newEmptyMVar, takeMVar)
import Control.Monad
import Data.Binary.Tagged
import JavaScript.WebSockets
import JavaScript.WebSockets.Internal
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

main :: IO ()
main = do
  print $ (B64.encode (encode (1 :: Int)))
  conn <- openConnection "ws://home.jle0.com:4270"
  block <- newEmptyMVar
  -- receiveText conn
  -- receiveText conn
  forkIO $ runningSum 0 conn
  forkIO . forever $ echo conn
  -- forkIO . forever $ do
  --   threadDelay 2000000
  --   print . second (fmap B64.encode) =<< viewQueues conn
  takeMVar block

runningSum :: Int -> Connection -> IO ()
runningSum n conn = do
  putStrLn "waiting for number"
  i <- receiveData conn
  print (n + i)
  -- print . second (fmap B64.encode) =<< viewQueues conn
  runningSum (n + i) conn

echo :: Connection -> IO ()
echo conn = do
  threadDelay 500000
  putStrLn "waiting for text"
  t <- receiveText conn
  print t
  -- print . second (fmap B64.encode) =<< viewQueues conn


-- main :: IO ()
-- main = withUrl "ws://home.jle0.com:4270" $ \conn -> forever $ do
--   t <- receive conn
--   T.putStrLn t


  -- putStrLn "Hello world!"
  -- c <- openConnection "ws://home.jle0.com:4270"
  -- send c (encodeTagged ("hello" :: String))
  -- forever $ do
  --   t <- receive c
  --   T.putStrLn t


