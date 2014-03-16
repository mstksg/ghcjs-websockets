{-# LANGUAGE OverloadedStrings #-}

module Main where

import JavaScript.WebSockets
import qualified Data.Text    as T
import Control.Monad
import Data.Binary.Tagged
import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn "Hello world!"
  c <- openConnection "ws://home.jle0.com:4270"
  send c (encodeTagged ("hello" :: String))
  forever $ do
    t <- receiveData c
    T.putStrLn t


