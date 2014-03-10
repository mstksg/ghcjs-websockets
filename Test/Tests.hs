{-# LANGUAGE OverloadedStrings #-}

module Main where

import JavaScript.WebSockets
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.Text.IO as T

main :: IO ()
main = withConn "my-test-server" ptest

ptest :: SocketProcess ()
ptest = do
  k <- (*2) <$> return 14
  liftIO $ print k
  e <- expect
  liftIO $ T.putStrLn e
  send "hello again"
  send "how are you"
  send "does this work?"
  send "yes!"
  forever $ do
    liftIO $ putStrLn "waiting for input:"
    e <- expect
    liftIO $ T.putStrLn e
  return ()


