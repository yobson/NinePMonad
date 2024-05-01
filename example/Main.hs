{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import Network.NineP.Monad
import Network.NineP.Server
import Control.Monad.IO.Class

fs :: (MonadIO m) => FileSystemT m ()
fs = dir "/" $ do
  file "hello" [#owner := "jameshobson"]
    (Reader $ return "Hello, World!")
    (Writer $ \_ -> return ())

conf :: FSServerConf
conf = FSServerConf
  { bindAddr = "unix!/tmp/example.sock"
  , debugLogs = True
  }

main :: IO ()
main = do
  putStrLn "Starting server"
  serveFileSystem conf fs
