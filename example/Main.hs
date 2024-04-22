{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import Network.NineP.Monad
import Network.NineP.Server

fs :: FileSystem ()
fs = dir "/" $ do
  file "hello" [#owner := "jameshobson"]
    (Reader $ return "Hello, World!")

conf :: FSServerConf
conf = FSServerConf
  { bindAddr = UnixDomain "/tmp/example.sock"
  }

main :: IO ()
main = do
  putStrLn "Starting server"
  serveFileSystem conf fs
