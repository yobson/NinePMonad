{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import Network.NineP.Monad
import Network.NineP.Server

fs :: FileSystem ()
fs = dir "/" $ do
  file "foo" [#perms := 0o777]
  file "bar"

conf :: FSServerConf
conf = FSServerConf
  { bindAddr  = "unix!/tmp/example.sock"
  , logProt   = True
  , logLevels = [Warning]
  }

main :: IO ()
main = do
  putStrLn "Starting server"
  serveFileSystem conf fs
