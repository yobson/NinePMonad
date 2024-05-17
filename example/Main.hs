{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import Network.NineP.Monad
import Network.NineP.Server
import Control.Monad.IO.Class

fs :: FileSystem ()
fs = dir "/" $ return ()

conf :: FSServerConf
conf = FSServerConf
  { bindAddr  = "unix!/tmp/example.sock"
  , logProt   = True
  , logLevels = []
  }

main :: IO ()
main = do
  putStrLn "Starting server"
  serveFileSystem conf fs
