{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import Network.NineP.Monad
import Network.NineP.Server

fs :: FileSystem ()
fs = dir "/" $ do
  file "welcome" [#perms := 0o664, #owner := "jameshobson"]
    (Reader (return "Welcome to my haskell 9p library"))
    (Writer (\_ -> return ()))
  file "bye!" $
    Reader $ return "Bye!"

conf :: FSServerConf
conf = FSServerConf
  { bindAddr = UnixDomain "/tmp/example.sock"
  }

main :: IO ()
main = do
  putStrLn "Starting server"
  serveFileSystem conf fs
