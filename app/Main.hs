{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Main where

import Network.NineP
import Control.Monad.State
import Data.IORef

fRead :: MonadState Int m => m String
fRead = do
  num <- get
  modify (+1)
  return $ "The count is " <> show num <> "\n"


fs :: FileSystemT (StateT Int IO) ()
fs = dir "/" $ do
      file "count" (StringR fRead)
      file "talk" [#perms := 0o777] (StringW $ liftIO . putStrLn)

hoist :: IORef Int -> StateT Int IO x -> IO x
hoist ref xm = do
  start <- readIORef ref
  (out, end) <- runStateT xm start
  writeIORef ref end
  return out

conf :: FSServerConf
conf = (defaultConf "tcp!0.0.0.0!8080") { logLevels = [Protocol, Warning, Info]}

main :: IO ()
main = do
  store <- newIORef 0
  hoistFileSystemServer conf (hoist store) fs
