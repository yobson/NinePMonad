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

hoist :: IORef Int -> StateT Int IO x -> IO x
hoist ref xm = do
  start <- readIORef ref
  (out, end) <- runStateT xm start
  writeIORef ref end
  return out

conf :: FSServerConf
conf = (defaultConf "/tmp/test")

main :: IO ()
main = do
  store <- newIORef 0
  hoistFileSystemServer conf (hoist store) fs
