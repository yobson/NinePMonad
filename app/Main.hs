{-# LANGUAGE OverloadedLabels #-}

module Main where

import Network.NineP.Monad
import Control.Monad.State
import Network.NineP.Effects.GlobalState


fs :: FileSystem ()
fs = dir "/" $ do
      file "Alexis"

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
