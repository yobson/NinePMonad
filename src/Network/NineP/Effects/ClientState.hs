{-# LANGUAGE GADTs, StrictData, TemplateHaskell, TypeOperators, DataKinds, TypeApplications #-}

{-|
Module      : Network.NineP.Effects.ClientState
Description : Effect for logging
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Portability : POSIX
-}

module Network.NineP.Effects.ClientState
( ClientState
, setFid
, setUName
, lookupFid
, runClientState
) where

import qualified Data.Map as Map
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State

import Data.Word

data ClState = CLState
  { fidMap :: Map.Map Word32 [FilePath]
  , uname  :: String
  }

initState :: ClState
initState = CLState
  { fidMap = Map.empty
  , uname = ""
  }

data ClientState r where
  SetFid :: Word32 -> [FilePath] -> ClientState ()
  SetUName :: String -> ClientState ()
  LookupFid :: Word32 -> ClientState [FilePath]
makeEffect ''ClientState

runClientState :: Eff (ClientState : es) a -> Eff es a
runClientState = evalState initState . reinterpret go
  where go :: ClientState x -> Eff (State ClState : es) x
        go (SetFid fid path) = modify (\s -> s {fidMap = Map.insert fid path $ fidMap s})
        go (SetUName n)      = modify (\s -> s { uname = n })
