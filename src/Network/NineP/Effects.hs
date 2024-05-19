{-# LANGUAGE DataKinds, TypeApplications, RankNTypes, TypeOperators, ScopedTypeVariables #-}

{-|
Module      : Network.NineP.Effects
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}

module Network.NineP.Effects
( module Network.NineP.Effects.Error
, module Network.NineP.Effects.Msg
, module Network.NineP.Effects.RunState
, module Network.NineP.Effects.Logger
, module Control.Monad.Freer
, App
, runApp
, runAppThrow
) where

import Control.Monad.Freer
import Control.Monad.IO.Class
import Network.Socket


import Network.NineP.Effects.Error
import Network.NineP.Effects.Msg
import Network.NineP.Effects.RunState
import Network.NineP.Effects.Logger
import Network.NineP.Monad

type App m = Eff '[NPMsg, LocalState m, Error NPError, Logger, m]

runApp :: MonadIO m => Bool -> [LogLevel] -> Socket -> FileSystemT m () -> App m a -> m (Either NPError a)
runApp logP levels sock fs = runM
                           . runFilterLogger levels logP
                           . runError @NPError 
                           . runLocalState fs
                           . runMsgHandle sock

runAppThrow :: (MonadIO m, MonadFail m) => Bool -> [LogLevel] -> Socket -> FileSystemT m () -> App m a -> m a
runAppThrow logP levels sock fs app = runApp logP levels sock fs app >>= either (const $ fail "FAILED") return
