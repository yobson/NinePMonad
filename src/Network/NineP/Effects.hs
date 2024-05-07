{-# LANGUAGE DataKinds, TypeApplications, RankNTypes, TypeOperators #-}

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
, module Effectful
, App
, runApp
, runAppThrow
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Network.Socket


import Network.NineP.Effects.Error
import Network.NineP.Effects.Msg
import Network.NineP.Effects.RunState
import Network.NineP.Effects.Logger
import Network.NineP.Monad

type App n = Eff '[NPMsg, LocalState n, Error NPError, Logger, IOE]

type m ~> n = forall x . m x -> n x

runApp :: (MonadIO m, Monad n) => Bool -> [LogLevel] -> Socket -> FileSystemT n () -> (n ~> IO) -> App n a -> m (Either NPError a)
runApp logP levels sock fs hoist = liftIO . runEff
                                          . runFilterLogger levels logP
                                          . runErrorNoCallStack @NPError 
                                          . runLocalState fs hoist
                                          . runMsgHandle sock

runAppThrow :: (MonadIO m, MonadFail m, Monad n) => Bool -> [LogLevel] -> Socket -> FileSystemT n () -> (n ~> IO) -> App n a -> m a
runAppThrow logP levels sock fs hoist app = runApp logP levels sock fs hoist app >>= either (const $ fail "FAILED") return
