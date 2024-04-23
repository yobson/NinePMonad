{-# LANGUAGE DataKinds, TypeApplications, RankNTypes, TypeOperators #-}


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

runApp :: (MonadIO m, Monad n) => Bool -> Socket -> FileSystemT n () -> (n ~> IO) -> App n a -> m (Either NPError a)
runApp l sock fs hoist = liftIO . runEff . (if l then runStdLogger else runSilentLogger)
                                         . runErrorNoCallStack @NPError 
                                         . runLocalState fs hoist
                                         . runMsgHandle sock

runAppThrow :: (MonadIO m, MonadFail m, Monad n) => Bool -> Socket -> FileSystemT n () -> (n ~> IO) -> App n a -> m a
runAppThrow l sock fs hoist app = runApp l sock fs hoist app >>= either (const $ fail "FAILED") return
