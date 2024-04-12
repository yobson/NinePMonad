{-# LANGUAGE DataKinds, TypeApplications, RankNTypes, TypeOperators #-}


module Network.NineP.Effects
( module Network.NineP.Effects.Error
, module Network.NineP.Effects.Msg
, module Network.NineP.Effects.RunState
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
import Network.NineP.Monad

type App n = Eff '[NPMsg, LocalState n, Error NPError, IOE]

type m ~> n = forall x . m x -> n x

runApp :: (MonadIO m, Monad n) => Socket -> FileSystemT n () -> (n ~> IO) -> App n a -> m (Either NPError a)
runApp sock fs hoist = liftIO . runEff . runErrorNoCallStack @NPError . runLocalState fs hoist . runMsgHandle sock

runAppThrow :: (MonadIO m, MonadFail m, Monad n) => Socket -> FileSystemT n () -> (n ~> IO) -> App n a -> m a
runAppThrow sock fs hoist app = runApp sock fs hoist app >>= either (const $ fail "") return
