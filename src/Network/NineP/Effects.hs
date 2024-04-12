{-# LANGUAGE DataKinds, TypeApplications #-}


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

type App = Eff '[NPMsg, LocalState, Error NPError, IOE]

runApp :: (MonadIO m) => Socket -> App a -> m (Either NPError a)
runApp sock = liftIO . runEff . runErrorNoCallStack @NPError . runLocalState . runMsgHandle sock

runAppThrow :: (MonadIO m, MonadFail m) => Socket -> App a -> m a
runAppThrow sock app = runApp sock app >>= either (const $ fail "") return
