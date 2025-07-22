{-# LANGUAGE DataKinds, TypeApplications, RankNTypes, TypeOperators, ScopedTypeVariables #-}

{-|
Module      : Network.NineP.Effects
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Portability : POSIX
-}

module Network.NineP.Effects
( module Network.NineP.Effects.Error
, module Network.NineP.Effects.Msg
, module Network.NineP.Effects.Logger
, module Network.NineP.Effects.GlobalState
, module Network.NineP.Effects.ClientState
, module Control.Monad.Freer
-- , App
-- , runApp
-- , runAppThrow
) where

import Control.Monad.Freer


import Network.NineP.Effects.Error
import Network.NineP.Effects.Msg
import Network.NineP.Effects.Logger
import Network.NineP.Effects.GlobalState
import Network.NineP.Effects.ClientState

-- type App m = Eff '[NPMsg, LocalState m, Error NPError, Logger, m]
-- 
-- runApp :: MonadIO m => Bool -> [LogLevel] -> Socket -> FileSystemT m () -> App m a -> m (Either NPError a)
-- runApp logP levels sock fs = runM
--                            . runFilterLogger levels logP
--                            . runError @NPError 
--                            . runLocalState fs
--                            . runMsgHandle sock
-- 
-- runAppThrow :: (MonadIO m, MonadFail m) => Bool -> [LogLevel] -> Socket -> FileSystemT m () -> App m a -> m a
-- runAppThrow logP levels sock fs app = runApp logP levels sock fs app >>= either (const $ fail "FAILED") return
