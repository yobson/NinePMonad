{-# LANGUAGE GADTs
           , DataKinds
           , TypeFamilies
           , TypeOperators
           , FlexibleContexts
           , LambdaCase
           , ScopedTypeVariables
           , TypeApplications
           , TemplateHaskell #-}

{-|
Module      : Network.NineP.Effects.Logger
Description : Effect for logging
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}

module Network.NineP.Effects.Logger
( Logger
, LogLevel(..)
, logMsg
, logProto
, runStdLogger
, runSilentLogger
, runFilterLogger
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Control.Monad
import qualified Data.NineP as N

-- | Indication of how to feel when reading a log message
data LogLevel = Info    -- ^ Happy
              | Warning -- ^ Melancholy
              | Fatal   -- ^ Upset
              deriving (Eq, Show)

data Logger :: Effect where
  LogMsg :: LogLevel -> String -> Logger m ()
  LogProto :: N.Msg -> Logger m ()

type instance DispatchOf Logger = 'Dynamic

makeEffect ''Logger

runStdLogger :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runStdLogger = interpret $ \_ -> \case
  LogMsg l msg -> printLog l msg
  LogProto msg -> liftIO $ print msg


runSilentLogger :: Eff (Logger : es) a -> Eff es a
runSilentLogger = interpret $ \_ -> \case
  LogMsg _ _ -> return ()
  LogProto _ -> return ()

runFilterLogger :: (IOE :> es) => [LogLevel] -> Bool -> Eff (Logger : es) a -> Eff es a
runFilterLogger levels proto = interpret $ \_ -> \case
  LogMsg level msg -> when (level `elem` levels) $ printLog level msg
  LogProto msg -> when proto $ liftIO $ print msg


printLog :: (MonadIO m) => LogLevel -> String -> m ()
printLog l msg = liftIO $ putStrLn $ concat ["[", show l, "] ", msg]
