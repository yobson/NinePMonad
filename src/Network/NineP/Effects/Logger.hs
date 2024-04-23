{-# LANGUAGE GADTs
           , DataKinds
           , TypeFamilies
           , TypeOperators
           , FlexibleContexts
           , LambdaCase
           , ScopedTypeVariables
           , TypeApplications
           , TemplateHaskell #-}

module Network.NineP.Effects.Logger
( Logger
, LogLevel(..)
, logMsg
, logProto
, runStdLogger
, runSilentLogger
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import qualified Data.NineP as N

data LogLevel = Info
              | Warning
              | Fatal

data Logger :: Effect where
  LogMsg :: LogLevel -> String -> Logger m ()
  LogProto :: N.Msg -> Logger m ()

type instance DispatchOf Logger = 'Dynamic

makeEffect ''Logger

runStdLogger :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runStdLogger = interpret $ \_ -> \case
  LogMsg _ msg -> liftIO $ putStrLn msg
  LogProto msg -> liftIO $ print msg


runSilentLogger :: Eff (Logger : es) a -> Eff es a
runSilentLogger = interpret $ \_ -> \case
  LogMsg _ _ -> return ()
  LogProto _ -> return ()
