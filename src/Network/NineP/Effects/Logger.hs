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
Copyright   : (c) James Hobson, 2025
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

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import qualified Data.NineP as N

-- | Indication of how to feel when reading a log message
data LogLevel = Info    -- ^ Happy
              | Warning -- ^ Melancholy
              | Fatal   -- ^ Upset
              | Protocol
              deriving (Eq)

instance Show LogLevel where
  show Info     = "INFO"
  show Warning  = "WARN"
  show Fatal    = "FATAL"
  show Protocol = "PROTO"

data Logger r where
  LogMsg :: LogLevel -> String -> Logger ()
  LogProto :: N.Msg -> Logger ()

makeEffect ''Logger

runStdLogger :: forall m es a . (MonadIO m, LastMember m es) => Eff (Logger : es) a -> Eff es a
runStdLogger = interpretM go
  where go :: Logger x -> m x
        go (LogMsg l msg) = printLog l msg
        go (LogProto msg) = liftIO $ print msg


runSilentLogger :: Eff (Logger : es) a -> Eff es a
runSilentLogger = interpret $ \case
  LogMsg _ _ -> return ()
  LogProto _ -> return ()

runFilterLogger :: (MonadIO m, LastMember m es) => [LogLevel] -> Eff (Logger : es) a -> Eff es a
runFilterLogger levels = interpretM $ \case
  LogMsg level msg -> when (level `elem` levels) $ printLog level msg
  LogProto     msg -> when (Protocol `elem` levels) $ printLog Protocol $ show msg


printLog :: (MonadIO m) => LogLevel -> String -> m ()
printLog l msg = liftIO $ do
  now <- getZonedTime
  liftIO $ putStr "["
  putStr $ formatTime defaultTimeLocale "%F %k:%M:%S %q" now
  putStr " | "
  putStr $ show l
  putStr "]\t"
  putStrLn msg
