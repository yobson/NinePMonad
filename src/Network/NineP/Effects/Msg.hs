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
Module      : Network.NineP.Effects.Msg
Description : Effect for handling 9p messages over sockets
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}


module Network.NineP.Effects.Msg 
( NPMsg
, recvMsg
, sendMsg
, setMsgSize
, runMsgHandle
) where

import Data.Kind
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State
import Control.Monad.IO.Class
import qualified Data.NineP as N
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Control.Exception (IOException, try, throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Int

import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger

data NPMsg r where
  RecvMsg    :: NPMsg N.Msg
  SendMsg    :: N.Msg -> NPMsg ()
  SetMsgSize :: Word32 -> NPMsg ()

makeEffect ''NPMsg

adapt :: (MonadIO m, LastMember m es, Members [Error NPError, State (LocalState m)] es) => IO a -> Eff es a
adapt m = liftIO (try m) >>= \case
  Left (e::IOException) -> throwError . NPError $ show e
  Right v               -> return v

feedIncremental :: Decoder N.Msg -> IO BL.ByteString -> IO (N.Msg, B.ByteString)
feedIncremental decoder@(Partial _) feeder = do
  !input <- feeder
  feedIncremental (pushChunks decoder input) feeder
feedIncremental (Done res _ x) _ = return (x, res)
feedIncremental _ _ = fail "Failed to parse"

data LocalState (m :: Type -> Type) = LState
  { leftOver :: B.ByteString
  , msgSize  :: Int64
  }

initialState :: LocalState m
initialState = LState B.empty 4096

runMsgHandle 
  :: forall m es a 
  .  (MonadIO m, LastMember m es, Members [Error NPError, Logger] es) 
  => Socket -> Eff (NPMsg : es) a -> Eff es a
runMsgHandle sock = evalState initialState . reinterpret go
  where 
    go :: (LastMember m (State (LocalState m) : es)) => NPMsg x -> Eff (State (LocalState m) : es) x
    go RecvMsg = do
      -- residual <- gets @(LocalState m) leftOver
      size     <- gets @(LocalState m) msgSize
      -- let !decoder = runGetIncremental (N.get @N.Msg) `pushChunk` residual
      bytes <- liftIO $ recv sock size
      let dec = runGet N.get bytes
      logProto dec
      return dec
      -- (res, left) <- liftIO $ feedIncremental decoder (liftIO $ recv sock size)
      --modify @(LocalState m) (\s -> s {leftOver = left})
    go (SendMsg msg) = do
      logProto msg
      liftIO $ sendAll sock $ runPut (N.put msg)
    go (SetMsgSize w) = modify @(LocalState m) $ \s -> s{msgSize = fromIntegral w}
