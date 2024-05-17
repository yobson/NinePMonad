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
import Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as BL
import GHC.Int

import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger

data NPMsg r where
  RecvMsg    :: NPMsg N.Msg
  SendMsg    :: N.Msg -> NPMsg ()
  SetMsgSize :: Word32 -> NPMsg ()

makeEffect ''NPMsg

adapt :: (MonadIO m, LastMember m es, Members [Error NPError, State LocalState] es) => IO a -> Eff es a
adapt m = liftIO (try m) >>= \case
  Left (e::IOException) -> throwError . NPError $ show e
  Right v               -> return v

feedIncremental :: (Monad m) => Decoder a -> m BL.ByteString -> m (Decoder a)
feedIncremental decoder@(Partial _) feeder = do
  input <- feeder
  feedIncremental (pushChunks decoder input) feeder
feedIncremental decoder _ = return decoder

data LocalState = LState
  { leftOver :: BL.ByteString
  , msgSize  :: Int64
  }

initialState :: LocalState
initialState = LState BL.empty 4096

runMsgHandle 
  :: forall m es a . (MonadIO m, LastMember m (State LocalState : es), Members [Error NPError, State LocalState, Logger] es) => Socket -> Eff (NPMsg : es) a -> Eff es a
runMsgHandle sock = evalState initialState . reinterpret go
  where 
    go :: LastMember m (State LocalState : es) => NPMsg x -> Eff (State LocalState : es) x
    go RecvMsg = do
      residual <- gets leftOver
      size     <- gets msgSize
      let decoder = runGetIncremental (N.get @N.Msg) `pushChunks` residual
      result <- adapt @m $ feedIncremental decoder (recv sock size)
      case result of --TODO: Better Errors
        Fail {}      -> throwError $ NPError "msg decode error"
        Partial _    -> throwError $ NPError "Impossible state"
        Done res _ x -> modify (\s -> s{leftOver = BL.fromStrict res}) >> logProto x >> return x
    go (SendMsg msg) = do
      logProto msg
      adapt @m $ sendAll sock $ runPut (N.put msg)
    go (SetMsgSize w) = modify $ \s -> s{msgSize = fromIntegral w}
