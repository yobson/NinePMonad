{-# LANGUAGE GADTs
           , DataKinds
           , TypeFamilies
           , TypeOperators
           , FlexibleContexts
           , LambdaCase
           , ScopedTypeVariables
           , TypeApplications
           , TemplateHaskell #-}


module Network.NineP.Effects.Msg 
( NPMsg
, recvMsg
, sendMsg
, setMsgSize
, runMsgHandle
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Effectful.State.Static.Local
import qualified Data.NineP as N
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Control.Monad.Catch (catch)
import Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import GHC.Int

import Network.NineP.Effects.Error

data NPMsg   :: Effect where
  RecvMsg    :: NPMsg m N.Msg
  SendMsg    :: N.Msg -> NPMsg m ()
  SetMsgSize :: Word32 -> NPMsg m ()

type instance DispatchOf NPMsg = 'Dynamic

makeEffect ''NPMsg

adapt :: (IOE :> es, Error NPError :> es, State LocalState :> es) => IO a -> Eff es a
adapt m = liftIO m
            `catch` \(e::IOException) -> throwError . NPError $ show e

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
  :: (HasCallStack, IOE :> es, Error NPError :> es) => Socket -> Eff (NPMsg : es) a -> Eff es a
runMsgHandle sock = reinterpret (evalState initialState) $ \_ -> \case
  RecvMsg -> do
    residual <- gets leftOver
    size     <- gets msgSize
    let decoder = runGetIncremental (N.get @N.Msg) `pushChunks` residual
    result <- adapt $ feedIncremental decoder (recv sock size)
    case result of --TODO: Better Errors
      Fail {}      -> throwError $ NPError "msg decode error"
      Partial _    -> throwError $ NPError "Impossible state"
      Done res _ x -> modify (\s -> s{leftOver = BL.fromStrict res}) >> return x
  SendMsg msg -> adapt $ sendAll sock $ runPut (N.put msg)
  SetMsgSize w -> modify $ \s -> s{msgSize = fromIntegral w}
