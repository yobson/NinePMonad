{-# LANGUAGE 
    GADTs
  , DataKinds
  , TypeFamilies
  , TypeOperators
  , FlexibleContexts
  , LambdaCase
  , ScopedTypeVariables
  , TypeApplications
  , TemplateHaskell
#-}

module Network.NineP.Effects.RunState
( LocalState
, runLocalState
, setName
, getRoot
, insertFid
, getQid
, getFid
, getName
, openFile
, getOpenFile
, execFOP
) where


import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Effectful.State.Static.Local
import Data.Word
import Data.NineP
-- import qualified Data.Map as Map

import Network.NineP.Monad

data LocalState :: Effect where
  SetName :: String -> LocalState m ()
  GetRoot :: (MonadIO n) => LocalState m (FileTree n ())
  InsertFid :: (MonadIO n) => Word32 -> FileTree n () -> LocalState m ()
  GetQid :: (MonadIO n) => (FileTree n ()) -> LocalState m Qid
  GetFid    :: (MonadIO n) => Word32 -> LocalState m (FileTree n ())
  GetName  :: LocalState m String
  OpenFile :: (MonadIO n) => Word32 -> File n -> Word8 -> LocalState m ()
  GetOpenFile :: (MonadIO n) => Word32 -> Word8 -> LocalState m (File n)
  ExecFOP :: (MonadIO n) => n a -> LocalState m a

type instance DispatchOf LocalState = 'Dynamic
makeEffect ''LocalState


runLocalState :: (HasCallStack, IOE :> es) => Eff (LocalState : es) a -> Eff es a
runLocalState = reinterpret (evalState ()) $ \_ -> const $ return undefined
