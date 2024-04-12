{-# LANGUAGE 
    GADTs
  , DataKinds
  , TypeFamilies
  , TypeOperators
  , FlexibleContexts
  , LambdaCase
  , ScopedTypeVariables
  , TypeApplications
  , RankNTypes
  , ViewPatterns
  , AllowAmbiguousTypes
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
import Effectful.State.Static.Local
import Data.Word
import Data.NineP
import qualified Data.Map as Map

import Network.NineP.Effects.Error
import Control.Exception (IOException)
import Control.Monad.Catch (catch)
import Control.Monad
import Network.NineP.Monad

data LocalState n :: Effect where
  SetName :: String -> LocalState n m ()
  GetRoot ::  LocalState n m (FileTree n ())
  InsertFid :: Word32 -> FileTree n () -> LocalState n m ()
  GetQid :: FileTree n () -> LocalState n m Qid
  GetFid    :: Word32 -> LocalState n m (FileTree n ())
  GetName  :: LocalState n m String
  OpenFile :: Word32 -> File n -> Word8 -> LocalState n m ()
  GetOpenFile :: Word32 -> Word8 -> LocalState n m (File n)
  ExecFOP :: n a -> LocalState n m a

type instance DispatchOf (LocalState n) = 'Dynamic

setName :: forall n es . (LocalState n :> es) => String -> Eff es ()
setName s = send $ SetName @n s


getRoot :: (LocalState n :> es) => Eff es (FileTree n ())
getRoot = send GetRoot

insertFid :: (LocalState n :> es) => Word32 -> FileTree n () -> Eff es ()
insertFid fid f = send $ InsertFid fid f

getQid :: (LocalState n :> es) => FileTree n () -> Eff es Qid
getQid = send . GetQid

getFid :: (LocalState n :> es) => Word32 -> Eff es (FileTree n ())
getFid = send . GetFid

getName :: forall n es . (LocalState n :> es) => Eff es String
getName = send $ GetName @n

openFile :: (LocalState n :> es) => Word32 -> File n -> Word8 -> Eff es ()
openFile fid f mode = send $ OpenFile fid f mode

getOpenFile :: (LocalState n :> es) => Word32 -> Word8 -> Eff es (File n)
getOpenFile fid mode = send $ GetOpenFile fid mode

execFOP :: (LocalState n :> es) => n a -> Eff es a
execFOP = send . ExecFOP

data RunState n = RunState
  { uname :: String
  , fidMap :: Map.Map Word32 (FileTree n ())
  }

initialState :: RunState n
initialState = RunState
  { uname = ""
  , fidMap = Map.empty
  }

type m ~> n = forall x . m x -> n x

adapt :: (IOE :> es, Error NPError :> es, State (RunState m) :> es) => (m ~> IO) -> m a -> Eff es a
adapt hoist (hoist -> m) = liftIO m
            `catch` \(e::IOException) -> throwError . NPError $ show e


runLocalState :: forall es m a . (HasCallStack, IOE :> es, Monad m, Error NPError :> es) => FileSystemT m () -> (m ~> IO) -> Eff (LocalState m : es) a -> Eff es a
runLocalState fs hoist = reinterpret (evalState $ initialState @m) $ \_ -> \case
  SetName name -> modify $ \(s :: RunState m) -> s {uname = name}
  GetRoot      -> do
    (_,fts) <- adapt hoist $ runFileSystemT fs
    when (length fts /= 1) $
      throwError $ NPError "File system does not have single root!"
    return $ head fts
  InsertFid fid ft -> do
    fMap <- gets fidMap
    let nMap = Map.insert fid ft fMap
    modify $ \s -> s {fidMap = nMap}

  ExecFOP fop -> adapt hoist fop
