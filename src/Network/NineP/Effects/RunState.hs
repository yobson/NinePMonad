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
  , TemplateHaskell
  , OverloadedLabels
#-}

{-|
Module      : Network.NineP.Effects.RunState
Description : Internal state of server
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}

module Network.NineP.Effects.RunState
( LocalState
, runLocalState
, setName
, getRoot
, insertFid
, getQid
, getFid
, forgetFid
, getName
, openFile
, getOpenFile
, getStat
, execFOP
, rec
, corec
) where


import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.IO.Class
import Data.Word
import Data.NineP
import qualified Data.Map as Map
import Data.Map ((!))

import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger
import Network.NineP.Monad
import Data.Bits

import Lens.Micro.TH
import Lens.Micro.Freer

type Tree m = FileTree m
type TreeF m = FileTreeF m Word64

data LocalState m r where
  SetName :: String -> LocalState m ()
  GetRoot ::  LocalState m (Tree m)
  InsertFid :: Word32 -> Tree m -> LocalState m ()
  GetQid :: Tree m -> LocalState m Qid
  GetFid    :: Word32 -> LocalState m (Tree m)
  GetName  :: LocalState m String
  OpenFile :: Word32 -> Tree m -> Word8 -> LocalState m ()
  ForgetFid :: Word32 -> LocalState m ()
  GetOpenFile :: Word32 -> Word8 -> LocalState m (Tree m)
  GetStat    :: Tree m -> LocalState m Stat
  ExecFOP :: m a -> LocalState m a

setName :: forall n es . (Member (LocalState n) es) => String -> Eff es ()
setName s = send $ SetName @n s


getRoot :: (Member (LocalState n) es) => Eff es (Tree n)
getRoot = send GetRoot

insertFid :: (Member (LocalState n) es) => Word32 -> Tree n -> Eff es ()
insertFid fid f = send $ InsertFid fid f

getQid :: (Member (LocalState n) es) => Tree n -> Eff es Qid
getQid = send . GetQid

getFid :: (Member (LocalState n) es) => Word32 -> Eff es (Tree n)
getFid = send . GetFid

forgetFid :: forall n es . (Member (LocalState n) es) => Word32 -> Eff es ()
forgetFid fid = send $ ForgetFid @n fid

getName :: forall n es . (Member (LocalState n) es) => Eff es String
getName = send $ GetName @n

openFile :: (Member (LocalState n) es) => Word32 -> Tree n -> Word8 -> Eff es ()
openFile fid f mode = send $ OpenFile fid f mode

getOpenFile :: forall n es . (Member (LocalState n) es) => Word32 -> Word8 -> Eff es (Tree n)
getOpenFile fid mode = send $ GetOpenFile @n fid mode

getStat :: forall n es . (Member (LocalState n) es) => Tree n -> Eff es Stat
getStat = send . GetStat @n

execFOP :: (Member (LocalState n) es) => n a -> Eff es a
execFOP = send . ExecFOP

rec :: (Functor f) => (f a -> a) -> Fix f -> a
rec f (Fix x) = f $ fmap (rec f) x

corec :: (Functor f) => (a -> f a) -> a -> Fix f
corec f i = Fix $ corec f <$> f i

buildTree :: Map.Map Word64 (FileTreeF m Word64) -> FileTree m
buildTree = buildTreeFrom 0

getNode :: Tree m -> TreeF m
getNode (unfix -> Leaf f) = Leaf f
getNode (unfix -> Branch d ch) = Branch d $ map qidPath ch

buildTreeFrom :: Word64 -> Map.Map Word64 (FileTreeF m Word64) -> FileTree m
buildTreeFrom st ft = corec (ft !) st

theQid :: Tree m -> Qid
theQid (unfix -> Branch q _) = Qid 0x80 0 $ dirQidPath q
theQid (unfix -> Leaf q) = Qid 0 0 $ fileQidPath q

theQidF :: FileTreeF m a -> Qid
theQidF (Branch q _) = Qid 0x80 0 $ dirQidPath q
theQidF (Leaf q) = Qid 0 0 $ fileQidPath q

modeF :: FileTreeF m a -> Word32
modeF ftF = typS .|. fromIntegral (getPropF #perms ftF)
  where qid = theQidF ftF
        typ = fromIntegral $ qid_typ qid
        typS = typ `shiftL` 24

qidPath :: Tree m -> Word64
qidPath = qid_path . theQid

qidPathF :: TreeF m -> Word64
qidPathF = qid_path . theQidF


data RunState n = RunState
  { _uname :: String
  , _fidMap :: Map.Map Word32 Word64
  , _openFiles :: Map.Map Word32 (TreeF n, Word8)
  , _fileMap :: Map.Map Word64 (TreeF n)
  }

makeLenses ''RunState

initialState :: RunState n
initialState = RunState
  { _uname = ""
  , _fidMap = Map.empty
  , _openFiles = Map.empty
  , _fileMap = Map.empty
  }

mkStat :: TreeF m -> Stat
mkStat f = Stat
  { st_typ    = 0
  , st_dev    = 0
  , st_qid    = theQidF f
  , st_mode   = modeF f
  , st_atime  = 0
  , st_mtime  = 0
  , st_length = 0
  , st_name   = getPropF #name f
  , st_uid    = getPropF #owner f
  , st_gid    = getPropF #group f
  , st_muid   = getPropF #owner f
  }

runLocalState :: forall m es a 
              .  (MonadIO m, LastMember m es, Members [Error NPError, Logger] es) 
              => FileSystemT m () -> Eff (LocalState m : es) a -> Eff es a
runLocalState fs eff = do
  fm <- sendM $ runFileSystemT fs
  evalState initialState{_fileMap = fm} $ reinterpret go eff
  where
    go :: (MonadIO m, LastMember m (State (RunState m) : es)) => LocalState m x -> Eff (State (RunState m) : es) x
    go (SetName name) = assign @(RunState m) uname name

    go GetRoot = do
      ftF <- use fileMap
      return $ buildTree ftF

    go (InsertFid fid ft) = modifying @(RunState m) fidMap $ Map.insert fid (qidPath ft)

    go (GetQid ft) = return $ theQid ft

    go (GetFid fid) = do
      ftF <- use fileMap
      fm <- use @(RunState m) fidMap
      case Map.lookup fid fm of
        Just qid -> return $ buildTreeFrom qid ftF
        Nothing -> do
          logMsg Warning $ "Did not find fid: " <> show fid <> " in fidMap"
          throwError $ NPError "Did not find fid in fid map"

    go (ForgetFid fid) = modifying @(RunState m) fidMap $ Map.delete fid

    go GetName = use @(RunState m) uname

    go (OpenFile fid f mode) = openFiles %= Map.insert fid (getNode f,mode)

    go (GetStat ft) = do
      fm <- use @(RunState m) fileMap
      case Map.lookup (qidPath ft) fm of
        Nothing -> do
          logMsg Warning $ "Did not find file with qid path: " <> show (qidPath ft)
          throwError $ NPError "Did not find qid in file map"
        Just f -> do
          return $ mkStat f

    go (GetOpenFile fid _) = do
      fm <- use @(RunState m) openFiles
      case Map.lookup fid fm of
        Just (treeF,_) -> do
          ftF <- use fileMap
          return $ buildTreeFrom (qidPathF treeF) ftF
        Nothing -> do
          logMsg Warning "Open file requested that was not opened"
          throwError $ NPError "File not open"

    go (ExecFOP fop) = sendM fop
