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
, execFOP
) where


import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Data.Word
import Data.NineP
import qualified Data.Map as Map

import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger
import Control.Exception (IOException)
import Control.Monad.Catch (catch)
import Control.Monad
import Network.NineP.Monad

import Lens.Micro.TH
import Lens.Micro.Effectful

data LocalState n :: Effect where
  SetName :: String -> LocalState n m ()
  GetRoot ::  LocalState n m (FileTree n Qid)
  InsertFid :: Word32 -> FileTree n Qid -> LocalState n m ()
  GetQid :: FileTree n Qid -> LocalState n m Qid
  GetFid    :: Word32 -> LocalState n m (FileTree n Qid)
  GetName  :: LocalState n m String
  OpenFile :: Word32 -> File n -> Word8 -> LocalState n m ()
  ForgetFid :: Word32 -> LocalState n m ()
  GetOpenFile :: Word32 -> Word8 -> LocalState n m (File n)
  ExecFOP :: n a -> LocalState n m a

type instance DispatchOf (LocalState n) = 'Dynamic

setName :: forall n es . (LocalState n :> es) => String -> Eff es ()
setName s = send $ SetName @n s


getRoot :: (LocalState n :> es) => Eff es (FileTree n Qid)
getRoot = send GetRoot

insertFid :: (LocalState n :> es) => Word32 -> FileTree n Qid -> Eff es ()
insertFid fid f = send $ InsertFid fid f

getQid :: (LocalState n :> es) => FileTree n Qid -> Eff es Qid
getQid = send . GetQid

getFid :: (LocalState n :> es) => Word32 -> Eff es (FileTree n Qid)
getFid = send . GetFid

forgetFid :: forall n es . (LocalState n :> es) => Word32 -> Eff es ()
forgetFid fid = send $ ForgetFid @n fid

getName :: forall n es . (LocalState n :> es) => Eff es String
getName = send $ GetName @n

openFile :: (LocalState n :> es) => Word32 -> File n -> Word8 -> Eff es ()
openFile fid f mode = send $ OpenFile fid f mode

getOpenFile :: (LocalState n :> es) => Word32 -> Word8 -> Eff es (File n)
getOpenFile fid mode = send $ GetOpenFile fid mode

execFOP :: (LocalState n :> es) => n a -> Eff es a
execFOP = send . ExecFOP

data RunState n = RunState
  { _uname :: String
  , _fidMap :: Map.Map Word32 Qid
  , _openFiles :: Map.Map Word32 (File n, Word8)
  , _qidCount  :: Word64
  , _localFS :: FileSystemT n ()
  , _localFT :: Maybe (FileTree n Qid)
  }

makeLenses ''RunState

initialState :: FileSystemT n () -> RunState n
initialState fs = RunState
  { _uname = ""
  , _fidMap = Map.empty
  , _openFiles = Map.empty
  , _qidCount  = 0
  , _localFS = fs
  , _localFT = Nothing
  }

type m ~> n = forall x . m x -> n x

adapt :: (IOE :> es, Error NPError :> es, State (RunState m) :> es) => (m ~> IO) -> m a -> Eff es a
adapt hoist (hoist -> m) = liftIO m
            `catch` \(e::IOException) -> throwError . NPError $ show e

annotateFS :: (IOE :> es, Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => (m ~> IO) -> FileSystemT m () -> Eff es (FileTree m Qid)
annotateFS hoist fs = do
  (_,fts) <- adapt hoist $ runFileSystemT fs
  when (length fts /= 1) $
    throwError $ NPError "File system does not have single root!"
  annotateFT $ head fts

updateLocalFS ::  (IOE :> es, Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => (m ~> IO) -> FileSystemT m () -> Eff es ()
updateLocalFS hoist fs = do
  (_,fts) <- adapt hoist $ runFileSystemT fs
  when (length fts /= 1) $
    throwError $ NPError "File system does not have single root!"
  oft' <- use localFT
  case oft' of
    Just oft -> localFT <~ Just <$> updateLocalFT oft (head fts)
    Nothing  -> localFT <~ Just <$> annotateFT (head fts)

updateLocalFT :: forall m es . (IOE :> es, Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => FileTree m Qid -> FileTree m () -> Eff es (FileTree m Qid)
updateLocalFT (Leaf q f) (Leaf _ f') | fileName f == fileName f' = return $ Leaf q f'
                                     | otherwise                 = annotateFT (Leaf () f')
updateLocalFT (Branch q d1 _) (Branch _ d2 []) | dirName d1 == dirName d2 = return $ Branch q d2 []
                                               | otherwise                = annotateFT (Branch () d2 [])
updateLocalFT (Branch q d1 []) (Branch _ d2 cs) = do
  children <- mapM annotateFT cs
  newBranch <- updateLocalFT @m (Branch q d1 []) (Branch () d2 [])
  case newBranch of
    (Branch qi di []) -> return $ Branch qi di children
    _                 -> throwError $ NPError "Impossible"
updateLocalFT (Branch q d1 (c:cs)) (Branch _ d2 (d:ds)) = do
  e <- updateLocalFT c d
  newBranch <- updateLocalFT (Branch q d1 cs) (Branch () d2 ds)
  case newBranch of
    (Branch qi di chs) -> return $ Branch qi di (e:chs)
    _                  -> throwError $ NPError "Impossible"
updateLocalFT _ xs = annotateFT xs


-- We must increment first as 0 is reserved for Qids yet to be calculated
annotateFT :: forall m es a . (Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => FileTree m a -> Eff es (FileTree m Qid)
annotateFT (Leaf _ f) = do
  modifying @(RunState m) qidCount (+1)
  c <- use @(RunState m) qidCount
  logMsg Info $ concat ["Setting file ", fileName f, " to have ", show (Qid 0 0 c)]
  return $ Leaf (Qid 0 0 c) f
annotateFT (Branch _ d ch) = do
  modifying @(RunState m) qidCount (+1)
  c <- use @(RunState m) qidCount
  children <- mapM annotateFT ch
  logMsg Info $ concat ["Setting dir ", dirName d, " to have ", show (Qid 0x80 0 c)]
  return $ Branch (Qid 0x80 0 c) d children

theQid :: FileTree n Qid -> Qid
theQid (Branch q _ _) = q
theQid (Leaf q _) = q

ft2list :: FileTree m a -> [FileTree m a]
ft2list l@(Leaf _ _)      = [l]
ft2list   (Branch q d ch) = Branch q d [] : concatMap ft2list ch

lookupQid :: (Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => Qid -> Eff es (FileTree m Qid)
lookupQid qid = do
  mlft <- use localFT
  lft <- case mlft of
    Nothing -> throwError $ NPError "No File Tree"
    Just fs -> return fs
  let fsList = ft2list lft
  case filter (\f -> theQid f == qid) fsList of
    [x] -> return x
    _   -> throwError $ NPError "Qid invarient not met"


runLocalState :: forall es m a . (HasCallStack, IOE :> es, Monad m, Error NPError :> es, Logger :> es) => FileSystemT m () -> (m ~> IO) -> Eff (LocalState m : es) a -> Eff es a
runLocalState fs hoist = reinterpret (evalState $ initialState @m fs) $ \_ -> \case
  SetName name -> assign @(RunState m) uname name
  GetRoot      -> do
    lfs <- use localFS
    lft <- annotateFS hoist lfs
    localFT ?= lft
    return lft
  InsertFid fid ft -> do
    let q = theQid ft
    logMsg Info $ "FidMap[" <> show fid <> "] = " <> show q
    modifying @(RunState m) fidMap (Map.insert fid q)

  GetQid ft -> return $ theQid ft
  GetFid fid -> do
    logMsg Info "Get Fid"
    fm <- use @(RunState m) fidMap
    case Map.lookup fid fm of
      Nothing -> throwError $ NPError "No fid in db" -- TODO: Catch this
      Just qid  -> do
        lfs <- use localFS
        logMsg Info "Updating local tree"
        updateLocalFS hoist lfs
        logMsg Info $ "Looking for file of Qid: " <> show qid
        ft <- lookupQid qid
        logMsg Info "Found it!"
        return ft
  ForgetFid fid -> do
    logMsg Info $ "fidMap[" <> show fid <> "] = _"
    modifying @(RunState m) fidMap (Map.delete fid)
  GetName -> use @(RunState m) uname
  OpenFile fid f mode -> do
    -- TODO: Move perms check here
    openFiles %= Map.insert fid (f, mode)
  GetOpenFile fid _ -> do
    -- TODO: Perms check
    m <- use openFiles
    case Map.lookup fid m of
      Nothing -> throwError $ NPError "File Not Open" -- TODO: Catch this
      Just (f,_) -> return f -- TODO: Perms Check

  ExecFOP fop -> adapt hoist fop
