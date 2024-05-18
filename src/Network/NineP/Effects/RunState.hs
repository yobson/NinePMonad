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
, getStats
, execFOP
) where


import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State
import Control.Monad.IO.Class
import Data.Word
import Data.NineP
import qualified Data.Map as Map

import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger
import Control.Exception (IOException, try)
import Network.NineP.Monad

import Lens.Micro.TH
import Lens.Micro.Freer

type Tree m = FileTreeF m Word64

data LocalState m r where
  SetName :: String -> LocalState m ()
  GetRoot ::  LocalState m (Tree m)
  InsertFid :: Word32 -> Tree m -> LocalState m ()
  GetQid :: Tree m -> LocalState m Qid
  GetFid    :: Word32 -> LocalState m (Tree m)
  GetName  :: LocalState m String
  OpenFile :: Word32 -> File m -> Word8 -> LocalState m ()
  ForgetFid :: Word32 -> LocalState m ()
  GetOpenFile :: Word32 -> Word8 -> LocalState m (File n)
  GetStats    :: Word32 -> LocalState m [Stat]
  ExecFOP :: m a -> LocalState m a

-- makeEffect ''LocalState

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

openFile :: (Member (LocalState n) es) => Word32 -> File n -> Word8 -> Eff es ()
openFile fid f mode = send $ OpenFile fid f mode

getOpenFile :: forall n es . (Member (LocalState n) es) => Word32 -> Word8 -> Eff es (File n)
getOpenFile fid mode = send $ GetOpenFile @n fid mode

getStats :: forall n es . (Member (LocalState n) es) => Word32 -> Eff es [Stat]
getStats = send . GetStats @n

execFOP :: (Member (LocalState n) es) => n a -> Eff es a
execFOP = send . ExecFOP

data RunState n = RunState
  { _uname :: String
  , _fidMap :: Map.Map Word32 Qid
  , _openFiles :: Map.Map Word32 (File n, Word8)
  , _qidCount  :: Word64
  , _localFS :: FileSystemT n ()
  , _localFT :: Maybe (Tree n)
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

adapt :: (MonadIO m, LastMember m es, Member (Error NPError) es) => IO a -> Eff es a
adapt m = liftIO (try m) >>= \case
  Left (e::IOException) -> throwError . NPError $ show e
  Right v               -> return v

-- annotateFS :: (MonadIO m, LastMember m es, Members [Error NPError, State (RunState m), Logger] es) => FileSystemT m () -> Eff es (FileTree m Qid)
-- annotateFS hoist fs = undefined -- do
--  (_,fts) <- adapt hoist $ runFileSystemT fs
--  when (length fts /= 1) $
--    throwError $ NPError "File system does not have single root!"
--  annotateFT $ head fts

-- updateLocalFS :: (MonadIO m, LastMember m es, Members [Error NPError, State (RunState m), Logger] es) => FileSystemT m () -> Eff es ()
-- updateLocalFS hoist fs = do
--   (_,fts) <- adapt hoist $ runFileSystemT fs
--   when (length fts /= 1) $
--     throwError $ NPError "File system does not have single root!"
--   oft' <- use localFT
--   case oft' of
--     Just oft -> localFT <~ Just <$> updateLocalFT oft (head fts)
--     Nothing  -> localFT <~ Just <$> annotateFT (head fts)

-- updateLocalFT :: forall m es . (IOE :> es, Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => FileTree m Qid -> FileTree m () -> Eff es (FileTree m Qid)
-- updateLocalFT (Leaf q f) (Leaf _ f') | fileName f == fileName f' = return $ Leaf q f'
--                                      | otherwise                 = annotateFT (Leaf () f')
-- updateLocalFT (Branch q d1 _) (Branch _ d2 []) | dirName d1 == dirName d2 = return $ Branch q d2 []
--                                                | otherwise                = annotateFT (Branch () d2 [])
-- updateLocalFT (Branch q d1 []) (Branch _ d2 cs) = do
--   children <- mapM annotateFT cs
--   newBranch <- updateLocalFT @m (Branch q d1 []) (Branch () d2 [])
--   case newBranch of
--     (Branch qi di []) -> return $ Branch qi di children
--     _                 -> throwError $ NPError "Impossible"
-- updateLocalFT (Branch q d1 (c:cs)) (Branch _ d2 (d:ds)) = do
--   e <- updateLocalFT c d
--   newBranch <- updateLocalFT (Branch q d1 cs) (Branch () d2 ds)
--   case newBranch of
--     (Branch qi di chs) -> return $ Branch qi di (e:chs)
--     _                  -> throwError $ NPError "Impossible"
-- updateLocalFT _ xs = annotateFT xs


-- We must increment first as 0 is reserved for Qids yet to be calculated
-- annotateFT :: forall m es a . (Monad m, Error NPError :> es, State (RunState m) :> es, Logger :> es) => FileTree m a -> Eff es (FileTree m Qid)
-- annotateFT (Leaf _ f) = do
--   modifying @(RunState m) qidCount (+1)
--   c <- use @(RunState m) qidCount
--   logMsg Info $ concat ["Setting file ", fileName f, " to have ", show (Qid 0 0 c)]
--   return $ Leaf (Qid 0 0 c) f
-- annotateFT (Branch _ d ch) = do
--   modifying @(RunState m) qidCount (+1)
--   c <- use @(RunState m) qidCount
--   children <- mapM annotateFT ch
--   logMsg Info $ concat ["Setting dir ", dirName d, " to have ", show (Qid 0x80 0 c)]
--   return $ Branch (Qid 0x80 0 c) d children

theQid :: Tree m -> Qid
theQid (Branch q _) = Qid 0x80 0 $ dirQidPath q
theQid (Leaf q) = Qid 0 0 $ fileQidPath q

--ft2list :: Tree m -> [Tree m]
--ft2list l@(Leaf _)      = [l]
--ft2list   (Branch d ch) = Branch d ch : concatMap ft2list ch

-- lookupQid :: (MonadIO m, LastMember m es, Members [Error NPError, State (RunState m), Logger] es) => Qid -> Eff es (Tree m)
-- lookupQid qid = do
--   mlft <- use localFT
--   lft <- case mlft of
--     Nothing -> throwError $ NPError "No File Tree"
--     Just fs -> return fs
--   let fsList = ft2list lft
--   case filter (\f -> theQid f == qid) fsList of
--     [x] -> return x
--     _   -> throwError $ NPError "Qid invarient not met"


runLocalState :: forall m es a 
              .  (MonadIO m, LastMember m (State (RunState m) : es), Members [Error NPError, State (RunState m), Logger] es) 
              => FileSystemT m () -> Eff (LocalState m : es) a -> Eff es a
runLocalState fs = evalState (initialState fs) . reinterpret go
  where
    go :: LocalState m x -> Eff (State (RunState m) : es) x
    go (SetName name) = assign @(RunState m) uname name
    go GetRoot = do
      lfs <- use @(RunState m) localFS
      lft <- undefined -- annotateFS hoist lfs
      localFT ?= lft
      return lft
    go (InsertFid fid ft) = do
      let q = theQid ft
      logMsg Info $ "FidMap[" <> show fid <> "] = " <> show q
      modifying @(RunState m) fidMap (Map.insert fid q)

    go (GetQid ft) = return $ theQid ft
    go (GetFid fid) = do
      logMsg Info "Get Fid"
      fm <- use @(RunState m) fidMap
      case Map.lookup fid fm of
        Nothing -> throwError $ NPError "No fid in db" -- TODO: Catch this
        Just qid  -> do
          lfs <- use @(RunState m) localFS
          logMsg Info "Updating local tree"
          undefined -- updateLocalFS hoist lfs
          logMsg Info $ "Looking for file of Qid: " <> show qid
          ft <- undefined -- lookupQid qid
          logMsg Info "Found it!"
          return ft
    go (ForgetFid fid) = do
      logMsg Info $ "fidMap[" <> show fid <> "] = _"
      modifying @(RunState m) fidMap (Map.delete fid)
    go GetName = use @(RunState m) uname
    go (OpenFile fid f mode) = do
      -- TODO: Move perms check here
      openFiles %= Map.insert fid (f, mode)
    go (GetStats fid) = undefined
    go (GetOpenFile fid _) = do
      -- TODO: Perms check
      m <- use @(RunState m) openFiles
      case Map.lookup fid m of
        Nothing -> throwError $ NPError "File Not Open" -- TODO: Catch this
        Just (f,_) -> return f -- TODO: Perms Check

    go (ExecFOP fop) = sendM @m fop
