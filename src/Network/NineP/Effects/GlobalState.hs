{-# LANGUAGE GADTs, ScopedTypeVariables, RankNTypes, TemplateHaskell, TypeOperators, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, DataKinds, StrictData, RecordWildCards, OverloadedLabels #-}
{-# OPTIONS -Wno-name-shadowing #-}

{-|
Module      : Network.NineP.Effects.GlobalState
Description : Effect for logging
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Portability : POSIX
-}

module Network.NineP.Effects.GlobalState
( GlobalState
, Tree(..)
, GState
, lookupQid
, fileTree
, walk
, children
, runGlobalState
, mkGlobalState
) where

import Control.Concurrent.STM
import Control.Monad
import Data.Word
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State
import Data.NineP (Qid(..))

import Network.NineP.Monad
import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger
import Data.List

import qualified Data.Map as Map


data Tree m = TDir  (Directory m) [Tree m]
            | TFile (File m)
            deriving (Show)

data GlobalState r where
  LookupQid   :: [FilePath] -> GlobalState Qid
  FileTree    :: GlobalState (Tree IO)
  Walk        :: [FilePath] -> GlobalState (Either (File IO) (Directory IO))
  Children    :: [FilePath] -> GlobalState [(Either (File IO) (Directory IO), Qid)]

data GState m = GState 
  { fileSystem :: TMVar (FileSystemT m ())
  , nextQID    :: TVar  Word64
  , qidMap     :: TMVar (Map.Map [FilePath] Qid)
  }

mkGlobalState :: Monad m => FileSystemT m () -> IO (GState m)
mkGlobalState fs = do
  fsT  <-  newTMVarIO fs
  qidT <-  newTVarIO 0
  qmap <-  newTMVarIO Map.empty
  return $ GState fsT qidT qmap

findFile :: Members [Error NPError, Logger] es => [Tree m] -> String -> Eff es (Tree m)
findFile tr n = case find go tr of
  Just f  -> logMsg Info ("Found: " <> n) >> return f
  Nothing -> throwError $ NPError "File not found"
  where go (TFile File{..}) = fileName == n
        go (TDir Directory{..} _) = dirName == n

walk' :: Members [Error NPError, Logger] es => Tree m -> [FilePath] -> Eff es (Tree m)
walk' tree@(TFile File{..}) [file] | fileName == file = return tree
walk' tree@(TDir Directory{..} _) [file] | dirName == file = return tree
walk' (TDir _ kids) (_:x:xs) = do
  logMsg Info $ "Looking for: " <> x
  f <- findFile kids x
  walk' f (x:xs)
walk' _ _ = throwError $ NPError "File not found!"

mkQid :: Word64 -> Tree m -> Qid
mkQid path (TDir _ _) = Qid {qid_typ = 0x80, qid_vers = 0, qid_path = path}
mkQid path (TFile _ ) = Qid {qid_typ = 0x00, qid_vers = 0, qid_path = path}
  

runGlobalState :: forall es m a . (LastMember IO es, Members '[Error NPError, Logger] es, Monad m) => GState m -> (m ~> IO) -> Eff (GlobalState : es) a -> Eff es a
runGlobalState initState runM eff =
  evalState initState $ reinterpret go eff
  where
    go :: (LastMember IO es, Members '[Error NPError, Logger] es) => GlobalState x -> Eff (State (GState m) : es) x
    go FileTree = do
      var <- gets @(GState m) fileSystem
      fsystem <- send $ atomically $ takeTMVar var
      tree <- send $ evalTree runM fsystem
      send $ atomically $ putTMVar var fsystem
      case tree of
        [x] -> return x
        _   -> throwError $ NPError "Tree has multiple roots!"
    go (LookupQid fp) = do
      logMsg Info $ "Looking up: " <> show fp
      qmapT <- gets @(GState m) qidMap
      qmap <- send $ atomically $ takeTMVar qmapT
      case Map.lookup fp qmap of
        Just qp -> do
          logMsg Info "Qid Found"
          send $ atomically $ putTMVar qmapT qmap
          return qp
        Nothing -> do
          logMsg Info "Qid not found!"
          qGenT <- gets @(GState m) nextQID
          root  <- go FileTree
          f     <- walk' root fp
          n <- send $ atomically $ do
            n <- readTVar qGenT
            modifyTVar qGenT (+1)
            let qid = mkQid n f
            putTMVar qmapT $ Map.insert fp qid qmap
            return qid
          logMsg Info $ unwords ["Creating new qid:", show fp, "→", show n]
          return n
    go (Walk path) = do
      root  <- go FileTree
      w <- walk' root path
      return $ toEither w
    go (Children path) = do
      root  <- go FileTree
      logMsg Info "Got root"
      logMsg Info $ "Walking to " <> show path
      node <- walk' root path
      logMsg Info $ "Got: " <> show node
      case node of
        TFile _     -> throwError $ NPError "File has no children"
        TDir _ kids -> do
          let files = map toEither kids
              names = map (getProp #name) files
          qids <- mapM (go . LookupQid . (snoc path)) names
          return $ zip files qids

toEither :: Tree m -> Either (File m) (Directory m)
toEither (TFile f ) = Left f
toEither (TDir d _) = Right d
{-# INLINE toEither #-}

snoc :: [a] -> a -> [a]
snoc xs x = xs <> [x]
{-# INLINE snoc #-}

  
evalReader :: (Monad m) => (m ~> IO) -> RawReader m -> RawReader IO
evalReader runM (RawReader f) = RawReader $ \offset count -> runM $ f offset count
{-# INLINE evalReader #-}

evalWriter :: (Monad m) => (m ~> IO) -> RawWriter m -> RawWriter IO
evalWriter runM (RawWriter f) = RawWriter $ \offset x -> runM (f offset x)
{-# INLINE evalWriter #-}

evalFile :: (Monad m) => (m ~> IO) -> File m -> File IO
evalFile runM f = File
  { fileName = fileName f
  , filePermissions = filePermissions f
  , fileOwner = fileOwner f
  , fileGroup = fileGroup f
  , fileRead = evalReader runM <$> fileRead f
  , fileWrite = evalWriter runM <$> fileWrite f
  , fileQIDPath = fileQIDPath f
  }
{-# INLINE evalFile #-}

evalDir :: (Monad m) => (m ~> IO) -> Directory m -> Directory IO
evalDir _ d = Directory
  { dirName = dirName d
  , dirPermissions = dirPermissions d
  , dirOwner = dirOwner d
  , dirGroup = dirGroup d
  , dirQIDPath = dirQIDPath d
  }
{-# INLINE evalDir #-}

treeAlgebra :: Monad m => FileSystemFT m (m [Tree m]) -> m [Tree m]
treeAlgebra (FileNode f r) = do
  rest <- r
  pure $ TFile f : rest

treeAlgebra (DirNode d childrenM r) = do
  children <- childrenM >>= runTree
  rest <- r
  pure $ TDir d children : rest

treeAlgebra (Embed xm) = join xm

runTree :: Monad m => FileSystemT m a -> m [Tree m]
runTree (Pure _)  = pure []
runTree (Free ft) = treeAlgebra (fmap runTree ft)

evalTree :: Monad m => (m ~> IO) -> FileSystemT m a -> IO [Tree IO]
evalTree h = fmap (map toIO) . h . runTree
  where toIO (TDir d kids) = TDir (evalDir h d) (map toIO kids)
        toIO (TFile f)     = TFile (evalFile h f)

makeEffect ''GlobalState
