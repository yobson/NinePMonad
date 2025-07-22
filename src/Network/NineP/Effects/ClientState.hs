{-# LANGUAGE GADTs, StrictData, TemplateHaskell, TypeOperators, DataKinds, TypeApplications #-}
{-# LANGUAGE PatternSynonyms, OverloadedLabels, ViewPatterns #-}

{-|
Module      : Network.NineP.Effects.ClientState
Description : Effect for logging
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Portability : POSIX
-}

module Network.NineP.Effects.ClientState
( ClientState
, setFid
, setUName
, lookupFid
, openFile
, statFid
, statDir
, getOpenFile
, clunkFid
, runClientState
) where

import qualified Data.Map as Map
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State
import Control.Monad.Freer.Error

import Data.Word
import Data.NineP (Qid, Stat(..))
import Network.NineP.Monad (File(..), Directory(..), getProp)
import Network.NineP.Effects.Error
import Network.NineP.Effects.Logger
import Network.NineP.Effects.GlobalState
import Data.Bits

data ClState = CLState
  { fidMap    :: Map.Map Word32 [FilePath]
  , uname     :: String
  , openFiles :: Map.Map Word32 (Either (File IO) (Directory IO), Qid)
  }

initState :: ClState
initState = CLState
  { fidMap = Map.empty
  , uname = ""
  , openFiles = Map.empty
  }

data ClientState r where
  SetFid :: Word32 -> [FilePath] -> ClientState ()
  SetUName :: String -> ClientState ()
  LookupFid :: Word32 -> ClientState (CanFail [FilePath])
  OpenFile :: Word32 -> Word8 -> ClientState (CanFail Qid)
  StatFid :: Word32 -> ClientState (CanFail Stat)
  StatDir :: Word32 -> ClientState (CanFail [Stat])
  GetOpenFile :: Word32 -> Word8 -> ClientState (CanFail (Either (File IO) (Directory IO)))
  ClunkFid :: Word32 -> ClientState ()
makeEffect ''ClientState

runClientState :: Members '[GlobalState, Logger] es => Eff (ClientState : es) a -> Eff es a
runClientState = evalState initState . reinterpret go
  where go :: Members '[GlobalState, Logger] es => ClientState x -> Eff (State ClState : es) x
        go (SetFid fid path) = modify (\s -> s {fidMap = Map.insert fid path $ fidMap s})
        go (SetUName n)      = modify (\s -> s { uname = n })
        go (LookupFid fid)   = do
          logMsg Info $ "Looking for fid: " <> show fid
          m <- gets fidMap
          returnMaybe ("Fid " <> show fid <> " not found") $ Map.lookup fid m
        go (OpenFile fid mode) = runError @NPError $ do
          path <- adaptErr $ go $ LookupFid fid
          name <- gets uname
          w <- adaptErr $ walk path
          logMsg Info $ "Open file with mode: " <> show mode
          case checkPerms w name mode of
            True -> do
              qid <- adaptErr $ lookupQid path
              modify (\s -> s { openFiles = Map.insert fid (w,qid) $ openFiles s })
              return qid
            False -> throwError "Do not have permission"
        go (StatFid fid) = runError @NPError $ do
          m <- gets fidMap
          case Map.lookup fid m of
            Just path -> do
              f <- adaptErr $ walk path
              q <- adaptErr $ lookupQid path
              return $ statFile f q
            Nothing -> throwError "File not open!"
        go (StatDir fid) = runError @NPError $ do
          path <- adaptErr $ go $ LookupFid fid
          logMsg Info "Found FID"
          files <- adaptErr $ children path
          logMsg Info $ "Statting: " <> show files
          return $ map (uncurry statFile) files
        go (GetOpenFile fid mode) = do
          m <- gets openFiles
          case Map.lookup fid m of
            Just (f,_) -> do
              name <- gets uname
              if checkPerms f name mode
                then return $ Right f
                else return $ Left "You don't have permission"
            Nothing -> return $ Left "File not open!"
        go (ClunkFid fid) = do
          modify $ \s -> s { openFiles = Map.delete fid (openFiles s)
                           , fidMap    = Map.delete fid (fidMap s)
                           }


statFile :: Either (File m) (Directory m) -> Qid -> Stat
statFile f q = Stat
  { st_typ    = 0
  , st_dev    = 0
  , st_qid    = q
  , st_mode   = (either (const 0x00) (const 0x80000000) f) .|. (fromIntegral $ getProp #perms f)
  , st_atime  = 0
  , st_mtime  = 0
  , st_length = either (const 1024) (const 0) f
  , st_name   = getProp #name f
  , st_uid    = getProp #owner f
  , st_gid    = getProp #group f
  , st_muid   = ""
  }
 

checkPerms :: Either (File m) (Directory m) -> String -> Word8 -> Bool
checkPerms ft uname (fromIntegral -> mode) = mode .&. perms' == mode 
  where owner  = getProp #owner ft
        perms  = getProp #perms ft
        perms'  | uname == owner = perms .|. (perms `shiftR` 6)
                | otherwise      = perms
{-# INLINE checkPerms #-}
