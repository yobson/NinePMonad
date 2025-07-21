{-# LANGUAGE GADTs, StrictData, TemplateHaskell, TypeOperators, DataKinds, TypeApplications #-}
{-# LANGUAGE PatternSynonyms, OverloadedLabels #-}

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
  LookupFid :: Word32 -> ClientState [FilePath]
  OpenFile :: Word32 -> Word8 -> ClientState Qid
  StatFid :: Word32 -> ClientState Stat
makeEffect ''ClientState

runClientState :: Members '[Error NPError, GlobalState] es => Eff (ClientState : es) a -> Eff es a
runClientState = evalState initState . reinterpret go
  where go :: Members '[Error NPError, GlobalState] es => ClientState x -> Eff (State ClState : es) x
        go (SetFid fid path) = modify (\s -> s {fidMap = Map.insert fid path $ fidMap s})
        go (SetUName n)      = modify (\s -> s { uname = n })
        go (LookupFid fid)   = do
          m <- gets fidMap
          maybe (throwError $ NPError "No Fid") return $ Map.lookup fid m
        go (OpenFile fid mode) = do
          path <- go $ LookupFid fid
          name <- gets uname
          w <- walk path
          case checkPerms w name mode of
            True -> do
              qid <- lookupQid path
              modify (\s -> s { openFiles = Map.insert fid (w,qid) $ openFiles s })
              return qid
            False -> throwError $ NPError "Do not have permission"
        go (StatFid fid) = do
          m <- gets openFiles
          case Map.lookup fid m of
            Just (f,q) -> return $ statFile f q
            Nothing -> throwError $ NPError "File not open!"

statFile :: Either (File m) (Directory m) -> Qid -> Stat
statFile f q = Stat
  { st_typ    = 0
  , st_dev    = 0
  , st_qid    = q
  , st_mode   = (either (const 0x00) (const 0x80000000) f) .|. (fromIntegral $ getProp #perms f)
  , st_atime  = 0
  , st_mtime  = 0
  , st_length = either (const 4096) (const 0) f
  , st_name   = getProp #name f
  , st_uid    = getProp #owner f
  , st_gid    = getProp #group f
  , st_muid   = ""
  }
 

pattern ModeRead, ModeWrite, ModeRW, ModeExec :: Word8
pattern ModeRead  = 0
pattern ModeWrite = 1
pattern ModeRW    = 2
pattern ModeExec  = 3

checkPerms :: Either (File m) (Directory m) -> String -> Word8 -> Bool
checkPerms ft uname mode = case mode of
                              ModeRead  -> canRead
                              ModeWrite -> canWrite
                              ModeRW    -> canRW
                              ModeExec  -> canExec
                              _ -> False
  where owner  = getProp #owner ft
        perms  = getProp #perms ft
        perms' | uname == owner = perms .|. (perms `shiftR` 6)
               | otherwise      = perms
        canRead  = perms' .&. 0o004 /= 0
        canWrite = perms' .&. 0o002 /= 0
        canRW    = perms' .&. 0o006 /= 0
        canExec  = perms' .&. 0o001 /= 0
{-# INLINE checkPerms #-}
