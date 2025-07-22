{-# LANGUAGE OverloadedLabels, PatternSynonyms, ViewPatterns, RankNTypes, TypeApplications, ScopedTypeVariables, DataKinds, MonoLocalBinds #-}

{-|
Module      : Network.NineP.Handler
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Portability : POSIX
-}

module Network.NineP.Handler where

import Data.NineP
import Data.Word
import Data.List
import Network.NineP.Monad (File(..),RawReader(..),RawWriter(..))
import Network.NineP.Effects
import Data.Binary.Put

import qualified Data.ByteString.Lazy as B

handleErr :: Members [Error NPError, NPMsg, Logger] es => Word16 -> NPError -> Eff es ()
handleErr tag e = do
  logMsg Warning e
  sendMsg $ Msg TRerror tag $ Rerror e
{-# INLINE handleErr #-}

handleRequest :: (Members '[Error NPError, Logger, NPMsg, ClientState, GlobalState] es, LastMember IO es) 
              => Msg -> Eff es ()
handleRequest (Msg typ tag body) = (handleTyp typ tag body) `catchError` (handleErr tag)
{-# INLINE handleRequest #-}

handleTyp :: (Members '[Error NPError, Logger, NPMsg, ClientState, GlobalState] es, LastMember IO es) 
          => Tag -> Word16 -> VarMsg -> Eff es ()
handleTyp TTversion tag body = handleTVersion tag body
handleTyp TTauth    tag body = handleTAuth    tag body
handleTyp TTattach  tag body = handleTAttach  tag body
handleTyp TTflush   tag body = handleTFlush   tag body
handleTyp TTwalk    tag body = handleTWalk    tag body
handleTyp TTopen    tag body = handleTOpen    tag body
-- handleTyp TTcreate  tag body = handleTCreate  tag body
handleTyp TTread    tag body = handleTRead    tag body
handleTyp TTwrite   tag body = handleTWrite   tag body
handleTyp TTclunk   tag body = handleTClunk   tag body
handleTyp TTstat    tag body = handleTStat    tag body
handleTyp TTwstat   tag body = handleTWStat   tag body
handleTyp _ _ _ = throwError "Can't handle this kind of message"
{-# INLINE handleTyp #-}

handleTVersion :: Members [Error NPError, NPMsg] es => Word16 -> VarMsg -> Eff es ()
handleTVersion tag (Tversion msize _) = do
  setMsgSize (min msize 4096)
  sendMsg $ Msg TRversion tag (Rversion 4096 "9P2000")
handleTVersion _ _ = throwError "Malformed Request"
{-# INLINE handleTVersion #-}

handleTAuth :: Member (Error NPError) es => Word16 -> VarMsg -> Eff es ()
handleTAuth _ _ = throwError "Auth not required"
{-# INLINE handleTAuth #-}

handleTFlush :: Member (NPMsg) es => Word16 -> VarMsg -> Eff es ()
handleTFlush tag _ = sendMsg $ Msg TRflush tag Rflush
{-# INLINE handleTFlush #-}

handleTAttach :: Members '[Logger, GlobalState, NPMsg, Error NPError, ClientState] es => Word16 -> VarMsg -> Eff es ()
handleTAttach tag (Tattach fid _ uname _) = do
  logMsg Info "Looking up root fid"
  qid <- canFail $ lookupQid ["/"]
  logMsg Info $ "Got: " <> show qid
  setFid fid ["/"]
  setUName uname
  sendMsg $ Msg TRattach tag (Rattach qid)
handleTAttach _ _ = throwError "Malformed Request"

handleTWalk :: Members '[NPMsg, ClientState, Error NPError, GlobalState, Logger] es 
            => Word16 -> VarMsg -> Eff es ()
handleTWalk tag (Twalk fid newFid path) = do
  dir <- canFail $ lookupFid fid
  logMsg Info "Got Fid"
  logMsg Info $ "Walking through: " <> show path <> " from " <> show dir
  qids <- mapM (canFail . lookupQid . (dir <>)) $ drop 1 $ inits path
  setFid newFid $ dir <> path
  sendMsg $ Msg TRwalk tag $ Rwalk qids
handleTWalk _ _ = throwError "Malformed Request"

handleTOpen :: Members '[NPMsg, Error NPError, ClientState] es => Word16 -> VarMsg -> Eff es ()
handleTOpen tag (Topen fid mode) = do
  qid <- canFail $ openFile fid mode
  sendMsg $ Msg TRopen tag $ Ropen qid 0
handleTOpen _ _ = throwError "Malformed Request"
{-# INLINE handleTOpen #-}

handleTRead :: (Members '[NPMsg, Error NPError, ClientState, Logger] es, LastMember IO es) => Word16 -> VarMsg -> Eff es ()
handleTRead tag (Tread fid offset count) = do
  logMsg Info "Read"
  file <- canFail $ getOpenFile fid ModeRead
  out <- case file of
    Left  fil -> do
      logMsg Info "Reading File"
      case fileRead fil of
        Just (RawReader r) -> do
          bytes <- send $ r offset count
          sendMsg $ Msg TRread tag $ Rread bytes
        Nothing -> throwError "No reader!"
    Right _ -> do
      logMsg Info "Reading Dir"
      dirStat <- canFail $ statDir fid
      let bytes = runPut $ mapM_ put dirStat
      sendMsg $ Msg TRread tag $ Rread $ B.take (fromIntegral count) $ B.drop (fromIntegral offset) bytes
  return out
handleTRead _ _ = throwError "Malformed Request"

handleTWrite :: (Members '[NPMsg, Error NPError, ClientState, Logger] es, LastMember IO es) => Word16 -> VarMsg -> Eff es ()
handleTWrite tag (Twrite fid offset dat) = do
  logMsg Info "Write"
  file <- canFail $ getOpenFile fid ModeWrite
  out <- case file of
    Left  fil -> do
      logMsg Info "Writing File"
      case fileWrite fil of
        Just (RawWriter w) -> do
          send $ w offset dat
          sendMsg $ Msg TRwrite tag $ Rwrite $ fromIntegral $ B.length dat
        Nothing -> throwError "No writer!"
    Right _ -> throwError "Can't write to directories"
  return out
handleTWrite _ _ = throwError "Malformed Request"

handleTClunk :: Members '[NPMsg, Error NPError, ClientState] es => Word16 -> VarMsg -> Eff es ()
handleTClunk tag (Tclunk fid) = do
  clunkFid fid
  sendMsg $ Msg TRclunk tag Rclunk
handleTClunk _ _ = throwError "Malformed Request"
{-# INLINE handleTClunk #-}

handleTStat :: Members '[NPMsg, Error NPError, ClientState, Logger] es => Word16 -> VarMsg -> Eff es ()
handleTStat tag (Tstat fid) = do
  logMsg Info "STAT"
  st <- canFail $ statFid fid
  sendMsg $ Msg TRstat tag $ Rstat [st]
handleTStat _ _ = throwError "Malformed Request"
{-# INLINE handleTStat #-}

handleTWStat :: Members '[NPMsg, Error NPError, ClientState, Logger] es => Word16 -> VarMsg -> Eff es ()
handleTWStat tag (Twstat _ _) = do
  logMsg Info "WSTAT"
  sendMsg $ Msg TRwstat tag Rwstat
handleTWStat _ _ = throwError "Malformed Request"
{-# INLINE handleTWStat #-}

pattern ModeRead, ModeWrite, ModeRW, ModeExec :: Word8
pattern ModeRead  = 4
pattern ModeWrite = 2
pattern ModeRW    = 6
pattern ModeExec  = 1
