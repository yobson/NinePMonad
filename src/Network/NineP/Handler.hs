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
import Network.NineP.Effects


handleErr :: Members [Error NPError, NPMsg] es => NPError -> Eff es ()
handleErr (Proto tag e) = sendMsg $ Msg TRerror tag $ Rerror e
handleErr e             = throwError e

handleRequest :: Members '[Error NPError, Logger, NPMsg, ClientState, GlobalState] es => Msg -> Eff es ()
handleRequest (Msg typ tag body) = handleTyp typ tag body `catchError` handleErr
{-# INLINE handleRequest #-}

handleTyp :: Members '[Error NPError, Logger, NPMsg, ClientState, GlobalState] es 
          => Tag -> Word16 -> VarMsg -> Eff es ()
handleTyp TTversion tag body = handleTVersion tag body
handleTyp TTauth    tag body = handleTAuth    tag body
handleTyp TTattach  tag body = handleTAttach  tag body
-- handleTyp TTflush   tag body = handleTFlush   tag body
handleTyp TTwalk    tag body = handleTWalk    tag body
handleTyp TTopen    tag body = handleTOpen    tag body
-- handleTyp TTcreate  tag body = handleTCreate  tag body
-- handleTyp TTread    tag body = handleTRead    tag body
-- handleTyp TTwrite   tag body = handleTWrite   tag body
-- handleTyp TTclunk   tag body = handleTClunk   tag body
handleTyp TTstat    tag body = handleTStat    tag body
handleTyp _ tag _ = throwError $ Proto tag "Can't handle this kind of message"
{-# INLINE handleTyp #-}

handleTVersion :: Members [Error NPError, NPMsg] es => Word16 -> VarMsg -> Eff es ()
handleTVersion tag (Tversion msize _) = do
  setMsgSize (min msize 4096)
  sendMsg $ Msg TRversion tag (Rversion 4096 "9P2000")
handleTVersion tag _ = throwError $ Proto tag "Malformed Request"
{-# INLINE handleTVersion #-}

handleTAuth :: Member (Error NPError) es => Word16 -> VarMsg -> Eff es ()
handleTAuth tag _ = throwError $ Proto tag "Auth not required"
{-# INLINE handleTAuth #-}

handleTAttach :: Members '[GlobalState, NPMsg, Error NPError, ClientState] es => Word16 -> VarMsg -> Eff es ()
handleTAttach tag (Tattach fid _ uname _) = do
  qid <- lookupQid ["/"]
  setFid fid ["/"]
  setUName uname
  sendMsg $ Msg TRattach tag (Rattach qid)
handleTAttach tag _ = throwError $ Proto tag "Malformed Request"

handleTWalk :: Members '[NPMsg, ClientState, Error NPError, GlobalState, Logger] es 
            => Word16 -> VarMsg -> Eff es ()
handleTWalk tag (Twalk fid newFid path) = do
  logMsg Info "Walk MSG"
  dir <- lookupFid fid
  logMsg Info "Got Fid"
  qids <- mapM (lookupQid . (dir <>)) $ inits path
  setFid newFid $ dir <> path
  sendMsg $ Msg TRwalk tag $ Rwalk qids
handleTWalk tag _ = throwError $ Proto tag "Malformed Request"

handleTOpen :: Members '[NPMsg, Error NPError, ClientState] es => Word16 -> VarMsg -> Eff es ()
handleTOpen tag (Topen fid mode) = do
  qid <- openFile fid mode
  sendMsg $ Msg TRopen tag $ Ropen qid 0
handleTOpen tag _ = throwError $ Proto tag "Malformed Request"
{-# INLINE handleTOpen #-}

handleTStat :: Members '[NPMsg, Error NPError, ClientState] es => Word16 -> VarMsg -> Eff es ()
handleTStat tag (Tstat fid) = do
  st <- statFid fid
  sendMsg $ Msg TRstat tag $ Rstat [st]
handleTStat tag _ = throwError $ Proto tag "Malformed Request"
{-# INLINE handleTStat #-}
