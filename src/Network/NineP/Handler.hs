{-# LANGUAGE OverloadedLabels, PatternSynonyms, ViewPatterns, RankNTypes, TypeApplications, ScopedTypeVariables #-}

module Network.NineP.Handler where

import Data.NineP
import Data.Word
import Data.List
import Network.NineP.Monad hiding (dir, file)
import Network.NineP.Effects
import Data.Bits

import qualified Data.ByteString.Lazy as B

handleErr :: CallStack -> NPError -> App n ()
handleErr _ (Proto tag e) = sendMsg $ Msg TRerror tag $ Rerror e
handleErr _ e         = throwError e

handleRequest :: Msg -> App n ()
handleRequest (Msg typ tag body) = handleTyp typ tag body `catchError` handleErr
{-# INLINE handleRequest #-}

handleTyp :: Tag -> Word16 -> VarMsg -> App n ()
handleTyp TTversion tag body = handleTVersion tag body
handleTyp TTauth    tag body = handleTAuth    tag body
handleTyp TTattach  tag body = handleTAttach  tag body
handleTyp TTflush   tag body = handleTFlush   tag body
handleTyp TTwalk    tag body = handleTWalk    tag body
handleTyp TTopen    tag body = handleTOpen    tag body
handleTyp TTcreate  tag body = handleTCreate  tag body
handleTyp TTread    tag body = handleTRead    tag body
handleTyp TTwrite   tag body = handleTWrite   tag body
handleTyp _ tag _ = throwError $ Proto tag "Can't handle this kind of message"
{-# INLINE handleTyp #-}

handleTVersion :: Word16 -> VarMsg -> App n ()
handleTVersion tag (Tversion msize _) = do
  setMsgSize (min msize 4096)
  sendMsg $ Msg TRversion tag (Rversion 4096 "9P2000")
handleTVersion tag _ = throwError $ Proto tag "Malformed Request"
{-# INLINE handleTVersion #-}

handleTAuth :: Word16 -> VarMsg -> App n ()
handleTAuth tag _ = throwError $ Proto tag "Auth not required"
{-# INLINE handleTAuth #-}

handleTAttach :: forall n . Word16 -> VarMsg -> App n ()
handleTAttach tag (Tattach fid _ uname _) = do
  setName @n uname
  root <- getRoot @n
  case root of
    Leaf _ _ -> throwError $ NPError "Root must be a dir"
    branch   -> do
      insertFid fid branch
      qid <- getQid branch
      sendMsg $ Msg TRattach tag (Rattach qid)
 
handleTAttach tag _ = throwError $ Proto tag "Malformed Request"

handleTFlush :: Word16 -> VarMsg -> App n ()
handleTFlush = undefined

handleTWalk :: Word16 -> VarMsg -> App n ()
handleTWalk tag (Twalk _ _ []) = throwError $ Proto tag "Empty walk"
handleTWalk tag (Twalk fid newFid path) = do
  dir <- getFid fid
  (newDir, qids) <- walk dir path
  insertFid newFid newDir
  sendMsg $ Msg TRwalk tag $ Rwalk qids

  where
    walk :: FileTree n () -> [String] -> App n (FileTree n (), [Qid])
    walk ft [] = do
      qid <- getQid ft
      return (ft, [qid])
    walk ft (x:xs) = do
      case lookupName ft x of
        Nothing -> do
          qid <- getQid ft
          return (ft, [qid])
        Just ft' -> do
          (nFt, qids) <- walk ft' xs
          qid <- getQid nFt
          return (nFt, qid : qids)
handleTWalk tag _ = throwError $ Proto tag "Malformed Request"

handleTOpen :: forall n . Word16 -> VarMsg -> App n ()
handleTOpen tag (Topen fid mode) = do
  file' <- getFid @n fid
  case file' of
    Branch {}   -> throwError $ Proto tag "Can't open dir"
    Leaf _ file -> do
      uname <- getName @n
      if checkPerms file' uname mode
         then do
           qid <- getQid file'
           openFile fid file mode
           sendMsg $ Msg TRopen tag $ Ropen qid 0
         else throwError $ Proto tag "Does not have permission"
         
handleTOpen tag _ = throwError $ Proto tag "Malformed Request"

handleTCreate :: Word16 -> VarMsg -> App n ()
handleTCreate tag (Tcreate fid name perm mode) = throwError $ Proto tag "Not implemented"
handleTCreate tag _ = throwError $ Proto tag "Malformed Request"

handleTRead :: forall n . Word16 -> VarMsg -> App n ()
handleTRead tag (Tread fid (fromIntegral -> offset) (fromIntegral -> count)) = do
  f <- getOpenFile @n fid ModeRead
  let r = fileRead f
  res <- case r of
           Just (Reader reader) -> B.take count . B.drop offset <$> execFOP reader
           _                    -> throwError $ Proto tag "Reading not allowed"
  sendMsg $ Msg TRread tag $ Rread res
handleTRead tag _ = throwError $ Proto tag "Malformed request"

handleTWrite :: Word16 -> VarMsg -> App n ()
handleTWrite tag _ = throwError $ Proto tag "Not implemented"


lookupName :: FileTree n a -> String -> Maybe (FileTree n a)
lookupName (Leaf _ _)        _  = Nothing
lookupName (Branch _ _ tree) fn = find (\ft -> getProp #name ft == fn) tree
{-# INLINE lookupName #-}

pattern ModeRead, ModeWrite, ModeRW, ModeExec :: Word8
pattern ModeRead  = 0
pattern ModeWrite = 1
pattern ModeRW    = 2
pattern ModeExec  = 3

checkPerms :: FileTree n a -> String -> Word8 -> Bool
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
