{-# LANGUAGE MonoLocalBinds #-}

module Network.NineP.Effects.Error
( module Control.Monad.Freer.Error
, NPError
, returnMaybe
, withErr
, CanFail
, adaptErr
, canFail
) where

{-|
Module      : Network.NineP.Effects.Error
Description : Error type
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Portability : POSIX
-}

-- import Data.Word
import Control.Monad.Freer
import Control.Monad.Freer.Error

-- data NPError = NPError String
--              | Proto Word16 String
--   deriving Show

type NPError = String

type CanFail x = Either NPError x

returnMaybe :: NPError -> Maybe a -> Eff es (CanFail a)
returnMaybe e = return . maybe (Left e) return

adaptErr :: Eff es (CanFail a) -> Eff (Error NPError : es) a
adaptErr xm = raise xm >>= either throwError return

canFail :: (Member (Error NPError) es) => Eff es (CanFail a) -> Eff es a
canFail xm = xm >>= either throwError return

withErr :: Eff es (CanFail a) -> (a -> Eff es (CanFail b)) -> Eff es (CanFail b)
withErr xm f = xm >>= either (pure . Left) f
