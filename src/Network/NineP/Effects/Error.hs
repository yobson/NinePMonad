
module Network.NineP.Effects.Error
( module Control.Monad.Freer.Error
, NPError(..)
) where

{-|
Module      : Network.NineP.Effects.Error
Description : Error type
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}

import Data.Word
import Control.Monad.Freer.Error

data NPError = NPError String
             | Proto Word16 String
  deriving Show
