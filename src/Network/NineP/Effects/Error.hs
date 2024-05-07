
module Network.NineP.Effects.Error
( module Effectful.Error.Static
, NPError(..)
) where

{-|
Module      : Network.NineP.Effects.Error
Description : Error stype
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}

import Data.Word
import Effectful.Error.Static

data NPError = NPError String
             | Proto Word16 String
  deriving Show
