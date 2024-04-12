
module Network.NineP.Effects.Error
( module Effectful.Error.Static
, NPError(..)
) where

import Data.Word
import Effectful.Error.Static

data NPError = NPError String
             | Proto Word16 String
  deriving Show
