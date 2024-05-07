{-# LANGUAGE TypeOperators, FlexibleContexts #-}

{-|
Module      : Lens.Micro.Effectful
Description : Common patterns for microlens and effectful
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
Portability : POSIX
-}

module Lens.Micro.Effectful where

import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Lens.Micro
import Data.Monoid

-- | Synonym for '(^.)', genralised for 'Reader'
view :: (Reader r :> es) => Getting a r a -> Eff es a
view l = do
  env <- ask
  return $ env ^. l
{-# INLINE view #-}

-- | Synonym for '(^?)', genralised for 'Reader'
preview :: (Reader r :> es) => Getting (First a) r a -> Eff es (Maybe a)
preview l = do
  env <- ask
  return $ env ^? l
{-# INLINE preview #-}

-- | This is '(^.)' for 'State'. If your state has a field named @foo@, you can write
--
-- @
--  x <- 'use' foo
-- @
use :: (State s :> es) => Getting a s a -> Eff es a
use l = do
  st <- get
  return $ st ^. l
{-# INLINE use #-}

preuse :: (State s :> es) => Getting (First a) s a -> Eff es (Maybe a)
preuse l = do
  st <- get
  return $ st ^? l
{-# INLINE preuse #-}

-- | Modify the state by applying a function to a part of it
(%=) :: (State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}
infix 4 %=

-- | A syninym for '(%=)'
modifying :: (State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
modifying = (%=)
{-# INLINE modifying #-}

-- | Assign a value to part of the state
(.=) :: (State s :> es) => ASetter s s a b -> b -> Eff es ()
l .= x = modify (l .~ x)
{-# INLINE (.=) #-}
infix 4 .=

-- | Synonym for '(.=)'
assign :: (State s :> es) => ASetter s s a b -> b -> Eff es ()
assign = (.=)
{-# INLINE assign #-}

-- | Same as '(.=)', but wraps the value in a 'Just'
(?=) :: (State s :> es) => ASetter s s a (Maybe b) -> b -> Eff es ()
l ?= b = l .= Just b
{-# INLINE (?=) #-}
infix 4 ?=

-- | Runs a monadic action, and then applies the result to part of the state.
(<~) :: (State s :> es) => ASetter s s a b -> Eff es b -> Eff es ()
l <~ mb = do
  b <- mb
  l .= b
{-# INLINE (<~) #-}
infixr 2 <~
