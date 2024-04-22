{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Lens.Micro.Effectful where


import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Lens.Micro
import Data.Monoid

view :: (Reader r :> es) => Getting a r a -> Eff es a
view l = do
  env <- ask
  return $ env ^. l
{-# INLINE view #-}

preview :: (Reader r :> es) => Getting (First a) r a -> Eff es (Maybe a)
preview l = do
  env <- ask
  return $ env ^? l
{-# INLINE preview #-}

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

(%=) :: (State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}
infix 4 %=

modifying :: (State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
modifying = (%=)
{-# INLINE modifying #-}

(.=) :: (State s :> es) => ASetter s s a b -> b -> Eff es ()
l .= x = modify (l .~ x)
{-# INLINE (.=) #-}
infix 4 .=

assign :: (State s :> es) => ASetter s s a b -> b -> Eff es ()
assign = (.=)
{-# INLINE assign #-}

(?=) :: (State s :> es) => ASetter s s a (Maybe b) -> b -> Eff es ()
l ?= b = l .= Just b
{-# INLINE (?=) #-}
infix 4 ?=

(<~) :: (State s :> es) => ASetter s s a b -> Eff es b -> Eff es ()
l <~ mb = do
  b <- mb
  l .= b
{-# INLINE (<~) #-}
infixr 2 <~
