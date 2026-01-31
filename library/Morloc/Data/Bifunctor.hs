{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Morloc.Data.Bifunctor
Description : The Bifunctor typeclass, with monadic instances
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Data.Bifunctor (Bifunctor (..)) where

import Control.Monad.Identity (runIdentity)

class Bifunctor f where
  bimapM :: (Monad m) => (a -> m a') -> (b -> m b') -> f a b -> m (f a' b')

  firstM :: (Monad m) => (a -> m a') -> f a b -> m (f a' b)
  firstM f = bimapM f return

  secondM :: (Monad m) => (b -> m b') -> f a b -> m (f a b')
  secondM g = bimapM return g

  bimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'
  bimap f g = runIdentity . bimapM (return . f) (return . g)

  first :: (a -> a') -> f a b -> f a' b
  first f = runIdentity . firstM (return . f)

  second :: (b -> b') -> f a b -> f a b'
  second f = runIdentity . secondM (return . f)

instance Bifunctor Either where
  bimapM f _ (Left a) = Left <$> f a
  bimapM _ g (Right b) = Right <$> g b

instance Bifunctor (,) where
  bimapM f g (a, b) = (,) <$> f a <*> g b

instance Bifunctor ((,,) x1) where
  bimapM f g (x1, x2, x3) = (,,) x1 <$> f x2 <*> g x3
