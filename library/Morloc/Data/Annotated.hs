{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Morloc.Data.Annotated
Description : Class of annotated entities
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.Annotated (Annotated(..)) where

class Annotated f where
  ann :: f a b -> a
  val :: f a b -> b
  annotate :: a -> b -> f a b

  annapp :: (a -> b -> c) -> f a b -> c
  annapp f x = f (ann x) (val x)

  reval :: (a -> b -> b') -> f a b -> f a b'
  reval f x = annotate (ann x) $ f (ann x) (val x)

  reann :: (a -> b -> a') -> f a b -> f a' b
  reann f x = annotate (f (ann x) (val x)) (val x)

  annappM :: (Monad m ) => (a -> b -> m c) -> f a b -> m c
  annappM f x = f (ann x) (val x)

  revalM :: Monad m => (a -> b -> m b') -> f a b -> m (f a b')
  revalM f x = annotate (ann x) <$> f (ann x) (val x)

  reannM :: Monad m => (a -> b -> m a') -> f a b -> m (f a' b)
  reannM f x = annotate <$> f (ann x) (val x) <*> pure (val x)

instance Annotated (,) where
  ann = fst
  val = snd
  annotate a b = (a, b)
