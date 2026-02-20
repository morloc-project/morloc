{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Morloc.Data.Annotated
Description : Class of annotated entities
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Typeclass for containers that pair an annotation with a value, providing
accessors and mapping operations over both components.
-}
module Morloc.Data.Annotated (Annotated (..)) where

class Annotated f where
  -- | Extract the annotation
  ann :: f a b -> a
  -- | Extract the value
  val :: f a b -> b
  -- | Pair an annotation with a value
  annotate :: a -> b -> f a b

  -- | Apply a function to both the annotation and the value
  annapp :: (a -> b -> c) -> f a b -> c
  annapp f x = f (ann x) (val x)

  -- | Replace the value using a function of both annotation and value
  reval :: (a -> b -> b') -> f a b -> f a b'
  reval f x = annotate (ann x) $ f (ann x) (val x)

  -- | Replace the annotation using a function of both annotation and value
  reann :: (a -> b -> a') -> f a b -> f a' b
  reann f x = annotate (f (ann x) (val x)) (val x)

  -- | Monadic 'annapp'
  annappM :: (Monad m) => (a -> b -> m c) -> f a b -> m c
  annappM f x = f (ann x) (val x)

  -- | Monadic 'reval'
  revalM :: (Monad m) => (a -> b -> m b') -> f a b -> m (f a b')
  revalM f x = annotate (ann x) <$> f (ann x) (val x)

  -- | Monadic 'reann'
  reannM :: (Monad m) => (a -> b -> m a') -> f a b -> m (f a' b)
  reannM f x = annotate <$> f (ann x) (val x) <*> pure (val x)

instance Annotated (,) where
  ann = fst
  val = snd
  annotate a b = (a, b)
