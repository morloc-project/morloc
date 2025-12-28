{-|
Module      : Morloc.Data.Rose
Description : The Rose tree data structure used for scoping in the parser
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : Apache-2.0
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.Rose
  ( Rose(..)
  , search
  , filter
  , mapScope
  , flatten
  , prune
  ) where

data Rose a = Nil | Rose a [Rose a]

instance Functor Rose where
  fmap f (Rose x xs) = Rose (f x) (map (fmap f) xs)
  fmap _ Nil = Nil


-- | Find all values that match a predicate
search :: (a -> Bool) -> Rose a -> [a]
search f = filter f . flatten


-- | Make list of values
flatten :: Rose a -> [a]
flatten Nil = []
flatten (Rose x xs) = x : concatMap flatten xs


-- | Remove branches where the value of the predicate returns true
prune :: (a -> Bool) -> Rose a -> Rose a
prune _ Nil = Nil
prune f (Rose x xs) = Rose x [prune f t | t@(Rose x' _) <- xs, not (f x')]


-- | Map with folded context from the local scope. Given `Rose x xs`, each `x`
-- is evaluated in the context of the immediate chilren, siblings, ancestors
-- back to root, and all aunts back to root.
mapScope
  :: Monoid b
  => (b -> a -> b) -- fold an element into scope context
  -> (a -> b -> c) -- update a value given scope context
  -> b  -- ^ initial element
  -> Rose a -- ^ input tree
  -> Rose c
mapScope _ _ _ Nil = Nil
mapScope f g b (Rose x kids) =
  let bn = foldl f mempty [k | Rose k _ <- kids] <> b
  in Rose (g x bn) (map (mapScope f g bn) kids)
