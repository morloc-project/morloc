{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Internal
Description : Proto-prelude re-exporting common utilities
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A project-wide prelude that re-exports commonly used modules (Data.Maybe,
Data.Either, Control.Monad, etc.) along with custom Bifunctor\/Bifoldable
typeclasses and utility functions. Imported transitively by nearly every
module via "Morloc.Namespace.Prim".

This module must NOT import anything from Morloc (other than Data.*) to
avoid circular dependencies, since the lexer depends on it.
-}
module Morloc.Internal
  ( concatMapM
  , unique
  , duplicates
  , module Data.Maybe
  , module Data.Either
  , module Data.List.Extra
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Monoid
  , module Data.Traversable
  , module Morloc.Data.Bifoldable
  , module Morloc.Data.Bifunctor
  , module Morloc.Data.Annotated
  , isLower
  , isUpper
  , toLower

    -- * Data.Foldable re-exports
  , foldlM
  , foldrM

    -- * Tuple utilities
  , return2

    -- * Operators
  , (|>>)
  , (</>)
  , (<|>)
  , (&&&)
  , (***)

    -- * Safe re-exports
  , module Safe

    -- * Stateful mapping
  , statefulMap
  , statefulMapM
  , statefulTraverseM
  , filterApart
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.State.Strict as CMS
import Data.Char (isLower, isUpper, toLower)
import Data.Either
import Data.Foldable (foldlM, foldrM)
import Data.List.Extra hiding (list)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Traversable
import Data.Tuple.Extra ((&&&), (***))
import Morloc.Data.Annotated
import Morloc.Data.Bifoldable
import Morloc.Data.Bifunctor
import Safe hiding (at, headDef, lastDef)
import System.FilePath
import Prelude hiding (mapM)

-- | Lift a binary function into a monadic return
return2 :: (Monad m) => (a -> b -> c) -> (a -> b -> m c)
return2 f x y = return $ f x y

-- | Concatenate the results of a monadic map
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | Remove duplicate elements while preserving first-occurrence order
unique :: (Ord a) => [a] -> [a]
unique = unique' Set.empty
  where
    unique' _ [] = []
    unique' set (x : xs)
      | Set.member x set = unique' set xs
      | otherwise = x : unique' (Set.insert x set) xs

-- | Return elements that appear more than once, in first-occurrence order
duplicates :: (Ord a) => [a] -> [a]
duplicates xs = unique $ filter isDuplicated xs
  where
    countMap = Map.fromList . map (\ks -> (head ks, length ks)) . group . sort $ xs
    isDuplicated k = fromJust (Map.lookup k countMap) > 1

-- | Map with threaded state
statefulMap :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
statefulMap _ s [] = (s, [])
statefulMap f s0 (x : xs) =
  let (s1, y) = f s0 x
   in let (sn, ys) = statefulMap f s1 xs
       in (sn, y : ys)

-- | Monadic 'statefulMap'
-- | 'statefulMapM' over any traversable.
statefulTraverseM :: (Monad m, Traversable t) => (s -> a -> m (s, b)) -> s -> t a -> m (s, t b)
statefulTraverseM f s0 t = do
  (t', s1) <- CMS.runStateT (traverse (\x -> CMS.StateT (fmap (\(s', y) -> (y, s')) . (`f` x))) t) s0
  return (s1, t')

statefulMapM :: (Monad m) => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
statefulMapM _ s [] = return (s, [])
statefulMapM f s (x : xs) = do
  (s', x') <- f s x
  (s'', xs') <- statefulMapM f s' xs
  return (s'', x' : xs')

-- | Extract the first element matching a predicate, returning it and the rest
filterApart :: (a -> Bool) -> [a] -> (Maybe a, [a])
filterApart _ [] = (Nothing, [])
filterApart f (x : xs)
  | f x = (Just x, xs)
  | otherwise = case filterApart f xs of
      (r, xs') -> (r, x : xs')

-- | Piped fmap: @x |>> f == fmap f x@
infixl 1 |>>

(|>>) :: (Functor f) => f a -> (a -> b) -> f b
(|>>) = flip fmap
