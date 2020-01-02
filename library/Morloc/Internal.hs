{-|
Module      : Morloc.Internal
Description : Internal utility functions
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module serves as a proto-prelude. Eventually I will probably want to
abandon the default prelude and create my own. But not just yet.
-}
module Morloc.Internal
  ( ifelse
  , conmap
  , unique
  , module Data.Maybe
  , module Data.Either
  , module Data.List.Extra
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Monoid
  -- ** selected functions from Data.Tuple.Extra
  , uncurry3
  , curry3
  , third
  -- ** operators
  , (|>>) -- piped fmap
  , (</>) -- Filesystem utility operators from System.FilePath
  , (<|>) -- alternative operator
  , (&&&) -- (a -> a') -> (b -> b') -> (a, b) -> (a', b')
  , (***) -- (a -> b) -> (a -> c) -> a -> (b, c) 
  -- ** map and set helper functions
  , keyset
  , valset
  , mapFold
  , mapSum
  , mapSumWith
  ) where

-- Don't import anything from Morloc here. This module should be VERY lowest
-- in the hierarchy, to avoid circular dependencies, since the lexer needs to
-- access it.
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.Either
import Data.List.Extra hiding (list) -- 'list' conflicts with Doc
import Data.Tuple.Extra ((***), (&&&))
import Data.Maybe
import Data.Monoid
import System.FilePath
import qualified Data.Map as Map
import qualified Data.Set as Set

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d 
uncurry3 f (x, y, z) = f x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f = \x y z -> f (x, y, z)

third :: (a, b, c) -> c
third (_, _, x) = x

keyset :: Ord k => Map.Map k b -> Set.Set k
keyset = Set.fromList . Map.keys

valset :: Ord b => Map.Map k b -> Set.Set b
valset = Set.fromList . Map.elems

mapFold :: Monoid b => (a -> b -> b) -> Map.Map k a -> b
mapFold f = Map.foldr f mempty

mapSum :: Monoid a => Map.Map k a -> a
mapSum = Map.foldr mappend mempty

mapSumWith :: Monoid b => (a -> b) -> Map.Map k a -> b
mapSumWith f = Map.foldr (\x y -> mappend y (f x)) mempty

ifelse :: Bool -> a -> a -> a
ifelse True x _ = x
ifelse False _ y = y

conmap :: (a -> [b]) -> [a] -> [b]
conmap f = concat . map f

-- of course nub could be used, but this should be faster for large lists
unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- | pipe the lhs functor into the rhs function
infixl 1 |>>

(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
