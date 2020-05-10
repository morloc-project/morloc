{-|
Module      : Morloc.Internal
Description : Internal utility functions
Copyright   : (c) Zebulun Arendsee, 2020
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
  , duplicates
  , isSorted
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
  -- ** safe versions of errant functions
  , module Safe
  , maximumOnMay
  , minimumOnMay
  , maximumOnDef
  , minimumOnDef
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
import Safe hiding (at)
import System.FilePath
import qualified Data.Map as Map
import qualified Data.Set as Set

maximumOnMay :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOnMay _ [] = Nothing
maximumOnMay f xs = Just $ maximumOn f xs

minimumOnMay :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOnMay _ [] = Nothing
minimumOnMay f xs = Just $ minimumOn f xs

maximumOnDef :: Ord b => a -> (a -> b) -> [a] -> a
maximumOnDef x _ [] = x
maximumOnDef _ f xs = maximumOn f xs

minimumOnDef :: Ord b => a -> (a -> b) -> [a] -> a
minimumOnDef x _ [] = x
minimumOnDef _ f xs = minimumOn f xs

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

-- | remove duplicated elements in a list while preserving order
unique :: Ord a => [a] -> [a]
unique xs = unique' Set.empty xs where 
  unique' set [] = []
  unique' set (x:xs)
    | Set.member x set = unique' set xs
    | otherwise = x : unique' (Set.insert x set) xs

-- | Build an ordered list of duplicated elements
duplicates :: Ord a => [a] -> [a] 
duplicates xs = filter isDuplicated xs where
  -- countMap :: Ord a => Map.Map a Int
  countMap = Map.fromList . map (\(k:ks) -> (k, length ks + 1)) . group . sort $ xs

  -- isDuplicated :: Ord a => a -> Bool
  isDuplicated k = fromJust (Map.lookup k countMap) > 1

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:(y:xs)) = x < y && isSorted (y:xs) 

-- | pipe the lhs functor into the rhs function
infixl 1 |>>

(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
