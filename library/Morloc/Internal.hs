{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

{-|
Module      : Morloc.Internal
Description : Internal utility functions
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module serves as a proto-prelude. Eventually I will probably want to
abandon the default prelude and create my own. But not just yet.
-}
module Morloc.Internal
  ( ifelse
  , concatMapM
  , unique
  , duplicates
  , module Data.Maybe
  , module Data.Either
  , module Data.List.Extra
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Monoid
  , module Data.Bifunctor
  , module Data.Traversable
  -- Data.Char characters
  , isUpper
  , isLower
  -- ** selected functions from Data.Foldable
  , foldlM
  , foldrM
  -- ** extra Map functions
  , mapKeysM
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
  , catEither
  , eitherM
  , eitherBimapM
  -- ** safe versions of errant functions
  , module Safe
  , maximumOnMay
  , minimumOnMay
  , maximumOnDef
  , minimumOnDef
  -- ** other useful functions
  , statefulMap
  , statefulMapM
  , filterApart
  -- ** Zip functions that fail on inputs of unequal length. These should be
  -- used when unequal lengths implies a compiler bug. In a better world, the
  -- typechecker would catch these issues before failing here.
  , equalZip
  , equalZipWith
  , equalZipWithM
  ) where

-- Don't import anything from Morloc here. This module should be VERY lowest
-- in the hierarchy, to avoid circular dependencies, since the lexer needs to
-- access it.
import Prelude hiding(mapM) -- replace Prelude mapM for lists with general traverable map
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.Either
import Data.Foldable (foldlM, foldrM)
import Data.List.Extra hiding (list) -- 'list' conflicts with Doc
import Data.Tuple.Extra ((***), (&&&))
import Data.Maybe
import Data.Monoid
import Data.Bifunctor
import Data.Traversable
import Data.Char (isUpper, isLower)
import Safe hiding (at, headDef, lastDef)
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
curry3 f x y z = f (x, y, z)

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

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | remove duplicated elements in a list while preserving order
unique :: Ord a => [a] -> [a]
unique = unique' Set.empty where 
  unique' _   [] = []
  unique' set (x:xs)
    | Set.member x set = unique' set xs
    | otherwise = x : unique' (Set.insert x set) xs

-- | Build an ordered list of duplicated elements
duplicates :: Ord a => [a] -> [a] 
duplicates xs = unique $ filter isDuplicated xs where
  -- countMap :: Ord a => Map.Map a Int
  countMap = Map.fromList . map (\ks -> (head ks, length ks)) . group . sort $ xs

  -- isDuplicated :: Ord a => a -> Bool
  isDuplicated k = fromJust (Map.lookup k countMap) > 1

statefulMap :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
statefulMap _ s [] = (s, [])
statefulMap f s0 (x:xs) =
  let (s1, y) = f s0 x
  in  let (sn, ys) = statefulMap f s1 xs
      in (sn, y:ys)

statefulMapM :: Monad m => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
statefulMapM _ s [] = return (s, [])
statefulMapM f s (x:xs) = do
  (s', x') <- f s x
  (s'', xs') <- statefulMapM f s' xs
  return (s'', x':xs')


-- pull one element from a list
filterApart :: (a -> Bool) -> [a] -> (Maybe a, [a])
filterApart _ [] = (Nothing, [])
filterApart f (x:xs)
  | f x = (Just x, xs)  
  | otherwise = case filterApart f xs of 
    (r, xs') -> (r, x:xs') 

equalZip :: [a] -> [b] -> [(a, b)]
equalZip (x:xs) (y:ys) = (x,y) : equalZip xs ys
equalZip [] [] = []
equalZip xs ys = error $ "Unequal lengths in equalZip: xs=" <> show (length xs) <> " ys=" <> show (length ys)

equalZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
equalZipWith f xs ys
    | length xs == length ys = zipWith f xs ys
    | otherwise = error $ "Unequal lengths in equalZipWith: xs=" <> show (length xs) <> " ys=" <> show (length ys)

equalZipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c] 
equalZipWithM f xs ys
    | length xs == length ys = zipWithM f xs ys
    | otherwise = error $ "Unequal lengths in equalZipWith: xs=" <> show (length xs) <> " ys=" <> show (length ys)

mapKeysM :: (Ord k', Monad m) => (k -> m k') -> Map.Map k v -> m (Map.Map k' v)
mapKeysM f x = do
    let (keys, vals) = unzip $ Map.toList x
    keys' <- mapM f keys
    return . Map.fromList $ zip keys' vals

catEither :: Either a a -> a
catEither (Left x) = x
catEither (Right x) = x

eitherBimapM :: Monad m => (a -> m a') -> (b -> m b') -> Either a b -> m (Either a' b')
eitherBimapM f _ (Left a) = Left <$> f a
eitherBimapM _ g (Right a) = Right <$> g a

eitherM :: (a -> m c) -> (b -> m c) -> Either a b -> m c
eitherM f _ (Left a) = f a
eitherM _ g (Right a) = g a

-- | pipe the lhs functor into the rhs function
infixl 1 |>>

(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
