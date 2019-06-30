{-|
Module      : Morloc.Util
Description : Miscellaneous small utility functions
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Util
(
    ifelse
  , conmap
  , unique
  , sort
  , repeated
  , maybe2bool
  , either2bool
  , maybeOne
  , zipWithOrDie
  , shareAttr
  , spreadAttr
  , groupSortWith
) where

import qualified Data.List as DL
import qualified Data.List.Extra as DLE
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

conmap :: (a -> [b]) -> [a] -> [b]
conmap f = concat . map f

ifelse :: Bool -> a -> a -> a
ifelse True  x _ = x
ifelse False _ y = y

unique :: Eq a => [a] -> [a]
unique = DL.nub

sort :: Ord a => [a] -> [a]
sort = DL.sort

repeated :: Ord a => [a] -> [a]
repeated xs = [y | (y:(_:_)) <- (DL.group . DL.sort) xs]

maybe2bool :: Maybe a -> Bool
maybe2bool (Just _) = True
maybe2bool Nothing = False

either2bool :: Either a b -> Bool
either2bool (Left _) = False
either2bool (Right _) = True

maybeOne :: [a] -> Maybe a
maybeOne [x] = Just x
maybeOne _  = Nothing

groupSortWith :: (Monad m, Ord c) => ([a] -> m b) -> [(c, a)] -> m [(c, b)]
groupSortWith f = mapM (\(x,xs) -> (,) <$> pure x <*> f xs) . DLE.groupSort

zipWithOrDie :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithOrDie f xs ys
  | length xs == length ys = zipWith f xs ys
  | otherwise = error "Expected lists of equal length"

spreadAttr
  :: (Ord a, Ord b)
  => [(a, Maybe a, Maybe b)]
  -> Map.Map a b
spreadAttr xs = asHash [(Set.toList sx, Set.toList sy) | (sx, sy) <- shareAttr xs]
  where
    asHash
      :: Ord a
      => [([a], [b])] -> Map.Map a b
    asHash xys =
      (Map.fromList . concat)
      [[(x, y) | x <- xs', y <- ys'] | (xs', ys') <- xys]

-- | For the list of tuples (l,r,c), find missing data that is present in a
-- related member. All l and r that are linked are considered to be in the same
-- group. At least one must have a defined c. This c is then assigned to all
-- other members of the set.
shareAttr :: (Ord a, Ord b) => [(a, Maybe a, Maybe b)] -> [(Set.Set a, Set.Set b)]
shareAttr xs = foldl f [] xs where

  f :: (Ord a, Ord b) => [(Set.Set a, Set.Set b)]
    -> (a, Maybe a, Maybe b)
    -> [(Set.Set a, Set.Set b)]
  f ((s, c):ss) (x, y, z) =
    if
      Set.member x s || maybeMember y s
    then
      (maybeInsert y (Set.insert x s), maybeInsert z c):ss
    else
      (s, c):(f ss (x,y,z))
  f [] (x, y, z) = [(maybeInsert y (Set.singleton x), maybeSingleton z)]

  maybeMember :: Ord a => Maybe a -> Set.Set a -> Bool
  maybeMember (Just x) s = Set.member x s
  maybeMember _ _ = False

  maybeInsert :: Ord a => Maybe a -> Set.Set a -> Set.Set a
  maybeInsert (Just x) s = Set.insert x s 
  maybeInsert _ s = s

  maybeSingleton :: Maybe c -> Set.Set c
  maybeSingleton (Just x) = Set.singleton x
  maybeSingleton _ = Set.empty
