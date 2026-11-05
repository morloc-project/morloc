{- |
Module      : Morloc.Data.Zip
Description : Pairing lists by position
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A plain zip drops the tail of the longer list, so a pairing that two lists
of different lengths reach silently loses data. The pairings here say what
they expect: equal lengths (a difference is a compiler defect), a prefix
(the shorter list decides), or positions.
-}
module Morloc.Data.Zip
  ( zipSame
  , zipWithSame
  , zip3Same
  , zipWith3Same
  , zipWithSameM
  , zipWithSameM_
  , zipWithPrefixM
  , zipWithPrefixM_
  , zip3Indexed
  , zipPrefix
  , zipWithPrefix
  , zip4Same
  , zip3Prefix
  , indexed
  ) where

import Data.Foldable (traverse_)
import Data.List (zipWith4)
import GHC.Stack (HasCallStack, withFrozenCallStack)

-- | The lists are walked together, so an infinite one meets the end of a
-- finite one rather than being measured.
sameLength :: HasCallStack => [a] -> [b] -> c -> c
sameLength xs ys r
  | together xs ys = r
  | otherwise = withFrozenCallStack (error "morloc bug: paired lists differ in length")
  where
    together (_ : as) (_ : bs) = together as bs
    together [] [] = True
    together _ _ = False

-- | Pair two lists that are equally long by construction.
zipSame :: HasCallStack => [a] -> [b] -> [(a, b)]
zipSame xs ys = withFrozenCallStack (sameLength xs ys (zip xs ys))

zipWithSame :: HasCallStack => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithSame f xs ys = withFrozenCallStack (sameLength xs ys (zipWith f xs ys))

zip3Same :: HasCallStack => [a] -> [b] -> [c] -> [(a, b, c)]
zip3Same xs ys zs = withFrozenCallStack (sameLength xs ys (sameLength ys zs (zip3 xs ys zs)))

zipWith3Same :: HasCallStack => (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3Same f xs ys zs = withFrozenCallStack (sameLength xs ys (sameLength ys zs (zipWith3 f xs ys zs)))

zip4Same :: HasCallStack => [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4Same as bs cs ds = withFrozenCallStack (sameLength as bs (sameLength bs cs (sameLength cs ds (zipWith4 (,,,) as bs cs ds))))

zipWithSameM :: (HasCallStack, Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithSameM f xs ys = withFrozenCallStack (sameLength xs ys (traverse (uncurry f) (zip xs ys)))

zipWithSameM_ :: (HasCallStack, Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithSameM_ f xs ys = withFrozenCallStack (sameLength xs ys (traverse_ (uncurry f) (zip xs ys)))

-- | Positions from @n@, with two equally long lists.
zip3Indexed :: HasCallStack => Int -> [a] -> [b] -> [(Int, a, b)]
zip3Indexed n xs ys = withFrozenCallStack (sameLength xs ys (zip3 [n ..] xs ys))

-- | Pair a list with a prefix of another: the shorter list decides, by
-- intent.
zipPrefix :: [a] -> [b] -> [(a, b)]
zipPrefix = zip

zipWithPrefix :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPrefix = zipWith

zipWithPrefixM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithPrefixM f xs ys = traverse (uncurry f) (zip xs ys)

zipWithPrefixM_ :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithPrefixM_ f xs ys = traverse_ (uncurry f) (zip xs ys)

zip3Prefix :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3Prefix = zip3

-- | Each element with its 0-based position.
indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]
