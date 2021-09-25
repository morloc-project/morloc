{-|
Module      : Morloc.Data.GMap
Description : A general map datatype (non-injective and non-surjective)
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.GMap
  ( elems
  , empty
  , innerKeys
  , insert
  , insertMany
  , keys
  , lookup
  , mapInnerKeys
  , mapKeys
  , mapVals
  , yIsX
  ) where

import Morloc.Namespace hiding (insert, lookup)
import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set

mapVals :: (c -> c') -> GMap a b c -> GMap a b c'
mapVals f (GMap x y) = GMap x (Map.map f y)

mapKeys :: (Ord a') => (a -> a') -> GMap a b c -> GMap a' b c
mapKeys f (GMap x y) = GMap (Map.mapKeys f x) y

mapInnerKeys :: (Ord b') => (b -> b') -> GMap a b c -> GMap a b' c 
mapInnerKeys f (GMap x y) = GMap (Map.map f x) (Map.mapKeys f y)

keys :: GMap a b c -> [a]
keys (GMap x _) = Map.keys x

innerKeys :: (Ord b) => GMap a b c -> (Set.Set b, Set.Set b)
innerKeys (GMap x y) = (Set.fromList (Map.elems x), Set.fromList (Map.keys y))

elems :: GMap a b c -> [c]
elems (GMap _ y) = Map.elems y

empty :: GMap a b c
empty = GMap Map.empty Map.empty

insert :: (Ord a, Ord b) => a -> b -> c -> GMap a b c -> GMap a b c
insert k1 k2 x (GMap m1 m2) = GMap (Map.insert k1 k2 m1) (Map.insert k2 x m2) 

insertMany :: (Ord a, Ord b) => [a] -> b -> c -> GMap a b c -> GMap a b c
insertMany ks k2 x (GMap m1 m2) = GMap m1' m2' where
  m1' = Map.union (Map.fromList (zip ks (repeat k2))) m1
  m2' = Map.insert k2 x m2

-- | Given `yIsX gmap x y`, the value `y` points to will be replaced with the
-- value `x` points to. If `x` is not in `gmap`, then Nothing is returned. If
-- `y` is in `gmap`, its previous link is silently lost.
yIsX :: (Ord a) => GMap a b c -> a -> a -> Maybe (GMap a b c)
yIsX (GMap m x) oldKey newKey = do
  i <- Map.lookup oldKey m
  return (GMap (Map.insert newKey i m) x)

lookup :: (Ord a, Ord b) => a -> GMap a b c -> GMapRet c
lookup k1 (GMap m1 m2) =
  case Map.lookup k1 m1 of
    Nothing -> GMapNoFst
    (Just k2) -> case Map.lookup k2 m2 of 
      Nothing -> GMapNoSnd
      (Just x) -> GMapJust x
