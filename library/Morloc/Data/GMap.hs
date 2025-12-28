{-|
Module      : Morloc.Data.GMap
Description : A general map datatype (non-injective and non-surjective)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.GMap
  ( elems
  , empty
  , innerKeys
  , insert
  , insertWith
  , insertWithM
  , change
  , insertMany
  , insertManyWith
  , keys
  , lookup
  , mapInnerKeys
  , mapKeys
  , mapVals
  , mapValsM
  , mapValsWithKeyM
  , yIsX
  ) where

import Morloc.Namespace hiding (insert, lookup)
import Prelude hiding (lookup)
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set

mapVals :: (c -> c') -> GMap a b c -> GMap a b c'
mapVals f (GMap x y) = GMap x (Map.map f y)

mapValsM :: (Ord b, Monad m) => (c -> m c') -> GMap a b c -> m (GMap a b c')
mapValsM f (GMap m1 m2) = do
    let m2list = Map.toList m2
    xs2 <- mapM (f . snd) m2list
    return $ GMap m1 (Map.fromList (zip (map fst m2list) xs2))

mapValsWithKeyM :: (Ord b, Monad m) => (b -> c -> m c') -> GMap a b c -> m (GMap a b c')
mapValsWithKeyM f (GMap m1 m2) = do
    let m2list = Map.toList m2
    xs2 <- mapM (uncurry f) m2list
    return $ GMap m1 (Map.fromList (zip (map fst m2list) xs2))

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

insertWith :: (Ord a, Ord b) => (c -> c -> c) -> a -> b -> c -> GMap a b c -> GMap a b c
insertWith f k1 k2 x (GMap m1 m2) = GMap (Map.insert k1 k2 m1) (Map.insertWith f k2 x m2)

insertWithM :: (Monad m, Ord a, Ord b) => (c -> c -> m c) -> a -> b -> c -> GMap a b c -> m (GMap a b c)
insertWithM f k1 k2 x1 (GMap m1 m2) = do
  let map1 = Map.insert k1 k2 m1
  x3 <- case Map.lookup k2 m2 of
    (Just x2) -> f x1 x2
    Nothing -> return x1
  return $ GMap map1 (Map.insert k2 x3 m2)

-- | Given an outer key, change the inner value. This may change the values
-- associated with many other outer keys.
change :: (Ord a, Ord b) => a -> c -> GMap a b c -> Maybe (GMap a b c)
change k1 v (GMap x y) = do
  k2 <- Map.lookup k1 x
  return $ GMap x (Map.insert k2 v y)

insertMany :: (Ord a, Ord b) => [a] -> b -> c -> GMap a b c -> GMap a b c
insertMany ks k2 x (GMap m1 m2) = GMap m1' m2' where
  m1' = Map.union (Map.fromList (zip ks (repeat k2))) m1
  m2' = Map.insert k2 x m2

insertManyWith :: (Ord a, Ord b) => (c -> c -> c) -> [a] -> b -> c -> GMap a b c -> GMap a b c
insertManyWith f ks k2 x (GMap m1 m2) = GMap m1' m2' where
  m1' = Map.union (Map.fromList (zip ks (repeat k2))) m1
  m2' = Map.insertWith f k2 x m2

-- | Given `yIsX gmap x y`, the value `y` points to will be replaced with the
-- value `x` points to. If `x` is not in `gmap`, then Nothing is returned. If
-- `y` is in `gmap`, its previous link is silently lost.
yIsX :: (Ord a) => a -> a -> GMap a b c -> Maybe (GMap a b c)
yIsX  oldKey newKey (GMap m x) = do
  i <- Map.lookup oldKey m
  return (GMap (Map.insert newKey i m) x)

lookup :: (Ord a, Ord b) => a -> GMap a b c -> GMapRet c
lookup k1 (GMap m1 m2) =
  case Map.lookup k1 m1 of
    Nothing -> GMapNoFst
    (Just k2) -> case Map.lookup k2 m2 of
      Nothing -> GMapNoSnd
      (Just x) -> GMapJust x
