{- |
Module      : Morloc.Data.GMap
Description : A general map datatype (non-injective and non-surjective)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A two-level map @GMap a b c@ that maps outer keys @a@ to inner keys @b@, and
inner keys @b@ to values @c@. Multiple outer keys may share the same inner
key (and thus value). Used in the compiler for type-alias indirection where
many names can refer to the same underlying definition.
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

import qualified Data.Set as Set
import qualified Morloc.Data.Map as Map
import Morloc.Namespace.Prim hiding (insert, lookup)
import Prelude hiding (lookup)

-- | Map a function over all values
mapVals :: (c -> c') -> GMap a b c -> GMap a b c'
mapVals f (GMap x y) = GMap x (Map.map f y)

-- | Monadic 'mapVals'
mapValsM :: (Ord b, Monad m) => (c -> m c') -> GMap a b c -> m (GMap a b c')
mapValsM f (GMap m1 m2) = do
  let m2list = Map.toList m2
  xs2 <- mapM (f . snd) m2list
  return $ GMap m1 (Map.fromList (zip (map fst m2list) xs2))

-- | Monadic map over values with access to the inner key
mapValsWithKeyM :: (Ord b, Monad m) => (b -> c -> m c') -> GMap a b c -> m (GMap a b c')
mapValsWithKeyM f (GMap m1 m2) = do
  let m2list = Map.toList m2
  xs2 <- mapM (uncurry f) m2list
  return $ GMap m1 (Map.fromList (zip (map fst m2list) xs2))

-- | Map a function over outer keys
mapKeys :: (Ord a') => (a -> a') -> GMap a b c -> GMap a' b c
mapKeys f (GMap x y) = GMap (Map.mapKeys f x) y

-- | Map a function over inner keys (updating both the key mapping and value map)
mapInnerKeys :: (Ord b') => (b -> b') -> GMap a b c -> GMap a b' c
mapInnerKeys f (GMap x y) = GMap (Map.map f x) (Map.mapKeys f y)

-- | Get all outer keys
keys :: GMap a b c -> [a]
keys (GMap x _) = Map.keys x

-- | Get the sets of (mapped-to inner keys, all inner keys)
innerKeys :: (Ord b) => GMap a b c -> (Set.Set b, Set.Set b)
innerKeys (GMap x y) = (Set.fromList (Map.elems x), Set.fromList (Map.keys y))

-- | Get all values
elems :: GMap a b c -> [c]
elems (GMap _ y) = Map.elems y

-- | An empty 'GMap'
empty :: GMap a b c
empty = GMap Map.empty Map.empty

-- | Insert a mapping from outer key to inner key to value
insert :: (Ord a, Ord b) => a -> b -> c -> GMap a b c -> GMap a b c
insert k1 k2 x (GMap m1 m2) = GMap (Map.insert k1 k2 m1) (Map.insert k2 x m2)

-- | Insert with a combining function for values at colliding inner keys
insertWith :: (Ord a, Ord b) => (c -> c -> c) -> a -> b -> c -> GMap a b c -> GMap a b c
insertWith f k1 k2 x (GMap m1 m2) = GMap (Map.insert k1 k2 m1) (Map.insertWith f k2 x m2)

-- | Monadic 'insertWith'
insertWithM ::
  (Monad m, Ord a, Ord b) => (c -> c -> m c) -> a -> b -> c -> GMap a b c -> m (GMap a b c)
insertWithM f k1 k2 x1 (GMap m1 m2) = do
  let map1 = Map.insert k1 k2 m1
  x3 <- case Map.lookup k2 m2 of
    (Just x2) -> f x1 x2
    Nothing -> return x1
  return $ GMap map1 (Map.insert k2 x3 m2)

{- | Given an outer key, replace its associated value. Since multiple outer
keys may share the same inner key, this affects all of them.
-}
change :: (Ord a, Ord b) => a -> c -> GMap a b c -> Maybe (GMap a b c)
change k1 v (GMap x y) = do
  k2 <- Map.lookup k1 x
  return $ GMap x (Map.insert k2 v y)

-- | Insert multiple outer keys that all map to the same inner key and value
insertMany :: (Ord a, Ord b) => [a] -> b -> c -> GMap a b c -> GMap a b c
insertMany ks k2 x (GMap m1 m2) = GMap m1' m2'
  where
    m1' = Map.union (Map.fromList (zip ks (repeat k2))) m1
    m2' = Map.insert k2 x m2

-- | 'insertMany' with a combining function for colliding inner keys
insertManyWith :: (Ord a, Ord b) => (c -> c -> c) -> [a] -> b -> c -> GMap a b c -> GMap a b c
insertManyWith f ks k2 x (GMap m1 m2) = GMap m1' m2'
  where
    m1' = Map.union (Map.fromList (zip ks (repeat k2))) m1
    m2' = Map.insertWith f k2 x m2

-- | Make @newKey@ point to the same inner key as @oldKey@. Returns 'Nothing'
-- if @oldKey@ is absent.
yIsX :: (Ord a) => a -> a -> GMap a b c -> Maybe (GMap a b c)
yIsX oldKey newKey (GMap m x) = do
  i <- Map.lookup oldKey m
  return (GMap (Map.insert newKey i m) x)

-- | Two-phase lookup: outer key to inner key, then inner key to value
lookup :: (Ord a, Ord b) => a -> GMap a b c -> GMapRet c
lookup k1 (GMap m1 m2) =
  case Map.lookup k1 m1 of
    Nothing -> GMapNoFst
    (Just k2) -> case Map.lookup k2 m2 of
      Nothing -> GMapNoSnd
      (Just x) -> GMapJust x
