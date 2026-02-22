{- |
Module      : Morloc.Data.Map.Extra
Description : Additional functions for the Map class
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Monadic extensions to "Data.Map.Strict" that are not provided by the standard
library: monadic union, map, and three-way merge operations.
-}
module Morloc.Data.Map.Extra
  ( mergeMaps
  , mergeMapsM
  , mapKeysWithM
  , mapM
  , mapWithKeyM
  , unionWithM
  , unionsWithM
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.List.Extra (groupSort)
import qualified Data.Map.Strict as Map
import Prelude hiding (mapM)
import qualified Prelude

onSndM :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
onSndM f (x, y) = (,) x <$> f y

-- | Monadic version of @Data.Map.unionWith@
unionWithM :: (Monad m, Ord a) => (b -> b -> m b) -> Map.Map a b -> Map.Map a b -> m (Map.Map a b)
unionWithM f m1 m2 = do
  pairs <- Prelude.mapM (onSndM (uncurry f)) (Map.toList $ Map.intersectionWith (,) m1 m2)

  return $
    Map.unions
      [ Map.difference m1 m2
      , Map.fromList pairs
      , Map.difference m2 m1
      ]

-- | Monadic version of @Data.Map.unionsWith@
unionsWithM :: (Monad m, Ord a) => (b -> b -> m b) -> [Map.Map a b] -> m (Map.Map a b)
unionsWithM f = foldM (unionWithM f) Map.empty

-- | Monadic version of @Data.Map.mapKeysWith@, merging values when keys collide
mapKeysWithM ::
  (Monad m, Ord k2) =>
  (a -> a -> m a) ->
  (k1 -> k2) ->
  Map.Map k1 a ->
  m (Map.Map k2 a)
mapKeysWithM f g m =
  Map.fromList
    <$> Prelude.mapM
      foldValues
      (groupSort $ map (first g) (Map.toList m))
  where
    foldValues (k, v : vs) = (,) k <$> foldM f v vs
    foldValues _ = undefined -- groupSort never produces empty value lists

-- | Monadic version of @Data.Map.map@
mapM :: (Monad m) => (a -> m b) -> Map.Map k a -> m (Map.Map k b)
mapM f = Map.traverseWithKey (\_ a -> f a)

-- | Monadic version of @Data.Map.mapWithKey@
mapWithKeyM :: (Monad m) => (k -> a -> m b) -> Map.Map k a -> m (Map.Map k b)
mapWithKeyM = Map.traverseWithKey

{- | Three-way merge of two maps: apply @fb@ to left-only, @fc@ to right-only,
and @fbc@ to entries present in both
-}
mergeMaps ::
  (Ord a) =>
  (b -> d) ->
  (c -> d) ->
  (b -> c -> d) ->
  Map.Map a b ->
  Map.Map a c ->
  Map.Map a d
mergeMaps fb fc fbc m1 m2 =
  Map.unions
    [ Map.map fb (Map.difference m1 m2)
    , Map.mapMaybeWithKey (\k v -> fbc v <$> Map.lookup k m2) (Map.intersection m1 m2)
    , Map.map fc (Map.difference m2 m1)
    ]

-- | Monadic version of 'mergeMaps'
mergeMapsM ::
  (Ord a, Monad m) =>
  (b -> m d) ->
  (c -> m d) ->
  (b -> c -> m d) ->
  Map.Map a b ->
  Map.Map a c ->
  m (Map.Map a d)
mergeMapsM fb fc fbc m1 m2 = do
  bs <- mapM fb $ Map.difference m1 m2
  bcs <- mapM (uncurry fbc) $ Map.intersectionWith (,) m1 m2
  cs <- mapM fc $ Map.difference m2 m1
  return $ Map.unions [bs, bcs, cs]
