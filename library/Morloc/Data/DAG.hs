{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Data.DAG
Description : Functions for working with directed acyclic graphs
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Operations on DAGs represented as @Map k (n, [(k, e)])@ where @k@ is the
node key, @n@ is node data, and @e@ is edge data. Used throughout the
compiler to represent module dependency graphs.
-}
module Morloc.Data.DAG
  ( edgelist
  , nodes
  , lookupNode
  , roots
  , mapNode
  , mapNodeM
  , mapNodeWithKeyM
  , mapEdge
  , synthesize
  , synthesizeNodes
  ) where

import qualified Data.Set as Set
import qualified Morloc.Data.Map as Map
import Morloc.Namespace.Prim
import Morloc.Namespace.State (MorlocMonad)

-- | Get all edges as @(source, target)@ pairs
edgelist :: DAG k e n -> [(k, k)]
edgelist d = concat [[(k, j) | (j, _) <- xs] | (k, (_, xs)) <- Map.toList d]

-- | Get all node values
nodes :: DAG k e n -> [n]
nodes = map fst . Map.elems

-- | Look up the node data for a given key
lookupNode :: (Ord k) => k -> DAG k e n -> Maybe n
lookupNode k d = case Map.lookup k d of
  (Just (n, _)) -> Just n
  Nothing -> Nothing

-- | Get all root keys (nodes with no incoming edges)
roots :: (Ord k) => DAG k e n -> [k]
roots d = Set.toList $ Set.difference parents children
  where
    g = edgelist d
    parents = Map.keysSet d
    children = Set.fromList (map snd g)

-- | Map a pure function over node data, leaving edges unchanged
mapNode :: (n1 -> n2) -> DAG k e n1 -> DAG k e n2
mapNode f = Map.map (first f)

-- | Map a monadic function over node data, leaving edges unchanged
mapNodeM :: (n1 -> MorlocMonad n2) -> DAG k e n1 -> MorlocMonad (DAG k e n2)
mapNodeM f = Map.mapM (\(n, xs) -> f n >>= (\n' -> return (n', xs)))

-- | Map a monadic function over node data with access to the key
mapNodeWithKeyM :: (k -> n1 -> MorlocMonad n2) -> DAG k e n1 -> MorlocMonad (DAG k e n2)
mapNodeWithKeyM f = Map.mapWithKeyM (\k (n, xs) -> f k n >>= (\n' -> return (n', xs)))

-- | Map a pure function over edge data, leaving nodes unchanged
mapEdge :: (e1 -> e2) -> DAG k e1 n -> DAG k e2 n
mapEdge f = Map.map (\(n, xs) -> (n, [(k, f e) | (k, e) <- xs]))

-- | Like 'synthesize' but keeps original edge data unchanged
synthesizeNodes ::
  (Ord k, Monad m) =>
  (k -> n1 -> [(k, e, n2)] -> m n2) ->
  DAG k e n1 ->
  m (Maybe (DAG k e n2))
synthesizeNodes f = synthesize f (\e _ _ -> return e)

{- | Bottom-up synthesis over a DAG: compute new node and edge values where
each node's new value depends on its children's already-computed values.
Returns 'Nothing' if the DAG contains cycles (detected by fixed-point
stall).
-}
synthesize ::
  (Ord k, Monad m) =>
  (k -> n1 -> [(k, e1, n2)] -> m n2) ->
  (e1 -> n2 -> n2 -> m e2) ->
  DAG k e1 n1 ->
  m (Maybe (DAG k e2 n2))
synthesize f fe d0 = go (Just Map.empty)
  where
    -- Iteratively process nodes whose children are all resolved
    go Nothing = return Nothing
    go (Just processed)
      | Map.size d0 == Map.size processed = return (Just processed)
      | otherwise = do
          processed' <- foldlM addIfReady processed (Map.toList d0)
          if Map.size processed' == Map.size processed
            -- No progress means a cycle
            then return Nothing
            else go (Just processed')

    -- Leaf nodes: no children to wait for
    addIfReady processed (key, (nodeVal, []))
      | Map.member key processed = return processed
      | otherwise = do
          newNode <- f key nodeVal []
          return $ Map.insert key (newNode, []) processed
    -- Interior nodes: all children must be processed first
    addIfReady processed (key, (nodeVal, childEdges))
      | Map.member key processed = return processed
      | otherwise = case mapM ((`Map.lookup` processed) . fst) childEdges of
          Nothing -> return processed
          (Just resolvedChildren) -> do
            let augmented = [(k, e, n2) | ((k, e), (n2, _)) <- zip childEdges resolvedChildren]
            newNode <- f key nodeVal augmented
            newEdges <- mapM (\((k2, e), (childNode, _)) -> (,) k2 <$> fe e newNode childNode) (zip childEdges resolvedChildren)
            return $ Map.insert key (newNode, newEdges) processed
