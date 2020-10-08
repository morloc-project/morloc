{-|
Module      : Morloc.Data.DAG
Description : Functions for working with directed acyclic graphs
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.DAG
  ( insertNode
  , insertNodeMaybe
  , insertEdge
  , member
  , keysSet
  , keys
  , edges
  , nodes
  , roots
  , leafs
  , findCycle
  , mapNode
  , mapEdge
  , mapEdgeWithNode
  , mapNodeWithEdge
  ) where

import Morloc.Namespace
import qualified Data.Map as Map 
import qualified Data.Set as Set 

-- | The ways a DAG may go wrong
data DagError k
  = DagHasCycle
  | DagHasUndefinedKey [k]
  | DagHasConflictingKey [k]

-- | Determine if a key is in the DAG. The key needn't be in the graph, it may be
-- only in the data map (i.e., it is a singleton).
member :: Ord k => k -> DAG k e n -> Bool
member k (DAG _ d) = Map.member k d 

insertNode :: Ord k => k -> n -> DAG k e n -> DAG k e n
insertNode k n (DAG g d) = DAG g (Map.insert k n d)

insertNodeMaybe :: Ord k => k -> n -> DAG k e n -> Maybe (DAG k e n)
insertNodeMaybe k n (DAG g d)
  | Map.member k d = Nothing
  | otherwise = Just $ DAG g (Map.insert k n d)

insertEdge :: Ord k => k -> k -> e -> DAG k e n -> DAG k e n
insertEdge k1 k2 e (DAG g d) = DAG (Map.alter f k1 g) d
  where  
    -- f :: Maybe [(k, e)] -> Maybe [(k, e)]
    f Nothing = Just [(k2, e)]
    f (Just xs) = Just $ (k2,e):xs

-- | Get set of all keys
keysSet :: Ord k => DAG k e n -> Set.Set k
keysSet (DAG g d) = Set.union (Map.keysSet g) (Map.keysSet d)

-- | Get list of all keys
keys :: Ord k => DAG k e n -> [k]
keys = Set.toList . keysSet

-- | Get all edges
edges :: Ord k => DAG k e n -> [e]
edges (DAG g _) = map snd (concat $ Map.elems g)

-- | Get all nodes
nodes :: Ord k => DAG k e n -> [n]
nodes (DAG _ d) = Map.elems d

-- | Get all roots
roots :: Ord k => DAG k e n -> [k]
roots (DAG d _) = Set.toList $ Set.difference parents children
  where
    children = Set.fromList . map fst . concat . Map.elems $ d
    parents = Map.keysSet d

-- | Get all leaves that have no children
leafs :: Ord k => DAG k e n -> [k]
leafs (DAG d _) = Set.toList $ Set.difference children parents
  where
    children = Set.fromList . map fst . concat . Map.elems $ d
    parents = Map.keysSet d


-- | Searches a DAG for a cycle, stops on the first observed cycle and returns
-- the path.
findCycle :: Ord k => DAG k e n -> Maybe [k]
findCycle d@(DAG g _) = case mapMaybe (findCycle' []) (roots d) of 
  [] -> Nothing
  (x:_) -> Just x
  where
    -- findCycle' :: [k] -> k -> Maybe [k]
    findCycle' seen k
      | elem k seen = Just seen
      | otherwise = case Map.lookup k g of
          Nothing -> Nothing -- we have reached a leaf
          (Just xs) -> case mapMaybe (findCycle' (k:seen)) (map fst xs) of
            [] -> Nothing
            (x:_) -> Just x

-- Map function over nodes independent of the edge data
mapNode :: (n1 -> n2) -> DAG k e n1 -> DAG k e n2
mapNode f (DAG g d) = DAG g (Map.map f d)

-- Map function over edges independent of the node data
mapEdge :: (e1 -> e2) -> DAG k e1 n -> DAG k e2 n
mapEdge f (DAG g d) = DAG (Map.map (map (\(x,y) -> (x, f y))) g) d

-- | map over edges given the nodes the edge connects
mapEdgeWithNode
  :: Ord k
  => (n -> e1 -> n -> e2)
  -> DAG k e1 n -> DAG k e2 n
mapEdgeWithNode f (DAG g d) = DAG (Map.mapWithKey runit g) d where
  -- runit :: Ord k => k -> [(k, a)] -> [(k, b)]
  runit k1 xs = case Map.lookup k1 d of 
    (Just n1) -> map (\(k2,e) -> (k2, f n1 e (fromJust (Map.lookup k2 d)))) xs
    Nothing -> error "Incomplete DAG"

-- | Map node data given edges and child data
mapNodeWithEdge
  :: Ord k
  => (n1 -> [(k, e, n1)] -> n2)
  -> DAG k e n1 -> DAG k e n2
mapNodeWithEdge f (DAG g d) = DAG g (Map.mapWithKey fkey d) where
  -- fkey :: k -> n1 -> n2
  fkey k1 n1 = case Map.lookup k1 g of
    Nothing -> f n1 []
    (Just xs) -> f n1 [(k2, e, fromJust (Map.lookup k2 d)) | (k2, e) <- xs]
