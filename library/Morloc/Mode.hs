module Morloc.Mode (Mode, asLIL) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Morloc.Graph
import Morloc.NodeAttribute

type Mode = Graph NodeAttr -> String

asLIL :: Graph NodeAttr -> String 
asLIL g = unlines $ foldr1 (++) $ parentChildMapI topLIL g where
  -- connect the parent to the top child 
  -- this function will be used by Graph.familyMap
  topLIL :: NodeAttr -> (Int, NodeAttr) -> String
  topLIL p (i, c) = intercalate "\t" [pval', pid', pos', typ', nam']
    where
    pos'  = show i
    pval' = fromMaybe "NO_VALUE" (nodeValue p)
    typ'  = fromMaybe "*"        (nodeType  c)
    nam'  = fromMaybe "NO_VALUE" (nodeValue c)
    pid'  = case nodeId p of
      Just x  -> show x
      Nothing -> "NO_ID"
