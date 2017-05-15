module Morloc.Mode (Mode, asLIL) where

import Data.List (intercalate)
import Morloc.Graph
import Morloc.NodeAttribute

type Mode = Graph NodeAttr -> String

asLIL :: Graph NodeAttr -> String 
asLIL = unlines . concat . toList . parentChildMapI link where
  link :: NodeAttr -> (Int, NodeAttr) -> String
  link parent (position, child) = 
    intercalate "\t"
    [ 
        showNodeValue parent
      , showNodeID    parent
      , show          position
      , showNodeType  child
      , showNodeValue child
    ]
