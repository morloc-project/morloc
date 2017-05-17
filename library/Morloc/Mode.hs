module Morloc.Mode (Mode, asLIL, asCode) where

import Data.List (intercalate)
import Morloc.Graph
import Morloc.NodeAttribute
import Morloc.Generator (generate)

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

asCode :: Graph NodeAttr -> String
asCode g = case generate g of
  (nexus, pools) -> nexusCode ++ poolCode where
    nexusCode = unlines ["NEXUS", indent nexus]
    poolCode = concatMap writePool pools

    indent :: String -> String
    indent = unlines . map (\s -> "  " ++ s) . lines

    writePool :: Show a => (a, String) -> String
    writePool (l,c) = unlines [show l, indent c]
