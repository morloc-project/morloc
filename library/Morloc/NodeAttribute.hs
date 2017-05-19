module Morloc.NodeAttribute
(
    NodeAttr(..)
  , nodeAttrS
  , showNodeID
  , showNodeValue
  , showNodeType
  , showNodePrimitive
) where

import Data.Maybe (fromMaybe)

data NodeAttr
  = NodeAttr {
      nodeID    :: Maybe Int
    , nodeValue :: Maybe String
    , nodeType  :: Maybe String
    , primitive :: Maybe Bool
  }
  deriving(Show,Eq,Ord)

-- make default NodeAttr based off a node name
nodeAttrS :: String -> NodeAttr
nodeAttrS s = NodeAttr {
    nodeID    = Nothing
  , nodeValue = Just s
  , nodeType  = Nothing
  , primitive = Just False
}

showNodeID :: NodeAttr -> String
showNodeID x = case nodeID x of
  Just s  -> show s
  Nothing -> "NO_ID"

showNodeValue :: NodeAttr -> String
showNodeValue x = fromMaybe "NO_VALUE" (nodeValue x)

showNodeType :: NodeAttr -> String
showNodeType x = fromMaybe "*" (nodeType  x)

showNodePrimitive :: NodeAttr -> String
showNodePrimitive x = case primitive x of
  Just s  -> show s
  Nothing -> "UNSET"

