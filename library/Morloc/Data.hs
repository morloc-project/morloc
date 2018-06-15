module Morloc.Data (
    WNode(..)
  , TNode(..)
  , Program(..)
) where

import Morloc.Graph (Graph)
import Morloc.Syntax (Import, MType, MData, BExpr)

data WNode
  = WNodeVar String
  | WNodeData MData 
  deriving(Show, Ord, Eq)

data TNode
  = TNodeType MType
  | TNodeSignature -- TODO kill the duplicant, move signatures into MType
      [MType]        -- inputs
      (Maybe MType)  -- optional output
      [BExpr]        -- constraints
  deriving(Show, Ord, Eq)

data Program
  = Workflow (Graph WNode)
  | Ontology [(String, TNode)]
  | Packages [Import]
  deriving(Show, Eq)
