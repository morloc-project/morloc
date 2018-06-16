module Morloc.Data (
    WNode(..)
  , TNode(..)
  , Program(..)
  , FunctionTree(..)
) where

import Morloc.Graph (Graph)
import Morloc.Syntax (Import, MType, MData, BExpr)

data WNode
  = WNodeVar
      String -- name
      String -- tag
  | WNodeData MData 
  deriving(Show, Ord, Eq)

data FunctionTree
  = FunctionTree
    String        -- name
    [String]      -- bound variables
    (Graph WNode) -- function composition tree
  deriving(Show, Eq)

data TNode
  = TNodeType MType
  | TNodeSignature -- TODO kill the duplicant, move signatures into MType
      [MType]        -- inputs
      (Maybe MType)  -- optional output
      [BExpr]        -- constraints
  deriving(Show, Ord, Eq)

data Program = Program {
      -- TODO this isn't really the workflow, but rather a list of functions.
      -- Each function may link to values inside other functions. It is these
      -- linkes that create the workflow.
      workflow :: [FunctionTree]
      -- TODO this isn't really the ontology, but rather just a list of type
      -- signatures. The ontology will hold the relations between them.
    , ontology :: [(
            String -- type name
          , TNode  -- type
        )]
      -- TODO these aren't really packages, just the in-script imported code,
      -- with none of the metadata, export lists, and other info that a real
      -- package should have.
    , packages :: [Import]
  }
  deriving(Show, Eq)
