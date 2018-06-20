module Morloc.Data (
    WNode(..)
  , TNode(..)
  , SNode(..) -- WTF 'sup with this name?
  , Script(..)
  , Program(..)
  , Function(..)
) where

import Morloc.Graph (Graph)
import Morloc.Syntax (Source, MType, MData, BExpr)

data WNode
  = WNode
      String -- name
      String -- tag
  | WLeaf MData 
  deriving(Show, Ord, Eq)

data SNode
  = SNode
    (WNode, Maybe Source)   -- parent
    [(WNode, Maybe Source)] -- children
  | SLeaf MData
  deriving(Show, Eq)

data Function a
  = Function
    String    -- name
    [String]  -- bound variables
    (Graph a) -- function composition tree
  deriving(Show, Eq)

instance Functor Function where
  fmap f (Function x xs g) = Function x xs (fmap f g)

data TNode
  = TNodeType MType
  | TNodeSignature -- TODO kill the duplicant, move signatures into MType
      [MType]        -- inputs
      (Maybe MType)  -- optional output
      [BExpr]        -- constraints
  deriving(Show, Ord, Eq)

data Script = Script {
      scriptBase :: String -- script basename (no extension)
    , scriptLang :: String -- script language
    , scriptCode :: String -- full script source code
  }
  deriving(Show, Ord, Eq)


data Program = Program {
      -- TODO this isn't really the workflow, but rather a list of functions.
      -- Each function may link to values inside other functions. It is these
      -- linkes that create the workflow.
      workflow :: [Function WNode]
      -- TODO this isn't really the ontology, but rather just a list of type
      -- signatures. The ontology will hold the relations between them.
    , ontology :: [(
            String -- type name
          , TNode  -- type
        )]
      -- TODO these aren't really packages, just the in-script sourced code,
      -- with none of the metadata, export lists, and other info that a real
      -- package should have.
    , packages :: [Source]
  }
  deriving(Show, Eq)
