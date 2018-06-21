module Morloc.Data (
    WNode(..)
  , TNode(..)
  , SNode(..) -- WTF 'sup with this name?
  , Script(..)
  , Program(..)
  , Function(..)
) where

import Morloc.Tree (Tree)
import Morloc.Syntax (Source, MType, MData, BExpr)

data WNode
  = WNode
      (Maybe Int) -- manifold id
      String      -- name
      String      -- tag
  | WLeaf
      (Maybe Int) -- manifold id
      MData 
  deriving(Show, Ord, Eq)

data SNode
  = SNode
    (WNode, Source)   -- parent
    [String]          -- bound arguments
    [(WNode, Source)] -- children
  | SLeaf
    (Maybe Int)
    MData
  deriving(Show, Eq)

data Function a
  = Function
    String   -- name
    [String] -- bound variables
    (Tree a) -- function composition tree
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
  deriving(Ord, Eq)


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

instance Show Script where
  show (Script base lang code) = code 


-- -- TODO eventually I should unify everything through a key-value system
--
-- data Triple = Triple Key Relation Value
--
-- type Key = Int
--
-- data Relation
--   = RIsA
--   | RPositional Int  -- the nth positional parameter
--   | RHasValue
--   | RHasConstraint
--   | ...
--
-- data Value
--   = VFunction
--   | VSource
--   | VConstraint
--   | ValueData MData
--   | ...
