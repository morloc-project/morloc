module Morloc.Syntax (
    Top(..)
  , Import(..)
  , Statement(..)
  , Source(..)
  , Expression(..)
  , MData(..)
  , BExpr(..)
  , AExpr(..)
  , MType(..)
  , Name
  , Tag
) where

import Data.List (intersperse)
import Data.Maybe (maybe)

type Name = String
type Tag = String

data Top
  = TopImport Import 
  | TopStatement Statement
  | TopSource Source
  deriving(Show, Ord, Eq)

data Source = Source Name [String] deriving(Ord, Eq)

-- TODO allow function types
data MType
  = MSpecific Name [MType] Tag
  | MGeneric Name [MType] Tag
  | MList MType Tag
  | MTuple [MType] Tag 
  | MRecord Name [(Name, MType)] Tag
  | MEmpty
  deriving(Show, Ord, Eq)

data MData
  = MInt Integer
  | MNum Double
  | MLog Bool
  | MLst [MData]
  | MTup [MData]
  | MRec [(Name, MData)]
  | MStr String
  deriving(Show, Ord, Eq)

data Statement
  = Signature
      Name           -- lhs
      [MType]        -- inputs
      (Maybe MType)  -- optional output
      [BExpr]        -- constraints
  | Declaration
      Name           -- lhs
      [Name]         -- bound variables
      Expression     -- rhs
  deriving(Show, Ord, Eq)

data Expression
  = ExprData MData
  | ExprApplication Name Tag [Expression]
  | ExprComposition Expression Expression
  deriving(Show, Ord, Eq)

data BExpr
  = BExprFunc Name [Name]
  | BExprBool Bool
  -- relative operators
  | EQ' AExpr AExpr
  | NE' AExpr AExpr
  | GT' AExpr AExpr
  | LT' AExpr AExpr
  | GE' AExpr AExpr
  | LE' AExpr AExpr
  -- logical operators
  | NOT BExpr
  | AND BExpr BExpr
  | OR  BExpr BExpr
  deriving(Show, Ord, Eq)

data AExpr
  = AExprFunc Name [Name]
  | AExprInt Integer
  | AExprReal Double
  | AExprAccess Name [AExpr]
  | Pos AExpr
  | Neg AExpr
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Pow AExpr AExpr
  | Mod AExpr AExpr
  | Quo AExpr AExpr
  deriving(Show, Ord, Eq)

data Import = Import {
      importPath :: [Name]
    , importQualifier :: Maybe Name
    , importRestriction :: Maybe [Name]
  } deriving(Show, Ord, Eq)

instance Show Source where
  show (Source n ls) = unlines (("source " ++ n) : ls)
