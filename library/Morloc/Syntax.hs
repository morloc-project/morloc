module Morloc.Syntax (
    Expr(..)
  , Op(..)
) where

data Expr
  = Float   Double
  | Integer Integer
  | String  String
  | Node    String
  | Bool    Bool
  | BinOp   Op Expr Expr
  | Apply   Expr [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Dot  -- "."
  deriving (Eq, Ord, Show)
