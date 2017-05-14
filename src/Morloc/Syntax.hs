module Morloc.Syntax where

type Node = String

data Expr
  = Float   Double
  | Integer Integer
  | String  String
  | Node    Node
  | BinOp   Op Expr Expr
  | Apply   Expr [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Dot  -- "."
  deriving (Eq, Ord, Show)
