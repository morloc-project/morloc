module Morloc.Syntax ( Expr(..) , Op(..) ) where

import Morloc.Data (MData)

data Expr
  = Value  MData
  | Array  [Expr]
  | BinOp  Op Expr Expr
  | Apply  Expr [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Dot  -- "."
  deriving (Eq, Ord, Show)
