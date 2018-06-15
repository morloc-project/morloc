module Morloc.Syntax ( Expr(..) , Op(..) ) where

import Morloc.Data (MData, MType)

-- This structure holds expressions raw expressions which can be invalid. For
-- example, `BinOp OpEqu (MInt 2) (MString "56")`, or `2 = "56"`. The validity
-- of Expr is determined in Evaluator. It might be better to have the parser
-- fail on any grammatically incorrect code.

data Expr
  = Value  MData
  | Array  [Expr]
  | BinOp  Op Expr Expr
  | Apply  Expr [Expr]
  | ConstDecl  Expr Expr         -- name value
  | FuncDecl   Expr [Expr] Expr  -- name [parameters] body
  | TypeDecl   Expr [Expr] Expr -- name [input] output
  deriving (Eq, Ord, Show)

data Op
  = OpDot  -- "."
  deriving (Eq, Ord, Show)
