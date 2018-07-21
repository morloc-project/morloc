module Morloc.Data
(
    MType(..)
  , MData(..)
  , TypeDecl(..)
  , Constraint(..)
  , DataDecl(..)
  , Tag
  , Name
) where

type Tag = Maybe String
type Name = String

data MType
  = TypeSpc Tag Name [MType]
  | TypeGen Tag Name [MType]
  | TypeFun Tag [MType] MType
  | TypeKwd Name MType
  | TypeEmp Tag
  deriving(Show, Ord, Eq)

-- This includes everything that can be on the rhs of a declaration
data MData
  = DataInt Integer
  | DataNum Double
  | DataLog Bool
  | DataLst [MData]
  | DataTup [MData]
  | DataRec [(Name, MData)]
  | DataStr String
  | DataFun Name [MData]
  | DataVar Name
  deriving(Show, Ord, Eq)

data Constraint
  = Constraint String
  deriving(Show, Ord, Eq)

data TypeDecl = TypeDecl Name MType deriving(Show, Ord, Eq)
data DataDecl = DataDecl Name [String] MData deriving(Show, Ord, Eq)
