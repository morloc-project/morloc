module Morloc.Data
(
    Manifold(..)
  , MType(..)
  , MData(..)
  , TypeDecl(..)
  , Constraint(..)
  , DataDecl(..)
  , Source(..)
  , Script(..)
  , Tag
  , Name
  , Alias
  , Language
  , Path
) where

import qualified Data.RDF as DR

type Tag      = Maybe String
type Name     = String
type Alias    = String
type Language = String
type Path     = String

data Manifold = Manifold {
      manifoldAlias      :: Name             -- name in Morlc script
    , manifoldSrcName    :: String           -- name in source language
    , manifoldRdfId      :: DR.Node          -- a unique ID
    , manifoldInput      :: [(MType, MData)] -- type and value of each input
    , manifoldType       :: MType            -- output type
    , manifoldSource     :: Maybe Source     -- source file
    , manifoldParameter  :: [Parameter]      -- function parameters
    , manifoldConstraint :: [Constraint]     -- constrains on input and output
  }

data Script = Script {
      scriptBase :: String -- script basename (no extension)
    , scriptLang :: String -- script language
    , scriptCode :: String -- full script source code
  }
  deriving(Ord, Eq)

data Parameter = Parameter {
      parameterName    :: String
    , parameterType    :: MType
    , parameterDefault :: Maybe MData
    , parameterValue   :: Maybe MData
    , parameterDoc     :: Maybe String
  }

instance Show Script where
  show (Script base lang code) = code 

data MType
  = TypeSpc Tag Name [MType]
  | TypeGen Tag Name [MType]
  | TypeFun Tag [MType] MType
  | TypeKwd Name MType
  | TypeEmp Tag
  | TypeUnk -- for example in `x = []`, where `[]` may contain anything 
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

data Source
  = Source
    Language
    (Maybe Path)
    [(Name, Maybe Alias)]
    deriving(Show, Ord, Eq)

data TypeDecl
  = TypeDecl
    Name
    MType
    [Constraint]
    deriving(Show, Ord, Eq)

data Constraint
  = Constraint
    String
  deriving(Show, Ord, Eq)

data DataDecl
  = DataDecl
    Name
    [String]
    MData
    deriving(Show, Ord, Eq)
