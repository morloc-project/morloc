module Morloc.Data
(
    Program(..)
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

type Tag      = Maybe String
type Name     = String
type Alias    = String
type Language = String
type Path     = String

data Program = Program {
      programTypes   :: [TypeDecl]
    , programData    :: [DataDecl]
    , programSources :: [Source]
  }
  deriving(Show, Ord, Eq)

instance Monoid Program where
  mempty = Program {
        programTypes   = []
      , programData    = []
      , programSources = []
    }
  mappend p1 p2 = Program {
        programTypes   = programTypes   p1 ++ programTypes   p2
      , programData    = programData    p1 ++ programData    p2
      , programSources = programSources p1 ++ programSources p2
    }

data Script = Script {
      scriptBase :: String -- script basename (no extension)
    , scriptLang :: String -- script language
    , scriptCode :: String -- full script source code
  }
  deriving(Ord, Eq)

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
