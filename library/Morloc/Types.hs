{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Types
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Types ( 
  -- ** Typeclasses
    MShow(..)
  , MorlocNodeLike(..)
  , MorlocTypeable(..)
  -- ** Synonyms
  , SparqlEndPoint  
  , AbstractType -- ^ A universal Morloc type
  , ConcreteType -- ^ A language-specific type
  , Name
  , Lang
  , Path
  , Code
  , Key
  , Value
  , Element
  , ScriptGenerator
  , CodeGenerator
  -- ** Data
  , Script(..)
  , Manifold(..)
  , Argument(..)
  , MData(..)
  , MType(..)
  , MTypeMeta(..)
  , GraphPredicate(..)
  , GraphObject(..)
  , SerialMap(..)
  -- ** Error handling
  , ThrowsError
  , MorlocError(..)
) where

import Data.Text                    ( Text       )
import Data.RDF                     ( Node       )
import Data.Map.Strict              ( Map        )
import Text.PrettyPrint.Leijen.Text ( Doc        )
import Text.Megaparsec.Error        ( ParseError )
import Data.Void                    ( Void       )

-- | Write into Morloc code
class MShow a where
  mshow :: a -> Doc

class MorlocNodeLike a where
  asRdfNode :: a -> Node
  fromRdfNode :: Node -> a

class MorlocTypeable a where
  asType :: a -> MType

type Name    = Text
type Lang    = Text
type Path    = Text
type Code    = Text
type Key     = Text
type Value   = Text
type Element = Text

type AbstractType = MType
type ConcreteType = MType

data Manifold = Manifold {
      mCallId       :: Key
    , mAbstractType :: Maybe AbstractType
    , mConcreteType :: Maybe ConcreteType
    , mExported     :: Bool
    , mCalled       :: Bool
    , mSourced      :: Bool
    , mMorlocName   :: Name
    , mCallName     :: Name
    , mSourcePath   :: Maybe Path
    , mSourceName   :: Maybe Name
    , mComposition  :: Maybe Name
    , mBoundVars    :: [Name]
    , mLang         :: Maybe Lang
    , mArgs         :: [Argument]
  }
  deriving(Show, Eq, Ord)

data Argument
  = ArgName Name
  -- ^ Morloc variables that are defined in scope
  | ArgCall Manifold
  -- ^ A call to some function
  | ArgData MData
  -- ^ Raw data defined in one of the Morloc internal types
  | ArgPosi Int
  -- ^ A manifold positional argument (passed into a function assignment)
  deriving(Show, Eq, Ord)

-- The values are left unparsed, since they will be used as text
data MData
  = Num' Text
  | Str' Text
  | Log' Bool -- booleans are parsed, since representation depend on language
  | Lst' [MData]
  | Rec' [(Name, MData)]
  | Tup' [MData]
  deriving(Show, Eq, Ord)

data MType
  = MConcType MTypeMeta Name [MType]
  | MAbstType MTypeMeta Name [MType]
  | MFuncType MTypeMeta [MType] MType
  deriving(Show, Eq, Ord)

-- TODO: add constraints
data MTypeMeta = MTypeMeta {
      metaName :: Maybe Name
    , metaProp :: [Name]
    , metaLang :: Maybe Lang
  }
  deriving(Show, Eq, Ord)

data SerialMap = SerialMap {
      serialLang :: Lang
    , serialPacker   :: Map MType Name
    , serialUnpacker :: Map MType Name
    , serialGenericPacker   :: Name
    , serialGenericUnpacker :: Name
    , serialSources :: [Path] -- Later, I might want to link the source files to
                              -- each function, but for now that isn't needed.
  }
  deriving(Show, Eq, Ord)

-- | Stores a URL for a SPARQL endpoint (e.g. "http://localhost:3030/morloc")
type SparqlEndPoint = String

-- | A code generator
type ScriptGenerator = SparqlEndPoint -> IO Script
type CodeGenerator = SparqlEndPoint -> IO Code

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: Text -- ^ full script source code
  }
  deriving(Ord, Eq)

data GraphPredicate
  = PElem Int
  | PAlias
  | PConstraint
  | PLabel
  | PLang
  | PLeft
  | PNamespace
  | POutput
  | PPath
  | PProperty
  | PRight
  | PType
  | PValue
  | PKey
  | PNot
  | PName
  | PImport
  deriving(Show, Eq, Ord)

data GraphObject
  = OLiteral Text
  | OAccess
  | OAtomicGenericType
  | OAtomicType
  | OBinaryOp
  | OBoolean
  | OCall
  | OData
  | ODataDeclaration
  | OEmptyType
  | OExport
  | OFunctionType
  | OImport
  | OList
  | OName
  | ONamedType
  | ONumber
  | OParameterizedGenericType
  | OParameterizedType
  | ORecord
  | ORecordEntry
  | ORestrictedImport
  | OScript
  | OSource
  | OString
  | OTuple
  | OType
  | OTypeDeclaration
  | OUnaryOp
  | OEmpty
  | OBinOp
  deriving(Show, Eq, Ord)

type ThrowsError = Either MorlocError
data MorlocError
  -- | Raised when assumptions about the input RDF are broken. This should not
  -- occur for RDF that has been validated.
  = InvalidRDF Text
  -- | Raised for calls to unimplemented features
  | NotImplemented Text
  -- | Raised for unsupported features (such as specific languages)
  | NotSupported Text
  -- | Raised by parsec on parse errors
  | SyntaxError (ParseError Char Void)
  -- | Raised when someone didn't customize their error messages
  | UnknownError
  -- | Raised when parent and child types conflict
  | TypeConflict Text Text
  deriving(Eq)

