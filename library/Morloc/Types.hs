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
  , SparqlSelectLike(..)
  , SparqlDatabaseLike(..)
  , RdfLike(..)
  , DocLike(..)
  -- ** Synonyms
  , SparqlEndPoint(..)  
  , AbstractType
  , ConcreteType
  , Name
  , Lang
  , Path
  , Code
  , Key
  , Value
  , Element
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
  -- ** Configuration
  , Config(..)
  -- ** Morloc monad
  , MorlocMonad
  , MorlocState(..)
  , MorlocReturn
) where

import Data.Text                    (Text)
import Data.RDF                     (Node, Triple)
import Data.Map.Strict              (Map)
import Text.PrettyPrint.Leijen.Text (Doc)
import Text.Megaparsec.Error        (ParseError)
import Data.Void                    (Void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State  (StateT)
import Control.Monad.Writer (WriterT)

-- | Write into Morloc code
class MShow a where
  mshow :: a -> Doc

class DocLike a where
  toDoc :: a -> Doc

class MorlocNodeLike a where
  asRdfNode :: a -> Node
  fromRdfNode :: Node -> a

class MorlocTypeable a where
  asType :: a -> MType

class SparqlSelectLike a where
  writeSparql :: Path -> a -> IO () -- ^ create SPARQL text
  showSparql :: a -> String

class RdfLike a where
  writeTurtle :: Path -> a -> IO () -- ^ create Turtle formatted file
  asTriples :: a -> [Triple]

class SparqlDatabaseLike a where
  sparqlUpload :: (RdfLike r) => a -> r -> MorlocMonad a

  -- FIXME: wrap this pig in Either
  sparqlSelect
    :: (SparqlSelectLike q)
    => q -> a -> IO [[Maybe Text]]

type MorlocMonadGen c e l s a = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a
type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)
data MorlocState = MorlocState {sparqlConn :: Maybe SparqlEndPoint}
type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

type Name    = Text
type Lang    = Text
type Path    = Text
type Code    = Text
type Key     = Text
type Value   = Text
type Element = Text

-- | Universal Morloc type
type AbstractType = MType

-- | Language-specific type
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

-- | The values are left unparsed, since they will be used as text
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
newtype SparqlEndPoint = SparqlEndPoint { endpoint :: String }

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: String  -- ^ script language
    , scriptCode :: Text -- ^ full script source code
  }
  deriving(Ord, Eq)

-- | Set of all legal predicates in a compiled Morloc script
data GraphPredicate
  = PElem Int   -- ^ Link a parent to an ordered child
  | PAlias      -- ^ Link an imported function to a Morloc name
  | PConstraint -- ^ Link a type to a constraint
  | PLabel      -- ^ Link a Morloc variable to a tag for the variable (TODO: replace this name)
  | PLang       -- ^ Link something to a specific programming language
  | PLeft       -- ^ Link something to its left-hand-side element
  | PNamespace  -- ^ Link a source or import to a namespace
  | POutput     -- ^ Link a function to its output
  | PPath       -- ^ Link a source to the file-system path
  | PProperty   -- ^ Link a type to a type property
  | PRight      -- ^ Link something to its right-hand-side element
  | PType       -- ^ Link something to its type (rdf:type)
  | PValue      -- ^ Link something to
  | PKey        -- ^ Link something to a key
  | PNot        -- ^ Negate an exepressoin
  | PName       -- ^ Link something to a Morloc name
  | PImport     -- ^ The left-hand imports the right-hand side
  deriving(Show, Eq, Ord)

-- | Set of all legal objects in a compiled Morloc script
data GraphObject
  -- literals
  =  OLiteral Text             -- ^ Stores a literal textual field, should always be with the predicate PValue
  -- top-level
  |  ORestrictedImport         -- ^ LHS is an import with some imports hidden
  |  OScript                   -- ^ LHS is a top-level script
  |  OSource                   -- ^ LHS represents a source file
  |  OExport                   -- ^ LHS represents an exported function (there should be an associated (?s PValue (OLiteral name)) triple.
  |  OImport                   -- ^ LHS is an import (there exists a triple: `?i PName ?j` where `i` is the LHS and `j` is a string literal for the module name)
  -- types and type declarations
  |  OType                     -- ^ LHS is a type
  |  OTypeDeclaration          -- ^ LHS is a type declaration (e.g. `Foo :: Matrix Int`)
  |  OAtomicGenericType        -- ^ LHS is an abstract type with no type parameters
  |  OAtomicType               -- ^ LHS is a concrete type with no type parameters
  |  OParameterizedGenericType -- ^ LHS is a parameterized, abstract type
  |  OParameterizedType        -- ^ LHS is a parameterized, concrete type
  |  OEmptyType                -- ^ LHS is an empty type
  |  OFunctionType             -- ^ LHS is a function type
  |  ONamedType                -- ^ LHS is a named type (e.g. a record entry)
  -- parts of a constraint
  |  OAccess                   -- ^ Index access to a container (inside a constraint)
  |  OUnaryOp                  -- ^ LHS is a unary operator in a constraint
  |  OBinOp                    -- ^ LHS is a binary operator in a constraint (soon to be deprecated and merged with OCall)
  -- data and data declarations and components
  |  OData                     -- ^ LHS is literal data
  |  ODataDeclaration          -- ^ LHS is a Morloc data (or function) declaration
  |  OBoolean                  -- ^ LHS is a boolean literal
  |  OCall                     -- ^ LHS is a function call
  |  OName                     -- ^ LHS is a Morloc name (manifold parameter or something defined outside the dataDeclaration scope)
  |  ONumber                   -- ^ LHS is a literal number
  |  OString                   -- ^ LHS is a literal string
  |  OList                     -- ^ LHS is an ordered list of homogenous data (with elements stored as PElem entries)
  |  OTuple                    -- ^ LHS is a tuple (elements stored as PElem entries)
  |  ORecord                   -- ^ LHS is a record (elements stored as PElem entries)
  |  ORecordEntry              -- ^ LHS is a record entry (cmp. ONamedType)

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
  -- | Raised when a SPARQL command fails
  | SparqlFail Text
  -- | Raised when a module cannot be loaded 
  | CannotLoadModule Text
  -- | System call failed
  | SystemCallError Text
  -- | Raised when there is an error in the code generators
  | GeneratorError Text
  -- | A truly weird and befuddling error that shouldn't ever occur
  | TrulyWeird
  deriving(Eq)

data Config = Config {
    configHome :: Text
  , configLibrary :: Text
  , configLangPython3 :: Text
  , configLangR :: Text
  , configLangPerl :: Text
  }
  deriving(Show, Ord, Eq)
