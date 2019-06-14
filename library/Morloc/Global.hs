{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Global
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Global ( 
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
  , Path
  , Code
  , Key
  , Value
  , Element
  -- ** Language
  , Lang(..)
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
  , ManifoldClass(..)
  -- ** Error handling
  , MorlocError(..)
  -- ** Configuration
  , Dependency(..)
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

import Morloc.Language (Lang(..))

-- | Write into Morloc code
class MShow a where
  mshow :: a -> Doc

class DocLike a where
  toDoc :: a -> Doc

class MorlocNodeLike a where
  asRdfNode :: a -> Node
  fromRdfNode :: Node -> a

class MorlocTypeable a where
  asType :: a -> MorlocMonad MType

class SparqlSelectLike a where
  writeSparql :: Path -> a -> IO () -- ^ create SPARQL text
  showSparql :: a -> String

class RdfLike a where
  writeTurtle :: Path -> a -> IO () -- ^ create Turtle formatted file
  asTriples :: a -> [Triple]

class SparqlDatabaseLike a where
  sparqlUpload :: (RdfLike r) => a -> r -> MorlocMonad a

  sparqlSelect
    :: (SparqlSelectLike q)
    => Text -- ^ A path prefix for stored sparql data and query
    -> q    -- ^ The query
    -> a    -- ^ The thing to be queried
    -> MorlocMonad [[Maybe Text]]

data Dependency
  = ModuleDependency Name Path Lang
  | ExecutableDependency Name Path
  | SourceCodeDependency Name Path Lang
  deriving(Show, Ord, Eq)

type MorlocMonadGen c e l s a = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a
type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)
data MorlocState = MorlocState {
      sparqlConn :: Maybe SparqlEndPoint
    , dependencies :: [Dependency]
  }
type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

type Name    = Text
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
      mid           :: Integer
    , mCallId       :: Text
    , mAbstractType :: Maybe AbstractType
    , mConcreteType :: Maybe ConcreteType
    , mExported     :: Bool
    -- ^ If False, then it is purely a local function
    -- If True, then this manifold will be
    -- 1) callable from the nexus (if this script is the base script)
    -- 2) imported if the module is imported
    , mCalled       :: Bool
    , mSourced      :: Bool
    -- ^ True if this function read from sourced (e.g., @source "R" ("runif")@)
    , mMorlocName   :: Name
    -- ^ e.g., in @source "R" ("runif" as rand_uniform)@, "rand_uniform" is
    -- morloc name and "runif" is the R name
    , mCallName     :: Name
    , mSourcePath   :: Maybe Path
    -- ^ e.g., "foo.py3", this is relative to the module directory path
    , mModulePath   :: Maybe Path
    -- ^ e.g., "$HOME/.morloc/lib/foo/main.loc
    , mSourceName   :: Maybe Name
    , mComposition  :: Maybe Name
    -- ^ The name of the declaration function. For example, in
    -- @foo x = sqrt x@, "foo" is mComposition
    , mBoundVars    :: [Name]
    , mLang         :: Maybe Lang
    -- ^ The language if specified (otherwise morloc should try to select one)
    , mArgs         :: [Argument]
  }
  deriving(Show, Eq, Ord)

-- | The role a manifold plays relative to a given language
data ManifoldClass
  = Cis      -- ^ Wrapper around a Morloc composition
  | Trans    -- ^ Wrapper around a foreign call
  | Source   -- ^ Wrapper around a source function
  | Uncalled -- ^ Does not need to be built in current language
  deriving(Show, Ord, Eq)

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

-- | A Morloc type, may be a language specific type.
data MType
  = MConcType MTypeMeta Name [MType]  -- ^ A non-generic, concrete type
  | MAbstType MTypeMeta Name [MType]  -- ^ A generic, wildcard, type
  | MFuncType MTypeMeta [MType] MType -- ^ A function type with a list of input types and a single output type
  deriving(Show, Eq)

instance Ord MType where
  -- Concrete is more specific 
  compare (MConcType _ _ _) (MAbstType _ _ _) = GT
  compare (MAbstType _ _ _) (MConcType _ _ _) = LT
  -- Function types are somewhat arbitrarily more specific than concrete types
  compare (MFuncType _ _ _) (MConcType _ _ _) = GT
  compare (MConcType _ _ _) (MFuncType _ _ _) = LT
  -- Function types are more specific than abstract types
  compare (MFuncType _ _ _) (MAbstType _ _ _) = GT
  compare (MAbstType _ _ _) (MFuncType _ _ _) = LT
  -- For similar types, compare first the children, then the properties, then the name
  -- TODO: This needs more thought.
  compare (MConcType d1 n1 xs1)  (MConcType d2 n2 xs2)  = compare (xs1, d1, n1)  (xs2, d2, n2)
  compare (MAbstType d1 n1 xs1)  (MAbstType d2 n2 xs2)  = compare (xs1, d1, n1)  (xs2, d2, n2)
  compare (MFuncType d1 ins1 o1) (MFuncType d2 ins2 o2) = compare (ins1, o1, d1) (ins2, o2, d2)

-- TODO: add constraints
data MTypeMeta = MTypeMeta {
      metaName :: Maybe Name
    , metaProp :: [Name]
    -- ^ A list of properties. Currently these are non-parameterized
    -- properties, such as "pack" or "unpack".
    , metaLang :: Maybe Lang
    -- ^ The language. TODO: make Nothing mean it is a Morloc function, and
    -- "Just lang" mean it is a concrete type from the language "lang".
  }
  deriving(Show, Eq, Ord)

data SerialMap = SerialMap {
      serialLang     :: Lang
    , serialPacker   :: Map MType Name
    , serialUnpacker :: Map MType Name
    , serialSources  :: [Path]
    -- ^ The absolute paths to the source files
  }
  deriving(Show, Eq, Ord)

-- | Stores a URL for a SPARQL endpoint (e.g. "http://localhost:3030/morloc")
newtype SparqlEndPoint = SparqlEndPoint { endpoint :: String }

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: Lang    -- ^ script language
    , scriptCode :: Text    -- ^ full script source code
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
  | PNot        -- ^ Negate an expressoin
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
  -- | Raised when an unsupported language is encountered
  | UnknownLanguage Text
  -- | Raised when parent and child types conflict
  | TypeConflict Text Text
  -- | Raised for general type errors
  | TypeError Text
  -- | Raised when a SPARQL command fails
  | SparqlFail Text
  -- | Raised when a module cannot be loaded 
  | CannotLoadModule Text
  -- | System call failed
  | SystemCallError Text Text Text
  -- | Raised when there is an error in the code generators
  | GeneratorError Text
  -- | Missing a serialization or deserialization function
  | SerializationError Text
  -- | Raised when a dependency is missing
  | DependencyError Dependency
  -- | Error in building a pool (i.e., in a compiled language)
  | PoolBuildError Script Text
  -- | A truly weird and befuddling error that shouldn't ever occur
  | TrulyWeird
  deriving(Eq)

-- | Configuration object that is passed with MorlocMonad
data Config = Config {
    configHome :: Text
  , configLibrary :: Text
  , configTmpDir :: Text
  , configLangPython3 :: Text
  , configLangR :: Text
  , configLangPerl :: Text
  }
  deriving(Show, Ord, Eq)
