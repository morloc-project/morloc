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
  -- compatibility ---
  , mLang           -- Kill all these as soon
  , mName           -- as possible
  , mConcreteType   --
  , mModulePath     --
  , mSourcePath     --
  , mSourced        --
  --------------------
  , Realization(..)
  , Argument(..)
  , FunctionDeclaration(..)
  , Call(..)
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
  -- ** Package metadata
  , PackageMeta(..)
  , defaultPackageMeta
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
  , statePackageMeta :: [PackageMeta]
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

-- | Stores everything needed to build one file
data Script = Script {
      scriptBase :: String  -- ^ script basename (no extension)
    , scriptLang :: Lang    -- ^ script language
    , scriptCode :: Text    -- ^ full script source code
    , scriptCompilerFlags :: [Text] -- ^ compiler/interpreter flags
  }
  deriving(Ord, Eq)

-- | Represents a Platonic function with zero or more concrete realizations
-- (Realization objects). The Manifold represents one node in the
-- functional directed graph. It is called a "manifold" since it will
-- eventually integrate several dimensions of the pipeline. One dimension is
-- the pure functional chain. A second is the orthgonal validation of the pure
-- values (possibly forming a validation graph between manifolds). A third
-- consists of summaries/descriptions of the pure data. These separate
-- dimensions can be associated with manifolds without obfuscating the pure
-- functional specification. None of this is yet implemented in the Haskell
-- version of Morloc, though it was extensively explored in the old C version,
-- and one day I will resurrect it.
data Manifold = Manifold {
      mid :: Integer
    -- ^ An ID uniquely identifying this manifold. Each manifold appears only
    -- once in the generated code.
    , mCallId :: Text
    -- ^ The RDF identifier for this manifold
    , mAbstractType :: Maybe AbstractType
    -- ^ The Morloc type for the wrapped function 
    , mRealizations :: [Realization]
    -- ^ Language specific instances for this manifold
    , mMorlocName :: Name
    -- ^ e.g., in @source "R" ("runif" as rand_uniform)@, "rand_uniform" is
    -- morloc name and "runif" is the R name
    , mExported :: Bool
    -- ^ If False, then it is purely a local function
    -- If True, then this manifold will be
    -- 1) callable from the nexus (if this script is the base script)
    -- 2) imported if the module is imported
    , mCalled :: Bool
    -- ^ Is this function called within a Morloc composition?
    , mDefined :: Bool
    -- ^ Is this a Morloc function
    , mComposition :: Maybe Name
    -- ^ The name of the declaration function. For example, in
    -- @foo x = sqrt x@, "foo" is mComposition
    , mBoundVars :: [Name]
    , mArgs :: [Argument]
    -- ^ Arguments passed to the wrapped function. There should be one element
    -- in this list for every input to the function specified in mAbstractType.
    -- This may change once currying is implemented.
  }
  deriving(Show, Eq, Ord)

-- | Store a function declaration, e.g., (foo x y = bar (baz x) y)
data FunctionDeclaration = FunctionDeclaration {
    fdId :: Key
  -- ^ RDF URI for the object of type DataDeclaration
  , fdName :: Name
  -- ^ name of the funtion ("foo")
  , fdCallId :: Key
  -- ^ RDF URI for the called function (URI of bar)
  , fdExported :: Bool
  -- ^ Is this function exported?
  , fdArgs :: [Name]
  -- ^ list of bound arguments (["x","y"])
} deriving(Show, Eq, Ord)

-- | Store a single call, e.g., in (foo x y = bar (baz x) y) there would be two
-- calls, "bar" and "baz", with 2 and 1 arguments, respectively.
data Call = Call {
      callId :: Key
    , callName :: Name
    , callArgs :: [Argument]
  } deriving(Show, Eq, Ord)

-- FIXME: The following functions are provided for compatibility. It allows
-- code to pretend the Manifold record is the same as the old one (before
-- adding realizations).
getReal :: (Realization -> a) -> Manifold -> a
getReal f m = case mRealizations m of
  [r] -> f r
  _ -> error ("Realization failure in m = " ++ show m)
mLang :: Manifold -> Lang
mLang = getReal rLang
mName :: Manifold -> Name
mName = getReal rName
mConcreteType :: Manifold -> Maybe ConcreteType
mConcreteType = getReal rConcreteType 
mModulePath :: Manifold -> Maybe Path
mModulePath = getReal rModulePath
mSourcePath :: Manifold -> Maybe Path
mSourcePath = getReal rSourcePath
mSourced :: Manifold -> Bool
mSourced = getReal rSourced

-- | Represents a concrete realization of a manifold in a specific language.
-- This object stores what is needed to generate the function wrapped by a
-- manifold in the final executable. A given manifold will have many
-- realizations, and the compiler will need to choose one for the ultimate
-- program. Eventually additional information will be stored within this
-- object, for example, performance metrics, which will be used in the
-- selection process.
data Realization = Realization {
      rLang :: Lang
    -- ^ The implementation language for the concrete function.
    , rName :: Name
    -- ^ Either the source name, if specified, or the morloc name. It is the
    -- function name that will be used in the generated source code.
    , rConcreteType :: Maybe ConcreteType
    -- ^ The language-specific type for this realization. For example: @sum Cpp
    -- :: "std::vector<double>" -> "int"@. If no concrete type is given, then
    -- the compiler will try to infer one. This is not yet implemented, but
    -- should be possible. There are three (non-exclusive) approaches. 1) We
    -- could look at other realizations in the same language that specify
    -- concrete types for arguments/outputs of the realization. 2) We could
    -- parse the source code to find type annotations. 3) We could generate a
    -- wrapper around the function and hammer it with data generated from
    -- different types until we find a combination of types that doesn't break.
    -- Also, in many cases we will not even need an explicit type, since the
    -- generic serialization functions will suffice, this is particularly true
    -- of dynamic languages (e.g., Python and R).
    , rModulePath :: Maybe Path
    -- ^ The path to the morloc package that defines the realization (e.g.,
    -- "$HOME/.morloc/lib/foo/main.loc)
    , rSourcePath :: Maybe Path
    -- ^ The path to the source code that contains the function in scope. The
    -- path is relative to the module path defined in mModulePath.
    , rSourced :: Bool
    -- ^ True if this function read from sourced (e.g., @source "R" ("runif")@)
} deriving(Show, Ord, Eq)

-- | The role a manifold plays relative to a given language.
-- TODO: reconsider what these really mean, the usage seems to have deviated
-- from the original intent.
data ManifoldClass
  = Cis      -- ^ Wrapper around a Morloc composition
  | Trans    -- ^ Wrapper around a foreign call
  | Source   -- ^ Wrapper around a source function
  | Uncalled -- ^ Does not need to be built in current language
  deriving(Show, Ord, Eq)

data Argument
  = ArgName Name
  -- ^ Morloc variables that are defined in scope
  | ArgCall Key
  -- ^ A call to some manifold (within scope)
  | ArgFunc Key [Argument]
  -- ^ A call to a morloc function (out of scope)
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

-- TODO: add constraints
data MTypeMeta = MTypeMeta {
      metaName :: Maybe Name
    , metaProp :: [[Name]]
    -- ^ A list of properties. Each property is a list of names. These may be
    -- single properties of the function (e.g., "packs") or relations
    -- describing generic variables (e.g., "Num a", like a Haskell typeclass),
    -- or more general logical relations. Nesting is currently not allowed.
    , metaLang :: Maybe Lang
    -- ^ The language. TODO: make Nothing mean it is a Morloc function, and
    -- "Just lang" mean it is a concrete type from the language "lang".
  }
  deriving(Show, Eq)

-- The most specific type is highest.
-- The lowest type is the generic 'a'.
-- The order of types is used to choose the most specific serialization functions.
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
  compare (MConcType d1 n1 xs1)  (MConcType d2 n2 xs2)  = compare (xs1, d1, n1)  (xs2, d2, n2)
  compare (MAbstType d1 n1 xs1)  (MAbstType d2 n2 xs2)  = compare (xs1, d1, n1)  (xs2, d2, n2)
  compare (MFuncType d1 ins1 o1) (MFuncType d2 ins2 o2) = compare (ins1, o1, d1) (ins2, o2, d2)

-- As with Ord instance of MType, the most specific type should be highest.
instance Ord MTypeMeta where
  compare (MTypeMeta n1 ps1 l1) (MTypeMeta n2 ps2 l2)
    -- having more properties is more specific
    | length ps1 /= length ps2 = compare (length ps1) (length ps2)
    | otherwise = case ((n1, l1),(n2, l2)) of
      -- having a concrete language is more specific
      ((_, Just _),(_, Nothing)) -> GT
      ((_, Nothing),(_, Just _)) -> LT
      -- otherwise, compare based off names
      _ -> compare (l1, n2, ps1) (l2, n2, ps2)

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

-- | Set of all legal predicates in a compiled Morloc script
data GraphPredicate
  = PElem       -- ^ Link a parent to an ordered child
  | PAlias      -- ^ Link an imported function to a Morloc name
  | PConstraint -- ^ Link a type to a constraint
  | PLabel      -- ^ Link a Morloc variable to a tag for the variable (TODO: replace this name)
  | PLang       -- ^ Link something to a specific programming language
  | PLeft       -- ^ Link something to its left-hand-side element
  | PNamespace  -- ^ Link a source or import to a namespace
  | POutput     -- ^ Link a function to its output
  | PPath       -- ^ Link a source to the file-system path
  | PPosition   -- ^ Link to the list index of an element
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
  |  OProperty                 -- ^ A property of the type (contains a 0 or more ordered elements)
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
  -- | Raise error if inappropriate function is called on unrealized manifold
  | NoBenefits
  -- | Raised when a branch is reached that should not be possible
  | CallTheMonkeys Text
  deriving(Eq)

data PackageMeta = PackageMeta {
    packageName        :: Text
  , packageVersion     :: Text
  , packageHomepage    :: Text
  , packageSynopsis    :: Text
  , packageDescription :: Text
  , packageCategory    :: Text
  , packageLicense     :: Text
  , packageAuthor      :: Text
  , packageMaintainer  :: Text
  , packageGithub      :: Text
  , packageBugReports  :: Text
  , packageGccFlags    :: Text
} deriving(Show, Ord, Eq)

defaultPackageMeta = PackageMeta {
    packageName        = ""
  , packageVersion     = ""
  , packageHomepage    = ""
  , packageSynopsis    = ""
  , packageDescription = ""
  , packageCategory    = ""
  , packageLicense     = ""
  , packageAuthor      = ""
  , packageMaintainer  = ""
  , packageGithub      = ""
  , packageBugReports  = ""
  , packageGccFlags    = ""
}

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
