{-|
Module      : Morloc.Namespace
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Namespace
  (
  -- ** re-export supplements to Prelude
    module Morloc.Internal
  -- ** Synonyms
  , MDoc
  , DAG
  -- ** Other functors
  , None(..)
  , One(..)
  , Many(..)
  -- ** Newtypes
  , CType(..)
  , ctype
  , GType(..)
  , generalType
  , EVar(..)
  , MVar(..)
  , TVar(..)
  , Name(..)
  , Path(..)
  , Code(..)
  -- ** Language
  , Lang(..)
  -- ** Data
  , Script(..)
  --------------------
  -- ** Error handling
  , MorlocError(..)
  -- ** Configuration
  , Config(..)
  -- ** Morloc monad
  , MorlocMonad
  , MorlocState(..)
  , MorlocReturn
  -- ** Package metadata
  , PackageMeta(..)
  , defaultPackageMeta
  -- * Types
  , Type(..)
  , UnresolvedType(..)
  , Source(..)
  -- ** Type extensions
  , Constraint(..)
  , Property(..)
  , langOf
  -- ** Types used in post-typechecking tree
  , SAnno(..)
  , SExpr(..)
  , GMeta(..)
  -- ** Typeclasses
  , HasOneLanguage(..)
  , Typelike(..)
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)
import Morloc.Internal
import Text.Megaparsec.Error (ParseError)
import Morloc.Language (Lang(..))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | no annotations for now
type MDoc = Doc ()

-- | A general purpose Directed Acyclic Graph (DAG)
type DAG key edge node = Map key (node, [(key, edge)])

type MorlocMonadGen c e l s a
   = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)

data MorlocState = MorlocState {
    statePackageMeta :: [PackageMeta]
  , stateVerbosity :: Int
  , stateCounter :: Int
}

type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

newtype Name = Name {unName :: Text} deriving (Show, Eq, Ord)
newtype Path = Path {unPath :: Text} deriving (Show, Eq, Ord)
newtype Code = Code {unCode :: Text} deriving (Show, Eq, Ord)

-- | Stores everything needed to build one file
data Script =
  Script
    { scriptBase :: !String -- ^ script basename (no extension)
    , scriptLang :: !Lang -- ^ script language
    , scriptCode :: !Code -- ^ full script source code
    , scriptCompilerFlags :: [Text] -- ^ compiler/interpreter flags
    , scriptInclude :: [Path] -- ^ paths to morloc module directories
    }
  deriving (Show, Ord, Eq)

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
  -- | Raised when a module cannot be loaded 
  | CannotLoadModule Text
  -- | System call failed
  | SystemCallError Text Text Text
  -- | Raised when there is an error in the code generators
  | GeneratorError Text
  -- | Missing a serialization or deserialization function
  | SerializationError Text
  -- | Error in building a pool (i.e., in a compiled language)
  | PoolBuildError Script Text
  -- | Raise error if inappropriate function is called on unrealized manifold
  | NoBenefits
  -- | Raise when a type alias substitution fails
  | SelfRecursiveTypeAlias TVar
  | MutuallyRecursiveTypeAlias [TVar]
  | BadTypeAliasParameters TVar Int Int 
  | ConflictingTypeAliases Type Type
  -- | Problems with the directed acyclic graph datastructures
  | DagMissingKey Text
  -- | Raised when a branch is reached that should not be possible
  | CallTheMonkeys Text
  --------------- T Y P E   E R R O R S --------------------------------------
  | MissingGeneralType
  | AmbiguousGeneralType
  | SubtypeError Type Type
  | ExistentialError
  | UnsolvedExistentialTerm
  | BadExistentialCast
  | AccessError Text
  | NonFunctionDerive
  | UnboundVariable EVar
  | OccursCheckFail
  | EmptyCut
  | TypeMismatch
  | ToplevelRedefinition
  | NoAnnotationFound -- I don't know what this is for
  | OtherError Text -- TODO: remove this option
  -- container errors
  | EmptyTuple
  | TupleSingleton
  | EmptyRecord
  -- module errors
  | MultipleModuleDeclarations [MVar]
  | BadImport MVar EVar
  | CannotFindModule MVar
  | CyclicDependency
  | SelfImport MVar
  | BadRealization
  | TooManyRealizations
  | MissingSource
  -- serialization errors
  | MissingPacker Text CType
  | MissingUnpacker Text CType
  -- type extension errors
  | AmbiguousPacker TVar
  | AmbiguousUnpacker TVar
  | AmbiguousCast TVar TVar
  | IncompatibleRealization MVar
  | MissingAbstractType
  | ExpectedAbstractType
  | CannotInferConcretePrimitiveType
  | ToplevelStatementsHaveNoLanguage
  | InconsistentWithinTypeLanguage
  | CannotInferLanguageOfEmptyRecord
  | ConflictingSignatures
  | CompositionsMustBeGeneral
  | IllegalConcreteAnnotation
  deriving (Eq)

data PackageMeta =
  PackageMeta
    { packageName :: !Text
    , packageVersion :: !Text
    , packageHomepage :: !Text
    , packageSynopsis :: !Text
    , packageDescription :: !Text
    , packageCategory :: !Text
    , packageLicense :: !Text
    , packageAuthor :: !Text
    , packageMaintainer :: !Text
    , packageGithub :: !Text
    , packageBugReports :: !Text
    , packageGccFlags :: !Text
    }
  deriving (Show, Ord, Eq)

defaultPackageMeta =
  PackageMeta
    { packageName = ""
    , packageVersion = ""
    , packageHomepage = ""
    , packageSynopsis = ""
    , packageDescription = ""
    , packageCategory = ""
    , packageLicense = ""
    , packageAuthor = ""
    , packageMaintainer = ""
    , packageGithub = ""
    , packageBugReports = ""
    , packageGccFlags = ""
    }

-- | Configuration object that is passed with MorlocMonad
data Config =
  Config
    { configHome :: !Path
    , configLibrary :: !Path
    , configTmpDir :: !Path
    , configLangPython3 :: !Path
    -- ^ path to python interpreter
    , configLangR :: !Path
    -- ^ path to R interpreter
    , configLangPerl :: !Path
    -- ^ path to perl interpreter
    }
  deriving (Show, Ord, Eq)


-- ================ T Y P E C H E C K I N G  =================================

newtype EVar = EVar { unEVar :: Text } deriving (Show, Eq, Ord)
newtype MVar = MVar { unMVar :: Text } deriving (Show, Eq, Ord)

data TVar = TV (Maybe Lang) Text deriving (Show, Eq, Ord)

data Source =
  Source
    { srcName :: Name
      -- ^ the name of the function in the source language
    , srcLang :: Lang
    , srcPath :: Maybe Path
    , srcAlias :: EVar
      -- ^ the morloc alias for the function (if no alias is explicitly given,
      -- this will be equal to the name
    }
  deriving (Ord, Eq, Show)

-- g: an annotation for the group of child trees (what they have in common)
-- f: a collection - before realization this will probably be Set
--                 - after realization it will be One
-- c: an annotation for the specific child tree
data SAnno g f c = SAnno (f (SExpr g f c, c)) g

data None = None
data One a = One a
data Many a = Many [a]

instance Functor One where
  fmap f (One x) = One (f x)

data SExpr g f c
  = UniS
  | VarS EVar
  | ListS [SAnno g f c]
  | TupleS [SAnno g f c]
  | LamS [EVar] (SAnno g f c)
  | AppS (SAnno g f c) [SAnno g f c]
  | NumS Scientific
  | LogS Bool
  | StrS Text
  | RecS [(EVar, SAnno g f c)]
  | CallS Source

-- | Description of the general manifold
data GMeta = GMeta {
    metaId :: Int
  , metaGType :: Maybe GType
  , metaName :: Maybe EVar -- the name, if relevant
  , metaProperties :: Set Property
  , metaConstraints :: Set Constraint
} deriving (Show, Ord, Eq)

newtype CType = CType { unCType :: Type }
  deriving (Show, Ord, Eq)

newtype GType = GType { unGType :: Type }
  deriving (Show, Ord, Eq)

-- a safe alternative to the CType constructor
ctype :: Type -> CType
ctype t
  | isJust (langOf t) = CType t
  | otherwise = error "COMPILER BUG - incorrect assignment to concrete type"

-- a safe alternative to the GType constructor
generalType :: Type -> GType
generalType t
  | isNothing (langOf t) = GType t
  | otherwise = error "COMPILER BUG - incorrect assignment to general type"

-- | Types, see Dunfield Figure 6
data Type
  = UnkT TVar
  -- ^ Unknown type: these may be serialized forms that do not need to be
  -- unserialized in the current environment but will later be passed to an
  -- environment where they can be deserialized. Alternatively, terms that are
  -- used within dynamic languages may need to type annotation.
  | VarT TVar
  -- ^ (a)
  | FunT Type Type
  -- ^ (A->B)  -- positional parameterized types
  | ArrT TVar [Type]
  -- ^ f [Type]  -- keyword parameterized types
  | NamT TVar [(Text, Type)] 
  -- ^ Foo { bar :: A, baz :: B }
  deriving (Show, Ord, Eq)

-- | Types, see Dunfield Figure 6
data UnresolvedType
  = VarU TVar
  -- ^ (a)
  | ExistU TVar [UnresolvedType] [UnresolvedType]
  -- ^ (a^) will be solved into one of the other types
  | ForallU TVar UnresolvedType
  -- ^ (Forall a . A)
  | FunU UnresolvedType UnresolvedType
  -- ^ (A->B)
  | ArrU TVar [UnresolvedType] -- positional parameterized types
  -- ^ f [UnresolvedType]
  | NamU TVar [(Text, UnresolvedType)] -- keyword parameterized types
  -- ^ Foo { bar :: A, baz :: B }
  deriving (Show, Ord, Eq)

data Property
  = Pack -- data structure to JSON
  | Unpack -- JSON to data structure
  | Cast -- casts from type A to B
  | GeneralProperty [Text]
  deriving (Show, Eq, Ord)

-- | Eventually, Constraint should be a richer type, but for they are left as
-- unparsed lines of text
newtype Constraint =
  Con Text
  deriving (Show, Eq, Ord)

class Typelike a where
  typeOf :: a -> Type

  nargs :: a -> Int
  nargs t = case typeOf t of
    (FunT _ t) -> 1 + nargs t
    _ -> 0

instance Typelike Type where
  typeOf = id

instance Typelike CType where
  typeOf (CType t) = t 

instance Typelike GType where
  typeOf (GType t) = t 

class HasOneLanguage a where
  langOf :: a -> Maybe Lang
  langOf' :: a -> Lang

instance HasOneLanguage CType where
  langOf (CType t) = langOf t

-- | Determine the language from a type, fail if the language is inconsistent.
-- Inconsistency in language should be impossible at the syntactic level, thus
-- an error in this function indicates a logical bug in the typechecker.
instance HasOneLanguage Type where
  langOf (UnkT (TV lang _)) = lang
  langOf (VarT (TV lang _)) = lang
  langOf x@(FunT t1 t2)
    | langOf t1 == langOf t2 = langOf t1
    | otherwise = error $ "inconsistent languages in" <> show x
  langOf x@(ArrT (TV lang _) ts)
    | all ((==) lang) (map langOf ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x 
  langOf (NamT _ []) = error "empty records are not allowed"
  langOf x@(NamT (TV lang _) ts)
    | all ((==) lang) (map (langOf . snd) ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x

instance HasOneLanguage TVar where
  langOf (TV lang _) = lang
