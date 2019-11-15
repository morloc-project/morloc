{-|
Module      : Morloc.Namespace
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Namespace
  (
  -- ** re-export supplements to Prelude
    module Morloc.Internal
  -- ** Typeclasses
  , MorlocTypeable(..)
  -- ** Synonyms
  , MDoc
  , AbstractType
  , ConcreteType
  , Name
  , Path
  , Code
  -- ** Newtypes
  , URI(..)
  -- ** Language
  , Lang(..)
  -- ** Data
  , Script(..)
  , Manifold(..)
  -- compatibility --- KILL THEM ALL!!!
  , mLang
  , mName
  , mConcreteType 
  , mModulePath
  , mSourcePath
  , mSourced 
  --------------------
  , Realization(..)
  , Argument(..)
  , FunctionDeclaration(..)
  , Call(..)
  , MData(..)
  , MType(..)
  , MTypeMeta(..)
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
  -- * Typechecking
  , EVar(..)
  , Expr(..)
  , Gamma
  , GammaIndex(..)
  , Import(..)
  , Indexable(..)
  , MVar(..)
  , Module(..)
  , Stack
  , StackState(..)
  , TVar(..)
  , Type(..)
  -- ** State manipulation
  , StackConfig(..)
  -- ** ModuleGamma paraphernalia
  , ModularGamma
  -- ** Type extensions
  , Constraint(..)
  , EType(..)
  , Property(..)
  , TypeSet(..)
  , langOf
  , langOf'
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Set (Set, empty)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)
import Morloc.Internal
import Text.Megaparsec.Error (ParseError)
import Morloc.Language (Lang(..))

-- | no annotations for now
type MDoc = Doc ()

class MorlocTypeable a where
  asType :: a -> MorlocMonad MType

data Dependency
  = ModuleDependency Name Path Lang
  | ExecutableDependency Name Path
  | SourceCodeDependency Name Path Lang
  deriving (Show, Ord, Eq)

type MorlocMonadGen c e l s a
   = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)

data MorlocState =
  MorlocState
    { dependencies :: [Dependency]
    , statePackageMeta :: [PackageMeta]
    , stateSerialMaps :: Map Lang SerialMap
    , stateVerbosity :: Int
    }

type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

type Name = Text

type Path = Text

type Code = Text

newtype URI =
  URI Text
  deriving (Show, Eq, Ord)

-- | Universal Morloc type
type AbstractType = MType

-- | Language-specific type
type ConcreteType = MType

-- | Stores everything needed to build one file
data Script =
  Script
    { scriptBase :: !String -- ^ script basename (no extension)
    , scriptLang :: !Lang -- ^ script language
    , scriptCode :: !Text -- ^ full script source code
    , scriptCompilerFlags :: [Text] -- ^ compiler/interpreter flags
    , scriptInclude :: [Path] -- ^ paths to morloc module directories
    }
  deriving (Show, Ord, Eq)

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
data Manifold =
  Manifold
    { mid :: !Integer
    -- ^ An ID uniquely identifying this manifold. Each manifold appears only
    -- once in the generated code.
    , mCallId :: !URI
    -- ^ The RDF identifier for this manifold
    , mAbstractType :: Maybe AbstractType
    -- ^ The Morloc type for the wrapped function 
    , mRealizations :: [Realization]
    -- ^ Language specific instances for this manifold
    , mMorlocName :: !Name
    -- ^ e.g., in @source "R" ("runif" as rand_uniform)@, "rand_uniform" is
    -- morloc name and "runif" is the R name
    , mExported :: !Bool
    -- ^ If False, then it is purely a local function
    -- If True, then this manifold will be
    -- 1) callable from the nexus (if this script is the top-level module)
    -- 2) imported if the module is imported
    , mCalled :: !Bool
    -- ^ Is this function called within a Morloc composition?
    , mDefined :: !Bool
    -- ^ Is this a Morloc function
    , mPassed :: !Bool
    -- ^ Is this function passed as an argument to a concrete function? If so,
    -- then its inputs must match the arguments passed to it by the concrete
    -- function (i.e., they must not be serialized).
    , mComposition :: Maybe Name
    -- ^ The name of the declaration function. For example, in
    -- @foo x = sqrt x@, "foo" is mComposition
    , mBoundVars :: [Name]
    , mArgs :: [Argument]
    -- ^ Arguments passed to the wrapped function. There should be one element
    -- in this list for every input to the function specified in mAbstractType.
    -- This may change once currying is implemented.
    }
  deriving (Show, Eq, Ord)

-- | Store a function declaration, e.g., (foo x y = bar (baz x) y)
data FunctionDeclaration =
  FunctionDeclaration
    { fdId :: !URI
    -- ^ RDF URI for the object of type DataDeclaration
    , fdName :: !Name
    -- ^ name of the funtion ("foo")
    , fdCallId :: !URI
    -- ^ RDF URI for the called function (URI of bar)
    , fdExported :: !Bool
    -- ^ Is this function exported?
    , fdArgs :: [Name]
    -- ^ list of bound arguments (["x","y"])
    }
  deriving (Show, Eq, Ord)

-- | Store a single call, e.g., in (foo x y = bar (baz x) y) there would be two
-- calls, "bar" and "baz", with 2 and 1 arguments, respectively.
data Call =
  Call
    { callId :: !URI
    , callName :: !Name
    , callArgs :: [Argument]
    }
  deriving (Show, Eq, Ord)

-- FIXME: The following functions are provided for compatibility. It allows
-- code to pretend the Manifold record is the same as the old one (before
-- adding realizations).
getReal :: (Realization -> a) -> Manifold -> a
getReal f m =
  case mRealizations m of
    [r] -> f r
    (r:_) -> f r
    [] -> error ("Missing realization in m = " ++ show m)

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
data Realization =
  Realization
    { rLang :: !Lang
    -- ^ The implementation language for the concrete function.
    , rName :: !Name
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
    , rSourced :: !Bool
    -- ^ True if this function read from sourced (e.g., @source "R" ("runif")@)
    }
  deriving (Show, Ord, Eq)

-- | The role a manifold plays relative to a given language.
-- TODO: reconsider what these really mean, the usage seems to have deviated
-- from the original intent.
data ManifoldClass
  = Cis -- ^ Wrapper around a Morloc composition
  | Trans -- ^ Wrapper around a foreign call
  | Source -- ^ Wrapper around a source function
  | Uncalled -- ^ Does not need to be built in current language
  deriving (Show, Ord, Eq)

data Argument
  = ArgName Name
  -- ^ Morloc variables that are defined in scope
  | ArgNest Name
  -- ^ Literal name of a manifold or morloc variable
  | ArgCall URI
  -- ^ A call to some manifold
  | ArgData MData
  -- ^ Raw data defined in one of the Morloc internal types
  | ArgPosi Int
  -- ^ A manifold positional argument (passed into a function assignment)
  deriving (Show, Eq, Ord)

-- | The values are left unparsed, since they will be used as text
data MData
  = Num' Text
  | Str' Text
  | Log' Bool -- booleans are parsed, since representation depend on language
  | Lst' [MData]
  | Rec' [(Name, MData)]
  | Tup' [MData]
  deriving (Show, Eq, Ord)

-- | A Morloc type, may be a language specific type.
data MType
  = MConcType !MTypeMeta !Name [MType] -- ^ A non-generic, concrete type
  | MAbstType !MTypeMeta !Name [MType] -- ^ A generic, wildcard, type
  | MFuncType !MTypeMeta [MType] MType -- ^ A function type with a list of input types and a single output type
  deriving (Show, Eq)

-- TODO: add constraints
data MTypeMeta =
  MTypeMeta
    { metaName :: Maybe Name
    , metaProp :: [[Name]]
    -- ^ A list of properties. Each property is a list of names. These may be
    -- single properties of the function (e.g., "packs") or relations
    -- describing generic variables (e.g., "Num a", like a Haskell typeclass),
    -- or more general logical relations. Nesting is currently not allowed.
    , metaLang :: Maybe Lang
    -- ^ The language. TODO: make Nothing mean it is a Morloc function, and
    -- "Just lang" mean it is a concrete type from the language "lang".
    }
  deriving (Show, Eq)

-- The most specific type is highest.
-- The lowest type is the generic 'a'.
-- The order of types is used to choose the most specific serialization functions.
instance Ord MType
  -- Concrete is more specific 
                                where
  compare (MConcType _ _ _) (MAbstType _ _ _) = GT
  compare (MAbstType _ _ _) (MConcType _ _ _) = LT
  -- Function types are somewhat arbitrarily more specific than concrete types
  compare (MFuncType _ _ _) (MConcType _ _ _) = GT
  compare (MConcType _ _ _) (MFuncType _ _ _) = LT
  -- Function types are more specific than abstract types
  compare (MFuncType _ _ _) (MAbstType _ _ _) = GT
  compare (MAbstType _ _ _) (MFuncType _ _ _) = LT
  -- For similar types, compare first the children, then the properties, then the name
  compare (MConcType d1 n1 xs1) (MConcType d2 n2 xs2) =
    compare (xs1, d1, n1) (xs2, d2, n2)
  compare (MAbstType d1 n1 xs1) (MAbstType d2 n2 xs2) =
    compare (xs1, d1, n1) (xs2, d2, n2)
  compare (MFuncType d1 ins1 o1) (MFuncType d2 ins2 o2) =
    compare (ins1, o1, d1) (ins2, o2, d2)

-- As with Ord instance of MType, the most specific type should be highest.
instance Ord MTypeMeta where
  compare (MTypeMeta n1 ps1 l1) (MTypeMeta n2 ps2 l2)
    -- having more properties is more specific
    | length ps1 /= length ps2 = compare (length ps1) (length ps2)
    | otherwise =
      case ((n1, l1), (n2, l2)) of
      -- having a concrete language is more specific
        ((_, Just _), (_, Nothing)) -> GT
        ((_, Nothing), (_, Just _)) -> LT
      -- otherwise, compare based off names
        _ -> compare (l1, n2, ps1) (l2, n2, ps2)

data SerialMap =
  SerialMap
    { serialLang :: !Lang
    , serialPacker :: Map MType Name
    , serialUnpacker :: Map MType Name
    , serialSources :: [Path]
    -- ^ The absolute paths to the source files
    }
  deriving (Show, Eq, Ord)

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
  -- | Raised when a dependency is missing
  | DependencyError Dependency
  -- | Error in building a pool (i.e., in a compiled language)
  | PoolBuildError Script Text
  -- | Raise error if inappropriate function is called on unrealized manifold
  | NoBenefits
  -- | Raised when a branch is reached that should not be possible
  | CallTheMonkeys Text
  --------------- T Y P E   E R R O R S --------------------------------------
  | MissingGeneralType
  | AmbiguousGeneralType
  | SubtypeError Type Type
  | ExistentialError
  | BadExistentialCast
  | AccessError Text
  | NonFunctionDerive
  | UnboundVariable EVar
  | OccursCheckFail
  | EmptyCut
  | TypeMismatch
  | UnexpectedPattern Expr Type
  | ToplevelRedefinition
  | NoAnnotationFound -- I don't know what this is for
  | OtherError Text
  -- container errors
  | EmptyTuple
  | TupleSingleton
  | EmptyRecord
  -- module errors
  | MultipleModuleDeclarations MVar
  | BadImport MVar EVar
  | CannotFindModule MVar
  | CyclicDependency
  | CannotImportMain
  | SelfImport MVar
  | BadRealization
  | TooManyRealizations
  | MissingSource
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
    { configHome :: !Text
    , configLibrary :: !Text
    , configTmpDir :: !Text
    , configLangPython3 :: !Text
    , configLangR :: !Text
    , configLangPerl :: !Text
    }
  deriving (Show, Ord, Eq)


-- ================ T Y P E C H E C K I N G  =================================

type Gamma = [GammaIndex]

newtype EVar = EV Text deriving (Show, Eq, Ord)
newtype MVar = MV Text deriving (Show, Eq, Ord)

data TVar = TV (Maybe Lang) Text deriving (Show, Eq, Ord)

type GeneralStack c e l s a
   = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

type Stack a = GeneralStack StackConfig MorlocError [Text] StackState a

data StackConfig =
  StackConfig
    { stackConfigVerbosity :: Int
    }

data StackState =
  StackState
    { stateVar :: Int
    , stateQul :: Int
    , stateSer :: [(Type, Type)]
    , stateDepth :: Int
    }
  deriving (Ord, Eq, Show)

-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG Expr TypeSet
  -- ^ (G,x:A) looked up in the (Var) and cut in (-->I)
  | ExistG TVar [Type]
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar Type
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | MarkEG EVar
  -- ^ ...
  | SrcG EVar Lang (Maybe Path) EVar
  -- ^ source
  | ConcreteG EVar Lang Type
  -- ^ store a local concrete type
  | UnsolvedConstraint Type Type
  -- ^ Store an unsolved serialization constraint containing one or more
  -- existential variables. When the existential variables are solved, the
  -- constraint will be written into the Stack state.
  deriving (Ord, Eq, Show)

data Import =
  Import
    { importModuleName :: MVar
    , importInclude :: Maybe [(EVar, EVar)]
    , importExclude :: [EVar]
    , importNamespace :: Maybe EVar -- currently not used
    }
  deriving (Ord, Eq, Show)

data Module =
  Module
    { moduleName :: MVar
    , modulePath :: Maybe Path
    , moduleImports :: [Import]
    , moduleExports :: [EVar]
    , moduleBody :: [Expr]
    , moduleTypeMap :: Map EVar TypeSet
    }
  deriving (Ord, Eq, Show)

-- | Terms, see Dunfield Figure 1
data Expr
  = SrcE Lang (Maybe Path) [(EVar, EVar)]
  -- ^ import "c" from "foo.c" ("f" as yolo)
  | Signature EVar EType
  -- ^ x :: A; e
  | Declaration EVar Expr
  -- ^ x=e1; e2
  | UniE
  -- ^ (())
  | VarE EVar
  -- ^ (x)
  | ListE [Expr]
  -- ^ [e]
  | TupleE [Expr]
  -- ^ (e1), (e1,e2), ... (e1,e2,...,en)
  | LamE EVar Expr
  -- ^ (\x -> e)
  | AppE Expr Expr
  -- ^ (e e)
  | AnnE Expr [Type]
  -- ^ (e : A)
  | NumE Scientific
  -- ^ number of arbitrary size and precision
  | LogE Bool
  -- ^ boolean primitive
  | StrE Text
  -- ^ literal string
  | RecE [(EVar, Expr)]
  deriving (Show, Ord, Eq)

-- | Types, see Dunfield Figure 6
data Type
  = VarT TVar
  -- ^ (a)
  | ExistT TVar [Type]
  -- ^ (a^) will be solved into one of the other types
  | Forall TVar Type
  -- ^ (Forall a . A)
  | FunT Type Type
  -- ^ (A->B)
  | ArrT TVar [Type]
  -- ^ f [Type]
  | RecT [(TVar, Type)]
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

-- | Extended Type that may represent a language specific type as well as sets
-- of properties and constrains.
data EType =
  EType
    { etype :: Type
    , eprop :: Set Property
    , econs :: Set Constraint
    , esource :: Maybe (Maybe Path, EVar)
    }
  deriving (Show, Eq, Ord)

data TypeSet =
  TypeSet (Maybe EType) [EType]
  deriving (Show, Eq, Ord)

type ModularGamma = Map MVar (Map EVar TypeSet)

class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable Type where
  index (ExistT t ts) = ExistG t ts
  index t = error $ "Can only index ExistT, found: " <> show t

class HasOneLanguage a where
  langOf :: a -> Maybe Lang
  langOf' :: a -> Lang

  langOf' (langOf -> Nothing) = MorlocLang
  langOf' (langOf -> (Just lang)) = lang

-- | Determine the language from a type, fail if the language is inconsistent.
-- Inconsistency in language should be impossible at the syntactic level, thus
-- an error in this function indicates a logical bug in the typechecker.
instance HasOneLanguage Type where
  langOf (VarT (TV lang _)) = lang
  langOf x@(ExistT (TV lang _) ts)
    | all ((==) lang) (map langOf ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x
  langOf x@(Forall (TV lang _) t)
    | lang == langOf t = lang
    | otherwise = error $ "inconsistent languages in " <> show x
  langOf x@(FunT t1 t2)
    | langOf t1 == langOf t2 = langOf t1
    | otherwise = error $ "inconsistent languages in" <> show x
  langOf x@(ArrT (TV lang _) ts)
    | all ((==) lang) (map langOf ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x 
  langOf (RecT []) = error "empty records are not allowed"
  langOf x@(RecT ts@((TV lang _, _):_))
    | all ((==) lang) (map (langOf . snd) ts) &&
      all ((==) lang) (map (\(TV l _, _) -> l) ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x

instance HasOneLanguage EType where
  langOf e = langOf (etype e) 
