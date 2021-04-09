{-|
Module      : Morloc.Namespace
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2021
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
  , NamType(..)
  , EVar(..)
  , MVar(..)
  , TVar(..)
  , Name(..)
  , Path
  , Code(..)
  , DirTree(..)
  , AnchoredDirTree(..)
  , unEVar
  , unTVar
  -- ** Language
  , Lang(..)
  -- ** Data
  , Script(..)
  , SysCommand(..)
  -- ** Serialization
  , UnresolvedPacker(..)
  , PackMap
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
  , unresolvedType2type
  , Source(..)
  -- ** Type extensions
  , Constraint(..)
  , Property(..)
  -- ** Types used in post-typechecking tree
  , SAnno(..)
  , SExpr(..)
  , GMeta(..)
  -- ** Typeclasses
  , HasOneLanguage(..)
  , Typelike(..)
  , Scoped(..)
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)
import Morloc.Internal
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec ()
import System.Directory.Tree (DirTree(..), AnchoredDirTree(..))
import Morloc.Language (Lang(..))

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
  , stateOutfile :: Maybe Path
}

type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

newtype Name = Name {unName :: Text} deriving (Show, Eq, Ord)
type Path = String
newtype Code = Code {unCode :: Text} deriving (Show, Eq, Ord)

data SysCommand
  = SysExe Path
  | SysMove Path Path
  | SysRun Code
  | SysInstall Path
  | SysUnlink Path
  deriving (Show, Ord, Eq)

-- | Stores everything needed to build one package
data Script =
  Script
    { scriptBase :: !String -- ^ script basename (no extension)
    , scriptLang :: !Lang -- ^ script language
    , scriptCode :: !(AnchoredDirTree Code) -- ^ file tree containing all code and metadata
    , scriptMake :: ![SysCommand] -- ^ Bash code to build the script 
    }
  deriving (Show, Ord, Eq)

data UnresolvedPacker =
  UnresolvedPacker
    { unresolvedPackerTerm :: Maybe EVar
    -- ^ The general import term used for this type. For example, the 'Map'
    -- type may have language-specific realizations such as 'dict' or 'hash',
    -- but it is imported as 'import xxx (Map)'.
    , unresolvedPackerCType :: UnresolvedType
    -- ^ The decomposed (unpacked) type
    , unresolvedPackerForward :: [Source]
    -- ^ The unpack function, there may be more than one, the compiler will make
    -- a half-hearted effort to find the best one. It is called "Forward" since
    -- it is moves one step towards serialization.
    , unresolvedPackerReverse :: [Source]
    }
  deriving (Show, Ord, Eq)

type PackMap = Map (TVar, Int) [UnresolvedPacker]

data MorlocError
  -- | Raised when assumptions about the input RDF are broken. This should not
  -- occur for RDF that has been validated.
  = InvalidRDF Text
  -- | Raised for calls to unimplemented features
  | NotImplemented Text
  -- | Raised for unsupported features (such as specific languages)
  | NotSupported Text
  -- | Raised by parsec on parse errors
  | SyntaxError (ParseErrorBundle Text Void)
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
  | PoolBuildError Text
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
  | BadRecordAccess
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

defaultPackageMeta :: PackageMeta
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

newtype MVar = MVar { unMVar :: Text } deriving (Show, Eq, Ord)

-- | An expression variable. [Text] is scope of the variable relative to the
-- module. The definition of the term must be along this path. The path is a
-- stack, so if the term is in the global namespace, then the list is empty.
-- Every step into a `where` statement puts a new name on the stack. So the
-- namespace path is reverse order of normal filesystem paths, with the root
-- global namespace as the last element.
data EVar = EV [Text] Text deriving (Show, Eq, Ord)

data TVar = TV (Maybe Lang) Text deriving(Show, Eq, Ord)

-- | Let the TVar type behave like the MVar newtype
unTVar :: TVar -> Text
unTVar (TV _ t) = t

unEVar :: EVar -> Text
unEVar (EV _ e) = e

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
  | AccS (SAnno g f c) Text
  | ListS [SAnno g f c]
  | TupleS [SAnno g f c]
  | LamS [EVar] (SAnno g f c)
  | AppS (SAnno g f c) [SAnno g f c]
  | NumS Scientific
  | LogS Bool
  | StrS Text
  | RecS [(Text, SAnno g f c)]
  | CallS Source

-- | Description of the general manifold
data GMeta = GMeta {
    metaId :: Int
  , metaGType :: Maybe GType
  , metaName :: Maybe EVar -- the name, if relevant
  , metaProperties :: Set Property
  , metaConstraints :: Set Constraint
  , metaPackers :: Map (TVar, Int) [UnresolvedPacker]
  -- ^ The (un)packers available in this node's module scope. FIXME: kludge
  , metaConstructors :: Map TVar Source
  -- ^ The constructors in this node's module scope. FIXME: kludge
  , metaTypedefs :: Map TVar (Type, [TVar])
  -- ^ Everything needed to make the prototypes and serialization generic
  -- functions in C++
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

data NamType
  = NamRecord
  | NamObject
  | NamTable
  deriving(Show, Ord, Eq)

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
  | NamT NamType TVar [Type] [(Text, Type)] 
  -- ^ Foo { bar :: A, baz :: B }
  deriving (Show, Ord, Eq)

-- | Types, see Dunfield Figure 6
data UnresolvedType
  = VarU TVar
  -- ^ (a)
  | ExistU TVar
    [UnresolvedType] -- ???
    [UnresolvedType] -- default types
  -- ^ (a^) will be solved into one of the other types
  | ForallU TVar UnresolvedType
  -- ^ (Forall a . A)
  | FunU UnresolvedType UnresolvedType
  -- ^ (A->B)
  | ArrU TVar [UnresolvedType] -- positional parameterized types
  -- ^ f [UnresolvedType]
  | NamU NamType TVar [UnresolvedType] [(Text, UnresolvedType)] -- keyword parameterized types
  -- ^ Foo { bar :: A, baz :: B }
  deriving (Show, Ord, Eq)

unresolvedType2type :: UnresolvedType -> Type 
unresolvedType2type (VarU v) = VarT v
unresolvedType2type (FunU t1 t2) = FunT (unresolvedType2type t1) (unresolvedType2type t2) 
unresolvedType2type (ArrU v ts) = ArrT v (map unresolvedType2type ts)
unresolvedType2type (NamU r v ts rs) = NamT r v (map unresolvedType2type ts) (zip (map fst rs) (map (unresolvedType2type . snd) rs))
unresolvedType2type (ExistU _ _ _) = error "Cannot cast existential type to Type"
unresolvedType2type (ForallU _ _) = error "Cannot cast universal type as Type"


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

class Scoped a where
  scopeOf :: a -> [Text]

instance Scoped EVar where
  scopeOf (EV ns _) = ns

class Typelike a where
  typeOf :: a -> Type

  -- | Break a type into its input arguments, and final output
  -- For example: decompose ((a -> b) -> [a] -> [b]) would 
  -- yield ([(a->b), [a]], [b])
  decompose :: a -> ([a], a)

  -- | like @decompose@ but concatentates the output type
  decomposeFull :: a -> [a]
  decomposeFull t = case decompose t of
    (xs, x) -> (xs ++ [x])

  nargs :: a -> Int
  nargs t = case typeOf t of
    (FunT _ t') -> 1 + nargs t'
    _ -> 0

instance Typelike Type where
  typeOf = id

  decompose (FunT t1 t2) = case decompose t2 of
    (ts, finalType) -> (t1:ts, finalType) 
  decompose t = ([], t)


instance Typelike CType where
  typeOf (CType t) = t 

  decompose t0 = case (decompose (unCType t0)) of
    (ts, t) -> (map CType ts, CType t)

instance Typelike GType where
  typeOf (GType t) = t 

  decompose t0 = case (decompose (unGType t0)) of
    (ts, t) -> (map GType ts, GType t)

class HasOneLanguage a where
  langOf :: a -> Maybe Lang
  langOf' :: a -> Lang

  langOf x = Just (langOf' x) 
  langOf' x = fromJust (langOf x)

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
  langOf (NamT _ _ _ []) = error "empty records are not allowed"
  langOf x@(NamT _ (TV lang _) _ ts)
    | all ((==) lang) (map (langOf . snd) ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x

instance HasOneLanguage TVar where
  langOf (TV lang _) = lang

instance HasOneLanguage UnresolvedType where
  langOf (VarU (TV lang _)) = lang
  langOf x@(ExistU (TV lang _) ts _)
    | all ((==) lang) (map langOf ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x
  langOf x@(ForallU (TV lang _) t)
    | lang == langOf t = lang
    | otherwise = error $ "inconsistent languages in " <> show x
  langOf x@(FunU t1 t2)
    | langOf t1 == langOf t2 = langOf t1
    | otherwise = error $ "inconsistent languages in" <> show x
  langOf x@(ArrU (TV lang _) ts)
    | all ((==) lang) (map langOf ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x 
  langOf (NamU _ _ _ []) = error "empty records are not allowed"
  langOf x@(NamU _ (TV lang _) _ rs)
    | all ((==) lang) (map (langOf . snd) rs) = lang
    | otherwise = error $ "inconsistent languages in " <> show x
