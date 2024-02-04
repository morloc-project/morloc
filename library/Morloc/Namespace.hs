{-# LANGUAGE ViewPatterns, OverloadedStrings, TypeFamilies #-}

{-|
Module      : Morloc.Namespace
Description : All types and datastructures
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Namespace
  (
  -- ** re-exports
    module Morloc.Internal
  -- ** Synonyms
  , MDoc
  , DAG
  , Scope
  -- ** Other functors
  , None(..)
  , One(..)
  , Or(..)
  , Many(..)
  , ManyPoly(..)
  -- ** Other classes
  , Three(..)
  , Defaultable(..)
  -- ** Indexed
  , IndexedGeneral(..)
  , Indexed
  -- ** Newtypes
  , MVar(..)
  , EVar(..)
  , TVar(..)
  , Typeclass(..)
  , CVar(..)
  , Key(..)
  , Label(..)
  , SrcName(..)
  , Path
  , Code(..)
  , DirTree(..)
  , AnchoredDirTree(..)
  -- ** Language
  , Lang(..)
  -- ** Data
  , Script(..)
  , SysCommand(..)
  , GMap(..)
  , GMapRet(..)
  --------------------
  -- ** Error handling
  , MorlocError(..)
  , TypeError(..)
  -- ** Configuration
  , Config(..)
  -- ** Morloc monad
  , MorlocMonad
  , MorlocState(..)
  , SignatureSet(..)
  , TermTypes(..)
  , MorlocReturn
  -- ** Package metadata
  , PackageMeta(..)
  -- * Types
  , NamType(..)
  , Type(..)
  , TypeU(..)
  , extractKey
  , type2typeu
  , EType(..)
  , unresolvedType2type
  , Source(..)
  -- * Typechecking
  , Gamma(..)
  , GammaIndex(..)
  -- * Mostly frontend expressions
  , Symbol(..)
  , AliasedSymbol(..)
  , Signature(..)
  , Expr(..)
  , ExprI(..)
  , Import(..)
  , Exports(..)
  -- ** Type extensions
  , Constraint(..)
  , Property(..)
  -- ** Types used in post-typechecking tree
  , AnnoS(..)
  , ExprS(..)
  , mapAnnoSM
  , mapExprSM
  , mapAnnoS
  , mapExprS
  , mapAnnoSC
  , mapAnnoSCM
  , mapAnnoSG
  , mapAnnoSGM
  , mapExprSC
  , mapExprSCM
  , mapExprSG
  , mapExprSGM
  -- ** Typeclasses
  , HasOneLanguage(..)
  , Typelike(..)
  -- , Decomposable(..)
  -- ** kludge
  , newVariable
  -- Partial order logic
  , isSubtypeOf
  , equivalent
  , mostGeneral
  , mostSpecific
  , mostSpecificSubtypes
  ) where

import Morloc.Language (Lang(..))

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (runIdentity)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import Morloc.Internal
import Text.Megaparsec (ParseErrorBundle)
import System.Directory.Tree (DirTree(..), AnchoredDirTree(..))
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.PartialOrd as P
import qualified Data.List as DL
import Data.Aeson (FromJSON(..), (.!=), (.:?), withObject)

import Morloc.Data.Doc
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as DT

---- Typeclasses

class Typelike a where
  typeOf :: a -> Type

  free :: a -> Set.Set a

  -- | substitute all appearances of a given variable with a given new type
  substituteTVar :: TVar -> a -> a -> a

  nargs :: a -> Int
  nargs (typeOf -> FunT ts _) = length ts
  nargs _ = 0

  -- | Curry function types. This converts types like `a -> (a -> a)` to
  -- `a -> a -> a`. Ideally, this should not be necessary, since these are
  -- equivalent types. Ideally, this equivalence would be deeply baked into
  -- the system and I wouldn't have to worry about fixing it ...
  -- FIXME: make it so
  normalizeType :: a -> a

class HasOneLanguage a where
  langOf :: a -> Maybe Lang
  langOf' :: a -> Lang

  langOf x = Just (langOf' x)
  langOf' x = fromJust (langOf x)

class Defaultable a where
  defaultValue :: a


---- Type definitions


-- | no annotations for now
type MDoc = Doc ()

-- | A general purpose Directed Acyclic Graph (DAG). Technically this structure
-- needn't be acyclic. You can use `findCycle` to check whether a given stucture
-- has cycles.
type DAG key edge node = Map key (node, [(key, edge)])

type Scope = Map TVar [([TVar], TypeU, Bool)]

data GMap a b c = GMap (Map a b) (Map b c)
  deriving(Show, Ord, Eq)

data GMapRet c
  = GMapNoFst -- ^ Failure on the first key
  | GMapNoSnd -- ^ Failure on the internal key (possible bug)
  | GMapJust c
  deriving(Show, Ord, Eq)

type MorlocMonadGen c e l s a
   = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)

data SignatureSet = Monomorphic TermTypes | Polymorphic Typeclass EVar EType [TermTypes]
  deriving(Show)


data MorlocState = MorlocState
  { statePackageMeta :: [PackageMeta]
  -- ^ The parsed contents of a package.yaml file
  , stateVerbosity :: Int
  , stateCounter :: Int
  -- ^ Used in Treeify generate new indices (starting from max parser index).
  -- Also used (after resetting to 0) in each of the backend generators.
  , stateDepth :: Int
  -- ^ store depth in the AnnoS tree in the frontend and backend typecheckers
  , stateSignatures :: GMap Int Int SignatureSet
  , stateConcreteTypedefs :: GMap Int MVar (Map Lang Scope)
  -- ^ stores type functions that are in scope for a given module and language
  , stateGeneralTypedefs  :: GMap Int MVar           Scope
  -- ^ Stores all concrete type definitions available to an index e.g.:
  --   `type Cpp (Map k v) = "std::map<$1,$2>" k v`
  --   Where `TVar` is `Map`
  --         `Type` is `"std::map<$1,$2>" k v`
  --         `[TVar]` is `[k,v]`
  , stateUniversalGeneralTypedefs :: Scope
  -- ^ store the general typedefs pooled across all modules -- for the truly desparate
  , stateUniversalConcreteTypedefs :: Map Lang Scope
  -- ^ store the concrete typedefs pooled across all modules -- for the truly desparate
  , stateInnerMogrifiers :: GMap Int MVar (Map Property [(TypeU, Source)])
  , stateUniversalInnerMogrifiers :: Map Property [(TypeU, Source)]
  , stateSources :: GMap Int MVar [Source]
  , stateAnnotations :: Map Int [TypeU]
  -- ^ Stores non-top-level annotations.
  , stateOutfile :: Maybe Path
  -- ^ The nexus filename ("nexus.py" by default)
  , stateExports :: [Int]
  -- ^ The indices of each exported term
  , stateName :: Map Int EVar
  -- ^ store the names of morloc compositions
  }
  deriving(Show)

{-
       A           - There can be only one general signature for a term within a scope
      / \          - All parental constraints are inherited
     /   \         - Order of constraint application does not matter
    /     \        - If you import two terms under the same alias, then they must be the same
   A+c1    A+c2      at the general level
    \     /   \    - You are, of course, free to import them under different aliases, in which
     \   /     \     case they will not be unified
      \ /       \
      A+c3       A+c4
      ---        ----
      A+c1+c2+c3 A+c2+c4

       a           - Is there a fundamental difference between types and constraints?
      / \          - A constraint reduces the set of elements contained in the type
     /   \         - A type specifies the broadest set, but this is still just a constraint
    /     \          of the set from all things to one kind of thing
   A+c1    B+c2    - The problem with the generic `a` being specialized into A and B and then being
    \     /   \      imported is that probably the intersection between A and B is empty. But maybe
     \   /     \      it would make more sense for the result to be a union type, where X may be
      \ /       \      either A or B. And which it is is determined at compile time from context.
       X         B+c4    Maybe A and B are two representations of the same thing, like a `map` type
      ---        ----     may be represented as either `[(a,b)]` or `([a]_n,[b]_n)`.
      error?     B+c2+c4  - No, that is too complicated. For now, there may be only one base type
    or union?               per scope and all imported types must agree (only constraints may
                            differ).

     f.1  f.2  f.3  -- Three different concrete type signatures for f, possibly with different sources
         / \           but all in the same language. Different languages do not interact, so add no
        /   \          new type-level complexity.
       /     \
    f.4       `-------.      -- Concrete type signatures may only be added where new functions
    --------------     \        are sourced.
    f.1 f.2 f.3 f.4     \
       \                 f.5
        `---------.      ---------------
                   \     f.1 f.2 f.3 f.5
                    \   /
                     \ /
                     ( )    -- no new sources
                     ---
                     f.1 f.2 f.3 f.4 f.5   -- We need to merge duplicates here, f.[123] are the same
                                              since they were all defined in the first scope. Since
                                              concrete types cannot be appended, this merging is
                                              trivial.

  -- In the downstream realization phase, the compiler needs to choose one instance for each function.
  -- Currently, this step consists only of choosing which language to use by minimizing the number of
  -- interops required and preferring "faster" languages.
-}

-- | stores everything that is given about a term
data TermTypes = TermTypes {
    termGeneral :: Maybe EType
  -- ^ A term may have many general types (up to one in each scope)
  , termConcrete :: [(MVar, Indexed Source)]
  -- ^ The module name (MVar) is needed to lookup package metadata (if needed).
  -- The source is optional, since language-specific types may specified
  -- without sources as interfaces.
  , termDecl :: [ExprI]
  -- ^ all declarations of this type
  --      AssE EVar ExprI [ExprI]
  --            ^     ^      ^----- TermType knows nothing about this
  --            '      '--- each ExprI in [ExprI] is one of these
  --            '--- this will match the term name
} deriving (Show)



-- | Distinguishes between term and type symbols in import/export expression
-- before they are separated in Treeify.
data Symbol = TypeSymbol TVar | TermSymbol EVar
  deriving (Show, Ord, Eq)

data Exports = ExportMany (Set.Set Symbol) | ExportAll
  deriving (Show, Ord, Eq)

data AliasedSymbol = AliasedType TVar TVar | AliasedTerm EVar EVar
  deriving (Show, Ord, Eq)

data Signature = Signature EVar (Maybe Label) EType
  deriving (Show, Ord, Eq)

data ExprI = ExprI Int Expr
  deriving (Show, Ord, Eq)

-- | Terms, see Dunfield Figure 1
data Expr
  = ModE MVar [ExprI]
  -- ^ the toplevel expression in a module
  | ClsE Typeclass [TVar] [Signature]
  | IstE Typeclass [TypeU] [ExprI]
  | TypE (Maybe (Lang, Bool)) TVar [TVar] TypeU
  -- ^ a type definition
  --   1. the language, Nothing is general
  --      If Just, the Bool specifies whether the definition is terminal
  --   2. type name
  --   3. parameters
  --   4. type
  | ImpE Import
  -- ^ a morloc module import
  | ExpE Symbol
  -- ^ a term that is exported from a module (should only exist at the toplevel)
  | SrcE Source
  -- ^ import "c" from "foo.c" ("f" as yolo).
  | SigE Signature
  -- ^ A type signature, the three parameters correspond to the term name, the
  -- optional label, and the type
  | AssE EVar ExprI [ExprI]
  -- ^ x=e1
  -- 1. term name
  -- 2. term
  -- 3. term where statements
  | UniE
  -- ^ (())
  | VarE EVar
  -- ^ (x)
  | AccE Key ExprI
  -- ^ person@age - access a field in a record
  | LstE [ExprI]
  | TupE [ExprI]
  | NamE [(Key, ExprI)]
  | AppE ExprI [ExprI]
  -- ^ Function application
  | LamE [EVar] ExprI
  -- ^ (\x -> e)
  | AnnE ExprI [TypeU]
  -- ^ (e : A)
  | RealE Scientific
  -- ^ number of arbitrary size and precision
  | IntE Integer
  -- ^ integer of arbitrayr size
  | LogE Bool
  -- ^ boolean primitive
  | StrE Text
  -- ^ string primitive
  deriving (Show, Ord, Eq)

data Import =
  Import
    { importModuleName :: MVar
    , importInclude :: Maybe [AliasedSymbol]
    , importExclude :: [Symbol]
    , importNamespace :: Maybe EVar -- currently not used
    }
  deriving (Ord, Eq, Show)


type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

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

-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG EVar TypeU
  -- ^ store a bound variable
  | ExistG TVar
    [TypeU] -- type parameters
    [(Key, TypeU)] -- keys
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar TypeU
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | SrcG Source
  -- ^ source
  deriving (Ord, Eq, Show)


data Gamma = Gamma
  { gammaCounter :: Int
  , gammaContext :: [GammaIndex]
  }

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

-- | Configuration object that is passed with MorlocMonad
data Config =
  Config
    { configHome :: !Path
    , configLibrary :: !Path
    , configPlain :: !Path
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

-- A module name
newtype MVar = MV { unMVar :: Text } deriving (Show, Eq, Ord)

-- A term name
newtype EVar = EV { unEVar :: Text } deriving (Show, Eq, Ord)


-- A type general name
newtype TVar = TV { unTVar :: Text } deriving (Show, Eq, Ord)

newtype Typeclass = Typeclass { unTypeclass :: Text } deriving (Show, Eq, Ord)

-- A concrete type name
newtype CVar = CV { unCVar :: Text } deriving (Show, Eq, Ord)

newtype Key = Key { unKey :: Text } deriving (Show, Eq, Ord)

newtype Label = Label { unLabel :: Text } deriving (Show, Eq, Ord)

-- The name of a source function, this is text which may be illegal in morloc
-- (such as the R function "file.exists")
newtype SrcName = SrcName {unSrcName :: Text} deriving (Show, Eq, Ord)

newtype Code = Code {unCode :: Text} deriving (Show, Eq, Ord)

-- this is a string because the path libraries want strings
type Path = String

data Source =
  Source
    { srcName :: SrcName
      -- ^ the name of the function in the source language
    , srcLang :: Lang
    , srcPath :: Maybe Path
      -- ^ "Maybe" since the path does not exist when we import a builtin
    , srcAlias :: EVar
      -- ^ the morloc alias for the function (if no alias is explicitly given,
      -- this will be equal to the name
    , srcLabel :: Maybe Label
      -- ^ an additional label for distinguishing this term from its synonyms
    }
  deriving (Ord, Eq, Show)

data AnnoS g f c = AnnoS g c (ExprS g f c)

data ExprS g f c
  = UniS
  | BndS EVar
  | VarS EVar (f (AnnoS g f c))
  | AccS Key (AnnoS g f c)
  | AppS (AnnoS g f c) [AnnoS g f c]
  | LamS [EVar] (AnnoS g f c)
  | LstS [AnnoS g f c]
  | TupS [AnnoS g f c]
  | NamS [(Key, AnnoS g f c)]
  | RealS Scientific
  | IntS Integer
  | LogS Bool
  | StrS Text
  | CallS Source

data Three a b c = A a | B b | C c
  deriving (Ord, Eq, Show)

data None = None
  deriving (Show)

newtype One a = One { unOne :: a }
  deriving (Show)

newtype Many a = Many { unMany :: [a] }
  deriving (Show)

data ManyPoly a = MonomorphicExpr (Maybe EType) [a] | PolymorphicExpr Typeclass EVar EType [(EType, [a])]
  deriving(Show, Eq, Ord)

data Or a b = L a | R b | LR a b
  deriving(Ord, Eq, Show)


type Indexed = IndexedGeneral Int

data IndexedGeneral k a = Idx k a
  deriving (Show, Ord, Eq)

data NamType
  = NamRecord
  | NamObject
  | NamTable
  deriving(Show, Ord, Eq)

-- | A basic type
data Type
  = UnkT TVar
  -- ^ Unknown type: these may be serialized forms that do not need to be
  -- unserialized in the current environment but will later be passed to an
  -- environment where they can be deserialized. Alternatively, terms that are
  -- used within dynamic languages may need no type annotation.
  | VarT TVar
  | FunT [Type] Type
  | AppT Type [Type]
  | NamT NamType TVar [Type] [(Key, Type)]
  deriving (Show, Ord, Eq)

-- | A type with existentials and universals
data TypeU
  = VarU TVar
  | ExistU TVar
    [TypeU] -- type parameters
    [(Key, TypeU)] -- key accesses into this type
  -- ^ (a^) will be solved into one of the other types
  | ForallU TVar TypeU
  -- ^ (Forall a . A)
  | FunU [TypeU] TypeU -- function
  | AppU TypeU [TypeU] -- type application
  | NamU NamType TVar [TypeU] [(Key, TypeU)] -- record / object / table
  deriving (Show, Ord, Eq)

-- | Extended Type that may represent a language specific type as well as sets
-- of properties and constrains.
data EType =
  EType
    { etype :: TypeU
    , eprop :: Set.Set Property
    , econs :: Set.Set Constraint
    }
  deriving (Show, Eq, Ord)


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

data TypeError
  = SubtypeError TypeU TypeU Text
  | InstantiationError TypeU TypeU Text
  | EmptyCut GammaIndex
  | OccursCheckFail TypeU TypeU Text
    -- ^ the msg should an identifier for the place where the occurs check failed
  | Mismatch TypeU TypeU Text
  | UnboundVariable EVar
  | KeyError Key TypeU
  | MissingConcreteSignature EVar Lang
  | MissingGeneralSignature EVar
  | ApplicationOfNonFunction
  | TooManyArguments
  | EmptyExpression EVar
  | MissingFeature Text
  | InfiniteRecursion
  | FunctionSerialization EVar

data MorlocError
  -- | An error that is associated with an expression index
  = IndexedError Int MorlocError
  -- | Raised for calls to unimplemented features
  | NotImplemented Text
  -- | Raised for unsupported features (such as specific languages)
  | NotSupported Text
  -- | Raised by parsec on parse errors
  | SyntaxError (ParseErrorBundle Text Void)
  -- | Raised when an unsupported language is encountered
  | UnknownLanguage Text
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
  -- | Raise when a type alias substitution fails
  | SelfRecursiveTypeAlias TVar
  | MutuallyRecursiveTypeAlias [Text]
  | BadTypeAliasParameters TVar Int Int
  | ConflictingTypeAliases TypeU TypeU
  -- | Problems with the directed acyclic graph datastructures
  | DagMissingKey Text
  -- | Raised when a branch is reached that should not be possible
  | CallTheMonkeys Text
  --------------- T Y P E   E R R O R S --------------------------------------
  | ConcreteTypeError TypeError
  | GeneralTypeError TypeError
  | ToplevelRedefinition
  | IncompatibleGeneralType TypeU TypeU
  | OtherError Text -- TODO: remove this option
  -- container errors
  | EmptyTuple
  | TupleSingleton
  | EmptyRecord
  -- module errors
  | MultipleModuleDeclarations [MVar]
  | NestedModule MVar
  | NonSingularRoot [MVar]
  | ImportExportError MVar Text
  | CannotFindModule MVar
  | CyclicDependency
  | SelfImport MVar
  | BadRealization
  | TooManyRealizations
  | MissingSource
  -- type extension errors
  | UndefinedType TVar
  | AmbiguousPacker Text
  | AmbiguousUnpacker Text
  | AmbiguousCast Text Text
  | IllegalPacker TypeU
  | CyclicPacker TypeU TypeU
  | ConflictingPackers TypeU TypeU
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
  -- type synthesis errors
  | CannotSynthesizeConcreteType MVar Source TypeU [Text]




---- Fundamental class instances

instance Functor (IndexedGeneral k) where
  fmap f (Idx i x) = Idx i (f x)

instance Functor One where
  fmap f (One x) = One (f x)

instance Functor Many where
  fmap f (Many x) = Many (map f x)

instance Functor ManyPoly where
  fmap f (MonomorphicExpr t xs) = MonomorphicExpr t (map f xs)
  fmap f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t (map (second (map f)) xs)

instance Traversable One where
  traverse f (One x) = One <$> f x

instance Traversable Many where
  traverse f (Many xs) = Many <$> traverse f xs

instance Traversable ManyPoly where
  traverse f (MonomorphicExpr t xs) = MonomorphicExpr t <$> traverse f xs
  traverse f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t <$> traverse f2 xs where
    f2 (t', x) = (,) t' <$> traverse f x

instance Foldable One where
  foldr f b (One a) = f a b

instance Foldable Many where
  foldr f b (Many xs) = foldr f b xs

instance Foldable ManyPoly where
  foldr f b (MonomorphicExpr _ xs) = foldr f b xs
  foldr f b (PolymorphicExpr _ _ _ (concatMap snd -> xs)) = foldr f b xs

instance Bifunctor Or where
  bimapM f _ (L a) = L <$> f a
  bimapM _ g (R a) = R <$> g a
  bimapM f g (LR a b) = LR <$> f a <*> g b

instance Bifoldable Or where
  bilistM f _ (L a) = f a |>> return
  bilistM _ g (R b) = g b |>> return
  bilistM f g (LR a b) = do
    c1 <- f a
    c2 <- g b
    return [c1, c2]


----- Special class instances

instance FromJSON Config where
  parseJSON =
    withObject "object" $ \o ->
      Config
        <$> o .:? "home" .!= "$HOME/.morloc"
        <*> o .:? "source" .!= "$HOME/.morloc/src/morloc"
        <*> o .:? "plain" .!= "morloclib"
        <*> o .:? "tmpdir" .!= "$HOME/.morloc/tmp"
        <*> o .:? "lang_python3" .!= "python3"
        <*> o .:? "lang_R" .!= "Rscript"
        <*> o .:? "lang_perl" .!= "perl"

instance FromJSON PackageMeta where
  parseJSON = withObject "object" $ \o ->
    PackageMeta <$> o .:? "name"        .!= ""
                <*> o .:? "version"     .!= ""
                <*> o .:? "homepage"    .!= ""
                <*> o .:? "synopsis"    .!= ""
                <*> o .:? "description" .!= ""
                <*> o .:? "category"    .!= ""
                <*> o .:? "license"     .!= ""
                <*> o .:? "author"      .!= ""
                <*> o .:? "maintainer"  .!= ""
                <*> o .:? "github"      .!= ""
                <*> o .:? "bug-reports" .!= ""
                <*> o .:? "gcc-flags"   .!= ""

instance Defaultable PackageMeta where
  defaultValue = PackageMeta
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

instance Defaultable MorlocState where
  defaultValue = MorlocState {
      statePackageMeta = []
    , stateVerbosity = 0
    , stateCounter = -1
    , stateDepth = 0
    , stateSignatures = GMap Map.empty Map.empty
    , stateConcreteTypedefs = GMap Map.empty Map.empty
    , stateGeneralTypedefs = GMap Map.empty Map.empty
    , stateUniversalConcreteTypedefs = Map.empty
    , stateUniversalGeneralTypedefs = Map.empty
    , stateInnerMogrifiers = GMap Map.empty Map.empty
    , stateUniversalInnerMogrifiers = Map.empty
    , stateSources = GMap Map.empty Map.empty
    , stateAnnotations = Map.empty
    , stateOutfile = Nothing
    , stateExports = []
    , stateName = Map.empty
  }

instance Annotated IndexedGeneral where
  val (Idx _ x) = x
  ann (Idx i _) = i
  annotate i x = Idx i x

instance HasOneLanguage Source where
  langOf s = Just (srcLang s)
  langOf' s = srcLang s

instance Typelike Type where
  typeOf = id

  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(UnkT _) = t
      sub t@(VarT v)
        | v0 == v = r0
        | otherwise = t
      sub (FunT ts t) = FunT (map sub ts) (sub t)
      sub (AppT v ts) = AppT (sub v) (map sub ts)
      sub (NamT r n ps es) = NamT r n ps [(k, sub t) | (k, t) <- es]

  free (UnkT _) = Set.empty
  free v@(VarT _) = Set.singleton v
  free (FunT ts t) = Set.unions (map free (t:ts))
  free (AppT t ts) = Set.unions (map free (t:ts))
  free (NamT _ _ _ es) = Set.unions (map (free . snd) es)

  normalizeType (FunT ts1 (FunT ts2 ft)) = normalizeType $ FunT (ts1 <> ts2) ft
  normalizeType (AppT t ts) = AppT (normalizeType t) (map normalizeType ts)
  normalizeType (NamT n v ds ks) = NamT n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType t = t


instance Typelike TypeU where
  -- This functions removes qualified and existential types.
  --  * all qualified terms are replaced with UnkT
  --  * all existentials are replaced with default values if a possible
  typeOf (VarU v) = VarT v
  typeOf (ExistU _ ps rs@(_:_)) = NamT NamRecord (TV "Record") (map typeOf ps) (map (second typeOf) rs)
  typeOf (ExistU v _ _) = typeOf (ForallU v (VarU v)) -- this will cause problems eventually
  typeOf (ForallU v t) = substituteTVar v (UnkT v) (typeOf t)
  typeOf (FunU ts t) = FunT (map typeOf ts) (typeOf t)
  typeOf (AppU t ts) = AppT (typeOf t) (map typeOf ts)
  typeOf (NamU n o ps rs) = NamT n o (map typeOf ps) (zip (map fst rs) (map (typeOf . snd) rs))

  free v@(VarU _) = Set.singleton v
  free v@(ExistU _ [] rs) = Set.unions $ Set.singleton v : map (free . snd) rs
  -- Why exactly do you turn ExistU into AppU? Not judging, but it seems weird ...
  free (ExistU v ts _) = Set.unions $ Set.singleton (AppU (VarU v) ts) : map free ts
  free (ForallU v t) = Set.delete (VarU v) (free t)
  free (FunU ts t) = Set.unions $ map free (t:ts)
  free (AppU t ts) = Set.unions $ map free (t:ts)
  free (NamU _ _ ps rs) = Set.unions $ map free (map snd rs <> ps)


  substituteTVar v (ForallU q r) t =
    if Set.member (VarU q) (free t)
    then
      let q' = newVariable r t -- get unused variable name from [a, ..., z, aa, ...]
          r' = substituteTVar q (VarU q') r -- substitute the new variable into the unqualified type
      in ForallU q' (substituteTVar v r' t)
    else
      ForallU q (substituteTVar v r t)

  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(VarU v)
        | v0 == v = r0 -- replace v with the new type
        | otherwise = t
      sub (ExistU v (map sub -> ps) (map (second sub) -> rs)) = ExistU v ps rs
      sub (ForallU v t)
        | v0 == v = ForallU v t -- stop looking if we hit a bound variable of the same name
        | otherwise = ForallU v (sub t)
      sub (FunU ts t) = FunU (map sub ts) (sub t)
      sub (AppU t ts) = AppU (sub t) (map sub ts)
      sub (NamU r n ps rs) = NamU r n (map sub ps) [(k, sub t) | (k, t) <- rs]

  normalizeType (FunU ts1 (FunU ts2 ft)) = normalizeType $ FunU (ts1 <> ts2) ft
  normalizeType (AppU t ts) = AppU (normalizeType t) (map normalizeType ts)
  normalizeType (NamU n v ds ks) = NamU n v (map normalizeType ds) (zip (map fst ks) (map (normalizeType . snd) ks))
  normalizeType (ForallU v t) = ForallU v (normalizeType t)
  normalizeType (ExistU v (map normalizeType -> ps) (map (second normalizeType) -> rs)) = ExistU v ps rs
  normalizeType t = t


----- Partial order logic

-- Types are partially ordered, 'forall a . a' is lower (more generic) than
-- Int. But 'forall a . a -> a' cannot be compared to 'forall a . a', since
-- they are different kinds.
-- The order of types is used to choose the most specific serialization functions.
-- As far as serialization is concerned, properties and constraints do not matter.
instance P.PartialOrd TypeU where
  (<=) (VarU v1) (VarU v2) = v1 == v2
  (<=) (ExistU v1 ts1 rs1) (ExistU v2 ts2 rs2)
    =  v1 == v2
    && length ts1 == length ts2
    && and (zipWith (P.<=) ts1 ts2)
    && and [maybe False (t1 P.<=) (lookup k rs2) | (k, t1) <- rs1]
  (<=) (ForallU v t1) t2
    | (P.==) (ForallU v t1) t2 = True
    | otherwise = (P.<=) (substituteFirst v t1 t2) t2
  (<=) (FunU (t11:rs1) t12) (FunU (t21:rs2) t22) = t11 P.<= t21 && FunU rs1 t12 P.<= FunU rs2 t22
  (<=) (FunU [] t12) (FunU [] t22) = t12 P.<= t22
  (<=) (AppU t1 (t11:rs1)) (AppU t2 (t21:rs2)) = t11 P.<= t21 && AppU t1 rs1 P.<= AppU t2 rs2
  (<=) (AppU t1 []) (AppU t2 []) = t1 P.<= t2
  -- the records do not need to be in the same order to be equivalent
  -- ---- do I need to sort on ps1/ps2 as well?
  (<=) (NamU o1 n1 ps1 ((k1,e1):rs1)) (NamU o2 n2 ps2 es2)
    = case DL.partition ((== k1) . fst) es2 of
       ([(_,e2)], rs2) -> e1 P.<= e2 && NamU o1 n1 ps1 rs1 P.<= NamU o2 n2 ps2 rs2
       _ -> False
  (<=) (NamU o1 n1 ps1 []) (NamU o2 n2 ps2 [])
    = o1 == o2 && n1 == n2 && length ps1 == length ps2
  (<=) _ _ = False

  (==) (ForallU v1 t1) (ForallU v2 t2) =
    if Set.member (VarU v1) (free t2)
    then
      let v = newVariable t1 t2
      in (P.==) (substituteTVar v1 (VarU v) t1) (substituteTVar v2 (VarU v) t2)
    else (P.==) t1 (substituteTVar v2 (VarU v1) t2)
  (==) a b = a == b

-- Substitute all v for the first term in t2 that corresponds to v in t1. If v
-- does not occur in t1, then t1 is returned unchanged (e.g., `forall a . Int`).
substituteFirst :: TVar -> TypeU -> TypeU -> TypeU
substituteFirst v t1 t2 = case findFirst v t1 t2 of
  (Just t) -> substituteTVar v t t1
  Nothing -> t1

findFirst :: TVar -> TypeU -> TypeU -> Maybe TypeU
findFirst v = f where
  f (VarU v') t2
    | v == v' = Just t2
    | otherwise = Nothing
  f (ForallU v1 t1) (ForallU v2 t2)
    | v == v1 = Nothing
    | otherwise = f t1 (substituteTVar v2 (VarU v1) t2)
  f (ForallU v1 t1) t2
    | v == v1 = Nothing
    | otherwise = f (substituteTVar v1 (VarU v1) t1) t2
  f (FunU ts1 t1) (FunU ts2 t2)
    = foldl firstOf Nothing (zipWith f (ts1 <> [t1]) (ts2 <> [t2]))
  f (AppU t1 ts1) (AppU t2 ts2)
    = foldl firstOf Nothing (zipWith f (t1:ts1) (t2:ts2))
  f (NamU o1 n1 ps1 ((k1,e1):rs1)) (NamU o2 n2 ps2 es2)
    = case DL.partition ((== k1) . fst) es2 of
       ([(_,e2)], rs2) -> firstOf (f e1 e2) (f (NamU o1 n1 ps1 rs1) (NamU o2 n2 ps2 rs2))
       _ -> Nothing
  f _ _ = Nothing

  firstOf :: Maybe a -> Maybe a -> Maybe a
  firstOf (Just x) _ = Just x
  firstOf _ (Just x) = Just x
  firstOf _ _ = Nothing

-- | is t1 a generalization of t2?
isSubtypeOf :: TypeU -> TypeU -> Bool
isSubtypeOf t1 t2 = case P.compare t1 t2 of
  (Just x) -> x <= EQ
  _ -> False

equivalent :: TypeU -> TypeU -> Bool
equivalent t1 t2 = isSubtypeOf t1 t2 && isSubtypeOf t2 t1

-- | find all types that are not greater than any other type
mostGeneral :: [TypeU] -> [TypeU]
mostGeneral = P.minima

-- | find all types that are not less than any other type
mostSpecific :: [TypeU] -> [TypeU]
mostSpecific = P.maxima

-- | find the most specific subtypes
mostSpecificSubtypes :: TypeU -> [TypeU] -> [TypeU]
mostSpecificSubtypes t ts = mostSpecific $ filter (`isSubtypeOf` t) ts


----- Pretty instances -------------------------------------------------------

instance (Pretty a, Pretty b) => Pretty (Or a b) where
  pretty (L x) = parens ("L" <+> pretty x)
  pretty (R x) = parens ("R" <+> pretty x)
  pretty (LR x y) = parens ("LR" <+> pretty x <> "," <+> pretty y)

instance Pretty NamType where
  pretty = viaShow

instance Pretty Type where
  pretty (UnkT v) = pretty v
  pretty (VarT v) = pretty v
  pretty (FunT [] t) = "() -> " <> pretty t
  pretty (FunT ts t) = encloseSep "(" ")" " -> " (map pretty (ts <> [t]))
  pretty (AppT t ts) = hsep (map pretty (t:ts))
  pretty (NamT o n ps rs)
    = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])

instance Pretty TypeU where
  pretty (FunU [] t) = "() -> " <> prettyTypeU t
  pretty (FunU ts t) = hsep $ punctuate " ->" (map prettyTypeU (ts <> [t]))
  pretty (ForallU _ t) = pretty t
  pretty t = prettyTypeU t

prettyTypeU :: TypeU -> Doc ann
prettyTypeU (ExistU v [] []) = angles $ pretty v
prettyTypeU (ExistU v ts rs)
  = angles $ pretty v
  <+> list (map prettyTypeU ts)
  <+> list (map ((\(x,y) -> tupled [x, y]) . bimap pretty prettyTypeU) rs)
prettyTypeU (ForallU _ t) = prettyTypeU t
prettyTypeU (VarU v) = pretty v
prettyTypeU (FunU [] t) = parens $ "() -> " <> prettyTypeU t
prettyTypeU (FunU ts t) = encloseSep "(" ")" " -> " (map prettyTypeU (ts <> [t]))
prettyTypeU (AppU t ts) = hsep $ map parenTypeU (t:ts) where
    parenTypeU t'@(AppU _ _) = parens $ prettyTypeU t'
    parenTypeU t' = prettyTypeU t'
prettyTypeU (NamU o n ps rs)
    = parens
    $ block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> prettyTypeU x | (k, x) <- rs])

instance Pretty EType where
  pretty (EType t (Set.toList -> ps) (Set.toList -> cs)) = case (ps, cs) of
    ([], []) -> pretty t
    _ -> parens (psStr ps <> pretty t <> csStr cs)
    where
      psStr [] = ""
      psStr [x] = pretty x <+> "=> "
      psStr xs = tupled (map pretty xs) <+> "=> "

      csStr [] = ""
      csStr xs = " |" <+> hsep (punctuate semi (map pretty xs))

instance Pretty Property where
  pretty Pack = "pack"
  pretty Unpack = "unpack"
  pretty Cast = "cast"
  pretty (GeneralProperty ts) = hsep (map pretty ts)

instance Pretty Constraint where
  pretty (Con x) = pretty x

instance Pretty EVar where
  pretty (EV v) = pretty v

instance Pretty MVar where
  pretty = pretty . unMVar


instance Pretty TVar where
  pretty (TV v) = pretty v

instance Pretty Typeclass where
  pretty = pretty . unTypeclass

instance Pretty Key where
  pretty (Key v) = pretty v

instance Pretty CVar where
  pretty v = pretty (unCVar v)

instance Pretty Label where
  pretty (Label v) = pretty v

instance Pretty SrcName where
  pretty = pretty . unSrcName

instance Pretty Code where
  pretty = pretty . unCode

instance Pretty Source where
  pretty s
    = "source" <+> pretty (srcLang s)
    <> maybe "" (\ path -> " from" <+> dquotes (pretty path)) (srcPath s)
    <+> dquotes (pretty (srcName s))
    <+> "as" <+> pretty (srcAlias s) <> maybe "" (\t -> ":" <> pretty t) (srcLabel s)

instance Pretty Symbol where
  pretty (TypeSymbol x) = viaShow x
  pretty (TermSymbol x) = viaShow x

instance Pretty TermTypes where
  pretty (TermTypes (Just t) cs es) = "TermTypes" <+> (align . vsep $ (parens (pretty t) : map pretty cs <> map pretty es))
  pretty (TermTypes Nothing cs es) = "TermTypes" <+> "?" <> (align . vsep $ (map pretty cs <> map pretty es))

instance Pretty SignatureSet where
  pretty (Monomorphic t) = pretty t
  pretty (Polymorphic cls v t ts)
    = "class" <+> pretty cls
    <+> (align . vsep $ (pretty v <+> "::" <+> parens (pretty t)) : map pretty ts)

instance (Pretty k1, Pretty k2, Pretty v) => Pretty (GMap k1 k2 v) where
  pretty (GMap m1 m2) = "GMap" <+> (align . vsep $ [pretty (Map.toList m1), pretty (Map.toList m2)])

instance Pretty AliasedSymbol where
  pretty (AliasedType x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias
  pretty (AliasedTerm x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias

instance Pretty None where
  pretty None = "()"

instance Pretty a => Pretty (One a) where
  pretty (One x) = pretty x

instance Pretty a => Pretty (Many a) where
  pretty (Many xs) = list $ map pretty xs

instance Pretty (AnnoS g f c) where
  pretty (AnnoS _ _ e) = pretty e

instance Pretty (ExprS g f c) where
  pretty UniS = "UniS"
  pretty (BndS v) = pretty v
  pretty (VarS v _) = pretty v
  pretty (AccS k e) = parens (pretty e) <> "[" <> pretty k <> "]"
  pretty (AppS e es) = pretty e <> vsep (map pretty es)
  pretty (LamS vs e) = parens ("\\" <+> hsep (map pretty vs) <+> "->" <+> pretty e)
  pretty (LstS es) = list (map pretty es)
  pretty (TupS es) = tupled (map pretty es)
  pretty (NamS rs) = encloseSep "{" "}" "," [pretty k <+> "=" <+> pretty v | (k,v) <- rs]
  pretty (RealS x) = viaShow x
  pretty (IntS x) = pretty x
  pretty (LogS x) = pretty x
  pretty (StrS x) = pretty x
  pretty (CallS src) = pretty src

instance (Pretty k, Pretty a) => Pretty (IndexedGeneral k a) where
  pretty (Idx i x) = parens (pretty i <> ":" <+> pretty x)

instance Pretty GammaIndex where
  pretty (VarG tv) = "VarG:" <+> pretty tv
  pretty (ExistG tv [] []) = angles (pretty tv)
  pretty (ExistG tv ts rs)
    = "ExistG:"
    <+> pretty tv
    <+> list (map (parens . pretty) ts)
    <+> list (map ((\(x,y) -> tupled [x, y]) . bimap pretty prettyTypeU) rs)
  pretty (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> pretty t
  pretty (MarkG tv) = "MarkG:" <+> pretty tv
  pretty (SrcG (Source ev1 lang _ _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
  pretty (AnnG v t) = pretty v <+> "::" <+> pretty t

instance Pretty ExprI where
  pretty (ExprI i e) = parens (pretty e) <> ":" <> pretty i

instance Pretty Expr where
  pretty UniE = "()"
  pretty (ModE v es) = align . vsep $ ("module" <+> pretty v) : map pretty es
  pretty (ClsE cls vs sigs) = "class" <+> pretty cls <+> hsep (map pretty vs) <> (align . vsep . map pretty) sigs
  pretty (IstE cls ts es) = "instance" <+> pretty cls <+> hsep (map (parens . pretty) ts) <> (align . vsep . map pretty) es
  pretty (TypE lang v vs t)
    = "type" <+> pretty lang <> "@" <> pretty v
    <+> sep (map pretty vs) <+> "=" <+> pretty t
  pretty (ImpE (Import m Nothing _ _)) = "import" <+> pretty m
  pretty (ImpE (Import m (Just xs) _ _)) = "import" <+> pretty m <+> tupled (map pretty xs)
  pretty (ExpE v) = "export" <+> pretty v
  pretty (VarE s) = pretty s
  pretty (AccE k e) = parens (pretty e) <> "@" <> pretty k
  pretty (LamE v e) = "\\" <+> pretty v <+> "->" <+> pretty e
  pretty (AnnE e ts) = parens
    $   pretty e
    <+> "::"
    <+> encloseSep "(" ")" "; " (map pretty ts)
  pretty (LstE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (TupE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (AppE f es) = vsep (map pretty (f:es))
  pretty (NamE rs) = block 4 "<RECORD>" (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])
  pretty (RealE x) = pretty (show x)
  pretty (IntE x) = pretty (show x)
  pretty (StrE x) = dquotes (pretty x)
  pretty (LogE x) = pretty x
  pretty (AssE v e es) = pretty v <+> "=" <+> pretty e <+> "where" <+> (align . vsep . map pretty) es
  pretty (SrcE (Source srcname lang file' alias label))
    = "source"
    <+> viaShow lang
    <> maybe "" (\f -> "from" <+> pretty f) file'
    <+> "("
    <> dquotes (pretty srcname) <+> "as" <+>  pretty alias <> maybe "" (\s -> ":" <> pretty s) label
    <> ")"
  pretty (SigE (Signature v _ e)) =
    pretty v <+> "::" <+> eprop' <> etype' <> econs'
    where
      eprop' :: Doc ann
      eprop' =
        case Set.toList (eprop e) of
          [] -> ""
          xs -> tupled (map pretty xs) <+> "=> "
      etype' :: Doc ann
      etype' = pretty (etype e)
      econs' :: Doc ann
      econs' =
        case Set.toList (econs e) of
          [] -> ""
          xs -> " where" <+> tupled (map (\(Con x) -> pretty x) xs)

instance Pretty Signature where
  pretty (Signature v _ e) = pretty v <+> "::" <+> pretty (etype e)

instance Show MorlocError where
  show = DT.unpack . render . pretty

instance Show TypeError where
  show = DT.unpack . render . pretty

instance Pretty MorlocError where
  pretty (IndexedError i e) = "At index" <+> pretty i <> ":" <+> pretty e
  pretty (NotImplemented msg) = "Not yet implemented: " <> pretty msg
  pretty (NotSupported msg) = "NotSupported: " <> pretty msg
  pretty (UnknownLanguage lang) =
    "'" <> pretty lang <> "' is not recognized as a supported language"
  pretty (SyntaxError err') = "SyntaxError: " <> pretty (errorBundlePretty err')
  pretty (SerializationError t) = "SerializationError: " <> pretty t
  pretty (CannotLoadModule t) = "CannotLoadModule: " <> pretty t
  pretty (SystemCallError cmd loc msg) =
    "System call failed at (" <>
    pretty loc <> "):\n" <> " cmd> " <> pretty cmd <> "\n" <> " msg>\n" <> pretty msg
  pretty (PoolBuildError msg) = "PoolBuildError: " <> pretty msg
  pretty (SelfRecursiveTypeAlias v) = "SelfRecursiveTypeAlias: " <> pretty v
  pretty (MutuallyRecursiveTypeAlias vs) = "MutuallyRecursiveTypeAlias: " <> tupled (map pretty vs)
  pretty (BadTypeAliasParameters v exp' obs)
    =  "BadTypeAliasParameters: for type alias " <> pretty v
    <> " expected " <> pretty exp'
    <> " parameters but found " <> pretty obs
  pretty (ConflictingTypeAliases t1 t2)
    = "ConflictingTypeAliases:"
    <> "\n  t1:" <+> pretty t1
    <> "\n  t2:" <+> pretty t2
  pretty (CallTheMonkeys msg) =
    "There is a bug in the code, send this message to the maintainer: " <> pretty msg
  pretty (GeneratorError msg) = "GeneratorError: " <> pretty msg
  pretty (ConcreteTypeError err') = "Concrete type error: " <> pretty err'
  pretty (GeneralTypeError err') = "General type error: " <> pretty err'
  pretty ToplevelRedefinition = "ToplevelRedefinition"
  pretty (OtherError msg) = "OtherError: " <> pretty msg
  -- TODO: this will be a common class of errors and needs an informative message
  pretty (IncompatibleGeneralType a b)
    = "Incompatible general types:" <+> parens (pretty a) <+> "vs" <+> parens (pretty b)
  -- container errors
  pretty EmptyTuple = "EmptyTuple"
  pretty TupleSingleton = "TupleSingleton"
  pretty EmptyRecord = "EmptyRecord"
  -- module errors
  pretty (MultipleModuleDeclarations mv) = "MultipleModuleDeclarations: " <> tupled (map pretty mv)
  pretty (NestedModule name') = "Nested modules are currently illegal: " <> pretty name'
  pretty (NonSingularRoot ms) = "Expected exactly one root module, found" <+> list (map pretty ms)
  pretty (ImportExportError (MV m) msg) = "Error in module '" <> pretty m <> "': "  <> pretty msg
  pretty (CannotFindModule name') = "Cannot find morloc module '" <> pretty name' <> "'"
  pretty CyclicDependency = "CyclicDependency"
  pretty (SelfImport _) = "SelfImport"
  pretty BadRealization = "BadRealization"
  pretty MissingSource = "MissingSource"
  -- serialization errors
  pretty (CyclicPacker t1 t2)
    = "Error CyclicPacker - a term is described as both a packer and an unpacker:\n  "
    <> pretty t1 <> "\n  " <> pretty t2
  -- type extension errors
  pretty (ConflictingPackers t1 t2)
    = "Error ConflictingPackers:"
    <> "\n  t1:" <+> pretty t1
    <> "\n  t2:" <+> pretty t2
  pretty (UndefinedType v)
    =  "UndefinedType: could not resolve type" <+> squotes (pretty v)
    <> ". You may be missing a language-specific type definition."
  pretty (AmbiguousPacker _) = "AmbiguousPacker"
  pretty (AmbiguousUnpacker _) = "AmbiguousUnpacker"
  pretty (AmbiguousCast _ _) = "AmbiguousCast"
  pretty (IllegalPacker t) = "IllegalPacker:" <+> pretty t
  pretty (IncompatibleRealization _) = "IncompatibleRealization"
  pretty MissingAbstractType = "MissingAbstractType"
  pretty ExpectedAbstractType = "ExpectedAbstractType"
  pretty CannotInferConcretePrimitiveType = "CannotInferConcretePrimitiveType"
  pretty ToplevelStatementsHaveNoLanguage = "ToplevelStatementsHaveNoLanguage"
  pretty InconsistentWithinTypeLanguage = "InconsistentWithinTypeLanguage"
  pretty CannotInferLanguageOfEmptyRecord = "CannotInferLanguageOfEmptyRecord"
  pretty ConflictingSignatures = "ConflictingSignatures: currently a given term can have only one type per language"
  pretty CompositionsMustBeGeneral = "CompositionsMustBeGeneral"
  pretty IllegalConcreteAnnotation = "IllegalConcreteAnnotation"
  pretty (DagMissingKey msg) = "DagMissingKey: " <> pretty msg
  pretty TooManyRealizations = "TooManyRealizations"
  pretty (CannotSynthesizeConcreteType m src t [])
    = "Cannot synthesize" <+> pretty (srcLang src) <+>
      "type for" <+> squotes (pretty (srcAlias src)) <+>
      "in module" <+> pretty m <+>
      "from general type:" <+> parens (pretty t)
  pretty (CannotSynthesizeConcreteType m src t vs)
    = pretty (CannotSynthesizeConcreteType m src t []) <> "\n" <>
      "  Cannot resolve concrete types for these general types:" <+> list (map pretty vs) <> "\n" <>
      "  Are you missing type alias imports?"

instance Pretty TypeError where
  pretty (SubtypeError t1 t2 msg)
    = "SubtypeError:" <+> pretty msg <> "\n  "
    <> "(" <> pretty t1 <+> "<:" <+> pretty t2 <> ")"
  pretty (InstantiationError t1 t2 msg)
    = "InstantiationError:" <+> "(" <> pretty t1 <+> "<:=" <+> pretty t2 <> ")" <> "\n"
    <> "   " <> align (pretty msg)
  pretty (EmptyCut gi) = "EmptyCut:" <+> pretty gi
  pretty OccursCheckFail {} = "OccursCheckFail"
  pretty (Mismatch t1 t2 msg)
    = "Mismatch"
    <+> tupled ["t1=" <> pretty t1, "t2=" <> pretty t2]
    <+> pretty msg
  pretty (UnboundVariable v) = "UnboundVariable:" <+> pretty v
  pretty (KeyError k t) = "KeyError:" <+> dquotes (pretty k) <+> "not found in record:" <+> pretty t
  pretty (MissingConcreteSignature e lang) = "No concrete signature found for" <+> pretty lang <+> "function named" <+> squotes (pretty e)
  pretty (MissingGeneralSignature e) = "MissingGeneralSignature for" <+> squotes (pretty e)
  pretty ApplicationOfNonFunction = "ApplicationOfNonFunction"
  pretty TooManyArguments = "TooManyArguments"
  pretty (MissingFeature msg) = "MissingFeature: " <> pretty msg
  pretty (EmptyExpression e) = "EmptyExpression:" <+> squotes (pretty e) <+> "has no bound signature or expression"
  pretty InfiniteRecursion = "InfiniteRecursion"
  pretty (FunctionSerialization v) = "Undefined function" <+> dquotes (pretty v) <> ", did you forget an import?"




------- Helper functions

mapExprSM :: (Traversable f, Monad m) => (AnnoS g f c -> m (AnnoS g' f c')) -> ExprS g f c -> m (ExprS g' f c')
mapExprSM f (VarS v xs) = VarS v <$> traverse f xs
mapExprSM f (AccS k x) = AccS k <$> f x
mapExprSM f (AppS x xs) = AppS <$> f x <*> mapM f xs
mapExprSM f (LamS vs x) = LamS vs <$> f x
mapExprSM f (LstS xs) = LstS <$> mapM f xs
mapExprSM f (TupS xs) = TupS <$> mapM f xs
mapExprSM f (NamS rs) = NamS <$> mapM (secondM f) rs
mapExprSM _ UniS = return UniS
mapExprSM _ (BndS v) = return $ BndS v
mapExprSM _ (RealS x) = return $ RealS x
mapExprSM _ (IntS x) = return $ IntS x
mapExprSM _ (LogS x) = return $ LogS x
mapExprSM _ (StrS x) = return $ StrS x
mapExprSM _ (CallS x) = return $ CallS x

mapAnnoSM :: (Traversable f, Monad m) => (ExprS g f c -> g -> c -> m (g', c')) -> AnnoS g f c -> m (AnnoS g' f c')
mapAnnoSM fun (AnnoS g c e) = do
  e' <- mapExprSM (mapAnnoSM fun) e
  (g', c') <- fun e g c
  return (AnnoS g' c' e')

mapAnnoS :: (Traversable f) => (ExprS g f c -> g -> c -> (g', c')) -> AnnoS g f c -> AnnoS g' f c'
mapAnnoS fun = runIdentity . mapAnnoSM (\x g c -> return (fun x g c))

mapExprS :: (Traversable f) => (AnnoS g f c -> AnnoS g' f c') -> ExprS g f c -> ExprS g' f c'
mapExprS fun = runIdentity . mapExprSM (return . fun)

mapAnnoSGM :: (Traversable f, Monad m) => (g -> m g') -> AnnoS g f c -> m (AnnoS g' f c)
mapAnnoSGM f = mapAnnoSM (\_ gi ci -> (,) <$> f gi <*> pure ci)

mapAnnoSCM :: (Traversable f, Monad m) => (c -> m c') -> AnnoS g f c -> m (AnnoS g f c')
mapAnnoSCM f = mapAnnoSM (\_ gi ci -> (,) gi <$> f ci)

mapAnnoSG :: (Traversable f) => (g -> g') -> AnnoS g f c -> AnnoS g' f c
mapAnnoSG f = mapAnnoS (\_ gi ci -> (f gi, ci))

mapAnnoSC :: (Traversable f) => (c -> c') -> AnnoS g f c -> AnnoS g f c'
mapAnnoSC f = mapAnnoS (\_ gi ci -> (gi, f ci))

mapExprSGM :: (Traversable f, Monad m) => (g -> m g') -> ExprS g f c -> m (ExprS g' f c)
mapExprSGM f = mapExprSM (\(AnnoS gi ci e) -> AnnoS <$> f gi <*> pure ci <*> mapExprSGM f e)

mapExprSCM :: (Traversable f, Monad m) => (c -> m c') -> ExprS g f c -> m (ExprS g f c')
mapExprSCM f = mapExprSM (\(AnnoS gi ci e) -> AnnoS gi <$> f ci <*> mapExprSCM f e)

mapExprSG :: (Traversable f) => (g -> g') -> ExprS g f c -> ExprS g' f c
mapExprSG f = mapExprS (\(AnnoS gi ci e) -> AnnoS (f gi) ci (mapExprSG f e))

mapExprSC :: (Traversable f) => (c -> c') -> ExprS g f c -> ExprS g f c'
mapExprSC f = mapExprS (\(AnnoS gi ci e) -> AnnoS gi (f ci) (mapExprSC f e))

extractKey :: TypeU -> TVar
extractKey (VarU v) = v
extractKey (ForallU _ t) = extractKey t
extractKey (AppU t _) = extractKey t
extractKey (NamU _ v _ _) = v
extractKey (ExistU v _ _) = v
extractKey t = error $ "Cannot currently handle functional type imports: " <> show t

type2typeu :: Type -> TypeU
type2typeu (VarT v) = VarU v
type2typeu (UnkT v) = ForallU v (VarU v) -- sus
type2typeu (FunT ts t) = FunU (map type2typeu ts) (type2typeu t)
type2typeu (AppT v ts) = AppU (type2typeu v) (map type2typeu ts)
type2typeu (NamT o n ps rs) = NamU o n (map type2typeu ps) [(k, type2typeu x) | (k,x) <- rs]

unresolvedType2type :: TypeU -> Type
unresolvedType2type (VarU v) = VarT v
unresolvedType2type ExistU {} = error "Cannot cast existential type to Type"
unresolvedType2type (ForallU _ _) = error "Cannot cast universal type as Type"
unresolvedType2type (FunU ts t) = FunT (map unresolvedType2type ts) (unresolvedType2type t)
unresolvedType2type (AppU v ts) = AppT (unresolvedType2type v) (map unresolvedType2type ts)
unresolvedType2type (NamU t n ps rs) = NamT t n (map unresolvedType2type ps) [(k, unresolvedType2type e) | (k, e) <- rs]

-- | get a fresh variable name that is not used in t1 or t2, it reside in the same namespace as the first type
newVariable :: TypeU -> TypeU -> TVar
newVariable t1 t2 = findNew variables (Set.union (allVars t1) (allVars t2))
  where
    variables = [1 ..] >>= flip replicateM ['a' .. 'z']

    findNew :: [String] -> Set.Set TypeU -> TVar
    findNew [] _ = error "No variable in the infinite list was OK with you? Sheesh, picky."
    findNew (x:xs) ts
      | Set.member (VarU v) ts = findNew xs ts
      | otherwise = v
      where
        v = TV $ DT.pack x

    allVars :: TypeU -> Set.Set TypeU
    allVars (ForallU v t) = Set.union (Set.singleton (VarU v)) (allVars t)
    allVars t = free t
