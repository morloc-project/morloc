{-# LANGUAGE ViewPatterns, OverloadedStrings, TypeFamilies #-}

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
  -- ** re-exports
    module Morloc.Internal
  -- ** Synonyms
  , MDoc
  , DAG
  -- ** Other functors
  , None(..)
  , One(..)
  , Many(..)
  -- ** Other classes
  , Defaultable(..)
  -- ** Indexed
  , IndexedGeneral(..)
  , Indexed
  , GIndex
  -- ** Newtypes
  , CType(..)
  , ctype
  , GType(..)
  , gtype
  , EVar(..)
  , TVar(..)
  , unTVar
  , MVar(..)
  , Name(..)
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
  -- ** Serialization
  , UnresolvedPacker(..)
  , PackMap
  --------------------
  -- ** Error handling
  , MorlocError(..)
  , TypeError(..)
  -- ** Configuration
  , Config(..)
  -- ** Morloc monad
  , MorlocMonad
  , MorlocState(..)
  , TermTypes(..)
  , MorlocReturn
  -- ** Package metadata
  , PackageMeta(..)
  -- * Types
  , NamType(..)
  , Type(..)
  , TypeU(..)
  , type2typeu
  , EType(..)
  , unresolvedType2type
  , Source(..)
  -- * Typechecking
  , Gamma(..)
  , GammaIndex(..)
  -- * Mostly frontend expressions
  , Symbol(..)
  , symbolName
  , AliasedSymbol(..)
  , Expr(..)
  , ExprI(..)
  , Import(..)
  , Exports(..)
  -- ** Type extensions
  , Constraint(..)
  , Property(..)
  -- ** Types used in post-typechecking tree
  , SAnno(..)
  , SExpr(..)
  , mapSAnno
  , mapSExpr
  -- ** Typeclasses
  , HasOneLanguage(..)
  , Typelike(..)
  -- , Decomposable(..)
  -- ** kludge
  , newVariable
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Text (Text)
import Prettyprinter (Doc)
import Data.Void (Void)
import Morloc.Internal
import Text.Megaparsec (ParseErrorBundle)
import System.Directory.Tree (DirTree(..), AnchoredDirTree(..))
import Morloc.Language (Lang(..))

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as DT

-- | no annotations for now
type MDoc = Doc ()

-- | A general purpose Directed Acyclic Graph (DAG). Technically this structure
-- needn't be acyclic. You can use `findCycle` to check whether a given stucture
-- has cycles.
type DAG key edge node = Map key (node, [(key, edge)])

data GMap a b c = GMap (Map a b) (Map b c)
  deriving(Show, Ord, Eq)

data GMapRet c
  = GMapNoFst -- ^ Failure on the first key
  | GMapNoSnd -- ^ Failure on the internal key (possible bug)
  | GMapJust c
  deriving(Show, Ord, Eq)

class Defaultable a where
  defaultValue :: a

type MorlocMonadGen c e l s a
   = ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)

data MorlocState = MorlocState
  { statePackageMeta :: [PackageMeta]
  -- ^ The parsed contents of a package.yaml file
  , stateVerbosity :: Int
  , stateCounter :: Int
  -- ^ Used in Treeify generate new indices (starting from max parser index).
  -- Also used (after resetting to 0) in each of the backend generators.
  , stateDepth :: Int
  -- ^ store depth in the SAnno tree in the frontend and backend typecheckers
  , stateSignatures :: GMap Int Int TermTypes
  , stateTypedefs :: GMap Int MVar (Map TVar [([TVar], TypeU)])
  -- ^ Stores all type definitions available to an index e.g.:
  --   `type Cpp (Map k v) = "std::map<$1,$2>" k v`
  --   Where `TVar` is `Map`
  --         `Type` is `"std::map<$1,$2>" k v`
  --         `[TVar]` is `[k,v]`
  , stateSources :: GMap Int MVar [Source]
  , stateAnnotations :: Map Int [TypeU]
  -- ^ Stores non-top-level annotations.
  , stateOutfile :: Maybe Path
  -- ^ The nexus filename ("nexus.pl" by default, though I should ditch perl)
  , statePackers :: GMap Int MVar PackMap
  , stateExports :: [Int]
  -- ^ The indices of each exported term
  , stateName :: Map Int EVar
  -- ^ store the names of morloc compositions
  }

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
  , termConcrete :: [(MVar, [EType], Maybe (Indexed Source))]
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
data Symbol = TypeSymbol DT.Text | TermSymbol DT.Text
  deriving (Show, Ord, Eq)

symbolName (TypeSymbol x) = x
symbolName (TermSymbol x) = x

data Exports = ExportMany (Set.Set Symbol) | ExportAll
  deriving (Show, Ord, Eq)

data AliasedSymbol = AliasedTerm DT.Text DT.Text | AliasedType DT.Text DT.Text
  deriving (Show, Ord, Eq)

data ExprI = ExprI Int Expr
  deriving (Show, Ord, Eq)

-- | Terms, see Dunfield Figure 1
data Expr
  = ModE MVar [ExprI]
  -- ^ the toplevel expression in a module
  | TypE TVar [TVar] TypeU
  -- ^ a type definition
  --   1. type name
  --   2. parameters
  --   3. type
  | ImpE Import
  -- ^ a morloc module import
  | ExpE Symbol
  -- ^ a term that is exported from a module (should only exist at the toplevel)
  | SrcE Source
  -- ^ import "c" from "foo.c" ("f" as yolo).
  | SigE EVar (Maybe Text) EType
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
  | AccE ExprI Text
  -- ^ person@age - access a field in a record
  | LstE [ExprI]
  | TupE [ExprI]
  | NamE [(Text, ExprI)]
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
    , unresolvedPackedType :: TypeU
    -- ^ The packed type (e.g., Map key val)
    , unresolvedUnpackedType :: TypeU
    -- ^ The decomposed (unpacked) type (e.g., [(key,val)])
    , unresolvedPackerForward :: [Source]
    -- ^ The unpack function, there may be more than one, the compiler will
    -- try to find the best one. It is called "Forward" since it moves one
    -- step towards serialization.
    , unresolvedPackerReverse :: [Source]
    , unresolvedPackerGeneralTypes :: Maybe (TypeU, TypeU)
    -- ^ The general packed and unpacked types, if available
    }
  deriving (Show, Ord, Eq)

type PackMap = Map TVar [UnresolvedPacker]


-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG EVar TypeU
  -- ^ store a bound variable
  | ExistG TVar
    [TypeU] -- type parameters
    [TypeU] -- type defaults
    [(Text, TypeU)] -- keys
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar TypeU
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | SrcG Source
  -- ^ source
  | SerialConstraint TypeU TypeU
  -- ^ Store an unsolved serialization constraint containing one or more
  -- existential variables. When the existential variables are solved, the
  -- constraint will be written into the Stack state.
  deriving (Ord, Eq, Show)


data Gamma = Gamma
  { gammaCounter :: Int
  , gammaContext :: [GammaIndex]
  }


data TypeError
  = SubtypeError TypeU TypeU Text
  | InstantiationError TypeU TypeU Text
  | EmptyCut GammaIndex
  | OccursCheckFail TypeU TypeU Text
    -- ^ the msg should an identifier for the place where the occurs check failed
  | Mismatch TypeU TypeU Text
  | UnboundVariable EVar
  | KeyError Text TypeU
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
  | MutuallyRecursiveTypeAlias [TVar]
  | BadTypeAliasParameters TVar Int Int 
  | ConflictingTypeAliases Type Type
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
  -- serialization errors
  | MissingPacker Text CType
  | MissingUnpacker Text CType
  -- type extension errors
  | AmbiguousPacker TVar
  | AmbiguousUnpacker TVar
  | AmbiguousCast TVar TVar
  | IllegalPacker TypeU
  | CyclicPacker TypeU
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

newtype MVar = MV { unMVar :: Text } deriving (Show, Eq, Ord)

newtype EVar = EV { unEVar :: Text } deriving (Show, Eq, Ord)

data TVar = TV (Maybe Lang) Text deriving(Show, Eq, Ord)

-- | Let the TVar type behave like the MVar newtype
unTVar :: TVar -> Text
unTVar (TV _ t) = t

data Source =
  Source
    { srcName :: Name
      -- ^ the name of the function in the source language
    , srcLang :: Lang
    , srcPath :: Maybe Path
    , srcAlias :: EVar
      -- ^ the morloc alias for the function (if no alias is explicitly given,
      -- this will be equal to the name
    , srcLabel :: Maybe Text
      -- ^ an additional label for distinguishing this term from its synonyms
    }
  deriving (Ord, Eq, Show)

-- g: an annotation for the group of child trees (what they have in common)
-- f: a collection - before realization this will be Many
--                 - after realization it will be One
-- c: an annotation for the specific child tree
data SAnno g f c = SAnno (f (SExpr g f c, c)) g

data None = None
  deriving (Show)

newtype One a = One { unOne :: a }
  deriving (Show)

newtype Many a = Many { unMany :: [a] }
  deriving (Show)


instance Functor One where
  fmap f (One x) = One (f x)

instance Functor Many where
  fmap f (Many x) = Many (map f x)

instance Foldable One where
  foldr f b (One a) = f a b

instance Foldable Many where
  foldr f b (Many xs) = foldr f b xs


data SExpr g f c
  = UniS
  | VarS EVar
  | AccS (SAnno g f c) Text
  | AppS (SAnno g f c) [SAnno g f c]
  | LamS [EVar] (SAnno g f c)
  -- containers
  | LstS [SAnno g f c]
  | TupS [SAnno g f c]
  | NamS [(Text, SAnno g f c)]
  -- primitives
  | RealS Scientific
  | IntS Integer
  | LogS Bool
  | StrS Text
  | CallS Source

mapSAnno :: Functor f => (g -> g') -> (c -> c') -> SAnno g f c -> SAnno g' f c'
mapSAnno fg fc (SAnno e g) = SAnno (fmap (mapSExpr' fg fc) e) (fg g) where
  mapSExpr' :: Functor f => (g -> g') -> (c -> c') -> (SExpr g f c, c) -> (SExpr g' f c', c')
  mapSExpr' fg' fc' (e', c) = (mapSExpr fg' fc' e', fc' c)

mapSExpr :: Functor f => (g -> g') -> (c -> c') -> SExpr g f c -> SExpr g' f c'
mapSExpr fg fc = fe where 
  m = mapSAnno fg fc
  fe UniS = UniS
  fe (VarS v) = VarS v
  fe (AccS x k) = AccS (m x) k
  fe (AppS x xs) = AppS (m x) (map m xs)
  fe (LamS vs x) = LamS vs (m x)
  fe (LstS xs) = LstS (map m xs)
  fe (TupS xs) = TupS (map m xs)
  fe (NamS rs) = NamS (zip (map fst rs) (map (m . snd) rs))
  fe (RealS x) = RealS x
  fe (IntS x) = IntS x
  fe (LogS x) = LogS x
  fe (StrS x) = StrS x
  fe (CallS src) = CallS src
   

type Indexed = IndexedGeneral Int

data IndexedGeneral k a = Idx k a
  deriving (Show, Ord, Eq)

instance Annotated IndexedGeneral where
  val (Idx _ x) = x
  ann (Idx i _) = i
  annotate i x = Idx i x

-- TODO: This should probably be a newtype, I want to avoid ambiguous Int's in signatures
type GIndex = Int

instance Functor (IndexedGeneral k) where
  fmap f (Idx i x) = Idx i (f x)

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
gtype :: Type -> GType
gtype t
  | isNothing (langOf t) = GType t
  | otherwise = error "COMPILER BUG - incorrect assignment to general type"

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
  -- ^ (a)
  | FunT [Type] Type
  | AppT Type [Type]
  | NamT NamType TVar [Type] [(Text, Type)]
  deriving (Show, Ord, Eq)

-- | A type with existentials and universals
data TypeU
  = VarU TVar
  -- ^ (a)
  | ExistU TVar
    [TypeU] -- type parameters
    [TypeU] -- default types
    [(Text, TypeU)] -- key accesses into this type
  -- ^ (a^) will be solved into one of the other types
  | ForallU TVar TypeU
  -- ^ (Forall a . A)
  | FunU [TypeU] TypeU -- function
  | AppU TypeU [TypeU] -- type application
  | NamU NamType TVar [TypeU] [(Text, TypeU)] -- record / object / table
  deriving (Show, Ord, Eq)

type2typeu :: Type -> TypeU
type2typeu (VarT v) = VarU v
type2typeu (UnkT v) = ForallU v (VarU v) -- sus
type2typeu (FunT ts t) = FunU (map type2typeu ts) (type2typeu t)
type2typeu (AppT v ts) = AppU (type2typeu v) (map type2typeu ts)
type2typeu (NamT o n ps rs) = NamU o n (map type2typeu ps) [(k, type2typeu x) | (k,x) <- rs]

-- | Extended Type that may represent a language specific type as well as sets
-- of properties and constrains.
data EType =
  EType
    { etype :: TypeU
    , eprop :: Set.Set Property
    , econs :: Set.Set Constraint
    }
  deriving (Show, Eq, Ord)

instance HasOneLanguage EType where
  langOf e = langOf (etype e) 

instance HasOneLanguage Source where
  langOf s = Just (srcLang s)
  langOf' s = srcLang s

unresolvedType2type :: TypeU -> Type 
unresolvedType2type (VarU v) = VarT v
unresolvedType2type ExistU {} = error "Cannot cast existential type to Type"
unresolvedType2type (ForallU _ _) = error "Cannot cast universal type as Type"
unresolvedType2type (FunU ts t) = FunT (map unresolvedType2type ts) (unresolvedType2type t)
unresolvedType2type (AppU v ts) = AppT (unresolvedType2type v) (map unresolvedType2type ts)
unresolvedType2type (NamU t n ps rs) = NamT t n (map unresolvedType2type ps) [(k, unresolvedType2type e) | (k, e) <- rs]


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
  --    FIXME: should I really just take the first in the list???
  typeOf (VarU v) = VarT v
  typeOf (ExistU v _ [] _) = typeOf (ForallU v (VarU v)) -- whatever
  typeOf (ExistU _ _ (t:_) _) = typeOf t
  typeOf (ForallU v t) = substituteTVar v (UnkT v) (typeOf t)
  typeOf (FunU ts t) = FunT (map typeOf ts) (typeOf t)
  typeOf (AppU t ts) = AppT (typeOf t) (map typeOf ts)
  typeOf (NamU n o ps rs) = NamT n o (map typeOf ps) (zip (map fst rs) (map (typeOf . snd) rs))

  free v@(VarU _) = Set.singleton v
  free v@(ExistU _ [] _ rs) = Set.unions $ Set.singleton v : map (free . snd) rs
  -- Why exactly do you turn ExistU into AppU? Not judging, but it seems weird ...
  free (ExistU v ts _ _) = Set.unions $ Set.singleton (AppU (VarU v) ts) : map free ts
  free (ForallU v t) = Set.delete (VarU v) (free t)
  free (FunU ts t) = Set.unions $ map free (t:ts)
  free (AppU t ts) = Set.unions $ map free (t:ts)
  free (NamU _ _ ps rs) = Set.unions $ map free (map snd rs <> ps)
  

  substituteTVar v@(TV _ _) (ForallU q r) t = 
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
      sub (ExistU v (map sub -> ps) (map sub -> ts) (map (second sub) -> rs)) = ExistU v ps ts rs
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
  normalizeType (ExistU v (map normalizeType -> ps) (map normalizeType -> ds) (map (second normalizeType) -> rs)) = ExistU v ps ds rs
  normalizeType t = t

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
        v = TV (langOf t1) (DT.pack x)

    allVars :: TypeU -> Set.Set TypeU
    allVars (ForallU v t) = Set.union (Set.singleton (VarU v)) (allVars t)
    allVars t = free t


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
  langOf (FunT _ t) = langOf t
  langOf (AppT t _) = langOf t
  langOf (NamT _ (TV lang _) _ _) = lang

instance HasOneLanguage TVar where
  langOf (TV lang _) = lang

instance HasOneLanguage TypeU where
  langOf (VarU (TV lang _)) = lang
  langOf (ExistU (TV lang _) _ _ _) = lang
  langOf (ForallU (TV lang _) _) = lang
  langOf (FunU _ t) = langOf t
  langOf (AppU t _) = langOf t
  langOf (NamU _ (TV lang _) _ _) = lang
