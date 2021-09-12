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
  -- ** Indexed
  , Indexed(..)
  , GIndex
  -- ** Newtypes
  , CType(..)
  , ctype
  , GType(..)
  , gtype
  , EVar(..)
  , unEVar
  , TVar(..)
  , unTVar
  , MVar(..)
  , Name(..)
  , Path
  , Code(..)
  , DirTree(..)
  , AnchoredDirTree(..)
  -- ** lookup info stored
  , metaConstraints
  , metaProperties
  , metaConstructors
  , metaType
  , metaName
  , metaTypedefs
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
  , defaultPackageMeta
  -- * Types
  , NamType(..)
  , CatType(..)
  , Type(..)
  , TypeU(..)
  , EType(..)
  , unresolvedType2type
  , Source(..)
  -- * Typechecking
  , Gamma(..)
  , GammaIndex(..)
  -- * Mostly frontend expressions
  , Expr(..)
  , ExprI(..)
  , Import(..)
  -- ** Type extensions
  , Constraint(..)
  , Property(..)
  -- ** Types used in post-typechecking tree
  , SAnno(..)
  , SExpr(..)
  -- ** Typeclasses
  , HasOneLanguage(..)
  , Typelike(..)
  , Decomposable(..)
  , TypeTreeLike(..)
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
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)
import Morloc.Internal
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec ()
import System.Directory.Tree (DirTree(..), AnchoredDirTree(..))
import Morloc.Language (Lang(..))

import qualified Data.Set as Set
import qualified Data.Text as DT

-- | no annotations for now
type MDoc = Doc ()

-- | A general purpose Directed Acyclic Graph (DAG). Technically this structure
-- needn't be acyclic, but it will raise errors. You can use `findCycle` to
-- check whether a given stucture has cycles.
type DAG key edge node = Map key (node, [(key, edge)])

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

data MorlocState = MorlocState {
    statePackageMeta :: [PackageMeta]
  , stateVerbosity :: Int
  , stateCounter :: Int
  , stateSignatures :: GMap Int Int TermTypes
  , stateOutfile :: Maybe Path
  , statePackers :: PackMap
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
  , termConcrete :: [(MVar, Source, [EType], Int)]
  -- ^ The module name (MVar) is needed to lookup package metadata (if needed),
  -- the final Int type refers to the concrete index for the source.
  , termDecl :: [ExprI]
  -- ^ all declarations of this type
  --      Declaration EVar ExprI [ExprI]
  --                   ^     ^      ^----- TermType knows nothing about this
  --                   '      '--- each ExprI in [ExprI] is one of these
  --                   '--- this will match the term name
} 

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
  | ExpE EVar
  -- ^ a term that is exported from a module (should only exist at the toplevel)
  | SrcE Source
  -- ^ import "c" from "foo.c" ("f" as yolo).
  | Signature EVar (Maybe Text) EType
  -- ^ A type signature, the three parameters correspond to the term name, the
  -- optional label, and the type
  | Declaration EVar ExprI [ExprI]
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
  | NulE
  -- ^ empty leaf from a cat tree

  | LstE ExprI ExprI
  -- ^ a homogenous list
  --
  -- LstE/LstS will have types AppU, but they are structurally different
  --
  --            Type (A = AppT or AppU)   Expr (E = TupE or TupS)   0 is NulT/NulU/NulS/NulE
  --  --------------------------------------------------------------------------------------------
  -- []         A List a                  E 0 0
  -- [x]        A List a                  E x 0
  -- [x,y]      A List a                  E x (E y 0)             -- check the right against the synthesized left type 
  -- [x,y,z]    A List a                  E x (E y (E z 0))

  | TupE ExprI ExprI
  -- ^ an arbitrary tuple
  --
  -- The TupE/TupS and AppU/AppT data types should be structurally isomorphic.
  --
  --                  Type (A = AppT or AppU)                   Expr (E = TupE or TupS)   0 is NulT/NulU/NulS/NulE
  --  ------------------------------------------------------------------------------------------------------------
  --  ()              A Tuple0 0                                E 0 0
  --  (a)             A Tuple1 a                                E 0 a
  --  (a,b)           A (A Tuple2 a) b                          E (E 0 a) b
  --  (a,b,c)         A (A (A Tuple3 a) b) c                    E (E (E 0 a) b) c
  --  ((a,b),c)       A (A Tuple2 (A (A Tuple2 a) b)) c         E (E 0 (E (E 0 a) b)) c
  --  (a,(b,c))       A (A Tuple2 x) (A (A Tuple2 b) c)         E (E 0 x) (E (E 0 b) c)
  --  (a, b, (c, d))  A (A (A Tuple3 a) b) (A (A Tuple2 c) d)   E (E (E 0 a) b) (E (E 0 c) d)
  --
  -- Alternative definitions for TupE and AppU would be `TupE' [ExprI]` and
  -- `AppU [TypeU]`. While superficially simpler, these definitions complicate
  -- structural recursion.

  | RecE ExprI (Text, ExprI)
  -- ^ A record, the initial (Text ExprI) terms are a key-value pair, the final
  --
  --                            Type (A = AppT or AppU)     Expr (E = TupE or TupS)   0 is NulT/NulU/NulS/NulE
  --  ---------------------------------------------------------------------------------------------------------
  --  {}                        <undefined>                 <undefined> ? this type could appear if we allow field removal
  --  {a=x}                     RecU 0 a x                  RecE 0 a x
  --  {a=x, b=y}                RecU (RecE 0 a x) b y       RecE (RecE 0 a x) b y
  --  M {}                      <undefined>                 <undefined>
  --  M {a=x}                   RecU M a x                  RecE M a x
  --  M {a=x, b=y}              RecU (RecE M a x) b y       RecE (RecE M a x) b y
  | AppE ExprI ExprI
  -- ^ Function application
  | LamE EVar ExprI
  -- ^ (\x -> e)
  | AnnE ExprI [TypeU]
  -- ^ (e : A)
  | NumE Scientific
  -- ^ number of arbitrary size and precision
  | LogE Bool
  -- ^ boolean primitive
  | StrE Text
  deriving (Show, Ord, Eq)

data Import =
  Import
    { importModuleName :: MVar
    , importInclude :: Maybe [(EVar, EVar)]
    , importExclude :: [EVar]
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
    , unresolvedPackerCType :: TypeU
    -- ^ The decomposed (unpacked) type
    , unresolvedPackerForward :: [Source]
    -- ^ The unpack function, there may be more than one, the compiler will make
    -- a half-hearted effort to find the best one. It is called "Forward" since
    -- it is moves one step towards serialization.
    , unresolvedPackerReverse :: [Source]
    }
  deriving (Show, Ord, Eq)

type PackMap = Map (TVar, Int) [UnresolvedPacker]


-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG EVar TypeU
  -- ^ store a bound variable
  | ExistG TVar
    [TypeU] -- type parameters
    [TypeU] -- type defaults
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
  | NotYetImplemented TypeU TypeU Text 
  | UnboundVariable EVar
  | KeyError Text TypeU
  | MissingConcreteSignature Source
  | ApplicationOfNonFunction
  deriving (Ord, Eq, Show)

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

data EVar = EV Text deriving (Show, Eq, Ord)

data TVar = TV (Maybe Lang) Text deriving(Show, Eq, Ord)

-- | Let the TVar type behave like the MVar newtype
unTVar :: TVar -> Text
unTVar (TV _ t) = t

unEVar :: EVar -> Text
unEVar (EV e) = e

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
data One a = One a
data Many a = Many [a]

instance Functor One where
  fmap f (One x) = One (f x)

data SExpr g f c
  = UniS
  | VarS EVar
  | AccS (SAnno g f c) Text
  | AppS (SAnno g f c) (SAnno g f c)
  | LamS EVar (SAnno g f c)
  -- containers
  | NulS -- don't confuse with UniS (unit), NulS is the container terminator
  | LstS (SAnno g f c) (SAnno g f c)
  | TupS (SAnno g f c) (SAnno g f c)
  | RecS (SAnno g f c) Text (SAnno g f c)
  -- primitives
  | NumS Scientific
  | LogS Bool
  | StrS Text
  | CallS Source

data Indexed a = Idx Int a

-- TODO: This should probably be a newtype, I want to avoid ambiguous Int's in signatures
type GIndex = Int

instance Functor Indexed where
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

data NamType = NamRecord | NamObject | NamTable
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
  | NulT
  -- ^ used for empty leafs in cat trees
  | FunT Type Type -- right associative function
  | AppT Type Type -- left associative type application
  | RecT NamType Type (TVar, Type)  -- left associative named type application
  deriving (Show, Ord, Eq)

-- | A type with existentials and universals
data TypeU
  = VarU TVar
  -- ^ (a)
  | ExistU TVar
    [TypeU] -- type parameters
    [TypeU] -- default types
  -- ^ (a^) will be solved into one of the other types
  | ForallU TVar TypeU
  -- ^ (Forall a . A)
  | NulU
  -- ^ empty type, mostly used for empty sets in containers, for example, the end of a tuple
  | FunU TypeU TypeU -- function
  | AppU TypeU TypeU -- type application
  | RecU NamType -- record / object / table
         TypeU -- left associated type
         (TVar, TypeU) -- one key/value pair
  deriving (Show, Ord, Eq)

class TypeTreeLike a where
  rec :: TVar -> [(Text, a)] -> a
  arr :: TVar -> [a] -> a

instance TypeTreeLike TypeU where
  arr v [] = CatU CatTypeArr (VarU v) NulU
  arr v (t:ts) = CatU CatTypeArr t (arr v ts)

  rec v [] = CatU (CatTypeRec NamRecord []) (VarU v) NulU
  rec v@(TV lang _) ((k, t):rs)
    = CatU (CatTypeRec NamRecord [])
           (CatU CatTypeEnt (VarU (TV lang k)) t)
           (rec v rs)


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
unresolvedType2type NulU = NulT
unresolvedType2type (ExistU _ _ _) = error "Cannot cast existential type to Type"
unresolvedType2type (ForallU _ _) = error "Cannot cast universal type as Type"
unresolvedType2type (VarU v) = VarT v
unresolvedType2type (CatU k t1 t2) = CatT k (unresolvedType2type t1) (unresolvedType2type t2)


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

class Decomposable a where
  -- | Break a type into its input arguments, and final output
  -- For example: decompose ((a -> b) -> [a] -> [b]) would 
  -- yield ([(a->b), [a]], [b])
  decompose :: a -> ([a], a)

  -- | like @decompose@ but concatentates the output type
  decomposeFull :: a -> [a]
  decomposeFull t = case decompose t of
    (xs, x) -> (xs ++ [x])

instance Decomposable Type where
  decompose (CatT _ t1 NulT) = ([], t1)
  decompose (CatT _ t1 t2) = case decompose t2 of
    (ts, finalType) -> (t1:ts, finalType) 
  decompose t = ([], t)

instance Decomposable TypeU where
  decompose (CatU _ t1 NulU) = ([], t1)
  decompose (CatU _ t1 t2) = case decompose t2 of
    (ts, finalType) -> (t1:ts, finalType) 
  decompose t = ([], t)

instance Decomposable Expr where
  decompose (CatE _ (ExprI _ e) (ExprI _ UniE)) = ([], e)
  decompose (CatE _ (ExprI _ e1) (ExprI _ e2)) = case decompose e2 of
    (es, finalExpr) -> (e1:es, finalExpr)
  decompose e = ([], e)

class Typelike a where
  typeOf :: a -> Type

  free :: a -> Set.Set a

  -- | substitute all appearances of a given variable with a given new type
  substituteTVar :: TVar -> a -> a -> a

  nargs :: a -> Int
  nargs t = case typeOf t of
    (CatT CatTypeFun _ t') -> 1 + nargs t'
    _ -> 0
  

instance Typelike Type where
  typeOf = id

  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(VarT v)
        | v0 == v = r0
        | otherwise = t
      sub (CatT k t1 t2) = CatT k (sub t1) (sub t2)
      sub t = t

  free v@(VarT _) = Set.singleton v
  free (CatT _ t1 t2) = Set.union (free t1) (free t2)
  free _ = Set.empty


instance Typelike TypeU where
  -- This functions removes qualified and existential types.
  --  * all qualified terms are replaced with UnkT
  --  * all existentials are replaced with default values if a possible
  --    FIXME: should I really just take the first in the list???
  typeOf NulU = NulT
  typeOf (VarU v) = VarT v
  typeOf (CatU k t1 t2) = CatT k (typeOf t1) (typeOf t2)
  typeOf (ExistU v _ []) = typeOf (ForallU v (VarU v)) -- whatever
  typeOf (ExistU _ _ (t:_)) = typeOf t
  typeOf (ForallU v t) = substituteTVar v (UnkT v) (typeOf t)

  free v@(VarU _) = Set.singleton v
  free v@(ExistU _ [] _) = Set.singleton v
  free (ExistU v ts _) = Set.unions $ Set.singleton (makeArru v ts) : map free ts
    where
      makeArru :: TVar -> [TypeU] -> TypeU
      makeArru v [] = CatU CatTypeArr (VarU v) NulU
      makeArru v (t:ts) = CatU CatTypeArr t (makeArru v ts)
  free (ForallU v t) = Set.delete (VarU v) (free t)
  free (CatU _ t1 t2) = Set.union (free t1) (free t2)
  free _ = Set.empty
  
  substituteTVar v0 r0 t0 = sub t0
    where
      sub t@(VarU v)
        | v0 == v = r0
        | otherwise = t
      sub (CatU k t1 t2) = CatU k (sub t1) (sub t2)
      sub t = t

  substituteTVar v@(TV _ _) (ForallU q r) t = 
    if Set.member (VarU q) (free t)
    then
      let q' = newVariable r t -- get unused variable name from [a, ..., z, aa, ...]
          r' = substituteTVar q (VarU q') r -- substitute the new variable into the unqualified type
      in ForallU q' (substituteTVar v r' t)
    else
      ForallU q (substituteTVar v r t)
  substituteTVar v r t = sub t
    where
      sub :: TypeU -> TypeU
      sub t'@(VarU v')
        | v == v' = r
        | otherwise = t'
      sub (CatU k t1 t2) = CatU k (sub t1) (sub t2)
      sub (ExistU v' ps ds) = ExistU v' (map sub ps) (map sub ds)


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
  langOf NulT = error "NulT has no language and should be left alone"
  langOf (UnkT (TV lang _)) = lang
  langOf (VarT (TV lang _)) = lang
  langOf t@(CatT _ t1 t2)
    | langOf t1 == langOf t2 = langOf t1
    | otherwise = error $ "inconsistent languages in" <> show t

instance HasOneLanguage TVar where
  langOf (TV lang _) = lang

instance HasOneLanguage TypeU where
  langOf NulU = error "NulU has no language and should be left alone"
  langOf (VarU (TV lang _)) = lang
  langOf x@(ExistU (TV lang _) ts _)
    | all ((==) lang) (map langOf ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x
  langOf x@(ForallU (TV lang _) t)
    | lang == langOf t = lang
    | otherwise = error $ "inconsistent languages in " <> show x
  langOf x@(CatU _ t1 t2)
    | langOf t1 == langOf t2 = langOf t1
    | otherwise = error $ "inconsistent languages in" <> show x


metaConstraints :: Int -> MorlocMonad [Constraint]
metaConstraints = undefined

metaProperties :: Int -> MorlocMonad [Property]
metaProperties = undefined

metaConstructors :: Int -> MorlocMonad [Source]
metaConstructors = undefined

metaType :: Int -> MorlocMonad (Maybe Type)
metaType = undefined

metaName :: Int -> MorlocMonad (Maybe EVar)
metaName = undefined

metaTypedefs :: Int -> MorlocMonad (Map TVar (Type, [TVar]))
metaTypedefs = undefined
