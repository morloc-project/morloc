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
  , GMap(..)
  , GMapRet(..)
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
  , TermTypes(..)
  , MorlocReturn
  -- ** Package metadata
  , PackageMeta(..)
  , defaultPackageMeta
  -- * Types
  , Type(..)
  , UnresolvedType(..)
  , EType(..)
  , unresolvedType2type
  , Source(..)
  -- ** Type extensions
  , Constraint(..)
  , Property(..)
  -- ** Types used in post-typechecking tree
  , SAnno(..)
  , SExpr(..)
  , GMeta(..)
  , GU
  , GR
  -- ** Typeclasses
  , HasOneLanguage(..)
  , Typelike(..)
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
        `---------.     ---------------
                   \    f.1 f.2 f.3 f.5
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

data TermTypes = TermTypes {
    termGeneral :: Maybe EType
  -- ^ A term may have many general types (up to one in each scope)
  , termConcrete :: [(MVar, Source, [EType])]
  -- ^ The module name (MVar) is needed to lookup package metadata (if needed)
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
  | IncompatibleGeneralType UnresolvedType UnresolvedType
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
data GMeta t = GMeta {
    metaId :: Int
  , metaGType :: t
  -- ^ General type. Before type inference the general type is unresolved. In
  -- the realization phase it is usually (always?) known.
  , metaName :: Maybe EVar -- the name, if relevant
  , metaProperties :: Set Property
  , metaConstraints :: Set Constraint
  , metaPackers :: Map (TVar, Int) [UnresolvedPacker]
  -- ^ The (un)packers available in this node's module scope. FIXME: kludge
  -- In (TVar, Int), TVar is that type to be (de)serialied and Int is the number of arguments the packer takes
  , metaConstructors :: Map TVar Source
  -- ^ The constructors in this node's module scope. FIXME: kludge
  , metaTypedefs :: Map TVar (Type, [TVar])
  -- ^ Everything needed to make the prototypes and serialization generic
  -- functions in C++. FIXME: kludge
} deriving (Show, Ord, Eq)

-- Metadata for a general type before it has been resolved
type GU = GMeta Int

-- General resolved metadata and type - used after type inference
type GR = GMeta (Maybe GType)

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

-- | A basic type
data Type
  = UnkT TVar
  -- ^ Unknown type: these may be serialized forms that do not need to be
  -- unserialized in the current environment but will later be passed to an
  -- environment where they can be deserialized. Alternatively, terms that are
  -- used within dynamic languages may need no type annotation.
  | VarT TVar
  -- ^ (a)
  | FunT Type Type
  -- ^ (A->B)  -- positional parameterized types
  | ArrT TVar [Type]
  -- ^ f [Type]  -- keyword parameterized types
  | NamT NamType TVar [Type] [(Text, Type)] 
  -- ^ Foo { bar :: A, baz :: B }
  deriving (Show, Ord, Eq)

-- | A type with existentials and universals
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

-- | Extended Type that may represent a language specific type as well as sets
-- of properties and constrains.
data EType =
  EType
    { etype :: UnresolvedType
    , eprop :: Set Property
    , econs :: Set Constraint
    }
  deriving (Show, Eq, Ord)

instance HasOneLanguage EType where
  langOf e = langOf (etype e) 

instance HasOneLanguage Source where
  langOf s = Just (srcLang s)
  langOf' s = srcLang s

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
