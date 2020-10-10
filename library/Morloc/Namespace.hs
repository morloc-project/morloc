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
  -- ** Newtypes
  , CType(..)
  , ctype
  , GType(..)
  , generalType
  , DefaultType(..)
  , EVar(..)
  , MVar(..)
  , Name(..)
  , Path(..)
  , Code(..)
  -- ** Language
  , Lang(..)
  -- ** Data
  , Script(..)
  --------------------
  , MData(..)
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
  -- * Typechecking
  , Expr(..)
  , Gamma
  , GammaIndex(..)
  , Import(..)
  , Source(..)
  , Indexable(..)
  , Stack
  , StackState(..)
  , TVar(..)
  , Type(..)
  , JsonType(..)
  -- ** State manipulation
  , StackConfig(..)
  -- ** ModuleGamma paraphernalia
  , ModularGamma
  -- ** Type extensions
  , Constraint(..)
  , EType(..)
  , Property(..)
  , TypeSet(..)
  , Typelike(..)
  , langOf
  -- ** Types used in post-typechecking tree
  , SAnno(..)
  , SExpr(..)
  , GMeta(..)
  , One(..)
  , Many(..)
  -- ** DAG and associated types
  , DAG
  , ParserNode(..)
  , ParserDag
  , PreparedNode(..)
  , PreparedDag
  , TypedNode(..)
  , TypedDag
  -- ** Types used in final translations
  , TypeM(..)
  , ExprM(..)
  , Argument(..)
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

-- | The values are left unparsed, since they will be used as text
data MData
  = Num' Text
  | Str' Text
  | Log' Bool -- booleans are parsed, since representation depend on language
  | Lst' [MData]
  | Rec' [(Text, MData)]
  | Tup' [MData]
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
  -- | Error in building a pool (i.e., in a compiled language)
  | PoolBuildError Script Text
  -- | Raise error if inappropriate function is called on unrealized manifold
  | NoBenefits
  -- | Raise when a type alias substitution fails
  | SelfRecursiveTypeAlias TVar
  | MutuallyRecursiveTypeAlias [TVar]
  | BadTypeAliasParameters TVar Int Int 
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
  | CannotImportMain
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

type Gamma = [GammaIndex]

newtype EVar = EVar { unEVar :: Text } deriving (Show, Eq, Ord)
newtype MVar = MVar { unMVar :: Text } deriving (Show, Eq, Ord)

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
  | ExistG TVar [Type] [DefaultType]
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar Type
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | MarkEG EVar
  -- ^ ...
  | SrcG Source
  -- ^ source
  | UnsolvedConstraint Type Type
  -- ^ Store an unsolved serialization constraint containing one or more
  -- existential variables. When the existential variables are solved, the
  -- constraint will be written into the Stack state.
  deriving (Ord, Eq, Show)

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

data Import =
  Import
    { importModuleName :: MVar
    , importInclude :: Maybe [(EVar, EVar)]
    , importExclude :: [EVar]
    , importNamespace :: Maybe EVar -- currently not used
    }
  deriving (Ord, Eq, Show)

-- g: an annotation for the group of child trees (what they have in common)
-- f: a collection - before realization this will probably be Set
--                 - after realization it will be One
-- c: an annotation for the specific child tree
data SAnno g f c = SAnno (f (SExpr g f c, c)) g

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

-- -- | Replace Type with SimpleType after typechecking (where all types should
-- -- be resolved and all universal and existential types removed)
-- data SimpleType
--   = VarSimple TVar
--   -- ^ (a)
--   | FunSimple Type SimpleType
--   -- ^ (A->B)
--   | ArrSimple TVar [SimpleType] -- positional parameterized types
--   -- ^ f [Type]
--   | NamSimple TVar [(Text, SimpleType)] -- keyword parameterized types
--   -- ^ Foo { bar :: A, baz :: B }
--   deriving (Show, Ord, Eq)


-- | A general purpose Directed Acyclic Graph (DAG)
type DAG key edge node = Map key (node, [(key, edge)])

-- | The type returned from the Parser. It contains all the information in a
-- single module but knows NOTHING about other modules.
data ParserNode = ParserNode  {
    parserNodePath :: Maybe Path
  , parserNodeBody :: [Expr]
  , parserNodeSourceMap :: Map (EVar, Lang) Source
  , parserNodeTypedefs :: Map TVar (Type, [TVar])
  , parserNodeExports :: Set EVar
} deriving (Show, Ord, Eq)
type ParserDag = DAG MVar Import ParserNode

-- | Node description after desugaring (substitute type aliases and resolve
-- imports/exports)
data PreparedNode = PreparedNode {
    preparedNodePath :: Maybe Path
  , preparedNodeBody :: [Expr]
  , preparedNodeSourceMap :: Map (EVar, Lang) Source
} deriving (Show, Ord, Eq)
type PreparedDag = DAG MVar [(EVar, EVar)] ParserNode

-- | Node description after type checking. This will later be fed into
-- `treeify` to make the SAnno objects that will be passed to Generator.
data TypedNode = TypedNode {
    typedNodePath :: Maybe Path
  , typedNodeBody :: Map EVar Expr
  , typedNodeTypeMap :: Map EVar TypeSet
  , typedNodeSourceMap :: Map (EVar, Lang) Source
  , typedNodeExport :: Set EVar
} deriving (Show, Ord, Eq)
type TypedDag = DAG MVar [(EVar, EVar)] TypedNode


-- | Terms, see Dunfield Figure 1
data Expr
  = SrcE [Source]
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

newtype CType = CType { unCType :: Type }
  deriving (Show, Ord, Eq)

newtype GType = GType { unGType :: Type }
  deriving (Show, Ord, Eq)

newtype DefaultType = DefaultType { unDefaultType :: Type }
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
  = VarT TVar
  -- ^ (a)
  | ExistT TVar [Type] [DefaultType]
  -- ^ (a^) will be solved into one of the other types
  | Forall TVar Type
  -- ^ (Forall a . A)
  | FunT Type Type
  -- ^ (A->B)
  | ArrT TVar [Type] -- positional parameterized types
  -- ^ f [Type]
  | NamT TVar [(Text, Type)] -- keyword parameterized types
  -- ^ Foo { bar :: A, baz :: B }
  deriving (Show, Ord, Eq)

-- | A simplified subset of the Type record
-- functions, existential, and universal types are removed
-- language-specific info is removed
data JsonType
  = VarJ Text
  -- ^ {"int"}
  | ArrJ Text [JsonType]
  -- ^ {"list":["int"]}
  | NamJ Text [(Text, JsonType)]
  -- ^ {"Foo":{"bar":"A","baz":"B"}}
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
  index (ExistT t ts ds) = ExistG t ts ds
  index t = error $ "Can only index ExistT, found: " <> show t

class Typelike a where
  typeOf :: a -> Type

  nargs :: a -> Int
  nargs t = case typeOf t of
    (FunT _ t) -> 1 + nargs t
    (Forall _ t) -> nargs t
    _ -> 0

instance Typelike Type where
  typeOf = id

instance Typelike EType where
  typeOf = etype

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
  langOf (VarT (TV lang _)) = lang
  langOf x@(ExistT (TV lang _) ts _)
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
  langOf (NamT _ []) = error "empty records are not allowed"
  langOf x@(NamT (TV lang _) ts)
    | all ((==) lang) (map (langOf . snd) ts) = lang
    | otherwise = error $ "inconsistent languages in " <> show x

instance HasOneLanguage EType where
  langOf e = langOf (etype e) 

instance HasOneLanguage TVar where
  langOf (TV lang _) = lang


-- | An argument that is passed to a manifold
data Argument
  = SerialArgument Int CType
  -- ^ A serialized (e.g., JSON string) argument.  The parameters are 1)
  -- argument name (e.g., x), and 2) argument type (e.g., double). Some types
  -- may not be serializable. This is OK, so long as they are only used in
  -- functions of the same language.
  | NativeArgument Int CType
  -- ^ A native argument with the same parameters as above
  | PassThroughArgument Int 
  -- ^ A serialized argument that is untyped in the current language. It cannot
  -- be deserialized, but will be passed eventually to a foreign argument where it
  -- does have a concrete type.
  deriving (Show, Ord, Eq)

instance HasOneLanguage Argument where
  langOf (SerialArgument _ c) = langOf c
  langOf (NativeArgument _ c) = langOf c
  langOf (PassThroughArgument _) = Nothing


data TypeM
  = Passthrough -- ^ serialized data that cannot be deserialized in this language
  | Serial CType -- ^ serialized data that may be deserialized in this language
  | Native CType
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  | ForallM [TVar] TypeM
  deriving(Show, Eq, Ord)

instance HasOneLanguage TypeM where
  langOf Passthrough = Nothing
  langOf (Serial c) = langOf c
  langOf (Native c) = langOf c
  langOf (Function xs f) = listToMaybe $ catMaybes (map langOf (f:xs))


-- | A grammar that describes the implementation of the pools. Expressions in
-- this grammar will be directly translated into concrete code.
data ExprM
  = ManifoldM Int [Argument] ExprM
  -- ^ A wrapper around a single source call or (in some cases) a container.

  | ForeignInterfaceM
      TypeM -- required type in the calling language
      ExprM -- expression in the foreign language
  -- ^ A generic interface to an expression in another language. Currently it
  -- will be resolved only to the specfic pool call interface type, where
  -- system calls pass serialized information between pools in different
  -- languages. Eventually, better methods will be added for certain pairs of
  -- languages.

  | PoolCallM
      TypeM -- serialized return data
      [MDoc] -- shell command components that preceed the passed data
      [Argument] -- argument passed to the foreign function (must be serialized)
  -- ^ Make a system call to another language

  | LetM Int ExprM ExprM
  -- ^ let syntax allows fine control over order of operations in the generated
  -- code. The Int is an index for a LetVarM. It is also important in languages
  -- such as C++ where values need to be declared with explicit types and
  -- special constructors.

  | AppM
      ExprM -- ManifoldM | SrcM | LamM
      [ExprM] 

  | SrcM TypeM Source
  -- ^ a within pool function call (cis)

  | LamM [Argument] ExprM
  -- ^ Nothing Evar will be auto generated

  | BndVarM TypeM Int
  -- ^ A lambda-bound variable. BndVarM only describes variables bound as positional
  -- arguments in a manifold. The are represented as integers since the name
  -- will be language-specific.
  --
  -- In the rewrite step, morloc declarations are removed. So the expression:
  --   x = 5
  --   foo y = mul x y
  -- Is rewritten as:
  --   \y -> mul 5 y
  -- So BndVarM does NOT include variables defined in the morloc script. It only
  -- includes lambda-bound variables. The only BndVarM is `y` (`mul` is SrcM). The
  -- literal name "y" is replaced, though, with the integer 1. This required in
  -- order to avoid name conflicts in concrete languages, for example consider
  -- the following (perfectly legal) morloc function:
  --   foo for = mul for 2
  -- If the string "for" were retained as the variable name, this would fail in
  -- many language where "for" is a keyword.

  | LetVarM TypeM Int
  -- ^ An internally generated variable id used in let assignments. When
  -- translated into a language, the integer will be used to generate a unique
  -- variable name (e.g. [a0,a1,...] or [a,b,c,...]).

  -- containers
  | ListM TypeM [ExprM]
  | TupleM TypeM [ExprM]
  | RecordM TypeM [(EVar, ExprM)]

  -- primitives
  | LogM TypeM Bool
  | NumM TypeM Scientific
  | StrM TypeM Text
  | NullM TypeM

  -- serialization - these must remain abstract, since required arguments
  -- will vary between languages.
  | SerializeM ExprM
  | DeserializeM ExprM

  | ReturnM ExprM
  -- ^ The return value of a manifold. I need this to distinguish between the
  -- values assigned in let expressions and the final return value. In some
  -- languages, this may not be necessary (e.g., R).
  deriving(Show)

instance HasOneLanguage ExprM where
  -- langOf :: a -> Maybe Lang
  langOf (ManifoldM _ _ e) = langOf e
  langOf (ForeignInterfaceM t _) = langOf t
  langOf (PoolCallM t _ _) = langOf t
  langOf (LetM _ _ e2) = langOf e2
  langOf (AppM e _) = langOf e
  langOf (SrcM _ src) = Just (srcLang src) 
  langOf (LamM _ e) = langOf e
  langOf (BndVarM t _) = langOf t
  langOf (LetVarM t _) = langOf t
  langOf (ListM t _) = langOf t
  langOf (TupleM t _) = langOf t
  langOf (RecordM t _) = langOf t
  langOf (LogM t _) = langOf t
  langOf (NumM t _) = langOf t
  langOf (StrM t _) = langOf t
  langOf (NullM t) = langOf t
  langOf (SerializeM e) = langOf e
  langOf (DeserializeM e) = langOf e
  langOf (ReturnM e) = langOf e
