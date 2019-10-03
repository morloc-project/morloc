{-|
Module      : Morloc.TypeChecker.Namespace
Description : Data structures and related functions
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.TypeChecker.Namespace
  ( EVar(..)
  , Expr(..)
  , Gamma
  , GammaIndex(..)
  , Import(..)
  , Indexable(..)
  , MVar(..)
  , Module(..)
  , Stack
  , StackState(..)
  , runStack
  , TVar(..)
  , Type(..)
  , TypeError(..)
  -- * State manipulation
  , fromType
  , toType
  -- * ModuleGamma paraphernalia
  , ModularGamma
  -- * Type extensions
  , Constraint(..)
  , EType(..)
  , Property(..)
  , TypeSet(..)
  ) where

import qualified Control.Monad.Except as ME
import qualified Control.Monad.Identity as MI
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import Morloc.Namespace (Path, Lang, (<>))
import qualified Morloc.Data.Text as MT

type Gamma = [GammaIndex]

newtype EVar =
  EV MT.Text
  deriving (Show, Eq, Ord)

newtype MVar =
  MV MT.Text
  deriving (Show, Eq, Ord)

newtype TVar =
  TV MT.Text
  deriving (Show, Eq, Ord)

-- | currently I do nothing with the Reader and Writer monads, but I'm leaving
-- them in for now since I will need them when I plug this all into Morloc.
runStack :: Stack a -> (Either TypeError a, [MT.Text])
runStack e =
  fst .
  MI.runIdentity .
  flip MS.runStateT emptyState . MW.runWriterT . ME.runExceptT . MR.runReaderT e $
  StackConfig 0

emptyState = StackState 0 0

type GeneralStack c e l s a
   = MR.ReaderT c (ME.ExceptT e (MW.WriterT l (MS.StateT s MI.Identity))) a

type Stack a = GeneralStack StackConfig TypeError [MT.Text] StackState a

data StackConfig =
  StackConfig
    { configVerbosity :: Int -- Not currently used
    }

data StackState =
  StackState
    { stateVar :: Int
    , stateQul :: Int
    }
  deriving (Ord, Eq, Show)

-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG Expr TypeSet
  -- ^ (G,x:A) looked up in the (Var) and cut in (-->I)
  | ExistG TVar
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar Type
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | MarkEG EVar
  -- ^ ...
  | SrcG (EVar, Lang, Maybe Path, EVar)
  -- ^ source
  | ConcreteG EVar Lang Type
  -- ^ store a local concrete type
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
  | AnnE Expr Type
  -- ^ (e : A)
  | NumE DS.Scientific
  -- ^ number of arbitrary size and precision
  | LogE Bool
  -- ^ boolean primitive
  | StrE MT.Text
  -- ^ literal string
  | RecE [(EVar, Expr)]
  deriving (Show, Ord, Eq)

-- | Types, see Dunfield Figure 6
data Type
  = UniT
  -- ^ (1)
  | VarT TVar
  -- ^ (a)
  | ExistT TVar
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
  | GeneralProperty [MT.Text]
  deriving (Show, Eq, Ord)

-- | Eventually, Constraint should be a richer type, but for they are left as
-- unparsed lines of text
newtype Constraint =
  Con MT.Text
  deriving (Show, Eq, Ord)

-- | Extended Type that may represent a language specific type as well as sets
-- of properties and constrains.
data EType =
  EType
    { etype :: Type
    , elang :: Maybe Lang
    , eprop :: Set.Set Property
    , econs :: Set.Set Constraint
    , esource :: Maybe (Maybe Path, EVar)
    }
  deriving (Show, Eq, Ord)

data TypeSet =
  TypeSet (Maybe EType) [EType]
  deriving (Show, Eq, Ord)

data TypeError
  = UnknownError
  | SubtypeError Type Type
  | ExistentialError
  | BadExistentialCast
  | AccessError MT.Text
  | NonFunctionDerive
  | UnboundVariable EVar
  | OccursCheckFail
  | EmptyCut
  | TypeMismatch
  | UnexpectedPattern Expr Type
  | ToplevelRedefinition
  | NoAnnotationFound -- I don't know what this is for
  | NotImplemented -- this should only be used as a placeholder
  | OtherError MT.Text
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
  deriving (Show, Ord, Eq)

type ModularGamma = Map.Map MVar (Map.Map EVar TypeSet)

class Typed a where
  toType :: a -> Maybe Type
  fromType :: Type -> a

instance Typed EType where
  toType e =
    case elang e of
      (Just _) -> Nothing
      Nothing -> Just (etype e)
  fromType t =
    EType
      { etype = t
      , elang = Nothing
      , eprop = Set.empty
      , econs = Set.empty
      , esource = Nothing
      }

instance Typed TypeSet where
  toType (TypeSet (Just e) _) = toType e
  toType (TypeSet Nothing _) = Nothing
  fromType t = TypeSet (Just (fromType t)) []

instance Typed Type where
  toType = Just
  fromType = id

class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable Type where
  index (ExistT t) = ExistG t
  index _ = error "Can only index ExistT"

instance Indexable Expr where
  index (AnnE x t) = AnnG x (fromType t)
  index _ = error "Can only index AnnE"
