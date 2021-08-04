{-|
Module      : Morloc.Frontend.Namespace
Description : All frontend types and datastructures
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Namespace
  ( module Morloc.Namespace
  , ExprI(..)
  , Expr(..)
  , mapExprM
  , Import(..)
  , Stack
  , StackState(..)
  , StackConfig(..)
  -- ** Typechecking
  , Gamma
  , GammaIndex(..)
  , EType(..)
  , TypeSet(..)
  , Indexable(..)
  -- ** ModuleGamma paraphernalia
  , ModularGamma
  -- rifraf
  , resolve
  , substituteT
  , isGeneric
  ) where

import Morloc.Namespace hiding (name)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Char as DC
import qualified Data.Text as DT



-- This functions removes qualified and existential types.
--  * all qualified terms are replaced with UnkT
--  * all existentials are replaced with default values if a possible
--    FIXME: should I really just take the first in the list???
resolve :: UnresolvedType -> Type
resolve (VarU v) = VarT v
resolve (FunU t1 t2) = FunT (resolve t1) (resolve t2)
resolve (ArrU v ts) = ArrT v (map resolve ts)
resolve (NamU r v ps rs) =
  let ts' = map (resolve . snd) rs
      ps' = map resolve ps 
  in NamT r v ps' (zip (map fst rs) ts')
resolve (ExistU v _ []) = resolve (ForallU v (VarU v)) -- whatever
resolve (ExistU _ _ (t:_)) = resolve t
resolve (ForallU v t) = substituteT v (UnkT v) (resolve t)

-- | substitute all appearances of a given variable with a given new type
substituteT :: TVar -> Type -> Type -> Type
substituteT v0 r0 t0 = sub t0
  where
    sub :: Type -> Type
    sub t@(UnkT _) = t
    sub t@(VarT v)
      | v0 == v = r0
      | otherwise = t
    sub (FunT t1 t2) = FunT (sub t1) (sub t2)
    sub (ArrT v ts) = ArrT v (map sub ts)
    sub (NamT r v ts rs) = NamT r v (map sub ts) [(x, sub t) | (x, t) <- rs]

-- | Determine if a type term is generic (i.e., is the first letter lowercase?)
isGeneric :: TVar -> Bool
isGeneric (TV _ typeStr) = maybe False (DC.isLower . fst) (DT.uncons typeStr)

data ExprI = ExprI Int Expr
  deriving (Show, Ord, Eq)

mapExpr :: (Expr -> Expr) -> ExprI -> ExprI
mapExpr f e0 = g e0 where
  g (ExprI i (ModE v xs)) = ExprI i (f $ ModE v (map g xs))
  g (ExprI i (Declaration v e es)) = ExprI i (f $ Declaration v (g e) (map g es))
  g (ExprI i (AccE e k)) = ExprI i (f $ AccE (g e) k)
  g (ExprI i (ListE xs)) = ExprI i (f $ ListE (map g xs))
  g (ExprI i (TupleE xs)) = ExprI i (f $ TupleE (map g xs))
  g (ExprI i (LamE v e)) = ExprI i (f $ LamE v (g e))
  g (ExprI i (AppE e1 e2)) = ExprI i (f $ AppE (g e1) (g e2))
  g (ExprI i (AnnE e ts)) = ExprI i (f $ AnnE (g e) ts)
  g (ExprI i (RecE rs)) = ExprI i (f $ RecE (zip (map fst rs) (map g (map snd rs))))
  g (ExprI i e) = ExprI i (f e)

mapExprM :: Monad m => (Expr -> m Expr) -> ExprI -> m ExprI
mapExprM f e0 = g e0 where
  g (ExprI i (ModE v xs)) = ExprI i <$> ((ModE v <$> mapM g xs) >>= f)
  g (ExprI i (Declaration v e es)) = ExprI i <$> ((Declaration v <$> g e <*> mapM g es) >>= f)
  g (ExprI i (AccE e k)) = ExprI i <$> ((AccE <$> g e <*> pure k) >>= f)
  g (ExprI i (ListE xs)) = ExprI i <$> ((ListE <$> mapM g xs) >>= f)
  g (ExprI i (TupleE xs)) = ExprI i <$> ((TupleE <$> mapM g xs) >>= f)
  g (ExprI i (LamE v e)) = ExprI i <$> ((LamE v <$> g e) >>= f)
  g (ExprI i (AppE e1 e2)) = ExprI i <$> ((AppE <$> g e1 <*> g e2) >>= f)
  g (ExprI i (AnnE e ts)) = ExprI i <$> ((AnnE <$> g e <*> pure ts) >>= f)
  g (ExprI i (RecE rs)) = ExprI i <$> ((RecE <$> (zip (map fst rs) <$> mapM g (map snd rs))) >>= f)
  g (ExprI i e) = ExprI i <$> f e

-- | Terms, see Dunfield Figure 1
data Expr
  = ModE MVar [ExprI]
  -- ^ the toplevel expression in a module
  | TypE TVar [TVar] UnresolvedType
  -- ^ a type definition
  --   1. type name
  --   2. parameters
  --   3. type
  | ImpE Import
  -- ^ a morloc module import
  | ExpE EVar
  -- ^ a term that is exported from a module (should only exist at the toplevel)
  | SrcE [Source]
  -- ^ import "c" from "foo.c" ("f" as yolo).
  | Signature EVar EType
  -- ^ x :: A
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
  | ListE [ExprI]
  -- ^ [e]
  | TupleE [ExprI]
  -- ^ (e1), (e1,e2), ... (e1,e2,...,en)
  | LamE EVar ExprI
  -- ^ (\x -> e)
  | AppE ExprI ExprI
  -- ^ (e e)
  | AnnE ExprI [UnresolvedType]
  -- ^ (e : A)
  | NumE Scientific
  -- ^ number of arbitrary size and precision
  | LogE Bool
  -- ^ boolean primitive
  | StrE Text
  -- ^ literal string
  | RecE [(Text, ExprI)]
  deriving (Show, Ord, Eq)

data Import =
  Import
    { importModuleName :: MVar
    , importInclude :: Maybe [(EVar, EVar)]
    , importExclude :: [EVar]
    , importNamespace :: Maybe EVar -- currently not used
    }
  deriving (Ord, Eq, Show)

-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG ExprI TypeSet
  -- ^ (G,x:A) looked up in the (Var) and cut in (-->I)
  | ExistG TVar
    [UnresolvedType] -- FIXME: document
    [UnresolvedType] -- default types
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar UnresolvedType
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | SrcG Source
  -- ^ source
  | UnsolvedConstraint UnresolvedType UnresolvedType
  -- ^ Store an unsolved serialization constraint containing one or more
  -- existential variables. When the existential variables are solved, the
  -- constraint will be written into the Stack state.
  deriving (Ord, Eq, Show)

type Gamma = [GammaIndex]

data TypeSet =
  TypeSet (Maybe EType) [EType]
  deriving (Show, Eq, Ord)

type ModularGamma = Map MVar (Map EVar TypeSet)

class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable UnresolvedType where
  index (ExistU t ts ds) = ExistG t ts ds
  index t = error $ "Can only index ExistT, found: " <> show t



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
    , stateSer :: [(UnresolvedType, UnresolvedType)]
    , stateDepth :: Int
    }
  deriving (Ord, Eq, Show)
