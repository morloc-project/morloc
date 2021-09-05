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
  , mapExprM
  , Stack
  , StackState(..)
  , StackConfig(..)
  -- rifraf
  , isGeneric
  -- * accessing state
  , copyState
  ) where

import Morloc.Namespace hiding (name)
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM
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


-- | Determine if a type term is generic (i.e., is the first letter lowercase?)
isGeneric :: TVar -> Bool
isGeneric (TV _ typeStr) = maybe False (DC.isLower . fst) (DT.uncons typeStr)

mapExpr :: (Expr -> Expr) -> ExprI -> ExprI
mapExpr f e0 = g e0 where
  g (ExprI i (ModE v xs)) = ExprI i (f $ ModE v (map g xs))
  g (ExprI i (Declaration v e es)) = ExprI i (f $ Declaration v (g e) (map g es))
  g (ExprI i (AccE e k)) = ExprI i (f $ AccE (g e) k)
  g (ExprI i (ListE xs)) = ExprI i (f $ ListE (map g xs))
  g (ExprI i (TupleE xs)) = ExprI i (f $ TupleE (map g xs))
  g (ExprI i (LamE vs e)) = ExprI i (f $ LamE vs (g e))
  g (ExprI i (AppE e es)) = ExprI i (f $ AppE (g e) (map g es))
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
  g (ExprI i (LamE vs e)) = ExprI i <$> ((LamE vs <$> g e) >>= f)
  g (ExprI i (AppE e es)) = ExprI i <$> ((AppE <$> g e <*> mapM g es) >>= f)
  g (ExprI i (AnnE e ts)) = ExprI i <$> ((AnnE <$> g e <*> pure ts) >>= f)
  g (ExprI i (RecE rs)) = ExprI i <$> ((RecE <$> (zip (map fst rs) <$> mapM g (map snd rs))) >>= f)
  g (ExprI i e) = ExprI i <$> f e

-- | 
copyState :: Int -> Int -> MorlocMonad ()
copyState oldIndex newIndex = do
  s <- MM.get
  case GMap.yIsX (stateSignatures s) oldIndex newIndex of
    (Just x) -> MM.put $ s {stateSignatures = x}
    Nothing -> MM.throwError . CallTheMonkeys $ "Failed to copy state"


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
    , stateSer :: [(TypeU, TypeU)]
    , stateDepth :: Int
    }
  deriving (Ord, Eq, Show)
