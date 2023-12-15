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
  , mapExpr
  , mapExprM
  -- rifraf
  , isGeneric
  -- * accessing state
  , copyState
  ) where

import Morloc.Namespace hiding (name)
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Data.Char as DC
import qualified Data.Text as DT


-- | Determine if a type term is generic (i.e., is the first letter lowercase?)
isGeneric :: MT.Text -> Bool
isGeneric typeStr = maybe False (DC.isLower . fst) (DT.uncons typeStr)

mapExpr :: (Expr -> Expr) -> ExprI -> ExprI
mapExpr f = g where
  g (ExprI i (ModE v xs)) = ExprI i . f $ ModE v (map g xs)
  g (ExprI i (AssE v e es)) = ExprI i . f $ AssE v (g e) (map g es)
  g (ExprI i (AccE e k)) = ExprI i . f $ AccE (g e) k
  g (ExprI i (LstE es)) = ExprI i . f $ LstE (map g es)
  g (ExprI i (TupE es)) = ExprI i . f $ TupE (map g es)
  g (ExprI i (AppE e es)) = ExprI i . f $ AppE (g e) (map g es)
  g (ExprI i (NamE rs)) = ExprI i . f $ NamE [(k, g e) | (k, e) <- rs]
  g (ExprI i (LamE vs e)) = ExprI i . f $ LamE vs (g e)
  g (ExprI i (AnnE e ts)) = ExprI i . f $ AnnE (g e) ts
  g (ExprI i e) = ExprI i (f e)

mapExprM :: Monad m => (Expr -> m Expr) -> ExprI -> m ExprI
mapExprM f = g where
  g (ExprI i (ModE v xs)) = ExprI i <$> (mapM g xs >>= f . ModE v)
  g (ExprI i (AssE v e es)) = ExprI i <$> ((AssE v <$> g e <*> mapM g es) >>= f)
  g (ExprI i (AccE e k)) = ExprI i <$> ((AccE <$> g e <*> pure k) >>= f)
  g (ExprI i (LstE es)) = ExprI i <$> (mapM g es >>= f . LstE)
  g (ExprI i (TupE es)) = ExprI i <$> (mapM g es >>= f . TupE)
  g (ExprI i (AppE e es)) = ExprI i <$> ((AppE <$> g e <*> mapM g es) >>= f)
  g (ExprI i (NamE rs)) = do
    es' <- mapM (g . snd) rs
    ExprI i <$> f (NamE (zip (map fst rs) es'))
  g (ExprI i (LamE vs e)) = ExprI i <$> (g e >>= f . LamE vs)
  g (ExprI i (AnnE e ts)) = ExprI i <$> ((AnnE <$> g e <*> pure ts) >>= f)
  g (ExprI i e) = ExprI i <$> f e


-- WARNING: silent bad things happen if this function does not copy all indices
copyState :: Int -> Int -> MorlocMonad ()
copyState oldIndex newIndex = do
  s <- MM.get
  case GMap.yIsX (stateSignatures s) oldIndex newIndex of
    (Just x) -> MM.put $ s {stateSignatures = x}
    Nothing -> return ()
