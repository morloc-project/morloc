{- |
Module      : Morloc.Frontend.Namespace
Description : All frontend types and datastructures
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Namespace
  ( module Morloc.Namespace.Prim
  , module Morloc.Namespace.Type
  , module Morloc.Namespace.Expr
  , module Morloc.Namespace.State
  , mapExpr
  , mapExprM
  -- rifraf
  , isGeneric

    -- * accessing state
  , copyState
  ) where

import qualified Data.Char as DC
import Data.Text (Text)
import qualified Data.Text as DT
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim hiding (name)
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import Morloc.Namespace.State

-- | Determine if a type term is generic (i.e., is the first letter lowercase?)
isGeneric :: Text -> Bool
isGeneric typeStr = maybe False (DC.isLower . fst) (DT.uncons typeStr)

mapExpr :: (Expr -> Expr) -> ExprI -> ExprI
mapExpr f = g
  where
    g (ExprI i (ModE v xs)) = ExprI i . f $ ModE v (map g xs)
    g (ExprI i (AssE v e es)) = ExprI i . f $ AssE v (g e) (map g es)
    g (ExprI i (LstE es)) = ExprI i . f $ LstE (map g es)
    g (ExprI i (TupE es)) = ExprI i . f $ TupE (map g es)
    g (ExprI i (AppE e es)) = ExprI i . f $ AppE (g e) (map g es)
    g (ExprI i (NamE rs)) = ExprI i . f $ NamE [(k, g e) | (k, e) <- rs]
    g (ExprI i (LamE vs e)) = ExprI i . f $ LamE vs (g e)
    g (ExprI i (AnnE e ts)) = ExprI i . f $ AnnE (g e) ts
    g (ExprI i e) = ExprI i (f e)

mapExprM :: (Monad m) => (Expr -> m Expr) -> ExprI -> m ExprI
mapExprM f = g
  where
    g (ExprI i (ModE v xs)) = ExprI i <$> (mapM g xs >>= f . ModE v)
    g (ExprI i (AssE v e es)) = ExprI i <$> ((AssE v <$> g e <*> mapM g es) >>= f)
    g (ExprI i (LstE es)) = ExprI i <$> (mapM g es >>= f . LstE)
    g (ExprI i (TupE es)) = ExprI i <$> (mapM g es >>= f . TupE)
    g (ExprI i (AppE e es)) = ExprI i <$> ((AppE <$> g e <*> mapM g es) >>= f)
    g (ExprI i (NamE rs)) = do
      es' <- mapM (g . snd) rs
      ExprI i <$> f (NamE (zip (map fst rs) es'))
    g (ExprI i (LamE vs e)) = ExprI i <$> (g e >>= f . LamE vs)
    g (ExprI i (AnnE e ts)) = ExprI i <$> ((AnnE <$> g e <*> pure ts) >>= f)
    g (ExprI i e) = ExprI i <$> f e

copyState :: Int -> Int -> MorlocMonad ()
copyState oldIndex newIndex = do
  s <- MM.get

  -- Could be defined more succinctly, but it is IMPERATIVE that every index
  -- is copied. Listing all fields will ensure that an error is raised if a
  -- new MorlocState field is added but included in this function.
  MM.put $
    MorlocState
      { statePackageMeta = statePackageMeta s
      , stateVerbosity = stateVerbosity s
      , stateCounter = stateCounter s
      , stateDepth = stateDepth s
      , stateSignatures = updateGMap (stateSignatures s)
      , stateTypeclasses = stateTypeclasses s
      , stateConcreteTypedefs = updateGMap (stateConcreteTypedefs s)
      , stateGeneralTypedefs = updateGMap (stateGeneralTypedefs s)
      , stateUniversalGeneralTypedefs = stateUniversalGeneralTypedefs s
      , stateUniversalConcreteTypedefs = stateUniversalConcreteTypedefs s
      , stateSources = updateGMap (stateSources s)
      , stateAnnotations = updateMap (stateAnnotations s)
      , stateOutfile = stateOutfile s
      , stateExports = updateList (stateExports s)
      , stateName = updateMap (stateName s)
      , stateManifoldConfig = updateMap (stateManifoldConfig s)
      , stateTypeQualifier = updateMap (stateTypeQualifier s)
      , stateSourceMap = updateMap (stateSourceMap s)
      , stateSourceText = stateSourceText s
      , stateBuildConfig = stateBuildConfig s
      , stateModuleName = stateModuleName s
      , stateInstall = stateInstall s
      , stateInstallDir = stateInstallDir s
      , stateClassDefs = stateClassDefs s
      , stateLangRegistry = stateLangRegistry s
      , stateExportGroups = stateExportGroups s
      }
  where
    updateGMap g = case GMap.yIsX oldIndex newIndex g of
      (Just g') -> g'
      Nothing -> g

    updateMap m = case Map.lookup oldIndex m of
      (Just x) -> Map.insert newIndex x m
      Nothing -> m

    updateList xs = if oldIndex `elem` xs then newIndex : xs else xs
