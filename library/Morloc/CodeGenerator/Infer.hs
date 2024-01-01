{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Infer
Description : Infer concrete types
Copyright   : (c) Zebulun Arendsee, 2023
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Infer
  ( getScope
  , inferConcreteType
  , inferConcreteTypeU
  , inferConcreteVar
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Frontend.Desugar as MFD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM
import Morloc.Data.Doc
import qualified Data.Map as Map
import qualified Control.Monad.State as CMS

getScope :: Int -> Lang -> MorlocMonad (Scope, Scope)
getScope i lang = do
  cscope <- getConcreteScope i lang
  gscope <- getGeneralScope i
  return (cscope, gscope)

getConcreteScope :: Int -> Lang -> MorlocMonad Scope
getConcreteScope i lang = do
  globalMap <- CMS.gets stateConcreteTypedefs
  case GMap.lookup i globalMap of
    GMapJust langmap -> case Map.lookup lang langmap of
      (Just typemap) -> do
        MM.sayVVV $ "looking up concrete map for index" <+> pretty i <+> "and found scope:" <+> viaShow typemap
        return typemap
      Nothing -> do
        MM.sayVVV $ "looking up concrete map for index" <+> pretty i <+> "and found nothing"
        return Map.empty
    _ -> do
      MM.sayVVV $ "Could not find a typedef map for index" <+> pretty i
      return Map.empty

getGeneralScope :: Int -> MorlocMonad Scope
getGeneralScope i = do
  globalMap <- CMS.gets stateGeneralTypedefs
  case GMap.lookup i globalMap of
    GMapJust scope -> return scope
    _ -> return Map.empty

inferConcreteTypeU :: (Scope, Scope) -> TypeU -> MorlocMonad TypeU
inferConcreteTypeU (cscope, gscope) t = do
  MM.sayVVV $ "cscope:" <+> viaShow cscope
  MM.sayVVV $ "gscope:" <+> viaShow gscope
  case MFD.pairEval cscope gscope t of
    (Left e) -> MM.throwError e
    (Right tu) -> return tu

inferConcreteType :: (Scope, Scope) -> Type -> MorlocMonad TypeF
inferConcreteType scope@(_, gscope) (type2typeu -> generalType) = do
  concreteType <- inferConcreteTypeU scope generalType
  MM.sayVVV $ "inferConcreteType\n  generalType:" <+> pretty generalType
            <> "\n  concreteType:" <+> pretty concreteType
  weave gscope generalType concreteType

weave :: Scope -> TypeU -> TypeU -> MorlocMonad TypeF
weave gscope = w where
  w (VarU v1) (VarU (TV v2)) = return $ VarF (FV v1 (CV v2))
  w (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM w ts1 ts2 <*> w t1 t2 
  w (AppU t1 ts1) (AppU t2 ts2) = AppF <$> w t1 t2 <*> zipWithM w ts1 ts2
  w t1@(NamU o1 v1 ts1 rs1) t2@(NamU o2 v2 ts2 rs2)
    | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2
        = NamF o1 (FV v1 (CV (unTVar v2)))
        <$> zipWithM w ts1 ts2
        <*> zipWithM (\ (_, t1) (k2, t2) -> (,) k2 <$> w t1 t2) rs1 rs2
    | otherwise = error $ "failed to weave: " <> "\n  t1: " <> show t1 <> "\n  t2: " <> show t2
  w t1 t2 = case MFD.evaluateStep gscope t1 of
    Nothing -> error $ "failed to weave: " <> "\n  t1: " <> show t1 <> "\n  t2: " <> show t2
    (Just t1') -> if t1 == t1'
      then error "failed to weave"
      else do
        MM.sayVVV $ "in weave cast" <+> pretty t1 <+> "to" <+> pretty t1'
        w t1' t2

inferConcreteVar :: Scope -> TVar -> FVar
inferConcreteVar scope gv = case Map.lookup gv scope of
  (Just ((_, t, True):_)) -> FV gv (CV . unTVar $ extractKey t)
  (Just ((_, t, False):_)) -> error $ "Substituting the non-terminal " <> show (extractKey t) <> " into type " <> show t
  _ -> error $ "Concrete type var inference error for " <> show gv <> " in scope " <> show scope
