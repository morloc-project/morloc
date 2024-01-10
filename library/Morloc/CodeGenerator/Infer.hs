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
  , inferConcreteTypeUniversal
  , inferConcreteTypeU
  , inferConcreteVar
  , evalGeneralStep
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.TypeEval as T
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

evalGeneralStep :: Int -> TypeU -> MorlocMonad (Maybe TypeU)
evalGeneralStep i t = do 
  globalMap <- MM.gets stateGeneralTypedefs
  gscope <- case GMap.lookup i globalMap of
    GMapJust scope -> return scope
    _ -> return Map.empty
  return $ T.evaluateStep gscope t

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


inferConcreteTypeU :: Lang -> Indexed TypeU -> MorlocMonad TypeU
inferConcreteTypeU lang t@(Idx i t0) = do
  MM.sayVVV $ "inferConcreteTypeU" <+> pretty lang <+> pretty t
  attemptT <- getScope i lang >>= inferConcreteTypeU' t0
  case attemptT of
    (Right t) -> return t
    (Left e1) -> do
      MM.sayVVV $ "Warning: failed to infer concrete type" <+> pretty t0 <+> "for index" <+> pretty i
                <> "\n  Observed error:" <> pretty e1
                <> "\n  Retrying with universal scope"
      gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
      cscopeUni <- CMS.gets stateUniversalConcreteTypedefs |>> fromMaybe Map.empty . Map.lookup lang
      attemptUni <- inferConcreteTypeU' t0 (cscopeUni, gscopeUni)
      case attemptUni of
        (Right t) -> return t
        (Left e2) -> MM.throwError e2

inferConcreteTypeU' :: TypeU -> (Scope, Scope)-> MorlocMonad (Either MorlocError TypeU)
inferConcreteTypeU' t (cscope, gscope) = do
  MM.sayVVV $ "cscope:" <+> viaShow cscope
  MM.sayVVV $ "gscope:" <+> viaShow gscope
  return $ T.pairEval cscope gscope t

inferConcreteType :: Lang -> Indexed Type -> MorlocMonad TypeF
inferConcreteType lang (Idx i (type2typeu -> generalType)) = do
  concreteType <- inferConcreteTypeU lang (Idx i generalType)
  (_, gscope) <- getScope i lang
  case weave gscope generalType concreteType of
    (Right tf) -> return tf
    (Left e1) -> do
      MM.sayVVV $ "Warning: failed to weave general type" <+> pretty generalType <+> "with concrete type" <+> pretty concreteType <+> "for index" <+> pretty i
                <> "\n  Observed error:" <> viaShow e1
                <> "\n  Retrying with universal scope"
      gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
      case weave gscopeUni generalType concreteType of
        (Right tf) -> return tf
        (Left _) -> MM.throwError CannotInferConcretePrimitiveType

inferConcreteTypeUniversal :: Lang -> Type -> MorlocMonad TypeF
inferConcreteTypeUniversal lang (type2typeu -> generalType) = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  concreteType <- inferConcreteTypeUUniversal lang generalType
  case weave gscopeUni generalType concreteType of
    (Right tf) -> return tf
    (Left _) -> MM.throwError CannotInferConcretePrimitiveType

inferConcreteTypeUUniversal :: Lang -> TypeU -> MorlocMonad TypeU
inferConcreteTypeUUniversal lang generalType = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  cscopeUni <- CMS.gets stateUniversalConcreteTypedefs |>> fromMaybe Map.empty . Map.lookup lang
  attemptUni <- inferConcreteTypeU' generalType (cscopeUni, gscopeUni)
  case attemptUni of
    (Right t) -> return t
    (Left e2) -> MM.throwError e2

weave :: Scope -> TypeU -> TypeU -> Either MDoc TypeF
weave gscope = w where
  w (VarU v1) (VarU (TV v2)) = return $ VarF (FV v1 (CV v2))
  w (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM w ts1 ts2 <*> w t1 t2 
  w (AppU t1 ts1) (AppU t2 ts2) = AppF <$> w t1 t2 <*> zipWithM w ts1 ts2
  w t1@(NamU o1 v1 ts1 rs1) t2@(NamU o2 v2 ts2 rs2)
    | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2
        = NamF o1 (FV v1 (CV (unTVar v2)))
        <$> zipWithM w ts1 ts2
        <*> zipWithM (\ (_, t1) (k2, t2) -> (,) k2 <$> w t1 t2) rs1 rs2
    | otherwise = Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <+> "\n  t2:" <+> pretty t2
  w t1 t2 = case T.evaluateStep gscope t1 of
    Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
    (Just t1') -> if t1 == t1'
      then Left "failed to weave"
      else do
        w t1' t2


inferConcreteVar :: Lang -> Indexed TVar -> MorlocMonad FVar
inferConcreteVar lang t@(Idx i v) = do
  MM.sayVVV $ "inferConcreteVar" <+> pretty lang <+> pretty t
  inferConcreteVar' v <$> getConcreteScope i lang

inferConcreteVar' :: TVar -> Scope -> FVar
inferConcreteVar' gv scope = case Map.lookup gv scope of
  (Just ((_, t, True):_)) -> FV gv (CV . unTVar $ extractKey t)
  (Just ((_, t, False):_)) -> error $ "Substituting the non-terminal " <> show (extractKey t) <> " into type " <> show t
  _ -> error $ "Concrete type var inference error for " <> show gv <> " in scope " <> show scope
