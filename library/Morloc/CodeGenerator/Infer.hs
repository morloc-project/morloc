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
  ( getConcreteMap
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

getConcreteMap :: Int -> Lang -> MorlocMonad Scope
getConcreteMap i lang = do
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

inferConcreteTypeU :: Scope -> TypeU -> MorlocMonad TypeU
inferConcreteTypeU scope t =
  case MFD.transformType scope t of
    (Left e) -> MM.throwError e
    (Right tu) -> return tu

inferConcreteType :: Scope -> Type -> MorlocMonad TypeF
inferConcreteType scope (type2typeu -> t) = do
  tu <- inferConcreteTypeU scope t
  weave t tu

weave :: TypeU -> TypeU -> MorlocMonad TypeF
weave (VarU v1) (VarU (TV v2)) = return $ VarF (FV v1 (CV v2))
weave (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM weave ts1 ts2 <*> weave t1 t2 
weave (AppU t1 ts1) (AppU t2 ts2) = AppF <$> weave t1 t2 <*> zipWithM weave ts1 ts2
weave (NamU o1 v1 ts1 rs1) (NamU o2 v2 ts2 rs2)
  | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2
      = NamF o1 (FV v1 (CV (unTVar v2)))
      <$> zipWithM weave ts1 ts2
      <*> zipWithM (\ (_, t1) (k2, t2) -> (,) k2 <$> weave t1 t2) rs1 rs2
  | otherwise = undefined
weave _ _ = undefined

inferConcreteVar :: Scope -> TVar -> FVar
inferConcreteVar scope gv = case Map.lookup gv scope of
  (Just ((_, t, True):_)) -> FV gv (CV . unTVar $ extractKey t)
  (Just ((_, t, False):_)) -> error $ "Substituting the non-terminal " <> show (extractKey t) <> " into type " <> show t
  _ -> error $ "Concrete type var inference error for " <> show gv <> " in scope " <> show scope
