{-# LANGUAGE ViewPatterns #-}

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
  , inferConcreteVar
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Frontend.Desugar as MFD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Control.Monad.State as CMS

getConcreteMap :: Int -> Lang -> MorlocMonad Scope
getConcreteMap i lang = do
  globalMap <- CMS.gets stateConcreteTypedefs
  case GMap.lookup i globalMap of
    GMapJust langmap -> case Map.lookup lang langmap of
      (Just typemap) -> return typemap
      Nothing -> return Map.empty
    _ -> return Map.empty

inferConcreteType :: Scope -> Type -> MorlocMonad TypeF
inferConcreteType scope (type2typeu -> t) =
  case MFD.evaluateType scope t of
    (Left e) -> MM.throwError e
    (Right tu) -> weave t tu

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
  (Just ((_, t):_)) -> FV gv (CV . unTVar $ extractKey t)
  _ -> error $ "Concrete type var inference error for " <> show gv <> " in scope " <> show scope
