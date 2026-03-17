{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Infer
Description : Infer concrete (language-specific) types from type aliases
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Maps general types to their concrete counterparts by evaluating type
aliases in the language-specific scope. Used by 'Express' and 'Serialize'
to determine how values are represented in each target language.
-}
module Morloc.CodeGenerator.Infer
  ( getScope
  , inferConcreteType
  , inferConcreteTypeUniversal
  , inferConcreteTypeU
  , inferConcreteVar
  , evalGeneralStep
  ) where

import qualified Control.Monad.State as CMS
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as T

-- TODO: do not use global scope here
getScope :: Int -> Lang -> MorlocMonad (Scope, Scope)
getScope _ lang = do
  cscope <- MM.getConcreteUniversalScope lang
  gscope <- MM.getGeneralUniversalScope
  MM.sayVVV $ "cscope:" <+> viaShow cscope
  return (cscope, gscope)

evalGeneralStep :: Int -> TypeU -> MorlocMonad (Maybe TypeU)
evalGeneralStep i t = T.evaluateStep <$> MM.getGeneralScope i <*> pure t

inferConcreteTypeU :: Lang -> Indexed TypeU -> MorlocMonad TypeU
inferConcreteTypeU lang (Idx i t0) = do
  attemptT <- inferConcreteTypeU' t0 <$> getScope i lang
  case attemptT of
    (Right t') -> return t'
    (Left _) -> do
      gscopeUni <- MM.getGeneralUniversalScope
      cscopeUni <- MM.getConcreteUniversalScope lang
      case inferConcreteTypeU' t0 (cscopeUni, gscopeUni) of
        (Right t') -> return t'
        (Left (SystemError e2)) -> MM.throwSourcedError i e2
        (Left e2) -> MM.throwError e2

inferConcreteTypeU' :: TypeU -> (Scope, Scope) -> Either MorlocError TypeU
inferConcreteTypeU' generalType (cscope, gscope) = T.pairEval cscope gscope generalType

inferConcreteType :: Lang -> Indexed Type -> MorlocMonad TypeF
inferConcreteType _ (Idx i (UnkT _)) =
  MM.throwSourcedError i "Cannot infer concrete type for UnkT. This may be an unsolved generic term"
inferConcreteType lang (Idx i (type2typeu -> generalType)) = do
  concreteType <- inferConcreteTypeU lang (Idx i generalType)
  (_, gscope) <- getScope i lang
  case weave gscope generalType concreteType of
    (Right tf) -> return tf
    (Left _) -> do
      gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
      case weave gscopeUni generalType concreteType of
        (Right tf) -> return tf
        (Left _) -> do
          -- Evaluate the general type one level and try again
          --
          -- Weaving will fail for parameterize type definitions, such as
          --   type (Foo a) = [(a, Str)]
          -- Here the primitive type (e.g., "std::vector<std::tuple<$1,std::string>>" a)
          -- cannot be woven with the `Foo a` type. So `Foo a` needs to be
          -- substituted for [(a, Str)], which can be woven.
          mayReducedGType <- evalGeneralStep i generalType
          case mayReducedGType of
            (Just reducedGType) -> inferConcreteType lang (Idx i (typeOf reducedGType))
            Nothing ->
              MM.throwSourcedError i $
                "Cannot infer concrete type for" <+> pretty generalType <> "\nCould not reduce type"

inferConcreteTypeUniversal :: Lang -> Type -> MorlocMonad TypeF
inferConcreteTypeUniversal lang t@(type2typeu -> generalType) = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  concreteType <- inferConcreteTypeUUniversal lang generalType
  case weave gscopeUni generalType concreteType of
    (Right tf) -> return tf
    (Left _) -> do
      -- Evaluate the general type one level and try again
      case T.evaluateStep gscopeUni generalType of
        (Just reducedGType) ->
          if reducedGType == generalType
            then
              MM.throwSystemError $
                "Failed to resolve concrete type for" <+> pretty t <+> "and cannot evaluate any further"
            else
              MM.throwSystemError $
                "Failed to infer concrete type for" <+> pretty generalType
                  <> ": Cannot unify with" <+> pretty reducedGType
        Nothing ->
          MM.throwSystemError $
            "Failed to infer concrete type for" <+> pretty t <+> ": Could not reduce type in broadest scope"

inferConcreteTypeUUniversal :: Lang -> TypeU -> MorlocMonad TypeU
inferConcreteTypeUUniversal lang generalType = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  cscopeUni <- CMS.gets stateUniversalConcreteTypedefs |>> fromMaybe Map.empty . Map.lookup lang
  let attemptUni = inferConcreteTypeU' generalType (cscopeUni, gscopeUni)
  case attemptUni of
    (Right t) -> return t
    (Left (SystemError e2)) ->
      MM.throwSystemError $
        "Failed to infer concrete universal type for lang"
          <+> pretty lang
          <+> "for type"
          <+> pretty generalType
          <> ":" <+> e2
    (Left e) -> MM.throwError e

weave :: Scope -> TypeU -> TypeU -> Either MDoc TypeF
weave gscope = w
  where
    w (VarU v1) (VarU (TV v2)) = return $ VarF (FV v1 (CV v2))
    w (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM w ts1 ts2 <*> w t1 t2
    w (AppU t1 ts1) (AppU t2 ts2) = AppF <$> w t1 t2 <*> weaveArgs ts1 ts2
    w t1@(NamU o1 v1 ts1 rs1) t2@(NamU o2 v2 ts2 rs2)
      | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2 =
          NamF o1 (FV v1 (CV (unTVar v2)))
            <$> zipWithM w ts1 ts2
            <*> zipWithM (\(_, t1') (k2', t2') -> (,) k2' <$> w t1' t2') rs1 rs2
      | otherwise = Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <+> "\n  t2:" <+> pretty t2
    w (EffectU effs t1) (EffectU _ t2) = EffectF (resolveEffectSet effs) <$> w t1 t2
    w (OptionalU t1) (OptionalU t2) = OptionalF <$> w t1 t2
    w (NatLitU n) (NatLitU _) = return $ NatLitF n
    w (NatLitU n) _ = return $ NatLitF n  -- Nat params may be erased in concrete type
    w (NatAddU _ _) _ = return $ NatLitF 0  -- Nat arithmetic erased in concrete type
    w (NatMulU _ _) _ = return $ NatLitF 0  -- Nat arithmetic erased in concrete type
    w (NatSubU _ _) _ = return $ NatLitF 0  -- Nat arithmetic erased in concrete type
    w (NatDivU _ _) _ = return $ NatLitF 0  -- Nat arithmetic erased in concrete type
    w (NatVarU _) _ = return $ NatLitF 0  -- Nat variable erased in concrete type
    w (ForallU v (VarU v')) _ | v == v' = return $ NatLitF 0  -- Unresolved variable (UnkT pattern)
    w t1 t2 = case T.evaluateStep gscope t1 of
      Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
      (Just t1') ->
        if t1 == t1'
          then Left ("failed to weave:" <> pretty t1 <+> "vs" <+> pretty t1')
          else do
            w t1' t2

    -- Weave type arguments, handling Nat params that may be erased in concrete type.
    -- Nat-kinded general args have no concrete counterpart, so we consume them
    -- without advancing the concrete list, but still emit a NatLitF placeholder.
    weaveArgs :: [TypeU] -> [TypeU] -> Either MDoc [TypeF]
    weaveArgs [] [] = Right []
    weaveArgs [] _ = Left "concrete type has more args than general type in weave"
    weaveArgs (NatLitU n : gs) cs = (NatLitF n :) <$> weaveArgs gs cs
    weaveArgs (NatAddU _ _ : gs) cs = (NatLitF 0 :) <$> weaveArgs gs cs
    weaveArgs (NatMulU _ _ : gs) cs = (NatLitF 0 :) <$> weaveArgs gs cs
    weaveArgs (NatSubU _ _ : gs) cs = (NatLitF 0 :) <$> weaveArgs gs cs
    weaveArgs (NatDivU _ _ : gs) cs = (NatLitF 0 :) <$> weaveArgs gs cs
    -- Unresolved nat dimension variable (opaque output dims): treat as erased
    weaveArgs (NatVarU _ : gs) cs = (NatLitF 0 :) <$> weaveArgs gs cs
    weaveArgs (ForallU v (VarU v') : gs) cs | v == v' = (NatLitF 0 :) <$> weaveArgs gs cs
    weaveArgs (g:gs) (c:cs) = (:) <$> w g c <*> weaveArgs gs cs
    weaveArgs _ [] = Left "general type has more non-Nat args than concrete type in weave"

inferConcreteVar :: Lang -> Indexed TVar -> MorlocMonad FVar
inferConcreteVar lang t0@(Idx i v) = do
  MM.sayVVV $ "inferConcreteVar" <+> pretty lang <+> pretty t0
  localScope <- MM.getConcreteScope i lang
  globalScope <- MM.getConcreteUniversalScope lang
  case Map.lookup v localScope of
    (Just ((_, t, _, True) : _)) -> return $ FV v (CV . unTVar $ extractKey t)
    (Just ((_, t, _, False) : _)) -> error $ "Substituting the non-terminal " <> show (extractKey t) <> " into type " <> show t
    _ -> case Map.lookup v globalScope of
      (Just ((_, t, _, True) : _)) -> do
        -- TODO fix this, the types should be in scope
        MM.sayVVV $ "WARNING: using global definition for v=" <> pretty v
        return $ FV v (CV . unTVar $ extractKey t)
      (Just ((_, t, _, False) : _)) -> error $ "Substituting the non-terminal " <> show (extractKey t) <> " into type " <> show t
      _ -> do
        -- Try transitive resolution: expand through general scope
        (cscope, gscope) <- getScope i lang
        case T.pairEval cscope gscope (VarU v) of
          Right (VarU v') -> return $ FV v (CV (unTVar v'))
          Right _ -> error $ "Transitive resolution of " <> show (unTVar v)
                          <> " yielded non-variable type"
          Left _ -> error $ "Cannot find type variable "
                         <> show (unTVar v) <> " in scope"
