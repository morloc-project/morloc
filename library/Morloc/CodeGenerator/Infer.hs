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
      -- Evaluate the general type one level and recurse. This is the
      -- same pattern as inferConcreteType: if a parameterized alias
      -- resolves only via its body (e.g. `type FixedPair (n :: Nat) a = (a, a)`
      -- where the concrete-scope mapping lives on Tuple2, not FixedPair),
      -- we step the alias and retry inference on the body.
      case T.evaluateStep gscopeUni generalType of
        (Just reducedGType)
          | reducedGType /= generalType ->
              inferConcreteTypeUniversal lang (typeOf reducedGType)
        _ ->
          MM.throwSystemError $
            "Failed to infer concrete type for" <+> pretty t
              <> ": Could not reduce type in broadest scope"

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
    w (EffectU effs t1) (EffectU _ t2) = mkEffectF (resolveEffectSet effs) <$> w t1 t2
    w (OptionalU t1) (OptionalU t2) = OptionalF <$> w t1 t2
    w (NatLitU n) (NatLitU _) = return $ NatLitF n
    w (NatLitU n) _ = return $ NatLitF n  -- Nat params may be erased in concrete type
    w NatVoidU _ = return NatVoidF  -- Erased phantom Nat slot
    w (NatAddU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w (NatMulU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w (NatSubU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w (NatDivU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w (NatVarU _) _ = return NatVoidF  -- Nat variable erased in concrete type
    w (LabeledU _ t1) t2 = w t1 t2
    w (ForallU v (VarU v')) _ | v == v' = return NatVoidF  -- Unresolved variable (UnkT pattern)
    w t1 t2 = case T.evaluateStep gscope t1 of
      Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
      (Just t1') ->
        if t1 == t1'
          then Left ("failed to weave:" <> pretty t1 <+> "vs" <+> pretty t1')
          else do
            w t1' t2

    -- Weave type arguments, handling Nat params that may be erased OR
    -- preserved in the concrete type. When the concrete head is also a Nat
    -- expression, consume it in lockstep; otherwise consume only the general
    -- arg (erased in concrete). Either way we emit a NatLitF placeholder.
    weaveArgs :: [TypeU] -> [TypeU] -> Either MDoc [TypeF]
    weaveArgs [] [] = Right []
    weaveArgs [] cs
      | all isNatLikeU cs = Right []  -- trailing Nat args in concrete only
      | otherwise = Left "concrete type has more non-Nat args than general type in weave"
    weaveArgs (NatLitU n : gs) cs = (NatLitF n :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (NatVoidU : gs) cs = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (NatAddU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (NatMulU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (NatSubU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (NatDivU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    -- Unresolved nat dimension variable (opaque output dims): treat as erased
    weaveArgs (NatVarU _ : gs) cs = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (ForallU v (VarU v') : gs) cs | v == v' = (NatVoidF :) <$> weaveArgs gs (dropNatHead cs)
    weaveArgs (g:gs) (c:cs) = (:) <$> w g c <*> weaveArgs gs cs
    weaveArgs _ [] = Left "general type has more non-Nat args than concrete type in weave"

    isNatLikeU :: TypeU -> Bool
    isNatLikeU (NatLitU _) = True
    isNatLikeU (NatVarU _) = True
    isNatLikeU (NatAddU _ _) = True
    isNatLikeU (NatMulU _ _) = True
    isNatLikeU (NatSubU _ _) = True
    isNatLikeU (NatDivU _ _) = True
    isNatLikeU NatVoidU = True
    isNatLikeU _ = False

    -- Drop a leading Nat-shaped concrete arg, if present.
    dropNatHead :: [TypeU] -> [TypeU]
    dropNatHead (c : cs) | isNatLikeU c = cs
    dropNatHead cs = cs

inferConcreteVar :: Lang -> Indexed TVar -> MorlocMonad FVar
inferConcreteVar lang t0@(Idx i v) = do
  MM.sayVVV $ "inferConcreteVar" <+> pretty lang <+> pretty t0
  localScope <- MM.getConcreteScope i lang
  globalScope <- MM.getConcreteUniversalScope lang
  case Map.lookup v localScope of
    (Just ((_, t, _, True) : _)) -> return $ FV v (CV . unTVar $ extractKey t)
    -- Non-terminal concrete alias: e.g. `type Cpp => Array a = List a`.
    -- Follow through the body's head (recursively) until a terminal entry
    -- is reached, then pair the *original* v with the resolved concrete
    -- name. This preserves the morloc-level identity (Array stays Array)
    -- while picking up the runtime concrete (std::vector here).
    (Just ((_, t, _, False) : _)) -> do
      FV _ cv <- inferConcreteVar lang (Idx i (extractKey t))
      return $ FV v cv
    _ -> case Map.lookup v globalScope of
      (Just ((_, t, _, True) : _)) -> do
        -- TODO fix this, the types should be in scope
        MM.sayVVV $ "WARNING: using global definition for v=" <> pretty v
        return $ FV v (CV . unTVar $ extractKey t)
      -- Same recursive resolution at the global scope level.
      (Just ((_, t, _, False) : _)) -> do
        FV _ cv <- inferConcreteVar lang (Idx i (extractKey t))
        return $ FV v cv
      _ -> do
        -- Not in any concrete scope. Try general scope: a general-only
        -- alias like `type MyVec (n :: Nat) a = Vector n a` has no
        -- concrete mapping of its own, but its body's head (Vector)
        -- should be resolvable. Recurse on the body's head, preserving
        -- the original morloc identity (`v`) while picking up the
        -- transitively resolved concrete name.
        --
        -- This is the bare-VarU analog of `expandHeadOnly`'s kind-based
        -- realignment: for general aliases that have params but no args
        -- in this lookup, we still want to chase the alias by its head.
        gscopeUni <- MM.getGeneralUniversalScope
        case Map.lookup v gscopeUni of
          (Just ((_, body, _, _) : _)) -> do
            FV _ cv <- inferConcreteVar lang (Idx i (extractKey body))
            return $ FV v cv
          _ -> do
            -- Last resort: transitive resolution via pairEval.
            (cscope, gscope) <- getScope i lang
            case T.pairEval cscope gscope (VarU v) of
              Right (VarU v') -> return $ FV v (CV (unTVar v'))
              Right t' -> MM.throwSourcedError i $
                "No concrete" <+> pretty lang <+> "type for"
                <+> squotes (pretty v) <> ":"
                <+> "alias resolves to a composite type"
                <+> parens (pretty t')
                <> ", but a single type variable is required here."
              Left _ -> MM.throwSourcedError i $
                "No concrete" <+> pretty lang <+> "type for"
                <+> squotes (pretty v) <> "."
                <+> "Add a 'type" <+> pretty lang <+> "=>"
                <+> pretty v <+> "= \"...\"' declaration,"
                <+> "or import a module that provides one"
                <+> parens ("e.g. root-" <> pretty lang) <> "."
