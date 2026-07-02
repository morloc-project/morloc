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
  inferConcreteTypeStructural lang i gscope generalType concreteType

-- | Parallel structural walk over (general, concrete) that handles the
-- AppU/VarU mismatch case at any depth. Parameterised newtypes whose
-- per-language form is a non-templated VarU (e.g. @newtype IFile a =
-- UInt64@ + @type Cpp => IFile a = "uint64_t"@) cannot be woven by the
-- pure 'weave' because its catch-all calls 'evaluateStep' on the
-- general type, which treats newtypes as opaque and returns Nothing.
-- The fix is to retain the general args (recursively inferring their
-- concrete forms) so the resulting AppF preserves both shape and
-- downstream pattern matches (e.g. @open's IFile-head check in
-- Imperative.hs).
--
-- For everything else we delegate to the existing pure 'weave' via
-- 'inferConcreteTypeWeave', which still handles transparent aliases,
-- template-bearing per-language forms, records, optionals, and the
-- usual gscope-step fallbacks.
inferConcreteTypeStructural
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeStructural lang i gscope g c = case (g, c) of
  -- Pierce EffectU / OptionalU wrappers so the AppU/VarU intercept
  -- fires through them: @<IO> (IFile a)@, @?(IFile a)@, etc.
  (EffectU effs g', EffectU _ c') ->
    mkEffectF (resolveEffectSet effs)
      <$> inferConcreteTypeStructural lang i gscope g' c'
  (OptionalU g', OptionalU c') ->
    OptionalF <$> inferConcreteTypeStructural lang i gscope g' c'
  -- AppU general / VarU concrete: parameterised newtype/alias whose
  -- per-language form is a non-templated VarU. Recurse on each general
  -- arg via inferConcreteType (giving each its own pairEval pass) so
  -- nested types -- including further newtype-vs-VarU pairs -- get
  -- their own concrete forms.
  (AppU (VarU vG) ts, VarU (TV vC)) -> do
    argTfs <- mapM (inferConcreteType lang . Idx i . typeOf) ts
    return $ AppF (VarF (FV vG (CV vC))) argTfs
  -- AppU/AppU shortcut: weave the head pairwise, recurse on each
  -- (g, c) arg pair so a nested AppU/VarU on the arg side still
  -- picks up the intercept. The type-arity guard rejects phantom-Nat
  -- aliases like @FixedPair (n :: Nat) a = (a, a)@ where the general
  -- side has [NatLit, Type] but the concrete resolves through Tuple2
  -- to a 2-type template -- a naive zip would pair NatLit with a
  -- type slot, then 'partitionKindArgsF' would strip it at render
  -- time and crash macro expansion. Mismatches fall through to
  -- 'weave', which steps the alias and re-weaves on the body's head.
  (AppU (VarU vG) ts1, AppU (VarU (TV vC)) ts2)
    | length ts1 == length ts2
    , length (fst (partitionKindArgsU ts1))
        == length (fst (partitionKindArgsU ts2)) -> do
        argTfs <- zipWithM
          (inferConcreteTypeStructural lang i gscope) ts1 ts2
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  -- Everything else (VarU/VarU, FunU, NamU, Nat*, leaves, mismatched
  -- shapes) routes through the existing pure weave + scope-step fall
  -- backs.
  _ -> inferConcreteTypeWeave lang i gscope g c

inferConcreteTypeWeave
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeWeave lang i gscope generalType concreteType =
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
          --
          -- 'evalGeneralStep' wraps 'TE.evaluateStep' which returns
          -- @Just t@ even when @t == generalType@ (no progress). For a
          -- guarded recursive record like @record Tree where children
          -- :: [Tree]@ the bnd-protected NamU branch in
          -- 'generalTransformType' returns the same NamU as input, so a
          -- naive recursion here loops forever. Compare structurally
          -- before recursing.
          mayReducedGType <- evalGeneralStep i generalType
          case mayReducedGType of
            (Just reducedGType)
              | reducedGType /= generalType ->
                  inferConcreteType lang (Idx i (typeOf reducedGType))
            _ ->
              MM.throwSourcedError i $
                "Cannot infer concrete type for" <+> pretty generalType <> "\nCould not reduce type"

inferConcreteTypeUniversal :: Lang -> Type -> MorlocMonad TypeF
inferConcreteTypeUniversal lang t@(type2typeu -> generalType) = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  concreteType <- inferConcreteTypeUUniversal lang generalType
  inferConcreteTypeUniversalStructural lang gscopeUni t generalType concreteType

-- | Structural walk over (general, concrete) in universal scope. Mirrors
-- 'inferConcreteTypeStructural' but recurses via 'inferConcreteTypeUniversal'
-- so nested type parameters resolve against the universal typedef scope,
-- not the per-language one. Needed for parameterised newtypes like
-- @OStream (IFile [Int])@ where the general side is @AppU@ and the
-- concrete side collapses to a bare @VarU UInt64@ -- the pure @weave@
-- can't bridge that mismatch and its @evaluateStep@ fallback treats the
-- newtype as opaque.
inferConcreteTypeUniversalStructural
  :: Lang -> Scope -> Type -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeUniversalStructural lang gscopeUni t g c = case (g, c) of
  (EffectU effs g', EffectU _ c') ->
    mkEffectF (resolveEffectSet effs)
      <$> inferConcreteTypeUniversalStructural lang gscopeUni t g' c'
  (OptionalU g', OptionalU c') ->
    OptionalF <$> inferConcreteTypeUniversalStructural lang gscopeUni t g' c'
  (AppU (VarU vG) ts, VarU (TV vC)) -> do
    argTfs <- mapM (inferConcreteTypeUniversal lang . typeOf) ts
    return $ AppF (VarF (FV vG (CV vC))) argTfs
  (AppU (VarU vG) ts1, AppU (VarU (TV vC)) ts2)
    | length ts1 == length ts2
    , length (fst (partitionKindArgsU ts1))
        == length (fst (partitionKindArgsU ts2)) -> do
        argTfs <- zipWithM
          (inferConcreteTypeUniversalStructural lang gscopeUni t) ts1 ts2
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  _ ->
    case weave gscopeUni g c of
      (Right tf) -> return tf
      (Left _) -> case T.evaluateStep gscopeUni g of
        (Just reducedGType)
          | reducedGType /= g ->
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

-- | 'weave' fuses a general TypeU with its language-specific concrete
-- TypeU into a single TypeF. Purely structural -- no cycle detection
-- at the weave level (that would collapse the intermediate alias-
-- expansion node that 'makeSerialAST'' relies on to emit the
-- recursive-schema @&name@ declaration alongside the @^name@ back-
-- reference). Leak prevention for recursive aliases with no concrete-
-- language mapping happens at the C++ render boundary in
-- 'CppTranslator.hs', where every @VarF@/@AppF@/@RecF@ rule consults
-- cscope to distinguish legitimate user mappings from pairEval bnd-
-- protect leaks.
weave :: Scope -> TypeU -> TypeU -> Either MDoc TypeF
weave gscope = w
  where
    w (VarU v1) (VarU (TV v2)) = return $ VarF (FV v1 (CV v2))
    w (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM w ts1 ts2 <*> w t1 t2
    -- AppU vs AppU: weave heads, then args. If heads weave but arg lists
    -- have mismatched lengths (e.g. general @Pair Int@ has 1 arg while
    -- the concrete-side resolution expanded to @"tuple" [int, ?(...)]@
    -- with 2 args), fall through to the catch-all @evaluateStep@ retry
    -- on @t1@. This is the same generalization the catch-all already
    -- performs for type-level mismatches; we just need to opt the AppU
    -- branch into it on arg-length failure instead of failing outright.
    w t1@(AppU h1 ts1) t2@(AppU h2 ts2) =
      case (AppF <$> w h1 h2 <*> weaveArgs ts1 ts2) of
        r@(Right _) -> r
        Left _ -> wStep t1 t2
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
    w t1 t2 = wStep t1 t2

    -- Step the general type one level via @evaluateStep@ and retry.
    wStep t1 t2 = case T.evaluateStep gscope t1 of
      Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
      (Just t1') ->
        if t1 == t1'
          then Left ("failed to weave:" <> pretty t1 <+> "vs" <+> pretty t1')
          else w t1' t2

    -- Weave type arguments, handling Nat params that may be erased OR
    -- preserved in the concrete type. When the concrete head is also a Nat
    -- expression, consume it in lockstep; otherwise consume only the general
    -- arg (erased in concrete). Either way we emit a NatLitF placeholder.
    weaveArgs :: [TypeU] -> [TypeU] -> Either MDoc [TypeF]
    weaveArgs [] [] = Right []
    weaveArgs [] cs
      | all isKindTypeU cs = Right []  -- trailing kind args in concrete only
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

    -- Drop a leading kind-shaped concrete arg, if present.
    dropNatHead :: [TypeU] -> [TypeU]
    dropNatHead (c : cs) | isKindTypeU c = cs
    dropNatHead cs = cs

inferConcreteVar :: Lang -> Indexed TVar -> MorlocMonad FVar
inferConcreteVar lang t0@(Idx i v) = do
  MM.sayVVV $ "inferConcreteVar" <+> pretty lang <+> pretty t0
  localScope <- MM.getConcreteScope i lang
  globalScope <- MM.getConcreteUniversalScope lang
  case Map.lookup v localScope of
    (Just ((_, t, _, True, _) : _)) -> return $ FV v (CV . unTVar $ extractKey t)
    -- Non-terminal concrete alias: e.g. `type Cpp => Array a = List a`.
    -- Follow through the body's head (recursively) until a terminal entry
    -- is reached, then pair the *original* v with the resolved concrete
    -- name. This preserves the morloc-level identity (Array stays Array)
    -- while picking up the runtime concrete (std::vector here).
    (Just ((_, t, _, False, _) : _)) -> do
      FV _ cv <- inferConcreteVar lang (Idx i (extractKey t))
      return $ FV v cv
    _ -> case Map.lookup v globalScope of
      (Just ((_, t, _, True, _) : _)) -> do
        -- TODO fix this, the types should be in scope
        MM.sayVVV $ "WARNING: using global definition for v=" <> pretty v
        return $ FV v (CV . unTVar $ extractKey t)
      -- Same recursive resolution at the global scope level.
      (Just ((_, t, _, False, _) : _)) -> do
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
        let
          -- Guard against self-recursive lookup: if the body's
          -- extracted key resolves back to v (e.g. a record whose
          -- general definition is `record Container a where ...` --
          -- the body's NamU carries the same TVar `Container`), a
          -- naive recursion loops forever because each iteration
          -- looks the same TVar up in gscopeUni and gets the same
          -- body back. When the body's key is v itself, fall through
          -- to pairEval, which produces a comprehensible
          -- "No concrete <lang> type for <v>" error naming the
          -- missing instance.
          gscopeBody = case Map.lookup v gscopeUni of
            (Just ((_, body, _, _, _) : _)) | extractKey body /= v -> Just (extractKey body)
            _ -> Nothing
        case gscopeBody of
          Just bodyKey -> do
            FV _ cv <- inferConcreteVar lang (Idx i bodyKey)
            return $ FV v cv
          Nothing -> do
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
