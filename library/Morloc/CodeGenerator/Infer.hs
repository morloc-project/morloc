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
import qualified Data.Set as Set
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
  (cscope, gscope) <- getScope i lang
  case weave cscope gscope generalType concreteType of
    (Right tf) -> return tf
    (Left _) -> do
      gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
      cscopeUni <- CMS.gets stateUniversalConcreteTypedefs |>> fromMaybe Map.empty . Map.lookup lang
      case weave cscopeUni gscopeUni generalType concreteType of
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
  cscopeUni <- CMS.gets stateUniversalConcreteTypedefs |>> fromMaybe Map.empty . Map.lookup lang
  concreteType <- inferConcreteTypeUUniversal lang generalType
  case weave cscopeUni gscopeUni generalType concreteType of
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

weave :: Scope -> Scope -> TypeU -> TypeU -> Either MDoc TypeF
weave cscope gscope = w Set.empty
  where
    -- Cycle detection on the GENERAL side. When @pairEval@ left a
    -- bnd-protected back-reference (the inner @Pair@ of @type Pair a =
    -- (a, ?(Pair a))@), the general head TVar reappears inside its own
    -- body. Without this guard the original @w (VarU v1) (VarU (TV v2))@
    -- arm below would synthesize @CV v2@ from the morloc-side TVar and
    -- ship the morloc identifier into the concrete-name slot -- the
    -- silent leak that surfaced as `Pair`/`BTree`/`Rose` tokens in
    -- pool.cpp. Emit @RecF v (Just cv)@ when cscope holds an explicit
    -- concrete mapping, @RecF v Nothing@ otherwise. The @Maybe CVar@
    -- payload is the type-level encoding of "no concrete mapping
    -- available" -- statically-typed translators (C++) must error on
    -- the @Nothing@ constructor rather than synthesizing a CVar from
    -- @v@'s text. Dynamic translators (Python, R) emit no type names
    -- and never inspect the slot, so they handle recursive tuple
    -- aliases without complaint.
    w :: Set.Set TVar -> TypeU -> TypeU -> Either MDoc TypeF
    w ancestors (VarU v) _ | Set.member v ancestors = recRef v
    w ancestors (AppU (VarU v) _) _ | Set.member v ancestors = recRef v
    w _ (VarU v1) (VarU (TV v2)) = return $ VarF (FV v1 (CV v2))
    w a (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM (w a) ts1 ts2 <*> w a t1 t2
    -- AppU vs AppU: weave heads, then args. If heads weave but arg lists
    -- have mismatched lengths (e.g. general @Pair Int@ has 1 arg while
    -- the concrete-side resolution expanded to @"tuple" [int, ?(...)]@
    -- with 2 args), fall through to the catch-all @evaluateStep@ retry
    -- on @t1@. This is the same generalization the catch-all already
    -- performs for type-level mismatches; we just need to opt the AppU
    -- branch into it on arg-length failure instead of failing outright.
    w a t1@(AppU h1 ts1) t2@(AppU h2 ts2) =
      case (AppF <$> w a h1 h2 <*> weaveArgs a ts1 ts2) of
        r@(Right _) -> r
        Left _ -> wStep a t1 t2
    w a t1@(NamU o1 v1 ts1 rs1) t2@(NamU o2 v2 ts2 rs2)
      | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2 =
          -- Push BOTH the general and the concrete-renamed names. A self-
          -- recursive field can mention either form (e.g. @record Tree
          -- where children :: [Tree]@ paired with @record Cpp => Tree =
          -- "tree_t"@: the field's body still says @Tree@ but after
          -- pairEval the outer record is named @tree_t@). Pushing both
          -- guarantees the back-reference is caught no matter which name
          -- appears in the concrete-side body.
          let a' = Set.insert v1 (Set.insert v2 a)
          in NamF o1 (FV v1 (CV (unTVar v2)))
               <$> zipWithM (w a') ts1 ts2
               <*> zipWithM (\(_, t1') (k2', t2') -> (,) k2' <$> w a' t1' t2') rs1 rs2
      | otherwise = Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <+> "\n  t2:" <+> pretty t2
    w a (EffectU effs t1) (EffectU _ t2) = mkEffectF (resolveEffectSet effs) <$> w a t1 t2
    w a (OptionalU t1) (OptionalU t2) = OptionalF <$> w a t1 t2
    w _ (NatLitU n) (NatLitU _) = return $ NatLitF n
    w _ (NatLitU n) _ = return $ NatLitF n  -- Nat params may be erased in concrete type
    w _ NatVoidU _ = return NatVoidF  -- Erased phantom Nat slot
    w _ (NatAddU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatMulU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatSubU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatDivU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatVarU _) _ = return NatVoidF  -- Nat variable erased in concrete type
    w a (LabeledU _ t1) t2 = w a t1 t2
    w _ (ForallU v (VarU v')) _ | v == v' = return NatVoidF  -- Unresolved variable (UnkT pattern)
    w a t1 t2 = wStep a t1 t2

    -- Step the general type one level via @evaluateStep@ and retry. When
    -- the head is a named alias, push it to the ancestor set so any
    -- recursive back-reference inside the unrolled body is caught
    -- structurally on the recursive @w@ call (rather than silently
    -- passing through as a VarU whose TVar text would be cast into the
    -- CV slot by the @VarU vs VarU@ arm above).
    wStep a t1 t2 = case T.evaluateStep gscope t1 of
      Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
      (Just t1') ->
        if t1 == t1'
          then Left ("failed to weave:" <> pretty t1 <+> "vs" <+> pretty t1')
          else
            let a' = case t1 of
                  VarU v -> Set.insert v a
                  AppU (VarU v) _ -> Set.insert v a
                  _ -> a
            in w a' t1' t2

    -- Weave type arguments, handling Nat params that may be erased OR
    -- preserved in the concrete type. When the concrete head is also a Nat
    -- expression, consume it in lockstep; otherwise consume only the general
    -- arg (erased in concrete). Either way we emit a NatLitF placeholder.
    weaveArgs :: Set.Set TVar -> [TypeU] -> [TypeU] -> Either MDoc [TypeF]
    weaveArgs _ [] [] = Right []
    weaveArgs _ [] cs
      | all isNatLikeU cs = Right []  -- trailing Nat args in concrete only
      | otherwise = Left "concrete type has more non-Nat args than general type in weave"
    weaveArgs a (NatLitU n : gs) cs = (NatLitF n :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (NatVoidU : gs) cs = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (NatAddU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (NatMulU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (NatSubU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (NatDivU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    -- Unresolved nat dimension variable (opaque output dims): treat as erased
    weaveArgs a (NatVarU _ : gs) cs = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (ForallU v (VarU v') : gs) cs | v == v' = (NatVoidF :) <$> weaveArgs a gs (dropNatHead cs)
    weaveArgs a (g:gs) (c:cs) = (:) <$> w a g c <*> weaveArgs a gs cs
    weaveArgs _ _ [] = Left "general type has more non-Nat args than concrete type in weave"

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

    -- Back-reference resolution. The morloc-side TVar @v@ is in the
    -- ancestor set, so we're inside its own alias body and emitting a
    -- recursion cut. Emit @RecF v (Just cv)@ when the user supplied
    -- an explicit concrete mapping (e.g. @record Cpp => Tree =
    -- "tree_t"@, @type Cpp => Pair a = "pair_t<$1>" a@). Emit
    -- @RecF v Nothing@ when no cscope entry exists -- the Maybe
    -- encodes "no concrete-language name available" at the type
    -- level so the morloc TVar text cannot silently masquerade as a
    -- concrete name downstream. Dynamic translators (Python, R) emit
    -- no type names and never inspect the slot; statically-typed
    -- translators (C++) must pattern-match on @Nothing@ and raise a
    -- diagnostic rather than letting an undeclared identifier reach
    -- the generated pool.
    recRef :: TVar -> Either MDoc TypeF
    recRef v = case Map.lookup v cscope of
      Just (entry : _) | Just cv <- extractCV entry -> Right $ RecF v (Just cv)
      _ -> Right $ RecF v Nothing

    extractCV :: ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) -> Maybe CVar
    extractCV (_, t, _, _) = case t of
      NamU _ (TV cv) _ _ -> Just (CV cv)
      AppU (VarU (TV cv)) _ -> Just (CV cv)
      VarU (TV cv) -> Just (CV cv)
      _ -> Nothing

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
            (Just ((_, body, _, _) : _)) | extractKey body /= v -> Just (extractKey body)
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
