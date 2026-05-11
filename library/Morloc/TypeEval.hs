{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.TypeEval
Description : Expand type aliases and reduce type applications
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Evaluates type expressions by expanding type aliases from the scope
(general and concrete), applying type arguments, and reducing applications.
Used by the typechecker and code generator to resolve user-defined types
to their canonical forms.
-}
module Morloc.TypeEval
  ( evaluateType
  , transformType
  , evaluateStep
  , pairEval
  , reduceType
  , reduceTypeLeaves
  , expandHeadOnly
  , expandLeavesOnce
  ) where

import qualified Data.Set as Set
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim
import Morloc.Namespace.State (MorlocError (..))
import Morloc.Namespace.Type

-- Evaluate a type expression with both the concrete and general scopes
--
-- This function does not know the concrete language, the parent sets that.
--
-- First try to resolve an expression with the concrete scope
-- If this fails, resolve one step with the general scope.
pairEval ::
  Scope -> -- concrete scope
  Scope -> -- general scope
  TypeU ->
  Either MorlocError TypeU
pairEval cscope gscope =
  -- transform the concrete type until an unresolvable node is reached
  generalTransformType Set.empty id resolveGen cscope
  where
    -- resolve by attempting to evaluate one step as in the general scope
    resolveGen f bnd t =
      case generalTransformType bnd (\_ _ -> return) resolveFail gscope t of
        (Right t') ->
          if t' /= t
            -- if general resolution succeeds, continue evaluation with the concrete scope
            then f bnd t'
            -- if it fails, return to the concrete scope to handle failure without
            -- general resolution option
            else generalTransformType Set.empty id resolveFail cscope t
        -- if no resolution is possible, propagate the error
        e -> e

evaluateStep :: Scope -> TypeU -> Maybe TypeU
evaluateStep scope t0 =
  case generalTransformType Set.empty (\_ _ -> return) resolveFail scope t0 of
    (Left _) -> Nothing
    (Right t) -> Just t

-- | evaluate a type exactly one step, return nothing if no evaluation is possible
reduceType :: Scope -> TypeU -> Maybe TypeU
reduceType scope t0 =
  case evaluateStep scope t0 of
    (Just t1) -> if t1 == t0 then Nothing else Just t1
    Nothing -> Nothing

-- | Reduce aliases one step inside compound types. When the top-level
-- constructor is not an alias (e.g., FunU), reduceType returns Nothing.
-- This function descends into compound types and reduces each leaf alias
-- one step via reduceType. Returns Nothing if no leaf changed.
reduceTypeLeaves :: Scope -> TypeU -> Maybe TypeU
reduceTypeLeaves scope t0 =
  let (t1, changed) = go t0
  in if changed then Just t1 else Nothing
  where
    go t = case reduceType scope t of
      Just t' -> (t', True)
      Nothing -> descend t

    descend (FunU ts t) =
      let (ts', cs1) = unzip (map go ts)
          (t', c2) = go t
      in (FunU ts' t', or (c2 : cs1))
    descend (AppU f ts) =
      let (f', c1) = go f
          (ts', cs) = unzip (map go ts)
      in (AppU f' ts', or (c1 : cs))
    descend (NamU o n ps rs) =
      let (ps', cs1) = unzip (map go ps)
          (vs', cs2) = unzip (map (go . snd) rs)
      in (NamU o n ps' (zip (map fst rs) vs'), or (cs1 ++ cs2))
    descend (EffectU effs t) =
      let (t', c) = go t in (EffectU effs t', c)
    descend (OptionalU t) =
      let (t', c) = go t in (OptionalU t', c)
    descend t = (t, False)

-- evaluate a type until terminal functions called, fail if termini are not reached
transformType :: Scope -> TypeU -> Either MorlocError TypeU
transformType = generalTransformType Set.empty id resolveFail

-- evaluate a type as far as possible given the type functions in scope
evaluateType :: Scope -> TypeU -> Either MorlocError TypeU
evaluateType = generalTransformType Set.empty id resolveIgnore

resolveIgnore ::
  (Set.Set TVar -> TypeU -> Either MorlocError TypeU) ->
  Set.Set TVar ->
  TypeU ->
  Either MorlocError TypeU
resolveIgnore f bnd (AppU (VarU v) ts) = AppU (VarU v) <$> mapM (f bnd) ts
resolveIgnore _ _ t@(VarU _) = return t
resolveIgnore _ _ _ = MM.throwSystemError "Compiler bug (__FILE__:__LINE__): Reached unexpected branch"

resolveFail ::
  (Set.Set TVar -> TypeU -> Either MorlocError TypeU) ->
  Set.Set TVar ->
  TypeU ->
  Either MorlocError TypeU
resolveFail _ _ (AppU (VarU v) _) =
  MM.throwSystemError $
    "Could not resolve type applied variable" <+> squotes (pretty v)
      <> ". You may be missing a language-specific type definition."
resolveFail _ _ (VarU v) =
  MM.throwSystemError $
    "Could not resolve type for variable" <+> squotes (pretty v)
      <> ". You may be missing a language-specific type definition."
resolveFail _ _ _ = MM.throwSystemError "Compiler bug (__FILE__:__LINE__): Reached unexpected branch"

generalTransformType ::
  Set.Set TVar ->
  ( (Set.Set TVar -> TypeU -> Either MorlocError TypeU) ->
    Set.Set TVar ->
    TypeU ->
    Either MorlocError TypeU
  ) ->
  ( (Set.Set TVar -> TypeU -> Either MorlocError TypeU) ->
    Set.Set TVar ->
    TypeU ->
    Either MorlocError TypeU
  ) ->
  Scope -> -- may be general or concrete scope
  TypeU ->
  Either MorlocError TypeU
generalTransformType bnd0 recurse' resolve' scope = f bnd0
  where
    recurse = recurse' f
    resolve = resolve' recurse

    f :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
    f bnd (ExistU v (ps, pc) (rs, rc)) = do
      ps' <- mapM (recurse bnd) ps
      rs' <- mapM (\(k, v') -> (,) k <$> recurse bnd v') rs
      return $ ExistU v (ps', pc) (rs', rc)
    f bnd (FunU ts t) = FunU <$> mapM (recurse bnd) ts <*> recurse bnd t
    f bnd (NamU o n ps rs) = do
      (n', o') <- case Map.lookup n scope of
        -- If the record type itself is aliased, substitute the name and record form
        (Just [(_, NamU o'' n'' _ _, _, _)]) -> return (n'', o'')
        -- Otherwise, keep the record name and form and recurse only into children
        _ -> return (n, o)
      ts' <- mapM (recurse bnd . snd) rs
      ps' <- mapM (recurse bnd) ps
      return $ NamU o' n' ps' (zip (map fst rs) ts')
    f bnd t0@(AppU (VarU v) ts)
      -- Handle generic case:
      --   type Cpp => A a b = "map<$1,$2>" a b
      --   foo Cpp :: A D [B] -> X
      --   -----------------------------------
      --   foo :: "map<$1,$2>" D [B] -> X
      --
      --   type Foo a = (a, A)
      --   f :: Foo Int -> B
      --   -----------------
      --   f :: (Int, A) -> B
      | Set.member v bnd = AppU (VarU v) <$> mapM (recurse bnd) ts
      -- Handle specialization, e.g.
      --   type Py => List Int64 = "np.ndarray" "int64"
      | otherwise =
          case Map.lookup v scope of
            (Just ts') -> do
              mergedAliases <- foldlM (mergeAliases ts) Nothing (map Just ts') |>> fmap (renameTypedefs bnd)
              case mergedAliases of
                (Just (vs, newType, _, isTerminal)) -> case isTerminal of
                  True -> terminate bnd $ foldr parsub newType (zip vs ts)
                  -- substitute the head term and re-evaluate
                  False -> recurse bnd $ foldr parsub newType (zip vs ts)
                Nothing ->
                  MM.throwSystemError $
                    "No matching alias found for" <+> pretty t0
                      <> "\n  Available aliases have"
                      <+> pretty (length ts') <+> "entries, none match the given arguments"
            _ -> resolve bnd t0
    -- t may be existential
    f bnd (AppU t ts) = AppU <$> recurse bnd t <*> mapM (recurse bnd) ts
    -- type Foo = A
    -- f :: Foo -> B
    -- -----------------
    -- f :: A -> B
    f bnd t0@(VarU v)
      | Set.member v bnd = return t0
      | otherwise = case Map.lookup v scope of
          (Just []) -> return t0
          (Just ts1) -> do
            -- new parameters may be added on the right that are not on the left
            mergedAliases <- foldlM (mergeAliases []) Nothing (map Just ts1)
            case mergedAliases of
              (Just (_, t2, _, isTerminal)) ->
                if isTerminal
                  then terminate bnd t2
                  else recurse bnd t2
              Nothing ->
                MM.throwSystemError $
                  "No matching alias found for" <+> pretty t0
                    <> "\n  Available aliases have"
                    <+> pretty (length ts1) <+> "entries, none match"
          Nothing -> resolve bnd t0
    f bnd (ForallU v t) = ForallU v <$> recurse (Set.insert v bnd) t
    f bnd (EffectU effs t) = EffectU effs <$> recurse bnd t
    f bnd (OptionalU t) = OptionalU <$> recurse bnd t
    f _ t@(NatVarU _) = return t  -- nat vars are not type aliases
    f _ t@NatVoidU = return t
    f _ t@(StrVarU _) = return t
    f _ t@StrVoidU = return t
    f _ t@(RecVarU _) = return t
    f _ t@RecVoidU = return t
    f _ t@(ListVarU _) = return t
    f _ t@ListVoidU = return t
    f _ t@(SetVarU _) = return t
    f _ t@SetVoidU = return t
    -- Unified carriers: uniform recursion across all operators and literal
    -- payloads.
    f bnd (OpU op args) = OpU op <$> mapM (recurse bnd) args
    f _ t@(LitU (LNat _)) = return t
    f _ t@(LitU (LStr _)) = return t
    f bnd (LitU (LRec fs)) = LitU . LRec <$> mapM (\(k, v) -> fmap ((,) k) (recurse bnd v)) fs
    f bnd (LitU (LList es)) = LitU . LList <$> mapM (recurse bnd) es
    f bnd (LitU (LSet es)) = LitU . LSet <$> mapM (recurse bnd) es
    f bnd (LabeledU n t) = LabeledU n <$> recurse bnd t

    terminate :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
    terminate bnd (ExistU v (ts, tc) (rs, rc)) = do
      ts' <- mapM (recurse bnd) ts
      rs' <- mapM (secondM (recurse bnd)) rs
      return $ ExistU v (ts', tc) (rs', rc)
    terminate bnd (FunU ts t) = FunU <$> mapM (recurse bnd) ts <*> recurse bnd t
    terminate bnd (ForallU v t) = ForallU v <$> recurse (Set.insert v bnd) t
    terminate bnd (AppU t ts) = AppU t <$> mapM (recurse bnd) ts
    terminate bnd (NamU o v ts rs) = NamU o v <$> mapM (recurse bnd) ts <*> mapM (secondM (recurse bnd)) rs
    terminate _ (VarU v) = return (VarU v)
    terminate _ t@(NatVarU _) = return t
    terminate bnd (EffectU effs t) = EffectU effs <$> recurse bnd t
    terminate bnd (OptionalU t) = OptionalU <$> recurse bnd t
    terminate _ t@NatVoidU = return t
    terminate _ t@(StrVarU _) = return t
    terminate _ t@StrVoidU = return t
    terminate _ t@(RecVarU _) = return t
    terminate _ t@RecVoidU = return t
    terminate _ t@(ListVarU _) = return t
    terminate _ t@ListVoidU = return t
    terminate _ t@(SetVarU _) = return t
    terminate _ t@SetVoidU = return t
    -- Unified carriers: uniform recursion.
    terminate bnd (OpU op args) = OpU op <$> mapM (recurse bnd) args
    terminate _ t@(LitU (LNat _)) = return t
    terminate _ t@(LitU (LStr _)) = return t
    terminate bnd (LitU (LRec fs)) = LitU . LRec <$> mapM (\(k, v) -> fmap ((,) k) (recurse bnd v)) fs
    terminate bnd (LitU (LList es)) = LitU . LList <$> mapM (recurse bnd) es
    terminate bnd (LitU (LSet es)) = LitU . LSet <$> mapM (recurse bnd) es
    terminate bnd (LabeledU n t) = LabeledU n <$> recurse bnd t

    renameTypedefs ::
      Set.Set TVar -> ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) -> ([TVar], TypeU, ArgDoc, Bool)
    renameTypedefs _ ([], t, d, isTerminal) = ([], t, d, isTerminal)
    renameTypedefs bnd (Left (v@(TV x), _) : vs, t, d, isTerminal)
      | Set.member v bnd =
          let (vs', t', d', isTerminal') = renameTypedefs bnd (vs, t, d, isTerminal)
              v' =
                head
                  [ x' | x' <- [TV (MT.show' i <> x) | i <- [(0 :: Int) ..]], not (Set.member x' bnd), x' `notElem` vs'
                  ]
              t'' = substituteTVar v (VarU v') t'
           in (v' : vs', t'', d', isTerminal')
      | otherwise =
          let (vs', t', d', isTerminal') = renameTypedefs bnd (vs, t, d, isTerminal)
           in (v : vs', t', d', isTerminal')
    renameTypedefs bnd (Right _ : vs, t, d, isTerminal) =
      renameTypedefs bnd (vs, t, d, isTerminal)

    -- When a type alias is imported from two places, this function reconciles them, if possible
    mergeAliases ::
      [TypeU] ->
      Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) ->
      Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) ->
      Either MorlocError (Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool))
    mergeAliases _ Nothing Nothing = Right Nothing
    mergeAliases tsMain Nothing (Just b)
      | checkAlias tsMain b = Right (Just b)
      | otherwise = Right Nothing
    mergeAliases tsMain (Just a) Nothing
      | checkAlias tsMain a = Right (Just a)
      | otherwise = Right Nothing
    -- TODO: should the docstring args be considered here?
    mergeAliases tsMain (Just a@(ts1, t1, _, isTerminal1)) (Just b@(ts2, t2, _, isTerminal2))
      -- if both are invalid, return nothing
      | not aIsValid && not bIsValid = Right Nothing
      -- if one is valid and the other isn't, return the valid one
      | aIsValid && not bIsValid = Right (Just a)
      | not aIsValid && bIsValid = Right (Just b)
      -- if they are both valid AND they are identical AND there is no specialization, return the first
      | -- the return types are the same
        isSubtypeOf t1 t2
          && isSubtypeOf t2 t1
          -- there is no specialization
          && nonspecialized
          -- the return type is concrete, not an alias for something else
          && isTerminal1 == isTerminal2 =
          return (Just a)
      -- handle specialization
      | not nonspecialized = return $ selectSpecialization a b
      | otherwise =
          MM.throwSystemError $
            "Cannot merge conflicting type aliases:"
              <> "\n  t1:" <+> pretty t1
              <> "\n  t2:" <+> pretty t2
      where
        aIsValid = checkAlias tsMain a
        bIsValid = checkAlias tsMain b
        -- True if all parameters in both aliases are generic
        nonspecialized =
          all
            (\(x, y) -> either (\_ -> either (const True) (const False) y) (const False) x)
            (zip ts1 ts2)

    selectSpecialization ::
      ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) ->
      ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) ->
      Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool)
    selectSpecialization a@(aps0, _, _, _) b@(bps0, _, _, _) = g aps0 bps0
      where
        g [] _ = Just a
        g _ [] = Just b
        g ((Right _) : _) ((Left _) : _) = Just a
        g ((Left _) : _) ((Right _) : _) = Just b
        g ((Left _) : aps) ((Left _) : bps) = g aps bps
        g ((Right ta) : aps) ((Right tb) : bps)
          | isSubtypeOf ta tb && isSubtypeOf tb ta = g aps bps
          | isSubtypeOf ta tb && not (isSubtypeOf tb ta) = Just b
          | not (isSubtypeOf ta tb) && isSubtypeOf tb ta = Just a
          | otherwise = Nothing

    checkAlias ::
      [TypeU] ->
      ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool) ->
      Bool
    checkAlias ts1 (ts2, _, _, _) =
      length ts1 == length ts2
        && all (\(x, y) -> either (const True) (\ytype -> isSubtypeOf ytype x) y) (zip ts1 ts2)

-- Replace a type variable with an expression. For example:
-- parsub ("a", "Int") -> "Map a b" -> "Map Int b"
parsub :: (TVar, TypeU) -> TypeU -> TypeU
parsub (v, t2) t1@(VarU v0)
  | v0 == v = t2 -- substitute
  | otherwise = t1 -- keep the original
parsub _ t@(NatVarU _) = t
parsub pair (ExistU t (ts, tc) (rs, rc)) = ExistU t (map (parsub pair) ts, tc) (zip (map fst rs) (map (parsub pair . snd) rs), rc)
parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
parsub pair (FunU ts t) = FunU (map (parsub pair) ts) (parsub pair t)
parsub pair (AppU t ts) = AppU (parsub pair t) (map (parsub pair) ts)
parsub pair (NamU o n ps rs) = NamU o n (map (parsub pair) ps) [(k', parsub pair t) | (k', t) <- rs]
parsub pair (EffectU effs t) = EffectU effs (parsub pair t)
parsub pair (OptionalU t) = OptionalU (parsub pair t)
parsub _ t@NatVoidU = t
parsub _ t@(StrVarU _) = t
parsub _ t@StrVoidU = t
parsub _ t@(RecVarU _) = t
parsub _ t@RecVoidU = t
parsub _ t@(ListVarU _) = t
parsub _ t@ListVoidU = t
parsub _ t@(SetVarU _) = t
parsub _ t@SetVoidU = t
-- Unified carriers: uniform recursion across all operators and literal payloads.
parsub pair (OpU op args) = OpU op (map (parsub pair) args)
parsub _ t@(LitU (LNat _)) = t
parsub _ t@(LitU (LStr _)) = t
parsub pair (LitU (LRec fs)) = LitU (LRec [(k, parsub pair v) | (k, v) <- fs])
parsub pair (LitU (LList es)) = LitU (LList (map (parsub pair) es))
parsub pair (LitU (LSet es)) = LitU (LSet (map (parsub pair) es))
parsub pair (LabeledU n t) = LabeledU n (parsub pair t)

-- | Expand the outermost type alias one step. Substitutes the alias body's
-- parameters with the actual args, but does NOT recurse into the args. This
-- preserves inner-element types when the outer alias changes the root type
-- (e.g. expanding Vector -> List without reducing Int32 -> Int).
--
-- When the call-site has fewer args than the alias has params, we attempt
-- a kind-based realignment: drop Nat-kinded params (filling them with the
-- 'NatVoidU' phantom-Nat sentinel) so the remaining type-kinded params line
-- up with the supplied args. This matters for list-literal lowering, which
-- strips Nat phantom args before we get here -- e.g.
-- `[1.0,2.0,3.0,4.0] :: Vector 4 Real` arrives as `AppU Vector [Real]`
-- (1 arg) while the alias is `Vector n a = List a` (2 params). Substituting
-- only `a -> Real` still produces `List Real`.
expandHeadOnly :: Scope -> TypeU -> Maybe TypeU
expandHeadOnly scope (AppU (VarU v) args) =
  case Map.lookup v scope of
    (Just ((paramKinds, body, _, _) : _))
      | length paramKinds == length args ->
          let params = [p | Left (p, _) <- paramKinds]
          in if length params == length args
             then Just $ foldr parsub body (zip params args)
             else Nothing
      | length args < length paramKinds ->
          -- Kind-based realignment: bind type-kinded params from `args`
          -- in order, fill Nat-kinded params with NatVoidU sentinels.
          let typeParams = [p | Left (p, KindType) <- paramKinds]
              natParams  = [p | Left (p, KindNat)  <- paramKinds]
          in if length typeParams == length args
             then Just $ foldr parsub body
                    (zip typeParams args
                      ++ [(p, NatVoidU) | p <- natParams])
             else Nothing
      | otherwise -> Nothing
    _ -> Nothing
expandHeadOnly scope (VarU v) =
  case Map.lookup v scope of
    (Just (([], body, _, _) : _)) -> Just body
    _ -> Nothing
expandHeadOnly _ _ = Nothing

-- | Like reduceTypeLeaves, but uses one-step head substitution
-- (expandHeadOnly) at each position rather than full evaluateStep. This is
-- crucial when an alias body's outer head is itself abstract (e.g.
-- `type Vector n a = List a` where List is abstract): full evaluation
-- recurses into the substituted body and `resolveFail`s on the abstract
-- head, returning Nothing -- masking the fact that ONE step of
-- substitution succeeded. expandLeavesOnce performs the substitution
-- and stops, returning the partially-reduced type.
--
-- Descends through ForallU/EffectU/OptionalU wrappers so that quantified
-- instance method types reduce too.
expandLeavesOnce :: Scope -> TypeU -> Maybe TypeU
expandLeavesOnce scope t0 =
  let (t1, changed) = go t0
  in if changed then Just t1 else Nothing
  where
    go t = case expandHeadOnly scope t of
      Just t' -> (t', True)
      Nothing -> descend t

    descend (ForallU v t) =
      let (t', c) = go t in (ForallU v t', c)
    descend (FunU ts t) =
      let (ts', cs1) = unzip (map go ts)
          (t', c2) = go t
      in (FunU ts' t', or (c2 : cs1))
    descend (AppU f ts) =
      let (f', c1) = go f
          (ts', cs) = unzip (map go ts)
      in (AppU f' ts', or (c1 : cs))
    descend (NamU o n ps rs) =
      let (ps', cs1) = unzip (map go ps)
          (vs', cs2) = unzip (map (go . snd) rs)
      in (NamU o n ps' (zip (map fst rs) vs'), or (cs1 ++ cs2))
    descend (EffectU effs t) =
      let (t', c) = go t in (EffectU effs t', c)
    descend (OptionalU t) =
      let (t', c) = go t in (OptionalU t', c)
    descend t = (t, False)
