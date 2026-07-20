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
  , expandWireParent
  , wireParentRoot
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
            -- If general resolution succeeds, continue evaluation with
            -- the concrete scope. Add any just-expanded alias name to
            -- bnd before recursing: the gscope walk's local
            -- self-recursion guard preserves back-references in the
            -- body as-is, and the subsequent cscope walk needs to
            -- leave them alone too. Without this, a recursive type
            -- alias like @type Pair a = (a, ?(Pair a))@ loops between
            -- gscope (which expands and protects via bnd) and cscope
            -- (which has no Pair entry, falls back to resolveGen,
            -- which re-expands).
            then f (bumpBnd t bnd) t'
            -- if it fails, return to the concrete scope to handle failure without
            -- general resolution option
            else generalTransformType Set.empty id resolveFail cscope t
        -- gscope's general walk failed (typically because the head is
        -- a non-NamU newtype, which 'generalTransformType' treats as
        -- nominally opaque). For per-language form derivation, a
        -- newtype without an explicit @type Lang => N = "..."@
        -- override should fall through to its body's per-language
        -- form (e.g. @newtype Filename = Str@ inherits Str's "str"
        -- mapping). Try one-step body expansion; if that succeeds,
        -- continue the cscope walk on the body.
        Left _ ->
          case expandNewtypeBodyOneStep gscope t of
            Just t' -> f (bumpBnd t bnd) t'
            Nothing -> generalTransformType Set.empty id resolveFail cscope t

    -- One-step body expansion for non-NamU newtypes. Returns Nothing
    -- for everything else (NamU newtypes are handled by the main walk
    -- via 'generalTransformType'; primitives and unknowns leave the
    -- type alone).
    expandNewtypeBodyOneStep :: Scope -> TypeU -> Maybe TypeU
    expandNewtypeBodyOneStep scope (VarU v) = case Map.lookup v scope of
      Just (([], body, _, _, TypedefNewtype) : _) -> case body of
        NamU{} -> Nothing
        _      -> Just body
      _ -> Nothing
    expandNewtypeBodyOneStep scope (AppU (VarU v) ts) = case Map.lookup v scope of
      Just ((paramKinds, body, _, _, TypedefNewtype) : _) -> case body of
        NamU{} -> Nothing
        _
          | length paramKinds == length ts ->
              let params = [p | Left (p, _) <- paramKinds]
              in if length params == length ts
                   then Just (foldr parsub body (zip params ts))
                   else Nothing
          | otherwise -> Nothing
      _ -> Nothing
    expandNewtypeBodyOneStep _ _ = Nothing

    bumpBnd :: TypeU -> Set.Set TVar -> Set.Set TVar
    bumpBnd (AppU (VarU v) _) bnd = Set.insert v bnd
    bumpBnd (VarU v) bnd = Set.insert v bnd
    bumpBnd _ bnd = bnd

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
--
-- A bound set tracks NamU record names we are currently inside. When
-- 'go' encounters a @VarU n@ with @n@ in the bound set, it leaves the
-- variable alone instead of letting 'reduceType' re-expand it. This is
-- the same termination trick that protects 'generalTransformType' on
-- recursive records; without it, repeated outer calls (e.g. from
-- Realize.handleMany) keep peeling another level off the recursion
-- and the compiler loops forever.
reduceTypeLeaves :: Scope -> TypeU -> Maybe TypeU
reduceTypeLeaves scope t0 =
  let (t1, changed) = go Set.empty t0
  in if changed then Just t1 else Nothing
  where
    go :: Set.Set TVar -> TypeU -> (TypeU, Bool)
    go bnd t@(VarU v)
      | Set.member v bnd = (t, False)
    go bnd t@(AppU (VarU v) _)
      | Set.member v bnd = descend bnd t
    go bnd t = case reduceType scope t of
      Just t' -> (t', True)
      Nothing -> descend bnd t

    descend bnd (FunU ts t) =
      let (ts', cs1) = unzip (map (go bnd) ts)
          (t', c2) = go bnd t
      in (FunU ts' t', or (c2 : cs1))
    -- Do NOT recurse into the head of an AppU. A partial application
    -- (the bare head with no args) would otherwise be passed to
    -- 'reduceType', which expands a parameterized alias by substituting
    -- its (now-absent) arguments. For a recursive alias like @List@
    -- whose body is @AppU (VarU List) [a]@, the result is a strictly
    -- larger AppU containing another bare @VarU List@, and the outer
    -- driver (Realize.handleMany) recurses on the new structure -- the
    -- compiler loops forever on guarded recursive records.
    descend bnd (AppU f ts) =
      let (ts', cs) = unzip (map (go bnd) ts)
      in (AppU f ts', or cs)
    descend bnd (NamU o n ps rs) =
      let bnd' = Set.insert n bnd
          (ps', cs1) = unzip (map (go bnd') ps)
          (vs', cs2) = unzip (map (go bnd' . snd) rs)
      in (NamU o n ps' (zip (map fst rs) vs'), or (cs1 ++ cs2))
    descend bnd (EffectU effs t) =
      let (t', c) = go bnd t in (EffectU effs t', c)
    descend bnd (OptionalU t) =
      let (t', c) = go bnd t in (OptionalU t', c)
    descend _ t = (t, False)

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
resolveIgnore _ _ _ = MM.throwCompilerBug "resolveIgnore reached an unexpected branch"

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
resolveFail _ _ _ = MM.throwCompilerBug "resolveFail reached an unexpected branch"

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
        (Just [(_, NamU o'' n'' _ _, _, _, _)]) -> return (n'', o'')
        -- Otherwise, keep the record name and form and recurse only into children
        _ -> return (n, o)
      -- Bind BOTH the original and the substituted name inside the
      -- record body so a recursive field referencing either form is
      -- left alone. A field like @VarU n@ (original) or @VarU n'@
      -- (concrete alias) would otherwise be looked up in scope and
      -- re-expanded into the same NamU forever. Add both: when @n@ and
      -- @n'@ differ (e.g. @record Tree where children :: [Tree]@ with a
      -- concrete @record Py => Tree = "dict"@ mapping), the children
      -- field still mentions the original @Tree@ name even though the
      -- outer NamU is now named @dict@. See
      -- plans/in-morloc-recursive-fields-* for context.
      let bnd' = Set.insert n' (Set.insert n bnd)
      ts' <- mapM (recurse bnd' . snd) rs
      ps' <- mapM (recurse bnd') ps
      return $ NamU o' n' ps' (zip (map fst rs) ts')
    -- Flatten nested AppU heads first. Substituting a partial-applied
    -- class head into a method's @f a@ signature produces @AppU (AppU H
    -- [n]) [a]@; the parser-produced canonical shape is @AppU H [n,
    -- a]@. Normalising here lets the @AppU (VarU v) ts@ branch below
    -- match either input shape.
    f bnd (AppU (AppU h innerArgs) outerArgs) = f bnd (AppU h (innerArgs ++ outerArgs))
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
                -- @v@ is added to bnd only when the substituted body is
                -- a record (NamU): records are the one alias shape that
                -- can be self-recursive via fields, and the bnd guard
                -- short-circuits those self-references during the
                -- walk. Non-record aliases (e.g. @type List a =
                -- "vector<$1>" a@) must NOT add @v@ to bnd, otherwise a
                -- nested use of the same alias in an argument position
                -- (@List (List Real)@) hits the bnd guard at the inner
                -- level and is left unevaluated, leaking the morloc
                -- general name into generated code.
                (Just (vs, newType, _, isTerminal, kind)) ->
                  let bnd' = if isSelfRecursive v newType then Set.insert v bnd else bnd
                      substituted = foldr parsub newType (zip vs ts)
                  in case (kind, isTerminal) of
                       -- Newtype and primitive are nominal for typeclass
                       -- instance lookup, but a record newtype's per-language
                       -- form is its structural NamU body (dict / struct /
                       -- named-list etc.). Treat NamU-bodied newtypes
                       -- transparently here so cscope/pairEval can reach the
                       -- record's structural representation without requiring
                       -- an explicit per-language form for every record.
                       -- Non-record newtypes remain opaque; schema generation
                       -- walks through them via 'expandWireParent' (see
                       -- Serial.hs).
                       (TypedefNewtype, _) -> case substituted of
                         NamU{} -> recurse bnd' substituted
                         _ -> resolve bnd t0
                       (TypedefPrimitive, _) -> resolve bnd t0
                       (TypedefAlias, True) -> terminate bnd' substituted
                       (TypedefAlias, False) -> recurse bnd' substituted
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
              -- Add the looked-up key @v@ to bnd so a recursive
              -- back-reference inside the substituted body (e.g. a
              -- @record LL where tail :: ?LL@ whose concrete alias
              -- @record Py => LL = "dict"@ substitutes name but
              -- preserves the body) does not loop. Without this, the
              -- inner @VarU v@ would look up @v@ again and recurse
              -- forever. The NamU branch already adds both original
              -- and substituted record names; this covers the bare
              -- @VarU v@ entry point.
              -- Same rule as the AppU branch: bnd extension is for
              -- self-recursive aliases only. A plain non-recursive
              -- alias like @type Foo = Bar@ neither needs nor wants
              -- @Foo@ in bnd. See `isSelfRecursive` for the structural
              -- check.
              (Just (_, t2, _, isTerminal, kind)) ->
                let bnd' = if isSelfRecursive v t2 then Set.insert v bnd else bnd
                in case kind of
                     -- Newtype: opaque for instance lookup, but a record
                     -- newtype (NamU body) is transparent for per-language
                     -- form lookup. See the AppU branch above for the
                     -- detailed rationale.
                     TypedefNewtype -> case t2 of
                       NamU{} -> recurse bnd' t2
                       _ -> resolve bnd t0
                     TypedefPrimitive -> resolve bnd t0
                     TypedefAlias
                       | isTerminal -> terminate bnd' t2
                       | otherwise -> recurse bnd' t2
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
    -- KVarU / VoidU carriers of kinds not covered above are inert w.r.t.
    -- alias resolution (they have no aliases to expand).
    f _ t@(KVarU _) = return t
    f _ t@(VoidU _) = return t
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
    terminate bnd (NamU o v ts rs) =
      -- Same bnd-extension as in @f@ above. We only have the local name
      -- @v@ here (no scope lookup), so we just add that one; the @f@
      -- path is the more general entry point that handles concrete
      -- aliases. @terminate@ is reached only after a full evaluation,
      -- by which point the recursive references inside @rs@ already
      -- reference whichever name @v@ holds.
      let bnd' = Set.insert v bnd
      in NamU o v <$> mapM (recurse bnd') ts <*> mapM (secondM (recurse bnd')) rs
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
    -- KVarU / VoidU carriers of kinds not covered above are inert.
    terminate _ t@(KVarU _) = return t
    terminate _ t@(VoidU _) = return t
    -- Unified carriers: uniform recursion.
    terminate bnd (OpU op args) = OpU op <$> mapM (recurse bnd) args
    terminate _ t@(LitU (LNat _)) = return t
    terminate _ t@(LitU (LStr _)) = return t
    terminate bnd (LitU (LRec fs)) = LitU . LRec <$> mapM (\(k, v) -> fmap ((,) k) (recurse bnd v)) fs
    terminate bnd (LitU (LList es)) = LitU . LList <$> mapM (recurse bnd) es
    terminate bnd (LitU (LSet es)) = LitU . LSet <$> mapM (recurse bnd) es
    terminate bnd (LabeledU n t) = LabeledU n <$> recurse bnd t

    renameTypedefs ::
      Set.Set TVar -> ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) -> ([TVar], TypeU, ArgDoc, Bool, TypedefKind)
    renameTypedefs _ ([], t, d, isTerminal, kind) = ([], t, d, isTerminal, kind)
    renameTypedefs bnd (Left (v@(TV x), _) : vs, t, d, isTerminal, kind)
      | Set.member v bnd =
          let (vs', t', d', isTerminal', kind') = renameTypedefs bnd (vs, t, d, isTerminal, kind)
              v' =
                head
                  [ x' | x' <- [TV (MT.show' i <> x) | i <- [(0 :: Int) ..]], not (Set.member x' bnd), x' `notElem` vs'
                  ]
              t'' = substituteTVar v (VarU v') t'
           in (v' : vs', t'', d', isTerminal', kind')
      | otherwise =
          let (vs', t', d', isTerminal', kind') = renameTypedefs bnd (vs, t, d, isTerminal, kind)
           in (v : vs', t', d', isTerminal', kind')
    renameTypedefs bnd (Right _ : vs, t, d, isTerminal, kind) =
      renameTypedefs bnd (vs, t, d, isTerminal, kind)

    -- True iff the substituted body contains a back-reference to the
    -- alias's own name (e.g. @record LL where tail :: ?LL@'s body
    -- mentions @LL@; @type Pair a = (a, ?(Pair a))@'s body mentions
    -- @Pair@). Only self-recursive aliases need @v@ added to bnd
    -- during the walk -- a non-recursive alias like @type List a =
    -- "vector<$1>" a@ has no self-reference, and adding @v@ would
    -- spuriously block a nested use such as @List (List Real)@.
    --
    -- The walk is structural and bounded by the body size, so the
    -- check adds O(body) per substitution. Recursive types' bodies
    -- are typically small (the recursion is at use sites, not in the
    -- definition), so this is cheap in practice.
    isSelfRecursive :: TVar -> TypeU -> Bool
    isSelfRecursive v = go
      where
        go (VarU v')           = v == v'
        go (AppU h ts)         = go h || any go ts
        go (NamU _ _ ps rs)    = any go ps || any (go . snd) rs
        go (FunU ts t)         = any go ts || go t
        go (ForallU _ t)       = go t
        go (EffectU _ t)       = go t
        go (OptionalU t)       = go t
        go (LabeledU _ t)      = go t
        go (OpU _ args)        = any go args
        go (LitU (LRec fs))    = any (go . snd) fs
        go (LitU (LList es))   = any go es
        go (LitU (LSet es))    = any go es
        go _                   = False

    -- When a type alias is imported from two places, this function reconciles them, if possible
    mergeAliases ::
      [TypeU] ->
      Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) ->
      Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) ->
      Either MorlocError (Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind))
    mergeAliases _ Nothing Nothing = Right Nothing
    mergeAliases tsMain Nothing (Just b)
      | checkAlias tsMain b = Right (Just b)
      | otherwise = Right Nothing
    mergeAliases tsMain (Just a) Nothing
      | checkAlias tsMain a = Right (Just a)
      | otherwise = Right Nothing
    -- TODO: should the docstring args be considered here?
    mergeAliases tsMain (Just a@(ts1, t1, _, isTerminal1, kind1)) (Just b@(ts2, t2, _, isTerminal2, kind2))
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
          && isTerminal1 == isTerminal2
          -- and both are the same kind of definition (alias vs newtype)
          && kind1 == kind2 =
          return (Just a)
      -- handle specialization
      | not nonspecialized = return $ selectSpecialization a b
      -- Two fully-generic terminal alias bodies for the same nominal
      -- type, with different concrete forms. This happens when distinct
      -- modules each declare their own per-language override for the
      -- newtype (e.g. table-py maps @Vector@ to a Python @list@ for
      -- table operations, while vector-py maps it to @numpy.ndarray@).
      -- The bodies are NOT subtype-comparable here -- they are concrete
      -- forms in disjoint spaces -- so a per-language conflict is a
      -- user's choice between competing definitions, not a type error.
      -- Keep the first definition; the order is determined by import
      -- order at scope-build time.
      | nonspecialized
      , isTerminal1 && isTerminal2
      , kind1 == TypedefAlias && kind2 == TypedefAlias =
          return (Just a)
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
      ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) ->
      ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) ->
      Maybe ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind)
    selectSpecialization a@(aps0, _, _, _, _) b@(bps0, _, _, _, _) = g aps0 bps0
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
      ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) ->
      Bool
    checkAlias ts1 (ts2, _, _, _, _) =
      length ts1 == length ts2
        && all (\(x, y) -> either (const True) (\ytype -> isSubtypeOf ytype x) y) (zip ts1 ts2)

-- Replace a type variable with an expression. For example:
-- parsub ("a", "Int") -> "Map a b" -> "Map Int b"
parsub :: (TVar, TypeU) -> TypeU -> TypeU
parsub (v, t2) t1@(VarU v0)
  | v0 == v = t2 -- substitute
  | otherwise = t1 -- keep the original
-- Kind-specific variables (NatVarU/StrVarU/RecVarU/ListVarU/SetVarU) are
-- substituted exactly like VarU when the typedef parameter name matches.
-- Without this, typedef bodies that use a kind-promoted variable inside an
-- arithmetic / operator expression (e.g. `type Foo (n :: Nat) (m :: Nat) =
-- Vector (n + m) Int`) would expand to a body that still references the
-- unbound NatVarU, leaving the Nat solver unable to reduce the result.
parsub (v, t2) t1@(NatVarU v0)
  | v0 == v = t2
  | otherwise = t1
parsub (v, t2) t1@(StrVarU v0)
  | v0 == v = t2
  | otherwise = t1
parsub (v, t2) t1@(RecVarU v0)
  | v0 == v = t2
  | otherwise = t1
parsub (v, t2) t1@(ListVarU v0)
  | v0 == v = t2
  | otherwise = t1
parsub (v, t2) t1@(SetVarU v0)
  | v0 == v = t2
  | otherwise = t1
parsub pair (ExistU t (ts, tc) (rs, rc)) = ExistU t (map (parsub pair) ts, tc) (zip (map fst rs) (map (parsub pair . snd) rs), rc)
parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
parsub pair (FunU ts t) = FunU (map (parsub pair) ts) (parsub pair t)
parsub pair (AppU t ts) = AppU (parsub pair t) (map (parsub pair) ts)
parsub pair (NamU o n ps rs) = NamU o n (map (parsub pair) ps) [(k', parsub pair t) | (k', t) <- rs]
parsub pair (EffectU effs t) = EffectU effs (parsub pair t)
parsub pair (OptionalU t) = OptionalU (parsub pair t)
parsub _ t@NatVoidU = t
parsub _ t@StrVoidU = t
parsub _ t@RecVoidU = t
parsub _ t@ListVoidU = t
parsub _ t@SetVoidU = t
-- KVarU / VoidU carriers of kinds not covered above. KVarU matches the
-- parametric substitution the same way as the per-kind NatVarU/StrVarU/
-- RecVarU/ListVarU/SetVarU cases above; VoidU is inert.
parsub (v, t2) t1@(KVarU (v0, _))
  | v0 == v = t2
  | otherwise = t1
parsub _ t@(VoidU _) = t
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
--
-- Newtypes are opaque to this function: a newtype head returns Nothing
-- because the typechecker and instance-resolution must see the newtype as
-- a distinct nominal type. Use 'expandWireParent' to follow newtypes for
-- schema generation, where the wire format is shared with the body.
expandHeadOnly :: Scope -> TypeU -> Maybe TypeU
expandHeadOnly = expandHeadWith stopAtNominalBoundary

-- | Like 'expandHeadOnly' but follows newtype wire-parent edges. Used by
-- schema generation in 'Morloc.CodeGenerator.Serial': a @newtype Foo = Bar@
-- has Bar's wire format, so schema walks descend through the newtype
-- boundary even though the typechecker treats them as nominally distinct.
-- Still halts at primitives -- those are the leaves of the wire-parent
-- chain.
expandWireParent :: Scope -> TypeU -> Maybe TypeU
expandWireParent = expandHeadWith stopAtPrimitive

-- Internal: shared body of 'expandHeadOnly' (stopAtNewtype=True) and
-- 'expandWireParent' (stopAtNewtype=False).
-- @stopAt@ predicate determines which kinds halt expansion. For
-- 'expandHeadOnly' this is newtype-or-primitive (both are opaque to the
-- typechecker). For 'expandWireParent' it is primitive only -- newtypes
-- are walked through to reach the wire format.
expandHeadWith :: (TypedefKind -> Bool) -> Scope -> TypeU -> Maybe TypeU
expandHeadWith stopAt scope (AppU (VarU v) args) =
  case Map.lookup v scope of
    (Just ((paramKinds, body, _, _, kind) : _))
      | stopAt kind -> Nothing
      | length paramKinds == length args ->
          let params = [p | Left (p, _) <- paramKinds]
          in if length params == length args
             then Just $ foldr parsub body (zip params args)
             else Nothing
      | length args < length paramKinds ->
          -- Kind-based realignment: bind type-kinded params from `args`
          -- in order, fill kind-kinded params (Nat/Str) with their
          -- void sentinels. This mirrors the macro indexing convention:
          -- $N counts TYPE params only, so when @args@ is shorter than
          -- @paramKinds@ the missing positions are always kind-kinded.
          let typeParams = [p | Left (p, KindType) <- paramKinds]
              natParams  = [p | Left (p, KindNat)  <- paramKinds]
              strParams  = [p | Left (p, KindStr)  <- paramKinds]
          in if length typeParams == length args
             then Just $ foldr parsub body
                    (zip typeParams args
                      ++ [(p, NatVoidU) | p <- natParams]
                      ++ [(p, StrVoidU) | p <- strParams])
             else Nothing
      | otherwise -> Nothing
    _ -> Nothing
expandHeadWith stopAt scope (VarU v) =
  case Map.lookup v scope of
    (Just (([], body, _, _, kind) : _))
      | stopAt kind -> Nothing
      | otherwise -> Just body
    _ -> Nothing
expandHeadWith _ _ _ = Nothing

-- | Walk the wire-parent chain to the structural root: repeatedly
-- expand transparent aliases and newtype wire-parents until a
-- primitive (or otherwise non-reducible) type is reached. Used by the
-- typechecker's literal-check rules to see whether a possibly-newtyped
-- expected type has the structural shape of the literal's natural
-- primitive (e.g. is @Vector 5 Real@ ultimately list-shaped at the
-- wire level, so a list literal can inhabit it).
--
-- Unlike 'expandWireParent', this walks repeatedly until fixed point.
-- The outer constructor (which determines literal acceptance) is what
-- callers inspect on the result; argument types are propagated by the
-- caller's element-typing rules.
wireParentRoot :: Scope -> TypeU -> TypeU
wireParentRoot scope t = case expandWireParent scope t of
  Just t' | t' /= t -> wireParentRoot scope t'
  _ -> t

-- | Halt at newtypes and primitives -- the typechecker sees them as
-- nominally distinct base forms.
stopAtNominalBoundary :: TypedefKind -> Bool
stopAtNominalBoundary TypedefNewtype = True
stopAtNominalBoundary TypedefPrimitive = True
stopAtNominalBoundary TypedefAlias = False

-- | Halt only at primitives. Used by schema generation, which follows the
-- wire-parent chain through newtypes to find the underlying wire format.
stopAtPrimitive :: TypedefKind -> Bool
stopAtPrimitive TypedefPrimitive = True
stopAtPrimitive _ = False

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
  let (t1, changed) = go Set.empty t0
  in if changed then Just t1 else Nothing
  where
    -- Same shape as 'reduceTypeLeaves.go': bound-set of NamU names cuts
    -- recursive expansion, and AppU heads are NOT recursed into so a
    -- parameterized alias is never expanded as a bare partial app.
    go :: Set.Set TVar -> TypeU -> (TypeU, Bool)
    go bnd t@(VarU v)
      | Set.member v bnd = (t, False)
    go bnd t@(AppU (VarU v) _)
      | Set.member v bnd = descend bnd t
    go bnd t = case expandHeadOnly scope t of
      Just t' -> (t', True)
      Nothing -> descend bnd t

    descend bnd (ForallU v t) =
      let (t', c) = go bnd t in (ForallU v t', c)
    descend bnd (FunU ts t) =
      let (ts', cs1) = unzip (map (go bnd) ts)
          (t', c2) = go bnd t
      in (FunU ts' t', or (c2 : cs1))
    descend bnd (AppU f ts) =
      let (ts', cs) = unzip (map (go bnd) ts)
      in (AppU f ts', or cs)
    descend bnd (NamU o n ps rs) =
      let bnd' = Set.insert n bnd
          (ps', cs1) = unzip (map (go bnd') ps)
          (vs', cs2) = unzip (map (go bnd' . snd) rs)
      in (NamU o n ps' (zip (map fst rs) vs'), or (cs1 ++ cs2))
    descend bnd (EffectU effs t) =
      let (t', c) = go bnd t in (EffectU effs t', c)
    descend bnd (OptionalU t) =
      let (t', c) = go bnd t in (OptionalU t', c)
    descend _ t = (t, False)
