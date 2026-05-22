{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Frontend.Typecheck
Description : Bidirectional type inference and checking for general types
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Implements bidirectional type inference over the 'AnnoS' trees produced by
'Treeify'. Checks general (language-independent) types and resolves type
aliases. Concrete (language-specific) types are checked later after language
segregation in the code generator.
-}
module Morloc.Frontend.Typecheck (typecheck, resolveTypes, evaluateAnnoSTypes, peakSExpr) where

import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal

{- | Each SAnno object in the input list represents one exported function.
Modules, scopes, imports and everything else are abstracted away.

Check the general types, do nothing to the concrete types which may only be
solved after segregation. Later the concrete types will need to be checked
for type consistency and correctness of packers.
-}
-- | True for primitive set-theoretic constraints (Member / Subset /
-- Disjoint), which 'dischargeConstraints' is responsible for resolving.
-- Typeclass-form constraints (@Constraint cls ts@) are handled by the
-- class-resolution pass and pass through this filter unchanged.
isPrimitiveConstraint :: Constraint -> Bool
isPrimitiveConstraint (Constraint _ _) = False
isPrimitiveConstraint (CMember _ _) = True
isPrimitiveConstraint (CSubset _ _) = True
isPrimitiveConstraint (CDisjoint _ _) = True

typecheck ::
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheck = mapM run
  where
    run :: AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
    run e0 = do
      -- standardize names for lambda bound variables (e.g., x0, x1 ...)
      let g0 = Gamma {gammaCounter = 0, gammaSlot = 0, gammaContext = IntMap.empty, gammaExist = Map.empty, gammaSolved = Map.empty, gammaDeferred = [], gammaNatSubs = Map.empty, gammaStrSubs = Map.empty, gammaRecSubs = Map.empty, gammaListSubs = Map.empty, gammaSetSubs = Map.empty, gammaEffSubs = Map.empty, gammaConstraints = [], gammaAssumedConstraints = Nothing, gammaIntVals = Map.empty}
      (g1, _, e1) <- synthG g0 e0
      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g1:"
      seeGamma g1
      insetSay "========================================================"
      let e2 = mapAnnoSG (fmap normalizeType) . applyGen g1 $ e1

      (g2, e3) <- resolveInstances g1 e2
      let g3 = apply g2 g2

      -- re-check deferred Nat constraints now that existentials are solved
      case recheckDeferred g3 of
        Left err -> MM.throwSystemError err
        Right remaining ->
          mapM_ (\(t1, t2) ->
            MM.sayV $ "Warning: unresolved Nat constraint:" <+> prettyTypeU t1 <+> "~" <+> prettyTypeU t2
            ) remaining

      -- discharge primitive set-theoretic constraints (Member / Subset /
      -- Disjoint) accumulated through subtype calls. Each constraint
      -- runs through reduceConstraint after the final substitution.
      --
      -- Three failure modes for *primitive* constraints (CMember /
      -- CSubset / CDisjoint):
      --   1. Contradiction: the constraint cannot ever be satisfied
      --      under any further substitution. Hard error.
      --   2. Undecidable AND not subsumed by the function's declared
      --      assumptions: the function body needs a constraint the
      --      signature did not declare. Hard error pointing at the
      --      missing constraint.
      --   3. Undecidable but subsumed by an assumption: silently
      --      discharged inside dischargeConstraints.
      --
      -- Typeclass-form constraints (@Constraint cls _@) are handled by
      -- the separate class-resolution machinery; we leave any leftover
      -- of that form in place rather than error.
      case dischargeConstraints g3 of
        Left err -> MM.throwSystemError ("Constraint violation: " <> err)
        Right g3' ->
          case filter isPrimitiveConstraint (gammaConstraints g3') of
            [] -> return ()
            cs ->
              -- Apply gamma so the displayed form reflects what was
              -- actually unsolved (the queue still carries pre-substitution
              -- variable names; the user wants to see post-substitution
              -- structure with internal @\@nN@ rename suffixes stripped).
              -- prettyConstraint also parenthesises compound args so the
              -- output reads as something that could be pasted into the
              -- signature's @=>@ clause.
              let displayed = map (prettyConstraint . apply g3') cs
                  -- The outermost VarS in the AnnoS names the function
                  -- being typechecked; surface it in the error so the
                  -- user knows which signature to amend.
                  fnName = case e0 of
                    AnnoS _ _ (VarS (EV n) _) -> Just n
                    _ -> Nothing
                  prefix = case fnName of
                    Just n -> "Unsolved constraint(s) on " <> dquotes (pretty n) <> "; add to its signature: "
                    Nothing -> "Unsolved constraint(s); add to the function signature: "
              in MM.throwSystemError $
                prefix <> hcat (punctuate ", " displayed)

      -- perform a final application of gamma the final expression and return
      -- (is this necessary?)
      return (applyGen g3 e3)

-- TypeU --> Type
resolveTypes :: AnnoS (Indexed TypeU) Many Int -> AnnoS (Indexed Type) Many Int
resolveTypes (AnnoS (Idx i t) ci e) =
  let t' = typeOf t
      -- C3: canonicalise NamS field order against the resolved record
      -- type. checkE handles top-level and nested-NamS positions, but
      -- records appearing inside tuples / lists / polymorphic arg
      -- positions can skip the checkE rule and reach here in source
      -- order. Reordering against the annotation guarantees every
      -- downstream NamS sees declared order.
      e' = case (t', e) of
        (NamT _ _ _ declared, NamS rs)
          | not (null declared)
          , let declKeys = map fst declared
          , Set.fromList declKeys == Set.fromList (map fst rs) ->
              let rsMap = Map.fromList rs
               in NamS [(k, rsMap Map.! k) | k <- declKeys]
        _ -> e
   in AnnoS (Idx i t') ci (f e')
  where
    f :: ExprS (Indexed TypeU) Many Int -> ExprS (Indexed Type) Many Int
    f (BndS x) = BndS x
    f (LetBndS x) = LetBndS x
    f (CallS x) = CallS x
    f (LetS v e1 e2) = LetS v (resolveTypes e1) (resolveTypes e2)
    f (VarS v xs) = VarS v (fmap resolveTypes xs)
    f (ExeS exe) = ExeS exe
    f (AppS x xs) = AppS (resolveTypes x) (map resolveTypes xs)
    f (LamS vs x) = LamS vs (resolveTypes x)
    f (LstS xs) = LstS (map resolveTypes xs)
    f (TupS xs) = TupS (map resolveTypes xs)
    f (NamS rs) = NamS (zip (map fst rs) (map (resolveTypes . snd) rs))
    f (RealS si x) = RealS si x
    f (IntS si x) = IntS si x
    f (LogS x) = LogS x
    f (StrS x) = StrS x
    f UniS = UniS
    f NullS = NullS
    f (DoBlockS e') = DoBlockS (resolveTypes e')
    f (EvalS e') = EvalS (resolveTypes e')
    f (CoerceS c e') = CoerceS c (resolveTypes e')
    f (IfS c t' e') = IfS (resolveTypes c) (resolveTypes t') (resolveTypes e')
    f (IntrinsicS intr es) = IntrinsicS intr (map resolveTypes es)

resolveInstances ::
  Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (Gamma, AnnoS (Indexed TypeU) Many Int)
resolveInstances g (AnnoS gi@(Idx genIndex gt) ci e0) = do
  gscope <- MM.getGeneralScope genIndex
  (g', e1) <- f gscope g e0
  return (g', AnnoS gi ci e1)
  where
    f ::
      Scope ->
      Gamma ->
      ExprS (Indexed TypeU) ManyPoly Int ->
      MorlocMonad (Gamma, ExprS (Indexed TypeU) Many Int)

    -- resolve instances
    f scope g0 (VarS v (PolymorphicExpr clsName _ _ rss)) = do
      -- find all instances that are a subtype of the inferred type
      -- Expand aliases in gt before matching (e.g., Vector 4 Int -> List Int)
      -- so instances for List match against Vector usage.
      let gtEval = case TE.evaluateType scope gt of
            Right et -> et
            Left _ -> gt
          emptyGamma = Gamma 0 0 IntMap.empty Map.empty Map.empty [] Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty [] Nothing Map.empty
          isCompatible t = isSubtypeOf2 scope t gtEval
                        || isJust (tryCoerce scope t gtEval emptyGamma)
          rssCompat = [x | x@(EType t _ _ _, _) <- rss, isCompatible t]
          -- Filter by alias-chain reachability: only keep instances whose
          -- type is reachable from gt by walking the alias chain. This
          -- prevents sibling aliases from inheriting each other's instances
          -- (e.g., instance Qux A should not match call site type A' even
          -- though both A and A' evaluate to the same root type).
          rssSubtypes = filterByAliasChain scope gt rssCompat


      -- find the most specific instance at the general level, this does not
      -- consider a type to be more specific it is more evaluated.
      --
      -- So for the types below
      --  type Stack = List
      --  type SpecialStack = Stack
      --
      -- And the instances here:
      --  instance Foo Stack where
      --      bar :: ...
      --  instance Foo SpecialStack where
      --      bar :: ...
      --
      --  The two bar instances would be considered equally specialized
      --
      --  They will be separated later when concrete types are considered. From
      --  the general perspective, the evaluate to being equal.
      (g2, es1) <- case mostSpecific [t | (EType t _ _ _, _) <- rssSubtypes] of
        -- if there are no suitable instances, die
        [] ->
          throwTypeError genIndex $
            "No instance found for" <+> pretty clsName
              <> "::"
              <> pretty v
              <> "\n  Are you missing a top-level type signature?"
        -- There may be many suitable instances from the general type level,
        -- however, they may differ at the concrete level, so keep all for know
        -- and let the concrete inference code sort things out later.
        manyTypes -> do
          -- Deduplicate alias-equivalent types: e.g., Array a and List a
          -- are structurally different but evaluate to the same type.
          -- Without this, N aliases cause exponential branching in recursive
          -- resolveInstances calls.
          let eval t = case TE.evaluateType scope t of
                Right et -> et
                Left _ -> t
              deduped = nubBy (\t1 t2 -> eval t1 == eval t2) manyTypes
              es0 = concat [rs | (t, rs) <- rssSubtypes,
                            any (\d -> eval (etype t) == eval d) deduped]
              -- When all matching instances are alias-equivalent (single
              -- deduped entry), propagate existential solutions normally.
              -- When multiple distinct instances exist (e.g. Integral Int
              -- and Integral Real), don't propagate -- solving to one
              -- would break the others.
              singleGroup = length deduped <= 1
          g1 <- connectInstance singleGroup g0 es0
          return (g1, es0)

      (g3, es2) <- statefulMapM resolveInstances g2 es1

      return (g3, VarS v (Many es2))
    f _ g0 (VarS v (MonomorphicExpr _ xs)) = statefulMapM resolveInstances g0 xs |>> second (VarS v . Many)
    -- propagate
    f _ g0 (AppS e es) = do
      (g1, e') <- resolveInstances g0 e
      (g2, es') <- statefulMapM resolveInstances g1 es
      return (g2, AppS e' es')
    f _ g0 (LamS vs e) = resolveInstances g0 e |>> second (LamS vs)
    f _ g0 (LstS es) = statefulMapM resolveInstances g0 es |>> second LstS
    f _ g0 (TupS es) = statefulMapM resolveInstances g0 es |>> second TupS
    f _ g0 (NamS rs) = do
      (g1, es') <- statefulMapM resolveInstances g0 (map snd rs)
      return (g1, NamS (zip (map fst rs) es'))

    -- let expressions
    f _ g0 (LetBndS v) = return (g0, LetBndS v)
    f _ g0 (LetS v e1 e2) = do
      (g1, e1') <- resolveInstances g0 e1
      (g2, e2') <- resolveInstances g1 e2
      return (g2, LetS v e1' e2')
    -- primitives
    f _ g0 UniS = return (g0, UniS)
    f _ g0 NullS = return (g0, NullS)
    f _ g0 (BndS v) = return (g0, BndS v)
    f _ g0 (CallS v) = return (g0, CallS v)
    f _ g0 (RealS si x) = return (g0, RealS si x)
    f _ g0 (IntS si x) = return (g0, IntS si x)
    f _ g0 (LogS x) = return (g0, LogS x)
    f _ g0 (StrS x) = return (g0, StrS x)
    f _ g0 (ExeS x) = return (g0, ExeS x)
    f _ g0 (DoBlockS e) = resolveInstances g0 e |>> second DoBlockS
    f _ g0 (EvalS e) = resolveInstances g0 e |>> second EvalS
    f _ g0 (CoerceS c e) = resolveInstances g0 e |>> second (CoerceS c)
    f _ g0 (IfS c t e) = do
      (g1, c') <- resolveInstances g0 c
      (g2, t') <- resolveInstances g1 t
      (g3, e') <- resolveInstances g2 e
      return (g3, IfS c' t' e')
    f _ g0 (IntrinsicS intr es) = do
      (g1, es') <- statefulMapM resolveInstances g0 es
      return (g1, IntrinsicS intr es')

    -- When unique (single alias-equivalent group), propagate gamma normally.
    -- When not unique (multiple distinct instances), skip failures and don't
    -- propagate, since solving to one instance would break the others.
    connectInstance :: Bool -> Gamma -> [AnnoS (Indexed TypeU) f c] -> MorlocMonad Gamma
    connectInstance _ g0 [] = return g0
    connectInstance singleGroup g0 (AnnoS (Idx i t) _ _ : es) = do
      scope <- MM.getGeneralScope i
      case subtype scope (stripCoercionWrappers gt) t g0 of
        (Left _) -> connectInstance singleGroup g0 es
        (Right g1)
          | singleGroup -> connectInstance singleGroup g1 es
          | otherwise   -> connectInstance singleGroup g0 es

-- | Filter typeclass instances to only those reachable from gt by walking
-- the alias chain. This prevents sibling aliases from inheriting each
-- other's instances: walking A' -> Str never reaches A (a sibling),
-- but walking B -> A does reach A (a parent).
--
-- This walks gt's chain only. Walking the instance's chain too (so that
-- e.g. an instance with head `Vector (d1*d2) a` matches a call site with
-- head `List a`) was tried and reverted: it broke nominal-distinct
-- aliases like Deque a / Array a / Vector n a -- all alias to List a,
-- but each has its own typeclass instances that should NOT be applied
-- to List call sites. Bidirectional walking conflates them.
filterByAliasChain :: Scope -> TypeU -> [(EType, [a])] -> [(EType, [a])]
filterByAliasChain scope gt0 candidates = go gt0
  where
    go currentGt =
      case [x | x@(EType t _ _ _, _) <- candidates, compatibleTypeU currentGt t] of
        [] -> case TE.reduceType scope currentGt of
          Just gt' | gt' /= currentGt -> go gt'
          _ -> case TE.reduceTypeLeaves scope currentGt of
            Just gt' | gt' /= currentGt -> go gt'
            _ -> case currentGt of
              -- Strip coercion wrappers: a -> ?a, a -> <E> a
              OptionalU t -> go t
              EffectU _ t -> go t
              _ -> []  -- nothing reachable
        matches -> matches

-- | Structural type compatibility for TypeU. ForallU-bound variables and
-- ExistU act as wildcards (match anything). Other VarU nodes require exact
-- name match. This checks if an instance type matches the call-site type
-- at a specific alias level without evaluating aliases.
--
-- Nat positions are treated permissively: if both positions are Nat-kinded
-- expressions (literal, variable, or arithmetic), they are considered
-- compatible at this filtering stage. Strict equivalence is enforced later
-- by `isSubtypeOf2` / `subtype`, which run the Nat SOP solver and can
-- reduce expressions like `2*2` to `4`. Without this leniency, an instance
-- whose head carries a Nat-arithmetic constraint (e.g. Vector (d1*d2) a)
-- would be filtered out before subtyping can prove the constraint, with
-- the call site's literal Nat (e.g. Vector 4 Real) treated as structurally
-- distinct from the instance's expression.
compatibleTypeU :: TypeU -> TypeU -> Bool
compatibleTypeU = go Set.empty Set.empty
  where
    go b1 b2 (ForallU v t1) t2 = go (Set.insert v b1) b2 t1 t2
    go b1 b2 t1 (ForallU v t2) = go b1 (Set.insert v b2) t1 t2
    go _  _  (ExistU _ _ _) _ = True
    go _  _  _ (ExistU _ _ _) = True
    go b1 b2 (VarU v1) (VarU v2)
      | Set.member v1 b1 || Set.member v2 b2 = True
      | otherwise = v1 == v2
    go b1 _  (VarU v1) _ = Set.member v1 b1
    go _  b2 _ (VarU v2) = Set.member v2 b2
    go b1 b2 (FunU as1 r1) (FunU as2 r2) =
      length as1 == length as2
        && all (uncurry (go b1 b2)) (zip as1 as2)
        && go b1 b2 r1 r2
    go b1 b2 (AppU h1 ps1) (AppU h2 ps2) =
      go b1 b2 h1 h2
        && length ps1 == length ps2
        && all (uncurry (go b1 b2)) (zip ps1 ps2)
    go b1 b2 (NamU o1 n1 ps1 rs1) (NamU o2 n2 ps2 rs2) =
      o1 == o2 && n1 == n2
        && length ps1 == length ps2
        && all (uncurry (go b1 b2)) (zip ps1 ps2)
        && length rs1 == length rs2
        && all (\((k1,v1),(k2,v2)) -> k1 == k2 && go b1 b2 v1 v2) (zip rs1 rs2)
    go b1 b2 (OptionalU t1) (OptionalU t2) = go b1 b2 t1 t2
    go b1 b2 (EffectU e1 t1) (EffectU e2 t2) = e1 == e2 && go b1 b2 t1 t2
    -- Nat-kinded positions: any Nat expression matches any other. The
    -- isSubtypeOf2 / subtype pipeline does proper Nat solving downstream.
    go _  _  t1 t2 | isNatExprT t1 && isNatExprT t2 = True
    go _  _  _ _ = False

    isNatExprT :: TypeU -> Bool
    isNatExprT (NatLitU _) = True
    isNatExprT (NatVarU _) = True
    isNatExprT (NatAddU _ _) = True
    isNatExprT (NatMulU _ _) = True
    isNatExprT (NatSubU _ _) = True
    isNatExprT (NatDivU _ _) = True
    isNatExprT NatVoidU = True
    isNatExprT _ = False

-- prepare a general, indexed typechecking error
throwTypeError :: Int -> MDoc -> MorlocMonad a
throwTypeError i msg = MM.throwSourcedError i ("General type error:" <+> msg)

-- A selector pattern (e.g. .0, .field) targets tuples and records only.
-- The selectorType existential unifies with List<a> by accident (both are
-- single-parameter AppU), but at runtime the pool reads garbage. Reject
-- the application here with a clear message and a hint.
rejectListSelectorTarget :: Int -> Selector -> TypeU -> MorlocMonad ()
rejectListSelectorTarget i s t = do
  scope <- MM.getGeneralScope i
  let t' = either (const t) id (TE.evaluateType scope t)
  case t' of
    AppU (VarU (TV "List")) _ ->
      MM.throwSourcedError i $
        "Index getter" <+> pretty s
          <+> "requires a tuple or record, got a list."
          <+> "Use 'head' or '!!' for list access."
    _ -> return ()

checkG ::
  Gamma ->
  AnnoS Int ManyPoly Int ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , AnnoS (Indexed TypeU) ManyPoly Int
    )
checkG g (AnnoS i j e) t = do
  annotation <- MM.gets stateAnnotations
  -- Use the body's concrete index (j) for error caret positions: i is
  -- often the variable-reference / export-list site and would mis-locate
  -- body errors.
  (g', t', e') <- case Map.lookup j annotation of
    Nothing -> checkE' j g e t
    (Just annType) -> do
      gAnn <- subtype' j annType t g
      checkE' j gAnn e t
  let annotatedBody = AnnoS (Idx i t') j e'
  -- When the user declared a signature at this site, verify that the
  -- body's evaluation-time effects are a subset of those the signature
  -- permits.  See spec/types/effects.md "Effect Checking".  Inner
  -- positions without an annotation flow through ordinary structural
  -- subtyping, which already rejects narrowing on EffectU types.
  case Map.lookup j annotation of
    Just annType -> checkEffectCoverage j (apply g' annType) annotatedBody
    Nothing -> return ()
  return (g', t', annotatedBody)

synthG ::
  Gamma ->
  AnnoS Int ManyPoly Int ->
  MorlocMonad
    ( Gamma
    , TypeU
    , AnnoS (Indexed TypeU) ManyPoly Int
    )
synthG g (AnnoS gi ci e) = do
  annotation <- MM.gets stateAnnotations
  -- Use the body's concrete index (ci) for error caret positions: gi
  -- is often the variable-reference / export-list site (e.g. the term
  -- name in `module Foo (val)`) and would mis-locate body errors.
  (g', t, e') <- case Map.lookup ci annotation of
    Nothing -> synthE' ci g e
    (Just annType) -> checkE' ci g e annType
  let annotatedBody = AnnoS (Idx gi t) ci e'
  -- Mirror the body-effect check from 'checkG'.  When a signature is
  -- attached at this synthesis site, verify the body's evaluation
  -- effects against it.
  case Map.lookup ci annotation of
    Just annType -> checkEffectCoverage ci (apply g' annType) annotatedBody
    Nothing -> return ()
  return (g', t, annotatedBody)

synthE ::
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
synthE _ g UniS = return (g, BT.unitU, UniS)
synthE _ g NullS =
  let (g1, v) = newvar "nullType_" g
   in return (g1, OptionalU v, NullS)
synthE _ g (RealS si x) = return (g, BT.realU, RealS si x)
synthE _ g (IntS si x) = return (g, BT.intU, IntS si x)
synthE _ g (LogS x) = return (g, BT.boolU, LogS x)
synthE _ g (StrS x) = return (g, BT.strU, StrS x)
-- Ensures pattern setting operations return the correct type.
-- Without this case, patterns that change type will pass silently, but lead to
-- corrupted data.
-- Setter pattern lambda: (\v -> .field v newVal) data
-- The body applies a pattern to 2+ args (data + set values).
-- Getters (1 arg) are NOT matched here and go through normal AppS.
synthE
  _
  g0
  ( AppS
      f0@( AnnoS
            _
            _
            ( LamS
                [_]
                ( AnnoS
                    _
                    _
                    ( AppS
                        ((AnnoS _ _ (ExeS (PatCall (PatternStruct _)))))
                        (_ : _ : _)
                      )
                  )
              )
          )
      [x0]
    ) = do
    (g1, patternType, f1) <- synthG g0 f0
    case patternType of
      (FunU _ selectType) -> do
        (g2, dataType, x1) <- checkG g1 x0 selectType
        return (g2, dataType, AppS f1 [x1])
      _ -> error "This should be unreachable"

-- synthesize a string interpolation pattern
synthE i g (AppS f@(AnnoS _ _ (ExeS (PatCall (PatternText _ _)))) es) = do
  (g1, _, f1) <- synthG g f
  (g2, _, es1, _) <- zipCheck i g1 es (take (length es) (repeat BT.strU))
  return (g2, BT.strU, AppS f1 es1)

-- handle getter patterns
synthE _ _ (AppS (AnnoS _ _ (ExeS (PatCall (PatternStruct _)))) []) = error "Unreachable application pattern to no data"
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) [e0]) = do
  -- generate an existential type that contains the pattern
  (g1, datType) <- selectorType g0 s

  -- type returned from pattern (with one element for each extracted value)
  retType <- return $ case selectorGetter datType s of
    [] -> error "Illegal empty selection"
    [t] -> t
    ts -> BT.tupleU ts
  let ft = FunU [datType] retType

  -- use selector-derived type to update context and data expression
  (g2, _, e') <- checkG g1 e0 datType

  -- Reject getter/selector pattern applied to a list. The selector
  -- existential unifies with List<a> as a single-slot tuple, producing
  -- garbage at runtime. Tuple/record getters only.
  rejectListSelectorTarget fgidx s (apply g2 datType)

  let f1 = (AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall (PatternStruct s))))

  return (g2, apply g2 retType, AppS f1 [e'])

-- handle setter patterns
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) (e0 : es0)) = do
  (g1, (unzip -> (setTypes, es1))) <-
    statefulMapM (\s' e -> synthG s' e |>> (\(a, b, c) -> (a, (b, c)))) g0 es0

  -- generate an existential type that contains the pattern
  (g2, outputType) <- selectorType g1 s |>> second (selectorSetter setTypes s)

  (g3, datType, e1) <- checkG g2 e0 outputType

  rejectListSelectorTarget fgidx s (apply g3 datType)

  let patternType = apply g3 $ FunU (datType : setTypes) outputType
      f1 = AnnoS (Idx fgidx patternType) fcidx (ExeS (PatCall (PatternStruct s)))

  return (g3, apply g3 outputType, AppS f1 (e1 : es1))
synthE _ g (ExeS (PatCall (PatternText s ss@(length -> n)))) = do
  let t = FunU (take n (repeat BT.strU)) BT.strU
  return (g, t, ExeS (PatCall (PatternText s ss)))

--   -->E0
synthE _ g (AppS f []) = do
  (g1, t1, f1) <- synthG g f
  return (g1, t1, AppS f1 [])

--   -->E
synthE i g0 (AppS f xs0) = do
  -- synthesize the type of the function
  (g1, funType0, funExpr0) <- synthG g0 f

  -- Resolve nat labels: if the function has labeled nat params (m:Int syntax)
  -- and corresponding args are int literals, inject NatVarU solutions into gamma
  let g1' = resolveNatLabels f funType0 xs0 g1

  etaExpandSynthE i g1' funType0 funExpr0 f xs0

-- -->I==>
-- Synthesize lambda expressions. The key optimization here is to avoid
-- re-synthesizing after eta expansion - we synthesize the body once with
-- proper context, then construct the expanded form directly.
synthE parentIdx g0 (LamS vs x) = do
  -- Create existentials for lambda-bound variables and add to context
  let (g1, paramTypes) = statefulMap (\g' v -> newvar (unEVar v <> "_x") g') g0 vs
      g2 = g1 ++> zipWith AnnG vs paramTypes

  -- Synthesize body ONCE with bound variables in context
  (g3, bodyType, bodyExpr) <- synthG g2 x

  -- Check if body returns a function (needs eta expansion)
  let normalBody = normalizeType (apply g3 bodyType)
  case normalBody of
    FunU extraArgTypes retType -> do
      -- Body returns a function: eta-expand WITHOUT re-synthesizing
      -- Create new bound variables for the extra arguments
      (g4, newVarsWithTypes) <-
        statefulMapM
          ( \g' t -> do
              let (g'', v) = evarname g' "v"
              return (g'', (v, t))
          )
          g3
          extraArgTypes

      let newVars = map fst newVarsWithTypes
          appliedExtraTypes = map (apply g4 . snd) newVarsWithTypes

      -- Add type annotations for new bound variables
      let g5 = g4 ++> zipWith AnnG newVars appliedExtraTypes

      -- Create typed variable references for the new parameters
      newVarExprs <-
        mapM
          ( \(v, t) -> do
              idx <- MM.getCounterWithPos parentIdx
              return $ AnnoS (Idx idx t) idx (BndS v)
          )
          (zip newVars appliedExtraTypes)

      -- Create the application of body to new variables
      appIdx <- MM.getCounterWithPos parentIdx
      let appliedRetType = apply g5 retType
          appliedBodyExpr = AppS (applyGen g5 bodyExpr) newVarExprs
          appliedBodyAnno = AnnoS (Idx appIdx appliedRetType) appIdx appliedBodyExpr

      -- Construct the full function type
      let allParamTypes = map (apply g5) paramTypes ++ appliedExtraTypes
          fullType = FunU allParamTypes appliedRetType

      return (g5, fullType, LamS (vs ++ newVars) appliedBodyAnno)
    _ -> do
      -- Body is not a function: just return the lambda as-is
      let funType = apply g3 (FunU paramTypes bodyType)
      return (g3, funType, LamS vs (applyGen g3 bodyExpr))

--   List
synthE _ g (LstS []) =
  let (g1, itemType) = newvar "itemType_" g
      listType = BT.listU itemType
   in return (g1, listType, LstS [])
synthE i g (LstS (e : es)) = do
  (g1, itemType, itemExpr) <- synthG g e
  (g2, listType, listExpr) <- checkE' i g1 (LstS es) (BT.listU itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr : es'))
    _ -> error "impossible"

--   Tuple
synthE _ g (TupS []) =
  let t = BT.tupleU []
   in return (g, t, TupS [])
synthE i g (TupS (e : es)) = do
  -- synthesize head
  (g1, itemType, itemExpr) <- synthG g e

  -- synthesize tail
  (g2, tupleType, tupleExpr) <- synthE' i g1 (TupS es)

  -- merge the head and tail
  t3 <- case tupleType of
    (AppU _ ts) -> return $ BT.tupleU (apply g2 itemType : ts)
    _ -> error "impossible" -- the general tuple will always be (AppU _ _)
  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors
  return (g2, t3, TupS (itemExpr : xs'))
synthE _ g0 (NamS rs) = do
  (g1, xs) <- statefulMapM (\s v -> synthG s v |>> (\(a, b, c) -> (a, (b, c)))) g0 (map snd rs)
  let (ts, es) = unzip xs
      ks = map fst rs
      (g2, t) = newvarRich ([], Closed) (zip ks ts, Closed) "record_" g1
      e = NamS (zip ks es)
  return (g2, t, e)

-- Any morloc variables should have been expanded by treeify. Any bound
-- variables should be checked against. I think (this needs formalization).
synthE _ g0 (VarS v (MonomorphicExpr (Just t0) xs0)) = do
  -- Rename type AND constraints together so primitive constraints
  -- (CMember / CSubset / CDisjoint) reference the same fresh-name
  -- variables that the type uses. The renamed constraints are queued
  -- onto gammaConstraints so they discharge against call-site
  -- substitutions at end-of-typecheck.
  --
  -- Constraint propagation: the *outermost* VarS encountered during a
  -- top-level typecheck represents the function being defined. Its
  -- declared @econs@ are taken as assumptions for the body and parked
  -- in @gammaAssumedConstraints@. A deferred obligation that cannot
  -- be decided at end-of-typecheck is allowed to discharge if it
  -- matches one of these assumptions: that is exactly the semantics
  -- of "the body may use the constraints the signature declared."
  -- Subsequent (inner) VarS calls just enqueue obligations on
  -- @gammaConstraints@ and do not touch the assumptions list.
  --
  -- Outermost-vs-inner is detected by @gammaAssumedConstraints ==
  -- Nothing@: 'run' initialises it to @Nothing@ and only the
  -- outermost VarS finds it so. Once any signature has been claimed,
  -- the slot is @Just _@ (possibly @Just []@ if the signature had no
  -- declared constraints) and stays that way. This Maybe sentinel is
  -- necessary because a signature with no constraints would otherwise
  -- be indistinguishable from "not yet seen" if we used a plain list.
  let (g1, t0') = renameEType g0 t0
      t1 = etype t0'
      newCs = Set.toList (econs t0')
      g1Cs = g1
        { gammaConstraints = gammaConstraints g1 ++ newCs
        , gammaAssumedConstraints = case gammaAssumedConstraints g1 of
            Nothing -> Just newCs
            Just _  -> gammaAssumedConstraints g1
        }
      g1' = g1Cs ++> [AnnG v t1]
  (g2, t2, xs1) <- foldCheck g1' xs0 t1
  -- Verify the body's evaluation-time effects fit within the declared
  -- signature.  Catches forces outside do-blocks (e.g. `f = !rint`)
  -- that the structural subtype rule cannot see, because EvalS strips
  -- the effect wrapper from the inner type.  See
  -- spec/types/effects.md "Effect Checking".  The body's own concrete
  -- index (ci) localises the diagnostic to the definition: this
  -- clause's index argument is the variable-reference / export-list
  -- site and would mis-point the caret at the module declaration.
  mapM_ (\x@(AnnoS _ ci _) -> checkEffectCoverage ci (apply g2 t1) x) xs1
  let xs2 = applyCon g2 $ VarS v (MonomorphicExpr (Just t0) xs1)
  return (g2, t2, xs2)
synthE _ g (VarS v (MonomorphicExpr Nothing (x : xs))) = do
  let (g0', freshT) = newvar (unEVar v <> "_rec") g
      g0'' = g0' ++> [AnnG v freshT]
  (g', t', x') <- synthG g0'' x
  (g'', t'', xs') <- foldCheck g' xs t'
  let xs'' = applyCon g'' $ VarS v (MonomorphicExpr Nothing (x' : xs'))
  return (g'', t'', xs'')
synthE _ g (VarS v (MonomorphicExpr Nothing [])) = do
  let (g', t) = newvar (unEVar v <> "_u") g
  return (g', t, VarS v (MonomorphicExpr Nothing []))
synthE i g0 (VarS v (PolymorphicExpr cls clsName t0 rs0)) = do
  (g1, rsChecked) <- checkInstances g0 (etype t0) rs0
  let (g2, t1) = rename g1 (etype t0)
  return (g2, t1, VarS v (PolymorphicExpr cls clsName t0 rsChecked))
  where
    -- Check each instance independently. Reset gammaContext between instances
    -- to prevent unbounded growth (each instance adds ~50-80 entries for
    -- existentials and solved types that are not needed by subsequent checks).
    checkInstances ::
      Gamma ->
      TypeU ->
      [(EType, [AnnoS Int ManyPoly Int])] ->
      MorlocMonad (Gamma, [(EType, [AnnoS (Indexed TypeU) ManyPoly Int])])
    checkInstances g _ [] = return (g, [])
    checkInstances g10 genType ((instType, es) : rs) = do
      -- convert qualified terms in the general type to existentials
      let (g11, genType') = toExistential g10 genType
      -- rename the instance type
      let (g12, instType') = rename g11 (etype instType)
      -- subtype the renamed instance type against the existential general
      g13 <- subtype' i instType' genType' g12
      -- Record the slot counter AFTER subtype'. Both toExistential and subtype'
      -- create ExistG entries that must be preserved: connectInstance (in
      -- resolveInstances) needs them in gammaContext to solve via access1.
      let slotAfterSubtype = gammaSlot g13
      -- check all implementations for this instance
      (g14, es') <- checkImplementations g13 genType' es

      -- Trim context back to post-subtype state. This removes entries
      -- from checkG (the main source of O(N^2) growth) while preserving
      -- existentials from toExistential and subtype' needed downstream.
      let gNext = gammaTrimAfter slotAfterSubtype g14

      -- Use the ORIGINAL general type, not the existentialized one above.
      -- Each instance gets its own existentials solved independently.
      (g15, rs') <- checkInstances gNext genType rs

      return (g15, (instType, es') : rs')

    -- check each implementation within each instance
    -- do not return modified Gamma state
    checkImplementations ::
      Gamma ->
      TypeU ->
      [AnnoS Int ManyPoly Int] ->
      MorlocMonad (Gamma, [AnnoS (Indexed TypeU) ManyPoly Int])
    checkImplementations g _ [] = return (g, [])
    checkImplementations g10 t (e@(AnnoS implGi _ _) : es) = do
      -- Temporarily remove any annotation that was propagated to this
      -- implementation's index via copyState/reindexExprI. An annotation
      -- like `mempty :: Str` on the usage site must not constrain checking
      -- of each instance implementation (e.g. the List instance's `[]`).
      implAnn <- MM.gets (Map.lookup implGi . stateAnnotations)
      MM.modify (\s -> s { stateAnnotations = Map.delete implGi (stateAnnotations s) })
      (g11, _, e') <- checkG g10 e t
      -- Restore
      case implAnn of
        Just ann' -> MM.modify (\s -> s { stateAnnotations = Map.insert implGi ann' (stateAnnotations s) })
        Nothing -> return ()

      -- check all the remaining implementations
      (g12, es') <- checkImplementations g11 t es

      -- return the final context and the applied expressions
      return (g12, applyGen g12 e' : es')

-- bare selector pattern (e.g., .0 or .1 used as a function argument, not applied)
synthE _ g0 (ExeS (PatCall (PatternStruct s))) = do
  (g1, datType) <- selectorType g0 s
  retType <- return $ case selectorGetter datType s of
    [] -> error "Illegal empty selection"
    [t] -> t
    ts -> BT.tupleU ts
  let ft = FunU [datType] retType
  return (g1, ft, ExeS (PatCall (PatternStruct s)))
-- This case will only be encountered in check, the existential generated here
-- will be subtyped against the type known from the VarS case.
synthE _ g (ExeS exe) = do
  let (g', t) = newvar "call_" g
  return (g', t, ExeS exe)
synthE _ g (BndS v) = do
  (g', t') <- case lookupE v g of
    (Just t) -> return (g, t)
    Nothing -> return $ newvar (unEVar v <> "_u") g
  return (g', t', BndS v)
synthE _ g (LetBndS v) = do
  (g', t') <- case lookupE v g of
    (Just t) -> return (g, t)
    Nothing -> return $ newvar (unEVar v <> "_u") g
  return (g', t', LetBndS v)
synthE _ g (CallS v) = do
  (g', t') <- case lookupE v g of
    (Just t) -> return (g, t)
    Nothing -> return $ newvar (unEVar v <> "_rec") g
  return (g', t', CallS v)
synthE _ g (LetS v e1 e2) = do
  (g1, t1, e1') <- synthG g e1
  let g2 = g1 ++> [AnnG v t1]
      -- Track known constant values for nat label resolution
      g2' = case tryEvalConst g2 (let AnnoS _ _ e = e1' in e) of
        Just val' -> g2 { gammaIntVals = Map.insert v val' (gammaIntVals g2) }
        Nothing -> g2
  (g3, t2, e2') <- synthG g2' e2
  return (g3, t2, LetS v e1' e2')
synthE i g (IfS cond thenE elseE) = do
  (g1, condType, cond') <- synthG g cond
  g2 <- subtype' i condType (VarU (TV "Bool")) g1
  (g3, t2, thenE') <- synthG g2 thenE
  (g4, t3, elseE') <- synthG g3 elseE
  scope <- MM.getGeneralScope i
  let t2' = apply g4 t2
      t3' = apply g4 t3
  -- Try strict subtype both directions first (zero-coercion path), then
  -- fall back to tryCoerce (which handles a -> ?a, a -> <E> a, and chains).
  -- The principle: a pure value can always be lifted into an effectful
  -- (or optional) wrapper, so mixed pure/effectful branches unify by
  -- lifting the pure one. Going the other way is unsafe and not allowed.
  case subtype scope t3' t2' g4 of
    Right g5 -> return (g5, apply g5 t2, IfS cond' thenE' elseE')
    Left _ -> case subtype scope t2' t3' g4 of
      Right g5 -> return (g5, apply g5 t3, IfS cond' thenE' elseE')
      Left _ -> case tryCoerce scope t3' t2' g4 of
        Just (coercions, g5) -> do
          elseE'' <- wrapAnnoCoercions i (apply g5 t3') coercions elseE'
          return (g5, apply g5 t2, IfS cond' thenE' elseE'')
        Nothing -> case tryCoerce scope t2' t3' g4 of
          Just (coercions, g5) -> do
            thenE'' <- wrapAnnoCoercions i (apply g5 t2') coercions thenE'
            return (g5, apply g5 t3, IfS cond' thenE'' elseE')
          Nothing -> MM.throwSourcedError i $
            "Branches of guard/if have incompatible types:"
            <> line <> "  then-branch: " <> prettyTypeU t2'
            <> line <> "  else-branch: " <> prettyTypeU t3'
synthE i g (DoBlockS e) = do
  (g1, t1, e1) <- synthG g e
  case apply g1 t1 of
    EffectU _ iT -> do
      -- Final expr is effectful: wrap it in EvalS so codegen forces the
      -- thunk and collectDoEffects picks up its effects alongside the
      -- non-final EvalS nodes.
      e1' <- wrapFinalEvalS i iT e1
      let collected = collectDoEffects e1'
      return (g1, EffectU collected iT, DoBlockS e1')
    bareT -> do
      -- Pure final: the block's type is <collected> bareT. An empty or
      -- smaller effect set is a subtype of any expected effect set, so no
      -- pure-to-effect lift is needed at the use site.
      let collected = collectDoEffects e1
      return (g1, EffectU collected bareT, DoBlockS e1)
synthE _ g (CoerceS coercion e) = do
  (g1, t1, e1) <- synthG g e
  return (g1, applyCoercion coercion t1, CoerceS coercion e1)
synthE i g (EvalS e) = do
  (g1, t1, e1) <- synthG g e
  let (g1', t1') = stripForallU g1 (apply g1 t1)
  case t1' of
    EffectU _ a -> return (g1', a, EvalS e1)
    ExistU _ _ _ -> do
      let (g2, bv) = tvarname g1' "effectInner_"
          bt = ExistU bv ([], Open) ([], Open)
          (g2b, ev) = tvarname g2 "effectVar_"
          thunkT = EffectU (EffectVar ev) bt
      g3 <- subtype' i (apply g2b t1') thunkT g2b
      return (g3, apply g3 bt, EvalS (applyGen g3 e1))
    t -> throwTypeError i $
      "Cannot force a non-effectful value (got type" <+> pretty t <> ")."
      <+> "The ! operator and non-final do-block statements require an effectful type <E> T."
      <+> "Pure expressions in a do-block should be bound via 'let' or moved to the final position."
synthE i g (IntrinsicS intr args) = do
  (g', argTypes, args') <- synthArgs g args
  g'' <- checkIntrinsicArgs i g' intr argTypes
  let (g''', expectedType) = intrinsicTypeG g'' intr
  return (g''', expectedType, IntrinsicS intr args')

-- | Strip ForallU wrappers by instantiating bound variables as existentials.
-- Follows the same pattern as `application` for ForallU.
stripForallU :: Gamma -> TypeU -> (Gamma, TypeU)
stripForallU g (ForallU v t) = stripForallU (g +> v) (substitute v t)
stripForallU g t = (g, t)

-- | Return type of a fully applied intrinsic, threading Gamma for fresh existentials
intrinsicTypeG :: Gamma -> Intrinsic -> (Gamma, TypeU)
intrinsicTypeG g IntrLoad =
  let (g', loadType) = newvar "load_" g
  in (g', EffectU ioEffectSet (OptionalU loadType))
intrinsicTypeG g IntrRead =
  let (g', readType) = newvar "read_" g
  in (g', OptionalU readType)
intrinsicTypeG g intr = (g, intrinsicType intr)

-- | Return type of a fully applied intrinsic (for intrinsics without fresh vars)
intrinsicType :: Intrinsic -> TypeU
intrinsicType IntrSave = EffectU ioEffectSet BT.unitU
intrinsicType IntrSaveM = EffectU ioEffectSet BT.unitU
intrinsicType IntrSaveJ = EffectU ioEffectSet BT.unitU
intrinsicType IntrLoad = EffectU ioEffectSet (OptionalU (ExistU (TV "load_a") ([], Open) ([], Open)))
intrinsicType IntrHash = BT.strU
intrinsicType IntrVersion = BT.strU
intrinsicType IntrCompiled = BT.strU
intrinsicType IntrLang = BT.strU
intrinsicType IntrSchema = BT.strU
intrinsicType IntrTypeof = BT.strU
intrinsicType IntrShow = BT.strU
intrinsicType IntrRead = OptionalU (ExistU (TV "read_a") ([], Open) ([], Open))
intrinsicType IntrDatafile = BT.strU

-- intrinsicArity is defined in Morloc.Namespace.Expr

-- | Synthesize types for a list of arguments
synthArgs ::
  Gamma ->
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad (Gamma, [TypeU], [AnnoS (Indexed TypeU) ManyPoly Int])
synthArgs g [] = return (g, [], [])
synthArgs g (a:as) = do
  (g1, t1, a') <- synthG g a
  (g2, ts, as') <- synthArgs g1 as
  return (g2, t1:ts, a':as')

-- | Check intrinsic argument count and types
checkIntrinsicArgs ::
  Int -> Gamma -> Intrinsic -> [TypeU] -> MorlocMonad Gamma
checkIntrinsicArgs i g intr argTypes = do
  let expected = intrinsicArity intr
      actual = length argTypes
  if actual /= expected
    then throwTypeError i $
      "@" <> pretty (intrinsicName intr) <+> "expects" <+> pretty expected
        <+> "arguments but got" <+> pretty actual
    else do
      -- Check specific argument types
      case (intr, argTypes) of
        -- @save/@savem/@savej: a -> Str -> {()}
        (IntrSave, [_, pathT]) -> subtype' i pathT BT.strU g
        (IntrSaveM, [_, pathT]) -> subtype' i pathT BT.strU g
        (IntrSaveJ, [_, pathT]) -> subtype' i pathT BT.strU g
        -- @load: Str -> {?a}
        (IntrLoad, [pathT]) -> subtype' i pathT BT.strU g
        -- @hash: a -> Str
        (IntrHash, [_]) -> return g
        -- @schema/@typeof: a -> Str (value ignored at runtime)
        (IntrSchema, [_]) -> return g
        (IntrTypeof, [_]) -> return g
        -- @show: a -> Str (any type accepted)
        (IntrShow, [_]) -> return g
        -- @read: Str -> ?a (arg must be Str)
        (IntrRead, [strT]) -> subtype' i strT BT.strU g
        -- @datafile: Str -> Str (path must be string literal)
        (IntrDatafile, [pathT]) -> subtype' i pathT BT.strU g
        -- compile-time constants: no args
        (IntrVersion, []) -> return g
        (IntrCompiled, []) -> return g
        (IntrLang, []) -> return g
        _ -> return g

etaExpandSynthE ::
  Int ->
  Gamma ->
  TypeU ->
  AnnoS (Indexed TypeU) ManyPoly Int ->
  AnnoS Int ManyPoly Int ->
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
etaExpandSynthE i g1 funType0 funExpr0 _f xs0 = do
  let normalType = normalizeType funType0
      numArgs = length xs0

  -- Check for arity errors before proceeding
  case normalType of
    FunU (length -> numParams) _
      | numArgs > numParams ->
          throwTypeError i $ "Invalid function application of type:\n  " <> prettyTypeU funType0
    _ -> return ()

  -- Process available args through application (no re-synthesis)
  (g2, funType1, inputExprs) <- application' i g1 xs0 normalType

  MM.sayVVV $
    "  funType1:" <+> pretty funType1
      <> "\n  inputExprs:" <+> list (map pretty inputExprs)

  case funType1 of
    FunU ts t -> case drop numArgs ts of
      -- full application
      [] -> return (g2, apply g2 t, AppS funExpr0 inputExprs)
      -- partial application: eta-expand without re-synthesis
      remainingParams -> do
        (g3, newVarsWithTypes) <-
          statefulMapM
            ( \g' tp -> do
                let (g'', v) = evarname g' "v"
                return (g'', (v, apply g2 tp))
            )
            g2
            remainingParams

        let newVars = map fst newVarsWithTypes
            newTypes = map snd newVarsWithTypes
            g4 = g3 ++> zipWith AnnG newVars newTypes

        -- Create typed variable references for the new params
        newVarExprs <-
          mapM
            ( \(v, tp) -> do
                idx <- MM.getCounterWithPos i
                return $ AnnoS (Idx idx tp) idx (BndS v)
            )
            newVarsWithTypes

        -- Build the application and lambda directly
        appIdx <- MM.getCounterWithPos i
        let retType = apply g4 t
            bodyExpr = AppS funExpr0 (inputExprs ++ newVarExprs)
            bodyAnno = AnnoS (Idx appIdx retType) appIdx bodyExpr
            fullType = FunU newTypes retType
        return (g4, fullType, LamS newVars bodyAnno)
    _ -> error "impossible"

expand :: Int -> Int -> Gamma -> ExprS Int f Int -> MorlocMonad (Gamma, ExprS Int f Int)
expand _ 0 g x = return (g, x)
expand parentIdx n g e@(AppS _ _) = do
  newIndex <- MM.getCounterWithPos parentIdx
  let (g', v') = evarname g "v"
  e' <- applyExistential parentIdx v' e
  let x' = LamS [v'] (AnnoS newIndex newIndex e')
  expand parentIdx (n - 1) g' x'
expand parentIdx n g (LamS vs' (AnnoS t ci e)) = do
  let (g', v') = evarname g "v"
  e' <- applyExistential parentIdx v' e
  expand parentIdx (n - 1) g' (LamS (vs' <> [v']) (AnnoS t ci e'))
expand _ _ g x = return (g, x)

applyExistential :: Int -> EVar -> ExprS Int f Int -> MorlocMonad (ExprS Int f Int)
applyExistential parentIdx v' (AppS f xs') = do
  newIndex <- MM.getCounterWithPos parentIdx
  return $ AppS f (xs' <> [AnnoS newIndex newIndex (BndS v')])
-- possibly illegal application, will type check after expansion
applyExistential parentIdx v' e = do
  appIndex <- MM.getCounterWithPos parentIdx
  varIndex <- MM.getCounterWithPos parentIdx
  return $ AppS (AnnoS appIndex appIndex e) [AnnoS varIndex varIndex (BndS v')]

application ::
  Int ->
  Gamma ->
  [AnnoS Int ManyPoly Int] -> -- the expressions that are passed to the function
  TypeU -> -- the function type
  MorlocMonad
    ( Gamma
    , TypeU -- output function type
    , [AnnoS (Indexed TypeU) ManyPoly Int] -- @e@, with type annotation
    )
--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
application i g0 es0 (FunU as0 b0) = do
  (g1, as1, es1, remainder) <- zipCheck i g0 es0 as0
  let funType = apply g1 $ FunU (as1 <> remainder) b0
  insetSay $ "remainder:" <+> vsep (map pretty remainder)
  return (g1, funType, es1)

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
application i g0 es (ForallU v s) = application' i (g0 +> v) es (substitute v s)
--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
application i g0 es (ExistU v@(TV s) ([], _) _) =
  case access1 v g0 of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just _ -> do
      let (g1, veas) = statefulMap (\g _ -> tvarname g "a_") g0 es
          (g2, vea) = tvarname g1 (s <> "o_")
          eas = [ExistU v' ([], Open) ([], Open) | v' <- veas]
          ea = ExistU vea ([], Open) ([], Open)
          f = FunU eas ea
      g3 <- case solveExistWith v f (map index eas ++ [index ea]) g2 of
        Left err -> throwTypeError i err
        Right Nothing -> return g2
        Right (Just g') -> return g'
      (g4, _, es', _) <- zipCheck i g3 es eas
      return (g4, apply g4 f, es')
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g0 of
      (Just (FunU ts t)) -> do
        (g1, ts', es', _) <- zipCheck i g0 es ts
        return (g1, apply g1 (FunU ts' t), es')
      (Just t) -> throwTypeError i $ "Application of term with non-functional type:\n   " <+> prettyTypeU t
      Nothing -> throwTypeError i $ "Expected function, but could not find type of term\n   " <+> pretty v
application i _ _ t =
  throwTypeError i $
    "Application of non-functional expression of type:" <+> prettyTypeU t

-- Tip together the arguments passed to an application
zipCheck ::
  Int ->
  Gamma ->
  [AnnoS Int ManyPoly Int] ->
  [TypeU] ->
  MorlocMonad
    ( Gamma
    , [TypeU]
    , [AnnoS (Indexed TypeU) ManyPoly Int]
    , [TypeU] -- remainder
    )
-- check the first elements, cdr down the remaining values
zipCheck i g0 (x0 : xs0) (t0 : ts0) = do
  (g1, t1, x1) <- checkG g0 x0 t0
  (g2, ts1, xs1, remainder) <- zipCheck i g1 xs0 ts0
  return (g2, t1 : ts1, x1 : xs1, remainder)
-- If there are fewer arguments than types, this may be OK, just partial application
zipCheck _ g0 [] ts = return (g0, [], [], ts)
-- If there are fewer types than arguments, then die
zipCheck i _ _ [] = MM.throwCompilerBugAt i "too many arguments in zipCheck"

foldCheck ::
  Gamma ->
  [AnnoS Int ManyPoly Int] ->
  TypeU ->
  MorlocMonad (Gamma, TypeU, [AnnoS (Indexed TypeU) ManyPoly Int])
foldCheck g [] t = return (g, t, [])
foldCheck g (x : xs) t = do
  (g', t', x') <- checkG g x t
  (g'', t'', xs') <- foldCheck g' xs t'
  return (g'', t'', x' : xs')

checkE ::
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
-- The single-arg form (e.g. `List Int`) treats the arg as the element
-- type. Guard against firing when the arg is non-Type-kinded (e.g.
-- `Foo (n :: Nat) = ...` instantiated as `Foo 3` -- here `3` is a Nat
-- dimension, not an element type, and forcing list elements to subtype
-- it produces a confusing kind mismatch). Falls through to the LstS
-- catch-all (line ~1230) which handles dimension checking via getNatArgs.
checkE i g1 (LstS (e : es)) (AppU v [t])
  | not (isNatExpr t || isStrExpr t || isRecExpr t
         || isListExpr t || isSetExpr t) = do
      (g2, t2, e') <- checkG g1 e t
      -- LstS [] will go to the normal Sub case
      (g3, t3, LstS es') <- checkE' i g2 (LstS es) (AppU v [t2])
      return (g3, t3, LstS (map (applyGen g3) (e' : es')))
checkE i g0 e0@(LamS vs body) t@(FunU as b)
  | length vs == length as = do
      let g1 = g0 ++> zipWith AnnG vs as
      (g2, t2, e2) <- checkG g1 body b

      let t3 = apply g2 (FunU as t2)
          e3 = applyCon g2 (LamS vs e2)

      return (g2, t3, e3)
  | otherwise = do
      (g', e') <- expand i (length as - length vs) g0 e0
      checkE' i g' e' t
checkE i g1 e1 (ForallU v a) = do
  checkE' i (g1 +> v) e1 (substitute v a)
checkE i g (IfS cond thenE elseE) t = do
  (g1, condType, cond') <- synthG g cond
  g2 <- subtype' i condType (VarU (TV "Bool")) g1
  (g3, t2, thenE') <- checkG g2 thenE t
  (g4, _, elseE') <- checkG g3 elseE (apply g3 t2)
  return (g4, apply g4 t2, IfS cond' thenE' elseE')
-- DoBlockS falls through to the general synth+subtype/coerce case (below).
-- synthE DoBlockS produces a flattened EffectU, and subtype handles effectful
-- finals via <E1> T <: <E2> T, while tryCoerce handles pure-final auto-lift.
checkE i g (EvalS e) t = do
  -- Synthesize first to get concrete EffectSet in annotations,
  -- then check the inner type against the expected type.
  -- This avoids creating an EffectVar that is never solved.
  (g1, t1, e1) <- synthG g e
  let (g1', t1') = stripForallU g1 (apply g1 t1)
  case t1' of
    EffectU _ a -> do
      g2 <- subtype' i a t g1'
      return (g2, apply g2 t, EvalS e1)
    ExistU _ _ _ -> do
      let (g2, bv) = tvarname g1' "effectInner_"
          bt = ExistU bv ([], Open) ([], Open)
          (g2b, ev) = tvarname g2 "effectVar_"
          thunkT = EffectU (EffectVar ev) bt
      g3 <- subtype' i (apply g2b t1') thunkT g2b
      g4 <- subtype' i (apply g3 bt) t g3
      return (g4, apply g4 t, EvalS (applyGen g4 e1))
    t' -> throwTypeError i $
      "Cannot force a non-effectful value (got type" <+> pretty t' <> ")."
      <+> "The ! operator and non-final do-block statements require an effectful type <E> T."
      <+> "Pure expressions in a do-block should be bound via 'let' or moved to the final position."

-- Resolve solved existentials so specific handlers (LstS, TupS, etc.) can match
checkE i g e t@(ExistU v _ _)
  | Just _ <- lookupU v g
  = checkE' i g e (apply g t)
-- Nat dimension checking for list literals against nat-parameterized aliases.
-- A multi-arg AppU like `Vector 4 Int32` (where `type Vector (n :: Nat) a =
-- List a`) does not match Rule A's single-arg pattern `(AppU v [t])`, so
-- without help the elements fall to the synth path and freeze as `Int`
-- before the subtype check, breaking literal defaulting. Before falling
-- back to synth+subtype, evaluate `b` through the alias scope: if it
-- reduces to a single-arg list shape `AppU _ [elemT]` whose element type
-- is a real element type (not Nat/Str/Rec/List/Set-kinded) and is not an
-- unsolved existential, re-enter the check against the reduced form so
-- Rule A fires and propagates `elemT` into the literals. Nat dimensions
-- are then checked against the *original* `b` so that `Vector 4 ...`
-- still length-checks at 4. Unsolved existentials in the element slot
-- fall through to the synth path for more flexible inference.
checkE i g1 e1@(LstS xs) b = do
  scope <- MM.getGeneralScope i
  let bEval = either (const b) id (TE.evaluateType scope b)
      canPropagate = case bEval of
        AppU _ [elemT] ->
          not (null xs)
          && not (isNatExpr elemT || isStrExpr elemT || isRecExpr elemT
                  || isListExpr elemT || isSetExpr elemT)
          && not (case elemT of { ExistU{} -> True; _ -> False })
        _ -> False
  if canPropagate
    then do
      (g2, _, e2) <- checkE' i g1 e1 bEval
      let natArgs = getNatArgs scope b
          anno = AnnoS (Idx i (apply g2 bEval)) i e2
      g3 <- checkListNatDims g2 natArgs anno
      return (g3, apply g3 b, e2)
    else do
      (g2, a, e2) <- synthE' i g1 e1
      let a' = apply g2 a
          b' = apply g2 b
      case subtype scope a' b' g2 of
        Right g3 -> do
          let natArgs = getNatArgs scope b'
              anno2 = AnnoS (Idx i a') i e2
          g4 <- checkListNatDims g3 natArgs anno2
          return (g4, apply g4 b', e2)
        Left err ->
          case tryCoerce scope a' b' g2 of
            Just (coercions, g3) -> do
              (finalExpr, _) <- foldlM
                (\(expr, currentType) coercion -> do
                  idx <- MM.getCounterWithPos i
                  let wrappedAnno = AnnoS (Idx idx currentType) i expr
                  return (CoerceS coercion wrappedAnno, applyCoercion coercion currentType))
                (e2, apply g3 a')
                coercions
              let natArgs = getNatArgs scope b'
                  anno3 = AnnoS (Idx i (apply g3 a')) i finalExpr
              g4 <- checkListNatDims g3 natArgs anno3
              return (g4, apply g4 b', finalExpr)
            Nothing -> MM.throwSourcedError i $
              "Type mismatch:"
              <> line <> "  expected: " <> prettyTypeU b'
              <> line <> "  inferred: " <> prettyTypeU a'
              <> line <> err
--   Sub (with coercion fallback)
-- Numeric literal defaulting: an `IntS` checked against any integer base
-- type (Int / Int8..Int64 / UInt / UInt8..UInt64) takes on that expected
-- type directly. Same for `RealS` against Real / Float32 / Float64. This
-- mirrors Haskell's polymorphic numeric literals: the literal `3` writes
-- the value into whichever integer-family slot the context supplies.
-- Without this, `3 :: Int8` would synthesize as Int and fail the
-- Int <: Int8 check now that the fixed-width integer types are
-- standalone base types rather than aliases of Int. Type aliases are
-- expanded through the scope so that user-defined names like
-- `type Char = UInt8` also accept integer literals directly.
-- A @let@ binding under a check-direction expected type. Without
-- this rule the let falls through to @checkEFallback@, which calls
-- @synthE@ on the whole let and then subtypes against @t@; that
-- synthesizes the let-BODY without a check direction, so any
-- literal-vs-alias coercion inside the body (e.g. the TupS rule
-- below firing on @(0, sub, sub) :: BTree Int@ with @sub :: BTree Int@
-- needing @BTree Int -> ?(BTree Int)@) never gets to run. Mirrors
-- @synthE LetS@ but uses @checkG@ for the body so the expected type
-- threads through to the literal that consumes it.
checkE i g (LetS v e1 e2) t = do
  (g1, t1, e1') <- synthG g e1
  let g2 = g1 ++> [AnnG v t1]
      g2' = case tryEvalConst g2 (let AnnoS _ _ e = e1' in e) of
        Just val' -> g2 { gammaIntVals = Map.insert v val' (gammaIntVals g2) }
        Nothing -> g2
  (g3, t2, e2') <- checkG g2' e2 t
  return (g3, t2, LetS v e1' e2')
-- A tuple literal checked against an expected type. If the expected
-- type reduces to a tuple shape with matching arity, check each
-- element against its corresponding expected position type. This
-- enables element-wise coercion at the literal's boundary -- a slot
-- declared as @?(LL Int)@ accepts a pure @LL Int@ value via the
-- standard @a -> ?a@ coercion that @checkG@ already supplies.
-- Without this rule, the checkEFallback path synthesizes the literal's
-- type as @(Int, LL Int)@ and runs strict subtype against
-- @LL Int = (Int, ?(LL Int))@, which fails at the @LL Int <: ?(LL Int)@
-- subgoal -- @tryCoerce@ only descends through OptionalU and won't fire
-- on a tuple-vs-tuple element mismatch from the outside.
--
-- C2.opt: compound literal (TupS/NamS) at an Optional expected type.
-- The literal cannot itself be @Null@, so it is unambiguously the
-- non-null inner T. Strip the Optional wrapper, recurse at T (which
-- routes through the regular TupS/NamS rules below), and wrap the
-- result in @CoerceToOptional@.
--
-- Without this, a nested literal like @(42, (7, (99, Null)))@ at type
-- @LL Int@ would fail: the outer rule expands @LL Int@ to its tuple
-- body and element-wise checks the inner tuple @(7, (99, Null))@ at
-- @?(LL Int)@, but the next-level TupS rule only matches
-- @AppU TupleN@-shaped expected types. The literal would synthesize
-- as @(Int, (Int, ?ex))@ and the subtype check against @?(LL Int)@
-- would fall through because @tryCoerce@ only walks Optional layers
-- at the outermost expression, not inside element checks.
checkE i g e@(TupS _) (OptionalU innerT) = checkOptionalLit i g e innerT
checkE i g e@(NamS _) (OptionalU innerT) = checkOptionalLit i g e innerT
checkE i g (TupS xs) t = do
  scope <- MM.getGeneralScope i
  let tApplied = apply g t
      -- Pull out the slot types if the type is already a tuple of the
      -- right arity. Else try one alias-reduction step (e.g. expand
      -- @BTree Int@ to its body's outer @Tuple3 [...]@) and re-check.
      -- Critical: use ONE-STEP @reduceType@, NOT full
      -- @evaluateType@. Full evaluation walks through the slot args
      -- recursively, expanding any record-typed argument (e.g.
      -- @Trie Int@ in a @(Str, Trie Int)@ slot) into its @NamU@ body.
      -- The subsequent element-wise check then faces an
      -- @AppU <: NamU@ subtype goal that no rule decomposes; the
      -- pretty-printer happens to render both sides identically
      -- (e.g. "Trie Int <: Trie Int") even though the typeU shapes
      -- differ, producing a baffling fall-through error.
      -- Only treat the head as a tuple when it actually IS the
      -- TupleN constructor for this arity. Matching on a bare
      -- @VarU _@ would also match user aliases that happen to have
      -- @length ts == length xs@ (e.g. @FixedPair (n :: Nat) a@
      -- applied as @FixedPair 2 Int@ has two type args, just like
      -- @Tuple2 Int Int@), and would element-wise check the literal
      -- against the alias's type args (@2@, @Int@) instead of the
      -- expanded body's slot types.
      tryMatch t' = case t' of
        AppU (VarU v) ts
          | v == BT.tuple (length xs)
          , length ts == length xs -> Just ts
        _ -> Nothing
      tsOpt = case tryMatch tApplied of
        Just ts -> Just ts
        Nothing -> TE.reduceType scope tApplied >>= tryMatch
  case tsOpt of
    Just ts -> do
      let go gAcc acc [] [] = return (gAcc, reverse acc)
          go gAcc acc (x:xrest) (et:trest) = do
            (g', _, x') <- checkG gAcc x et
            go g' (x' : acc) xrest trest
          go _ _ _ _ = error "unreachable: arity matched by guard"
      (gFinal, xs') <- go g [] xs ts
      -- Return the ORIGINAL expected type, not the stepped form.
      -- Keeping the alias identity (e.g. @BTree Int@) on the AST
      -- preserves the wire-format-declaration vs back-reference name
      -- agreement that downstream codegen depends on.
      return (gFinal, apply gFinal t, TupS xs')
    Nothing -> checkEFallback i g (TupS xs) t
-- C3: Record literal {k1=v1, ...} checked against a declared record type
-- NamU. Reorder the literal's fields to match declared field order, reject
-- literals whose key set differs from the declaration, and propagate
-- check direction into each field value so nested record literals
-- (e.g. {outer={inner={...}}}) also get reordered.
checkE i g (NamS rs) t = do
  scope <- MM.getGeneralScope i
  let tEval = either (const (apply g t)) id (TE.evaluateType scope (apply g t))
  case tEval of
    NamU _ _ _ declared | not (null declared) -> do
      let litKeys = map fst rs
          declKeys = map fst declared
          litSet = Set.fromList litKeys
          declSet = Set.fromList declKeys
      if litSet == declSet
        then do
          let rsMap = Map.fromList rs
              triples = [(k, rsMap Map.! k, fieldT) | (k, fieldT) <- declared]
          -- Recurse with check-direction so nested records get reordered.
          let go gAcc acc [] = return (gAcc, reverse acc)
              go gAcc acc ((k, v, fieldT) : rest) = do
                (gNext, _, v') <- checkG gAcc v fieldT
                go gNext ((k, v') : acc) rest
          (gFinal, fields) <- go g [] triples
          return (gFinal, apply gFinal tEval, NamS fields)
        else
          let missing = Set.toList (Set.difference declSet litSet)
              extra = Set.toList (Set.difference litSet declSet)
              missingPart =
                if null missing
                  then mempty
                  else line <> "  missing field(s):"
                    <+> hsep (punctuate "," (map pretty missing))
              extraPart =
                if null extra
                  then mempty
                  else line <> "  unknown field(s):"
                    <+> hsep (punctuate "," (map pretty extra))
           in MM.throwSourcedError i $
                "Record literal does not match declared type"
                <+> prettyTypeU tEval <> ":" <> missingPart <> extraPart
    _ -> checkEFallback i g (NamS rs) t
checkE i g (IntS si x) t = do
  scope <- MM.getGeneralScope i
  let tEval = either (const t) id (TE.evaluateType scope t)
  if BT.isIntegerBaseType t || BT.isIntegerBaseType tEval
    then return (g, t, IntS si x)
    else checkEFallback i g (IntS si x) t
checkE i g (RealS si x) t = do
  scope <- MM.getGeneralScope i
  let tEval = either (const t) id (TE.evaluateType scope t)
  if BT.isRealBaseType t || BT.isRealBaseType tEval
    then return (g, t, RealS si x)
    else checkEFallback i g (RealS si x) t
checkE i g1 e1 b = checkEFallback i g1 e1 b

checkEFallback ::
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
checkEFallback i g1 e1 b = do
  (g2, a, e2) <- synthE' i g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  scope <- MM.getGeneralScope i
  case subtype scope a' b' g2 of
    -- Pick the type with more record keys: when an open-keyed expected
    -- type (b) is checked against a synthesized record literal that has
    -- additional keys, the InstLReach rule for two record existentials
    -- only solves the literal's existential, leaving the expected
    -- existential's view stuck at its narrower keyset. Using a' here
    -- keeps the literal's full type on the AST so the schema generator
    -- emits all fields. For non-record cases a' and b' are equivalent
    -- post-substitution, so the choice is a no-op.
    Right g3 ->
      let aApplied = apply g3 a'
          bApplied = apply g3 b'
      in return (g3, pickWiderRecord aApplied bApplied, e2)
    Left err ->
      case tryCoerce scope a' b' g2 of
        Just (coercions, g3) -> do
          (finalExpr, _) <- foldlM
            (\(expr, currentType) coercion -> do
              idx <- MM.getCounterWithPos i
              let wrappedAnno = AnnoS (Idx idx currentType) i expr
              return (CoerceS coercion wrappedAnno, applyCoercion coercion currentType))
            (e2, apply g3 a')
            coercions
          return (g3, apply g3 b', finalExpr)
        Nothing -> MM.throwSourcedError i $
          "Type mismatch:"
          <> line <> "  expected: " <> prettyTypeU b'
          <> line <> "  inferred: " <> prettyTypeU a'
          <> line <> err

-- | Check a compound literal (TupS or NamS) against the inner type of an
-- Optional expected type, then wrap the result in @CoerceToOptional@.
-- See the comment at the @checkE _ _ (TupS _) (OptionalU _)@ clause for
-- the motivating case.
checkOptionalLit ::
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
checkOptionalLit i g e innerT = do
  (g', _, e') <- checkE' i g e innerT
  idx <- MM.getCounterWithPos i
  -- Propagate the general-typedef scope from the original index to the
  -- fresh one. Without this, downstream phases (e.g. Express's
  -- reduce-and-retry) call @MM.getGeneralScope idx@ and get an empty
  -- scope, so a type like @Pair Str@ that depends on the alias @Pair@
  -- being in scope fails to reduce -- surfacing as
  -- @Expected a tuple type, got: Pair Str@. Mirrors the pattern
  -- @propagateScope@ uses for concrete typedefs in Express.hs.
  MM.modify $ \s ->
    case GMap.yIsX i idx (stateGeneralTypedefs s) of
      Just g'' -> s { stateGeneralTypedefs = g'' }
      Nothing -> s
  let appliedInner = apply g' innerT
      innerAnno = AnnoS (Idx idx appliedInner) i e'
  return (g', apply g' (OptionalU innerT), CoerceS CoerceToOptional innerAnno)

-- | Choose the record-shaped type with the most keys. Non-record cases
-- fall back to the second argument (the standard "return the expected
-- type" convention used by checkEFallback).
pickWiderRecord :: TypeU -> TypeU -> TypeU
pickWiderRecord a b = case (recordKeyCount a, recordKeyCount b) of
  (Just na, Just nb) | na > nb -> a
  _ -> b
  where
    recordKeyCount (ExistU _ _ (rs, _)) = Just (length rs)
    recordKeyCount (NamU _ _ _ rs) = Just (length rs)
    recordKeyCount _ = Nothing

subtype' :: Int -> TypeU -> TypeU -> Gamma -> MorlocMonad Gamma
subtype' i a b g = do
  scope <- MM.getGeneralScope i
  insetSay $ parens (pretty a) <+> "<:" <+> parens (pretty b)
  case subtype scope a b g of
    (Left err') -> MM.throwSourcedError i err'
    (Right g') -> do
      let newDeferred = drop (length (gammaDeferred g)) (gammaDeferred g')
      mapM_ (\(t1, t2) ->
        MM.sayV $ "Warning: deferred Nat constraint:" <+> prettyTypeU t1 <+> "~" <+> prettyTypeU t2
        ) newDeferred
      return g'

-- | Extract nat-kinded argument values from a type, given the Scope.
-- For Matrix 2 4 Int with alias params [(m, KindNat), (n, KindNat), (a, KindType)]
-- and args [NatLitU 2, NatLitU 4, VarU Int], returns [NatLitU 2, NatLitU 4].
--
-- When a typedef wraps its Nat params via a body expression (e.g.
-- `type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int`), the surface
-- args of Foo do not match what the list value should be dimensioned
-- against -- the actual dimension lives at the Vector level as `(n+m)`.
-- So follow the typedef reduction chain and return the Nat args from
-- the DEEPEST level whose reduction still yields Nat args. This works
-- because reducing past the last Nat-parameterized typedef (e.g. into
-- `List a`) drops Nat info, while staying too shallow misses
-- intermediate wrapping.
getNatArgs :: Scope -> TypeU -> [TypeU]
getNatArgs scope (AppU (VarU v) args) =
  case Map.lookup v scope of
    Just ((params, _, _, _) : _) ->
      let myNatArgs = [arg | (Left (_, KindNat), arg) <- zip params args]
      in case TE.reduceType scope (AppU (VarU v) args) of
           Just t' -> case getNatArgs scope t' of
             [] -> myNatArgs   -- the reduction dropped Nat info; keep ours
             nas -> nas        -- recursion found a deeper Nat layer; use it
           Nothing -> myNatArgs
    _ -> []
getNatArgs _ _ = []

-- | Recursively check nat dimension constraints on list literals.
-- At each nesting level, checks length(xs) ~ natArg.
-- For checking (NatLitU): error on mismatch.
-- For inference (NatVarU): solves the variable via NatSolver.
checkListNatDims :: Gamma -> [TypeU] -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad Gamma
checkListNatDims g [] _ = return g
checkListNatDims g (nat:nats) (AnnoS _ idx (LstS xs)) = do
  g' <- subtype' idx (NatLitU (fromIntegral (length xs))) nat g
  foldlM (\g'' x -> checkListNatDims g'' nats x) g' xs
checkListNatDims g _ _ = return g

-- | Try to find a coercion chain from type a to type b.
-- Returns a list of coercions (inside-out) and the resulting gamma.
-- Recursion terminates when the target is not OptionalU.
tryCoerce :: Scope -> TypeU -> TypeU -> Gamma -> Maybe ([Coercion], Gamma)
tryCoerce scope a (OptionalU b) g =
  case subtype scope a b g of
    Right g' -> Just ([CoerceToOptional], g')
    Left _ -> case tryCoerce scope a b g of
      Just (cs, g') -> Just (CoerceToOptional : cs, g')
      Nothing -> Nothing
tryCoerce _ _ _ _ = Nothing

-- | Wrap an already-annotated subexpression in successive CoerceS layers,
-- producing a new annotated expression whose type reflects each coercion.
-- Mirrors the inner fold of 'checkEFallback' but operates on an AnnoS
-- (where the branch sub-expressions of IfS already live) rather than a
-- raw ExprS.
wrapAnnoCoercions ::
  Int                                       -- ^ source position index
  -> TypeU                                  -- ^ starting type of the AnnoS
  -> [Coercion]                             -- ^ coercions to apply, outermost last
  -> AnnoS (Indexed TypeU) ManyPoly Int     -- ^ subexpression to wrap
  -> MorlocMonad (AnnoS (Indexed TypeU) ManyPoly Int)
wrapAnnoCoercions i startT coercions anno0 = do
  (_, finalAnno) <- foldlM step (startT, anno0) coercions
  return finalAnno
  where
    step (curT, anno) coercion = do
      idx <- MM.getCounterWithPos i
      let newT = applyCoercion coercion curT
          newExpr = CoerceS coercion anno
      return (newT, AnnoS (Idx idx newT) i newExpr)

-- | Strip OptionalU wrappers that result from coercion.
-- Used in instance resolution to match the underlying type.
stripCoercionWrappers :: TypeU -> TypeU
stripCoercionWrappers (OptionalU t) = stripCoercionWrappers t
stripCoercionWrappers t = t

-- | Wrap the innermost (final) expression of a do-block body in EvalS.
-- Walks past LetS wrappers (introduced by desugarDo) and updates their type
-- annotations to the unwrapped inner type. A fresh index is allocated for
-- the new EvalS node so indexing metadata stays unique.
wrapFinalEvalS
  :: Int
  -> TypeU
  -> AnnoS (Indexed TypeU) ManyPoly Int
  -> MorlocMonad (AnnoS (Indexed TypeU) ManyPoly Int)
wrapFinalEvalS i iT (AnnoS (Idx gi _) ci (LetS v e1 e2)) = do
  e2' <- wrapFinalEvalS i iT e2
  return (AnnoS (Idx gi iT) ci (LetS v e1 e2'))
wrapFinalEvalS i iT final = do
  newIdx <- MM.getCounterWithPos i
  return (AnnoS (Idx newIdx iT) (annoSCtx final) (EvalS final))
  where
    annoSCtx (AnnoS _ c _) = c

-- | Collect effect labels from all EvalS nodes within a do-block body.
-- Deeply traverses the full expression tree to find nested EvalS nodes
-- (e.g., inside tuples, applications, let bindings).
collectDoEffects :: AnnoS (Indexed TypeU) f c -> EffectSet
collectDoEffects = go
  where
    go (AnnoS _ _ expr) = case expr of
      EvalS e -> effectOfAnno e `effUnion` go e
      LetS _ e1 e2 -> go e1 `effUnion` go e2
      AppS f args -> unions (go f : map go args)
      TupS es -> unions (map go es)
      LstS es -> unions (map go es)
      NamS rs -> unions (map (go . snd) rs)
      LamS _ e -> go e
      IfS c t e -> go c `effUnion` go t `effUnion` go e
      DoBlockS e -> go e
      CoerceS _ e -> go e
      IntrinsicS _ es -> unions (map go es)
      _ -> emptyEffectSet

    effectOfAnno (AnnoS (Idx _ (EffectU effs _)) _ _) = effs
    effectOfAnno _ = emptyEffectSet

    unions = foldl effUnion emptyEffectSet

    effUnion a b
      | a == emptyEffectSet = b
      | b == emptyEffectSet = a
      | a == b = a
      | otherwise = EffectUnion a b

-- | Compute the set of effects produced when an expression is evaluated.
--
-- Evaluation-time effects fire at every 'EvalS' (force) node that is
-- reachable from the root WITHOUT crossing a thunk boundary.  Thunks
-- (lambdas and do-blocks) carry their inner effects in their TYPE, so
-- those effects do not fire when the surrounding expression is reduced;
-- only forcing the thunk later via 'EvalS' would trigger them.
--
-- This is the inference half of the rule in 'spec/types/effects.md'
-- under "Effect Inference".  The companion check
-- 'checkEffectCoverage' verifies that the inferred set is a subset of
-- the declared effect set on the surrounding signature.
inferExprEffects :: AnnoS (Indexed TypeU) f c -> EffectSet
inferExprEffects = go
  where
    go (AnnoS _ _ expr) = case expr of
      EvalS e -> effectOfAnno e `effUnion` go e
      LetS _ e1 e2 -> go e1 `effUnion` go e2
      AppS f args -> unions (go f : map go args)
      TupS es -> unions (map go es)
      LstS es -> unions (map go es)
      NamS rs -> unions (map (go . snd) rs)
      IfS c t e -> go c `effUnion` go t `effUnion` go e
      CoerceS _ e -> go e
      IntrinsicS _ es -> unions (map go es)
      -- Thunk boundaries: their effects live in the type, not in the
      -- evaluation of the surrounding expression.
      DoBlockS _ -> emptyEffectSet
      LamS _ _ -> emptyEffectSet
      _ -> emptyEffectSet

    effectOfAnno (AnnoS (Idx _ (EffectU effs _)) _ _) = effs
    effectOfAnno _ = emptyEffectSet

    unions = foldl effUnion emptyEffectSet

    effUnion a b
      | a == emptyEffectSet = b
      | b == emptyEffectSet = a
      | a == b = a
      | otherwise = EffectUnion a b

-- | Strip lambda layers from the body in lockstep with function-arrow
-- layers on the expected type.  Returns the innermost expected return
-- type and the innermost body (after lambdas are peeled).  Used to
-- locate the "actual return position" where evaluation-time effects
-- of the function body must be covered.
--
-- A function whose declared type has N argument arrows and whose body
-- is a single LamS binding N parameters peels cleanly.  Anything else
-- (point-free, partial lambda, mismatched arity) returns as-is and
-- relies on the surrounding structural check to have rejected the
-- shape.
peelLambdaLayers
  :: TypeU
  -> AnnoS (Indexed TypeU) f c
  -> (TypeU, AnnoS (Indexed TypeU) f c)
peelLambdaLayers (FunU args ret) body@(AnnoS _ _ (LamS vs inner))
  | length args == length vs = peelLambdaLayers ret inner
  | otherwise = (FunU args ret, body)
peelLambdaLayers t body = (t, body)

-- | After structural typechecking, verify that the evaluation-time
-- effects of a function body are a subset of the effects the declared
-- return type permits.  Fires the widening-rule rejection described in
-- 'spec/types/effects.md' under "Effect Checking".
--
-- Catches forces that escape do-blocks, the case the type-level
-- subtype rule cannot see because EvalS strips the effect wrapper
-- from the inner type.  The caret is placed on the bound force
-- ('<-' / EvalS) that produced the uncovered effect; only when no
-- single node can be blamed does it fall back to the surrounding
-- index ('idx').
--
-- Variable-aware: the call sites pass @apply g' annType@, and 'apply'
-- substitutes solved effect-row variables ('applyEff', see the
-- Applicable TypeU instance), so the declared row already reflects any
-- effect-variable solutions.  The comparison is then a concrete-label
-- subset: a universally-quantified tail variable contributes no
-- concrete label and cannot cover an always-performed body effect,
-- which would unsoundly hide it.
checkEffectCoverage
  :: Int
  -> TypeU
  -> AnnoS (Indexed TypeU) f Int
  -> MorlocMonad ()
checkEffectCoverage idx declared body = do
  let (innerDecl, innerBody) = peelLambdaLayers declared body
      declEffs = case innerDecl of
        EffectU effs _ -> resolveEffectSet effs
        _ -> Set.empty
      inferred = resolveEffectSet (inferExprEffects innerBody)
      uncovered = Set.difference inferred declEffs
  if Set.null uncovered
    then return ()
    else
      let blameIdx = case uncoveredEvalIdxs uncovered innerBody of
            (i : _) -> i
            [] -> idx
       in MM.throwSourcedError blameIdx $
            "Body produces uncovered effect(s)" <+> renderEffectLabels uncovered
              <> "; declared type permits" <+> renderEffectLabels declEffs <> "."
              <+> "Either add the effect to the signature or sequence the operation in a do-block."
  where
    renderEffectLabels s
      | Set.null s = "<>"
      | otherwise = "<" <> hcat (punctuate ", " (map pretty (Set.toList s))) <> ">"

-- | Concrete indices of the force ('EvalS') nodes whose forced
-- expression carries an effect in the uncovered set, in pre-order.
-- Traverses only the nodes 'inferExprEffects' counts as
-- evaluation-time (thunk boundaries -- lambdas and do-blocks -- are
-- not crossed), so the first result is the force the user must fix.
uncoveredEvalIdxs
  :: Set.Set EffectLabel
  -> AnnoS (Indexed TypeU) f Int
  -> [Int]
uncoveredEvalIdxs uncovered = go
  where
    go (AnnoS _ ci expr) = case expr of
      EvalS e
        | not (Set.null (Set.intersection (effLabels e) uncovered)) -> [ci]
        | otherwise -> go e
      LetS _ e1 e2 -> go e1 ++ go e2
      AppS f' args -> go f' ++ concatMap go args
      TupS es -> concatMap go es
      LstS es -> concatMap go es
      NamS rs -> concatMap (go . snd) rs
      IfS c t e -> go c ++ go t ++ go e
      CoerceS _ e -> go e
      IntrinsicS _ es -> concatMap go es
      _ -> []

    effLabels (AnnoS (Idx _ (EffectU effs _)) _ _) = resolveEffectSet effs
    effLabels _ = Set.empty

-- helpers

-- apply context to a AnnoS
applyGen ::
  (Functor gf, Traversable f, Applicable g) =>
  Gamma ->
  AnnoS (gf g) f c ->
  AnnoS (gf g) f c
applyGen g = mapAnnoSG (fmap (apply g))

applyCon ::
  (Functor gf, Traversable f, Applicable g) =>
  Gamma ->
  ExprS (gf g) f c ->
  ExprS (gf g) f c
applyCon g = mapExprSG (fmap (apply g))

evaluateAnnoSTypes ::
  (Traversable f) => AnnoS (Indexed TypeU) f Int -> MorlocMonad (AnnoS (Indexed TypeU) f Int)
evaluateAnnoSTypes = mapAnnoSGM resolve
  where
    resolve :: Indexed TypeU -> MorlocMonad (Indexed TypeU)
    resolve (Idx m t) = do
      scope <- getScope m
      case TE.evaluateType scope t of
        (Left (SystemError e)) -> MM.throwSourcedError m e
        (Left e) -> MM.throwError e
        (Right tu) -> return (Idx m tu)

    getScope :: Int -> MorlocMonad Scope
    getScope i = do
      globalMap <- MM.gets stateGeneralTypedefs
      case GMap.lookup i globalMap of
        GMapNoFst -> return Map.empty
        GMapNoSnd -> return Map.empty
        GMapJust scope -> return scope

---- debugging

synthE' ::
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
synthE' i g x = do
  enter "synthE"
  insetSay $ "synthesize type for: " <> peakSExpr x
  r@(g', t, _) <- synthE i g x
  leave "synthE"
  seeGamma g'
  insetSay $ "synthesized type = " <> pretty t
  return r

checkE' ::
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
checkE' i g x t = do
  enter "checkE"
  insetSay $ "check if expr: " <> peakSExpr x
  insetSay $ "matches type: " <> pretty t
  r@(g', t', _) <- checkE i g x t
  leave "checkE"
  seeGamma g'
  seeType t'
  return r

application' ::
  Int ->
  Gamma ->
  [AnnoS Int ManyPoly Int] ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , [AnnoS (Indexed TypeU) ManyPoly Int]
    )
application' i g es t = do
  enter "application"
  seeType t
  insetSay $ "es:" <+> list [peakSExpr e | (AnnoS _ _ e) <- es]
  r@(g', t', _) <- application i g es t
  leave "application"
  seeGamma g'
  seeType t'
  return r

-- | Try to reduce an expression to a compile-time constant. Handles:
--   - Integer literals
--   - Tuple literals (recursively)
--   - Let-bound and lambda-bound variable references (via gammaIntVals)
--   - Let expressions (with local constant propagation)
--   - Index accessors on tuples: .0 (5, 6, 7) => 5
-- Returns Nothing for anything involving foreign function calls,
-- non-constant variables, or unsupported expression forms.
tryEvalConst :: Gamma -> ExprS g f c -> Maybe ConstVal
tryEvalConst _ (IntS _ n) = Just (ConstInt n)
tryEvalConst _ (StrS s) = Just (ConstStr s)
tryEvalConst g (LetBndS v) = Map.lookup v (gammaIntVals g)
tryEvalConst g (BndS v) = Map.lookup v (gammaIntVals g)
tryEvalConst g (TupS es) = ConstTup <$> mapM (\(AnnoS _ _ e) -> tryEvalConst g e) es
tryEvalConst g (LstS es) = ConstList <$> mapM (\(AnnoS _ _ e) -> tryEvalConst g e) es
tryEvalConst g (LetS v (AnnoS _ _ e1) (AnnoS _ _ e2)) = do
  val' <- tryEvalConst g e1
  tryEvalConst (g { gammaIntVals = Map.insert v val' (gammaIntVals g) }) e2
-- Index accessor on tuple literal or known tuple: .i (a, b, c) => element i
tryEvalConst g (AppS (AnnoS _ _ (ExeS (PatCall (PatternStruct
  (SelectorIdx (idx, SelectorEnd) []))))) [AnnoS _ _ inner])
  = case tryEvalConst g inner of
      Just (ConstTup vs) | idx >= 0, idx < length vs -> Just (vs !! idx)
      _ -> Nothing
-- Lambda application: (\x -> body) arg => beta-reduce
tryEvalConst g (AppS (AnnoS _ _ (LamS vs (AnnoS _ _ body))) args)
  | length vs == length args = do
    vals <- mapM (\(AnnoS _ _ e) -> tryEvalConst g e) args
    let g' = g { gammaIntVals = foldl (\m (v', val') -> Map.insert v' val' m) (gammaIntVals g) (zip vs vals) }
    tryEvalConst g' body
tryEvalConst _ _ = Nothing

-- | Try to reduce an expression to an integer constant.
tryEvalInt :: Gamma -> ExprS g f c -> Maybe Integer
tryEvalInt g e = case tryEvalConst g e of
  Just (ConstInt n) -> Just n
  _ -> Nothing

tryExtractIntPre :: Gamma -> AnnoS Int ManyPoly Int -> Maybe Integer
tryExtractIntPre g (AnnoS _ _ e) = tryEvalInt g e

-- | Try to reduce an expression to a string constant.
tryEvalStr :: Gamma -> ExprS g f c -> Maybe Text
tryEvalStr g e = case tryEvalConst g e of
  Just (ConstStr s) -> Just s
  _ -> Nothing

tryExtractStrPre :: Gamma -> AnnoS Int ManyPoly Int -> Maybe Text
tryExtractStrPre g (AnnoS _ _ e) = tryEvalStr g e

-- | Try to reduce an expression to a list of string constants. Used to
-- lift @l:[Str]@ label arguments into ListLitU [StrLitU ...] at the call
-- site so subsequent @r - l@, @r # l@, etc. reduce.
tryEvalStrList :: Gamma -> ExprS g f c -> Maybe [Text]
tryEvalStrList g e = case tryEvalConst g e of
  Just (ConstList vs) -> mapM asStr vs
  _ -> Nothing
  where
    asStr (ConstStr s) = Just s
    asStr _ = Nothing

tryExtractStrListPre :: Gamma -> AnnoS Int ManyPoly Int -> Maybe [Text]
tryExtractStrListPre g (AnnoS _ _ e) = tryEvalStrList g e

-- | Resolve nat / str labels from literal arguments.
-- When a function has labeled nat params (e.g., m:Int -> Tensor1 m Real)
-- and the corresponding arguments are int literals or let-bound ints,
-- inject NatVarU solutions into gamma so the return type gets concrete
-- dimensions. Same for Str labels (e.g., f:Str -> Tagged f a) — extract
-- string literals from the corresponding args and inject StrVarU solutions
-- into gammaStrSubs. See plans/tables/05-labels-as-type-vars.md.
resolveNatLabels ::
  AnnoS Int ManyPoly Int ->  -- the function expression (pre-synthesis)
  TypeU ->                    -- the synthesized (renamed) function type
  [AnnoS Int ManyPoly Int] -> -- the arguments
  Gamma -> Gamma
resolveNatLabels (AnnoS _ _ (VarS _ (MonomorphicExpr (Just et) _))) funType args g
  | not (Map.null labels) =
    let -- Nat labels: match each original NatVarU name to its renamed counterpart
        origNvs = nub (collectNatVarNames (etype et))
        renamedNvs = nub (collectNatVarNames funType)
        natRenMap = Map.fromList (zip origNvs renamedNvs)
        natSolutions = Map.fromList
          [ (renamedVar, NatLitU n)
          | (origVar, argIdx) <- Map.toList labels
          , Just renamedVar <- [Map.lookup origVar natRenMap]
          , argIdx < length args
          , Just n <- [tryExtractIntPre g (args !! argIdx)]
          ]
        -- Str labels: same pattern, but for StrVarU vars and string literals
        origSvs = nub (collectStrVarNames (etype et))
        renamedSvs = nub (collectStrVarNames funType)
        strRenMap = Map.fromList (zip origSvs renamedSvs)
        strSolutions = Map.fromList
          [ (renamedVar, StrLitU s)
          | (origVar, argIdx) <- Map.toList labels
          , Just renamedVar <- [Map.lookup origVar strRenMap]
          , argIdx < length args
          , Just s <- [tryExtractStrPre g (args !! argIdx)]
          ]
        -- List labels: l:[Str] params bind l as a List Str-kinded type
        -- variable. Lift a literal [Str] argument into a ListLitU of
        -- StrLitU at the call site so subsequent r - l, r # l, etc.
        -- reduce.
        origLvs = nub (collectListVarNames (etype et))
        renamedLvs = nub (collectListVarNames funType)
        listRenMap = Map.fromList (zip origLvs renamedLvs)
        listSolutions = Map.fromList
          [ (renamedVar, ListLitU (map StrLitU ss))
          | (origVar, argIdx) <- Map.toList labels
          , Just renamedVar <- [Map.lookup origVar listRenMap]
          , argIdx < length args
          , Just ss <- [tryExtractStrListPre g (args !! argIdx)]
          ]
    in g { gammaNatSubs = Map.union natSolutions (gammaNatSubs g)
         , gammaStrSubs = Map.union strSolutions (gammaStrSubs g)
         , gammaListSubs = Map.union listSolutions (gammaListSubs g)
         }
  where
    labels = enatLabels et
resolveNatLabels _ _ _ g = g

peakSExpr :: ExprS Int ManyPoly Int -> MDoc
peakSExpr UniS = "UniS"
peakSExpr NullS = "NullS"
peakSExpr (VarS v (MonomorphicExpr mayT _)) = "VarS" <+> pretty v <+> "::" <+> maybe "?" pretty mayT
peakSExpr (VarS v (PolymorphicExpr cls _ t _)) = "VarS" <+> pretty cls <+> " => " <+> pretty v <+> "::" <+> pretty t
peakSExpr (BndS v) = "BndS" <+> pretty v
peakSExpr (AppS _ xs) = "AppS" <+> "nargs=" <> pretty (length xs)
peakSExpr (LamS vs _) = "LamS" <> tupled (map pretty vs)
peakSExpr (LstS xs) = "LstS" <> "n=" <> pretty (length xs)
peakSExpr (TupS xs) = "TupS" <> "n=" <> pretty (length xs)
peakSExpr (NamS rs) = "NamS" <> encloseSep "{" "}" "," (map (pretty . fst) rs)
peakSExpr (RealS _ x) = "RealS" <+> viaShow x
peakSExpr (IntS _ x) = "IntS" <+> pretty x
peakSExpr (LogS x) = "LogS" <+> pretty x
peakSExpr (StrS x) = "StrS" <+> pretty x
peakSExpr (ExeS exe) = "ExeS" <+> pretty exe
peakSExpr (LetS v _ _) = "LetS" <+> pretty v
peakSExpr (LetBndS v) = "LetBndS" <+> pretty v
peakSExpr (CallS v) = "CallS" <+> pretty v
peakSExpr (DoBlockS _) = "DoBlockS"
peakSExpr (EvalS _) = "EvalS"
peakSExpr (CoerceS _ _) = "CoerceS"
peakSExpr (IfS _ _ _) = "IfS"
peakSExpr (IntrinsicS intr _) = "@" <> pretty (intrinsicName intr)
