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
module Morloc.Frontend.Typecheck (typecheck, typecheckRoot, typecheckRootAt, validate, resolveTypes, evaluateAnnoSTypes, peakSExpr) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as MT
import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal
import qualified Morloc.Typecheck.NatSolver as NS

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

-- | Resolve all deferred numeric literals at end of typecheck.
--
-- The queue contains @(idx, v, kind)@ tuples: an @IntS@ or @RealS@ at
-- source index @idx@ was checked against unsolved existential @v@. The
-- resolution is two-pass so that the outcome is independent of the
-- order args were processed:
--
-- (1) RealDefault first. A @RealS@ literal cannot demote to an integer
--     type, so any existential touched by RealS must be pinned to
--     @Real@ before IntDefault entries get a chance to default it.
-- (2) IntDefault second. An @IntS@ literal accepts either an integer
--     base type or a real base type (integer-to-real promotion), so by
--     the time we get here any existential already solved to @Real@
--     by phase 1 is fine, and any still-unsolved existential gets the
--     @Int@ default.
--
-- For each entry: if @v@ is already solved to a compatible base type,
-- accept; if solved to an incompatible type (Bool / Str / etc.), emit
-- a sourced error; if unsolved, solve with the phase's default.
resolvePendingNumLits :: Gamma -> [(Int, TVar, NumLitKind)] -> MorlocMonad Gamma
resolvePendingNumLits g0 entries = do
  -- the class obligations on existentials, closed under superclasses
  closed <- superclassClosure [c | c@(Constraint _ [_]) <- gammaConstraints g0]
  let numericVars = Set.fromList
        [ v
        | Constraint cls [t] <- closed
        , cls == BT.numericClass
        , ExistU v _ _ <- [apply g0 t]
        ]
  let (intLits0, realLits0) = partition (\(_, _, k) -> k == IntDefault) entries
      -- an integer literal whose type must be Numeric defaults like a real one
      (numericInts, intLits) = partition (\(_, v, _) -> Set.member (chainEnd v) numericVars) intLits0
      realLits = realLits0 ++ numericInts
  g1 <- foldlM (resolveOne BT.isRealBaseType BT.realU) g0 realLits
  foldlM (resolveOne acceptableForInt BT.intU) g1 intLits
  where
    chainEnd v = case apply g0 (ExistU v ([], Open) ([], Open)) of
      ExistU v' _ _ -> v'
      _ -> v

    -- An IntS literal can adopt any integer base type OR any real base
    -- type (via promotion). The check-mode rule for @IntS@ above mirrors
    -- this acceptance.
    acceptableForInt t = BT.isIntegerBaseType t || BT.isRealBaseType t

    -- A literal may inhabit a type alias or a newtype whose wire parent is a
    -- numeric base. The check-mode rules for @IntS@/@RealS@ already walk that
    -- chain (evaluate the alias, then follow the wire parent); a DEFERRED
    -- literal must be judged by the same standard, or the identical program
    -- is accepted or rejected purely on whether the literal happened to meet
    -- its type directly or through an existential.
    baseOKThroughAliases :: (TypeU -> Bool) -> TypeU -> MorlocMonad Bool
    baseOKThroughAliases baseOK t = do
      scope <- MM.getGeneralScope
      let tEval = either (const t) id (TE.evaluateType scope t)
          tWire = TE.wireParentRoot scope tEval
      return (baseOK t || baseOK tEval || baseOK tWire)

    resolveOne :: (TypeU -> Bool) -> TypeU -> Gamma -> (Int, TVar, NumLitKind) -> MorlocMonad Gamma
    resolveOne baseOK defaultT g (i, v, _) =
      -- Chain-walk via apply: when the deferred existential @v@ was
      -- unified with another existential (e.g. a lambda-parameter
      -- existential), @lookupU v@ only returns the immediate
      -- solution, leaving the chain head as another @ExistU@. @apply@
      -- walks the chain to its end -- either a concrete type or the
      -- innermost unsolved existential, both of which the cases below
      -- handle. Effect wrappers @<E> T@ are also peeled: a do-block's
      -- final expression carries an effectful return type, and the
      -- literal living inside that expression should be judged on the
      -- payload type, not on the @EffectU@ envelope.
      let original = ExistU v ([], Open) ([], Open)
          resolved = peelWrappers (apply g original)
      in case resolved of
           ExistU vEnd _ _ ->
             -- Chain end is still unsolved: apply the default there.
             -- Solving the chain end propagates through every
             -- intermediate link via subsequent applies.
             case solveExist vEnd defaultT g of
               Left err        -> MM.throwSystemError err
               Right Nothing   -> return g
               Right (Just g') -> return g'
           t -> do
             ok <- baseOKThroughAliases baseOK t
             if ok
               then return g
               else MM.throwSourcedError i $
                 "Numeric literal cannot have type " <> prettyTypeU t

    -- | Strip @EffectU@ and @OptionalU@ wrappers from a type. A
    -- numeric literal embedded in a do-block has its containing
    -- expression typed @<E> T@ where T is what the literal should
    -- inhabit; similarly for optional return positions where the
    -- literal flows through a @?T@ coercion. Pre-existing checkE
    -- rules for these wrappers handle the standalone case, so by the
    -- time we get here only the chain-end wrappers remain.
    peelWrappers :: TypeU -> TypeU
    peelWrappers (EffectU _ t)  = peelWrappers t
    peelWrappers (OptionalU t)  = peelWrappers t
    peelWrappers t              = t

typecheck ::
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheck = mapM (typecheckRoot >=> assertStructuralTypesExpanded)

-- | Every alias of an arrow, effect or optional type is expanded where it is
-- declared (see 'expandStructuralAliases'), and later stages read those
-- types by their shape, so none may remain on a typed node.
assertStructuralTypesExpanded :: AnnoS (Indexed TypeU) Many Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
assertStructuralTypesExpanded = mapAnnoSGM check
  where
    check g@(Idx i t) = do
      scope <- MM.getGeneralScope
      case structuralAliasesIn scope t of
        [] -> return g
        (v : _) -> MM.throwCompilerBugAt i $
          "the alias" <+> squotes (pretty v)
          <+> "of a function, effect or optional type was not expanded:" <+> pretty t

-- | Check one tree on its own: its outermost 'VarS' is the definition being
-- checked, and nothing outside the tree constrains it.
typecheckRoot :: AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckRoot = typecheckWith Nothing

-- | Check one tree against a type, as a use of it at that type is checked.
typecheckRootAt :: TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckRootAt t = typecheckWith (Just t)

typecheckWith :: Maybe TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckWith expected = run
  where
    run :: AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
    run e0 = do
      -- The general scope is needed to expand type aliases inside the
      -- primitive constraints discharged below.
      scope <- MM.getGeneralScope
      -- standardize names for lambda bound variables (e.g., x0, x1 ...)
      let g0 = emptyGamma
      (g1raw, _, e1) <- case expected of
        Nothing -> synthG g0 e0
        Just t -> checkG g0 e0 t

      -- Resolve numeric literals that were checked against unsolved
      -- existentials BEFORE instance resolution. Typeclass dispatch
      -- (@(+)@, @(<=)@, etc.) needs concrete types; a literal like
      -- @3 <= 5@ leaves the comparator's @a@ existential unsolved
      -- after argument processing, so without applying the @Int@
      -- default here @resolveInstances@ would fail with "No instance
      -- found for Ord::<=". Order: synth -> numeric defaults ->
      -- instance resolution -> Nat-deferral recheck.
      g1 <- resolvePendingNumLits g1raw (gammaPendingNumLits g1raw)

      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g1:"
      seeGamma g1
      insetSay "========================================================"
      e2 <- coerceLiteralsToOptional . mapAnnoSG (fmap normalizeType) . applyGen g1 $ e1

      (g2, e3) <- resolveInstances g1 e2
      let g3 = apply g2 g2

      -- Re-check deferred kind constraints now that existentials are solved,
      -- each one reported at the term where it arose (DIM-6).
      let rootIndex = case e0 of AnnoS gi _ _ -> gi
          site p = Map.findWithDefault rootIndex p (gammaDeferredAt g3)
          natEquation p@(t1, t2) = case (typeUToNatExpr (apply g3 t1), typeUToNatExpr (apply g3 t2)) of
            (Just a, Just b) -> Just (site p, p, (a, b))
            _ -> Nothing
          natEquations = mapMaybe natEquation (gammaDeferred g3)
          trivial (a, b) = NS.normalize (NS.NatSub a b) == NS.NatSOP []
      forM_ (natComponents [eq | eq@(_, _, ab) <- reverse natEquations, not (trivial ab)]) $ \eqs ->
        case (NS.naturalSolution [ab | (_, _, ab) <- eqs], eqs) of
          (Just True, _) -> return ()
          (_, []) -> return ()
          (answer, (i, _, _) : _) ->
            MM.throwSourcedError i $ vsep
              [ case answer of
                  Just False -> "These kind-level equations have no solution in the natural numbers:"
                  _ -> "The compiler cannot decide whether these kind-level equations have a solution in the natural numbers:"
              , vsep [ let (d1, d2) = prettyTypeUPair (apply g3 t1) (apply g3 t2)
                       in "  " <> d1 <+> "~" <+> d2
                     | (_, (t1, t2), _) <- eqs ]
              ]
      let natPairs = [p | (_, p, _) <- natEquations]
      remaining <- fmap concat . flip mapM (filter (`notElem` natPairs) (gammaDeferred g3)) $ \p ->
        case recheckDeferred g3 { gammaDeferred = [p] } of
          Left err -> MM.throwSourcedError (site p) err
          Right rs -> return [(site p, r) | r <- rs]
      case remaining of
        [] -> return ()
        ((i0, _) : _) ->
          MM.throwSourcedError i0 $ vsep
            [ "The compiler cannot decide whether these kind-level equations hold:"
            , vsep [ let (d1, d2) = prettyTypeUPair t1 t2
                     in "  " <> d1 <+> "~" <+> d2
                   | (_, (t1, t2)) <- remaining ]
            , "  Annotate the term with concrete values."
            ]

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
      case dischargeConstraints scope g3 of
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
      --
      -- Renormalise after applyGen: substituting a partial-applied
      -- existential solution back through the tree can produce nested
      -- AppU shapes (@AppU (AppU H [n]) [a]@); downstream codegen
      -- (weave, evaluateStep, inferConcreteType) only understands the
      -- flat canonical shape.
      -- A Nat is a natural number, but Nat subtraction is not clamped, so a
      -- signature can compute a negative dimension. It typechecks, builds,
      -- and then dies at run time as a malformed schema string naming
      -- neither the dimension nor the signature it came from. Every Nat is
      -- solved by now, so reject it here.
      let finalE = mapAnnoSG (fmap normalizeType) (applyGen g3 e3)
      _ <- mapAnnoSGM checkFinalType finalE
      return finalE

    -- Problems that only become visible once every kind variable is solved.
    -- Both are errors the user would otherwise meet at run time, or not at
    -- all: an operator that could not reduce leaves no trace in a program
    -- whose result type nobody wrote down.
    checkFinalType :: Indexed TypeU -> MorlocMonad (Indexed TypeU)
    checkFinalType ix@(Idx i t) = case negativeNats t of
      (n : _) ->
        MM.throwSourcedError i $
          "A dimension may not be negative, but this one is " <> pretty n
            <> ". Nat subtraction is not clamped, so check the arithmetic"
            <> " in the signature."
      [] -> case collidingUnions t of
        (ks : _) ->
          MM.throwSourcedError i $
            "Columns cannot be combined because both sides have: "
              <> hcat (punctuate ", " (map pretty ks))
        [] -> return ix

    -- A union of two fully known rows that share a key. `reduceRecUnion`
    -- leaves such a union symbolic rather than reducing it, so nothing
    -- catches the collision unless the caller annotates the result and
    -- forces the solver to look.
    collidingUnions :: TypeU -> [[Text]]
    collidingUnions = go
      where
        go (RecUnionU a b)
          | Just ka <- groundRowKeys a
          , Just kb <- groundRowKeys b
          , shared <- [k | k <- ka, k `elem` kb]
          , not (null shared) = [shared]
        go (LitU (LRec fs)) = concatMap (go . snd) fs
        go (LitU (LList es)) = concatMap go es
        go (LitU (LSet es)) = concatMap go es
        go (LitU _) = []
        go (OpU _ ts) = concatMap go ts
        go (FunU ts t') = concatMap go (t' : ts)
        go (AppU t' ts) = concatMap go (t' : ts)
        go (NamU _ _ ps rs) = concatMap go (ps <> map snd rs)
        go (ForallU _ t') = go t'
        go (EffectU _ t') = go t'
        go (OptionalU t') = go t'
        go (LabeledU _ t') = go t'
        go (ExistU _ (ts, _) (rs, _)) = concatMap go (ts <> map snd rs)
        go _ = []

    -- The field names of a row, when every field is known.
    groundRowKeys :: TypeU -> Maybe [Text]
    groundRowKeys (LitU (LRec fs)) = Just (map fst fs)
    groundRowKeys (RecExtendU k _ rest) = (k :) <$> groundRowKeys rest
    groundRowKeys _ = Nothing

    -- Every negative Nat literal in a type, in no particular order.
    negativeNats :: TypeU -> [Integer]
    negativeNats = go
      where
        go (LitU (LNat n)) = [n | n < 0]
        go (LitU (LRec fs)) = concatMap (go . snd) fs
        go (LitU (LList es)) = concatMap go es
        go (LitU (LSet es)) = concatMap go es
        go (LitU _) = []
        -- An expression with a free variable may be non-negative for some
        -- value of it (DIM-6); only a ground one has a value to check.
        go t@(OpU _ ts) = case typeUToNatExpr t of
          Just ne
            | NS.isGround ne -> [c | NS.NatSOP [NS.NatProduct c _] <- [NS.normalize ne], c < 0]
            | otherwise -> []
          Nothing -> concatMap go ts
        go (FunU ts t) = concatMap go (t : ts)
        go (AppU t ts) = concatMap go (t : ts)
        go (NamU _ _ ps rs) = concatMap go (ps <> map snd rs)
        go (ForallU _ t) = go t
        go (EffectU _ t) = go t
        go (OptionalU t) = go t
        go (LabeledU _ t) = go t
        go (ExistU _ (ts, _) (rs, _)) = concatMap go (ts <> map snd rs)
        go VarU{} = []
        go KVarU{} = []
        go VoidU{} = []
        go ListVarU{} = []
        go ListVoidU = []
        go SetVarU{} = []
        go SetVoidU = []

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
    f (ConS tv n k xs) = ConS tv n k (map resolveTypes xs)
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
  gscope <- MM.getGeneralScope
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
          -- `isSubtypeOfOpen` rather than `isSubtypeOf2`: at this point the
          -- occurrence type may still carry an unsolved existential -- the
          -- result of `unpack x` that nothing downstream constrains -- and
          -- an empty gamma cannot instantiate it, so every candidate would
          -- be rejected and the user told no instance exists.
          isCompatible t = isSubtypeOfOpen scope t gtEval
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
    f _ g0 (ConS tv n i xs) = do
      (g1, xs') <- statefulMapM resolveInstances g0 xs
      return (g1, ConS tv n i xs')
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
    connectInstance singleGroup g0 (AnnoS (Idx _ t) _ _ : es) = do
      scope <- MM.getGeneralScope
      case subtype scope (stripCoercionWrappers gt) t g0 of
        (Left _) -> connectInstance singleGroup g0 es
        (Right g1)
          | singleGroup -> connectInstance singleGroup g1 es
          | otherwise   -> connectInstance singleGroup g0 es

-- | Filter typeclass instances to only those reachable from @gt@ by
-- walking its alias chain to a matching root-headed candidate.
-- Invariant 3 (frontend) guarantees every instance head is a newtype,
-- a primitive, or a bare type variable -- never a transparent alias.
-- So all cousins on a tree converge at the same root; walking @gt@'s
-- chain alone is enough to find the shared instance. Walking the
-- instance's chain too would erase the nominal boundary between
-- sibling newtypes (Deque a / Array a / Vector n a) that all share
-- @List a@ as their wire-parent body but each own their own instances.
--
-- A recursive alias changes with every step, so the walk is bounded: each
-- step removes a level of the non-recursive alias chains, which are no
-- longer than the scope, pushes a recursive alias's occurrences at least a
-- level deeper, past where any candidate's head can look, or strips one of
-- the call-site type's own wrappers.
filterByAliasChain :: Scope -> TypeU -> [(EType, [a])] -> [(EType, [a])]
filterByAliasChain scope gt0 candidates = go fuel gt0
  where
    fuel = Map.size scope + typeDepth gt0 + maximum (0 : [typeDepth t | (EType t _ _ _, _) <- candidates]) + 1
    go n currentGt =
      case [x | x@(EType t _ _ _, _) <- candidates, compatibleTypeU currentGt t] of
        [] | n <= 0 -> []
        [] -> case TE.reduceType scope currentGt of
          Just gt' | gt' /= currentGt -> go (n - 1) gt'
          _ -> case TE.reduceTypeLeaves scope currentGt of
            Just gt' | gt' /= currentGt -> go (n - 1) gt'
            _ -> case currentGt of
              -- Strip coercion wrappers: a -> ?a, a -> <E> a
              OptionalU t -> go (n - 1) t
              EffectU _ t -> go (n - 1) t
              _ -> []  -- nothing reachable
        matches -> matches
    typeDepth :: TypeU -> Int
    typeDepth t = 1 + maximum (0 : map typeDepth (typeUChildren t))

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
    go b1 b2 (AppU h1 ps1) (AppU h2 ps2)
      | length ps1 == length ps2 =
          go b1 b2 h1 h2
            && all (uncurry (go b1 b2)) (zip ps1 ps2)
      -- Partial-application match: the side with more args has a
      -- prefix-applied head that should unify with the shorter side's
      -- head. Mirrors the partial-app rule in 'subtype' so an
      -- instance like @Foldable (Vector n)@ (whose method's
      -- @Vector n a@ has two args at the leaf) is recognised as
      -- compatible with a call site at @f a@ (one arg) when @f@ is
      -- an existential or bound TVar.
      | length ps1 > length ps2
      , let k = length ps1 - length ps2
            (ps1prefix, ps1suffix) = splitAt k ps1
      = go b1 b2 (AppU h1 ps1prefix) h2
          && all (uncurry (go b1 b2)) (zip ps1suffix ps2)
      | length ps2 > length ps1
      , let k = length ps2 - length ps1
            (ps2prefix, ps2suffix) = splitAt k ps2
      = go b1 b2 h1 (AppU h2 ps2prefix)
          && all (uncurry (go b1 b2)) (zip ps1 ps2suffix)
      | otherwise = False
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

-- | Result type of bracket slicing on a container. For @AppU h args@,
-- if the first arg is Nat-kinded (Vector-style: @Vector n a@), replace
-- it with 'NatVoidU' (the erased-phantom-Nat wildcard) so the output
-- length is independent of the input length AND two slice results
-- compare equal at the Nat slot. A fresh existential here would never
-- get solved -- there is no constraint that pins it -- and would
-- propagate to downstream stages as @forall bslice_dim_N . _@, breaking
-- instance dispatch and Eq comparisons between independent slices.
-- 'NatVoidU' is the right granularity: "this Nat is unknown; treat as
-- wildcard against any other Nat" (see Typecheck/Internal.hs NatVoidU
-- subtype rule). Non-Nat-parameterized containers (List-style) pass
-- through.
bracketSliceResultType :: Gamma -> TypeU -> (Gamma, TypeU)
bracketSliceResultType g t@(AppU h args) = case args of
  (a : rest) | isKindTypeU' a ->
    (g, AppU h (NatVoidU : rest))
  _ -> (g, t)
bracketSliceResultType g t = (g, t)

isKindTypeU' :: TypeU -> Bool
isKindTypeU' (NatLitU _) = True
isKindTypeU' (NatVarU _) = True
isKindTypeU' (NatAddU _ _) = True
isKindTypeU' (NatMulU _ _) = True
isKindTypeU' (NatSubU _ _) = True
isKindTypeU' (NatDivU _ _) = True
isKindTypeU' NatVoidU = True
isKindTypeU' _ = False

-- prepare a general, indexed typechecking error
throwTypeError :: Int -> MDoc -> MorlocMonad a
throwTypeError i msg = MM.throwSourcedError i ("General type error:" <+> msg)

-- A selector pattern (e.g. .0, .field) targets tuples and records only.
-- The selectorType existential unifies with List<a> by accident (both are
-- single-parameter AppU), but at runtime the pool reads garbage. Reject
-- the application here with a clear message and a hint.
rejectListSelectorTarget :: Int -> Selector -> TypeU -> MorlocMonad ()
rejectListSelectorTarget i s t = do
  scope <- MM.getGeneralScope
  let t' = either (const t) id (TE.evaluateType scope t)
  case t' of
    AppU (VarU (TV "List")) _ ->
      MM.throwSourcedError i $
        "Index getter" <+> pretty s
          <+> "requires a tuple or record, got a list."
          <+> "Use 'head' or '!!' for list access."
    -- The evaluated form, so an alias cannot carry a `data` type past
    -- the check the way the raw one would.
    _ -> rejectSumSelectorTarget i s t'

-- | Refuse a getter aimed at a `data` type.
--
-- A projection names a position, but which positions exist depends on
-- which constructor a value carries, and that is not known until the
-- value is matched. A getter would therefore be well typed only when
-- every arm happened to agree at that position -- a property of one
-- declaration rather than of the type, and one that a later arm silently
-- breaks. Matching is the eliminator: a `|` clause fixes the constructor
-- first, so its fields have definite types.
rejectSumSelectorTarget :: Int -> Selector -> TypeU -> MorlocMonad ()
rejectSumSelectorTarget i s t = do
  scope <- MM.getGeneralScope
  case TE.dataHead scope (TE.whnf scope t) of
    Just (v, _, _) ->
      MM.throwSourcedError i $
        "Getter" <+> pretty s <+> "cannot be applied to" <+> pretty v
          <> ", which is a `data` type. Which fields exist depends on the"
          <+> "constructor, so match on it with a `|` clause instead."
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
  annotation <- fmap (scopeType (isJust (gammaRigid g)) (gammaScoped g)) . Map.lookup j <$> MM.gets stateAnnotations
  -- Use the body's concrete index (j) for error caret positions: i is
  -- often the variable-reference / export-list site and would mis-locate
  -- body errors.
  (g', t', e') <- case annotation of
    Nothing -> checkE' j g e t
    (Just annType) -> do
      gAnn <- subtype' j annType t g
      checkE' j gAnn e t
  let annotatedBody = AnnoS (Idx i t') j e'
  -- When the user declared a signature at this site, verify that the
  -- body's evaluation-time effects are a subset of those the signature
  -- permits (EFF-10).  Inner
  -- positions without an annotation flow through ordinary structural
  -- subtyping, which already rejects narrowing on EffectU types.
  case annotation of
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

-- | The declared parameters of a type, as (var, kind) pairs.
-- | The constructor a tag test names. The desugar always emits a bare
-- reference here, so anything else is a compiler bug rather than a user
-- error.
-- | The text of a literal string argument. The constructor-pattern lowering
-- always puts one here, so anything else is a compiler bug, not user error.
literalStr :: AnnoS Int ManyPoly Int -> Maybe Text
literalStr (AnnoS _ _ (StrS t)) = Just t
literalStr _ = Nothing

unAnnoSE :: AnnoS Int ManyPoly Int -> ExprS Int ManyPoly Int
unAnnoSE (AnnoS _ _ e) = e

-- | Resolve a constructor name against the scrutinee's own `data`
-- declaration, yielding that constructor's field types with the type's
-- parameters instantiated. Rejects a name that belongs to some other type,
-- which is the check that keeps a clause set from mixing two `data` types.
checkCtorBelongs :: Int -> Gamma -> TypeU -> Text -> MorlocMonad [TypeU]
checkCtorBelongs i g subjectT n = do
  scope <- MM.getGeneralScope
  let resolved = apply g subjectT
  case TE.dataHead scope (TE.whnf scope resolved) of
    Just (tv, args, ctors)
      | Just fs <- lookup n ctors ->
          let params = [p | (p, _) <- typeParamsOf scope tv]
              inst t = foldr (\(p, a) acc -> substituteTVar p a acc) t (zip params args)
           in return (map inst fs)
    _ ->
      MM.throwSourcedError i $
        squotes (pretty n) <+> "is not a constructor of" <+> pretty resolved

typeParamsOf :: Scope -> TVar -> [(TVar, Kind)]
typeParamsOf scope v = case Map.lookup v scope of
  Just ((ps, _, _, _, _) : _) -> [pk | Left pk <- ps]
  _ -> []

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
-- A `data` constructor. The name is unique across the program, so it
-- determines its type on its own -- no annotation, no instance search.
--
-- A constructor that takes arguments is checked against the argument types
-- its declaration gave it. The type's own parameters are instantiated with
-- fresh existentials first and shared across every argument and the result,
-- which is what ties `Some x :: Opt a` to the `a` of `x`.
synthE i g0 (ConS tv n ord xs) = do
  scope <- MM.getGeneralScope
  let params = [p | (p, _) <- typeParamsOf scope tv]
      (g1, freshArgs) = statefulMap (\g _ -> newvar (unTVar tv <> "_") g) g0 params
      sub = zip params freshArgs
      inst = foldr (\(p, fresh) t -> substituteTVar p fresh t) `flip` sub
      resultT = case freshArgs of
        [] -> VarU tv
        _ -> AppU (VarU tv) freshArgs
  declaredArgs <- case lookup n =<< TE.dataCtors scope (TE.whnf scope (VarU tv)) of
    Just as -> return (map inst as)
    Nothing ->
      MM.throwSourcedError i $
        "Constructor" <+> squotes (pretty n) <+> "is not a constructor of type"
          <+> pretty tv
  when (length declaredArgs /= length xs) $
    MM.throwSourcedError i $
      "Constructor" <+> squotes (pretty n) <+> "takes"
        <+> pretty (length declaredArgs) <+> "arguments but was given"
        <+> pretty (length xs)
  (g2, xs') <- statefulMapM (\g (x, t) -> do
                                (g', _, x') <- checkG g x t
                                return (g', x')) g1 (zip xs declaredArgs)
  return (g2, apply g2 resultT, ConS tv n ord xs')

-- A directly applied setter is a redex: reduce it so the setter's own
-- rule below sees the receiver. See 'reduceSetterRedex'.
synthE i g0 e | Just e' <- reduceSetterRedex e = synthE i g0 e'

-- synthesize a string interpolation pattern
synthE i g (AppS f@(AnnoS _ _ (ExeS (PatCall (PatternText _ _)))) es) = do
  (g1, _, f1) <- synthG g f
  (g2, _, es1, _) <- zipCheck i g1 es (take (length es) (repeat BT.strU))
  return (g2, BT.strU, AppS f1 es1)

-- handle getter patterns
synthE _ _ (AppS (AnnoS _ _ (ExeS (PatCall (PatternStruct _)))) []) = error "Unreachable application pattern to no data"
-- Unified PatternStruct with bracket steps in the Selector. The
-- desugar produces this form for any accessor whose chain or group
-- contains a bracket-index / bracket-slice -- including
-- @.(.0.[i], .1) f@. The App's args are the bracket bounds in DFS
-- order followed by the receiver; the receiver synthesises against
-- the same selectorType-based structural existential as the pure-
-- field case, with the extra bracket bounds checked individually.
synthE i g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) args)
  | bracketArity s > 0 = do
      let n = bracketArity s
      when (length args /= n + 1) $
        MM.throwSourcedError i $
          "PatternStruct with bracket-containing selector expects"
          <+> pretty (n + 1) <+> "args, got" <+> pretty (length args)
      let bracketArgs = take n args
          rcvArg      = last args
      -- Generate fresh existentials for each bracket arg's type. We
      -- intentionally do NOT pin to Int/?Int here: the same flexibility
      -- as the standalone PatternBracketIndex/Slice handlers (codegen
      -- inserts the per-language __to_index__ cast).
      let nameForArgIx j = MT.pack ("_pat_bracket_arg_" <> show j <> "_")
          (gArgs, bracketArgTypes) =
            foldl (\(gAcc, ts) j ->
                     let (gNext, ty) = newvar (nameForArgIx j) gAcc
                     in (gNext, ts ++ [ty]))
                  (g0, [])
                  ([0 .. n - 1] :: [Int])
      -- Receiver synth + IFile transparency.
      (gsyn, rcvSyn, rcvExpr) <- synthG gArgs rcvArg
      let rcvResolved = apply gsyn rcvSyn
      (gReady, expectedInner, isIFile) <- case (extractKey rcvResolved, rcvResolved) of
        (v, AppU _ (innerType : _))
          | v == BT.ifileVar -> return (gsyn, innerType, True)
        _                    -> return (gsyn, rcvResolved, False)
      -- Generate the structural existential the selector expects of
      -- the receiver (or its IFile inner type) and unify. selectorGetter
      -- is computed on the EXISTENTIAL form (structural), and the
      -- existential's solutions then propagate via 'apply'.
      (gSel, datType) <- selectorType gReady s
      gUnified <- subtype' fgidx datType expectedInner gSel
      rejectListSelectorTarget fgidx s (apply gUnified datType)
      -- Result type: tuple of leaf types (or single type if just one).
      let retType = case selectorGetter datType s of
            []  -> error "Illegal empty selection"
            [t] -> t
            ts  -> BT.tupleU ts
      -- Type-check each bracket arg against its fresh existential.
      (gArgChecked, bracketArgsChecked) <- typecheckBracketArgs gUnified bracketArgs bracketArgTypes
      let resolvedRetType = apply gArgChecked retType
          rcvFunInputType = if isIFile then rcvResolved else apply gArgChecked expectedInner
          ft = FunU (map (apply gArgChecked) bracketArgTypes <> [rcvFunInputType]) resolvedRetType
          f1 = AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall (PatternStruct s)))
      return (gArgChecked, resolvedRetType, AppS f1 (bracketArgsChecked <> [rcvExpr]))
  where
    typecheckBracketArgs
      :: Gamma
      -> [AnnoS Int ManyPoly Int]
      -> [TypeU]
      -> MorlocMonad (Gamma, [AnnoS (Indexed TypeU) ManyPoly Int])
    typecheckBracketArgs g [] [] = return (g, [])
    typecheckBracketArgs g (e:es) (t:ts) = do
      (g', _, e') <- checkG g e t
      (g'', es') <- typecheckBracketArgs g' es ts
      return (g'', e' : es')
    typecheckBracketArgs _ _ _ =
      error "typecheckBracketArgs: length mismatch (should be unreachable)"
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) [e0]) = do
  -- IFile pattern transparency: when the receiver synthesizes to
  -- `IFile a`, the selector operates on `a`'s structure (the user
  -- sees the file's value directly). The result still gets the
  -- field's type (not wrapped in IFile). At codegen, Express.hs
  -- detects the IFile receiver and emits an IFileField intrinsic
  -- that walks to the field in the file. Non-IFile receivers stay
  -- on the existing structural-existential unification path.
  (gsyn, rcvSyn, e0Syn) <- synthG g0 e0
  let rcvResolved = apply gsyn rcvSyn
  case (extractKey rcvResolved, rcvResolved) of
    (v, AppU _ (innerType : _)) | v == BT.ifileVar -> do
      -- Generate the structural existential and unify with the
      -- IFile's inner type. The function's input stays as IFile a
      -- so codegen can detect it.
      (g1, structType) <- selectorType gsyn s
      retType <- return $ case selectorGetter structType s of
        [] -> error "Illegal empty selection"
        [t] -> t
        ts -> BT.tupleU ts
      g2 <- subtype' fgidx structType innerType g1
      rejectListSelectorTarget fgidx s (apply g2 innerType)
      let ft = FunU [rcvResolved] (apply g2 retType)
          f1 = AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall (PatternStruct s)))
      return (g2, apply g2 retType, AppS f1 [e0Syn])
    _ -> do
      -- Existing path: generate structural existential, check the
      -- receiver against it.
      (g1, datType) <- selectorType gsyn s
      retType <- return $ case selectorGetter datType s of
        [] -> error "Illegal empty selection"
        [t] -> t
        ts -> BT.tupleU ts
      let ft = FunU [datType] retType
      (g2, _, e') <- checkG g1 e0 datType
      rejectListSelectorTarget fgidx s (apply g2 datType)
      let f1 = AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall (PatternStruct s)))
      return (g2, apply g2 retType, AppS f1 [e'])

-- Setter synth. Allocate fresh slot existentials, seat them in
-- @outputType@ via @selectorSetter@, then check the receiver so its
-- field types pin the placeholders. Only then are the set-values
-- checked, so an unadorned integer literal like @.count = 42@ can
-- inhabit a fixed-width @Int32@ slot via @checkE (IntS ...)@ instead
-- of freezing to @Int@ under @synthG@. Same order-swap shape as the
-- @IntrMap@ synth (see below).
synthE i g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) (e0 : es0)) = do
  let (g1, slotVars) = statefulMap (\g _ -> newvar "set_slot_" g) g0 es0
  (g2, structType) <- selectorType g1 s
  outputType <- case selectorSetter slotVars s structType of
    Just t -> return t
    Nothing -> MM.throwSourcedError i "this setter's paths do not fit its receiver"
  (g3, datType, e1) <- checkG g2 e0 outputType
  rejectListSelectorTarget fgidx s (apply g3 datType)
  (g4, es1) <-
    statefulMapM
      (\g (e, slotT) -> do
         (g', _, e') <- checkG g e (apply g slotT)
         return (g', e'))
      g3
      (zip es0 slotVars)
  let setTypes = map (apply g4) slotVars
      receiverType = apply g4 datType
      -- The setter returns its receiver with the written fields' types
      -- replaced. Rebuilding from the receiver, rather than from the
      -- selector's own structural type, is what keeps the fields the
      -- selector does not name, and keeps the receiver's key order,
      -- which is the record's layout on the wire.
      resultType =
        fromMaybe (apply g4 outputType) (selectorSetter setTypes s receiverType)
      patternType = FunU (receiverType : setTypes) resultType
      f1 = AnnoS (Idx fgidx patternType) fcidx (ExeS (PatCall (PatternStruct s)))
  return (g4, resultType, AppS f1 (e1 : es1))
synthE _ g (ExeS (PatCall (PatternText s ss@(length -> n)))) = do
  let t = FunU (take n (repeat BT.strU)) BT.strU
  return (g, t, ExeS (PatCall (PatternText s ss)))

-- Bracket-index pattern (xs[i]). Args: [index, receiver]. The index type
-- is left as a fresh existential so the user can supply any integral
-- type: the pool-dispatch codegen path resolves a per-instance
-- __to_index__ cast, the pure-runtime path requires an integral wire
-- type. The receiver shape is "container of elemType" with both the
-- container constructor and the element type left as existentials so
-- chained record/bracket accessors propagate constraints correctly.
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall PatternBracketIndex))) [iExpr, rcvExpr]) = do
  -- IFile pattern transparency: if rcvExpr synthesizes to IFile a,
  -- peel the IFile and continue with `a` as the receiver structure
  -- for the bracket pattern. The function's argument keeps its IFile
  -- wrapper so Express.hs can detect it; the result is the bracketed
  -- element type. Pure-typed (IO effect is implicit; Serialize.hs
  -- effect-wrap thunks the runtime call). Non-IFile receivers stay
  -- on the existing structural-existential path.
  (gsyn, rcvSyn, rcvSynExpr) <- synthG g0 rcvExpr
  let rcvResolved = apply gsyn rcvSyn
  case (extractKey rcvResolved, rcvResolved) of
    (v, AppU _ (innerType : _)) | v == BT.ifileVar -> do
      let (g1, elemType) = newvar "bidx_elem_" gsyn
          (g2, fExist)   = newvar "bidx_f_" g1
          (g3, idxType)  = newvar "bidx_idx_" g2
          expectedInner  = AppU fExist [elemType]
      g4 <- subtype' fgidx expectedInner innerType g3
      (g5, _, i') <- checkG g4 iExpr idxType
      let resultType = apply g5 elemType
          ft = FunU [apply g5 idxType, rcvResolved] resultType
          f1 = AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall PatternBracketIndex))
      return (g5, resultType, AppS f1 [i', rcvSynExpr])
    _ -> do
      let (g1, elemType) = newvar "bidx_elem_" gsyn
          (g2, fExist)   = newvar "bidx_f_" g1
          (g3, idxType)  = newvar "bidx_idx_" g2
          expectedRcv    = AppU fExist [elemType]
      g4 <- subtype' fgidx expectedRcv rcvResolved g3
      (g5, _, i')   <- checkG g4 iExpr idxType
      let resultType = apply g5 elemType
          ft = FunU [apply g5 idxType, apply g5 expectedRcv] resultType
          f1 = AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall PatternBracketIndex))
      return (g5, resultType, AppS f1 [i', rcvSynExpr])
synthE i _ (AppS (AnnoS _ _ (ExeS (PatCall PatternBracketIndex))) args) =
  MM.throwSourcedError i $
    "A bracket index expects 2 args, got" <+> pretty (length args)

-- Bracket-slice pattern (xs[i:j:k]). Args: [start, stop, step, receiver].
-- Each bound is checked against a fresh bare existential so user
-- expressions of any type unify naturally (Null bounds synth as
-- OptionalU v and solve the existential to that; integer literals
-- solve it to Int; user-annotated values solve it to their stated
-- type). The "optional" nature of each slot is structural -- carried
-- by NullS vs non-Null at the AST level -- not type-level. Codegen
-- enforces semantic validity: pool dispatch resolves each non-Null
-- bound's __to_index__ instance; pure-runtime evaluation requires a
-- known wire integer. Result type is the receiver type with the outer
-- Nat parameter replaced by a fresh existential (for Nat-parameterized
-- containers like Vector); otherwise the receiver type is preserved.
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall PatternBracketSlice))) [startE, stopE, stepE, rcvExpr]) = do
  (g1, rcvType, rcv') <- synthG g0 rcvExpr
  let rcvType' = apply g1 rcvType
      (g1a, bareResultType) = bracketSliceResultType g1 rcvType'
      (g2, startTy) = newvar "bslice_start_" g1a
      (g3, stopTy)  = newvar "bslice_stop_"  g2
      (g4, stepTy)  = newvar "bslice_step_"  g3
  (g5, _, s1) <- checkG g4 startE startTy
  (g6, _, s2) <- checkG g5 stopE  stopTy
  (g7, _, s3) <- checkG g6 stepE  stepTy
  -- IFile slice: peel the IFile and use the inner type as the slice
  -- result (which must itself be a list type for the slice to make
  -- sense; codegen errors if not). Pure-typed for chain-composability
  -- reasons (see PatternBracketIndex above). Non-IFile receivers use
  -- the existing bracketSliceResultType logic.
  let resolvedRcv = apply g7 rcvType'
      isIFile = extractKey resolvedRcv == BT.ifileVar
      resultType =
        if isIFile
          then case resolvedRcv of
            AppU _ (innerType : _) -> innerType
            _ -> apply g7 bareResultType
          else apply g7 bareResultType
      ft = FunU [apply g7 startTy, apply g7 stopTy, apply g7 stepTy, rcvType']
              resultType
      f1 = AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall PatternBracketSlice))
  return (g7, resultType, AppS f1 [s1, s2, s3, rcv'])
synthE i _ (AppS (AnnoS _ _ (ExeS (PatCall PatternBracketSlice))) args) =
  MM.throwSourcedError i $
    "A bracket slice expects 4 args, got" <+> pretty (length args)

-- Bare bracket patterns (when used as values, not applied). The
-- function types match the AppS arities above; index and bound types
-- are polymorphic existentials, resolved at the application site.
synthE _ g (ExeS (PatCall PatternBracketIndex)) = do
  let (g1, a) = newvar "bidx_elem_" g
      (g2, r) = newvar "bidx_recv_" g1
      (g3, i) = newvar "bidx_idx_"  g2
      ft = FunU [i, r] a
  return (g3, ft, ExeS (PatCall PatternBracketIndex))
synthE _ g (ExeS (PatCall PatternBracketSlice)) = do
  let (g1, a) = newvar "bslice_recv_"  g
      (g2, s) = newvar "bslice_start_" g1
      (g3, t) = newvar "bslice_stop_"  g2
      (g4, p) = newvar "bslice_step_"  g3
      ft = FunU [OptionalU s, OptionalU t, OptionalU p, a] a
  return (g4, ft, ExeS (PatCall PatternBracketSlice))

--   -->E0
synthE _ g (AppS f []) = do
  (g1, t1, f1) <- synthG g f
  return (g1, t1, AppS f1 [])

--   -->E
synthE i g0 (AppS f xs0) = do
  -- synthesize the type of the function
  (g1, funType0, funExpr0) <- synthG g0 f

  -- Resolve nat labels: if the function has labeled nat params (m@Int syntax)
  -- and corresponding args are int literals, inject NatVarU solutions into gamma
  let g1' = resolveNatLabels f funType0 xs0 g1

  etaExpandSynthE i g1' funType0 funExpr0 f xs0

-- -->I==>
-- Synthesize lambda expressions. The key optimization here is to avoid
-- re-synthesizing after eta expansion - we synthesize the body once with
-- proper context, then construct the expanded form directly.
synthE _ g0 (LamS vs x) = do
  -- Create existentials for lambda-bound variables and add to context
  let (g1, paramTypes) = statefulMap (\g' v -> newvar (unEVar v <> "_x") g') g0 vs
      g2 = g1 ++> zipWith AnnG vs paramTypes

  -- Synthesize body ONCE with bound variables in context
  (g3, bodyType, bodyExpr) <- synthG g2 x

  let funType = apply g3 (FunU paramTypes bodyType)
  return (g3, funType, LamS vs (applyGen g3 bodyExpr))

--   List
synthE _ g (LstS []) =
  let (g1, itemType) = newvar "itemType_" g
      listType = BT.listU itemType
   in return (g1, listType, LstS [])
synthE i g (LstS (e : es)) = do
  (g1, itemType, itemExpr) <- synthElem g e
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
  (g1, itemType, itemExpr) <- synthElem g e

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
-- Another signed term a definition names, referenced by its signature alone,
-- is instantiated here, its constraints becoming obligations over the
-- instantiating variables. A reference's type never keeps its quantifier:
-- one left in place travels into a synthesized record or tuple and meets the
-- expected type there as a rigid variable.
synthE _ g0 (VarS v (MonomorphicExpr (Just t0) []))
  | Just _ <- gammaAssumedConstraints g0 = do
      let (g1, t0') = renameEType g0 t0
          (g2, t2) = instantiateObligations g1 (etype t0') (Set.toList (econs t0'))
      return (g2, t2, VarS v (MonomorphicExpr (Just t0) []))
synthE _ g0 (VarS v (MonomorphicExpr (Just t0sig) xs0)) = do
  -- a local signature's type variables named by an enclosing signature are
  -- that signature's variables
  let enclosing = if isLocalName v then gammaScoped g0 else Map.empty
      t0 = scopeEType (isJust (gammaRigid g0)) enclosing t0sig
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
      inScope = Map.union (Map.fromList (zip (fst (unqualify (etype t0))) (fst (unqualify t1)))) enclosing
  -- checking a definition against its signature, the body may use what the
  -- declared constraints entail, their superclasses included
  given <- case gammaRigid g1 of
    Just _ -> superclassClosure newCs
    Nothing -> return newCs
  let outermost = isNothing (gammaAssumedConstraints g1)
      g1Cs = g1
        { gammaConstraints = gammaConstraints g1 ++ newCs
        , gammaAssumedConstraints = case gammaAssumedConstraints g1 of
            Nothing -> Just given
            Just _  -> gammaAssumedConstraints g1
        -- checking a definition against its signature, the signature's
        -- effect variables are as rigid as its type variables
        , gammaRigid = case gammaRigid g1 of
            Just rigid | outermost -> Just (Set.union rigid (Set.fromList (typeEffectVars t1)))
            r -> r
        }
      g1' = (g1Cs ++> [AnnG v t1]) {gammaScoped = inScope}
  (g2scoped, t2, xs1) <- foldCheck g1' xs0 t1
  let g2 = g2scoped {gammaScoped = gammaScoped g0}
  -- Verify the body's evaluation-time effects fit within the declared
  -- signature.  Catches forces outside do-blocks (e.g. `f = !rint`)
  -- that the structural subtype rule cannot see, because EvalS strips
  -- the effect wrapper from the inner type (EFF-10).  The body's own concrete
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
-- Checking a definition against its signature: a method use is instantiated
-- here so the constraint it needs, over the same fresh variables, is recorded
-- as an obligation; which instance it picks is not this check's business.
synthE _ g0 (VarS v (PolymorphicExpr cls clsName t0 _))
  | Just _ <- gammaRigid g0 = do
      tcls <- MM.gets stateTypeclasses
      let (g1, t1, rw) = renameWithMap g0 (etype t0)
          cvars = maybe [] classVars (Map.lookup clsName tcls)
          obligation = Constraint cls [rw (VarU cv) | cv <- cvars]
          (g2, t2) = instantiateObligations g1 t1 [obligation]
      return (g2, t2, VarS v (PolymorphicExpr cls clsName t0 []))
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
    -- Checking against a signature, the body's quantified variables are
    -- rigid under their own names; a recursive call instantiates the
    -- signature afresh, so it must not reuse them.
    (Just t) | Just _ <- gammaRigid g -> return (rename g t)
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
  scope <- MM.getGeneralScope
  let t2' = apply g4 t2
      t3' = apply g4 t3
  -- Try strict subtype both directions first (zero-coercion path), then
  -- fall back to tryCoerce (which handles a -> ?a and chains). Two
  -- suspension branches join by row inclusion (<> T <: <E> T); a value
  -- branch against a suspension branch is a type error, since a value is
  -- never silently a suspension.
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
  -- Strip forall wrappers before matching EffectU: a polymorphic final
  -- like a typeclass method 'random :: forall a. <Random> a' otherwise
  -- appears as ForallU (EffectU ...) and falls into the bareT branch,
  -- producing '<collected> (forall a. <Random> a)' -- a nested effect
  -- type. Mirrors the forcedView call in synthE (EvalS ...).
  (g1', t1') <- forcedView g1 t1
  case t1' of
    EffectU _ iT -> do
      -- Final expr is effectful: wrap it in EvalS so codegen forces the
      -- thunk and collectDoEffects picks up its effects alongside the
      -- non-final EvalS nodes.
      e1' <- wrapFinalEvalS i iT e1
      let collected = collectDoEffects e1'
      return (g1', EffectU collected iT, DoBlockS e1')
    bareT -> do
      -- Pure final: the block returns it, so the block's type is
      -- <collected> bareT; with nothing forced that is <> bareT, still a
      -- suspension.
      let collected = collectDoEffects e1
      return (g1', EffectU collected bareT, DoBlockS e1)
synthE _ g (CoerceS coercion e) = do
  (g1, t1, e1) <- synthG g e
  return (g1, applyCoercion coercion t1, CoerceS coercion e1)
synthE i g (EvalS e) = do
  (g1, t1, e1) <- synthG g e
  (g1', t1') <- forcedView g1 t1
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
      <+> "An effectful type <E> T is required here -- this diagnostic fires at a bare non-final do-block statement, the RHS of a '<-' bind, or the '!' operator."
      <+> "Pure expressions should be bound with a pure 'let' or moved to the do-block's final position."
-- IntrMap: the desugar-inserted implicit map for bracket-accessor
-- chains. Typed as @(a -> b) -> f a -> f b@ with @f@ a fresh
-- existential so the same intrinsic covers @List@, @Vector m@, etc.
-- The receiver is checked before the function so @a@ and @f@ are
-- pinned from the concrete list type; the function-check then sees a
-- concrete input and the pattern-selector synth can propagate a
-- concrete result into @b@. Checking the function first leaves @a@
-- open, which lets the selector synth grow independent structural
-- existentials that never bind @b@ -- the tuple-broadcast leak that
-- surfaced as @cannot serialize type: map_result_<N>@.
synthE _ g (IntrinsicS IntrMap [funcE, listE]) = do
  let (g1, a) = newvar "map_elem_"      g
      (g2, b) = newvar "map_result_"    g1
      (g3, f) = newvar "map_container_" g2
      listExpectedT = AppU f [a]
      resultT       = AppU f [b]
  (g4, _, listE') <- checkG g3 listE listExpectedT
  let funcExpectedT = FunU [apply g4 a] (apply g4 b)
  (g5, _, funcE') <- checkG g4 funcE funcExpectedT
  return (g5, apply g5 resultT, IntrinsicS IntrMap [funcE', listE'])
synthE _ _ (IntrinsicS IntrMap args) =
  error $ "IntrMap expects 2 args (lambda, list), got " <> show (length args)
-- IntrTagTest: the constructor-pattern tag test.
--
-- The second argument is the constructor's NAME, carried as data by the
-- desugar rather than as a reference to the term the declaration bound. A
-- payload constructor is a function into its type (@Circle :: Real ->
-- Shape@), so it could never be checked as a value of the type being
-- tested. Validating the name against the scrutinee's own constructor
-- table works for both tiers and rejects a constructor borrowed from
-- another type just as firmly as the value check it replaces.
synthE i g (IntrinsicS IntrTagTest [subjectE, nameE])
  | Just n <- literalStr nameE = do
      (g1, subjectT, subjectE') <- synthG g subjectE
      _ <- checkCtorBelongs i g1 subjectT n
      (g2, _, nameE') <- synthG g1 nameE
      return (g2, BT.boolU, IntrinsicS IntrTagTest [subjectE', nameE'])
synthE _ _ (IntrinsicS IntrTagTest args) =
  error $ "IntrTagTest expects (subject, constructor name), got " <> show (length args)
-- IntrCtorField: read one field out of a value whose constructor a guarding
-- tag test has already established. The result is the constructor's declared
-- field type with the owning type's parameters instantiated against the
-- scrutinee -- the same table 'ConS' reads to CHECK arguments going in, read
-- here to synthesize one coming out.
synthE i g (IntrinsicS IntrCtorField [subjectE, nameE, idxE])
  | Just n <- literalStr nameE
  , IntS _ idx <- unAnnoSE idxE = do
      (g1, subjectT, subjectE') <- synthG g subjectE
      fieldTs <- checkCtorBelongs i g1 subjectT n
      (g2, _, nameE') <- synthG g1 nameE
      (g3, _, idxE') <- synthG g2 idxE
      case drop (fromIntegral idx) fieldTs of
        (t : _) ->
          return (g3, apply g3 t, IntrinsicS IntrCtorField [subjectE', nameE', idxE'])
        [] ->
          MM.throwSourcedError i $
            "constructor" <+> squotes (pretty n) <+> "has no field"
              <+> pretty (show idx)
synthE _ _ (IntrinsicS IntrCtorField args) =
  error $ "IntrCtorField expects (subject, constructor name, index), got " <> show (length args)
-- IntrWrite: @Int -> OStream a -> [a] -> <IO> ()@. Handle-before-list
-- is the natural partial-application shape: @write 3 o@ is a
-- reusable [a] -> <IO> () sink for callbacks. The handle is
-- check-mode'd FIRST so the OStream's element type pins the fresh
-- existential @a@; then the list is check-mode'd against @[a]@,
-- which gives the integer literals inside it a chance to inhabit
-- the OStream's type (e.g. UInt64) via the check-mode IntS rule.
-- The generic synth path would synth the list first and freeze its
-- elements to @Int@ before @a@ is visible -- @write 0 (o ::
-- OStream UInt64) [1]@ would then fail with "Cannot compare types
-- UInt64 and Int" at the handle subtyping step.
synthE _ g (IntrinsicS IntrWrite [levelE, handleE, listE]) = do
  let (g1, a) = newvar "write_a_" g
      listExpectedT   = AppU (VarU BT.list) [a]
      handleExpectedT = AppU (VarU BT.ostreamVar) [a]
  (g2, _, handleE') <- checkG g1 handleE handleExpectedT
  (g3, _, levelE')  <- checkG g2 levelE  BT.intU
  (g4, _, listE')   <- checkG g3 listE   listExpectedT
  return ( g4
         , EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
         , IntrinsicS IntrWrite [levelE', handleE', listE']
         )
synthE _ _ (IntrinsicS IntrWrite args) =
  error $ "IntrWrite expects 3 args (level, handle, list), got " <> show (length args)
-- The fold accumulator. `Cell b` carries the accumulated type, which is
-- what forces a folding handler's init, step and combine to agree with
-- each other and with the handler that consumes the result. A bare
-- integer handle would leave those types floating free, so a formatter
-- pairing `@init=zeroHist` with `@fold=addCounts` would typecheck and
-- then misread the accumulator -- an unsound path, not a missed warning.
--
-- The handle is checked before the value for the reason @write's rule
-- gives: it pins the fresh existential, so a literal in the value still
-- has a chance to inhabit the accumulator's own type rather than freezing
-- to its default first.
synthE _ g (IntrinsicS IntrCellNew [initE]) = do
  let (g1, b) = newvar "cell_acc_" g
  (g2, _, initE') <- checkG g1 initE b
  return ( g2
         , EffectU ioEffectSet (AppU (VarU BT.cellVar) [apply g2 b])
         , IntrinsicS IntrCellNew [initE']
         )
synthE _ _ (IntrinsicS IntrCellNew args) =
  error $ "IntrCellNew expects 1 arg (init), got " <> show (length args)
synthE _ g (IntrinsicS IntrCellGet [handleE]) = do
  (g1, b, handleE') <- checkCellHandle g handleE
  return ( g1
         , EffectU ioEffectSet b
         , IntrinsicS IntrCellGet [handleE']
         )
synthE _ _ (IntrinsicS IntrCellGet args) =
  error $ "IntrCellGet expects 1 arg (handle), got " <> show (length args)
synthE _ g (IntrinsicS IntrCellPut [handleE, valE]) = do
  (g2, b, handleE') <- checkCellHandle g handleE
  (g3, _, valE')    <- checkG g2 valE b
  return ( g3
         , EffectU ioEffectSet BT.unitU
         , IntrinsicS IntrCellPut [handleE', valE']
         )
synthE _ _ (IntrinsicS IntrCellPut args) =
  error $ "IntrCellPut expects 2 args (handle, value), got " <> show (length args)
synthE _ g (IntrinsicS IntrCellReduce [combineE, handleE]) = do
  (g2, bT, handleE') <- checkCellHandle g handleE
  (g3, _, combineE') <- checkG g2 combineE (FunU [bT, bT] bT)
  return ( g3
         , EffectU ioEffectSet (apply g3 bT)
         , IntrinsicS IntrCellReduce [combineE', handleE']
         )
synthE _ _ (IntrinsicS IntrCellReduce args) =
  error $ "IntrCellReduce expects 2 args (combine, handle), got " <> show (length args)
-- @replay :: IStream a -> ([a] -> <IO> ()) -> <IO> (). The function receives
-- each frame of the stream as a list; the element type comes from the stream.
synthE _ g (IntrinsicS IntrReplay [handleE, fnE]) = do
  let (g1, a) = newvar "replay_a_" g
  (g2, _, handleE') <- checkG g1 handleE (AppU (VarU BT.istreamVar) [a])
  (g3, _, fnE') <- checkG g2 fnE (FunU [BT.listU (apply g2 a)] (EffectU ioEffectSet BT.unitU))
  return ( g3
         , EffectU ioEffectSet BT.unitU
         , IntrinsicS IntrReplay [handleE', fnE']
         )
synthE i _ (IntrinsicS IntrReplay args) =
  MM.throwCompilerBugAt i $ "IntrReplay expects 2 args (stream, function), got " <> pretty (length args)
-- @channel :: <IO> (IStream a); the element type comes from where the
-- stream is used.
synthE _ g (IntrinsicS IntrChannel []) =
  let (g1, a) = newvar "channel_a_" g
   in return (g1, EffectU ioEffectSet (AppU (VarU BT.istreamVar) [a]), IntrinsicS IntrChannel [])
synthE i _ (IntrinsicS IntrChannel args) =
  MM.throwCompilerBugAt i $ "IntrChannel expects no args, got " <> pretty (length args)
-- @spawn :: IStream a -> (OStream a -> <IO> ()) -> <IO> (). The producer
-- writes the channel the stream reads.
synthE _ g (IntrinsicS IntrSpawn [handleE, fnE]) = do
  let (g1, a) = newvar "spawn_a_" g
  (g2, _, handleE') <- checkG g1 handleE (AppU (VarU BT.istreamVar) [a])
  (g3, _, fnE') <- checkG g2 fnE (FunU [AppU (VarU BT.ostreamVar) [apply g2 a]] (EffectU ioEffectSet BT.unitU))
  return (g3, EffectU ioEffectSet BT.unitU, IntrinsicS IntrSpawn [handleE', fnE'])
synthE i _ (IntrinsicS IntrSpawn args) =
  MM.throwCompilerBugAt i $ "IntrSpawn expects 2 args (stream, producer), got " <> pretty (length args)
-- @settle :: IStream a -> <IO> ().
synthE _ g (IntrinsicS IntrSettle [handleE]) = do
  let (g1, a) = newvar "settle_a_" g
  (g2, _, handleE') <- checkG g1 handleE (AppU (VarU BT.istreamVar) [a])
  return (g2, EffectU ioEffectSet BT.unitU, IntrinsicS IntrSettle [handleE'])
synthE i _ (IntrinsicS IntrSettle args) =
  MM.throwCompilerBugAt i $ "IntrSettle expects 1 arg (stream), got " <> pretty (length args)
-- Bespoke rule for @try. Its argument may fail in any number of ways --
-- an intrinsic Err arm auto-required into a throw, a foreign function
-- raising natively, an explicit @throw -- and those failures have no
-- common type, so the caught error is always a rendered message. The
-- result is therefore @Try Str a@ regardless of the body.
--
-- The body's own effects pass through: catching a failure does not
-- discharge the IO the body performed on its way there. A pure body
-- stays pure.
synthE _ g (IntrinsicS IntrTry [bodyE]) = do
  (g0, bodyT, bodyE') <- synthG g bodyE
  let (g1, openT) = stripForallU g0 (apply g0 bodyT)
      resultT = case openT of
        EffectU e t -> mkEffectU e (BT.tryU BT.strU t)
        t           -> BT.tryU BT.strU t
  return (g1, resultT, IntrinsicS IntrTry [bodyE'])
synthE i _ (IntrinsicS IntrTry args) =
  MM.throwCompilerBugAt i $ "IntrTry expects 1 arg (body), got " <> pretty (length args)
-- @stream :: IFile [a] -> <IO> IStream a. An IFile carries the type of the
-- whole value in its file and an IStream the type of one element, so the
-- receiver is checked against a list-valued IFile and the element read from
-- the solved variable. Checking (rather than reading the receiver's type
-- syntactically) sees through aliases and solves an inferred receiver.
synthE _ g (IntrinsicS IntrStream [handleE]) = do
  let (g1, a) = newvar "stream_a_" g
  (g2, _, handleE') <- checkG g1 handleE (AppU (VarU BT.ifileVar) [BT.listU a])
  return ( g2
         , EffectU ioEffectSet (AppU (VarU BT.istreamVar) [apply g2 a])
         , IntrinsicS IntrStream [handleE']
         )
synthE i _ (IntrinsicS IntrStream args) =
  throwTypeError i $ "@stream expects 1 arguments but got" <+> pretty (length args)
-- @next :: IStream a -> <IO> (Try Str [a]). A mid-stream decode failure is
-- an Err arm. Typed like @stream, by checking the receiver.
synthE _ g (IntrinsicS IntrNext [handleE]) = do
  let (g1, a) = newvar "next_a_" g
  (g2, _, handleE') <- checkG g1 handleE (AppU (VarU BT.istreamVar) [a])
  return ( g2
         , EffectU ioEffectSet (BT.tryU BT.strU (BT.listU (apply g2 a)))
         , IntrinsicS IntrNext [handleE']
         )
synthE i _ (IntrinsicS IntrNext args) =
  throwTypeError i $ "@next expects 1 arguments but got" <+> pretty (length args)
synthE i g (IntrinsicS intr args) = do
  (g', argTypes, args') <- synthArgs g args
  g'' <- checkIntrinsicArgs i g' intr argTypes
  let (g''', expectedType) = intrinsicTypeG g'' intr argTypes
  return (g''', expectedType, IntrinsicS intr args')

-- | Strip ForallU wrappers by instantiating bound variables as existentials.
-- Follows the same pattern as `application` for ForallU.
stripForallU :: Gamma -> TypeU -> (Gamma, TypeU)
stripForallU g (ForallU v t) = stripForallU (g +> v) (substitute v t)
stripForallU g t = (g, t)

-- | The type of an expression at a position that runs it: foralls
-- stripped, and an alias that names a suspension (@type IOInt = <IO> Int@)
-- expanded so the row is visible.
forcedView :: Gamma -> TypeU -> MorlocMonad (Gamma, TypeU)
forcedView g t0 = do
  let (g1, t1) = stripForallU g (apply g t0)
  case t1 of
    EffectU _ _ -> return (g1, t1)
    ExistU _ _ _ -> return (g1, t1)
    _ -> do
      scope <- MM.getGeneralScope
      return $ case TE.evaluateType scope t1 of
        Right t2@(EffectU _ _) -> (g1, t2)
        _ -> (g1, t1)

-- | Peel ForallU layers without touching bound variables. Cheap for
-- inspecting the underlying constructor of an annotation type (e.g. to
-- see whether it is an EffectU) when no gamma is available.
peelForallU :: TypeU -> TypeU
peelForallU (ForallU _ t) = peelForallU t
peelForallU t = t

-- | Return type of a fully applied intrinsic, threading Gamma for fresh existentials.
-- Receives the synthesized argument types so result types like `@next`'s `[a]`
-- can extract `a` from the receiver's type.
intrinsicTypeG :: Gamma -> Intrinsic -> [TypeU] -> (Gamma, TypeU)
-- @load :: Str -> <IO> (Try Str a). Failure (missing file, decode
-- mismatch, schema mismatch) is an Err arm, not an effect.
intrinsicTypeG g IntrLoad _ =
  let (g', loadType) = newvar "load_" g
  in (g', EffectU ioEffectSet (BT.tryU BT.strU loadType))
-- @read :: Str -> Try Str a. A JSON parse touches nothing, so with
-- failure carried in the result it is pure and composes in `map`.
intrinsicTypeG g IntrRead _ =
  let (g', readType) = newvar "read_" g
  in (g', BT.tryU BT.strU readType)
-- @open: polymorphic return -- the user's inline ascription (e.g.
-- `@open path :: IFile Sequence`) resolves the existential to the
-- concrete handle type at typecheck time; codegen then inspects the
-- resolved TypeF to dispatch to the right runtime entry point.
intrinsicTypeG g IntrOpen _ =
  let (g', openType) = newvar "open_" g
  in (g', EffectU ioEffectSet (BT.tryU BT.strU openType))
-- @unpack: the decoded type is resolved by the synthesized ascription.
intrinsicTypeG g IntrUnpack _ =
  let (g', unpackType) = newvar "unpack_" g
  in (g', EffectU ioEffectSet unpackType)
-- @close: arg is any handle type (a fresh existential); user-side use
-- always has the handle bound to a known type, so this resolves
-- without needing ascription.
intrinsicTypeG g IntrClose _ = (g, EffectU ioEffectSet BT.unitU)
-- @append: polymorphic return like @open; the user ascription resolves
-- to the concrete OStream/IStream/IFile shape.
intrinsicTypeG g IntrAppend _ =
  let (g', appendType) = newvar "append_" g
  in (g', EffectU ioEffectSet (BT.tryU BT.strU appendType))
-- @stdin :: <IO> (Try Str (IStream a)). Claims the stdin slot; a second
-- open fails via the CAS-per-kind uniqueness guard. Piped-content decode
-- errors surface at @next, not here (no isatty pre-check -- users
-- may want the handle before any read).
-- @stdout / @stderr: handle setup only, and they cannot fail.
intrinsicTypeG g IntrStdin _ =
  let (g', a) = newvar "stdin_a" g in
  (g', EffectU ioEffectSet (BT.tryU BT.strU (AppU (VarU BT.istreamVar) [a])))
intrinsicTypeG g IntrStdout _ =
  let (g', a) = newvar "stdout_a" g in
  (g', EffectU ioEffectSet (AppU (VarU BT.ostreamVar) [a]))
intrinsicTypeG g IntrStderr _ =
  let (g', a) = newvar "stderr_a" g in
  (g', EffectU ioEffectSet (AppU (VarU BT.ostreamVar) [a]))
-- @throw :: e -> a. A bottom, not a tracked effect: it does not return,
-- so there is nothing for an effect row to describe. The payload is
-- rendered into the traceback at the throw site and does not survive as a
-- value, which is what keeps behaviour the same whether the throw and the
-- @try that catches it land in the same pool or not.
intrinsicTypeG g IntrThrow _ =
  let (g', a) = newvar "throw_" g
  in (g', a)
intrinsicTypeG g intr _ = (g, intrinsicType intr)

-- | Extract the element type `a` from a `Handle a` (IFile/IStream/OStream)
-- after typecheck has constrained it. Falls back to a placeholder if the
-- expected subtype constraint failed silently -- that branch is
-- unreachable on a well-typed program but defends codegen against
-- a missing-arg-type assertion.
-- | Check a @Cell b@ handle and return the accumulator type it carries.
--
-- Always the first check in a cell intrinsic's rule, for the reason
-- @write's rule gives: it pins the fresh existential, so a literal in a
-- later argument still has a chance to inhabit the accumulator's own type
-- rather than freezing to its default first.
checkCellHandle ::
  Gamma ->
  AnnoS Int ManyPoly Int ->
  MorlocMonad (Gamma, TypeU, AnnoS (Indexed TypeU) ManyPoly Int)
checkCellHandle g handleE = do
  let (g1, b) = newvar "cell_acc_" g
  (g2, _, handleE') <- checkG g1 handleE (AppU (VarU BT.cellVar) [b])
  return (g2, apply g2 b, handleE')

-- | Return type of a fully applied intrinsic (for intrinsics without fresh vars)
intrinsicType :: Intrinsic -> TypeU
intrinsicType IntrSave = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
intrinsicType IntrSaveM = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
intrinsicType IntrSaveJ = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
intrinsicType IntrLoad = EffectU ioEffectSet (BT.tryU BT.strU (ExistU (TV "load_a") ([], Open) ([], Open)))
intrinsicType IntrHash = BT.strU
intrinsicType IntrVersion = BT.strU
intrinsicType IntrCompiled = BT.strU
intrinsicType IntrLang = BT.strU
intrinsicType IntrSchema = BT.strU
intrinsicType IntrTypeof = BT.strU
intrinsicType IntrShow = BT.strU
intrinsicType IntrRead = BT.tryU BT.strU (ExistU (TV "read_a") ([], Open) ([], Open))
intrinsicType IntrDatafile = BT.strU
intrinsicType IntrUnpack =
  error "intrinsicType: IntrUnpack must be typed via intrinsicTypeG"
-- IntrOpen and IntrClose flow through intrinsicTypeG (fresh existentials).
intrinsicType IntrOpen =
  error "intrinsicType: IntrOpen must be typed via intrinsicTypeG"
intrinsicType IntrClose =
  error "intrinsicType: IntrClose must be typed via intrinsicTypeG"
intrinsicType IntrFSchema = EffectU ioEffectSet (BT.tryU BT.strU BT.strU)
intrinsicType IntrFLength = EffectU ioEffectSet (BT.tryU BT.strU BT.intU)
intrinsicType IntrStreamLayout =
  EffectU ioEffectSet (BT.tryU BT.strU (BT.listU (BT.tupleU [BT.u64U, BT.u64U, BT.u64U])))
intrinsicType IntrTell = EffectU ioEffectSet BT.u64U
intrinsicType IntrTmpfile = EffectU ioEffectSet (BT.tryU BT.strU BT.strU)
intrinsicType IntrNext =
  error "intrinsicType: IntrNext is typed in synthE (its element type comes from the receiver)"
intrinsicType IntrStream =
  error "intrinsicType: IntrStream is typed in synthE (its element type comes from the receiver)"
intrinsicType IntrWrite = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
-- One clause each, not a guard over a list: this match is exhaustive on
-- purpose, so that a new Intrinsic constructor fails the build here rather
-- than falling through to a runtime error.
intrinsicType IntrCellNew =
  error "intrinsicType: IntrCellNew must be typed via synthE's dedicated clause"
intrinsicType IntrCellGet =
  error "intrinsicType: IntrCellGet must be typed via synthE's dedicated clause"
intrinsicType IntrCellPut =
  error "intrinsicType: IntrCellPut must be typed via synthE's dedicated clause"
intrinsicType IntrCellReduce =
  error "intrinsicType: IntrCellReduce must be typed via synthE's dedicated clause"
intrinsicType IntrReplay =
  error "intrinsicType: IntrReplay must be typed via synthE's dedicated clause"
intrinsicType IntrChannel =
  error "intrinsicType: IntrChannel must be typed via synthE's dedicated clause"
intrinsicType IntrSpawn =
  error "intrinsicType: IntrSpawn must be typed via synthE's dedicated clause"
intrinsicType IntrSettle =
  error "intrinsicType: IntrSettle must be typed via synthE's dedicated clause"
intrinsicType IntrAppend =
  error "intrinsicType: IntrAppend must be typed via intrinsicTypeG (polymorphic return)"
intrinsicType IntrConcat = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
intrinsicType IntrFlush = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
-- IntrMap is handled by its own synthE clause and never reaches this fallback.
intrinsicType IntrMap =
  error "intrinsicType: IntrMap must be typed via synthE's dedicated clause"
intrinsicType IntrMapOptional =
  error "intrinsicType: IntrMapOptional is emitted after typechecking"
intrinsicType IntrTagTest =
  error "intrinsicType: IntrTagTest must be typed via synthE's dedicated clause"
intrinsicType IntrCtorField =
  error "intrinsicType: IntrCtorField must be typed via synthE's dedicated clause"
-- IntrStdin/Stdout/Stderr flow through intrinsicTypeG (fresh existential
-- element type resolved by the user's inline ascription).
intrinsicType IntrStdin =
  error "intrinsicType: IntrStdin must be typed via intrinsicTypeG (polymorphic element type)"
intrinsicType IntrStdout =
  error "intrinsicType: IntrStdout must be typed via intrinsicTypeG (polymorphic element type)"
intrinsicType IntrStderr =
  error "intrinsicType: IntrStderr must be typed via intrinsicTypeG (polymorphic element type)"
intrinsicType IntrThrow =
  error "intrinsicType: IntrThrow must be typed via intrinsicTypeG (polymorphic return type)"
intrinsicType IntrTry =
  error "intrinsicType: IntrTry must be typed via synthE's dedicated clause"
intrinsicType IntrCollect =
  error "intrinsicType: IntrCollect is expanded at desugar and never reaches typecheck"
-- IntrIFileWalk is synthesized by Express.hs / Nexus.hs with a typed result;
-- it never appears as a user-facing intrinsic so no type rule is needed.
intrinsicType IntrIFileWalk =
  error "intrinsicType: IntrIFileWalk is synthesized post-typecheck and carries its result type from the originating pattern application"

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
        -- @save: Int -> Str -> a -> <IO>(). Level constrained to integer
        -- domain; path must be Str; the value is unconstrained. Path-first
        -- (after the level) mirrors @savem/@savej.
        (IntrSave, [levelT, pathT, _]) -> do
          g' <- subtype' i levelT BT.intU g
          subtype' i pathT BT.strU g'
        -- @savem/@savej: Str -> a -> <IO>(). Path-first makes
        -- @savem path a reusable one-arg sink for callbacks.
        (IntrSaveM, [pathT, _]) -> subtype' i pathT BT.strU g
        (IntrSaveJ, [pathT, _]) -> subtype' i pathT BT.strU g
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
        -- @unpack: [U8] -> <IO> a
        (IntrUnpack, [bytesT]) -> subtype' i bytesT (BT.listU BT.u8U) g
        -- @open: Str -> <IO> a (return type resolved by user ascription)
        (IntrOpen, [pathT]) -> subtype' i pathT BT.strU g
        -- @close: any handle type -> <IO> ()
        (IntrClose, [_]) -> return g
        -- @fschema: Str -> <IO> Str
        (IntrFSchema, [pathT]) -> subtype' i pathT BT.strU g
        -- @flen: IFile a -> <IO> Int. Receiver is any handle type;
        -- the runtime enforces kind == IFILE.
        (IntrFLength, [_]) -> return g
        -- @streamLayout: IFile [a] -> <IO> [(U64,U64,U64)]. Pin the receiver to
        -- a list-shaped IFile so both a wrong handle kind and non-list content
        -- are rejected at compile time. Runtime enforces kind == IFILE.
        (IntrStreamLayout, [argT]) ->
          let (g'a, a) = newvar "streamlayout_a_" g
              expectedT = AppU (VarU BT.ifileVar) [BT.listU a]
           in subtype' i argT expectedT g'a
        -- @next and @stream have their own synthE clauses.
        (IntrNext, _) -> MM.throwCompilerBugAt i "IntrNext is typed in synthE"
        (IntrReplay, _) -> MM.throwCompilerBugAt i "IntrReplay is typed in synthE"
        (IntrStream, _) -> MM.throwCompilerBugAt i "IntrStream is typed in synthE"
        -- @write is special-cased in synthE so the OStream's element
        -- type can pin the list literals' element type via check-mode
        -- propagation; see the synthE branch above.
        -- @append: Str -> <IO> a (user ascription resolves)
        (IntrAppend, [pathT]) -> subtype' i pathT BT.strU g
        -- @concat: [Str] -> Str -> <IO> ()
        (IntrConcat, [pathsT, destT]) -> do
          g1 <- subtype' i pathsT (AppU (VarU BT.list) [BT.strU]) g
          subtype' i destT BT.strU g1
        -- @flush: OStream a -> <IO> (). Pin the receiver to OStream so
        -- the type system catches misuse on IFile/IStream at compile
        -- time. The runtime ALSO enforces kind == OSTREAM (defence in
        -- depth) but the static check is the user-facing diagnostic.
        (IntrFlush, [handleT]) ->
          let (g'a, a) = newvar "flush_a_" g
              expectedT = AppU (VarU BT.ostreamVar) [a]
           in subtype' i handleT expectedT g'a
        -- @throw's payload must already be a Str. The runtime reads it as
        -- UTF-8 bytes, so any other shape would render as an empty message
        -- and silently lose the error. Render first (@show) to throw
        -- something structured.
        (IntrThrow, [msgT]) -> subtype' i msgT BT.strU g
        -- compile-time constants: no args
        (IntrVersion, []) -> return g
        (IntrCompiled, []) -> return g
        (IntrLang, []) -> return g
        _ -> return g

-- | Synthesize an element of a list or tuple literal. A numeric literal is
-- checked against a fresh variable instead, so a later element or argument
-- can still give it its type, as it can a literal passed on its own.
synthElem ::
  Gamma ->
  AnnoS Int ManyPoly Int ->
  MorlocMonad (Gamma, TypeU, AnnoS (Indexed TypeU) ManyPoly Int)
synthElem g x@(AnnoS _ _ e)
  | numLit e = let (g1, v) = newvar "lit_" g in checkG g1 x v
  | otherwise = synthG g x
  where
    numLit (IntS _ _) = True
    numLit (RealS _ _) = True
    numLit _ = False

-- | Defer a literal of kind @k@ at existential @v@. One entry per variable
-- and kind decides them all, so the literals of a long list add one.
deferNumLit :: (Int, TVar, NumLitKind) -> Gamma -> Gamma
deferNumLit lit@(_, v, k) g
  | any (\(_, v', k') -> v' == v && k' == k) (gammaPendingNumLits g) = g
  | otherwise = g {gammaPendingNumLits = lit : gammaPendingNumLits g}

-- | A numeric literal whose type was deferred and later resolved to an
-- optional is coerced to it, as a literal checked against that optional is.
coerceLiteralsToOptional ::
  AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) ManyPoly Int)
coerceLiteralsToOptional (AnnoS (Idx i t) c e) = case (t, e) of
  (OptionalU inner, IntS {}) -> wrap inner
  (OptionalU inner, RealS {}) -> wrap inner
  _ -> AnnoS (Idx i t) c <$> mapExprSM coerceLiteralsToOptional e
  where
    wrap inner = do
      idx <- MM.getCounterWithPos i
      lit <- coerceLiteralsToOptional (AnnoS (Idx idx inner) i e)
      return (AnnoS (Idx i t) c (CoerceS CoerceToOptional lit))

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
  (g2raw, funType1, inputExprs) <- application' i g1 xs0 normalType

  -- Resolve any pending numeric literals locally now that this
  -- application's arguments are fully processed. Without this, the
  -- AppS result type stays as an unsolved existential, and a
  -- downstream context (e.g. a do-block checking against @<IO> Int@)
  -- can instantiate the existential to the WRAPPED type @<IO> Int@
  -- via the InstantiateL rule that absorbs effects into bare
  -- existentials. Resolving here pins the existential to a concrete
  -- numeric type (or to whatever a sibling arg already pinned it to)
  -- so the propagated return type is correct and instance resolution
  -- can dispatch later. Only the literals this application registered are
  -- resolved: an enclosing application's literal may still be pinned by a
  -- later argument of that application.
  let before = Set.fromList (gammaPendingNumLits g1)
      own = filter (`Set.notMember` before) (gammaPendingNumLits g2raw)
  g2 <- resolvePendingNumLits g2raw own

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
      (g4, as1, es', _) <- zipCheck i g3 es eas
      let funType = apply g4 (FunU as1 ea)
      return (g4, funType, es')
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g0 of
      (Just (FunU ts t)) -> do
        (g1, ts', es', _) <- zipCheck i g0 es ts
        let funType = apply g1 (FunU ts' t)
        return (g1, funType, es')
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

-- Singular/plural agreement for arity diagnostics.
arguments :: Int -> MDoc
arguments 1 = "argument"
arguments _ = "arguments"

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
-- A directly applied setter is a redex here too: the App-Check rule
-- below would synthesise the wrapper lambda first, which is what hides
-- the receiver. See 'reduceSetterRedex'.
checkE i g e t | Just e' <- reduceSetterRedex e = checkE i g e' t
-- The single-arg form (e.g. `List Int`) treats the arg as the element
-- type. Guard against firing when the arg is non-Type-kinded (e.g.
-- `Foo (n :: Nat) = ...` instantiated as `Foo 3` -- here `3` is a Nat
-- dimension, not an element type, and forcing list elements to subtype
-- it produces a confusing kind mismatch). Falls through to the LstS
-- catch-all (line ~1230) which handles dimension checking via getNatArgs.
-- Element propagation when the expected type IS the canonical list
-- constructor. Restricting to @BT.list@ here ensures that single-arg
-- newtypes like @newtype MyVec a = List a@ -- whose @AppU MyVec [a]@
-- shape would otherwise match this pattern -- get routed to the
-- general LstS rule (below) which walks the newtype wire-parent
-- before propagating element types. Without the guard, the empty-list
-- base case at the bottom of this recursion would fall to
-- synth+subtype with @List <: MyVec@, which fails because newtypes
-- are nominal.
checkE i g1 (LstS (e : es)) (AppU (VarU v) [t])
  | v == BT.list
  , plainElem t = do
      (g2, t2, e') <- checkG g1 e t
      (g3, t3, rest) <- checkRest g2 t2 [] es
      -- the final context is applied once, not once per element
      return (g3, t3, LstS (map (applyGen g3) (e' : rest)))
  where
    plainElem x = not (isNatExpr x || isStrExpr x || isRecExpr x || isListExpr x || isSetExpr x)
    checkRest g tc acc (x : xs)
      | plainElem tc = do
          (g', tc', x') <- checkG g x tc
          checkRest g' tc' (x' : acc) xs
    -- LstS [] goes to the normal Sub case
    checkRest g tc acc xs = do
      (g', t', LstS xs') <- checkE' i g (LstS xs) (AppU (VarU v) [tc])
      return (g', t', reverse acc ++ xs')
checkE i g0 (LamS vs body) t@(FunU as b)
  | length vs == length as = do
      let g1 = g0 ++> zipWith AnnG vs as
      (g2, t2, e2) <- checkG g1 body b

      let t3 = apply g2 (FunU as t2)
          e3 = applyCon g2 (LamS vs e2)

      return (g2, t3, e3)
  -- Fewer parameters than the type has arguments: the body computes the
  -- function the rest of the type describes, and is checked against it. It is
  -- not eta-expanded: that would move the body's work from each application
  -- of this lambda to each call of the function it returns.
  | length vs < length as = do
      let (asHere, asRest) = splitAt (length vs) as
          g1 = g0 ++> zipWith AnnG vs asHere
      (g2, t2, e2) <- checkG g1 body (FunU asRest b)
      return (g2, apply g2 (FunU asHere t2), applyCon g2 (LamS vs e2))
  -- More parameters than this arrow group has arguments. A signature written
  -- @A -> (B -> C)@ parses as @FunU [A] (FunU [B] C)@, so a two-parameter
  -- definition lands here with one argument to match against. Re-nest the
  -- surplus parameters into the return type and recurse, which is exactly what
  -- 'Morloc.Typecheck.Internal.subtype' already does to reconcile the two
  -- spellings. A genuine arity error still surfaces: the re-nested return
  -- fails to check because it is not a function.
  --
  -- Recursion terminates because each step consumes @length as@ parameters
  -- (at least one, since @length vs > length as >= 0@ and the @null as@ case
  -- cannot recur). Do NOT reach for 'expand' here -- it was previously called
  -- with the negative difference, appending a parameter per step and never
  -- reaching its base case, which hung the compiler.
  | not (null as) = do
      let (vsHere, vsRest) = splitAt (length as) vs
          g1 = g0 ++> zipWith AnnG vsHere as
      (g2, t2, e2) <- checkG g1 (AnnoS i i (LamS vsRest body)) b
      let t3 = apply g2 (FunU as t2)
          e3 = applyCon g2 (LamS vsHere e2)
      return (g2, t3, e3)
  -- A zero-argument arrow group cannot absorb any parameter, so there is
  -- nothing left to reconcile.
  | otherwise = throwTypeError i $
      "This definition takes" <+> pretty (length vs) <+> arguments (length vs)
      <> ", but its type accepts none. The declared type is:" <+> pretty t
-- Checking against a quantified type: in inference the variable is an
-- existential the body may solve; checking a definition against its
-- signature makes it rigid, so the body must work for every instance of it.
checkE i g1 e1 (ForallU v a) = case gammaRigid g1 of
  Just rigid -> checkE' i (g1 {gammaRigid = Just (Set.insert v rigid)} +> VarG v) e1 a
  Nothing -> checkE' i (g1 +> v) e1 (substitute v a)
checkE i g (IfS cond thenE elseE) t = do
  (g1, condType, cond') <- synthG g cond
  g2 <- subtype' i condType (VarU (TV "Bool")) g1
  (g3, t2, thenE') <- checkG g2 thenE t
  (g4, _, elseE') <- checkG g3 elseE (apply g3 t2)
  return (g4, apply g4 t2, IfS cond' thenE' elseE')
-- DoBlockS falls through to the general synth+subtype/coerce case (below).
-- synthE DoBlockS produces a flattened EffectU, and subtype handles effectful
-- finals via <E1> T <: <E2> T, while tryCoerce handles pure-final auto-lift.
-- A do-block checked against a two-layer suspension @<E> (<E'> T)@ is the
-- outer layer, and its final statement, itself a suspension, may be
-- returned rather than run: this is the only way to build a suspension
-- that yields a suspension. When the returned reading does not fit (the
-- final statement is itself two layers deep, so @do t@ is @t@), the
-- statement is run, as in 'synthE'.
checkE i g (DoBlockS e) t@(EffectU _ (EffectU _ _)) = do
  (g1, t1, e1) <- synthG g e
  (g1', t1') <- forcedView g1 t1
  scope <- MM.getGeneralScope
  case t1' of
    EffectU _ iT
      | Right g2 <- subtype scope (EffectU (collectDoEffects e1) t1') t g1' ->
          return (g2, apply g2 t, DoBlockS e1)
      | otherwise -> do
          e1' <- wrapFinalEvalS i iT e1
          let blockT = EffectU (collectDoEffects e1') iT
          g2 <- subtype' i blockT t g1'
          return (g2, apply g2 t, DoBlockS e1')
    bareT -> do
      let blockT = EffectU (collectDoEffects e1) bareT
      g2 <- subtype' i blockT t g1'
      return (g2, apply g2 t, DoBlockS e1)
checkE i g (EvalS e) t = do
  -- Synthesize first to get concrete EffectSet in annotations,
  -- then check the inner type against the expected type.
  -- This avoids creating an EffectVar that is never solved.
  (g1, t1, e1) <- synthG g e
  (g1', t1') <- forcedView g1 t1
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
      <+> "An effectful type <E> T is required here -- this diagnostic fires at a bare non-final do-block statement, the RHS of a '<-' bind, or the '!' operator."
      <+> "Pure expressions should be bound with a pure 'let' or moved to the do-block's final position."

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
checkE i g1 e1@(LstS _) b = do
  scope <- MM.getGeneralScope
  -- Apply the current gamma before consulting wire forms: when a
  -- containing context (e.g. a @(expr :: T)@ annotation) already
  -- solved the existentials in @b@, the solved type is the one we
  -- need to walk the wire-parent on.
  let bApplied = apply g1 b
  -- Walk through transparent aliases AND newtype wire-parents to find
  -- the structural wire-form of the expected type. A list literal can
  -- inhabit a newtype like @Vector 5 Real@ if its wire-parent chain
  -- bottoms out at a list shape (e.g. @List Real@). If the wire-parent
  -- walk does not yield a list shape (e.g. for @Map a b@ with no
  -- newtype body), fall back to the @Packable@ instance's wire form.
  --
  -- Use 'wireParentRoot' directly rather than 'evaluateType' here:
  -- 'evaluateType' descends into and re-expands type args (e.g. for
  -- @type Pat = [Pat]@, each call peels one more inner @Pat@ layer,
  -- which makes the @bWire /= bApplied@ termination check loop).
  -- 'wireParentRoot' only walks the outer head and halts at the
  -- primitive list constructor, which is exactly what we need.
  let bWire = TE.wireParentRoot scope bApplied
      isListShape w = case w of
        AppU _ [elemT] ->
          not (isNatExpr elemT || isStrExpr elemT || isRecExpr elemT
               || isListExpr elemT || isSetExpr elemT)
        _ -> False
      tier1Ok = isListShape bWire && bWire /= bApplied
  tier2 <- if tier1Ok then return Nothing else findPackableWireForm bApplied
  let effectiveWire = if tier1Ok
        then Just bWire
        else case tier2 of
          Just w | isListShape w -> Just w
          _                      -> Nothing
  case effectiveWire of
    Just w -> do
      (g2, _, e2) <- checkE' i g1 e1 w
      let natArgs = getNatArgs scope b
          anno = AnnoS (Idx i (apply g2 bApplied)) i e2
      g3 <- checkListNatDims g2 natArgs anno
      return (g3, apply g3 b, e2)
    Nothing -> do
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
            Nothing ->
              MM.throwSourcedError i $
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
checkE _ g (LetS v e1 e2) t = do
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
  scope <- MM.getGeneralScope
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
        Nothing -> case TE.reduceType scope tApplied >>= tryMatch of
          Just ts -> Just ts
          -- Walk through newtype boundaries (e.g. @newtype Pair a = (a, a)@
          -- has a tuple wire-parent that 'reduceType' won't expose, since
          -- it halts at newtype heads).
          Nothing -> tryMatch (TE.wireParentRoot scope tApplied)
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
  scope <- MM.getGeneralScope
  let tApplied = apply g t
      tEval = either (const tApplied) id (TE.evaluateType scope tApplied)
      -- Walk through newtype boundaries as well (e.g. @record Bar where
      -- {...}@ is a newtype whose body is the @NamU@ shape the literal
      -- inhabits). 'evaluateType' halts at newtypes; 'wireParentRoot'
      -- expands them.
      tWire = TE.wireParentRoot scope tApplied
      pickNamU candidate = case candidate of
        NamU _ _ _ declared | not (null declared) -> Just (candidate, declared)
        _ -> Nothing
      namMatch = pickNamU tEval `orElse` pickNamU tWire
      orElse x y = case x of { Just _ -> x; Nothing -> y }
  case namMatch of
    Just (matchedT, declared) -> do
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
          return (gFinal, apply gFinal matchedT, NamS fields)
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
                <+> prettyTypeU matchedT <> ":" <> missingPart <> extraPart
    Nothing -> checkEFallback i g (NamS rs) t
-- Integer / real literal acceptance. The literal inhabits an expected
-- type @t@ when @t@'s wire-parent chain (transparent aliases + newtype
-- wire-parents) reaches a member of the integer (or real) base-type
-- family. The literal types at @t@ itself (the user's annotated form);
-- codegen handles per-language conversion when @t@'s effective
-- per-language form differs from the wire-parent's via the appropriate
-- @*Like@ instance.
checkE i g (IntS si x) t = do
  scope <- MM.getGeneralScope
  let tApplied = apply g t
      tEval = either (const tApplied) id (TE.evaluateType scope tApplied)
      tWire = TE.wireParentRoot scope tEval
      -- Integer literal can inhabit any integer base type (Int / Int8..Int64
      -- / UInt8..UInt64) directly, AND any real base type (Real / Float32 /
      -- Float64) via integer-to-real promotion. This is what lets @4 + 2.3@
      -- typecheck: the @4@ is checked against the same type slot as @2.3@,
      -- so when that slot resolves to @Real@, the integer literal adopts
      -- @Real@ as well. The AST node stays as IntS -- codegen consults
      -- the resolved type annotation via CodeGenerator/NumericLiteral to
      -- pick the wire form and convert the value.
      acceptable u = BT.isIntegerBaseType u || BT.isRealBaseType u
  if acceptable tApplied || acceptable tEval || acceptable tWire || literalAtRigid IntDefault g tApplied
    then return (g, tApplied, IntS si x)
    else case tApplied of
      -- Numeric literal checked against an unsolved existential: defer
      -- the type commitment. A later arg (e.g. @vec :: Vector n Int8@)
      -- may pin the existential, at which point the literal adopts that
      -- type via the deferred-numeric-literal resolution at the end of
      -- typechecking. If nothing pins it, apply the @Int@ default.
      ExistU v _ _ ->
        let g' = deferNumLit (i, v, IntDefault) g
        in return (g', tApplied, IntS si x)
      _ -> checkEFallback i g (IntS si x) t
checkE i g (RealS si x) t = do
  scope <- MM.getGeneralScope
  let tApplied = apply g t
      tEval = either (const tApplied) id (TE.evaluateType scope tApplied)
      tWire = TE.wireParentRoot scope tEval
  if BT.isRealBaseType tApplied || BT.isRealBaseType tEval || BT.isRealBaseType tWire || literalAtRigid RealDefault g tApplied
    then return (g, tApplied, RealS si x)
    else case tApplied of
      ExistU v _ _ ->
        let g' = deferNumLit (i, v, RealDefault) g
        in return (g', tApplied, RealS si x)
      _ -> checkEFallback i g (RealS si x) t
-- String, boolean, and unit literal acceptance. Same wire-parent walk
-- pattern as IntS/RealS above: a literal inhabits a newtype if the
-- newtype's wire-parent chain reaches the literal's natural primitive.
checkE i g (StrS x) t = do
  scope <- MM.getGeneralScope
  let tEval = either (const t) id (TE.evaluateType scope t)
      tWire = TE.wireParentRoot scope tEval
  if t == BT.strU || tEval == BT.strU || tWire == BT.strU
    then return (g, t, StrS x)
    else checkEFallback i g (StrS x) t
checkE i g (LogS x) t = do
  scope <- MM.getGeneralScope
  let tEval = either (const t) id (TE.evaluateType scope t)
      tWire = TE.wireParentRoot scope tEval
  if t == BT.boolU || tEval == BT.boolU || tWire == BT.boolU
    then return (g, t, LogS x)
    else checkEFallback i g (LogS x) t
checkE i g UniS t = do
  scope <- MM.getGeneralScope
  let tEval = either (const t) id (TE.evaluateType scope t)
      tWire = TE.wireParentRoot scope tEval
  if t == BT.unitU || tEval == BT.unitU || tWire == BT.unitU
    then return (g, t, UniS)
    else checkEFallback i g UniS t
-- Bidirectional-checking rule for function application. Standard
-- App-Check from Dunfield-Krishnaswami: synthesise f, instantiate
-- its Foralls to fresh existentials, then unify the function's
-- return type with the expected type BEFORE checking the args. This
-- pins the function's parameter existentials so each arg is checked
-- at a concrete type rather than at an open existential.
--
-- Motivating case: a literal at a structurally recursive type
-- (e.g. `type Pair a = (a, ?(Pair a))`) needs to be CHECKED at
-- `Pair Str` so the existing checkE TupS / checkOptionalLit rules
-- can unfold the alias one ply at a time. Synthesis would commit to
-- a finite tuple shape `(Str, (Str, ?ex))`, and rolling that back
-- into the recursive alias is narrowing, not subtyping.
--
-- The rule fires only when the function's return type after
-- ForallU-stripping is a bare existential variable (the
-- `forall a. ... -> a` shape, post-instantiation). Two reasons:
--
--   1. That is exactly the case where pre-subtype is informative
--      and safe -- it just pins the type variable to whatever the
--      expected type is. There are no type-level transformations
--      (RecDiff `r - f`, NatArith `m * n`, OptionalU wrappers, etc.)
--      to invert.
--   2. For functions with structured return types (e.g.
--      `f :: T1 n Real -> T1 n Real` or `dropF :: f@Str -> r - f`),
--      pre-subtype either defers uselessly (matching NatVar against
--      NatVar) or over-commits (forcing the row variable to a shape
--      the actual arg cannot satisfy). The OLD synth+subtype path
--      via checkEFallback handles those cases correctly.
--
-- Pattern getters/setters (ExeS PatCall in the function position)
-- also defer to checkEFallback: synthE has dedicated rules for those
-- that the App-Check shape would bypass.
--
-- Any other case the rule can't handle cleanly (propagation
-- subtype fails, partial application, arity mismatch) is delegated
-- to checkEFallback, preserving the existing synth-and-subtype
-- behaviour.
-- A setter whose result type is already known. Seating the expected
-- type into the value slots before anything is checked against them is
-- what lets @.(.n = 42) r@ write an @I32@ field: checked on its own the
-- literal freezes at @Int@ and the receiver check then reports @I32@
-- against @Int@. The receiver is still checked before the values, so
-- the field types it carries reach them too; the two orderings are not
-- in conflict, because the expected type only ever pins slots the
-- receiver would pin the same way.
--
-- A setter that changes its receiver's type has an output the expected
-- type cannot be seated into, and falls back to synth-and-subtype.
checkE i g0 e@(AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) (e0 : es0@(_ : _))) t
  | not (selectorHasBracket s) = do
      let (g1, slotVars) = statefulMap (\g _ -> newvar "set_slot_" g) g0 es0
      (g2, structType) <- selectorType g1 s
      scope <- MM.getGeneralScope
      let seated = do
            outputType <- selectorSetter slotVars s structType
            g3 <- either (const Nothing) Just (subtype scope outputType (apply g2 t) g2)
            return (g3, outputType)
      case seated of
        Nothing -> checkEFallback i g0 e t
        Just (g3, outputType) -> do
          (g4, datType, e1) <- checkG g3 e0 (apply g3 outputType)
          rejectListSelectorTarget fgidx s (apply g4 datType)
          (g5, es1) <-
            statefulMapM
              (\g (e', slotT) -> do
                 (g'', _, e'') <- checkG g e' (apply g slotT)
                 return (g'', e''))
              g4
              (zip es0 slotVars)
          let setTypes = map (apply g5) slotVars
              receiverType = apply g5 datType
              resultType =
                fromMaybe (apply g5 outputType) (selectorSetter setTypes s receiverType)
              patternType = FunU (receiverType : setTypes) resultType
              f1 = AnnoS (Idx fgidx patternType) fcidx (ExeS (PatCall (PatternStruct s)))
          return (g5, resultType, AppS f1 (e1 : es1))
checkE i g0 e@(AppS (AnnoS _ _ (ExeS _)) _) t = checkEFallback i g0 e t
-- If the expected type is wrapped in OptionalU, the App-Check shortcut
-- would pin the function's bare-existential return to the FULL optional
-- type, which then propagates the OptionalU into the function's
-- parameter existentials (e.g. for at :: f a -> a, ea := ?alpha forces
-- xs :: f ?alpha against the actual xs :: [alpha], producing an
-- occurs check `alpha <: ?alpha`). Falling back lets synth pin the
-- return at the unwrapped type and tryCoerce wrap the result at the
-- application boundary.
checkE i g0 e@(AppS _ _) t@(OptionalU _) = checkEFallback i g0 e t
checkE i g0 (AppS f xs) t = do
  (g1, funType0, funExpr0) <- synthG g0 f
  let g1' = resolveNatLabels f funType0 xs g1
      (g2, funType1) = stripForallU g1' (normalizeType funType0)
      -- The head is synthesised above. Every path out of this rule other
      -- than the one below still needs the application's own type, so
      -- finish from the head already in hand instead of synthesising the
      -- application from the start, which would synthesise the head a
      -- second time. A head that is itself a definition carries that
      -- definition's whole body, so the duplication compounds once per
      -- level of nesting.
      --
      -- An application to no arguments has a synthesis rule of its own
      -- that does not go through 'etaExpandSynthE', so it is left to the
      -- fallback.
      finish
        | null xs = checkEFallback i g0 (AppS f xs) t
        | otherwise = do
            (g', a, e') <- etaExpandSynthE i g1' funType0 funExpr0 f xs
            reconcileSynth i g' a e' t
  case funType1 of
    FunU paramTypes returnType
      | length xs == length paramTypes
      , isBareExistU (apply g2 returnType) -> do
          scope <- MM.getGeneralScope
          let returnApplied = apply g2 returnType
              expectedApplied = apply g2 t
          case subtype scope returnApplied expectedApplied g2 of
            Right g3 -> do
              (g4, _, xsAnn, _) <-
                zipCheck i g3 xs (map (apply g3) paramTypes)
              -- Re-subtype AFTER the args. The pre-args subtype only
              -- carries information that the expected type already
              -- has; when the bare-existential return collides with
              -- something that itself contains unsolved variables
              -- (the only way to reach here with a deferral) the
              -- inner arg check still has to pin things, and the
              -- post-args re-subtype is what propagates those
              -- pinnings back to the expected type.
              case subtype scope (apply g4 returnType) (apply g4 t) g4 of
                Right g5 -> return (g5, apply g5 t, AppS funExpr0 xsAnn)
                Left _ -> finish
            Left _ -> finish
      | otherwise -> finish
    _ -> finish
  where
    isBareExistU (ExistU _ _ _) = True
    isBareExistU _ = False
-- A name in argument position whose own type takes more arguments than the
-- expected type accepts. `zipWith add3 xs ys` hands a three-argument
-- function to a slot that wants a two-argument one, so each element of the
-- result is a one-argument closure. Eta-expanding to the expected arity
-- builds that closure explicitly. Without it the pool calls the
-- three-argument manifold with two arguments and dies at run time.
--
-- Only a bare reference needs this. An application is already handled by
-- 'etaExpandSynthE', and a lambda by the LamS rule above.
checkE i g0 e0 t
  | isBareFunctionRef e0
  , FunU as _ <- normalizeType t = do
      (g1, a1, e1) <- synthE' i g0 e0
      case snd (stripForallU g1 (normalizeType (apply g1 a1))) of
        FunU ts _
          | length ts > length as -> do
              (g2, e2) <- etaExpandToArity i (length as) g0 e0
              checkE' i g2 e2 t
        _ -> reconcileSynth i g1 a1 e1 t
  where
    isBareFunctionRef VarS{} = True
    isBareFunctionRef BndS{} = True
    isBareFunctionRef CallS{} = True
    isBareFunctionRef _ = False
checkE i g1 e1 b = checkEFallback i g1 e1 b

-- | Wrap a function-valued expression in a lambda of the given arity, so a
-- term that takes more arguments than its context supplies becomes an
-- explicit closure rather than an under-applied call.
etaExpandToArity ::
  Int ->
  Int ->
  Gamma ->
  ExprS Int ManyPoly Int ->
  MorlocMonad (Gamma, ExprS Int ManyPoly Int)
etaExpandToArity parentIdx arity g0 e0 = do
  let (g1, vs) = statefulMap (\g _ -> evarname g "v") g0 ([1 .. arity] :: [Int])
  argAnnos <- mapM
    (\v -> do
       idx <- MM.getCounterWithPos parentIdx
       return (AnnoS idx idx (BndS v)))
    vs
  appIdx <- MM.getCounterWithPos parentIdx
  -- The moved reference keeps the index it was checked at. A fresh index
  -- carries only a source location, so the per-index typedef scope that
  -- resolves a module-local alias in its type would be lost.
  let appE = AppS (AnnoS parentIdx parentIdx e0) argAnnos
  return (g1, LamS vs (AnnoS appIdx appIdx appE))

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
  reconcileSynth i g2 a e2 b

-- | Reconcile an already-synthesized expression against an expected type:
-- subtype, else coerce, else report. Split out of 'checkEFallback' so a
-- caller that has synthesized for its own reasons can finish the check
-- without synthesizing twice.
reconcileSynth ::
  Int ->
  Gamma ->
  TypeU ->
  ExprS (Indexed TypeU) ManyPoly Int ->
  TypeU ->
  MorlocMonad
    ( Gamma
    , TypeU
    , ExprS (Indexed TypeU) ManyPoly Int
    )
reconcileSynth i g2 a e2 b = do
  let a' = apply g2 a
      b' = apply g2 b
  scope <- MM.getGeneralScope
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
        Nothing ->
          MM.throwSourcedError i $
            "Type mismatch:"
            <> line <> "  expected: " <> prettyTypeU b'
            <> line <> "  inferred: " <> prettyTypeU a'
            <> line <> err
            <> missingInstanceHint scope a' b'

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
  let appliedInner = apply g' innerT
      innerAnno = AnnoS (Idx idx appliedInner) i e'
  return (g', apply g' (OptionalU innerT), CoerceS CoerceToOptional innerAnno)

-- | A setter is desugared into a lambda (@\\v -> pat v values...@) so
-- that it can also be written unapplied, as in @map .(.x = 1) xs@.
-- Applied directly, that wrapper changes what the typechecker sees
-- first: the lambda is synthesised before its argument, so the setter
-- rule meets a bare parameter where the receiver should be and has
-- nothing to read the written fields' types from. The values are then
-- pinned to whatever they synthesise to on their own, which is why
-- @.(.a = "new") r@ was rejected when @a@ is @?Str@, and the rebuilt
-- record's type keeps only the fields the selector names.
--
-- So reduce the redex before either direction of the typechecker looks
-- at it. The parameter is compiler-generated, occurs exactly once, and
-- occurs in argument position, so substituting the receiver for it is
-- a copy rather than a duplication of work.
reduceSetterRedex ::
  ExprS Int ManyPoly Int ->
  Maybe (ExprS Int ManyPoly Int)
reduceSetterRedex
  ( AppS
      ( AnnoS _ _
          ( LamS [v]
              ( AnnoS _ _
                  ( AppS
                      pat@(AnnoS _ _ (ExeS (PatCall (PatternStruct _))))
                      (AnnoS _ _ (BndS v') : values@(_ : _))
                    )
                )
            )
        )
      [receiver]
    )
    | v == v' = Just (AppS pat (receiver : values))
reduceSetterRedex _ = Nothing

-- | Choose the record-shaped type with the most keys. Non-record cases
-- fall back to the second argument (the standard "return the expected
-- type" convention used by checkEFallback).
--
-- On a tie a structural expected type is kept, but re-laid-out in the
-- order the synthesised type uses. A record's key order is its layout
-- on the wire, and the synthesised type is the one the expression's own
-- sub-nodes were built from; an expected type ordering the same keys
-- differently -- a pattern selector's structural type sorts them --
-- would relabel the fields without moving the values behind them.
--
-- A declared record is never re-laid-out. Its order is the one its
-- language forms are written against, and a subtype against a declared
-- record solves the synthesised side to it, so the two agree already.
pickWiderRecord :: TypeU -> TypeU -> TypeU
pickWiderRecord a b = case (recordKeys a, recordKeys b) of
  (Just ka, Just kb)
    | length ka > length kb -> a
    | ka /= kb, List.sort ka == List.sort kb -> reorderRecordKeys ka b
  _ -> b
  where
    recordKeys (ExistU _ _ (rs, _)) = Just (map fst rs)
    recordKeys (NamU _ _ _ rs) = Just (map fst rs)
    recordKeys _ = Nothing

-- | Rewrite a structural record type's rows into the given key order.
-- The key list must be a permutation of the type's own keys; any key it
-- does not name leaves the type untouched. A declared record ('NamU')
-- is returned as it is: its row order is the order its per-language
-- forms are built against.
reorderRecordKeys :: [Key] -> TypeU -> TypeU
reorderRecordKeys ks t = case t of
  ExistU v ps (rs, rc) -> maybe t (\rs' -> ExistU v ps (rs', rc)) (permute rs)
  _ -> t
  where
    permute rs = mapM (\k -> (,) k <$> lookup k rs) ks

subtype' :: Int -> TypeU -> TypeU -> Gamma -> MorlocMonad Gamma
subtype' i a b g = do
  scope <- MM.getGeneralScope
  insetSay $ parens (pretty a) <+> "<:" <+> parens (pretty b)
  case subtype scope a b g of
    (Left err') -> MM.throwSourcedError i err'
    (Right g') -> do
      mapM_ (\(t1, t2) ->
        MM.sayV $ "Warning: deferred Nat constraint:" <+> prettyTypeU t1 <+> "~" <+> prettyTypeU t2
        ) (newlyDeferred g g')
      return (locateDeferred i g g')

-- | Group Nat equations that share a variable. Each group starts with its
-- equation that comes first in the input.
natComponents :: [(Int, (TypeU, TypeU), (NS.NatExpr, NS.NatExpr))] -> [[(Int, (TypeU, TypeU), (NS.NatExpr, NS.NatExpr))]]
natComponents = foldr insert []
  where
    vars (_, _, (a, b)) = NS.freeNatVars a <> NS.freeNatVars b
    insert x groups =
      let (linked, apart) = List.partition (any (not . Set.null . Set.intersection (vars x) . vars)) groups
      in (x : concat linked) : apart

-- | Attribute the kind equations deferred between two contexts to term
-- @i@, unless an inner term already claimed them.
locateDeferred :: Int -> Gamma -> Gamma -> Gamma
locateDeferred i g g' = g' { gammaDeferredAt = foldr (\p -> Map.insertWith (\_ old -> old) p i) (gammaDeferredAt g') (newlyDeferred g g') }

-- | The kind equations deferred between two contexts.
newlyDeferred :: Gamma -> Gamma -> [(TypeU, TypeU)]
newlyDeferred g g' = take (length (gammaDeferred g') - length (gammaDeferred g)) (gammaDeferred g')

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
-- | When a 'Type mismatch' error involves a fully-applied nominal
-- newtype on one side (the inferred type) and an existential
-- application on the other (the expected type, typically arising
-- from a polymorphic class method like @sum :: Foldable f => f a ->
-- a@), the most likely cause is a missing typeclass instance for
-- the newtype. The subtype check fails structurally because
-- newtypes are nominally distinct from their body and the class
-- can't dispatch without an instance. Append a hint pointing at
-- the newtype so users don't have to decode the structural
-- "Cannot compare" message.
missingInstanceHint :: Scope -> TypeU -> TypeU -> MDoc
missingInstanceHint scope inferred expected
  | Just nativeTv <- newtypeHead scope inferred
  , hasExistHead expected =
      line <> "  hint:" <+> squotes (pretty nativeTv)
        <+> "is a newtype; if a polymorphic class is expected here,"
        <+> "the newtype needs an explicit instance"
        <+> "(declare e.g. 'instance ClassName"
        <+> pretty nativeTv <> "' alongside the newtype)."
  | otherwise = mempty
  where
    newtypeHead :: Scope -> TypeU -> Maybe TVar
    newtypeHead s t = case t of
      VarU v          -> isNewtypeKind s v
      AppU (VarU v) _ -> isNewtypeKind s v
      _               -> Nothing

    isNewtypeKind :: Scope -> TVar -> Maybe TVar
    isNewtypeKind s v = case Map.lookup v s of
      Just ((_, _, _, _, TypedefNewtype) : _) -> Just v
      _ -> Nothing

    hasExistHead :: TypeU -> Bool
    hasExistHead (AppU (ExistU _ _ _) _) = True
    hasExistHead (ExistU _ _ _)          = True
    hasExistHead _                       = False

getNatArgs :: Scope -> TypeU -> [TypeU]
getNatArgs scope (AppU (VarU v) args) =
  case Map.lookup v scope of
    Just ((params, _, _, _, _) : _) ->
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
-- Widen through effect wrappers: @<E1> T@ coerces to @<E2> ?T@ by
-- widening the inner value type, when the closed row E1 is included in
-- the closed row E2 (the same inclusion the EffectU <: EffectU subtype
-- rule uses). This is what lets a do-block result satisfy a declared
-- @<E> ?T@ return. Open-row tails decline. The inner OptionalU is placed
-- under the EffectU by 'applyCoercion'.
tryCoerce scope (EffectU e1 i1) (EffectU e2 i2) g
  | closedIncluded (applyEff g e1) (applyEff g e2) = tryCoerce scope i1 i2 g
  where
    closedIncluded a b = not (effectSetHasVar a) && not (effectSetHasVar b) && effectSubsetOf a b
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

    -- Peel ForallU so a polymorphic effectful type ('forall a. <E> a',
    -- e.g. a class method 'random :: forall a. <Random> a') still
    -- contributes its effects. Without this the empty effect set
    -- collapses via 'mkEffectU', turning '<> Real' into a bare 'Real'
    -- at the next 'apply' and breaking the enclosing EvalS.
    effectOfAnno (AnnoS (Idx _ t) _ _) = case peelForallU t of
      EffectU effs _ -> effs
      _ -> emptyEffectSet

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
-- This is the inference half of EFF-10: only a bind runs a suspension
-- (EFF-6).  The companion check
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

    effectOfAnno (AnnoS (Idx _ t) _ _) = case peelForallU t of
      EffectU effs _ -> effs
      _ -> emptyEffectSet

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
-- return type permits.  Fires the widening-rule rejection of EFF-10.
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
    else do
      -- Look up per-label escapability so the fix suggestion adapts:
      -- - non-escapable missing labels can only be declared
      -- - escapable missing labels may alternatively be handled by
      --   a user-defined handler function
      -- - `Err` specifically has @catch as its built-in handler.
      effectMap <- MM.gets stateEffects
      let anyEscapable = any (\l -> Map.lookup l effectMap == Just True)
                             (Set.toList uncovered)
          errMissing = Set.member "Err" uncovered
          handlerLine
            | not anyEscapable = ""
            | errMissing =
                "\nEscapable effects may alternatively be handled locally"
                  <+> "with a handler function;"
                  <+> "@catch specifically discharges Err."
            | otherwise =
                "\nEscapable effects may alternatively be handled locally"
                  <+> "with a handler function."
          blameIdx = case uncoveredEvalIdxs uncovered innerBody of
            (i : _) -> i
            [] -> idx
      MM.throwSourcedError blameIdx $
        "Effect-coverage failure: body produces effect(s) not in the signature."
          <> "\n  declared:" <+> renderEffectLabels declEffs
          <> "\n  inferred:" <+> renderEffectLabels inferred
          <> "\n  missing: " <+> renderEffectLabels uncovered
          <> "\nFix by declaring the missing effect(s) in the signature."
          <> handlerLine
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

    effLabels (AnnoS (Idx _ t) _ _) = case peelForallU t of
      EffectU effs _ -> resolveEffectSet effs
      _ -> Set.empty

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
      scope <- MM.getGeneralScope
      case TE.evaluateType scope t of
        (Left (SystemError e)) -> MM.throwSourcedError m e
        (Left e) -> MM.throwError e
        (Right tu) -> return (Idx m tu)

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
  (g', t, e) <- synthE i g x
  leave "synthE"
  seeGamma g'
  insetSay $ "synthesized type = " <> pretty t
  return (locateDeferred i g g', t, e)

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
  (g', t', e) <- checkE i g x t
  leave "checkE"
  seeGamma g'
  seeType t'
  return (locateDeferred i g g', t', e)

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
-- When a function has labeled nat params (e.g., m@Int -> Tensor1 m Real)
-- and the corresponding arguments are int literals or let-bound ints,
-- inject NatVarU solutions into gamma so the return type gets concrete
-- dimensions. Same for Str labels (e.g., f@Str -> Tagged f a) - extract
-- string literals from the corresponding args and inject StrVarU solutions
-- into gammaKindSubs. See plans/tables/05-labels-as-type-vars.md.
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
        keyed k = Map.mapKeys (\v -> (v, k))
        combined = Map.unions
          [ keyed KindNat natSolutions
          , keyed KindStr strSolutions
          , keyed (KindList KindStr) listSolutions
          ]
    in g { gammaKindSubs = Map.union combined (gammaKindSubs g) }
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
peakSExpr (ConS tv n _ _) = "ConS" <+> pretty tv <> "." <> pretty n
peakSExpr (ExeS exe) = "ExeS" <+> pretty exe
peakSExpr (LetS v _ _) = "LetS" <+> pretty v
peakSExpr (LetBndS v) = "LetBndS" <+> pretty v
peakSExpr (CallS v) = "CallS" <+> pretty v
peakSExpr (DoBlockS _) = "DoBlockS"
peakSExpr (EvalS _) = "EvalS"
peakSExpr (CoerceS _ _) = "CoerceS"
peakSExpr (IfS _ _ _) = "IfS"
peakSExpr (IntrinsicS intr _) = "@" <> pretty (intrinsicName intr)


-------------------------------------------------------------------------------
-- Signatures as contracts

-- | Check each definition against its signature as a contract: the
-- signature's type variables are rigid, so the body must work at every
-- instance of them, and every class method the body uses at a rigid type must
-- follow from the signature's constraints (a superclass follows from its
-- subclass). The trees are the definitions' own bodies, with references to
-- other signed top-level terms typed by their signatures ('Treeify'). The
-- results are discarded: this only rejects programs.
validate :: [AnnoS Int ManyPoly Int] -> [(Int, ClassName, [TypeU])] -> MorlocMonad ()
validate trees instances = do
  mapM_ one trees
  mapM_ checkSuperclassInstance instances
  where
    one e0@(AnnoS i _ _) = do
      let g0 = emptyGamma {gammaRigid = Just Set.empty}
      (g1, _, _) <- synthG g0 e0
      let rigid = fromMaybe Set.empty (gammaRigid g1)
          givens = fromMaybe [] (gammaAssumedConstraints g1)
          obligations = [apply g1 c | c@(Constraint _ _) <- gammaConstraints g1]
      -- a signature's effect variable may be passed through, never fixed
      mapM_
        (\(v, row) -> case applyEff g1 row of
            EffectVar w | w == v || not (Set.member w rigid) -> return ()
            row' ->
              MM.throwSourcedError i $
                "The definition fixes the effect variable" <+> squotes (pretty v)
                  <+> "of its signature to" <+> prettyEffectSet row'
                  <> "; declare those effects in the signature instead")
        [(v, row) | (v, row) <- Map.toList (gammaEffSubs g1), Set.member v rigid]
      closure <- superclassClosure givens
      heads <- instanceHeads
      mapM_
        (requireEntailed i closure)
        [ c
        | c@(Constraint cls args) <- obligations
        , any (mentionsRigid rigid) args
        , not (any (covers args) (Map.findWithDefault [] cls heads))
        ]

-- | A name the renamer gave a local binding: its first renamed segment is a
-- number (a recursion token alone is not local).
isLocalName :: EVar -> Bool
isLocalName (EV v) = case MT.breakOn "`" v of
  (_, rest) -> case MT.uncons (MT.drop 1 rest) of
    Just (c, _) -> c >= '0' && c <= '9'
    Nothing -> False

-- | A type whose head quantifiers over names in scope are dropped, those
-- names meaning the variables they are in scope as: rigid variables when
-- checking a definition against its signature, the existentials an enclosing
-- quantifier was opened to otherwise.
scopeType :: Bool -> Map.Map TVar TVar -> TypeU -> TypeU
scopeType rigid m t
  | Map.null m = t
  | otherwise = foldr (\(from, to) acc -> open to (substituteTVar from (VarU to) acc)) (strip t) (Map.toList m)
  where
    open to x = if rigid then x else substitute to x
    strip (ForallU v x)
      | Map.member v m = strip x
      | otherwise = ForallU v (strip x)
    strip x = x

scopeEType :: Bool -> Map.Map TVar TVar -> EType -> EType
scopeEType rigid m et
  | Map.null m = et
  | otherwise = et {etype = scopeType rigid m (etype et), econs = Set.map sub (econs et)}
  where
    sub (Constraint cls ts) = Constraint cls (map (scopeType rigid m) ts)
    sub c = c

-- | The effect-row variables of a type.
typeEffectVars :: TypeU -> [TVar]
typeEffectVars t = case t of
  EffectU es x -> effectSetVars es <> typeEffectVars x
  ForallU _ x -> typeEffectVars x
  FunU ts x -> concatMap typeEffectVars ts <> typeEffectVars x
  AppU x ts -> typeEffectVars x <> concatMap typeEffectVars ts
  NamU _ _ ps rs -> concatMap typeEffectVars ps <> concatMap (typeEffectVars . snd) rs
  OptionalU x -> typeEffectVars x
  _ -> []

-- | A quantified type instantiated with fresh existentials; the constraints,
-- rewritten over the same existentials, are recorded as obligations.
instantiateObligations :: Gamma -> TypeU -> [Constraint] -> (Gamma, TypeU)
instantiateObligations g (ForallU v t) cs =
  instantiateObligations (g +> v) (substitute v t) (map (substC v) cs)
  where
    substC w (Constraint c ts) = Constraint c (map (substitute w) ts)
    substC _ c = c
instantiateObligations g t cs = (g {gammaConstraints = gammaConstraints g ++ cs}, t)

-- | An instance exists only where an instance of each of its class's
-- superclasses exists at the same types: @instance Mn Int@ needs
-- @instance Sg Int@ when @class Sg a => Mn a@. A superclass with no methods
-- leaves no trace of its instances and is not checked.
checkSuperclassInstance :: (Int, ClassName, [TypeU]) -> MorlocMonad ()
checkSuperclassInstance (i, cls, ts0) = do
  -- the instance's own type variables are fixed names while matching
  let ts = map (snd . unqualify) ts0
  classDefs <- MM.gets stateClassDefs
  tcls <- MM.gets stateTypeclasses
  let vars = [classVars inst | inst <- Map.elems tcls, className inst == cls]
      needed = case (Map.lookup cls classDefs, vars) of
        (Just scs, vs : _) | length vs == length ts ->
          [ (sc, map (substAll (zip vs ts)) sargs) | Constraint sc sargs <- scs ]
        _ -> []
      substAll ps t = foldr (\(v, r) acc -> substituteTVar v r acc) t ps
      methodsOf sc = [inst | inst <- Map.elems tcls, className inst == sc]
  mapM_
    ( \(sc, args) -> case methodsOf sc of
        [] -> return ()
        insts ->
          let heads = [h | inst <- insts, tt <- instanceTerms inst, Just h <- [instanceHeadOf inst tt]]
           in if any (covers args) heads
                then return ()
                else
                  MM.throwSourcedError i $
                    "The instance" <+> pretty cls <+> hsep (map (parens . pretty) ts)
                      <+> "needs an instance" <+> pretty sc <+> hsep (map (parens . pretty) args)
                      <+> "of its superclass, and there is none."
    )
    needed

-- | Every instance head of every class, as its quantified variables and head
-- types. An obligation an instance head matches -- e.g. @Eq a@ under a
-- catch-all @instance Eq a@ -- holds without a declared constraint.
instanceHeads :: MorlocMonad (Map.Map ClassName [([TVar], [TypeU])])
instanceHeads = do
  tcls <- MM.gets stateTypeclasses
  return $
    Map.fromListWith (<>)
      [ (className inst, [h])
      | inst <- Map.elems tcls
      , tt <- instanceTerms inst
      , Just h <- [instanceHeadOf inst tt]
      ]

-- | The head of one implementation of a class method: the instance's
-- quantified variables and the types it instantiates the class at.
instanceHeadOf :: Instance -> TermTypes -> Maybe ([TVar], [TypeU])
instanceHeadOf inst tt = do
  et <- termGeneral tt
  let (instVars, instBody) = unqualify (etype et)
      (_, classBody) = unqualify (etype (classType inst))
  sub <- matchTypeU (Set.fromList (classVars inst)) classBody instBody
  h <- mapM (`Map.lookup` sub) (classVars inst)
  return (instVars, h)

-- | Whether an instance head matches the given class arguments.
covers :: [TypeU] -> ([TVar], [TypeU]) -> Bool
covers args (instVars, h) =
  isJust (matchTypeU (Set.fromList instVars) (headTuple h) (headTuple args))
  where
    headTuple xs = AppU (VarU (TV "__instance_head__")) xs

-- | A quantified type's variables and body.

mentionsRigid :: Set.Set TVar -> TypeU -> Bool
mentionsRigid rigid t = any isRigid (Set.toList (free t))
  where
    isRigid (VarU v) = Set.member v rigid
    isRigid _ = False

requireEntailed :: Int -> [Constraint] -> Constraint -> MorlocMonad ()
requireEntailed defIdx closure c
  | c `elem` closure = return ()
  | otherwise =
      MM.throwSourcedError defIdx $
        "This definition needs" <+> prettyConstraint c
          <+> "but its signature does not declare it (the signature's"
          <+> "constraints and their superclasses give" <+> declared <> ")."
  where
    declared = case closure of
      [] -> "no constraints"
      cs -> hcat (punctuate ", " (map prettyConstraint cs))

-- | Constraints together with every superclass constraint they imply.
superclassClosure :: [Constraint] -> MorlocMonad [Constraint]
superclassClosure cs0 = do
  classDefs <- MM.gets stateClassDefs
  tcls <- MM.gets stateTypeclasses
  let varsOf cls = case [classVars inst | inst <- Map.elems tcls, className inst == cls] of
        (vs : _) -> Just vs
        [] -> Nothing
      supers (Constraint cls args) = case (Map.lookup cls classDefs, varsOf cls) of
        (Just scs, Just vs) | length vs == length args ->
          [ Constraint sc (map (substAll (zip vs args)) sargs)
          | Constraint sc sargs <- scs
          ]
        _ -> []
      supers _ = []
      substAll ps t = foldr (\(v, r) acc -> substituteTVar v r acc) t ps
      grow seen [] = seen
      grow seen (c : rest)
        | c `elem` seen = grow seen rest
        | otherwise = grow (c : seen) (supers c <> rest)
  return (grow [] cs0)

-- | A literal may inhabit a rigid type variable when the constraints in scope
-- (closed under superclasses) give that variable a class whose values have
-- literals: 'BT.integralClass' for an integer literal, 'BT.numericClass' for
-- a real one.
literalAtRigid :: NumLitKind -> Gamma -> TypeU -> Bool
literalAtRigid kind g (VarU v) = case gammaRigid g of
  Just rigid
    | Set.member v rigid ->
        let wanted = case kind of
              IntDefault -> [BT.integralClass, BT.numericClass]
              RealDefault -> [BT.numericClass]
         in any
              (\c -> case c of
                  Constraint cls [VarU v'] -> v' == v && cls `elem` wanted
                  _ -> False)
              (fromMaybe [] (gammaAssumedConstraints g))
  _ -> False
literalAtRigid _ _ _ = False

-- | One-way matching: bind the pattern's variables (those in @pats@) so the
-- pattern equals the target. Other structure must agree exactly.
matchTypeU :: Set.Set TVar -> TypeU -> TypeU -> Maybe (Map.Map TVar TypeU)
matchTypeU pats = go Map.empty
  where
    go sub (VarU v) t
      | Set.member v pats = case Map.lookup v sub of
          Nothing -> Just (Map.insert v t sub)
          Just t' -> if t' == t then Just sub else Nothing
    go sub (FunU as a) (FunU bs b)
      | length as == length bs = goList sub (a : as) (b : bs)
    go sub (AppU f as) (AppU h bs)
      | length as == length bs = goList sub (f : as) (h : bs)
    go sub (OptionalU a) (OptionalU b) = go sub a b
    go sub (EffectU _ a) (EffectU _ b) = go sub a b
    go sub (NamU o1 n1 ps1 rs1) (NamU o2 n2 ps2 rs2)
      | o1 == o2 && n1 == n2 && length ps1 == length ps2 && map fst rs1 == map fst rs2 =
          goList sub (ps1 <> map snd rs1) (ps2 <> map snd rs2)
    go sub a b = if a == b then Just sub else Nothing
    goList sub [] [] = Just sub
    goList sub (a : as) (b : bs) = go sub a b >>= \sub' -> goList sub' as bs
    goList _ _ _ = Nothing
