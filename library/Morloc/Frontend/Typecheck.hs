{-# LANGUAGE CPP #-}
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
import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
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
typecheck ::
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheck = mapM run
  where
    run :: AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
    run e0 = do
      -- standardize names for lambda bound variables (e.g., x0, x1 ...)
      let g0 = Gamma {gammaCounter = 0, gammaSlot = 0, gammaContext = IntMap.empty, gammaExist = Map.empty, gammaSolved = Map.empty, gammaDeferred = [], gammaNatSubs = Map.empty, gammaIntVals = Map.empty}
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

      -- perform a final application of gamma the final expression and return
      -- (is this necessary?)
      return (applyGen g3 e3)

-- TypeU --> Type
resolveTypes :: AnnoS (Indexed TypeU) Many Int -> AnnoS (Indexed Type) Many Int
resolveTypes (AnnoS (Idx i t) ci e) =
  AnnoS (Idx i (typeOf t)) ci (f e)
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
    f (RealS x) = RealS x
    f (IntS x) = IntS x
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
          emptyGamma = Gamma 0 0 IntMap.empty Map.empty Map.empty [] Map.empty Map.empty
          isCompatible t = isSubtypeOf2 scope t gtEval
                        || isJust (tryCoerce scope t gtEval emptyGamma)
          rssSubtypes = [x | x@(EType t _ _ _, _) <- rss, isCompatible t]


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
          g1 <- connectInstance g0 es0
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
    f _ g0 (RealS x) = return (g0, RealS x)
    f _ g0 (IntS x) = return (g0, IntS x)
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

    connectInstance :: Gamma -> [AnnoS (Indexed TypeU) f c] -> MorlocMonad Gamma
    connectInstance g0 [] = return g0
    connectInstance g0 (AnnoS (Idx i t) _ _ : es) = do
      scope <- MM.getGeneralScope i
      case subtype scope (stripCoercionWrappers gt) t g0 of
        (Left e) -> throwTypeError i e
        (Right g1) -> connectInstance g1 es

-- prepare a general, indexed typechecking error
throwTypeError :: Int -> MDoc -> MorlocMonad a
throwTypeError i msg = MM.throwSourcedError i ("General type error:" <+> msg)

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
  (g', t', e') <- case Map.lookup j annotation of
    Nothing -> checkE' i g e t
    (Just annType) -> do
      gAnn <- subtype' i annType t g
      checkE' i gAnn e t
  return (g', t', AnnoS (Idx i t') j e')

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
  (g', t, e') <- case Map.lookup ci annotation of
    Nothing -> synthE' gi g e
    (Just annType) -> checkE' gi g e annType
  return (g', t, AnnoS (Idx gi t) ci e')

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
synthE _ g (RealS x) = return (g, BT.realU, RealS x)
synthE _ g (IntS x) = return (g, BT.intU, IntS x)
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

  let f1 = (AnnoS (Idx fgidx ft) fcidx (ExeS (PatCall (PatternStruct s))))

  return (g2, apply g2 retType, AppS f1 [e'])

-- handle setter patterns
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) (e0 : es0)) = do
  (g1, (unzip -> (setTypes, es1))) <-
    statefulMapM (\s' e -> synthG s' e |>> (\(a, b, c) -> (a, (b, c)))) g0 es0

  -- generate an existential type that contains the pattern
  (g2, outputType) <- selectorType g1 s |>> second (selectorSetter setTypes s)

  (g3, datType, e1) <- checkG g2 e0 outputType

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
  let (g1, t1) = rename g0 (etype t0)
      g1' = g1 ++> [AnnG v t1]
  (g2, t2, xs1) <- foldCheck g1' xs0 t1
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
  g5 <- subtype' i t3 t2 g4
  return (g5, apply g5 t2, IfS cond' thenE' elseE')
synthE _ g (DoBlockS e) = do
  (g1, t1, e1) <- synthG g e
  let effs = collectDoEffects e1
  return (g1, EffectU effs t1, DoBlockS e1)
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
    t -> throwTypeError i $ "Cannot force non-thunk type:" <+> pretty t
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
zipCheck i _ _ [] = MM.throwSourcedError i "Compiler bug (__FILE__:__LINE__): too many arguments in zipCheck"

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
checkE i g1 (LstS (e : es)) (AppU v [t]) = do
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
checkE _ g (DoBlockS e) (EffectU effs t) = do
  (g1, t1, e1) <- checkG g e t
  return (g1, EffectU effs t1, DoBlockS e1)
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
    t' -> throwTypeError i $ "Cannot force non-thunk type:" <+> pretty t'

-- Resolve solved existentials so specific handlers (LstS, TupS, etc.) can match
checkE i g e t@(ExistU v _ _)
  | Just _ <- lookupU v g
  = checkE' i g e (apply g t)
--   Sub (with coercion fallback)
checkE i g1 e1 b = do
  (g2, a, e2) <- synthE' i g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  scope <- MM.getGeneralScope i
  case subtype scope a' b' g2 of
    Right g3 -> return (g3, apply g3 b', e2)
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
-- Coerce a pure value to an effectful type: a -> <E> a
tryCoerce scope a (EffectU effs b) g =
  case subtype scope a b g of
    Right g' -> Just ([CoerceToEffect (resolveEffectSet effs)], g')
    Left _ -> case tryCoerce scope a b g of
      Just (cs, g') -> Just (CoerceToEffect (resolveEffectSet effs) : cs, g')
      Nothing -> Nothing
tryCoerce _ _ _ _ = Nothing

-- | Strip OptionalU wrappers that result from coercion.
-- Used in instance resolution to match the underlying type.
stripCoercionWrappers :: TypeU -> TypeU
stripCoercionWrappers (OptionalU t) = stripCoercionWrappers t
stripCoercionWrappers t = t

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
tryEvalConst _ (IntS n) = Just (ConstInt n)
tryEvalConst g (LetBndS v) = Map.lookup v (gammaIntVals g)
tryEvalConst g (BndS v) = Map.lookup v (gammaIntVals g)
tryEvalConst g (TupS es) = ConstTup <$> mapM (\(AnnoS _ _ e) -> tryEvalConst g e) es
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

-- | Resolve nat labels from int literal arguments.
-- When a function has labeled nat params (e.g., m:Int -> Tensor1 m Real)
-- and the corresponding arguments are int literals or let-bound ints,
-- inject NatVarU solutions into gamma so the return type gets concrete dimensions.
resolveNatLabels ::
  AnnoS Int ManyPoly Int ->  -- the function expression (pre-synthesis)
  TypeU ->                    -- the synthesized (renamed) function type
  [AnnoS Int ManyPoly Int] -> -- the arguments
  Gamma -> Gamma
resolveNatLabels (AnnoS _ _ (VarS _ (MonomorphicExpr (Just et) _))) funType args g
  | not (Map.null labels) =
    let origNvs = nub (collectNatVarNames (etype et))
        renamedNvs = nub (collectNatVarNames funType)
        renMap = Map.fromList (zip origNvs renamedNvs)
        solutions = Map.fromList
          [ (renamedVar, NatLitU n)
          | (origVar, argIdx) <- Map.toList labels
          , Just renamedVar <- [Map.lookup origVar renMap]
          , argIdx < length args
          , Just n <- [tryExtractIntPre g (args !! argIdx)]
          ]
    in g { gammaNatSubs = Map.union solutions (gammaNatSubs g) }
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
peakSExpr (RealS x) = "RealS" <+> viaShow x
peakSExpr (IntS x) = "IntS" <+> pretty x
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
