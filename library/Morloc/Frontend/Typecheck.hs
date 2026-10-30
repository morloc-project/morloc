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
import qualified Morloc.Typecheck.NatDecide as ND

-- | True for the set-theoretic constraints that 'dischargeConstraints'
-- resolves. A class-form constraint left in gamma is not reported.
isPrimitiveConstraint :: Constraint -> Bool
isPrimitiveConstraint (Constraint _ _) = False
isPrimitiveConstraint (CMember _ _) = True
isPrimitiveConstraint (CSubset _ _) = True
isPrimitiveConstraint (CDisjoint _ _) = True

-- | Resolve the numeric literals that were checked against an unsolved
-- existential. Real literals go first, so that an existential shared by a
-- real and an integer literal becomes @Real@ whatever the argument order;
-- an integer literal accepts a real type, never the reverse. An integer
-- literal whose type must be Numeric defaults like a real one.
resolvePendingNumLits :: Gamma -> [(Int, TVar, NumLitKind)] -> MorlocMonad Gamma
resolvePendingNumLits g0 entries = do
  closed <- superclassClosure [c | c@(Constraint _ [_]) <- gammaConstraints g0]
  let numericVars = Set.fromList
        [ v
        | Constraint cls [t] <- closed
        , cls == BT.numericClass
        , ExistU v _ _ <- [apply g0 t]
        ]
  let (intLits0, realLits0) = partition (\(_, _, k) -> k == IntDefault) entries
      (numericInts, intLits) = partition (\(_, v, _) -> Set.member (chainEnd v) numericVars) intLits0
      realLits = realLits0 ++ numericInts
  g1 <- foldlM (resolveOne BT.isRealBaseType BT.realU) g0 realLits
  foldlM (resolveOne acceptableForInt BT.intU) g1 intLits
  where
    chainEnd v = case apply g0 (ExistU v ([], Open) ([], Open)) of
      ExistU v' _ _ -> v'
      _ -> v

    acceptableForInt t = BT.isIntegerBaseType t || BT.isRealBaseType t

    -- Must accept exactly what the check-mode rules for IntS/RealS accept.
    baseOKThroughAliases :: (TypeU -> Bool) -> TypeU -> MorlocMonad Bool
    baseOKThroughAliases baseOK t = do
      scope <- MM.getGeneralScope
      let tEval = either (const t) id (TE.evaluateType scope t)
          tWire = TE.wireParentRoot scope tEval
      return (baseOK t || baseOK tEval || baseOK tWire)

    resolveOne :: (TypeU -> Bool) -> TypeU -> Gamma -> (Int, TVar, NumLitKind) -> MorlocMonad Gamma
    resolveOne baseOK defaultT g (i, v, _) =
      -- apply walks an existential chain to its end; the default is
      -- solved there.
      let original = ExistU v ([], Open) ([], Open)
          resolved = peelWrappers (apply g original)
      in case resolved of
           ExistU vEnd _ _ ->
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

    -- A literal in a do-block or an optional position is judged on the
    -- payload type.
    peelWrappers :: TypeU -> TypeU
    peelWrappers (EffectU _ t)  = peelWrappers t
    peelWrappers (OptionalU t)  = peelWrappers t
    peelWrappers t              = t

typecheck ::
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheck = mapM typecheckRoot

-- | A recursive call reaches the one function its definition becomes, so it
-- has that definition's type. A class method named inside its own instance
-- at another type resolves by that type (CLS-2); bound to the instance
-- being defined instead, it would run that instance on values of the wrong
-- type (#160). A mismatch between two ground types is reported as a
-- compiler bug; a mismatch involving an unsolved type is not checked.
assertBackEdgeTypes :: AnnoS (Indexed TypeU) Many Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
assertBackEdgeTypes root = go Map.empty root >> return root
  where
    go :: Map.Map EVar TypeU -> AnnoS (Indexed TypeU) Many Int -> MorlocMonad ()
    go env (AnnoS (Idx i t) _ e) = case e of
      CallS v -> case Map.lookup v env of
        Just tdef
          | groundU t
          , groundU tdef
          , typeOf t /= typeOf tdef ->
              MM.throwCompilerBugAt i $
                "the call of" <+> squotes (pretty v) <+> "here is bound to its own definition, of type"
                  <+> pretty tdef <> ", at another type," <+> pretty t
        _ -> return ()
      VarS v (Many xs) -> mapM_ (go (Map.insert v t env)) xs
      _ -> mapM_ (go env) (foldExprS (: []) e)
    groundU ty = case ty of
      ExistU {} -> False
      VarU _ -> False
      KVarU _ -> False
      ListVarU _ -> False
      SetVarU _ -> False
      ForallU _ _ -> False
      FunU ts x -> all groundU (x : ts)
      AppU x ts -> all groundU (x : ts)
      NamU _ _ ps rs -> all groundU ps && all (groundU . snd) rs
      EffectU _ x -> groundU x
      OptionalU x -> groundU x
      OpU _ ts -> all groundU ts
      LabeledU _ x -> groundU x
      _ -> True

-- | The record literals whose type checking left open, before instance
-- resolution (REC-4).
openRecordLiterals :: AnnoS (Indexed TypeU) ManyPoly Int -> Set.Set Int
openRecordLiterals t = Set.fromList [i | AnnoS (Idx i (ExistU {})) _ (NamS _) <- annoNodes t]

-- | REC-4: an instance never gives a record literal its type. A literal that
-- was open before instance resolution and has a type after it is rejected
-- at the literal; one still open stays an anonymous record.
rejectInstanceTypedRecords :: Set.Set Int -> AnnoS (Indexed TypeU) Many Int -> MorlocMonad ()
rejectInstanceTypedRecords open t =
  case [i | AnnoS (Idx i ty) _ (NamS _) <- annoNodes t, Set.member i open, not (isExist ty)] of
    [] -> return ()
    (i : _) ->
      MM.throwSourcedError i
        "This record literal has no type of its own: a class method cannot give it one. Give it a type with a signature or an annotation"
  where
    isExist ExistU {} = True
    isExist _ = False

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
typecheckRoot = typecheckWith Nothing >=> assertStructuralTypesExpanded >=> assertBackEdgeTypes

-- | Check one tree against a type, as a use of it at that type is checked.
typecheckRootAt :: TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckRootAt t = typecheckWith (Just t) >=> assertStructuralTypesExpanded >=> assertBackEdgeTypes

typecheckWith :: Maybe TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckWith expected = run
  where
    run :: AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
    run e0 = do
      scope <- MM.getGeneralScope
      let g0 = emptyGamma
      (g1raw, _, e1) <- case expected of
        Nothing -> synthG g0 e0
        Just t -> checkG g0 e0 t

      -- Literal defaults must precede instance resolution: in @3 <= 5@
      -- the class variable is solved only by the Int default.
      g1 <- resolvePendingNumLits g1raw (gammaPendingNumLits g1raw)

      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g1:"
      seeGamma g1
      insetSay "========================================================"
      e2 <- coerceLiteralsToOptional . mapAnnoSG (fmap normalizeType) . applyGen g1 $ e1

      let openRecords = openRecordLiterals e2
      (g2, e3) <- resolveInstances g1 e2
      let g3 = apply g2 g2

      -- Decide the Nat and Int conditions left open now that existentials
      -- are solved: the deferred equations, each at the term where it
      -- arose, and the non-negativity of every Nat position still open, at
      -- the term whose type holds it (DIM-6, KIND-4, DIM-7).
      let rootIndex = case e0 of AnnoS gi _ _ -> gi
          site p = Map.findWithDefault rootIndex p (gammaDeferredAt g3)
          natEquation p@(t1, t2) = case (typeUToNatExpr (apply g3 t1), typeUToNatExpr (apply g3 t2)) of
            (Just a, Just b) -> Just (site p, p, (a, b))
            _ -> Nothing
          natEquations = mapMaybe natEquation (gammaDeferred g3)
          trivial (a, b) = NS.normalize (NS.NatSub a b) == NS.NatSOP []
          -- Both sides of an equation fill Nat positions, so each is a
          -- natural too.
          conditions = firstOfEach $
            concat
              [ [ (i, ND.NatEq a b, d1 <+> "~" <+> d2)
                , (i, ND.NatGe a (NS.NatLit 0), d1 <+> ">= 0")
                , (i, ND.NatGe b (NS.NatLit 0), d2 <+> ">= 0")
                ]
              | (i, (t1, t2), ab@(a, b)) <- reverse natEquations
              , not (trivial ab)
              , let (d1, d2) = prettyTypeUPair (apply g3 t1) (apply g3 t2)
              ]
              <> [ (j, ND.NatGe e (NS.NatLit 0), prettyTypeU pos <+> ">= 0")
                 | AnnoS (Idx j t) _ _ <- annoNodes e3
                 , pos <- natPositions (apply g3 t)
                 , Just e <- [typeUToNatExpr pos]
                 , not (NS.isGround e)
                 ]
          -- Each condition once, at its first term.
          firstOfEach cs = go Set.empty cs
            where
              go _ [] = []
              go seen (x@(_, c, _) : rest)
                | Set.member c seen = go seen rest
                | otherwise = x : go (Set.insert c seen) rest
      forM_ (conditionGroups conditions) $ \grp ->
        case ND.decide Set.empty [c | (_, c, _) <- grp] of
          ND.Holds _ -> return ()
          answer -> case grp of
            [] -> return ()
            ((i, _, _) : _) ->
              MM.throwSourcedError i $ vsep
                [ case answer of
                    ND.Refuted _ -> "These kind-level conditions have no solution:"
                    _ -> "The compiler cannot decide whether these kind-level conditions have a solution:"
                , vsep ["  " <> d | (_, _, d) <- grp]
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

      -- A contradicted set constraint is an error, and so is an undecided
      -- one that the signature's assumptions do not subsume. Class-form
      -- constraints still in gamma are dropped by the filter below.
      case dischargeConstraints scope g3 of
        Left err -> MM.throwSystemError ("Constraint violation: " <> err)
        Right g3' ->
          case filter isPrimitiveConstraint (gammaConstraints g3') of
            [] -> return ()
            cs ->
              let displayed = map (prettyConstraint . apply g3') cs
                  fnName = case e0 of
                    AnnoS _ _ (VarS (EV n) _) -> Just n
                    _ -> Nothing
                  prefix = case fnName of
                    Just n -> "Unsolved constraint(s) on " <> dquotes (pretty n) <> "; add to its signature: "
                    Nothing -> "Unsolved constraint(s); add to the function signature: "
              in MM.throwSystemError $
                prefix <> hcat (punctuate ", " displayed)

      -- Substitution can leave nested AppU (@AppU (AppU H [n]) [a]@);
      -- codegen reads only the flat shape.
      let finalE = mapAnnoSG (fmap normalizeType) (applyGen g3 e3)
      _ <- mapAnnoSGM checkFinalType finalE
      rejectInstanceTypedRecords openRecords finalE
      return finalE

    -- Errors visible only once every kind variable is solved: a part of a
    -- Nat may be negative (KIND-3), so a whole one can come out negative.
    checkFinalType :: Indexed TypeU -> MorlocMonad (Indexed TypeU)
    checkFinalType ix@(Idx i t) = case undefinedNats t of
      (e : _) ->
        MM.throwSourcedError i $
          "The Nat" <+> squotes (prettyTypeU e) <+> "divides by a number that is not positive"
      [] -> checkFinalType' ix
    -- A divisor that reduces to a number that is not positive.
    undefinedNats t =
      [ d
      | p <- natPositions t
      , d@(NatDivU _ b) <- universeU p
      , Just e <- [typeUToNatExpr b]
      , Just k <- [NS.evalNat Map.empty e]
      , k <= 0
      ]
    checkFinalType' :: Indexed TypeU -> MorlocMonad (Indexed TypeU)
    checkFinalType' ix@(Idx i t) = case negativeNats t of
      (n : _) ->
        MM.throwSourcedError i $
          "A dimension may not be negative, but this one is " <> pretty n <> "."
      [] -> case collidingUnions t of
        (ks : _) ->
          MM.throwSourcedError i $
            "Columns cannot be combined because both sides have: "
              <> hcat (punctuate ", " (map pretty ks))
        [] -> case missingFields t of
          ((k, ks) : _) ->
            MM.throwSourcedError i $
              "There is no field" <+> squotes (pretty k) <+> "among"
                <+> hsep (punctuate "," (map (squotes . pretty) ks))
          [] -> return ix

    -- A field projected from a known row that lacks it.
    missingFields :: TypeU -> [(Text, [Text])]
    missingFields t0 =
      [ (k, ks)
      | ProjectFieldU r (StrLitU k) <- universeU t0
      , Just ks <- [groundRowKeys r]
      , k `notElem` ks
      ]

    universeU :: TypeU -> [TypeU]
    universeU t0 = t0 : concatMap universeU (typeUChildren t0)

    -- A union of two known rows that share a key; 'reduceRecUnion' leaves
    -- it symbolic, so nothing else reports it.
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
      -- A record inside a tuple, list or polymorphic argument can reach
      -- here in source order; every later NamS must be in declared order.
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
    f (LabelS k e') = LabelS k (resolveTypes e')
    f (IfS c t' e') = IfS (resolveTypes c) (resolveTypes t') (resolveTypes e')
    f (IntrinsicS intr es) = IntrinsicS intr (map resolveTypes es)

resolveInstances ::
  Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (Gamma, AnnoS (Indexed TypeU) Many Int)
resolveInstances g (AnnoS gi@(Idx _ gt) ci e0) = do
  gscope <- MM.getGeneralScope
  (g', e1) <- f gscope g e0
  return (g', AnnoS gi ci e1)
  where
    f ::
      Scope ->
      Gamma ->
      ExprS (Indexed TypeU) ManyPoly Int ->
      MorlocMonad (Gamma, ExprS (Indexed TypeU) Many Int)

    f scope g0 (VarS v (PolymorphicExpr clsName _ _ rss)) = do
      let gtEval = case TE.evaluateType scope gt of
            Right et -> et
            Left _ -> gt
          -- Open: the occurrence type may still hold an unsolved
          -- existential, which an empty gamma cannot instantiate.
          isCompatible t = isSubtypeOfOpen scope t gtEval
                        || isJust (tryCoerce scope t gtEval emptyGamma)
          rssCompat = [x | x@(EType t _ _ _, _) <- rss, isCompatible t]
          -- Sibling aliases do not share instances: @instance Qux A@ does
          -- not match @A'@ though both evaluate to the same type.
          rssMatching = filterByAliasChain scope gt rssCompat
          -- An instance that gives no implementation of the method leaves
          -- it to the most specific more general instance that does: answer
          -- (b) of the open CLS-12.
          implementedMoreGeneral h =
            let general = [et | (t, rs) <- rssMatching, not (null rs), let et = etype t, generalizes True et h]
             in mostSpecific general
          hasFallback h = not (null (implementedMoreGeneral h))
          rssSubtypes = [x | x@(t, rs) <- rssMatching, not (null rs && hasFallback (etype t))]


      -- Every most specific candidate is kept; Morloc.CodeGenerator.Realize
      -- (collapseExpr) picks one by the alias chain of the use's type.
      (g2, es1) <- case mostSpecific [t | (EType t _ _ _, _) <- rssSubtypes] of
        [] ->
          throwTypeError ci $
            "No instance found for" <+> pretty clsName
              <> "::"
              <> pretty v
              <+> "at type" <+> prettyTypeU gtEval
              <> noInstanceAdvice scope clsName v gtEval
        manyTypes -> do
          -- Without deduplication, N aliases branch exponentially.
          let eval t = case TE.evaluateType scope t of
                Right et -> et
                Left _ -> t
              deduped = nubBy (\t1 t2 -> eval t1 == eval t2) manyTypes
              chosen t = any (\d -> eval (etype t) == eval d) deduped
              -- A representation cast agrees with every instance of its
              -- newtype, so a more specific instance does not shadow it
              -- (NEWT-7).
              es0 = concat [rs | (t, rs) <- rssSubtypes, chosen t]
                <> concat [filter isRepresentationCast rs | (t, rs) <- rssSubtypes, not (chosen t)]
              -- A cast does not count as a group of its own.
              explicitGroups = nubBy (\t1 t2 -> eval t1 == eval t2)
                [etype t | (t, rs) <- rssSubtypes, chosen t, not (all isRepresentationCast rs)]
              singleGroup = length explicitGroups <= 1
          g1 <- connectInstance singleGroup g0 es0
          return (g1, es0)

      (g3, es2) <- statefulMapM resolveInstances g2 es1

      return (g3, VarS v (Many es2))
    f _ g0 (VarS v (MonomorphicExpr _ xs)) = statefulMapM resolveInstances g0 xs |>> second (VarS v . Many)
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

    f _ g0 (LetBndS v) = return (g0, LetBndS v)
    f _ g0 (LetS v e1 e2) = do
      (g1, e1') <- resolveInstances g0 e1
      (g2, e2') <- resolveInstances g1 e2
      return (g2, LetS v e1' e2')
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
    f scope g0 (ExeS x@(PatCall p))
      | Just (what, pos, methods) <- bracketRule p
      , FunU args _ <- gt
      , length args > pos = do
          checkBracketReceiver scope ci what methods (args !! pos)
          return (g0, ExeS x)
    f _ g0 (ExeS x) = return (g0, ExeS x)
    f _ g0 (DoBlockS e) = resolveInstances g0 e |>> second DoBlockS
    f _ g0 (EvalS e) = resolveInstances g0 e |>> second EvalS
    f _ g0 (CoerceS c e) = resolveInstances g0 e |>> second (CoerceS c)
    f _ g0 (LabelS k e) = resolveInstances g0 e |>> second (LabelS k)
    f _ g0 (IfS c t e) = do
      (g1, c') <- resolveInstances g0 c
      (g2, t') <- resolveInstances g1 t
      (g3, e') <- resolveInstances g2 e
      return (g3, IfS c' t' e')
    f _ g0 (IntrinsicS intr es) = do
      (g1, es') <- statefulMapM resolveInstances g0 es
      return (g1, IntrinsicS intr es')

    -- Solutions propagate only when every candidate is in one
    -- alias-equivalent group; otherwise solving to one would exclude the
    -- others (e.g. Integral Int and Integral Real).
    connectInstance :: Bool -> Gamma -> [AnnoS (Indexed TypeU) f c] -> MorlocMonad Gamma
    connectInstance _ g0 [] = return g0
    connectInstance singleGroup g0 (AnnoS (Idx _ t) _ _ : es) = do
      scope <- MM.getGeneralScope
      case subtype scope (stripCoercionWrappers gt) t g0 of
        (Left _) -> connectInstance singleGroup g0 es
        (Right g1)
          | singleGroup -> connectInstance singleGroup g1 es
          | otherwise   -> connectInstance singleGroup g0 es

-- | PAT-5: a bracket step's receiver is a list or an IFile, which every
-- back end brackets natively, or reaches some instance of the step's
-- classes. A receiver nothing has determined yet is left to code
-- generation (POLY-6).
checkBracketReceiver :: Scope -> Int -> MDoc -> [(EVar, Int)] -> TypeU -> MorlocMonad ()
checkBracketReceiver scope i what methods rcv = do
  heads <- concat <$> mapM (uncurry instanceReceiverHeads) methods
  when (classifyBracketReceiver scope heads rcv == ReceiverNone) $
    throwTypeError i $
      "A bracket step needs a receiver with" <+> what <+> "instance, but"
        <+> prettyTypeU rcv <+> "has none"

-- | A derived @pack@ or @unpack@: a lambda over a representation cast,
-- which no program can write. Instance bodies reach resolution typechecked,
-- so the cast is already a coercion.
isRepresentationCast :: AnnoS g f c -> Bool
isRepresentationCast (AnnoS _ _ (LamS _ (AnnoS _ _ (CoerceS (CoerceNewtype _ _) _)))) = True
isRepresentationCast (AnnoS _ _ (LamS _ (AnnoS _ _ (IntrinsicS intr _)))) =
  intr `elem` [IntrNewtypeUnwrap, IntrNewtypeWrap]
isRepresentationCast _ = False

-- | The instances reachable by walking @gt@'s alias chain. Only @gt@'s
-- chain is walked: an instance head is never a transparent alias
-- (Restructure.checkInstanceOnRoot), and
-- walking it would merge sibling newtypes (Deque a, Array a) that share
-- the body @List a@ but own separate instances.
--
-- Each step removes a non-recursive alias level, pushes a recursive
-- alias a level deeper than any candidate head reaches, or strips a
-- wrapper, which bounds the fuel.
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

-- | Whether an instance type matches a call-site type at one alias level,
-- without evaluating aliases. Bound variables and existentials match
-- anything. Any two Nat expressions match: 'subtype' runs the Nat solver
-- later, and only it can equate @Vector (d1*d2) a@ with @Vector 4 a@.
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
    -- A record evaluated to its body still carries its name and parameters,
    -- and an instance head names it nominally.
    go b1 b2 t1@NamU{} t2@(VarU _) = go b1 b2 (nominal t1) t2
    go b1 b2 t1@NamU{} t2@(AppU _ _) = go b1 b2 (nominal t1) t2
    go b1 b2 t1@(VarU _) t2@NamU{} = go b1 b2 t1 (nominal t2)
    go b1 b2 t1@(AppU _ _) t2@NamU{} = go b1 b2 t1 (nominal t2)
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
      -- Partial application, as in 'subtype': @Vector n a@ matches @f a@.
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
    go _  _  t1 t2 | isNatExprT t1 && isNatExprT t2 = True
    go _  _  _ _ = False

    nominal (NamU _ n [] _) = VarU n
    nominal (NamU _ n ps _) = AppU (VarU n) ps
    nominal t = t

    isNatExprT t = maybe False fillsNat (kindOfU t)

-- | Result type of slicing a container. A leading Nat parameter becomes
-- 'NatVoidU', which matches any Nat. A fresh existential would never be
-- solved and would reach later stages as a quantified variable.
bracketSliceResultType :: Gamma -> TypeU -> (Gamma, TypeU)
bracketSliceResultType g t@(AppU h args) = case args of
  (a : rest) | maybe False fillsNat (kindOfU a) ->
    (g, AppU h (NatVoidU : rest))
  _ -> (g, t)
bracketSliceResultType g t = (g, t)


-- | The advice for a method with no instance at a type. A signature can only
-- help while the type is still undetermined; a bodied newtype that lacks
-- 'pack' is missing its deriving clause (NEWT-8).
noInstanceAdvice :: Scope -> ClassName -> EVar -> TypeU -> MDoc
noInstanceAdvice scope cls v t
  | not (determined t) = "\n  Are you missing a top-level type signature?"
  | cls == ClassName "Packable", v == EV "pack", FunU _ out <- t, Just n <- bodiedNewtype out =
      "\n  The newtype" <+> squotes (pretty n) <+> "has no 'pack'; add 'deriving (Packable)' to its declaration"
        <+> "or declare a Packable instance."
  | otherwise = mempty
  where
    determined (VarU (TV n)) = not (isTypeVariableName n)
    determined ExistU {} = False
    determined (ForallU _ _) = False
    determined x = all determined (typeUChildren x)

    bodiedNewtype x = case x of
      VarU n -> newtypeName n
      AppU (VarU n) _ -> newtypeName n
      _ -> Nothing

    newtypeName n = case Map.lookup n scope of
      Just ((_, NamU {}, _, _, _) : _) -> Nothing
      Just ((_, _, _, _, TypedefNewtype) : _) -> Just n
      _ -> Nothing

throwTypeError :: Int -> MDoc -> MorlocMonad a
throwTypeError i msg = MM.throwSourcedError i ("General type error:" <+> msg)

-- A selector targets tuples and records only, but its type also unifies
-- with @List a@; such an application must be rejected here.
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
    -- The evaluated form, so an alias cannot hide a `data` type.
    _ -> rejectSumSelectorTarget i s t'

-- | Refuse a getter aimed at a `data` type: which fields exist depends on
-- the constructor, which only a match fixes.
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
  -- Errors point at j, the body; i is often the reference or export site.
  (g', t', e') <- case annotation of
    Nothing -> checkE' j g e t
    (Just annType) -> do
      gAnn <- subtype' j annType t g
      checkE' j gAnn e t
  let annotatedBody = AnnoS (Idx i t') j e'
  -- EFF-10
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
  -- Errors point at ci, the body; gi is often the reference or export site.
  (g', t, e') <- case Map.lookup ci annotation of
    Nothing -> synthE' ci g e
    (Just annType) -> checkE' ci g e annType
  let annotatedBody = AnnoS (Idx gi t) ci e'
  -- EFF-10
  case Map.lookup ci annotation of
    Just annType -> checkEffectCoverage ci (apply g' annType) annotatedBody
    Nothing -> return ()
  return (g', t, annotatedBody)

-- | The constructor-pattern lowering always puts a string literal here, so
-- Nothing is a compiler bug, not a user error.
literalStr :: AnnoS Int ManyPoly Int -> Maybe Text
literalStr (AnnoS _ _ (StrS t)) = Just t
literalStr _ = Nothing

unAnnoSE :: AnnoS Int ManyPoly Int -> ExprS Int ManyPoly Int
unAnnoSE (AnnoS _ _ e) = e

-- | The field types of a constructor of the scrutinee's `data` type, with
-- its parameters instantiated. Rejecting a constructor of another type
-- keeps a clause set from mixing two `data` types.
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
-- A constructor name is unique, so it alone determines its type. The
-- type's parameters become fresh existentials shared by the arguments and
-- the result, tying `Some x :: Opt a` to the `a` of `x`.
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

synthE i g (AppS f@(AnnoS _ _ (ExeS (PatCall (PatternText _ _)))) es) = do
  (g1, _, f1) <- synthG g f
  (g2, _, es1, _) <- zipCheck i g1 es (take (length es) (repeat BT.strU))
  return (g2, BT.strU, AppS f1 es1)

synthE _ _ (AppS (AnnoS _ _ (ExeS (PatCall (PatternStruct _)))) []) = error "Unreachable application pattern to no data"
-- A selector with bracket steps, e.g. @.(.0.[i], .1) f@. The arguments
-- are the bracket bounds in DFS order, then the receiver.
synthE i g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall (PatternStruct s)))) args)
  | bracketArity s > 0 = do
      let n = bracketArity s
      when (length args /= n + 1) $
        MM.throwSourcedError i $
          "PatternStruct with bracket-containing selector expects"
          <+> pretty (n + 1) <+> "args, got" <+> pretty (length args)
      let bracketArgs = take n args
          rcvArg      = last args
      -- Bounds are not pinned to Int: codegen inserts the per-language
      -- __to_index__ cast.
      let nameForArgIx j = MT.pack ("_pat_bracket_arg_" <> show j <> "_")
          (gArgs, bracketArgTypes) =
            foldl (\(gAcc, ts) j ->
                     let (gNext, ty) = newvar (nameForArgIx j) gAcc
                     in (gNext, ts ++ [ty]))
                  (g0, [])
                  ([0 .. n - 1] :: [Int])
      (gsyn, rcvSyn, rcvExpr) <- synthG gArgs rcvArg
      let rcvResolved = apply gsyn rcvSyn
      (gReady, expectedInner, isIFile) <- case (headTVar rcvResolved, rcvResolved) of
        (Just v, AppU _ (innerType : _))
          | v == BT.ifileVar -> return (gsyn, innerType, True)
        _                    -> return (gsyn, rcvResolved, False)
      -- selectorGetter reads the structural existential, not the receiver.
      (gSel, datType) <- selectorType gReady s
      gUnified <- subtype' fgidx datType expectedInner gSel
      rejectListSelectorTarget fgidx s (apply gUnified datType)
      let retType = case selectorGetter datType s of
            []  -> error "Illegal empty selection"
            [t] -> t
            ts  -> BT.tupleU ts
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
  -- On an @IFile a@ receiver the selector reads @a@. The function's input
  -- stays @IFile a@, which codegen (Express.hs) detects.
  (gsyn, rcvSyn, e0Syn) <- synthG g0 e0
  let rcvResolved = apply gsyn rcvSyn
  case (headTVar rcvResolved, rcvResolved) of
    (Just v, AppU _ (innerType : _)) | v == BT.ifileVar -> do
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

-- Setter. The receiver is checked before the new values, so that
-- @.count = 42@ can fill an @Int32@ field rather than synthesize @Int@.
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
      -- Built from the receiver, not the selector's structural type, to
      -- keep unnamed fields and the receiver's key order (the wire layout).
      resultType =
        fromMaybe (apply g4 outputType) (selectorSetter setTypes s receiverType)
      patternType = FunU (receiverType : setTypes) resultType
      f1 = AnnoS (Idx fgidx patternType) fcidx (ExeS (PatCall (PatternStruct s)))
  return (g4, resultType, AppS f1 (e1 : es1))
synthE _ g (ExeS (PatCall (PatternText s ss@(length -> n)))) = do
  let t = FunU (take n (repeat BT.strU)) BT.strU
  return (g, t, ExeS (PatCall (PatternText s ss)))

-- xs[i]. The index type stays open; codegen supplies the __to_index__
-- cast.
synthE _ g0 (AppS (AnnoS fgidx fcidx (ExeS (PatCall PatternBracketIndex))) [iExpr, rcvExpr]) = do
  -- An IFile receiver is indexed through; the function's input keeps the
  -- IFile so codegen detects it.
  (gsyn, rcvSyn, rcvSynExpr) <- synthG g0 rcvExpr
  let rcvResolved = apply gsyn rcvSyn
  case (headTVar rcvResolved, rcvResolved) of
    (Just v, AppU _ (innerType : _)) | v == BT.ifileVar -> do
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

-- xs[i:j:k]. Each bound gets its own bare existential; whether a bound is
-- absent is carried by NullS, not by the type. The result type is from
-- 'bracketSliceResultType'.
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
  -- Slicing an @IFile a@ gives @a@; codegen rejects a non-list @a@.
  let resolvedRcv = apply g7 rcvType'
      isIFile = headTVar resolvedRcv == Just BT.ifileVar
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

-- Bracket patterns used as values, not applied.
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
  (g1, funType0, funExpr0) <- synthG g0 f

  -- Literal arguments solve the variables their labels bind (m@Int).
  let g1' = resolveNatLabels f funType0 xs0 g1

  etaExpandSynthE i g1' funType0 funExpr0 f xs0

-- -->I==>
synthE _ g0 (LamS vs x) = do
  let (g1, paramTypes) = statefulMap (\g' v -> newvar (unEVar v <> "_x") g') g0 vs
      g2 = g1 ++> zipWith AnnG vs paramTypes

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
  (g1, itemType, itemExpr) <- synthElem g e

  (g2, tupleType, tupleExpr) <- synthE' i g1 (TupS es)

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

-- A signed term referenced by its signature alone is instantiated, its
-- constraints becoming obligations. A reference's type never keeps its
-- quantifier: one left in place would meet an expected type inside a
-- record or tuple as a rigid variable.
synthE _ g0 (VarS v (MonomorphicExpr (Just t0) []))
  | Just _ <- gammaAssumedConstraints g0 = do
      let (g1, t0') = renameEType g0 t0
          (g2, t2) = instantiateObligations g1 (etype t0') (Set.toList (econs t0'))
      return (g2, t2, VarS v (MonomorphicExpr (Just t0) []))
synthE vi g0 (VarS v (MonomorphicExpr (Just t0sig) xs0)) = do
  -- A local signature's type variables named by an enclosing signature are
  -- that signature's variables.
  let enclosing = if isLocalName v then gammaScoped g0 else Map.empty
      t0 = scopeEType (isJust (gammaRigid g0)) enclosing t0sig
  -- The type and its constraints are renamed together. The outermost VarS
  -- is the definition being checked: its constraints become the body's
  -- assumptions. It is the only VarS that finds gammaAssumedConstraints
  -- Nothing; after it the slot is Just, even for an empty list.
  let (g1, t0') = renameEType g0 t0
      -- A local signature writing a Nat or Int variable of an enclosing one
      -- means that variable, as for its type variables.
      enclosingNats = if isLocalName v then gammaScopedNats g0 else Map.empty
      freshNats = Map.fromList (zip (List.nub (collectNatVarNames (etype t0))) (List.nub (collectNatVarNames (etype t0'))))
      scopedNats = Map.fromList [(n', enc) | (n, n') <- Map.toList freshNats, Just enc <- [Map.lookup n enclosingNats]]
      t1 = renameKindedVars scopedNats Map.empty Map.empty Map.empty Map.empty (etype t0')
      newCs = Set.toList (econs t0')
      inScope = Map.union (Map.fromList (zip (fst (unqualify (etype t0))) (fst (unqualify t1)))) enclosing
      natRen = Map.map (\n' -> Map.findWithDefault n' n' scopedNats) freshNats
      inScopeNats = Map.union natRen enclosingNats
      -- A label whose Nat occurs nowhere else in the type still names one.
      labelVars = Map.fromList
        [ (x, Map.findWithDefault (TV (unTVar lv <> "@" <> unEVar v)) lv natRen)
        | AnnoS _ _ (LamS vs _) <- xs0
        , (lv, idx) <- Map.toList (enatLabels t0)
        , not (Set.member lv (strLabelVars (etype t0)))
        , x <- take 1 (drop idx vs)
        ]
  -- Against a signature, the body may also use the superclasses.
  given <- case gammaRigid g1 of
    Just _ -> superclassClosure newCs
    Nothing -> return newCs
  let outermost = isNothing (gammaAssumedConstraints g1)
      contract = isJust (gammaRigid g1)
      sigNats = [n | n <- List.nub (collectNatVarNames t1 <> Map.elems labelVars), n `notElem` Map.elems scopedNats]
      g1Base = g1
        { gammaConstraints = gammaConstraints g1 ++ newCs
        , gammaAssumedConstraints = case gammaAssumedConstraints g1 of
            Nothing -> Just given
            Just _  -> gammaAssumedConstraints g1
        -- Against a signature, its effect variables are rigid too.
        , gammaRigid = case gammaRigid g1 of
            Just rigid | outermost -> Just (Set.union rigid (Set.fromList (typeEffectVars t1)))
            r -> r
        }
      -- Every signature checked as a contract, at any depth, adds its Nat
      -- and Int variables and its domain (POLY-7, KIND-4).
      g1Cs
        | contract = g1Base
            { gammaRigidNats = Set.union (gammaRigidNats g1) (Set.fromList sigNats)
            , gammaRigidDomain = gammaRigidDomain g1 <> [(vi, p) | p <- natPositions t1]
            , gammaLabelVars = Map.union labelVars (gammaLabelVars g1)
            }
        | otherwise = g1Base
      g1' = (g1Cs ++> [AnnG v t1]) {gammaScoped = inScope, gammaScopedNats = inScopeNats}
  (g2scoped, t2, xs1) <- foldCheck g1' xs0 t1
  let g2 = g2scoped {gammaScoped = gammaScoped g0, gammaScopedNats = gammaScopedNats g0}
  -- EFF-10: catches forces such as `f = !rint`, which subtyping cannot
  -- see since EvalS strips the effect. Reported at the body's index ci.
  mapM_ (\x@(AnnoS _ ci _) -> checkEffectCoverage ci (apply g2 t1) x) xs1
  let xs2 = applyCon g2 $ VarS v (MonomorphicExpr (Just t0) xs1)
      -- A local signed definition is checked as a contract with its Nat
      -- and Int variables rigid, but this node is also its use, which
      -- takes those variables afresh (POLY-7); the rest of its type is as
      -- the check left it.
      (g3, tUse)
        | contract && not outermost =
            let (ga, fresh) = statefulMap (\g' (TV s) -> tvarname g' (s <> "@u")) g2 sigNats
             in (ga, renameKindedVars (Map.fromList (zip sigNats fresh)) Map.empty Map.empty Map.empty Map.empty t2)
        | otherwise = (g2, t2)
  return (g3, tUse, xs2)
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
-- Against a signature, a method use only records its constraint as an
-- obligation; the instance is not chosen here.
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
    -- Each instance is checked independently; the context is trimmed
    -- between instances to keep it from growing quadratically.
    checkInstances ::
      Gamma ->
      TypeU ->
      [(EType, [AnnoS Int ManyPoly Int])] ->
      MorlocMonad (Gamma, [(EType, [AnnoS (Indexed TypeU) ManyPoly Int])])
    checkInstances g _ [] = return (g, [])
    checkInstances g10 genType ((instType, es) : rs) = do
      let (g11, genType') = toExistential g10 genType
      let (g12, instType') = rename g11 (etype instType)
      g13 <- subtype' i instType' genType' g12
      -- The ExistG entries made so far must survive the trim:
      -- connectInstance solves through them.
      let slotAfterSubtype = gammaSlot g13
      (g14, es') <- checkImplementations g13 genType' es

      let gNext = gammaTrimAfter slotAfterSubtype g14

      -- The original general type: each instance gets its own existentials.
      (g15, rs') <- checkInstances gNext genType rs

      return (g15, (instType, es') : rs')

    checkImplementations ::
      Gamma ->
      TypeU ->
      [AnnoS Int ManyPoly Int] ->
      MorlocMonad (Gamma, [AnnoS (Indexed TypeU) ManyPoly Int])
    checkImplementations g _ [] = return (g, [])
    checkImplementations g10 t (e@(AnnoS implGi _ _) : es) = do
      -- An annotation copied from the use site (`mempty :: Str`) must not
      -- constrain each implementation (the List instance's `[]`).
      implAnn <- MM.gets (Map.lookup implGi . stateAnnotations)
      MM.modify' (\s -> s { stateAnnotations = Map.delete implGi (stateAnnotations s) })
      (g11, _, e') <- checkG g10 e t
      case implAnn of
        Just ann' -> MM.modify' (\s -> s { stateAnnotations = Map.insert implGi ann' (stateAnnotations s) })
        Nothing -> return ()

      (g12, es') <- checkImplementations g11 t es

      return (g12, applyGen g12 e' : es')

-- A selector used as a value, not applied.
synthE _ g0 (ExeS (PatCall (PatternStruct s))) = do
  (g1, datType) <- selectorType g0 s
  retType <- return $ case selectorGetter datType s of
    [] -> error "Illegal empty selection"
    [t] -> t
    ts -> BT.tupleU ts
  let ft = FunU [datType] retType
  return (g1, ft, ExeS (PatCall (PatternStruct s)))
-- The existential is subtyped against the type the enclosing VarS knows.
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
      -- Known constants, for Nat label resolution.
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
  -- Two suspensions join at the union of their rows: <> T beside <e> T
  -- is <e> T with e free (EFF-8). A value never silently becomes a
  -- suspension, so a value branch beside a suspension branch is an error.
  case joinSuspensions scope g4 t2' t3' of
    Just (g5, tj) -> return (g5, tj, IfS cond' thenE' elseE')
    Nothing -> case subtype scope t3' t2' g4 of
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
  -- forcedView strips foralls, so that @forall a. <Random> a@ is seen as
  -- effectful rather than nested in another effect.
  (g1', t1') <- forcedView g1 t1
  case t1' of
    EffectU _ iT -> do
      -- An effectful final expression is forced like any other statement.
      e1' <- wrapFinalEvalS i iT e1
      let collected = collectDoEffects e1'
      return (g1', EffectU collected iT, DoBlockS e1')
    bareT -> do
      -- Even with nothing forced, the block is a suspension (<> bareT).
      let collected = collectDoEffects e1
      return (g1', EffectU collected bareT, DoBlockS e1)
synthE _ g (CoerceS coercion e) = do
  (g1, t1, e1) <- synthG g e
  return (g1, apply g1 (applyCoercion coercion t1), CoerceS coercion e1)
synthE _ g (LabelS k e) = do
  (g1, t1, e1) <- synthG g e
  return (g1, t1, LabelS k e1)
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
-- IntrMap, the implicit map of bracket-accessor chains:
-- @(a -> b) -> f a -> f b@. The receiver is checked before the function:
-- with @a@ still open, a selector in the function would make structural
-- existentials that never bind @b@.
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
-- IntrEq / IntrLe: two values of one type, compared on the wire (INTR-7).
synthE _ g (IntrinsicS intr [aE, bE])
  | intr `elem` [IntrEq, IntrLe] = do
      (g1, ta, aE') <- synthG g aE
      (g2, _, bE') <- checkG g1 bE (apply g1 ta)
      return (g2, BT.boolU, IntrinsicS intr [aE', bE'])
-- IntrNewtypeUnwrap: the representation cast of a derived @unpack@
-- (NEWT-7). The argument's type is a newtype; the result is its
-- representation at the newtype's arguments.
synthE i g (IntrinsicS IntrNewtypeUnwrap [xE]) = do
  (g1, tx, xE') <- synthG g xE
  scope <- MM.getGeneralScope
  let tx' = apply g1 tx
  case TE.expandWireParent scope tx' of
    Just body -> return (g1, body, CoerceS (CoerceNewtype Unwrap body) xE')
    Nothing ->
      MM.throwSourcedError i $
        "Cannot unwrap" <+> squotes (pretty tx') <> ": it is not a newtype with a representation."
synthE _ _ (IntrinsicS IntrNewtypeUnwrap args) =
  error $ "IntrNewtypeUnwrap expects one argument, got " <> show (length args)
synthE i _ (IntrinsicS IntrNewtypeWrap _) =
  MM.throwSourcedError i "A newtype wrap needs the newtype it builds from its context."
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
-- IntrListLenTest and IntrListElem: the list-pattern length test and element
-- read. The subject is checked against @List a@ itself, not a fresh
-- container, so a list pattern rejects every other container.
synthE _ g (IntrinsicS IntrListLenTest [subjectE, lenE])
  | IntS _ _ <- unAnnoSE lenE = do
      let (g1, a) = newvar "listpat_elem_" g
      (g2, _, subjectE') <- checkG g1 subjectE (BT.listU a)
      (g3, _, lenE') <- synthG g2 lenE
      return (g3, BT.boolU, IntrinsicS IntrListLenTest [subjectE', lenE'])
synthE _ _ (IntrinsicS IntrListLenTest args) =
  error $ "IntrListLenTest expects (subject, literal length), got " <> show (length args)
synthE _ g (IntrinsicS IntrListElem [subjectE, idxE])
  | IntS _ _ <- unAnnoSE idxE = do
      let (g1, a) = newvar "listpat_elem_" g
      (g2, _, subjectE') <- checkG g1 subjectE (BT.listU a)
      (g3, _, idxE') <- synthG g2 idxE
      return (g3, apply g3 a, IntrinsicS IntrListElem [subjectE', idxE'])
synthE _ _ (IntrinsicS IntrListElem args) =
  error $ "IntrListElem expects (subject, literal index), got " <> show (length args)
-- IntrWrite: @Int -> OStream a -> [a] -> <IO> Try Str ()@. The handle is
-- checked before the list, so that in @write 0 (o :: OStream UInt64) [1]@
-- the literal can be a UInt64 rather than synthesize Int.
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
-- The fold accumulator. `Cell b` carries the accumulated type, forcing a
-- folding handler's init, step and combine to agree; an untyped handle
-- would be unsound.
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
-- @replay :: IStream a -> ([a] -> <IO> ()) -> <IO> (); each frame is a list.
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
-- @spawn :: IStream a -> (OStream a -> <IO> ()) -> <IO> ()
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
-- @try: failures have no common type, so the caught error is always a
-- rendered message, @Try Str a@. The body's effects pass through.
synthE _ g (IntrinsicS IntrTry [bodyE]) = do
  (g0, bodyT, bodyE') <- synthG g bodyE
  let (g1, openT) = stripForallU g0 (apply g0 bodyT)
      resultT = case openT of
        EffectU e t -> mkEffectU e (BT.tryU BT.strU t)
        t           -> BT.tryU BT.strU t
  return (g1, resultT, IntrinsicS IntrTry [bodyE'])
synthE i _ (IntrinsicS IntrTry args) =
  MM.throwCompilerBugAt i $ "IntrTry expects 1 arg (body), got " <> pretty (length args)
-- @stream :: IFile [a] -> <IO> IStream a. The receiver is checked, not
-- read syntactically, so that aliases and inferred receivers work.
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
synthE i _ (IntrinsicS IntrDatafile [pathE])
  | not (isStaticStr pathE) =
      MM.throwSourcedError i
        "the argument of @datafile must be a string literal, since the path is resolved when the program is built"
synthE i g (IntrinsicS intr args) = do
  (g', argTypes, args') <- synthArgs g args
  g'' <- checkIntrinsicArgs i g' intr argTypes
  let (g''', expectedType) = intrinsicTypeG g'' intr argTypes
  return (g''', expectedType, IntrinsicS intr args')

-- | A string literal, possibly reached through a name bound to one.
isStaticStr :: AnnoS g ManyPoly c -> Bool
isStaticStr (AnnoS _ _ (StrS _)) = True
isStaticStr (AnnoS _ _ (VarS _ (MonomorphicExpr _ [e]))) = isStaticStr e
isStaticStr _ = False

-- | Strip ForallU wrappers by instantiating bound variables as existentials.
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

-- | Peel ForallU layers without instantiating, where no gamma is at hand.
peelForallU :: TypeU -> TypeU
peelForallU (ForallU _ t) = peelForallU t
peelForallU t = t

-- | Return type of a fully applied intrinsic, threading Gamma for fresh
-- existentials.
intrinsicTypeG :: Gamma -> Intrinsic -> [TypeU] -> (Gamma, TypeU)
-- @load :: Str -> <IO> (Try Str a)
intrinsicTypeG g IntrLoad _ =
  let (g', loadType) = newvar "load_" g
  in (g', EffectU ioEffectSet (BT.tryU BT.strU loadType))
-- @read :: Str -> Try Str a. A JSON parse touches nothing, so with
-- failure carried in the result it is pure and composes in `map`.
intrinsicTypeG g IntrRead _ =
  let (g', readType) = newvar "read_" g
  in (g', BT.tryU BT.strU readType)
-- @open: the return type comes from an ascription
-- (`@open path :: IFile Sequence`).
intrinsicTypeG g IntrOpen _ =
  let (g', openType) = newvar "open_" g
  in (g', EffectU ioEffectSet (BT.tryU BT.strU openType))
-- @unpack: the decoded type is resolved by the synthesized ascription.
intrinsicTypeG g IntrUnpack _ =
  let (g', unpackType) = newvar "unpack_" g
  in (g', EffectU ioEffectSet unpackType)
intrinsicTypeG g IntrClose _ = (g, EffectU ioEffectSet BT.unitU)
-- @append: the return type comes from an ascription, as for @open.
intrinsicTypeG g IntrAppend _ =
  let (g', appendType) = newvar "append_" g
  in (g', EffectU ioEffectSet (BT.tryU BT.strU appendType))
-- @stdin can fail (a second open); @stdout and @stderr cannot.
intrinsicTypeG g IntrStdin _ =
  let (g', a) = newvar "stdin_a" g in
  (g', EffectU ioEffectSet (BT.tryU BT.strU (AppU (VarU BT.istreamVar) [a])))
intrinsicTypeG g IntrStdout _ =
  let (g', a) = newvar "stdout_a" g in
  (g', EffectU ioEffectSet (AppU (VarU BT.ostreamVar) [a]))
intrinsicTypeG g IntrStderr _ =
  let (g', a) = newvar "stderr_a" g in
  (g', EffectU ioEffectSet (AppU (VarU BT.ostreamVar) [a]))
-- @throw :: Str -> a. A bottom, not a tracked effect.
intrinsicTypeG g IntrThrow _ =
  let (g', a) = newvar "throw_" g
  in (g', a)
intrinsicTypeG g intr _ = (g, intrinsicType intr)

-- | Check a @Cell b@ handle and return @b@. Called before a cell
-- intrinsic's other arguments are checked, as for IntrWrite.
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
intrinsicType IntrSaveJ = EffectU ioEffectSet (BT.tryU BT.strU BT.unitU)
intrinsicType IntrLoad = EffectU ioEffectSet (BT.tryU BT.strU (ExistU (TV "load_a") ([], Open) ([], Open)))
intrinsicType IntrHash = BT.strU
intrinsicType IntrEq = error "intrinsicType: IntrEq must be typed via synthE's dedicated clause"
intrinsicType IntrLe = error "intrinsicType: IntrLe must be typed via synthE's dedicated clause"
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
intrinsicType IntrMap =
  error "intrinsicType: IntrMap must be typed via synthE's dedicated clause"
intrinsicType IntrMapOptional =
  error "intrinsicType: IntrMapOptional is emitted after typechecking"
intrinsicType IntrTagTest =
  error "intrinsicType: IntrTagTest must be typed via synthE's dedicated clause"
intrinsicType IntrNewtypeUnwrap =
  error "intrinsicType: IntrNewtypeUnwrap must be typed via synthE's dedicated clause"
intrinsicType IntrNewtypeWrap =
  error "intrinsicType: IntrNewtypeWrap must be typed via checkE's dedicated clause"
intrinsicType IntrCtorField =
  error "intrinsicType: IntrCtorField must be typed via synthE's dedicated clause"
intrinsicType IntrListLenTest =
  error "intrinsicType: IntrListLenTest must be typed via synthE's dedicated clause"
intrinsicType IntrListElem =
  error "intrinsicType: IntrListElem must be typed via synthE's dedicated clause"
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
intrinsicType IntrIFileWalk =
  error "intrinsicType: IntrIFileWalk is synthesized post-typecheck and carries its result type from the originating pattern application"
intrinsicType IntrConform =
  error "intrinsicType: IntrConform is synthesized post-typecheck and carries the type of the literal it builds"
intrinsicType IntrCellSlots =
  error "intrinsicType: IntrCellSlots is synthesized post-typecheck"
intrinsicType IntrReduceList =
  error "intrinsicType: IntrReduceList is synthesized post-typecheck"

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
      case (intr, argTypes) of
        (IntrSave, [levelT, pathT, _]) -> do
          g' <- subtype' i levelT BT.intU g
          subtype' i pathT BT.strU g'
        (IntrSaveJ, [pathT, _]) -> subtype' i pathT BT.strU g
        (IntrLoad, [pathT]) -> subtype' i pathT BT.strU g
        (IntrHash, [_]) -> return g
        (IntrSchema, [_]) -> return g
        (IntrTypeof, [_]) -> return g
        (IntrShow, [_]) -> return g
        (IntrRead, [strT]) -> subtype' i strT BT.strU g
        (IntrDatafile, [pathT]) -> subtype' i pathT BT.strU g
        (IntrUnpack, [bytesT]) -> subtype' i bytesT (BT.listU BT.u8U) g
        (IntrOpen, [pathT]) -> subtype' i pathT BT.strU g
        (IntrClose, [_]) -> return g
        (IntrFSchema, [pathT]) -> subtype' i pathT BT.strU g
        -- @flen accepts any handle here; the runtime checks for an IFile.
        (IntrFLength, [_]) -> return g
        (IntrStreamLayout, [argT]) ->
          let (g'a, a) = newvar "streamlayout_a_" g
              expectedT = AppU (VarU BT.ifileVar) [BT.listU a]
           in subtype' i argT expectedT g'a
        (IntrNext, _) -> MM.throwCompilerBugAt i "IntrNext is typed in synthE"
        (IntrReplay, _) -> MM.throwCompilerBugAt i "IntrReplay is typed in synthE"
        (IntrStream, _) -> MM.throwCompilerBugAt i "IntrStream is typed in synthE"
        (IntrAppend, [pathT]) -> subtype' i pathT BT.strU g
        (IntrConcat, [pathsT, destT]) -> do
          g1 <- subtype' i pathsT (AppU (VarU BT.list) [BT.strU]) g
          subtype' i destT BT.strU g1
        (IntrFlush, [handleT]) ->
          let (g'a, a) = newvar "flush_a_" g
              expectedT = AppU (VarU BT.ostreamVar) [a]
           in subtype' i handleT expectedT g'a
        -- The runtime reads a throw's payload as UTF-8 bytes; any other
        -- type would be lost as an empty message.
        (IntrThrow, [msgT]) -> subtype' i msgT BT.strU g
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

  case normalType of
    FunU (length -> numParams) _
      | numArgs > numParams ->
          throwTypeError i $ "Invalid function application of type:\n  " <> prettyTypeU funType0
    _ -> return ()

  (g2raw, funType1, inputExprs) <- application' i g1 xs0 normalType

  -- Resolve this application's literals now: an unsolved result could
  -- otherwise be instantiated to a wrapped type like @<IO> Int@ by an
  -- enclosing context. Only this application's own literals: an
  -- enclosing application's may still be fixed by its later arguments.
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
      -- partial application
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

        newVarExprs <-
          mapM
            ( \(v, tp) -> do
                idx <- MM.getCounterWithPos i
                return $ AnnoS (Idx idx tp) idx (BndS v)
            )
            newVarsWithTypes

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

-- Check the arguments of an application against its parameter types
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
zipCheck i g0 (x0 : xs0) (t0 : ts0) = do
  (g1, t1, x1) <- checkG g0 x0 t0
  (g2, ts1, xs1, remainder) <- zipCheck i g1 xs0 ts0
  return (g2, t1 : ts1, x1 : xs1, remainder)
-- partial application
zipCheck _ g0 [] ts = return (g0, [], [], ts)
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
-- See 'reduceSetterRedex'.
checkE i g e t | Just e' <- reduceSetterRedex e = checkE i g e' t
-- Only the List constructor itself: a newtype @MyVec a = List a@ goes to
-- the general LstS rule below, which walks its wire parent. An argument
-- that is not an element type (a Nat, say) also goes there.
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
  -- Fewer parameters than arguments. Not eta-expanded: that would move the
  -- body's work from each application of this lambda to each call of the
  -- function it returns.
  | length vs < length as = do
      let (asHere, asRest) = splitAt (length vs) as
          g1 = g0 ++> zipWith AnnG vs asHere
      (g2, t2, e2) <- checkG g1 body (FunU asRest b)
      return (g2, apply g2 (FunU asHere t2), applyCon g2 (LamS vs e2))
  -- More parameters than this arrow group has arguments (@FunU [A] (FunU
  -- [B] C)@): the surplus parameters are checked against the return type,
  -- as 'subtype' does. Each step consumes at least one parameter, so this
  -- terminates; an eta-expansion here would add parameters and never end.
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
-- IntrNewtypeWrap: the representation cast of a derived @pack@ (NEWT-8),
-- from the representation of the newtype it is checked against.
checkE i g (IntrinsicS IntrNewtypeWrap [xE]) t = do
  scope <- MM.getGeneralScope
  let t' = apply g t
  case TE.expandWireParent scope t' of
    Just body -> do
      (g1, _, xE') <- checkG g xE body
      return (g1, apply g1 t', CoerceS (CoerceNewtype Wrap (apply g1 t')) xE')
    Nothing ->
      MM.throwSourcedError i $
        "Cannot wrap a value as" <+> squotes (pretty t') <> ": it is not a newtype with a representation."
checkE i g (IfS cond thenE elseE) t = do
  (g1, condType, cond') <- synthG g cond
  g2 <- subtype' i condType (VarU (TV "Bool")) g1
  (g3, t2, thenE') <- checkG g2 thenE t
  (g4, _, elseE') <- checkG g3 elseE (apply g3 t2)
  return (g4, apply g4 t2, IfS cond' thenE' elseE')
-- A do-block against @<E> (<E'> T)@: a suspension as its final statement
-- is returned rather than run, the only way to build a suspension that
-- yields one. If that reading does not fit, the statement is run, as in
-- 'synthE'. Other do-blocks take the general synth-and-subtype rule.
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
  -- Synthesized first, so no unsolved EffectVar is created.
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
-- A list literal against a type whose wire form is a list (@Vector 4
-- Int32@): checked against the list form, so the element type reaches the
-- literals, then length-checked against the original type's Nat arguments.
checkE i g1 e1@(LstS _) b = do
  scope <- MM.getGeneralScope
  let bApplied = apply g1 b
  -- Failing a list wire parent, the Packable instance's wire form is used
  -- (@Map a b@). Not 'evaluateType': it re-expands arguments, so for
  -- @type Pat = [Pat]@ the @bWire /= bApplied@ test would never settle.
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
-- As @synthE LetS@, but the body is checked, so the expected type reaches
-- the literals in it.
checkE _ g (LetS v e1 e2) t = do
  (g1, t1, e1') <- synthG g e1
  let g2 = g1 ++> [AnnG v t1]
      g2' = case tryEvalConst g2 (let AnnoS _ _ e = e1' in e) of
        Just val' -> g2 { gammaIntVals = Map.insert v val' (gammaIntVals g2) }
        Nothing -> g2
  (g3, t2, e2') <- checkG g2' e2 t
  return (g3, t2, LetS v e1' e2')
-- A tuple or record literal is never Null, so against @?T@ it is checked
-- at T and coerced.
checkE i g e@(TupS _) (OptionalU innerT) = checkOptionalLit i g e innerT
checkE i g e@(NamS _) (OptionalU innerT) = checkOptionalLit i g e innerT
checkE i g (TupS xs) t = do
  scope <- MM.getGeneralScope
  let tApplied = apply g t
      -- One 'reduceType' step, not 'evaluateType', which would also expand
      -- record-typed slots (@Trie Int@) into NamU bodies that no subtype
      -- rule matches against AppU. The head must be the TupleN
      -- constructor itself, not any alias with as many arguments.
      tryMatch t' = case t' of
        AppU (VarU v) ts
          | v == BT.tuple (length xs)
          , length ts == length xs -> Just ts
        _ -> Nothing
      tsOpt = case tryMatch tApplied of
        Just ts -> Just ts
        Nothing -> case TE.reduceType scope tApplied >>= tryMatch of
          Just ts -> Just ts
          -- 'reduceType' halts at a newtype (@newtype Pair a = (a, a)@).
          Nothing -> tryMatch (TE.wireParentRoot scope tApplied)
  case tsOpt of
    Just ts -> do
      let go gAcc acc [] [] = return (gAcc, reverse acc)
          go gAcc acc (x:xrest) (et:trest) = do
            (g', _, x') <- checkG gAcc x et
            go g' (x' : acc) xrest trest
          go _ _ _ _ = error "unreachable: arity matched by guard"
      (gFinal, xs') <- go g [] xs ts
      -- The original type, not the reduced one: codegen relies on the
      -- alias name (@BTree Int@) surviving.
      return (gFinal, apply gFinal t, TupS xs')
    Nothing -> checkEFallback i g (TupS xs) t
-- A record literal against a declared record: fields are put in declared
-- order, the key sets must agree, and each value is checked.
checkE i g (NamS rs) t = do
  scope <- MM.getGeneralScope
  let tApplied = apply g t
      tEval = either (const tApplied) id (TE.evaluateType scope tApplied)
      -- 'evaluateType' halts at a newtype; 'wireParentRoot' expands it.
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
-- A numeric literal inhabits @t@ when @t@'s alias and wire-parent chain
-- reaches a numeric base type; it is typed at @t@ itself.
checkE i g (IntS si x) t = do
  scope <- MM.getGeneralScope
  let tApplied = apply g t
      tEval = either (const tApplied) id (TE.evaluateType scope tApplied)
      tWire = TE.wireParentRoot scope tEval
      -- An integer literal may be real (@4 + 2.3@); the node stays IntS and
      -- codegen converts it by its type.
      acceptable u = BT.isIntegerBaseType u || BT.isRealBaseType u
  if acceptable tApplied || acceptable tEval || acceptable tWire || literalAtRigid IntDefault g tApplied
    then return (g, tApplied, IntS si x)
    else case tApplied of
      -- Deferred: a later argument may still fix the type, see
      -- 'resolvePendingNumLits'.
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
-- As for IntS, a literal inhabits a type whose wire-parent chain reaches
-- its primitive.
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
-- A setter against a known result type: the expected type is seated into
-- the value slots first, so @.(.n = 42) r@ can write an @I32@ field. The
-- expected type only pins slots the receiver would pin the same way. A
-- setter that changes its receiver's type cannot be seated and falls back.
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
-- Patterns have synthesis rules that the App-Check rule below would bypass.
checkE i g0 e@(AppS (AnnoS _ _ (ExeS _)) _) t = checkEFallback i g0 e t
-- Against @?T@ the App-Check rule would push the optional into the
-- parameters (@at :: f a -> a@ would need @xs :: f ?a@); the fallback
-- coerces the result instead.
checkE i g0 e@(AppS _ _) t@(OptionalU _) = checkEFallback i g0 e t
-- App-Check (Dunfield-Krishnaswami): when the return type is a bare
-- existential, it is unified with the expected type before the arguments
-- are checked, so a literal at a recursive type (@type Pair a = (a, ?(Pair
-- a))@) is checked rather than synthesized to a finite shape. A structured
-- return (@r - f@, @m * n@) has nothing safe to pin; it and every other
-- case take the synth-and-subtype path.
checkE i g0 (AppS f xs) t = do
  (g1, funType0, funExpr0) <- synthG g0 f
  let g1' = resolveNatLabels f funType0 xs g1
      (g2, funType1) = stripForallU g1' (normalizeType funType0)
      -- Finish from the head already synthesized: synthesizing it again
      -- would compound once per level of nesting. A nullary application
      -- has its own synthesis rule, so it takes the fallback.
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
              -- Again after the arguments, to carry what they solved back
              -- to the expected type.
              case subtype scope (apply g4 returnType) (apply g4 t) g4 of
                Right g5 -> return (g5, apply g5 t, AppS funExpr0 xsAnn)
                Left _ -> finish
            Left _ -> finish
      | otherwise -> finish
    _ -> finish
  where
    isBareExistU (ExistU _ _ _) = True
    isBareExistU _ = False
-- A bare reference whose type takes more arguments than the expected type
-- (`zipWith add3 xs ys`) is eta-expanded to the expected arity, so that it
-- is never called with too few arguments. Applications and lambdas are
-- handled by 'etaExpandSynthE' and the LamS rule.
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

-- | Reconcile a synthesized expression with an expected type: subtype,
-- else coerce, else report.
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
    -- Between two record existentials InstLReach solves only the literal's,
    -- so the expected side can keep a narrower key set.
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

-- | Check a tuple or record literal at @T@ and coerce it to @?T@.
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

-- | A setter is desugared to a lambda (@\\v -> pat v values...@) so it can
-- be unapplied. Applied directly, the lambda would hide the receiver from
-- the setter rule, so the redex is reduced first. The parameter is
-- compiler-generated and occurs exactly once, so this duplicates no work.
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

-- | The join of two suspension types whose rows have at most one tail
-- variable between them: the union of the rows, with each branch a subtype
-- of the result. Nothing when either is not a suspension, the rows have
-- two distinct tails, or the payloads do not meet in either direction.
joinSuspensions :: Scope -> Gamma -> TypeU -> TypeU -> Maybe (Gamma, TypeU)
joinSuspensions scope g (EffectU e1 i1) (EffectU e2 i2)
  | (_, vs1) <- effectSetParts (applyEff g e1)
  , (_, vs2) <- effectSetParts (applyEff g e2)
  , Set.size (Set.union vs1 vs2) <= 1 =
      let row = unionEffectSet (applyEff g e1) (applyEff g e2)
          into inner = do
            g1 <- subtype scope (EffectU e1 i1) (EffectU row inner) g
            g2 <- subtype scope (apply g1 (EffectU e2 i2)) (apply g1 (EffectU row inner)) g1
            return (g2, apply g2 (EffectU row inner))
       in either (const Nothing) Just (into i1 `orElse` into i2)
  where
    orElse (Left _) b = b
    orElse a _ = a
joinSuspensions _ _ _ _ = Nothing

-- | The record type with more keys, else the expected type @b@. On a tie
-- a structural @b@ takes the synthesized key order: key order is the wire
-- layout, and the expression's sub-nodes were built in that order. A
-- declared record is never reordered; subtyping already made the
-- synthesized side agree with it.
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

-- | Put a structural record type's rows in the given key order. If a key
-- is missing, or the type is not a structural record, it is unchanged.
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

-- | Conditions grouped so that no two groups share a variable, each group
-- in input order.
conditionGroups :: [(Int, ND.Condition, MDoc)] -> [[(Int, ND.Condition, MDoc)]]
conditionGroups xs = map (map snd . List.sortOn fst) (foldl insert [] (zip [0 :: Int ..] xs))
  where
    vars (_, (_, c, _)) = ND.condVars [c]
    insert groups x =
      let (linked, apart) = List.partition (any (not . Set.null . Set.intersection (vars x) . vars)) groups
      in (x : concat linked) : apart

-- | Attribute the kind equations deferred between two contexts to term
-- @i@, unless an inner term already claimed them.
locateDeferred :: Int -> Gamma -> Gamma -> Gamma
locateDeferred i g g' = g' { gammaDeferredAt = foldr (\p -> Map.insertWith (\_ old -> old) p i) (gammaDeferredAt g') (newlyDeferred g g') }

-- | The kind equations deferred between two contexts.
newlyDeferred :: Gamma -> Gamma -> [(TypeU, TypeU)]
newlyDeferred g g' = take (length (gammaDeferred g') - length (gammaDeferred g)) (gammaDeferred g')

-- | A hint for a mismatch between a newtype and an existential application
-- (@sum :: Foldable f => f a -> a@): most likely the newtype lacks an
-- instance.
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

-- | The Nat arguments of a type (@Matrix 2 4 Int@ gives @[2, 4]@), taken
-- from the deepest alias level that still has any:
-- @type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int@ is dimensioned by
-- @n + m@.
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

-- | Check each nesting level of a list literal: @length xs ~ n@.
checkListNatDims :: Gamma -> [TypeU] -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad Gamma
checkListNatDims g [] _ = return g
checkListNatDims g (nat:nats) (AnnoS _ idx (LstS xs)) = do
  g' <- subtype' idx (NatLitU (fromIntegral (length xs))) nat g
  foldlM (\g'' x -> checkListNatDims g'' nats x) g' xs
checkListNatDims g _ _ = return g

-- | A coercion chain from a to b, inside-out.
tryCoerce :: Scope -> TypeU -> TypeU -> Gamma -> Maybe ([Coercion], Gamma)
-- @<E1> T@ to @<E2> ?T@ when closed row E1 is in closed row E2; an open
-- row declines.
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

-- | Wrap an annotated expression in CoerceS layers.
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
stripCoercionWrappers :: TypeU -> TypeU
stripCoercionWrappers (OptionalU t) = stripCoercionWrappers t
stripCoercionWrappers t = t

-- | Wrap the final expression of a do-block body, past its LetS layers, in
-- EvalS.
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

-- | The effects of every EvalS node in a do-block body, at any depth.
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
      LabelS _ e -> go e
      IntrinsicS _ es -> unions (map go es)
      _ -> emptyEffectSet

    -- @forall a. <Random> a@ still contributes its effects.
    effectOfAnno (AnnoS (Idx _ t) _ _) = case peelForallU t of
      EffectU effs _ -> effs
      _ -> emptyEffectSet

    unions = foldl effUnion emptyEffectSet

    effUnion a b
      | a == emptyEffectSet = b
      | b == emptyEffectSet = a
      | a == b = a
      | otherwise = EffectUnion a b

-- | The effects of evaluating an expression: those of every EvalS reachable
-- without crossing a lambda or do-block, whose effects are in their type
-- (EFF-6, EFF-10).
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
      LabelS _ e -> go e
      IntrinsicS _ es -> unions (map go es)
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

-- | Peel lambdas off the body in step with arrows off the type, while the
-- arities agree; any other shape is returned as it is.
peelLambdaLayers
  :: TypeU
  -> AnnoS (Indexed TypeU) f c
  -> (TypeU, AnnoS (Indexed TypeU) f c)
peelLambdaLayers (FunU args ret) body@(AnnoS _ _ (LamS vs inner))
  | length args == length vs = peelLambdaLayers ret inner
  | otherwise = (FunU args ret, body)
peelLambdaLayers t body = (t, body)

-- | EFF-10: the effects of evaluating a body must be among those its
-- declared return type permits. The error points at the force that
-- produced an uncovered effect, else at @idx@. Callers pass the declared
-- type with gamma applied; only concrete labels count, since a quantified
-- tail variable cannot cover an effect the body always performs.
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

-- | The indices of the forces that carry an uncovered effect, in pre-order,
-- over the same nodes 'inferExprEffects' visits.
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
      LabelS _ e -> go e
      IntrinsicS _ es -> concatMap go es
      _ -> []

    effLabels (AnnoS (Idx _ t) _ _) = case peelForallU t of
      EffectU effs _ -> resolveEffectSet effs
      _ -> Set.empty


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

-- | Reduce an expression to a compile-time constant, if it is one.
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
tryEvalConst g (AppS (AnnoS _ _ (ExeS (PatCall (PatternStruct
  (SelectorIdx (idx, SelectorEnd) []))))) [AnnoS _ _ inner])
  = case tryEvalConst g inner of
      Just (ConstTup vs) | idx >= 0, idx < length vs -> Just (vs !! idx)
      _ -> Nothing
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

-- | Try to reduce an expression to a list of string constants.
tryEvalStrList :: Gamma -> ExprS g f c -> Maybe [Text]
tryEvalStrList g e = case tryEvalConst g e of
  Just (ConstList vs) -> mapM asStr vs
  _ -> Nothing
  where
    asStr (ConstStr s) = Just s
    asStr _ = Nothing

tryExtractStrListPre :: Gamma -> AnnoS Int ManyPoly Int -> Maybe [Text]
tryExtractStrListPre g (AnnoS _ _ e) = tryEvalStrList g e

-- | Each label of a signature with the argument it binds, where given.
labelArguments :: EType -> [a] -> [(TVar, a)]
labelArguments et args = [(v, a) | (v, idx) <- Map.toList (enatLabels et), a <- take 1 (drop idx args)]

-- | The labels that bind a Str or a list of Str rather than an Int.
strLabelVars :: TypeU -> Set.Set TVar
strLabelVars t = Set.fromList (collectStrVarNames t <> collectListVarNames t)

-- | Solve labeled kind parameters (@m\@Int@, @f\@Str@, @l:[Str]@) from
-- constant arguments, so that the return type, and type operators such as
-- @r - l@ in it, reduce.
resolveNatLabels ::
  AnnoS Int ManyPoly Int ->  -- the function expression (pre-synthesis)
  TypeU ->                    -- the synthesized (renamed) function type
  [AnnoS Int ManyPoly Int] -> -- the arguments
  Gamma -> Gamma
resolveNatLabels (AnnoS _ _ (VarS _ (MonomorphicExpr (Just et) _))) funType args g
  | not (Map.null labels) =
    let -- Nat and Int variables: each original name to its renamed one
        origNvs = nub (collectNatVarNames (etype et))
        renamedNvs = nub (collectNatVarNames funType)
        natRenMap = Map.fromList (zip origNvs renamedNvs)
        natSolutions = Map.fromList
          [ (renamedVar, n)
          | (origVar, arg) <- labelArguments et args
          , Just renamedVar <- [Map.lookup origVar natRenMap]
          , Just n <- [labelValue arg]
          ]
        origSvs = nub (collectStrVarNames (etype et))
        renamedSvs = nub (collectStrVarNames funType)
        strRenMap = Map.fromList (zip origSvs renamedSvs)
        strSolutions = Map.fromList
          [ (renamedVar, StrLitU s)
          | (origVar, arg) <- labelArguments et args
          , Just renamedVar <- [Map.lookup origVar strRenMap]
          , Just s <- [tryExtractStrPre g arg]
          ]
        origLvs = nub (collectListVarNames (etype et))
        renamedLvs = nub (collectListVarNames funType)
        listRenMap = Map.fromList (zip origLvs renamedLvs)
        listSolutions = Map.fromList
          [ (renamedVar, ListLitU (map StrLitU ss))
          | (origVar, arg) <- labelArguments et args
          , Just renamedVar <- [Map.lookup origVar listRenMap]
          , Just ss <- [tryExtractStrListPre g arg]
          ]
        keyed k = Map.mapKeys (\v -> (v, k))
        combined = Map.unions
          [ Map.fromList [((v, natKind v), t) | (v, t) <- Map.toList natSolutions]
          , keyed KindStr strSolutions
          , keyed (KindList KindStr) listSolutions
          ]
    in g { gammaKindSubs = Map.union combined (gammaKindSubs g) }
  where
    labels = enatLabels et
    intLabels = Set.fromList [w | IntVarU w <- universeT funType]
    natKind v = if Set.member v intLabels then KindInt else KindNat
    universeT t = t : concatMap universeT (typeUChildren t)
    -- A literal, or a parameter that the enclosing signature's label binds
    -- to an Int.
    labelValue a@(AnnoS _ _ e) = case (tryExtractIntPre g a, e) of
      (Just n, _) -> Just (NatLitU n)
      (_, BndS x) -> IntVarU <$> Map.lookup x (gammaLabelVars g)
      _ -> Nothing
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
peakSExpr (LabelS _ _) = "LabelS"
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
      (g1, _, e1) <- synthG g0 e0
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
      checkRigidNats i g1 [(j, t) | AnnoS (Idx j t) _ _ <- annoNodes e1]
      closure <- superclassClosure givens
      heads <- instanceHeads
      mapM_
        (requireEntailed i closure)
        [ c
        | c@(Constraint cls args) <- obligations
        , any (mentionsRigid rigid) args
        , not (any (covers args) (Map.findWithDefault [] cls heads))
        ]

-- | POLY-7: the body of a definition holds for every value of its
-- signature's Nat variables at which the signature's own Nat positions are
-- non-negative (KIND-4). The Nat equations checking left, solved or
-- deferred, and the non-negativity of every Nat position in the body's
-- types, that reach one of those variables are decided with them rigid
-- (DIM-7).
checkRigidNats :: Int -> Gamma -> [(Int, TypeU)] -> MorlocMonad ()
checkRigidNats i g bodyTypes
  | Set.null rigid = return ()
  | (j : _) <- [j | (j, ps) <- bySignature, ND.Refuted _ <- [ND.decide Set.empty (nonNegative ps)]] =
      MM.throwSourcedError j
        "No value of the Nat and Int variables of this signature makes every Nat position in it non-negative (KIND-4)"
  | otherwise = case reaching of
      [] -> return ()
      conds -> case ND.decideAssuming rigid domain conds of
        ND.Holds _ -> return ()
        ND.Refuted env ->
          MM.throwSourcedError (failingTerm env) $ vsep
            [ "The body does not hold for every value of the Nat and Int variables of its signature (POLY-7)."
            , if Map.null env
                then "These conditions have no solution:"
                else
                  "It fails at" <+> hsep (punctuate "," [natName v <+> "=" <+> pretty n | (v, n) <- Map.toList env])
                    <> ", where these conditions have no solution:"
            , vsep ["  " <> prettyCond c | c <- conds]
            ]
        ND.Unknown ->
          MM.throwSourcedError (firstTagged conds) $ vsep
            [ "The compiler cannot decide whether these conditions hold for every value"
                <+> "of the Nat and Int variables of the signature (POLY-7, DIM-7):"
            , vsep ["  " <> prettyCond c | c <- conds]
            ]
  where
    rigid = gammaRigidNats g
    nonNegative ts = [ND.NatGe e (NS.NatLit 0) | t <- ts, Just e <- [typeUToNatExpr t]]
    domain = nonNegative (map snd (gammaRigidDomain g))
    bySignature = Map.toList (Map.fromListWith (flip (<>)) [(j, [p]) | (j, p) <- gammaRigidDomain g])
    solved =
      [ ND.NatEq (if k == KindInt then NS.IntVar v else NS.NatVar v) e
      | ((v, k), t) <- Map.toList (gammaKindSubs g)
      , fillsNat k
      , Just e <- [typeUToNatExpr (apply g t)]
      ]
    -- Each deferred equation at the term where it arose, and each Nat
    -- position at the term whose type holds it.
    deferredAt =
      [ (Map.findWithDefault i p (gammaDeferredAt g), ND.NatEq a b)
      | p@(t1, t2) <- gammaDeferred g
      , Just a <- [typeUToNatExpr (apply g t1)]
      , Just b <- [typeUToNatExpr (apply g t2)]
      ]
    deferred = map snd deferredAt
    located = [(j, c) | (j, t) <- bodyTypes, c <- nonNegative (natPositions (apply g t))]
    positions = map snd located
    tagged = deferredAt <> located
    reaching = ND.reaching rigid (Set.toList (Set.fromList (filter (not . trivial) (solved <> deferred <> positions))))
    trivial (ND.NatEq a b) = a == b
    trivial _ = False
    -- The innermost term with a condition that, beside the body's
    -- equations, has no solution at the counterexample (when the
    -- equations alone have one), or else the definition.
    failingTerm env =
      let pinned = map (pin env)
          equations = [c | c@(ND.NatEq _ _) <- reaching]
          base = if refuted (pinned equations) then [] else pinned equations
       in case [j | (j, c) <- reverse tagged, refuted (pin env c : base)] of
            (j : _) -> j
            [] -> i
    refuted cs = case ND.decide Set.empty cs of
      ND.Refuted _ -> True
      _ -> False
    pin env = ND.substCond (NS.substituteNat (Map.map NS.NatLit env))
    -- The innermost term holding one of the conditions, or the definition.
    firstTagged conds = case [j | (j, c) <- reverse tagged, c `elem` conds] of
      (j : _) -> j
      [] -> i
    prettyCond (ND.NatEq a b) = prettyNat a <+> "=" <+> prettyNat b
    prettyCond (ND.NatGe a b) = prettyNat a <+> ">=" <+> prettyNat b
    -- One name per variable across every condition: a signature's own as
    -- written, an inferred one marked with '?'.
    prettyNat e = case e of
      NS.NatLit n -> pretty n
      NS.NatVar v -> natName v
      NS.IntVar v -> natName v
      NS.NatAdd x y -> parens (prettyNat x <+> "+" <+> prettyNat y)
      NS.NatSub x y -> parens (prettyNat x <+> "-" <+> prettyNat y)
      NS.NatMul x y -> parens (prettyNat x <+> "*" <+> prettyNat y)
      NS.NatDiv x y -> parens (prettyNat x <+> "/" <+> prettyNat y)
    natName v@(TV t)
      | Set.member v rigid = pretty (MT.takeWhile (/= '@') t)
      | otherwise = "?" <> pretty (MT.takeWhile (/= '@') t <> MT.filter (`elem` ['0' .. '9']) (MT.dropWhile (/= '@') t))

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
