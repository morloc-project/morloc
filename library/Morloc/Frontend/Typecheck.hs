{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
module Morloc.Frontend.Typecheck (typecheck, typecheckRoot, isPattern, typecheckRootAt, validate, Checked, checkSigned, inferScheme, defaultedExport, checkedAt, chosenLengthsApplied, useConstraint, resolveTypes, evaluateAnnoSTypes, peakSExpr) where

import Data.Foldable (toList)
import Morloc.Data.Zip (indexed, zipExact, zipPrefix, zipSame, zipWithSame)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as MT
import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Frontend.Instances as FI
import Morloc.Frontend.Namespace
import Morloc.Frontend.Rename (displayName)
import Morloc.Frontend.PatternCheck (missingKeyError, receiverShape, rowFields, tupleWidthError)
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal
import Morloc.Typecheck.Solve (Residual (..), Solution (..), solve)
import qualified Data.IntMap.Strict as IntMap
import qualified Morloc.Typecheck.NatSolver as NS
import qualified Morloc.Typecheck.NatDecide as ND

-- | True for the set-theoretic constraints that 'dischargeConstraints'
-- resolves. A class-form constraint left in gamma is not reported.
isPrimitiveConstraint :: Constraint -> Bool
isPrimitiveConstraint (Constraint _ _) = False
isPrimitiveConstraint (CMember _ _) = True
isPrimitiveConstraint (CSubset _ _) = True
isPrimitiveConstraint (CDisjoint _ _) = True

-- | The type a pending literal's variable has now, its optional wrapper
-- removed: a literal in an optional position is judged on the payload type.
-- An effect is never removed: a literal is pure (EFF-5).
literalEnd :: Gamma -> TVar -> TypeU
literalEnd g v = peel (apply g (ExistU v ([], Open) ([], Open)))
  where
    peel (OptionalU t) = peel t
    peel t = t

-- | Resolve the numeric literals that were checked against an unsolved
-- existential. Real literals go first, so that an existential shared by a
-- real and an integer literal becomes @Real@ whatever the argument order;
-- an integer literal accepts a real type, never the reverse. An integer
-- literal whose type must be Numeric defaults like a real one. A literal
-- whose existential is one of the given signature variables stays open.
resolvePendingNumLits :: Set.Set TVar -> Gamma -> [(Int, TVar, NumLitKind)] -> MorlocMonad Gamma
resolvePendingNumLits held g0 entries = do
  closed <- superclassClosure [c | c@(Constraint _ [_]) <- wantedConstraints g0]
  let numericVars = Set.fromList
        [ v
        | Constraint cls [t] <- closed
        , cls == BT.numericClass
        , ExistU v _ _ <- [apply g0 t]
        ]
  let (intLits0, realLits0) = partition (\(_, _, k) -> k == IntDefault) (filter (\(_, v, _) -> Set.notMember (chainEnd v) held) entries)
      (numericInts, intLits) = partition (\(_, v, _) -> Set.member (chainEnd v) numericVars) intLits0
      realLits = realLits0 ++ numericInts
  g1 <- foldlM (resolveOne BT.isRealBaseType BT.realU) g0 realLits
  foldlM (resolveOne acceptableForInt BT.intU) g1 intLits
  where
    chainEnd v = case literalEnd g0 v of
      ExistU v' _ _ -> v'
      VarU u -> u
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
      case literalEnd g v of
           ExistU vEnd _ _ ->
             case solveExist vEnd defaultT g of
               Left err        -> MM.throwSourcedError i err
               Right Nothing   -> return g
               Right (Just g') -> return g'
           t -> do
             ok <- baseOKThroughAliases baseOK t
             if ok
               then return g
               else MM.throwSourcedError i $
                 "Numeric literal cannot have type " <> prettyTypeU t


-- | NUM-10: the default of the variable @v@ under the class constraints
-- that mention it, if they are each of a numeric class or a
-- one-argument class of @internal@, and one of them is numeric on it: @Int@,
-- or @Real@ under @Numeric@. A constraint over a type holding @v@, as
-- @Eq [a]@ or @Show (a, b)@, is one instances reduce to constraints on its
-- variables. @var@ names the variables a type is.
numericDefault :: Map.Map ClassName ClassInfo -> (TypeU -> Maybe TVar) -> TVar -> [Constraint] -> Maybe TypeU
numericDefault classes var v cs
  | all alone cs && any (on BT.numericClass) cs = Just BT.realU
  | all alone cs && any (on BT.integralClass) cs = Just BT.intU
  | otherwise = Nothing
  where
    plain cls =
      cls == BT.integralClass
        || cls == BT.numericClass
        || maybe False (\ci -> ciModule ci == MV "internal" && length (ciVars ci) == 1) (Map.lookup cls classes)
    alone c = case c of
      Constraint cls [_] -> plain cls
      _ -> False
    on cls c = case c of
      Constraint cls' [t] -> cls' == cls && var t == Just v
      _ -> False

-- | The class constraints mentioning each variable @var@ names.
constraintsOn :: (TypeU -> Maybe TVar) -> [Constraint] -> Map.Map TVar [Constraint]
constraintsOn var cs =
  Map.fromListWith
    (<>)
    [ (v, [c])
    | c@(Constraint _ ts) <- cs
    , v <- List.nub (mapMaybe var (concatMap everyType ts))
    ]

-- | NUM-10 at the end of a tree: each existential nothing determines, other
-- than those in @held@, is solved to its default.
defaultNumeric :: Set.Set TVar -> Gamma -> MorlocMonad Gamma
defaultNumeric held g = do
  classes <- MM.gets (ieClasses . stateInstances)
  let var t = case t of
        ExistU w _ _ -> Just w
        _ -> Nothing
      defaults =
        [ (v, t)
        | (v, cs) <- Map.toList (constraintsOn var (map (apply g) (wantedConstraints g)))
        , Set.notMember v held
        , Just t <- [numericDefault classes var v cs]
        ]
      solveTo gx (v, t) = case solveExist v t gx of
        Right (Just g') -> g'
        _ -> gx
  return (foldl solveTo g defaults)

-- | NUM-10 at an unsigned export, which is a use of its inferred type: each
-- quantified variable the rule defaults is replaced by its default.
defaultedExport :: Map.Map ClassName ClassInfo -> EType -> TypeU
defaultedExport classes et =
  let (vs, t) = unqualify (etype et)
      var u = case u of
        VarU w | w `elem` vs -> Just w
        _ -> Nothing
      on = constraintsOn var (Set.toList (econs et))
      defaults = [(v, d) | v <- vs, Just d <- [numericDefault classes var v (Map.findWithDefault [] v on)]]
   in foldr ForallU (foldr (\(v, d) acc -> substituteTVar v d acc) t defaults) [v | v <- vs, v `notElem` map fst defaults]

-- | The unsolved existentials the constrained variables of a signature being
-- checked became, at the end of their chains. A use's obligations already
-- name their existentials; only the variables of a definition's own
-- signature are written as variables here.
constrainedVars :: Gamma -> Set.Set TVar
constrainedVars g =
  Set.fromList
    [ end
    | Constraint _ ts <- wantedConstraints g
    , VarU v <- concatMap everyType ts
    , end <- case apply g (ExistU v ([], Open) ([], Open)) of
        ExistU v' _ _ -> [v, v']
        _ -> [v]
    ]

everyType :: TypeU -> [TypeU]
everyType t = t : concatMap everyType (typeUChildren t)

-- | The Str, Rec and List variables of a type, as 'renameWithMap' collects
-- them. A Set variable is not renamed per signature, so it is not one.
rigidSlotVars :: TypeU -> Set.Set TVar
rigidSlotVars t = Set.fromList (collectStrVarNames t <> collectRecVarNames t <> collectListVarNames t)

typecheck ::
  [AnnoS Int ManyPoly Int] ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheck = mapM typecheckRoot

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
typecheckRoot = typecheckWith Nothing >=> assertStructuralTypesExpanded

-- | Check one tree against a type, as a use of it at that type is checked.
typecheckRootAt :: TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckRootAt t = typecheckWith (Just t) >=> assertStructuralTypesExpanded

typecheckWith :: Maybe TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
typecheckWith expected e0 = do
  chosenLengthsApplied e0
  -- An exported command's own lambda is applied once per call (DIM-10).
  let g0 = emptyGamma {gammaDefinitionLambda = ownLambda e0}
  (g1raw, rootT, e1) <- case expected of
    Nothing -> synthG g0 e0
    Just t -> checkG g0 e0 t
  finishRoot g1raw rootT e1

-- | What follows inference of a tree: literal defaults, instance
-- resolution, and the checks that need every existential solved.
finishRoot :: Gamma -> TypeU -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
finishRoot g1raw rootT e1 = run
  where
    run :: MorlocMonad (AnnoS (Indexed TypeU) Many Int)
    run = do
      scope <- MM.getGeneralScope
      -- Literal defaults must precede instance resolution: in @3 <= 5@
      -- the class variable is solved only by the Int default. A signature's
      -- constrained variable left in the root's type is not defaulted, so
      -- the export stays generic (SIG-5).
      let rootVars = Set.fromList (map fst (varForms (apply g1raw rootT)))
      let held = Set.intersection rootVars (constrainedVars g1raw)
      g1 <- resolvePendingNumLits held g1raw (gammaPendingNumLits g1raw) >>= defaultNumeric held

      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g1:"
      seeGamma g1
      insetSay "========================================================"
      e2 <- coerceLiteralsToOptional . mapAnnoSG (fmap normalizeType) . applyGen g1 $ e1

      let openRecords = openRecordLiterals e2
      (g2, e3) <- resolveTree g1 e2
      g3 <- case settleDeferred scope (apply g2 g2) of
        Left (site, err) -> MM.throwSourcedError (fromMaybe (case e1 of AnnoS (Idx gi _) _ _ -> gi) site) err
        Right g -> return g
      checkShapes g3 e1

      -- Decide the Nat and Int conditions left open now that existentials
      -- are solved: the deferred equations, each at the term where it
      -- arose, and the non-negativity of every Nat position still open, at
      -- the term whose type holds it (DIM-6, KIND-4, DIM-7).
      let rootIndex = case e1 of AnnoS (Idx gi _) _ _ -> gi
          site p = Map.findWithDefault rootIndex p (gammaDeferredAt g3)
          natEquation p@(t1, t2) = case (natPositionExpr (apply g3 t1), natPositionExpr (apply g3 t2)) of
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
                 , Just e <- [natPositionExpr pos]
                 , not (NS.isGround e)
                 ]
          -- Each condition once, at its first term.
          firstOfEach cs = go Set.empty cs
            where
              go _ [] = []
              go seen (x@(_, c, _) : rest)
                | Set.member c seen = go seen rest
                | otherwise = x : go (Set.insert c seen) rest
      -- A label value known only at run time, and the length of a
      -- command's input, stand for every value they may take, given the
      -- part of the command's domain its input is checked against: the
      -- Nat positions over what the input gives (DIM-10, DIM-3, KIND-4).
      let rootType = case e3 of AnnoS (Idx _ t) _ _ -> apply g3 t
          inputs = fst (uncurryU rootType)
          given = Set.union
            (runTimeVars inputs)
            (Set.fromList [v | IntVarU v <- everyType rootType, Set.notMember v (gammaObserved g3)])
          runTime = Set.union (gammaObserved g3) (runTimeVars inputs)
          entryDomain =
            [ ND.NatGe e (NS.NatLit 0)
            | pos <- natPositions rootType
            , Just e <- [natPositionExpr pos]
            , all (`Set.member` given) (NS.freeNatVars e)
            ]
      -- An input's length is the caller's; equating it with a length a call
      -- chooses cannot be checked when the command starts (DIM-10).
      let taken = filter (`Set.member` gammaObserved g3) (inwardNats rootType)
          isCall (AppS _ _) = True
          isCall PatS {} = True
          isCall _ = False
          calls = [j | AnnoS (Idx j t) _ x <- annoNodes e3, isCall x, any (`elem` taken) (collectNatVarNames (apply g3 t))]
      case e3 of
        AnnoS (Idx i _) _ _
          | not (null taken) ->
              throwTypeError (fromMaybe i (listToMaybe calls))
                "the length of an input of this command is taken to equal a length a call in its body chooses at run time, which the input cannot be checked against (DIM-10); give the command a signature that states the relation"
        _ -> return ()
      forM_ (conditionGroups conditions) $ \grp -> do
        let cs = [c | (_, c, _) <- grp]
            observed = Set.intersection runTime (ND.condVars cs)
            answer
              | Set.null observed = ND.decide Set.empty cs
              | otherwise = ND.decideAssuming observed entryDomain cs
            unsolvable = case ND.decide Set.empty cs of
              ND.Refuted _ -> True
              _ -> False
        case answer of
          ND.Holds _ -> return ()
          _ -> case grp of
            [] -> return ()
            ((i, _, _) : _) ->
              MM.throwSourcedError i $ vsep
                [ case answer of
                    _ | unsolvable -> "These kind-level conditions have no solution:"
                    _ | not (Set.null observed) ->
                      "These kind-level conditions do not hold for every length and label value the input may give:"
                    ND.Refuted _ -> "These kind-level conditions have no solution:"
                    _ -> "The compiler cannot decide whether these kind-level conditions have a solution:"
                , vsep ["  " <> d | (_, _, d) <- grp]
                , if Set.null observed || unsolvable
                    then mempty
                    else "  A length or label known only at run time is checked only where a signature states it; give the enclosing function a signature that labels the value and states the relation (DIM-3)."
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
        Left err -> MM.throwRejected ("Constraint violation: " <> err)
        Right g3' ->
          case filter isPrimitiveConstraint (gammaConstraints g3') of
            [] -> return ()
            cs ->
              let displayed = map (prettyConstraint . apply g3') cs
                  fnName = case e1 of
                    AnnoS _ _ (VarS (EV n) _) -> Just n
                    _ -> Nothing
                  prefix = case fnName of
                    Just n -> "Unsolved constraint(s) on " <> dquotes (pretty n) <> "; add to its signature: "
                    Nothing -> "Unsolved constraint(s); add to the function signature: "
              in MM.throwRejected $
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
    f x@PatS {} = mapExprS resolveTypes x
    f (LamS vs x) = LamS vs (resolveTypes x)
    f (LstS xs) = LstS (map resolveTypes xs)
    f (TupS xs) = TupS (map resolveTypes xs)
    f (NamS rs) = NamS (map (second resolveTypes) rs)
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

-- | How a use whose class arguments nothing fixed yet is treated. A lenient
-- pass leaves it unresolved, so that a later pass may find it fixed by
-- another use's instance (CLS-20), and counts the uses it leaves and those it
-- resolves; the strict pass reports it. A strict pass over a tree whose type
-- is still generic leaves it too: such a tree is never realized (SIG-5).
data Strictness = Strict Bool | Lenient (IORef (Int, Int))

-- | Resolve every class constraint of a tree's uses: lenient passes until
-- none is left undetermined or a pass resolves no more uses than the one
-- before, then a strict one if some still is. Each pass after the first
-- starts from the solutions the one before found, and adds its deferred
-- equations afresh.
--
-- The constraints of the definition the tree checks are its assumptions,
-- met where it is used, not here.
resolveTree :: Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (Gamma, AnnoS (Indexed TypeU) Many Int)
resolveTree g0 e =
  lenientPasses g0 e >>= \case
    (g, Just e') -> return (g, e')
    (g, Nothing) -> resolveRoot (Strict generic) g e
      where
        generic = case e of
          AnnoS (Idx _ t) _ _ -> FI.hasOpenVars (apply g t)

-- | The lenient passes of 'resolveTree': the tree resolved if none left a
-- use, and the gamma the next pass starts from.
lenientPasses :: Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (Gamma, Maybe (AnnoS (Indexed TypeU) Many Int))
lenientPasses g0 e = go g0 (-1)
  where
    go g before = do
      ref <- liftIO (newIORef (0, 0))
      (g', e') <- resolveRoot (Lenient ref) g e
      (left, resolved) <- liftIO (readIORef ref)
      let next = g' {gammaDeferred = gammaDeferred g0, gammaDeferredAt = gammaDeferredAt g0}
      if
        | left == 0 -> return (g', Just e')
        | resolved > before -> go next resolved
        | otherwise -> return (next, Nothing)

resolveRoot :: Strictness -> Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (Gamma, AnnoS (Indexed TypeU) Many Int)
resolveRoot mode g = \case
  AnnoS gi ci (VarS v (MonomorphicExpr _ xs)) -> do
    (g', xs') <- statefulMapM (resolveInstances mode) g xs
    return (g', AnnoS gi ci (VarS v (Many [] (map ((,) Nothing) xs'))))
  e -> resolveInstances mode g e

resolveInstances ::
  Strictness -> Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (Gamma, AnnoS (Indexed TypeU) Many Int)
-- A shape is checked from the typed tree ('checkShapes'); the binding that
-- holds one goes with it.
resolveInstances mode g (AnnoS _ _ (LetS _ (AnnoS _ _ (ShapeS {})) body)) = resolveInstances mode g body
resolveInstances mode g (AnnoS _ _ (ShapeS _ _ r)) = resolveInstances mode g r
resolveInstances mode g (AnnoS gi@(Idx _ gt) ci e0) = do
  gscope <- MM.getGeneralScope
  env <- MM.gets stateInstances
  (g', e1) <- f gscope env g e0
  return (g', AnnoS gi ci e1)
  where
    recur = resolveInstances mode

    f ::
      Scope ->
      InstanceEnv ->
      Gamma ->
      ExprS (Indexed TypeU) ManyPoly Int ->
      MorlocMonad (Gamma, ExprS (Indexed TypeU) Many Int)

    -- CLS-2, CLS-16: the use's class arguments select one instance, whose
    -- implementations of the method are kept.
    f scope env g0 (VarS v (PolymorphicExpr cls method t0 rss)) = do
      let use = apply g0 gt
      c <- useConstraint scope g0 env cls method (etype t0) use
      case FI.resolve FI.usePolicy scope env [] c of
        Right (EvDerived _ arg) ->
          deriveComparison scope env g0 cls v >>= \case
            Just (g1, body) -> do
              counted second
              (g2, body') <- recur g1 body
              return (g2, VarS v (Many [EvDerived cls arg] [(Nothing, body')]))
            Nothing -> unresolved c (FI.NoInstance c [])
        Right ev -> do
          -- An instance that declares the method with no implementation is
          -- reported where the use is realized (CLS-14 (b)); one that does
          -- not declare it is reported here.
          (primary, casts) <- case FI.methodSources FI.defaultPolicy env ev v of
            Right x -> return x
            Left (FI.MethodOmitted key _)
              | any (\(k, _, _) -> k == key) rss -> return (key, [])
              | otherwise ->
                  let gtEval = evalOr scope use
                   in throwTypeError ci $
                        "The use of" <+> squotes (pretty v) <+> "at type" <+> prettyTypeU gtEval
                          <+> "selects the instance" <+> prettyConstraint (FI.keyConstraint key)
                          <> ", which gives no implementation of it"
                          <> noInstanceAdvice scope cls v gtEval
            Left (FI.NotAnInstance _) ->
              MM.throwCompilerBug $ "resolveInstances: the evidence for" <+> prettyConstraint c <+> "is not an instance"
          counted second
          let given = concat [rs | (k, _, rs) <- rss, k == primary]
              es0 = given <> concat [filter isRepresentationCast rs | (k, _, rs) <- rss, k `elem` casts]
              instanceTypes = [etype t | (k, t, _) <- rss, k == primary || k `elem` casts]
          g1 <-
            if null es0
              then connectInstanceTypes scope g0 use instanceTypes
              else return (connectTypes scope g0 use [t | AnnoS (Idx _ t) _ _ <- es0])
          (g2, es) <- statefulMapM recur g1 es0
          return (g2, VarS v (Many [ev] (map ((,) (evidenceInstance ev)) es)))
        Left e@(FI.Undetermined _ _) -> deferOr g0 (VarS v (Many [] [])) (unresolved c e)
        Left e -> unresolved c e
      where
        unresolved c e =
          let gtEval = evalOr scope (apply g0 gt)
              lead = case e of
                FI.Undetermined _ _ -> "The type of this term is not determined: the use of"
                _ -> "The use of"
           in throwTypeError ci $
                lead <+> squotes (pretty v) <+> "at type" <+> prettyTypeU gtEval
                  <+> "needs" <+> prettyConstraint c <> ":" <+> FI.explainResolveError prettyConstraint e
                  <> case e of
                    FI.Disagrees _ _ -> mempty
                    _ -> noInstanceAdvice scope cls v gtEval
    -- CLS-5: a use of a signed term at a type meets the term's constraints
    -- there. One nothing fixes yet is left to the uses inside the term.
    f scope env g0 (VarS v (MonomorphicExpr me xs)) = do
      cs <- case me of
        Just et | not (Set.null (econs et)) -> signatureConstraints scope g0 et (apply g0 gt)
        _ -> return []
      evs <- catMaybes <$> meet g0 cs
      statefulMapM recur g0 xs |>> second (VarS v . Many evs . map ((,) Nothing))
      where
        -- A selected instance fixes the dependent arguments it has, which
        -- other constraints may need (CLS-20), so a constraint nothing fixes
        -- yet waits for the rest.
        meet _ [] = return []
        meet gk pending =
          let resolved c = FI.resolve FI.usePolicy scope env [] (apply gk c)
              undetermined c = case resolved c of
                Left (FI.Undetermined _ _) -> True
                _ -> False
           in case break (not . undetermined) pending of
                (_, []) -> forM pending $ \c -> case resolved c of
                  Left e -> Nothing <$ deferOr g0 UniS (constraintUnmet (apply gk c) e)
                  Right _ -> MM.throwCompilerBug "resolved an undetermined constraint"
                (before, c : after) -> do
                  ev <- one (apply gk c) (resolved c)
                  (Just ev :) <$> meet (fixedBy gk (apply gk c) ev) (before <> after)
        one c (Right ev@(EvDerived cls arg)) = do
          derived <- derivedAt scope env cls arg
          if derived then ev <$ counted second else constraintUnmet c (FI.NoInstance c [])
        one _ (Right ev) = ev <$ counted second
        one c (Left e) = constraintUnmet c e
        fixedBy gk (Constraint _ args) (EvInstance key tys _) =
          let Constraint _ heads = FI.keyConstraintAt key tys
              gk' = foldl (+>) gk [v | ExistU v _ _ <- concatMap everyType args]
           in connectTypes scope gk' (FunU args BT.unitU) [FunU heads BT.unitU]
        fixedBy gk _ _ = gk
        constraintUnmet c e =
          throwTypeError ci $
            ( case e of
                FI.Undetermined _ _ -> "The type of this term is not determined: the use of"
                _ -> "The use of"
            ) <+> squotes (pretty v) <+> "at type" <+> prettyTypeU (evalOr scope (apply g0 gt))
              <+> "needs" <+> prettyConstraint c <> ":" <+> FI.explainResolveError prettyConstraint e
    -- Arguments first: an argument's instance may fix its result's type,
    -- which the function's own class arguments may need (CLS-20).
    f _ _ g0 (AppS e es) = do
      (g1, es') <- statefulMapM recur g0 es
      (g2, e') <- recur g1 e
      return (g2, AppS e' es')
    f _ _ g0 (LamS vs e) = recur g0 e |>> second (LamS vs)
    f _ _ g0 (LstS es) = statefulMapM recur g0 es |>> second LstS
    f _ _ g0 (TupS es) = statefulMapM recur g0 es |>> second TupS
    f _ _ g0 (NamS rs) =
      statefulMapM (\gk (k, x) -> second ((,) k) <$> recur gk x) g0 rs |>> second NamS
    f _ _ g0 (LetBndS v) = return (g0, LetBndS v)
    f _ _ g0 (LetS v e1 e2) = do
      (g1, e1') <- recur g0 e1
      (g2, e2') <- recur g1 e2
      return (g2, LetS v e1' e2')
    f _ _ g0 UniS = return (g0, UniS)
    f _ _ g0 NullS = return (g0, NullS)
    f _ _ g0 (BndS v) = return (g0, BndS v)
    f _ _ g0 (CallS v) = return (g0, CallS v)
    f _ _ g0 (RealS si x) = return (g0, RealS si x)
    f _ _ g0 (IntS si x) = return (g0, IntS si x)
    f _ _ g0 (LogS x) = return (g0, LogS x)
    f _ _ g0 (StrS x) = return (g0, StrS x)
    f _ _ g0 (ConS tv n i xs) = do
      (g1, xs') <- statefulMapM recur g0 xs
      return (g1, ConS tv n i xs')
    f _ _ g0 (ExeS x) = return (g0, ExeS x)
    f scope env g0 (PatS fg@(Idx _ ft) fc ev app) = do
      (g1, app') <- statefulTraverseM recur g0 app
      case (bracketRule (patAppPattern app), ft) of
        (Just (what, pos, methods), FunU args _)
          | length args > pos -> checkBracketReceiver scope fc what methods (args !! pos)
        _ -> return ()
      ev' <- patEvidence scope env g1 fc (apply g1 ft) app' ev
      return (g1, PatS fg fc ev' app')
    f _ _ g0 (DoBlockS e) = recur g0 e |>> second DoBlockS
    f _ _ g0 (EvalS e) = recur g0 e |>> second EvalS
    f _ _ g0 (CoerceS c e) = recur g0 e |>> second (CoerceS c)
    f _ _ g0 (LabelS k e) = recur g0 e |>> second (LabelS k)
    f _ _ _ (ShapeS {}) = MM.throwCompilerBug "a shape reached instance resolution inside its node"
    f _ _ g0 (IfS c t e) = do
      (g1, c') <- recur g0 c
      (g2, t') <- recur g1 t
      (g3, e') <- recur g2 e
      return (g3, IfS c' t' e')
    -- A map is a use of the class's @map@, unless the intrinsic does the same:
    -- the instance selected is the standard one over a plain list, or none
    -- implements it.
    f scope env g0 (IntrinsicS IntrMap [funcE, listE, use@(AnnoS (Idx _ useT) _ (VarS _ (PolymorphicExpr cls method t0 _)))]) = do
      (g1, funcE') <- recur g0 funcE
      (g2, listE') <- recur g1 listE
      c <- useConstraint scope g2 env cls method (etype t0) (apply g2 useT)
      library <- MM.asks configLibrary
      let intrinsic = return (g2, IntrinsicS IntrMap [funcE', listE'])
          method = do
            (g3, use') <- recur g2 use
            return (g3, AppS use' [funcE', listE'])
      case FI.resolve FI.usePolicy scope env [] c of
        Left (FI.NoInstance _ _) -> intrinsic
        Right ev | FI.intrinsicMapServes library env ev -> intrinsic
        _ -> method
    -- Inside an expansion of @map@ itself, the use is a back-edge to it.
    f _ _ g0 (IntrinsicS IntrMap [funcE, listE, use]) = do
      (g1, funcE') <- recur g0 funcE
      (g2, listE') <- recur g1 listE
      (g3, use') <- recur g2 use
      return (g3, AppS use' [funcE', listE'])
    f _ _ g0 (IntrinsicS intr es) = do
      (g1, es') <- statefulMapM recur g0 es
      return (g1, IntrinsicS intr es')

    -- CLS-2: what serves a pattern's receiver and casts its bracket bounds.
    -- What nothing has determined by the strict pass is an error (POLY-6).
    patEvidence ::
      Scope -> InstanceEnv -> Gamma -> Int -> TypeU -> PatApp x -> PatEvidence -> MorlocMonad PatEvidence
    patEvidence scope env g1 fc ft app old = case (app, ft) of
      -- A result that is a function extends the flattened inputs.
      (PatIndex _ _, FunU (i : r : _) _) -> at indexMethods r [i]
      (PatSlice {}, FunU (a : b : c : r : _) _) -> at sliceMethods r [a, b, c]
      (PatGet _ bs _, FunU ins _) | (bts, r : _) <- splitAt (length bs) ins -> at getterMethods r bts
      _ -> return old
      where
        at methods r bts = do
          rcv <-
            if any ((== Just BT.ifileVar) . headTVar) (TE.aliasChain scope r)
              then return Nothing
              else serve methods r
          PatEvidence rcv <$> mapM (serve boundMethods) bts
        serve methods t = case FI.resolveArg scope g1 env methods t of
          Right ev -> return ev
          Left e@(FI.Undetermined _ _) -> Nothing <$ deferOr g1 UniS (refuse "The type of this term is not determined:" t e)
          Left e -> refuse "This pattern" t e
        refuse lead t e =
          throwTypeError fc $
            lead <+> "the pattern on" <+> prettyTypeU t <+> "cannot select an instance:"
              <+> FI.explainResolveError prettyConstraint e

    -- A use nothing has determined yet: counted and left by a lenient pass,
    -- left in a generic tree, else reported.
    deferOr :: Gamma -> ExprS (Indexed TypeU) Many Int -> MorlocMonad (Gamma, ExprS (Indexed TypeU) Many Int) -> MorlocMonad (Gamma, ExprS (Indexed TypeU) Many Int)
    deferOr g0 placeholder err = case mode of
      Lenient _ -> counted first >> return (g0, placeholder)
      Strict True -> return (g0, placeholder)
      Strict False -> err

    counted :: ((Int -> Int) -> (Int, Int) -> (Int, Int)) -> MorlocMonad ()
    counted which = case mode of
      Lenient ref -> liftIO (modifyIORef' ref (which (+ 1)))
      Strict _ -> return ()

    -- DERIV-6: an Eq or Ord method at a type with no instance is the
    -- derived one.
    deriveComparison ::
      Scope -> InstanceEnv -> Gamma -> ClassName -> EVar -> MorlocMonad (Maybe (Gamma, AnnoS (Indexed TypeU) ManyPoly Int))
    deriveComparison scope env g0 cls (EV method)
      | Just methods <- derivedComparisons (unClassName cls)
      , Just w <- lookup method methods
      , use@(FunU (compared : _) _) <- apply g0 gt = do
          derived <- derivedAt scope env cls compared
          if not derived
            then return Nothing
            else do
              let (g1, x) = evarname g0 "x"
                  (g2, y) = evarname g1 "y"
              body <- derivedBody ci x y w
              lamIdx <- MM.getCounterWithPos ci
              (g3, _, typed) <- checkG g2 (AnnoS lamIdx lamIdx (LamS [x, y] body)) use
              return (Just (g3, typed))
      | otherwise = return Nothing

-- | The class constraint of a method use: the class's variables as the use's
-- type instantiates the method's type. No coercion is applied (CLS-17).
useConstraint :: Scope -> Gamma -> InstanceEnv -> ClassName -> EVar -> TypeU -> TypeU -> MorlocMonad Constraint
useConstraint scope g0 env cls v method use = case Map.lookup v (ieMethods env) of
  Nothing -> MM.throwCompilerBug $ "resolveInstances: no method" <+> pretty v
  Just (_, vars) -> do
    let (g1, t1, rw) = renameWithMap g0 method
    (g2, sub, body) <- return (instantiateAll g1 t1)
    case subtype scope use body g2 of
      Right g3 -> return (Constraint cls [apply g3 (sub (rw (VarU cv))) | cv <- vars])
      Left err ->
        MM.throwCompilerBug $
          "resolveInstances: the use type" <+> prettyTypeU use <+> "is not an instance of the method type"
            <+> prettyTypeU method <> ":" <+> err

-- | The constraints of a signature at a use of its term. A variable only a
-- constraint names is one a dependency determines (CLS-18, CLS-20): the use
-- leaves it open and the selected instance fixes it.
signatureConstraints :: Scope -> Gamma -> EType -> TypeU -> MorlocMonad [Constraint]
signatureConstraints scope g0 et use = do
  let (g1, et') = renameEType g0 et
      quantified = fst (unqualify (etype et'))
      dependent =
        List.nub
          [ v
          | Constraint _ ts <- Set.toList (econs et')
          , (v@(TV n), VarPlain) <- concatMap varForms ts
          , isTypeVariableName n
          , v `notElem` quantified
          ]
      (g2, sub, body) = instantiateAll g1 (etype et')
      owed = [(c, map (\t -> foldr substitute t dependent) ts) | Constraint c ts <- Set.toList (econs et')]
  case subtype scope use body g2 of
    _ | null owed -> return []
    Right g3 -> return [Constraint c (map (apply g3 . sub) ts) | (c, ts) <- owed]
    Left err ->
      MM.throwCompilerBug $
        "signatureConstraints: the use type" <+> prettyTypeU use <+> "is not an instance of the signature"
          <+> prettyTypeU (etype et) <> ":" <+> err

-- | Solve what the selected instance's implementations fix in the use's
-- type: a dependent class argument (CLS-20) and the method's own
-- variables.
connectTypes :: Scope -> Gamma -> TypeU -> [TypeU] -> Gamma
connectTypes scope g0 use = foldl (\g t -> either (const g) id (subtype scope (stripCoercionWrappers use) t g)) g0

-- | 'connectTypes' through instance types, each of their variables a fresh
-- existential.
connectInstanceTypes :: Scope -> Gamma -> TypeU -> [TypeU] -> MorlocMonad Gamma
connectInstanceTypes scope g0 use ts =
  let (g1, ts') = statefulMap (\g t -> let (g', t') = rename g t in toExistential g' t') g0 ts
   in return (connectTypes scope g1 use ts')

evalOr :: Scope -> TypeU -> TypeU
evalOr scope t = case TE.evaluateType scope t of
  Right et -> et
  Left _ -> t

-- | DERIV-6: Eq and Ord, the compiler's classes, are derived at a type that
-- is not a bare type variable.
derivableAt :: Scope -> ClassName -> TypeU -> MorlocMonad Bool
derivableAt scope cls t = case (derivedComparisons (unClassName cls), evalOr scope t) of
  (Nothing, _) -> return False
  (_, VarU (TV n)) | isTypeVariableName n -> return False
  (Just methods, _) -> do
    GMap _ sigmap <- MM.gets stateSignatures
    let declared = Set.fromList [v | Polymorphic c v _ <- Map.elems sigmap, c == cls]
    return (declared == Set.fromList [EV m | (m, _) <- methods])

-- | DERIV-6: whether the comparison of a class with no instance at @t@ is
-- derived there. Ord is derived only where Eq is too: not where a
-- hand-written Eq instance is selected.
derivedAt :: Scope -> InstanceEnv -> ClassName -> TypeU -> MorlocMonad Bool
derivedAt scope env cls t = do
  derivable <- derivableAt scope cls t
  return (derivable && not (unClassName cls == "Ord" && handWrittenEq))
  where
    handWrittenEq = case FI.resolve FI.defaultPolicy scope env [] (Constraint (ClassName "Eq") [t]) of
      Right ev@(EvInstance k _ _) ->
        let source = either (const k) fst (FI.methodSources FI.defaultPolicy env ev (EV "=="))
         in not (all isWireEq (concat [FI.declImpls (EV "==") d | d <- FI.instanceDecls env (ikClass k), idKey d == source]))
      _ -> False

-- | An implementation of @==@ that is @eq on its parameters, as a deriving
-- clause writes it.
isWireEq :: Impl -> Bool
isWireEq (ImplBody _ (ExprI _ (LamE [x, y] (ExprI _ (IntrinsicE IntrEq [ExprI _ (VarE _ x'), ExprI _ (VarE _ y')]))))) =
  x == x' && y == y'
isWireEq _ = False

-- | A derived comparison's body over its parameters @x@ and @y@, untyped,
-- each node at the use's source position.
derivedBody :: Int -> EVar -> EVar -> WireComparison -> MorlocMonad (AnnoS Int ManyPoly Int)
derivedBody site x y = go
  where
    node e = do
      i <- MM.getCounterWithPos site
      return (AnnoS i i e)
    go (WireCall intr swap) = do
      a <- node (BndS x)
      b <- node (BndS y)
      node (IntrinsicS intr (if swap then [b, a] else [a, b]))
    go (WireNot w) = do
      c <- go w
      t <- node (LogS True)
      e <- node (LogS False)
      node (IfS c e t)

-- | PAT-5: a bracket step's receiver is a list or an IFile, which every
-- back end brackets natively, or reaches some instance of the step's
-- classes. A receiver nothing has determined yet is left to code
-- generation (POLY-6).
checkBracketReceiver :: Scope -> Int -> MDoc -> [ArgMethod] -> TypeU -> MorlocMonad ()
checkBracketReceiver scope i what methods rcv = do
  env <- MM.gets stateInstances
  let heads = concatMap (FI.instanceReceiverHeads env) methods
  when (classifyBracketReceiver scope heads rcv == ReceiverNone) $
    throwTypeError i $
      "A bracket step needs a receiver with" <+> what <+> "instance, but"
        <+> prettyTypeU rcv <+> "has none"

-- | PAT-5, SIG-10: a bracket step on a receiver whose type is already known
-- in full is checked where it is written, before its shape is unified, so
-- the error names the missing instance. A receiver with a variable in it
-- waits for the check after instance resolution.
earlyBracketCheck :: Int -> Pattern -> TypeU -> MorlocMonad ()
earlyBracketCheck i p rcv = case bracketRule p of
  Just (what, _, methods)
    | null [() | (TV n, _) <- varForms rcv, isTypeVariableName n] -> do
        scope <- MM.getGeneralScope
        checkBracketReceiver scope i what methods rcv
  _ -> return ()

-- | A derived @pack@ or @unpack@: a lambda over a representation cast,
-- which no program can write. Instance bodies reach resolution typechecked,
-- so the cast is already a coercion.
isRepresentationCast :: AnnoS g f c -> Bool
isRepresentationCast (AnnoS _ _ (LamS _ (AnnoS _ _ (CoerceS (CoerceNewtype _ _) _)))) = True
isRepresentationCast (AnnoS _ _ (LamS _ (AnnoS _ _ (IntrinsicS intr _)))) =
  intr `elem` [IntrNewtypeUnwrap, IntrNewtypeWrap]
isRepresentationCast _ = False

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

-- | The field types of a constructor of the scrutinee's `data` type, with
-- its parameters instantiated. Rejecting a constructor of another type
-- keeps a clause set from mixing two `data` types.
checkCtorBelongs :: Int -> Gamma -> TypeU -> Text -> MorlocMonad [TypeU]
checkCtorBelongs i g subjectT n = do
  scope <- MM.getGeneralScope
  let resolved = apply g subjectT
  case lookup n =<< TE.dataCtorsAt scope (TE.whnf scope resolved) of
    Just fs -> return fs
    _ ->
      MM.throwSourcedError i $
        squotes (pretty n) <+> "is not a constructor of" <+> pretty resolved

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
-- A literal's type is a variable of its class (NUM-5), decided once the
-- definition or root holding it is.
synthE i g (RealS si x) = let (g', t) = literalVar i RealDefault g in return (g', t, RealS si x)
synthE i g (IntS si x) = let (g', t) = literalVar i IntDefault g in return (g', t, IntS si x)
synthE _ g (LogS x) = return (g, BT.boolU, LogS x)
synthE _ g (StrS x) = return (g, BT.strU, StrS x)
-- A constructor name is unique, so it alone determines its type. The
-- type's parameters become fresh existentials shared by the arguments and
-- the result, tying `Some x :: Opt a` to the `a` of `x`.
synthE i g0 (ConS tv n ord xs) = do
  scope <- MM.getGeneralScope
  let params = TE.dataParams scope tv
      (g1, freshArgs) = statefulMap (\g _ -> newvar (unTVar tv <> "_") g) g0 params
      resultT = case freshArgs of
        [] -> VarU tv
        _ -> AppU (VarU tv) freshArgs
  declaredArgs <- case lookup n =<< TE.dataCtorsOf scope tv freshArgs of
    Just as -> return as
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
                                return (g', x')) g1 (zipSame xs declaredArgs)
  return (g2, apply g2 resultT, ConS tv n ord xs')

-- A directly applied setter is a redex: reduce it so the setter's own
-- rule below sees the receiver. See 'reduceSetterRedex'.
synthE i g0 e | Just e' <- reduceSetterRedex e = synthE i g0 e'

synthE i g (PatS fgidx fcidx ev (PatText s xs)) = do
  (g1, _, es1, _) <- zipCheck i g (map fst xs) (map (const BT.strU) xs)
  let ft = FunU (map (const BT.strU) xs) BT.strU
      xs1 = [(e, t) | (e, (_, t)) <- zipSame es1 xs]
  return (g1, BT.strU, PatS (Idx fgidx ft) fcidx ev (PatText s xs1))

-- A selector with bracket steps, e.g. @.(.0.[i], .1) f@. The arguments
-- are the bracket bounds in DFS order, then the receiver.
synthE _ g0 (PatS fgidx fcidx ev (PatGet s bracketArgs@(_ : _) rcvArg)) = do
      let n = length bracketArgs
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
      return (gArgChecked, resolvedRetType, PatS (Idx fgidx ft) fcidx ev (PatGet s bracketArgsChecked rcvExpr))
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
synthE _ g0 (PatS fgidx fcidx ev (PatGet s [] e0)) = do
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
      return (g2, apply g2 retType, PatS (Idx fgidx ft) fcidx ev (PatGet s [] e0Syn))
    _ -> do
      rejectListSelectorTarget fgidx s rcvResolved
      (g1, datType) <- selectorType gsyn s
      retType <- return $ case selectorGetter datType s of
        [] -> error "Illegal empty selection"
        [t] -> t
        ts -> BT.tupleU ts
      let ft = FunU [datType] retType
      (g2, _, e') <- checkG g1 e0 datType
      rejectListSelectorTarget fgidx s (apply g2 datType)
      return (g2, apply g2 retType, PatS (Idx fgidx ft) fcidx ev (PatGet s [] e'))

-- Setter. The receiver is checked before the new values, so that
-- @.count = 42@ can fill an @Int32@ field rather than synthesize @Int@.
synthE i g0 (PatS fgidx fcidx ev (PatSet s e0 es0)) = do
  let (g1, slots) = mapAccumL (\g _ -> newvar "set_slot_" g) g0 es0
      slotVars = toList slots
  (g2, structType) <- selectorType g1 s
  outputType <- case selectorSetter slotVars s structType of
    Just t -> return t
    Nothing -> MM.throwSourcedError i "this setter's paths do not fit its receiver"
  (g3, datType, e1) <- checkG g2 e0 outputType
  rejectListSelectorTarget fgidx s (apply g3 datType)
  (g4, es1) <-
    statefulTraverseM
      (\g (e, slotT) -> do
         (g', _, e') <- checkG g e (apply g slotT)
         return (g', e'))
      g3
      (NE.zip es0 slots)
  let setTypes = map (apply g4) slotVars
      receiverType = apply g4 datType
      -- Built from the receiver, not the selector's structural type, to
      -- keep unnamed fields and the receiver's key order (the wire layout).
      resultType =
        fromMaybe (apply g4 outputType) (selectorSetter setTypes s receiverType)
      patternType = FunU (receiverType : setTypes) resultType
  return (g4, resultType, PatS (Idx fgidx patternType) fcidx ev (PatSet s e1 es1))

-- xs[i]. The index type stays open; codegen supplies the __to_index__
-- cast.
synthE _ g0 (PatS fgidx fcidx ev (PatIndex iExpr rcvExpr)) = do
  -- An IFile receiver is indexed through; the function's input keeps the
  -- IFile so codegen detects it.
  (gsyn, rcvSyn, rcvSynExpr) <- synthG g0 rcvExpr
  let rcvResolved = apply gsyn rcvSyn
  earlyBracketCheck fcidx PatternBracketIndex rcvResolved
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
      return (g5, resultType, PatS (Idx fgidx ft) fcidx ev (PatIndex i' rcvSynExpr))
    _ -> do
      let (g1, elemType) = newvar "bidx_elem_" gsyn
          (g2, fExist)   = newvar "bidx_f_" g1
          (g3, idxType)  = newvar "bidx_idx_" g2
          expectedRcv    = AppU fExist [elemType]
      g4 <- subtype' fgidx expectedRcv rcvResolved g3
      (g5, _, i')   <- checkG g4 iExpr idxType
      let resultType = apply g5 elemType
          ft = FunU [apply g5 idxType, apply g5 expectedRcv] resultType
      return (g5, resultType, PatS (Idx fgidx ft) fcidx ev (PatIndex i' rcvSynExpr))
-- xs[i:j:k]. Each bound gets its own bare existential; whether a bound is
-- absent is carried by NullS, not by the type. The result type is from
-- 'bracketSliceResultType'.
synthE _ g0 (PatS fgidx fcidx ev (PatSlice startE stopE stepE rcvExpr)) = do
  (g1, rcvType, rcv') <- synthG g0 rcvExpr
  let rcvType' = apply g1 rcvType
  earlyBracketCheck fcidx PatternBracketSlice rcvType'
  let (g1a, bareResultType) = bracketSliceResultType g1 rcvType'
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
  return (g7, resultType, PatS (Idx fgidx ft) fcidx ev (PatSlice s1 s2 s3 rcv'))
--   -->E0
synthE _ g (AppS f []) = do
  (g1, t1, f1) <- synthG g f
  return (g1, t1, AppS f1 [])

--   -->E
synthE i g0 (AppS f xs0) = do
  (g1, funType0, funExpr0) <- leaveLambda g0 <$$> synthG (appliedDefinition f (length xs0) g0) f
  overApplied i f xs0 (apply g1 funType0) (synthE i g0) $ do
    -- Literal arguments solve the variables their labels bind (m@Int).
    let g1' = observeResultNats f funType0 (resolveNatLabels f funType0 xs0 g1)
    etaExpandSynthE i g1' funType0 funExpr0 f xs0


-- -->I==>
synthE i g0 (LamS vs x) = do
  let (g1, paramTypes) = statefulMap (\g' v -> newvar (unEVar v <> "_x") g') (enterLambda g0) vs
      g2 = g1 ++> zipWithSame AnnG vs paramTypes

  (g3, bodyType, bodyExpr) <- synthG g2 x

  let funType = apply g3 (FunU paramTypes bodyType)
  noEscapedLengths i g0 g3 funType
  return (leaveLambda g0 g3, funType, LamS vs (applyGen g3 bodyExpr))

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
  (g1, xs) <- statefulMapM (\s (k, v) -> synthG s v |>> (\(a, b, c) -> (a, ((k, b), (k, c))))) g0 rs
  let (g2, t) = newvarRich ([], Closed) (map fst xs, Closed) "record_" g1
      e = NamS (map snd xs)
  return (g2, t, e)

-- A signed term referenced by its signature alone is instantiated, its
-- constraints becoming obligations. A reference's type never keeps its
-- quantifier: one left in place would meet an expected type inside a
-- record or tuple as a rigid variable.
synthE si g0 (VarS v (MonomorphicExpr (Just t0) []))
  | Just _ <- gammaAssumedConstraints g0 = do
      let (g1, t0') = renameEType g0 t0
          (g2, t2) = instantiateObligations si g1 (etype t0') (Set.toList (econs t0'))
      return (g2, t2, VarS v (MonomorphicExpr (Just t0) []))
-- A signature's variables are its own, quantified over it alone, even where
-- an enclosing signature writes the same name (POLY-4).
synthE vi g0 (VarS v (MonomorphicExpr (Just t0) xs0)) = do
  -- The type and its constraints are renamed together. The outermost VarS
  -- is the definition being checked: its constraints become the body's
  -- assumptions. It is the only VarS that finds gammaAssumedConstraints
  -- Nothing; after it the slot is Just, even for an empty list.
  let (g1, t0') = renameEType g0 t0
      t1 = etype t0'
      newCs = Set.toList (econs t0')
      inScope = Map.union (Map.fromList (zipSame (fst (unqualify (etype t0))) (fst (unqualify t1)))) (gammaScoped g0)
      natRen = Map.fromList (zipSame (List.nub (collectNatVarNames (etype t0))) (List.nub (collectNatVarNames t1)))
      -- A label whose Nat occurs nowhere else in the type still names one.
      labels =
        [ (x, Map.findWithDefault (TV (unTVar lv <> "@" <> unEVar v)) lv natRen, idx)
        | AnnoS _ _ (LamS vs _) <- xs0
        , (lv, idx) <- Map.toList (enatLabels t0)
        , not (Set.member lv (strLabelVars (etype t0)))
        , x <- take 1 (drop idx vs)
        ]
      labelVars = Map.fromList [(x, lv) | (x, lv, _) <- labels]
      -- A label on an unsigned type cannot be negative (KIND-4).
      unsignedLabels =
        [ IntVarU lv
        | (_, lv, idx) <- labels
        , VarU a <- take 1 (drop idx (fst (uncurryU t1)))
        , a `elem` [BT.u8, BT.u16, BT.u32, BT.u64, BT.uint]
        ]
  -- Against a signature, the body may also use the superclasses.
  given <- case gammaRigid g1 of
    Just _ -> superclassClosure newCs
    Nothing -> return newCs
  let outermost = isNothing (gammaAssumedConstraints g1)
      contract = isJust (gammaRigid g1)
      -- A Nat the function chooses (DIM-10) is not rigid in its body.
      allLabels = Map.elems labelVars <> [Map.findWithDefault lv lv natRen | lv <- Map.keys (enatLabels t0)]
      sigNats = List.nub (collectNatVarNames t1 <> Map.elems labelVars) List.\\ outwardNats t1 allLabels
      -- Checking against a signature, at any depth, its effect variables
      -- are rigid and its constraints are given to its body. Both stay in
      -- gamma after the body, over names no other signature uses.
      g1Base = g1
        { gammaConstraints = gammaConstraints g1 ++ filter isPrimitiveConstraint newCs
        , gammaWanteds = reverse [Wanted vi c False | c@(Constraint _ _) <- newCs] <> gammaWanteds g1
        , gammaAssumedConstraints = case gammaAssumedConstraints g1 of
            Nothing -> Just given
            Just old
              | contract -> Just (old <> given)
              | otherwise -> gammaAssumedConstraints g1
        , gammaRigid = case gammaRigid g1 of
            Just rigid -> Just (Set.union rigid (Set.fromList (collectEffVarNames t1)))
            r -> r
        }
      -- Every signature checked as a contract, at any depth, adds its Nat
      -- and Int variables and its domain (POLY-7, KIND-4).
      g1Cs
        | contract = g1Base
            { gammaRigidNats = Set.union (gammaRigidNats g1) (Set.fromList sigNats)
            , gammaRigidSlots = Set.union (gammaRigidSlots g1) (rigidSlotVars t1)
            , gammaRigidDomain = gammaRigidDomain g1 <> [(vi, p) | p <- natPositions t1 <> unsignedLabels]
            , gammaLabelVars = Map.union labelVars (gammaLabelVars g1)
            }
        | otherwise = g1Base {gammaLabelVars = Map.union labelVars (gammaLabelVars g1)}
      g1' = (g1Cs ++> [AnnG v t1]) {gammaScoped = inScope, gammaDefinitionLambda = v <$ ownLambdaArity xs0}
  (g2scoped, t2, xs1) <- foldCheck g1' xs0 t1
  let g2 = g2scoped {gammaScoped = gammaScoped g0, gammaDefinitionLambda = gammaDefinitionLambda g0}
  -- EFF-10: catches forces such as `f = !rint`, which subtyping cannot
  -- see since EvalS strips the effect. Reported at the body's index ci.
  mapM_ (\x@(AnnoS _ ci _) -> checkEffectCoverage ci (apply g2 t1) x) xs1
  let xs2 = applyCon g2 $ VarS v (MonomorphicExpr (Just t0) xs1)
      -- A local signed definition is checked as a contract with its
      -- variables rigid, but this node is also its use, which instantiates
      -- the signature afresh, constraints included (POLY-4).
      -- The variables of the two, each against its own, are recorded, so
      -- that a check finishing at a type can equate them ('checkedAt').
      (g3, tUse)
        | contract && not outermost =
            let (ga, t0'') = renameEType g2 t0
                (gb, tu) = instantiateObligations vi ga (etype t0'') (Set.toList (econs t0''))
                -- a length only the result holds is chosen at each use, so
                -- the use observes it (DIM-10)
                chosen = Set.fromList (outwardNats tu (Map.elems (enatLabelsRenamed t0 (etype t0''))))
             in ( gb { gammaNestedUses = signaturePairs t1 (etype t0'') <> gammaNestedUses gb
                     , gammaObserved = Set.union chosen (gammaObserved gb)
                     }
                , tu
                )
        | otherwise = (g2, t2)
  return (g3, tUse, xs2)
synthE _ g (VarS v (MonomorphicExpr Nothing (x : xs))) = do
  let (g0', freshT) = newvar (unEVar v <> "_rec") g
      g0'' = g0' ++> [AnnG v freshT]
  (g'rec, t', x') <- synthG g0'' x
  g' <- if any isBackEdge (annoNodes x') then recursiveUses g'rec t' x' freshT else return g'rec
  (g'', t'', xs') <- foldCheck g' xs t'
  let xs'' = applyCon g'' $ VarS v (MonomorphicExpr Nothing (x' : xs'))
  return (g'', t'', xs'')
  where
    isBackEdge (AnnoS _ _ (CallS w)) = w == v
    isBackEdge _ = False

    -- Without a signature, a recursive call is at the definition's own
    -- type. A call's effects may widen where it is used, so the call is
    -- held to the definition, not the reverse; the final call of a do-block
    -- is typed by its result's payload, and only there may the definition's
    -- result effect be dropped. Each call observes its own lengths: the Nat
    -- variables of the definition's type that neither a signature nor the
    -- enclosing scope fixes are renamed apart, so only the other parts are
    -- tied.
    recursiveUses g1 t' x' freshT = do
      scope <- MM.getGeneralScope
      let at = case x' of AnnoS (Idx gi _) _ _ -> gi
          defT = apply g1 t'
          enclosing = Set.fromList (concat [collectNatVarNames (apply g1 et) | AnnG w et <- IntMap.elems (gammaContext g), w /= v])
          renamable = [n | NatVarU n <- List.nub (everyType defT), Set.notMember n (gammaRigidNats g1), Set.notMember n enclosing]
          (g2, natRen) = statefulMap (\gx n -> second (\n' -> (n, NatVarU n')) (tvarname gx (unTVar n <> "_call"))) g1 renamable
          defT' = foldr (\(n, n') acc -> substituteTVar n n' acc) defT natRen
          useT = apply g2 freshT
          payload t = case t of
            FunU ts (EffectU _ r) -> FunU ts r
            EffectU _ r -> r
            _ -> t
          calls = length (filter isBackEdge (annoNodes x'))
          finalCalls = length (concatMap doFinal (annoNodes x'))
          doFinal (AnnoS _ _ (DoBlockS body)) = [() | isFinalCall body]
          doFinal _ = []
          isFinalCall (AnnoS _ _ e) = case e of
            LetS _ _ body -> isFinalCall body
            EvalS y -> isFinalCall y
            AppS f _ -> isBackEdge f
            _ -> False
      case subtype scope useT defT' g2 of
        Right gx -> return gx
        Left _
          | calls == finalCalls
          , Right gx <- subtype scope useT (payload defT') g2 -> return gx
        Left err -> throwTypeError at $ "A recursive use of" <+> squotes (pretty (displayName v)) <+> "is not at the type of its definition:" <+> err
synthE _ g (VarS v (MonomorphicExpr Nothing [])) = do
  let (g', t) = newvar (unEVar v <> "_u") g
  return (g', t, VarS v (MonomorphicExpr Nothing []))
-- Against a signature, a method use only records its constraint as an
-- obligation; the instance is not chosen here.
synthE mi g0 (VarS v (PolymorphicExpr cls clsName t0 rs0))
  | Just _ <- gammaRigid g0 = do
      methods <- MM.gets (ieMethods . stateInstances)
      let (g1, t1, rw) = renameWithMap g0 (etype t0)
          cvars = maybe [] snd (Map.lookup clsName methods)
          obligation = Constraint cls [rw (VarU cv) | cv <- cvars]
          (g2, t2) = instantiateObligations mi g1 t1 [obligation]
      return (g2, t2, VarS v (PolymorphicExpr cls clsName t0 [(k, et, []) | (k, et, _) <- rs0]))
synthE i g0 (VarS v (PolymorphicExpr cls clsName t0 rs0)) = do
  (g1, rsChecked) <- checkInstances g0 (etype t0) rs0
  let (g2, t1) = rename g1 (etype t0)
  return (g2, t1, VarS v (PolymorphicExpr cls clsName t0 rsChecked))
  where
    -- Each instance is checked independently; the context is trimmed
    -- between instances to keep it from growing quadratically, unless a body
    -- left a use only a later use of the method can determine (CLS-20): its
    -- existentials are solved then.
    checkInstances ::
      Gamma ->
      TypeU ->
      [(InstanceKey, EType, [AnnoS Int ManyPoly Int])] ->
      MorlocMonad (Gamma, [(InstanceKey, EType, [AnnoS (Indexed TypeU) ManyPoly Int])])
    checkInstances g _ [] = return (g, [])
    checkInstances g10 genType ((key, instType, es) : rs) = do
      let (g11, genType') = toExistential g10 genType
      let (g12, instType') = rename g11 (etype instType)
      g13 <- subtype' i instType' genType' g12
      -- The ExistG entries made so far must survive the trim:
      -- connectInstance solves through them.
      let slotAfterSubtype = gammaSlot g13
      (g14, es', settled) <- checkImplementations g13 genType' es

      let gNext = if settled then gammaTrimAfter slotAfterSubtype g14 else g14

      -- The original general type: each instance gets its own existentials.
      (g15, rs') <- checkInstances gNext genType rs

      return (g15, (key, instType, es') : rs')

    checkImplementations ::
      Gamma ->
      TypeU ->
      [AnnoS Int ManyPoly Int] ->
      MorlocMonad (Gamma, [AnnoS (Indexed TypeU) ManyPoly Int], Bool)
    checkImplementations g _ [] = return (g, [], True)
    checkImplementations g10 t (e@(AnnoS implGi _ _) : es) = do
      -- An annotation copied from the use site (`mempty :: Str`) must not
      -- constrain each implementation (the List instance's `[]`).
      implAnn <- MM.gets (Map.lookup implGi . stateAnnotations)
      MM.modify' (\s -> s { stateAnnotations = Map.delete implGi (stateAnnotations s) })
      (g11raw, _, e') <- checkG g10 e t
      case implAnn of
        Just ann' -> MM.modify' (\s -> s { stateAnnotations = Map.insert implGi ann' (stateAnnotations s) })
        Nothing -> return ()
      -- What the body's own uses fix is solved while its gamma holds them
      -- (CLS-20). The body is resolved again, and its errors reported, at
      -- each use that selects the instance.
      (g11, settled) <- MM.recoverRejected
        ( lenientPasses g11raw e' |>> \(g, resolved) ->
            (g {gammaDeferred = gammaDeferred g11raw, gammaDeferredAt = gammaDeferredAt g11raw}, isJust resolved)
        )
        (return (g11raw, False))

      (g12, es', settledRest) <- checkImplementations g11 t es

      return (g12, applyGen g12 e' : es', settled && settledRest)

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
    (Just t) | Just _ <- gammaRigid g -> return (observeRecursive g t)
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
synthE _ g (ShapeS w sh r) = do
  (g1, t1, r1) <- synthG g r
  return (g1, t1, ShapeS w sh r1)
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
--
-- A third argument is the use of @map@ the map becomes (Treeify), checked
-- here at the map's own type, so that the chain owes the class constraint a
-- written @map@ owes (CLS-5).
synthE _ g (IntrinsicS IntrMap (funcE : listE : mapUse)) = do
  let (g1, a) = newvar "map_elem_"      g
      (g2, b) = newvar "map_result_"    g1
      (g3, f) = newvar "map_container_" g2
      listExpectedT = AppU f [a]
      resultT       = AppU f [b]
  (g4, _, listE') <- checkG g3 listE listExpectedT
  let funcExpectedT = FunU [apply g4 a] (apply g4 b)
  (g5, _, funcE') <- checkG g4 funcE funcExpectedT
  case mapUse of
    [use] -> do
      (g6, _, use') <- checkG g5 use (apply g5 (FunU [funcExpectedT, listExpectedT] resultT))
      let added = length (gammaWanteds g6) - length (gammaWanteds g5)
          g7 = g6 {gammaWanteds = [w {wIntrinsic = True} | w <- take added (gammaWanteds g6)] <> drop added (gammaWanteds g6)}
      return (g7, apply g7 resultT, IntrinsicS IntrMap [funcE', listE', use'])
    _ -> return (g5, apply g5 resultT, IntrinsicS IntrMap [funcE', listE'])
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
synthE i _ (IntrinsicS IntrNewtypeUnwrap args) =
  MM.throwCompilerBugAt i $ "IntrNewtypeUnwrap expects one argument, got " <> pretty (length args)
synthE i _ (IntrinsicS IntrNewtypeWrap _) =
  MM.throwSourcedError i "A newtype wrap needs the newtype it builds from its context."
-- The intrinsics only the compiler writes ('viewInternal').
--
-- A tag test carries the constructor's NAME, not a reference to the term the
-- declaration bound: a payload constructor is a function into its type, so it
-- could never be checked as a value of the type being tested. The name is
-- checked against the scrutinee's own constructor table instead. A field read
-- follows a tag test that fixed the constructor; a list element read follows
-- a length test, and both check the subject against @List a@ itself, so a list
-- pattern rejects every other container.
synthE i g (IntrinsicS intr args)
  | Malformed <- viewInternal intr args =
      MM.throwCompilerBugAt i $ "malformed" <+> pretty (show intr) <+> "intrinsic"
  | Internal app <- viewInternal intr args = case app of
      TagTest subjectE nameE n -> do
        (g1, subjectT, subjectE') <- synthG g subjectE
        _ <- checkCtorBelongs i g1 subjectT n
        (g2, _, nameE') <- synthG g1 nameE
        return (g2, BT.boolU, IntrinsicS intr [subjectE', nameE'])
      CtorField subjectE nameE n idxE idx -> do
        (g1, subjectT, subjectE') <- synthG g subjectE
        fieldTs <- checkCtorBelongs i g1 subjectT n
        (g2, _, nameE') <- synthG g1 nameE
        (g3, _, idxE') <- synthG g2 idxE
        case drop idx fieldTs of
          (t : _) -> return (g3, apply g3 t, IntrinsicS intr [subjectE', nameE', idxE'])
          [] ->
            MM.throwSourcedError i $
              "constructor" <+> squotes (pretty n) <+> "has no field" <+> pretty (show idx)
      ListLenTest subjectE lenE _ -> do
        let (g1, a) = newvar "listpat_elem_" g
        (g2, _, subjectE') <- checkG g1 subjectE (BT.listU a)
        (g3, _, lenE') <- synthG g2 lenE
        return (g3, BT.boolU, IntrinsicS intr [subjectE', lenE'])
      ListElem subjectE idxE -> do
        let (g1, a) = newvar "listpat_elem_" g
        (g2, _, subjectE') <- checkG g1 subjectE (BT.listU a)
        (g3, _, idxE') <- synthG g2 idxE
        return (g3, apply g3 a, IntrinsicS intr [subjectE', idxE'])
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
synthE i _ (IntrinsicS IntrWrite args) =
  MM.throwCompilerBugAt i $ "IntrWrite expects 3 args (level, handle, list), got " <> pretty (length args)
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
synthE i _ (IntrinsicS IntrCellNew args) =
  MM.throwCompilerBugAt i $ "IntrCellNew expects 1 arg (init), got " <> pretty (length args)
synthE _ g (IntrinsicS IntrCellGet [handleE]) = do
  (g1, b, handleE') <- checkCellHandle g handleE
  return ( g1
         , EffectU ioEffectSet b
         , IntrinsicS IntrCellGet [handleE']
         )
synthE i _ (IntrinsicS IntrCellGet args) =
  MM.throwCompilerBugAt i $ "IntrCellGet expects 1 arg (handle), got " <> pretty (length args)
synthE _ g (IntrinsicS IntrCellPut [handleE, valE]) = do
  (g2, b, handleE') <- checkCellHandle g handleE
  (g3, _, valE')    <- checkG g2 valE b
  return ( g3
         , EffectU ioEffectSet BT.unitU
         , IntrinsicS IntrCellPut [handleE', valE']
         )
synthE i _ (IntrinsicS IntrCellPut args) =
  MM.throwCompilerBugAt i $ "IntrCellPut expects 2 args (handle, value), got " <> pretty (length args)
synthE _ g (IntrinsicS IntrCellReduce [combineE, handleE]) = do
  (g2, bT, handleE') <- checkCellHandle g handleE
  (g3, _, combineE') <- checkG g2 combineE (FunU [bT, bT] bT)
  return ( g3
         , EffectU ioEffectSet (apply g3 bT)
         , IntrinsicS IntrCellReduce [combineE', handleE']
         )
synthE i _ (IntrinsicS IntrCellReduce args) =
  MM.throwCompilerBugAt i $ "IntrCellReduce expects 2 args (combine, handle), got " <> pretty (length args)
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
  MM.throwCompilerBugAt i $ "@stream expects 1 arguments but got" <+> pretty (length args)
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
  MM.throwCompilerBugAt i $ "@next expects 1 arguments but got" <+> pretty (length args)
synthE i _ (IntrinsicS IntrDatafile [pathE])
  | not (isStaticStr pathE) =
      MM.throwSourcedError i
        "the argument of @datafile must be a string literal, since the path is resolved when the program is built"
synthE i g (IntrinsicS intr args) = do
  (g', argTypes, args') <- synthArgs g args
  g'' <- checkIntrinsicArgs i g' intr argTypes
  case intrinsicTypeG g'' intr argTypes of
    Just (g''', expectedType) -> return (g''', expectedType, IntrinsicS intr args')
    Nothing -> MM.throwCompilerBugAt i $ pretty (show intr) <+> "reached the generic intrinsic rule"

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
intrinsicTypeG :: Gamma -> Intrinsic -> [TypeU] -> Maybe (Gamma, TypeU)
-- @load :: Str -> <IO> (Try Str a)
intrinsicTypeG g IntrLoad _ =
  let (g', loadType) = newvar "load_" g
  in Just (g', EffectU ioEffectSet (BT.tryU BT.strU loadType))
-- @read :: Str -> Try Str a. A JSON parse touches nothing, so with
-- failure carried in the result it is pure and composes in `map`.
intrinsicTypeG g IntrRead _ =
  let (g', readType) = newvar "read_" g
  in Just (g', BT.tryU BT.strU readType)
-- @open: the return type comes from an ascription
-- (`@open path :: IFile Sequence`).
intrinsicTypeG g IntrOpen _ =
  let (g', openType) = newvar "open_" g
  in Just (g', EffectU ioEffectSet (BT.tryU BT.strU openType))
-- @unpack: the decoded type is resolved by the synthesized ascription.
intrinsicTypeG g IntrUnpack _ =
  let (g', unpackType) = newvar "unpack_" g
  in Just (g', EffectU ioEffectSet unpackType)
intrinsicTypeG g IntrClose _ = Just (g, EffectU ioEffectSet BT.unitU)
-- @append: the return type comes from an ascription, as for @open.
intrinsicTypeG g IntrAppend _ =
  let (g', appendType) = newvar "append_" g
  in Just (g', EffectU ioEffectSet (BT.tryU BT.strU appendType))
-- @stdin can fail (a second open); @stdout and @stderr cannot.
intrinsicTypeG g IntrStdin _ =
  let (g', a) = newvar "stdin_a" g in
  Just (g', EffectU ioEffectSet (BT.tryU BT.strU (AppU (VarU BT.istreamVar) [a])))
intrinsicTypeG g IntrStdout _ =
  let (g', a) = newvar "stdout_a" g in
  Just (g', EffectU ioEffectSet (AppU (VarU BT.ostreamVar) [a]))
intrinsicTypeG g IntrStderr _ =
  let (g', a) = newvar "stderr_a" g in
  Just (g', EffectU ioEffectSet (AppU (VarU BT.ostreamVar) [a]))
-- @throw :: Str -> a. A bottom, not a tracked effect.
intrinsicTypeG g IntrThrow _ =
  let (g', a) = newvar "throw_" g
  in Just (g', a)
intrinsicTypeG g intr _ = (,) g <$> intrinsicType intr

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

-- | The type of a fully applied intrinsic that needs no fresh variables;
-- Nothing for one typed by its own rule.
intrinsicType :: Intrinsic -> Maybe TypeU
intrinsicType IntrSave = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.unitU))
intrinsicType IntrSaveJ = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.unitU))
intrinsicType IntrLoad = Nothing
intrinsicType IntrHash = Just (BT.strU)
intrinsicType IntrEq = Nothing
intrinsicType IntrLe = Nothing
intrinsicType IntrVersion = Just (BT.strU)
intrinsicType IntrCompiled = Just (BT.strU)
intrinsicType IntrLang = Just (BT.strU)
intrinsicType IntrSchema = Just (BT.strU)
intrinsicType IntrTypeof = Just (BT.strU)
intrinsicType IntrShow = Just (BT.strU)
intrinsicType IntrRead = Nothing
intrinsicType IntrDatafile = Just (BT.strU)
intrinsicType IntrUnpack = Nothing
intrinsicType IntrOpen = Nothing
intrinsicType IntrClose = Nothing
intrinsicType IntrFSchema = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.strU))
intrinsicType IntrFLength = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.intU))
intrinsicType IntrStreamLayout = Just (EffectU ioEffectSet (BT.tryU BT.strU (BT.listU (BT.tupleU [BT.u64U, BT.u64U, BT.u64U]))))
intrinsicType IntrTell = Just (EffectU ioEffectSet BT.u64U)
intrinsicType IntrTmpfile = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.strU))
intrinsicType IntrNext = Nothing
intrinsicType IntrStream = Nothing
intrinsicType IntrWrite = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.unitU))
-- One clause each, not a guard over a list: this match is exhaustive on
-- purpose, so that a new Intrinsic constructor fails the build here rather
-- than falling through to a runtime error.
intrinsicType IntrCellNew = Nothing
intrinsicType IntrCellGet = Nothing
intrinsicType IntrCellPut = Nothing
intrinsicType IntrCellReduce = Nothing
intrinsicType IntrReplay = Nothing
intrinsicType IntrChannel = Nothing
intrinsicType IntrSpawn = Nothing
intrinsicType IntrSettle = Nothing
intrinsicType IntrAppend = Nothing
intrinsicType IntrConcat = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.unitU))
intrinsicType IntrFlush = Just (EffectU ioEffectSet (BT.tryU BT.strU BT.unitU))
intrinsicType IntrMap = Nothing
intrinsicType IntrMapOptional = Nothing
intrinsicType IntrTagTest = Nothing
intrinsicType IntrNewtypeUnwrap = Nothing
intrinsicType IntrNewtypeWrap = Nothing
intrinsicType IntrCtorField = Nothing
intrinsicType IntrListLenTest = Nothing
intrinsicType IntrListElem = Nothing
intrinsicType IntrStdin = Nothing
intrinsicType IntrStdout = Nothing
intrinsicType IntrStderr = Nothing
intrinsicType IntrThrow = Nothing
intrinsicType IntrTry = Nothing
intrinsicType IntrCollect = Nothing
intrinsicType IntrIFileWalk = Nothing
intrinsicType IntrConform = Nothing
intrinsicType IntrCellSlots = Nothing
intrinsicType IntrReduceList = Nothing
intrinsicType IntrShape = Nothing

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
  let actual = length argTypes
  if maybe False (/= actual) (intrinsicArity intr)
    then MM.throwCompilerBugAt i $
      "@" <> pretty (intrinsicName intr) <+> "expects" <+> pretty (intrinsicArity intr)
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

-- | A fresh variable for the type of the literal at @i@, held for its
-- class.
literalVar :: Int -> NumLitKind -> Gamma -> (Gamma, TypeU)
literalVar i k g = case newvar "lit" g of
  (g', t@(ExistU v _ _)) -> (deferNumLit (i, v, k) g', t)
  r -> r

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

-- | APP-4: an application with more arguments than its head's type shows,
-- where that type's result is still open (an existential, or a variable it
-- quantifies), applies the result to the rest: @f xs1 xs2@ is checked as
-- @(f xs1) xs2@ from the context before the head was synthesized. A result
-- that is a rigid variable or a suspension takes no more arguments, and the
-- application is checked as written.
overApplied ::
  Int ->
  AnnoS Int ManyPoly Int ->
  [AnnoS Int ManyPoly Int] ->
  TypeU ->
  (ExprS Int ManyPoly Int -> MorlocMonad r) ->
  MorlocMonad r ->
  MorlocMonad r
overApplied i f xs funType0 again asWritten = case openResultAfter (normalizeType funType0) of
  Just n | length xs > n, n > 0 -> do
    let (now, later) = splitAt n xs
    k <- MM.getCounterWithPos i
    again (AppS (AnnoS k k (AppS f now)) later)
  _ -> asWritten

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

  case normalizeType (snd (unqualify (apply g1 funType0))) of
    FunU (length -> numParams) _
      | numArgs > numParams ->
          throwTypeError i $ "Invalid function application of type:\n  " <> prettyTypeU funType0
    _ -> return ()

  -- A literal argument is decided once the whole definition is (NUM-5).
  (g2, funType1, inputExprs) <- application' i g1 xs0 normalType

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
            g4 = g3 ++> map (uncurry AnnG) newVarsWithTypes

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

-- | The parameters of a function type whose result is still open: an
-- existential, or a variable the type quantifies (instantiated to one when
-- applied). 'Nothing' for any other result.
openResultAfter :: TypeU -> Maybe Int
openResultAfter = go Set.empty
  where
    go bound (ForallU v t) = go (Set.insert v bound) t
    go bound (FunU ps r) = case r of
      ExistU _ ([], _) ([], _) -> Just (length ps)
      VarU v | Set.member v bound -> Just (length ps)
      _ -> Nothing
    go _ _ = Nothing

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
zipCheck i _ xs [] =
  throwTypeError i $ "a function is applied to" <+> pretty (length xs) <+> "more argument" <> (if length xs == 1 then "" else "s") <+> "than it takes"

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
      let g1 = enterLambda g0 ++> zipWithSame AnnG vs as
      (g2, t2, e2) <- checkG g1 body b

      let t3 = apply g2 (FunU as t2)
          e3 = applyCon g2 (LamS vs e2)
      noEscapedLengths i g0 g2 t3
      return (leaveLambda g0 g2, t3, e3)
  -- Fewer parameters than arguments. Not eta-expanded: that would move the
  -- body's work from each application of this lambda to each call of the
  -- function it returns.
  | length vs < length as = do
      let (asHere, asRest) = splitAt (length vs) as
          g1 = enterLambda g0 ++> zipWithSame AnnG vs asHere
      (g2, t2, e2) <- checkG g1 body (FunU asRest b)
      let t3 = apply g2 (FunU asHere t2)
      noEscapedLengths i g0 g2 t3
      return (leaveLambda g0 g2, t3, applyCon g2 (LamS vs e2))
  -- More parameters than this arrow group has arguments, and the result is
  -- the next group: one type (APP-1), checked as one lambda.
  | FunU bs c <- b, not (null as) = checkE i g0 (LamS vs body) (FunU (as <> bs) c)
  -- More parameters than this arrow group has arguments (@FunU [A] (FunU
  -- [B] C)@): the surplus parameters are checked against the return type,
  -- as 'subtype' does. Each step consumes at least one parameter, so this
  -- terminates; an eta-expansion here would add parameters and never end.
  | not (null as) = do
      let (vsHere, vsRest) = splitAt (length as) vs
          g1 = g0 ++> zipWithSame AnnG vsHere as
      (g2, t2, e2) <- checkG g1 (AnnoS i i (LamS vsRest body)) b
      let t3 = apply g2 (FunU as t2)
          e3 = applyCon g2 (LamS vsHere e2)
      noEscapedLengths i g0 g2 t3
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
  tier2 <- if tier1Ok then return Nothing else (\env -> FI.packableWireForm scope env bApplied) <$> MM.gets stateInstances
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
      -- an optional of a numeric type takes the literal as its payload,
      -- coerced (OPT-7)
      OptionalU inner | acceptable (apply g inner) -> reconcileSynth i g inner (IntS si x) t
      _ -> reconcileSynth i g BT.intU (IntS si x) t
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
      OptionalU inner | BT.isRealBaseType (apply g inner) -> reconcileSynth i g inner (RealS si x) t
      _ -> reconcileSynth i g BT.realU (RealS si x) t
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
checkE i g0 e@(PatS fgidx fcidx ev (PatSet s e0 es0)) t = do
      let (g1, slots) = mapAccumL (\g _ -> newvar "set_slot_" g) g0 es0
          slotVars = toList slots
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
            statefulTraverseM
              (\g (e', slotT) -> do
                 (g'', _, e'') <- checkG g e' (apply g slotT)
                 return (g'', e''))
              g4
              (NE.zip es0 slots)
          let setTypes = map (apply g5) slotVars
              receiverType = apply g5 datType
              resultType =
                fromMaybe (apply g5 outputType) (selectorSetter setTypes s receiverType)
              patternType = FunU (receiverType : setTypes) resultType
          return (g5, resultType, PatS (Idx fgidx patternType) fcidx ev (PatSet s e1 es1))
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
  (g1, funType0, funExpr0) <- leaveLambda g0 <$$> synthG (appliedDefinition f (length xs) g0) f
  overApplied i f xs (apply g1 funType0) (\e -> checkE i g0 e t) $ do
    let g1' = observeResultNats f funType0 (resolveNatLabels f funType0 xs g1)
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
  -- A numeric type is never optional, so a value of an unknown numeric type
  -- expected as an optional is of the payload type, coerced (OPT-7, NUM-10).
  case (a', b') of
    (ExistU v ([], _) ([], _), OptionalU _)
      | numericExist g2 v
      , Just (coercions, g3) <- tryCoerce scope a' b' g2 -> coerced a' b' g3 coercions
    _ -> reconciled scope a' b'
  where
    coerced a' b' g3 coercions = do
      (finalExpr, _) <- foldlM
        (\(expr, currentType) coercion -> do
          idx <- MM.getCounterWithPos i
          let wrappedAnno = AnnoS (Idx idx currentType) i expr
          return (CoerceS coercion wrappedAnno, applyCoercion coercion currentType))
        (e2, apply g3 a')
        coercions
      return (g3, apply g3 b', finalExpr)
    reconciled scope a' b' =
      case subtype scope a' b' g2 of
      -- Between two record existentials InstLReach solves only the literal's,
      -- so the expected side can keep a narrower key set.
      Right g3 ->
        let aApplied = apply g3 a'
            bApplied = apply g3 b'
        in return (g3, pickWiderRecord aApplied bApplied, e2)
      Left err ->
        case tryCoerce scope a' b' g2 of
          Just (coercions, g3) -> coerced a' b' g3 coercions
          Nothing ->
            MM.throwSourcedError i $
              "Type mismatch:"
              <> line <> "  expected: " <> prettyTypeU b'
              <> line <> "  inferred: " <> prettyTypeU a'
              <> line <> err
              <> missingInstanceHint scope a' b'

-- | An unsolved existential a numeric literal is pending at, or that an
-- @Integral@ or @Numeric@ wanted constrains.
numericExist :: Gamma -> TVar -> Bool
numericExist g v =
  any (\(_, w, _) -> endsAt w) (gammaPendingNumLits g)
    || any
      ( \c -> case apply g c of
          Constraint cls [ExistU w _ _] -> w == v && (cls == BT.integralClass || cls == BT.numericClass)
          _ -> False
      )
      (wantedConstraints g)
  where
    endsAt w = case literalEnd g w of
      ExistU w' _ _ -> w' == v
      _ -> False

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
      (AnnoS _ _ (LamS [v] (AnnoS _ _ (PatS gi ci ev (PatSet s (AnnoS _ _ (BndS v')) values)))))
      [receiver]
    )
    | v == v' = Just (PatS gi ci ev (PatSet s receiver values))
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
conditionGroups xs = map (map snd . List.sortOn fst) (foldl insert [] (indexed xs))
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
    hasExistHead (ExistU _ ([], _) ([], _)) = True
    hasExistHead _                       = False

-- | The Nat arguments of a type (@Matrix 2 4 Int@ gives @[2, 4]@), taken
-- from the deepest alias level that still has any:
-- @type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int@ is dimensioned by
-- @n + m@.
getNatArgs :: Scope -> TypeU -> [TypeU]
getNatArgs scope (AppU (VarU v) args) =
  case Map.lookup v scope of
    Just ((params, _, _, _, _) : _) ->
      let myNatArgs = [arg | (Left (_, KindNat), arg) <- zipSame params args]
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
      PatS _ _ _ app -> unions (map go (toList app))
      TupS es -> unions (map go es)
      LstS es -> unions (map go es)
      NamS rs -> unions (map (go . snd) rs)
      LamS _ e -> go e
      IfS c t e -> go c `effUnion` go t `effUnion` go e
      DoBlockS e -> go e
      CoerceS _ e -> go e
      LabelS _ e -> go e
      ShapeS _ _ r -> go r
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
      PatS _ _ _ app -> unions (map go (toList app))
      TupS es -> unions (map go es)
      LstS es -> unions (map go es)
      NamS rs -> unions (map (go . snd) rs)
      IfS c t e -> go c `effUnion` go t `effUnion` go e
      CoerceS _ e -> go e
      LabelS _ e -> go e
      ShapeS _ _ r -> go r
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
      PatS _ _ _ app -> concatMap go app
      TupS es -> concatMap go es
      LstS es -> concatMap go es
      NamS rs -> concatMap (go . snd) rs
      IfS c t e -> go c ++ go t ++ go e
      CoerceS _ e -> go e
      LabelS _ e -> go e
      ShapeS _ _ r -> go r
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
        (Left (Rejected Unplaced e)) -> MM.throwSourcedError m e
        (Left (Fault Nothing e)) -> MM.throwError (Fault (Just (AtNode m)) e)
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
tryEvalConst g (PatS _ _ _ (PatGet (SelectorIdx (idx, SelectorEnd) []) [] (AnnoS _ _ inner)))
  = case tryEvalConst g inner of
      Just (ConstTup vs) | idx >= 0, idx < length vs -> Just (vs !! idx)
      _ -> Nothing
tryEvalConst g (AppS (AnnoS _ _ (LamS vs (AnnoS _ _ body))) args)
  | length vs == length args = do
    vals <- mapM (\(AnnoS _ _ e) -> tryEvalConst g e) args
    let g' = g { gammaIntVals = foldl (\m (v', val') -> Map.insert v' val' m) (gammaIntVals g) (zipSame vs vals) }
    tryEvalConst g' body
tryEvalConst g (ShapeS _ _ (AnnoS _ _ e)) = tryEvalConst g e
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
        natRenMap = Map.fromList (zipSame origNvs renamedNvs)
        natSolutions = Map.fromList
          [ (renamedVar, n)
          | (origVar, arg) <- labelArguments et args
          , Just renamedVar <- [Map.lookup origVar natRenMap]
          , Just n <- [labelValue arg]
          ]
        origSvs = nub (collectStrVarNames (etype et))
        renamedSvs = nub (collectStrVarNames funType)
        strRenMap = Map.fromList (zipSame origSvs renamedSvs)
        strSolutions = Map.fromList
          [ (renamedVar, StrLitU s)
          | (origVar, arg) <- labelArguments et args
          , Just renamedVar <- [Map.lookup origVar strRenMap]
          , Just s <- [tryExtractStrPre g arg]
          ]
        origLvs = nub (collectListVarNames (etype et))
        renamedLvs = nub (collectListVarNames funType)
        listRenMap = Map.fromList (zipSame origLvs renamedLvs)
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
        -- A label given no value before run time stands for every value.
        observed = Set.fromList
          [ renamedVar
          | (origVar, arg) <- labelArguments et args
          , Set.notMember origVar (strLabelVars (etype et))
          , Just renamedVar <- [Map.lookup origVar natRenMap]
          , Nothing <- [labelValue arg]
          ]
    in g { gammaKindSubs = Map.union combined (gammaKindSubs g)
         , gammaObserved = Set.union observed (gammaObserved g)
         }
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

-- | The Nats of a type, each with whether it stands outward (DIM-10): a
-- function's arguments take the opposite of its position.
natPolarity :: TypeU -> [(Bool, TVar)]
natPolarity = natsAt True
  where
    natsAt out x = case x of
      FunU ts r -> concatMap (natsAt (not out)) ts <> natsAt out r
      NatVarU v -> [(out, v)]
      IntVarU v -> [(out, v)]
      _ -> concatMap (natsAt out) (typeUChildren x)

-- | The Nats of a signature the function chooses (DIM-10): those, other
-- than the given label variables, that occur only outward.
outwardNats :: TypeU -> [TVar] -> [TVar]
outwardNats t labelled =
  let occs = natPolarity t
   in List.nub [v | (True, v) <- occs, (False, v) `notElem` occs, v `notElem` labelled]

-- | The Nats of a type with an inward occurrence: those its caller gives.
inwardNats :: TypeU -> [TVar]
inwardNats t = List.nub [v | (False, v) <- natPolarity t]

-- | A recursive call instantiates its definition's signature afresh. The
-- lengths it chooses are observed there (DIM-10), and so are its labelled
-- ones, since a recursive call's label arguments are not read.
observeRecursive :: Gamma -> TypeU -> (Gamma, TypeU)
observeRecursive g t =
  let (g1, t1) = rename g t
   in (g1 {gammaObserved = Set.union (Set.fromList (outwardNats t1 [] <> labelled t1)) (gammaObserved g1)}, t1)
  where
    labelled t1 = [v | IntVarU v <- universe t1]
    universe x = x : concatMap universe (typeUChildren x)

-- | A function that chooses a length (DIM-10) chooses it again at each
-- application, so one type can stand for its result only where it is
-- applied to all its arguments: named alone or partially applied, it would
-- give every later application one length. A signed, method or exported
-- definition is applied once per call at the root of its body (through
-- `let` bodies and `if` branches), so a reference there is exempt; an
-- unsigned definition's body takes the arguments its reference gives.
chosenLengthsApplied :: AnnoS Int ManyPoly Int -> MorlocMonad ()
chosenLengthsApplied (AnnoS _ _ root) = void (foldM (walk rootEnv Nothing) Set.empty (foldExprS (: []) root))
  where
    rootEnv = case root of
      VarS v def -> Map.singleton v def
      _ -> Map.empty
    -- ctx Nothing: exempt; Just n: the expression is applied to n
    -- arguments. env: the definitions a recursive call may name.
    walk env ctx seen (AnnoS i _ e) = case e of
      LamS vs x -> walk env (fmap (\n -> max 0 (n - length vs)) ctx) seen x
      AppS f@(AnnoS fi _ (VarS v def)) xs -> do
        let ctx' = fmap (+ length xs) ctx
        applied ctx' fi v def
        seen' <- definition env ctx' seen v f def
        foldM (walk env (Just 0)) seen' xs
      AppS (AnnoS fi _ (CallS v)) xs -> do
        mapM_ (applied (fmap (+ length xs) ctx) fi v) (Map.lookup v env)
        foldM (walk env (Just 0)) seen xs
      AppS f xs -> walk env (fmap (+ length xs) ctx) seen f >>= \seen' -> foldM (walk env (Just 0)) seen' xs
      VarS v def -> applied ctx i v def >> definition env ctx seen v (AnnoS i i e) def
      CallS v -> mapM_ (applied ctx i v) (Map.lookup v env) >> return seen
      LetS _ x body -> walk env (Just 0) seen x >>= \seen' -> walk env ctx seen' body
      IfS c x y -> walk env (Just 0) seen c >>= \seen' -> walk env ctx seen' x >>= \seen'' -> walk env ctx seen'' y
      LabelS _ x -> walk env ctx seen x
      DoBlockS x -> walk env ctx seen x
      EvalS x -> walk env ctx seen x
      CoerceS _ x -> walk env ctx seen x
      _ -> foldM (walk env (Just 0)) seen (foldExprS (: []) e)
    -- A definition's body is the same at each of its references.
    definition env ctx seen v (AnnoS _ _ f) def =
      let bodies = foldExprS (: []) f
          ctx' = case def of
            MonomorphicExpr Nothing _ -> ctx
            _ -> Nothing
          key = (v, [j | AnnoS j _ _ <- bodies], ctx')
       in if Set.member key seen then return seen else foldM (walk (Map.insert v def env) ctx') (Set.insert key seen) bodies
    applied (Just n) at v def
      | Just et <- signatureOf def
      , (args, _) <- uncurryU (etype et)
      , n < length args
      , (l : _) <- outwardNats (etype et) (Map.keys (enatLabels et)) =
          throwTypeError at $
            squotes (pretty (displayName v)) <+> "chooses the length" <+> squotes (pretty l)
              <+> "at each application (DIM-10), so it must be applied to all"
              <+> pretty (length args) <+> "of its arguments here"
    applied _ _ _ _ = return ()

-- | Apply to the context of a synthesis result.
(<$$>) :: Functor m => (Gamma -> Gamma) -> m (Gamma, a, b) -> m (Gamma, a, b)
f <$$> m = (\(g, a, b) -> (f g, a, b)) <$> m
infixl 4 <$$>

-- | An unsigned definition is typed afresh at each reference, so applied
-- there to all its parameters its lambda runs once (DIM-10).
appliedDefinition :: AnnoS g ManyPoly c -> Int -> Gamma -> Gamma
appliedDefinition (AnnoS _ _ (VarS v (MonomorphicExpr Nothing bodies))) n g
  | Just k <- ownLambdaArity bodies, k <= n = g {gammaDefinitionLambda = Just v}
appliedDefinition _ _ g = g

-- | Entering a lambda: any lambda in its body is not a definition's own.
enterLambda :: Gamma -> Gamma
enterLambda g = g {gammaDefinitionLambda = Nothing}

-- | The definition a root is, when its bodies are lambdas.
ownLambda :: AnnoS g ManyPoly c -> Maybe EVar
ownLambda (AnnoS _ _ e@(VarS v _)) = v <$ ownLambdaArity (foldExprS (: []) e)
ownLambda _ = Nothing

-- | When every body of a definition is a lambda, the most parameters a
-- reference must give for each of them to run once.
ownLambdaArity :: [AnnoS g f c] -> Maybe Int
ownLambdaArity bodies = case [length vs | AnnoS _ _ (LamS vs _) <- bodies] of
  ns@(_ : _) | length ns == length bodies -> Just (maximum ns)
  _ -> Nothing

-- | Leaving a lambda restores the mark it entered with, for the next of a
-- definition's bodies.
leaveLambda :: Gamma -> Gamma -> Gamma
leaveLambda before after = after {gammaDefinitionLambda = gammaDefinitionLambda before}

-- | A length observed in a lambda's body is one value per evaluation of the
-- body (DIM-10): it may not reach a type outside the lambda, the variables
-- in scope around it, or, unless the lambda runs once per call of its
-- definition ('gammaDefinitionLambda'), the lambda's own type.
noEscapedLengths :: Int -> Gamma -> Gamma -> TypeU -> MorlocMonad ()
noEscapedLengths i before after t
  | Set.null fresh = return ()
  | any (`Set.member` fresh) (concatMap (collectNatVarNames . apply after) reach) =
      throwTypeError i
        "this lambda's type, or a variable around it, holds a length that a call in its body chooses, and the call may choose another at each application of the lambda (DIM-10); use the length only inside the body, or give the function a signature"
  | otherwise = return ()
  where
    fresh = Set.difference (gammaObserved after) (gammaObserved before)
    own = gammaDefinitionLambda before
    outer = [x | AnnG v x <- IntMap.elems (gammaContext before), Just v /= own]
    reach = if isJust own then outer else t : outer

-- | At a call, a Nat the called function chooses is a value known only at
-- run time (DIM-10).
observeResultNats :: AnnoS Int ManyPoly Int -> TypeU -> Gamma -> Gamma
observeResultNats (AnnoS _ _ (VarS _ def)) funType g
  | Just et <- signatureOf def =
  let renamed = Map.fromList (zipSame (List.nub (collectNatVarNames (etype et))) (List.nub (collectNatVarNames funType)))
      labelled = [r | v <- Map.keys (enatLabels et), Just r <- [Map.lookup v renamed]]
   in g {gammaObserved = Set.union (Set.fromList (outwardNats funType labelled)) (gammaObserved g)}
observeResultNats _ _ g = g

-- | The signature a reference is typed by: its own, or its class method's.
signatureOf :: ManyPoly a -> Maybe EType
signatureOf (MonomorphicExpr et _) = et
signatureOf (PolymorphicExpr _ _ et _) = Just et

-- | The Nat and Int variables of types that a run-time value gives: a
-- Vector's length and a label's value.
runTimeVars :: [TypeU] -> Set.Set TVar
runTimeVars = Set.fromList . concatMap go
  where
    go t = case t of
      AppU (VarU v) [n, a] | v == BT.vector -> bare n <> go a
      IntVarU v -> [v]
      _ -> concatMap go (typeUChildren t)
    bare n = case typeUToNatExpr n of
      Just (NS.NatVar x) -> [x]
      Just (NS.IntVar x) -> [x]
      _ -> []

peakSExpr :: ExprS Int ManyPoly Int -> MDoc
peakSExpr UniS = "UniS"
peakSExpr (PatS _ _ _ app) = "PatS" <+> pretty (patAppPattern app)
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
peakSExpr (ShapeS _ sh _) = "ShapeS" <+> viaShow sh
peakSExpr (IfS _ _ _) = "IfS"
peakSExpr (IntrinsicS intr _) = "@" <> pretty (intrinsicName intr)


-------------------------------------------------------------------------------
-- Signatures as contracts

-- | Check each definition against its signature as a contract: the
-- signature's type variables are rigid, so the body must work at every
-- instance of them, and every class method the body uses at a rigid type must
-- follow from the signature's constraints (a superclass follows from its
-- subclass) or be met by an instance ('resolve'). The trees are the
-- definitions' own bodies, with references to other signed top-level terms
-- typed by their signatures ('Treeify'). The results are discarded: this
-- only rejects programs.
validate :: [AnnoS Int ManyPoly Int] -> MorlocMonad ()
validate = mapM_ (void . checkSigned)

-- | A definition checked against its signature: the gamma once its body's
-- constraints are solved, its type, and its typed tree.
data Checked = Checked Gamma TypeU (AnnoS (Indexed TypeU) ManyPoly Int)

-- | A checked definition at a type it is used at: the signature's variables
-- become unknowns that the type solves, and checking finishes on the typed
-- tree, so the body is not inferred again. Each nested signed definition in
-- the tree has one use, whose type its body is connected to, and a recursive
-- call is at the definition's own type, as when the body is inferred at the
-- type.
checkedAt :: TypeU -> Checked -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
checkedAt key (Checked g0 rootT0 e0) = do
  scope <- MM.getGeneralScope
  let g1 = apply g0 g0
      -- the signature's variables, and those only its constraints name
      dependent =
        Set.fromList
          [ v
          | Constraint _ ts <- fromMaybe [] (gammaAssumedConstraints g1)
          , (v@(TV n), VarPlain) <- concatMap varForms ts
          , isTypeVariableName n
          ]
      rigid = Set.union (fromMaybe Set.empty (gammaRigid g1)) dependent
      open = openVars rigid
      e1@(AnnoS (Idx rootIdx _) _ rootE) = mapAnnoSG (fmap (open . apply g1)) e0
      rootT = open (apply g1 rootT0)
      rootName = case rootE of
        VarS v _ -> Just v
        _ -> Nothing
      connect at g (a, b) = case subtype scope a b g of
        Right g' -> return g'
        Left err -> throwTypeError at err
      nested = [(open a, open b) | (a, b) <- gammaNestedUses g1]
      -- each back-edge with the type of the definition it calls back to: the
      -- innermost enclosing one of its name (a definition expanded at two
      -- uses is two copies, each its own type)
      recursive = backEdges (maybe Map.empty (\v -> Map.singleton v rootT) rootName) e1
      backEdges scope0 (AnnoS (Idx j t) _ x) = case x of
        CallS v | Just d <- Map.lookup v scope0 -> [(j, v, d, t)]
        VarS v (MonomorphicExpr (Just _) xs@(_ : _)) -> concatMap (backEdges (Map.insert v t scope0)) xs
        _ -> concatMap (backEdges scope0) (foldExprS (: []) x)
  g2 <- connect rootIdx (openRigidGamma rigid dependent g1) (rootT, key)
  g3 <- foldM (connect rootIdx) g2 nested
  g4 <-
    foldM
      -- a call's effects may widen where it is used (a do-block), so the
      -- call's type is held to the definition's, not the reverse
      ( \g (j, v, d, t) -> case subtype scope t d g of
          Right g' -> return g'
          Left err ->
            MM.throwSourcedError j $
              "This recursive use of"
                <+> squotes (pretty (displayName v))
                <+> "is at" <+> prettyTypeU (apply g t) <> ", not at the type of the definition it recurses to,"
                <+> prettyTypeU (apply g d) <> " (polymorphic recursion is not supported):" <+> err
      )
      g3
      recursive
  -- the root is annotated with the key as the use wrote it, as checking
  -- against the key annotates it
  let keyed = case e1 of AnnoS (Idx gi _) ci x -> AnnoS (Idx gi key) ci x
  out <- finishRoot g4 rootT keyed >>= assertStructuralTypesExpanded
  checkLiteralTypes out
  -- the coercions to optional the check made of values not optional there
  let ofPlain = Set.fromList [gi | AnnoS (Idx gi _) _ (CoerceS CoerceToOptional (AnnoS (Idx _ xt) _ _)) <- annoNodes e1, not (isOptionalU xt)]
  return (collapseOptionals ofPlain out)

-- | The labels of a renaming of a signature, by the renamed variable names.
enatLabelsRenamed :: EType -> TypeU -> Map.Map Int TVar
enatLabelsRenamed et renamed =
  let ren = Map.fromList (zipSame (nub (collectNatVarNames (etype et))) (nub (collectNatVarNames renamed)))
   in Map.fromList [(idx, Map.findWithDefault lv lv ren) | (lv, idx) <- Map.toList (enatLabels et)]

-- | The variables of two renamings of one signature, each with its
-- counterpart: the quantified ones, the use's an existential, and the kind
-- variables of each kind.
signaturePairs :: TypeU -> TypeU -> [(TypeU, TypeU)]
signaturePairs a b =
  pairs VarU (\v -> ExistU v ([], Open) ([], Open)) (fst (unqualify a)) (fst (unqualify b))
    <> pairs NatVarU NatVarU (natOnly a) (natOnly b)
    <> pairs IntVarU IntVarU (intOnly a) (intOnly b)
    <> pairs StrVarU StrVarU (nub (collectStrVarNames a)) (nub (collectStrVarNames b))
    <> pairs RecVarU RecVarU (nub (collectRecVarNames a)) (nub (collectRecVarNames b))
    <> pairs ListVarU ListVarU (nub (collectListVarNames a)) (nub (collectListVarNames b))
    <> pairs row row (nub (collectEffVarNames a)) (nub (collectEffVarNames b))
  where
    row v = EffectU (EffectVar v) BT.unitU
    pairs f g xs ys = [(f x, g y) | (x, y) <- zipSame xs ys]
    natOnly t = nub [v | NatVarU v <- everyType t]
    intOnly t = nub [v | IntVarU v <- everyType t]

-- | Every literal's final type accepts it: an integer literal a numeric base
-- type, a real one a real type. A type still a variable is not judged.
checkLiteralTypes :: AnnoS (Indexed TypeU) Many Int -> MorlocMonad ()
checkLiteralTypes e = do
  scope <- MM.getGeneralScope
  let judged t = case t of
        VarU _ -> False
        ExistU {} -> False
        _ -> True
      accepts baseOK t =
        let tEval = either (const t) id (TE.evaluateType scope t)
         in baseOK t || baseOK tEval || baseOK (TE.wireParentRoot scope tEval)
      peel t = case t of
        EffectU _ x -> peel x
        OptionalU x -> peel x
        _ -> t
      check :: Int -> TypeU -> (TypeU -> Bool) -> MorlocMonad ()
      check li t0 baseOK =
        let t = peel t0
         in when (judged t && not (accepts baseOK t)) $
          MM.throwSourcedError li ("Numeric literal cannot have type " <> prettyTypeU t)
  sequence_
    [ case x of
        IntS li _ -> check li t (\u -> BT.isIntegerBaseType u || BT.isRealBaseType u)
        RealS li _ -> check li t BT.isRealBaseType
        _ -> return ()
    | AnnoS (Idx _ t) _ x <- annoNodes e
    , isLit x
    ]
  where
    isLit IntS {} = True
    isLit RealS {} = True
    isLit _ = False

-- | OPT-2: @?(?T)@ is @?T@, so a coercion to optional, of the nodes @ours@,
-- whose value instantiation made optional is no coercion; the value keeps its
-- type.
collapseOptionals :: Set.Set Int -> AnnoS (Indexed TypeU) Many Int -> AnnoS (Indexed TypeU) Many Int
collapseOptionals ours = go
  where
    go (AnnoS (Idx gi t) ci e) = case e of
      CoerceS CoerceToOptional x@(AnnoS (Idx _ xt) _ _)
        | Set.member gi ours && isOptionalU xt -> case go x of
            AnnoS (Idx _ xt') xci xe -> AnnoS (Idx gi xt') xci xe
      _ -> AnnoS (Idx gi t) ci (MM.runIdentity (mapExprSM (pure . go) e))

isOptionalU :: TypeU -> Bool
isOptionalU (OptionalU _) = True
isOptionalU _ = False

-- | Check one definition against its signature ('validate').
checkSigned :: AnnoS Int ManyPoly Int -> MorlocMonad Checked
checkSigned e0@(AnnoS i _ _) = do
      let g0 = emptyGamma {gammaRigid = Just Set.empty}
      (g1unsettled, rootT, e1) <- synthG g0 e0
      scope <- MM.getGeneralScope
      g1 <- case settleDeferred scope g1unsettled of
        Left (site, err) -> MM.throwSourcedError (fromMaybe i site) err
        Right g -> return g
      -- An equation over a variable of the signature must hold at every
      -- value of it; one still undecided is not shown to (POLY-1). Nat
      -- equations are decided by 'checkRigidNats' (POLY-7).
      case [ p
           | p@(t1, t2) <- gammaDeferred g1
           , not (natDeferred p)
           , not (Set.null (Set.intersection (gammaRigidSlots g1) (rigidSlotVars (apply g1 t1) <> rigidSlotVars (apply g1 t2))))
           ] of
        p@(t1, t2) : _ ->
          let (d1, d2) = prettyTypeUPair (apply g1 t1) (apply g1 t2)
           in MM.throwSourcedError (Map.findWithDefault i p (gammaDeferredAt g1)) $
                "The compiler cannot show that this equation holds for every value of the signature's variables:"
                  <+> d1 <+> "~" <+> d2
        [] -> return ()
      let rigid = fromMaybe Set.empty (gammaRigid g1)
          givens = fromMaybe [] (gammaAssumedConstraints g1)
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
      checkShapes g1 e1
      env <- MM.gets stateInstances
      let fixed = Set.unions [rigid, gammaRigidNats g1, gammaRigidSlots g1]
          mentionsRigid t = any ((`Set.member` fixed) . fst) (varForms t)
          given = map (FI.fixRigidVars fixed . apply g1) givens
          policy = FI.usePolicy {FI.policySkolems = Set.map FI.rigidVarName fixed}
      -- A literal whose type is a variable of the signature needs that
      -- variable's class among the signature's constraints.
      sequence_
        [ MM.throwSourcedError li ("Numeric literal cannot have type" <+> prettyTypeU (VarU u))
        | (li, v, k) <- gammaPendingNumLits g1
        , VarU u <- [literalEnd g1 v]
        , Set.member u rigid
        , not (literalAtRigid k g1 (VarU u))
        ]
      -- Literal defaults come first: in @3 <= 5@ only the Int default fixes
      -- the class argument.
      g2 <- resolvePendingNumLits rigid g1 (gammaPendingNumLits g1) >>= defaultNumeric Set.empty
      let (g3, solution) = solve policy scope env given (FI.fixRigidVars fixed) (FI.unfixRigidVars fixed) g2 (gammaWanteds g2)
          uses = Map.fromList [(j, (ci, v, t)) | AnnoS (Idx j t) ci (VarS v _) <- annoNodes e1]
          args c = case c of
            Constraint _ ts -> ts
            _ -> []
          useError = unmetAtUse scope uses g3
          derivedIn ev = case ev of
            EvDerived {} -> [ev]
            EvInstance _ _ ctx -> concatMap derivedIn ctx
            EvGiven _ -> []
          derivedOrd site cls t = derivedAt scope env cls t >>= \d ->
            if d then return () else useError site (Constraint cls [t]) (FI.NoInstance (Constraint cls [t]) [])
      forM_ (solResidual solution) $ \(w@(Wanted site c0 _), residual) ->
        let c = apply g3 c0
         in case residual of
              Unmet (FI.NoInstance _ _)
                | wIntrinsic w && not (any mentionsRigid (args c)) -> return ()
                | any mentionsRigid (args c) -> unmet i given [] (FI.fixRigidVars fixed c)
              Unmet e -> useError site c e
              Undecided vs -> useError site c (FI.Undetermined c vs)
      -- DERIV-6, for each derived comparison the evidence holds, an
      -- instance's context included: the class derives at the type, and Ord
      -- only where Eq is derived too; at a type holding the signature's
      -- variables, only where no hand-written Eq could match once they are
      -- known, whatever the givens say.
      sequence_
        [ case unClassName cls of
            "Ord" -> case FI.resolve policy scope env [] (Constraint (ClassName "Eq") [t]) of
              Right (EvDerived _ _) -> return ()
              Left (FI.NoInstance _ _) -> derivedOrd site cls t
              Left e -> useError site (Constraint cls [t]) e
              Right _ -> derivedOrd site cls t
            _ -> derivableAt scope cls t >>= \d ->
              if d then return () else useError site (Constraint cls [t]) (FI.NoInstance (Constraint cls [t]) [])
        | (site, evs) <- IntMap.toList (solEvidence solution)
        , EvDerived cls t <- concatMap derivedIn evs
        ]
      return (Checked g3 rootT e1)

-- | Report a wanted at @site@ that resolution could not meet at the use it
-- arose at, as instance resolution at the use reports it.
unmetAtUse :: Scope -> Map.Map Int (Int, EVar, TypeU) -> Gamma -> Int -> Constraint -> FI.ResolveError -> MorlocMonad a
unmetAtUse scope uses g3 site c e = case Map.lookup site uses of
  Just (ci, v, t) ->
    let use = evalOr scope (apply g3 t)
        lead = case e of
          FI.Undetermined _ _ -> "The type of this term is not determined: the use of"
          _ -> "The use of"
        advice = case (e, c) of
          (FI.Disagrees _ _, _) -> mempty
          (_, Constraint cls _) -> noInstanceAdvice scope cls v use
          _ -> mempty
     in throwTypeError ci $
          lead <+> squotes (pretty v) <+> "at type" <+> prettyTypeU use
            <+> "needs" <+> prettyConstraint c <> ":" <+> FI.explainResolveError prettyConstraint e <> advice
  Nothing ->
    MM.throwSourcedError site $
      "This term needs" <+> prettyConstraint c <> ":" <+> FI.explainResolveError prettyConstraint e <> "."

-- | SIG-10, POLY-9: what a definition whose type no signature states must
-- already satisfy, whatever its uses: a literal ends at a type of its class,
-- and a class constraint over no unknown has an instance. A constraint over
-- an unknown waits for the use that fixes it.
groundCheck :: Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad ()
groundCheck g1 e1 = do
  scope <- MM.getGeneralScope
  env <- MM.gets stateInstances
  let open = Set.fromList [v' | (_, v, _) <- gammaPendingNumLits g1, ExistU v' _ _ <- [literalEnd g1 v]]
  g2 <- resolvePendingNumLits open g1 (gammaPendingNumLits g1)
  let (g3, solution) = solve FI.usePolicy scope env [] id id g2 (gammaWanteds g2)
      uses = Map.fromList [(j, (ci, v, t)) | AnnoS (Idx j t) ci (VarS v _) <- annoNodes e1]
      ground c = case c of
        Constraint _ ts -> null [() | t <- ts, (TV n, _) <- varForms (apply g3 t), isTypeVariableName n]
        _ -> False
  forM_ (solResidual solution) $ \(w@(Wanted site c0 _), residual) ->
    let c = apply g3 c0
     in case residual of
          Unmet e | ground c, not (wIntrinsic w) -> unmetAtUse scope uses g3 site c e
          _ -> return ()
  mapM_ (\(AnnoS (Idx j t) _ _) -> checkFinalType (Idx j (apply g3 t))) (annoNodes e1)

-- | POLY-9: whether a type keeps a tuple or record whose other elements or
-- fields are unknown.
isPattern :: TypeU -> Bool
isPattern t = or [open ps pc || open rs rc | ExistU _ (ps, pc) (rs, rc) <- everyType t]
  where
    open xs c = not (null xs) && c == Open

-- | PAT-1: a tuple pattern on a value nothing else has determined makes it
-- a tuple of the pattern's width, so the type a definition is given holds
-- what its patterns destructure.
imposeShape :: Gamma -> TypeU -> PatShape -> Gamma
imposeShape g t sh = case (apply g t, sh) of
  (ExistU v (ps, _) ([], _), Shape _ (FormTuple ss))
    | length ps <= length ss ->
    let (g1, rest) = statefulMap (\gx _ -> newvar "shape_" gx) g (drop (length ps) ss)
        xs = ps <> rest
     in case solveExist v (BT.tupleU xs) g1 of
          Right (Just g2) -> foldl (\gx (x, s') -> imposeShape gx x s') g2 (zipSame xs ss)
          _ -> g
  (AppU (VarU tv) ts, Shape _ (FormTuple ss))
    | tv == BT.tuple (length ts), length ts == length ss ->
        foldl (\gx (x, s') -> imposeShape gx x s') g (zipSame ts ss)
  _ -> g

-- | POLY-8, NUM-5: the most general type of an unsigned definition, its
-- constraints and its literals' classes included. Left, with the reason,
-- for a type no signature could state: one holding a kind-level or effect
-- variable, an open shape, an undecided equation, or a constraint neither
-- an instance nor the type's variables answer.
inferScheme :: AnnoS Int ManyPoly Int -> MorlocMonad (Either MDoc EType)
inferScheme e0 = do
  scope <- MM.getGeneralScope
  env <- MM.gets stateInstances
  let g0 = emptyGamma {gammaRigid = Just Set.empty, gammaAssumedConstraints = Just [], gammaDefinitionLambda = ownLambda e0}
  (g1s, t0, e1) <- synthG g0 e0
  checkShapes g1s e1
  let g1u = foldl (\g (AnnoS (Idx _ t) _ _, sh) -> imposeShape g t sh) g1s [(r, sh) | AnnoS _ _ (ShapeS _ sh r) <- annoNodes e1]
  case settleDeferred scope g1u of
    Left _ -> groundCheck g1u e1 >> return (Left "an undecided kind-level equation")
    Right g1 -> groundCheck g1 e1 >> do
      let t1 = apply g1 t0
          exists = List.nub [(v, not (null ps && null rs)) | ExistU v (ps, _) (rs, _) <- everyType t1]
          openShape = isPattern t1
          gen = map fst exists
          stated = and
            [ case form of
                VarPlain -> not (isTypeVariableName n) || v `elem` gen
                _ -> False
            | (v@(TV n), form) <- varForms t1
            ]
          chainEnd v = case literalEnd g1 v of
            ExistU v' _ _ -> Just v'
            _ -> Nothing
          (held, rest) = List.partition (\(_, v, _) -> maybe False (`elem` gen) (chainEnd v)) (gammaPendingNumLits g1)
          litClass k = if k == RealDefault then BT.numericClass else BT.integralClass
          litWanteds =
            [ Wanted site (Constraint (litClass k) [ExistU v' ([], Open) ([], Open)]) False
            | (site, v, k) <- held
            , Just v' <- [chainEnd v]
            ]
          names = take (length gen) [TV n | n <- map MT.singleton ['a' .. 'z'] <> [MT.pack ("t" <> show k) | k <- [(0 :: Int) ..]]]
          fixed = Set.fromList names
          policy = FI.usePolicy {FI.policySkolems = Set.map FI.rigidVarName fixed}
          bind g (v, n) = case solveExist v (VarU n) g of
            Right (Just g') -> Just g'
            _ -> Nothing
          mentionsGen c = case c of
            Constraint _ ts -> any (\(v, _) -> Set.member v fixed) (concatMap varForms ts)
            _ -> False
          generalized g2 =
            let (g3, solution) = solve policy scope env [] (FI.fixRigidVars fixed) (FI.unfixRigidVars fixed) g2 (gammaWanteds g2)
                residuals = [(w, apply g3 (wPred w), r) | (w, r) <- solResidual solution]
                answered (w, c, r) = case r of
                  Unmet (FI.NoInstance _ _) -> mentionsGen c || wIntrinsic w
                  _ -> False
                t2 = apply g3 (apply g2 t1)
                owed = List.nub [c | x@(_, c, _) <- residuals, answered x, mentionsGen c]
                implies c' c = any (FI.sameConstraint c) (FI.superClosure env [c'])
                -- a constraint another implies, and does not follow from, is
                -- not stated
                schemeCs = [c | c <- owed, not (any (\c' -> implies c' c && not (implies c c')) owed)]
                unknown = not (null [() | Constraint _ ts <- schemeCs, ExistU {} <- concatMap everyType ts])
             in if all answered residuals && not unknown
                  then
                    Right
                      EType
                        { etype = foldr ForallU t2 names
                        , econs = Set.fromList schemeCs
                        , edocs = ArgDocSig defaultValue (map (const defaultValue) (fst (uncurryU t2))) defaultValue
                        , enatLabels = Map.empty
                        }
                  else Left "a constraint neither an instance nor the type's variables answer"
      if
        | openShape -> return (Left "an open tuple or record shape")
        | any snd exists -> return (Left "a tuple or record no named type gives")
        | not stated -> return (Left "a kind-level, effect or annotated variable")
        | not (null (collectEffVarNames t1)) -> return (Left "an effect variable")
        | not (null (gammaDeferred g1)) -> return (Left "an undecided equation")
        | not (null (gammaConstraints g1)) -> return (Left "a set constraint")
        | not (all (\(_, _, k) -> Map.member (litClass k) (ieClasses env)) held) -> return (Left "a literal whose class is not in scope")
        | otherwise -> do
            g2 <- resolvePendingNumLits (Set.fromList gen) g1 rest >>= defaultNumeric (Set.fromList gen)
            case foldM bind g2 (zipSame gen names) of
              Nothing -> return (Left "a variable that cannot be generalized")
              Just g2b -> return (generalized g2b {gammaWanteds = litWanteds <> gammaWanteds g2b})

-- | PAT-1: each destructuring pattern fits the value it destructures. A
-- tuple pattern needs a tuple of its width, a record pattern a record with
-- its fields. A value whose type is still unsolved must not already
-- contradict the pattern, and the patterns on one unsolved value must agree.
checkShapes :: Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad ()
checkShapes g e = do
  scope <- MM.getGeneralScope
  foldM_
    (\seen (sh, t) -> fits scope seen sh (apply g t))
    Map.empty
    [(sh, t) | AnnoS _ _ (ShapeS _ sh (AnnoS (Idx _ t) _ _)) <- annoNodes e]
  where
    -- @seen@: the tuple width or record fields each unsolved value is
    -- destructured with so far
    fits :: Scope -> Map.Map TVar (Either Int [Key]) -> PatShape -> TypeU -> MorlocMonad (Map.Map TVar (Either Int [Key]))
    fits _ seen ShapeAny _ = return seen
    fits scope seen (Shape i form) t0 = case receiverShape scope t0 of
      ForallU _ t -> fits scope seen (Shape i form) t
      AppU (VarU v) ts
        | FormTuple ss <- form
        , v == BT.tuple (length ts) ->
            if length ts == length ss
              then foldM (\m (s', t') -> fits scope m s' t') seen (zipSame ss ts)
              else tupleWidthError i (length ss) (pretty (length ts))
      t | FormRecord kss <- form, Just (fs, closed) <- rowFields t -> fields closed fs kss seen
      ExistU v (ps, pc) (rs, rc) -> do
        seen' <- agree v
        case form of
          FormTuple ss
            | not (null rs) || (pc == Closed && null ps) -> notA "a record"
            | length ps > length ss || (pc == Closed && length ps /= length ss) ->
                tupleWidthError i (length ss) ((if pc == Closed then "" else "at least ") <> pretty (length ps))
            | otherwise -> foldM (\m (s', t') -> fits scope m s' t') seen' (zipPrefix ss ps)
          FormRecord kss
            | not (null ps) -> notA "a tuple"
            | otherwise -> fields (rc == Closed) rs kss seen'
      t -> MM.throwSourcedError i $
        "This" <+> kind <+> "pattern destructures a value of type" <+> prettyTypeU t
      where
        fields closed fs kss m0 = case [k | (k, _) <- kss, k `notElem` map fst fs] of
          (k : _) | closed -> missingKeyError i k
          _ -> foldM (\m (k, s') -> maybe (return m) (fits scope m s') (lookup k fs)) m0 kss

        agree v = case (Map.lookup v seen, form) of
          (Just (Left n), FormTuple ss)
            | n /= length ss -> tupleWidthError i (length ss) (pretty n)
          (Just (Left _), FormRecord _) -> notA "a tuple"
          (Just (Right _), FormTuple _) -> notA "a record"
          (Just (Right ks), FormRecord kss) -> return (Map.insert v (Right (List.nub (ks <> map fst kss))) seen)
          (_, FormTuple ss) -> return (Map.insert v (Left (length ss)) seen)
          (_, FormRecord kss) -> return (Map.insert v (Right (map fst kss)) seen)

        kind :: MDoc
        kind = case form of
          FormRecord _ -> "record"
          FormTuple _ -> "tuple"

        notA :: MDoc -> MorlocMonad a
        notA what =
          MM.throwSourcedError i $
            "This" <+> kind <+> "pattern destructures a value that is also used as" <+> what

-- | POLY-7: the body of a definition holds for every value of its
-- signature's Nat variables at which the signature's own Nat positions are
-- non-negative (KIND-4). The Nat equations checking left, solved or
-- deferred, and the non-negativity of every Nat position in the body's
-- types, that reach one of those variables are decided with them rigid
-- (DIM-7).
checkRigidNats :: Int -> Gamma -> [(Int, TypeU)] -> MorlocMonad ()
checkRigidNats i g bodyTypes
  | Set.null rigidAll = return ()
  | (j : _) <- [j | (j, ps) <- bySignature, ND.Refuted _ <- [ND.decide Set.empty (nonNegative ps)]] =
      MM.throwSourcedError j
        "No value of the Nat and Int variables of this signature makes every Nat position in it non-negative (KIND-4)"
  | otherwise = case reaching of
      [] -> return ()
      conds -> case ND.decideAssuming rigidAll domain conds of
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
    -- A size over the signature's own rows stands for every value too.
    sizes = opaqueSizes (map (apply g . snd) bodyTypes <> map snd (gammaRigidDomain g) <> map (apply g) (Map.elems (gammaKindSubs g)))
    rigidSizes = Set.fromList
      [ name
      | (name, SizeU r) <- Map.toList sizes
      , let rows = Set.fromList (collectRecVarNames r)
      , not (Set.null rows)
      , rows `Set.isSubsetOf` gammaRigidSlots g
      ]
    rigidAll = Set.unions [rigid, rigidSizes, gammaObserved g]
    nonNegative ts = [ND.NatGe e (NS.NatLit 0) | t <- ts, Just e <- [natPositionExpr t]]
    domain = nonNegative (map snd (gammaRigidDomain g))
    bySignature = Map.toList (Map.fromListWith (flip (<>)) [(j, [p]) | (j, p) <- gammaRigidDomain g])
    solved =
      [ ND.NatEq (if k == KindInt then NS.IntVar v else NS.NatVar v) e
      | ((v, k), t) <- Map.toList (gammaKindSubs g)
      , fillsNat k
      , Just e <- [natPositionExpr (apply g t)]
      ]
    -- Each deferred equation at the term where it arose, and each Nat
    -- position at the term whose type holds it.
    deferredAt =
      [ (Map.findWithDefault i p (gammaDeferredAt g), ND.NatEq a b)
      | p@(t1, t2) <- gammaDeferred g
      , Just a <- [natPositionExpr (apply g t1)]
      , Just b <- [natPositionExpr (apply g t2)]
      ]
    deferred = map snd deferredAt
    located = [(j, c) | (j, t) <- bodyTypes, c <- nonNegative (natPositions (apply g t))]
    positions = map snd located
    tagged = deferredAt <> located
    reaching = ND.reaching rigidAll (Set.toList (Set.fromList (filter (not . trivial) (solved <> deferred <> positions))))
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
    -- A signature's variable by the name it wrote, numbered where two
    -- signatures write the same name; an inferred one marked with '?'.
    natName v@(TV t)
      | Just s <- Map.lookup v sizes = prettyTypeU s
      | Set.member v rigid, Set.notMember (base t) sharedBases = pretty (base t)
      | Set.member v rigid = pretty (numbered t)
      | otherwise = "?" <> pretty (numbered t)
    base = MT.takeWhile (/= '@')
    numbered t = base t <> MT.filter (`elem` ['0' .. '9']) (MT.dropWhile (/= '@') t)
    sharedBases = Set.fromList [b | (b, k) <- Map.toList (Map.fromListWith (+) [(base t, 1 :: Int) | TV t <- Set.toList rigid]), k > 1]

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

-- | A quantified type instantiated with fresh existentials; the constraints,
-- rewritten over the same existentials, are recorded as obligations of the
-- term at @site@.
instantiateObligations :: Int -> Gamma -> TypeU -> [Constraint] -> (Gamma, TypeU)
instantiateObligations site g (ForallU v t) cs =
  instantiateObligations site (g +> v) (substitute v t) (map (substC v) cs)
  where
    substC w (Constraint c ts) = Constraint c (map (substitute w) ts)
    substC _ c = c
instantiateObligations site g t cs = (owe site (map substC cs) g', t)
  where
    -- a variable only the constraints name is one a dependency determines
    -- (CLS-20): a fresh existential at each use, as a quantified one is
    dependent =
      List.nub
        [ v
        | Constraint _ ts <- cs
        , (v@(TV n), VarPlain) <- concatMap varForms ts
        , isTypeVariableName n
        ]
    (g', fresh) = statefulMap (\gx v@(TV n) -> second ((,) v) (newvar (n <> "_dep") gx)) g dependent
    substC (Constraint c ts) = Constraint c (map (\x -> foldr (\(v, e) acc -> substituteTVar v e acc) x fresh) ts)
    substC c = c

-- | Record constraints the term at @site@ owes: class constraints as
-- wanteds, the others as primitive constraints.
owe :: Int -> [Constraint] -> Gamma -> Gamma
owe site cs g =
  g { gammaWanteds = reverse [Wanted site c False | c@(Constraint _ _) <- cs] <> gammaWanteds g
    , gammaConstraints = gammaConstraints g ++ [c | c <- cs, isPrimitiveConstraint c]
    }

-- | The class constraints of a gamma's wanteds.
wantedConstraints :: Gamma -> [Constraint]
wantedConstraints g = map wPred (gammaWanteds g)

-- | An obligation neither the signature nor an instance meets. @open@: the
-- variables of it nothing in the definition fixed.
unmet :: Int -> [Constraint] -> [TVar] -> Constraint -> MorlocMonad ()
unmet defIdx givens open c = do
  closure <- superclassClosure givens
  let declared = case closure of
        [] -> "no constraints"
        cs -> hcat (punctuate ", " (map prettyConstraint cs))
      undetermined = case open of
        [] -> mempty
        vs -> "; nothing in the definition fixes" <+> hsep (punctuate "," (map pretty vs))
  MM.throwSourcedError defIdx $
    "This definition needs" <+> prettyConstraint c
      <+> "but its signature does not declare it (the signature's"
      <+> "constraints and their superclasses give" <+> declared <> ")" <> undetermined <> "."

-- | Constraints together with every superclass constraint they imply.
superclassClosure :: [Constraint] -> MorlocMonad [Constraint]
superclassClosure cs0 = do
  classes <- MM.gets (ieClasses . stateInstances)
  let supers (Constraint cls args) = case Map.lookup cls classes of
        Just ci | Just pairs <- zipExact (ciVars ci) args ->
          [ Constraint sc (map (substAll pairs) sargs)
          | Constraint sc sargs <- ciSupers ci
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

