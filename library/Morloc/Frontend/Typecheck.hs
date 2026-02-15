{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

{- |
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Typecheck (typecheck, resolveTypes, evaluateAnnoSTypes, peakSExpr) where

import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
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
      let g0 = Gamma {gammaCounter = 0, gammaContext = [], gammaSolved = Map.empty}
      (g1, _, e1) <- synthG g0 e0
      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g1:"
      seeGamma g1
      insetSay "========================================================"
      let e2 = mapAnnoSG (fmap normalizeType) . applyGen g1 $ e1

      (g2, e3) <- resolveInstances g1 e2
      let g3 = apply g2 g2

      -- apply inferred type information to the extracted type qualifiers
      -- this information was uploaded by the `recordParameter` function
      s <- MM.get

      insetSay "g3:"
      seeGamma g3
      insetSay $ "stateTypeQualifier s:" <+> viaShow (stateTypeQualifier s)
      let qmap = Map.map (prepareQualifierMap g3) (stateTypeQualifier s)
      MM.put (s {stateTypeQualifier = qmap})

      insetSay $ "Qualifier Map:" <+> viaShow qmap

      -- perform a final application of gamma the final expression and return
      -- (is this necessary?)
      return (applyGen g3 e3)

-- The typechecker goes through two passes assigning two different var names
-- to the qualifiers. The first is never resolved, and is left as
-- existential. So here I remove them. This is hacky as hell. Need a cleaner
-- solution.
prepareQualifierMap :: Gamma -> [(TVar, TypeU, Int)] -> [(TVar, TypeU, Int)]
prepareQualifierMap g = takeLast . filter notExistential . map f
  where
    f
      ( TV . head . MT.splitOn "___" . unTVar -> v
        , apply g -> t
        , i
        ) = (v, t, i)

    notExistential (_, ExistU {}, _) = False
    notExistential _ = True

    takeLast :: [(TVar, TypeU, Int)] -> [(TVar, TypeU, Int)]
    takeLast = reverse . findFirsts [] . reverse
      where
        findFirsts :: [TVar] -> [(TVar, TypeU, Int)] -> [(TVar, TypeU, Int)]
        findFirsts _ [] = []
        findFirsts observed ((v, t, i) : rs)
          | elem v observed = []
          | otherwise = (v, t, i) : findFirsts (v : observed) rs

-- Upload a solved universal qualifier to the stateTypeQualifier list
recordParameter :: Int -> TVar -> TypeU -> MorlocMonad ()
recordParameter i v t = do
  s <- MM.get
  let size = findTypeKindSize v t
      updatedMap =
        Map.insertWith
          (\xs ys -> ys <> xs)
          i
          [(v, ExistU v ([], Open) ([], Open), size)]
          (stateTypeQualifier s)
  MM.put $ s {stateTypeQualifier = updatedMap}

findTypeKindSize :: TVar -> TypeU -> Int
findTypeKindSize v = head . catMaybes . f
  where
    f (AppU (VarU v') ts)
      | v == v' = [Just (1 + (length ts))]
      | otherwise = concat $ map f ts
    f (AppU t ts) = concat $ map f (t : ts)
    f (VarU v')
      | v == v' = [Just 1]
      | otherwise = [Nothing]
    f (ExistU v' (ts1, _) (map snd . fst -> ts2))
      | v == v' = [Just (1 + (length ts1))]
      | otherwise = concat $ map f (ts1 <> ts2)
    f (ForallU _ t) = f t
    f (FunU ts t) = concat $ map f (t : ts)
    f (NamU _ v' ts1 (map snd -> ts2))
      | v == v' = [Just (1 + (length ts1))]
      | otherwise = concat $ map f (ts1 <> ts2)

-- TypeU --> Type
resolveTypes :: AnnoS (Indexed TypeU) Many Int -> AnnoS (Indexed Type) Many Int
resolveTypes (AnnoS (Idx i t) ci e) =
  AnnoS (Idx i (typeOf t)) ci (f e)
  where
    f :: ExprS (Indexed TypeU) Many Int -> ExprS (Indexed Type) Many Int
    f (BndS x) = BndS x
    f (LetBndS x) = LetBndS x
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
      -- this resolve general aliases all the way to the general termini
      let rssSubtypes = [x | x@(EType t _ _, _) <- rss, isSubtypeOf2 scope t gt]

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
      (g2, es1) <- case mostSpecific [t | (EType t _ _, _) <- rssSubtypes] of
        -- if there are no suitable instances, die
        [] -> throwTypeError genIndex $
          "No instance found for" <+> pretty clsName
            <> "::"
            <> pretty v
            <> "\n  Are you missing a top-level type signature?"

        -- There may be many suitable instances from the general type level,
        -- however, they may differ at the concrete level, so keep all for know
        -- and let the concrete inference code sort things out later.
        manyTypes -> do
          -- filter out just the instances that are in the most specific set
          let es0 = concat [rs | (t, rs) <- rssSubtypes, etype t `elem` manyTypes]
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
    f _ g0 (BndS v) = return (g0, BndS v)
    f _ g0 (RealS x) = return (g0, RealS x)
    f _ g0 (IntS x) = return (g0, IntS x)
    f _ g0 (LogS x) = return (g0, LogS x)
    f _ g0 (StrS x) = return (g0, StrS x)
    f _ g0 (ExeS x) = return (g0, ExeS x)

    connectInstance :: Gamma -> [AnnoS (Indexed TypeU) f c] -> MorlocMonad Gamma
    connectInstance g0 [] = return g0
    connectInstance g0 (AnnoS (Idx i t) _ _ : es) = do
      scope <- MM.getGeneralScope i
      case subtype scope gt t g0 of
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
  (g', t', e') <- case Map.lookup i annotation of
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
  (g', t, e') <- case Map.lookup gi annotation of
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
synthE _ g (RealS x) = return (g, BT.realU, RealS x)
synthE _ g (IntS x) = return (g, BT.intU, IntS x)
synthE _ g (LogS x) = return (g, BT.boolU, LogS x)
synthE _ g (StrS x) = return (g, BT.strU, StrS x)
-- Ensures pattern setting operations return the correct type.
-- Without this case, patterns that change type will pass silently, but lead to
-- corrupted data.
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
                        (_ : _)
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

  etaExpandSynthE i g1 funType0 funExpr0 f xs0

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
      (g4, newVarsWithTypes) <- statefulMapM (\g' t -> do
        let (g'', v) = evarname g' "v"
        return (g'', (v, t))) g3 extraArgTypes

      let newVars = map fst newVarsWithTypes
          appliedExtraTypes = map (apply g4 . snd) newVarsWithTypes

      -- Add type annotations for new bound variables
      let g5 = g4 ++> zipWith AnnG newVars appliedExtraTypes

      -- Create typed variable references for the new parameters
      newVarExprs <- mapM (\(v, t) -> do
        idx <- MM.getCounterWithPos parentIdx
        return $ AnnoS (Idx idx t) idx (BndS v)) (zip newVars appliedExtraTypes)

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
  (g2, t2, xs1) <- foldCheck g1 xs0 t1
  let xs2 = applyCon g2 $ VarS v (MonomorphicExpr (Just t0) xs1)
  return (g2, t2, xs2)
synthE _ g (VarS v (MonomorphicExpr Nothing (x : xs))) = do
  (g', t', x') <- synthG g x
  (g'', t'', xs') <- foldCheck g' xs t'
  let xs'' = applyCon g'' $ VarS v (MonomorphicExpr Nothing (x' : xs'))
  return (g'', t'', xs'')
synthE _ g (VarS v (MonomorphicExpr Nothing [])) = do
  let (g', t) = newvar (unEVar v <> "_u") g
  return (g', t, VarS v (MonomorphicExpr Nothing []))
synthE i g0 (VarS v (PolymorphicExpr cls clsName t0 rs0)) = do
  (g1, rs') <- checkInstances g0 (etype t0) rs0
  let (g2, t1) = rename g1 (etype t0)
  return (g2, t1, VarS v (PolymorphicExpr cls clsName t0 rs'))
  where
    -- check each instance
    -- do not return modified Gamma state
    checkInstances ::
      Gamma ->
      TypeU ->
      [(EType, [AnnoS Int ManyPoly Int])] ->
      MorlocMonad (Gamma, [(EType, [AnnoS (Indexed TypeU) ManyPoly Int])])
    checkInstances g _ [] = return (g, [])
    checkInstances g10 genType ((instType, es) : rs) = do
      -- check this first instance
      -- convert qualified terms in the general type to existentials
      let (g11, genType') = toExistential g10 genType
      -- rename the instance type
      let (g12, instType') = rename g11 (etype instType)
      -- subtype the renamed instance type against the existential general
      g13 <- subtype' i instType' genType' g12
      -- check all implementations for this instance
      (g14, es') <- checkImplementations g13 genType' es

      -- check all remaining instances
      -- Use the ORIGINAL general type, not the existntialized one above.
      -- this means each existential can be solved independently for each
      -- instance.
      (g15, rs') <- checkInstances g14 genType rs

      return (g15, (instType, es') : rs')

    -- check each implementation within each instance
    -- do not return modified Gamma state
    checkImplementations ::
      Gamma ->
      TypeU ->
      [AnnoS Int ManyPoly Int] ->
      MorlocMonad (Gamma, [AnnoS (Indexed TypeU) ManyPoly Int])
    checkImplementations g _ [] = return (g, [])
    checkImplementations g10 t (e : es) = do
      -- check this instance
      (g11, _, e') <- checkG g10 e t

      -- check all the remaining implementations
      (g12, es') <- checkImplementations g11 t es

      -- return the final context and the applied expressions
      return (g12, applyGen g12 e' : es')

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
synthE _ g (LetS v e1 e2) = do
  (g1, t1, e1') <- synthG g e1
  let g2 = g1 ++> [AnnG v t1]
  (g3, t2, e2') <- synthG g2 e2
  return (g3, t2, LetS v e1' e2')

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
    FunU (length -> numParams) _ | numArgs > numParams ->
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
        (g3, newVarsWithTypes) <- statefulMapM (\g' tp -> do
          let (g'', v) = evarname g' "v"
          return (g'', (v, apply g2 tp))) g2 remainingParams

        let newVars = map fst newVarsWithTypes
            newTypes = map snd newVarsWithTypes
            g4 = g3 ++> zipWith AnnG newVars newTypes

        -- Create typed variable references for the new params
        newVarExprs <- mapM (\(v, tp) -> do
          idx <- MM.getCounterWithPos i
          return $ AnnoS (Idx idx tp) idx (BndS v)) newVarsWithTypes

        -- Build the application and lambda directly
        appIdx <- MM.getCounterWithPos i
        let retType = apply g4 t
            bodyExpr = AppS funExpr0 (inputExprs ++ newVarExprs)
            bodyAnno = AnnoS (Idx appIdx retType) appIdx bodyExpr
            fullType = FunU newTypes retType
        return (g4, fullType, LamS newVars bodyAnno)
    _ -> error "impossible"

etaExpand ::
  Gamma ->
  AnnoS Int ManyPoly Int ->
  [AnnoS Int ManyPoly Int] ->
  TypeU ->
  MorlocMonad (Maybe (Gamma, ExprS Int ManyPoly Int))
etaExpand g0 f0 xs0 t0 = etaExpand' g0 f0 xs0 t0
  where
    -- ignore qualification
    etaExpand' g f xs (ForallU _ t) = etaExpand' g f xs t
    -- find the number of applied terms for the term and the type
    etaExpand' g f@(AnnoS gidx _ _) xs@(length -> termSize) (normalizeType -> FunU (length -> typeSize) _)
      -- if they are equal, then the function is fully applied, do nothing
      | termSize == typeSize = return Nothing
      -- if there are fewer terms, then eta expand
      | termSize < typeSize = Just <$> etaExpandE (AppS f xs)
      -- if there are more terms than the type permits, then raise an error
      | otherwise = throwTypeError gidx $ "Invalid function application of type:\n  " <> prettyTypeU t0

      where
        etaExpandE :: ExprS Int ManyPoly Int -> MorlocMonad (Gamma, ExprS Int ManyPoly Int)
        etaExpandE e@(AppS _ _) = tryExpand (typeSize - termSize) e
        etaExpandE e@(LamS vs _) = tryExpand (typeSize - termSize - length vs) e
        etaExpandE e = return (g, e)

        tryExpand n e
          -- A partially applied term intended to return a function (e.g., `(\x y -> add x y) x |- Real -> Real`)
          -- A fully applied term
          | n <= 0 = return (g, e)
          | otherwise = expand gidx n g e
    -- If term that is applied is not a function, do nothing. The application is
    -- not correct, but the error will be caught later.
    etaExpand' _ _ _ _ = return Nothing

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
  case access1 v (gammaContext g0) of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      let (g1, veas) = statefulMap (\g _ -> tvarname g "a_") g0 es
          (g2, vea) = tvarname g1 (s <> "o_")
          eas = [ExistU v' ([], Open) ([], Open) | v' <- veas]
          ea = ExistU vea ([], Open) ([], Open)
          f = FunU eas ea
          g3 = cacheSolved v f $ g2 {gammaContext = rs <> [SolvedG v f] <> map index eas <> [index ea] <> ls}
      (g4, _, es', _) <- zipCheck i g3 es eas
      return (g4, apply g4 f, es')
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g0 of
      (Just (FunU ts t)) -> do
        (g1, ts', es', _) <- zipCheck i g0 es ts
        return (g1, apply g1 (FunU ts' t), es')
      (Just t) -> throwTypeError i $ "Application of term with non-functional type:\n   " <+> prettyTypeU t
      Nothing -> throwTypeError i $ "Expected function, but could not find type of term\n   " <+> pretty v

application i _ _ t = throwTypeError i
  $ "Application of non-functional expression of type:" <+> prettyTypeU t

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
  recordParameter i v a
  checkE' i (g1 +> v) e1 (substitute v a)

--   Sub
checkE i g1 e1 b = do
  (g2, a, e2) <- synthE' i g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  g3 <- subtype' i a' b' g2
  return (g3, apply g3 b', e2)

subtype' :: Int -> TypeU -> TypeU -> Gamma -> MorlocMonad Gamma
subtype' i a b g = do
  scope <- MM.getGeneralScope i
  insetSay $ parens (pretty a) <+> "<:" <+> parens (pretty b)
  case subtype scope a b g of
    (Left err') -> MM.throwSourcedError i err'
    (Right x) -> return x

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

peakSExpr :: ExprS Int ManyPoly Int -> MDoc
peakSExpr UniS = "UniS"
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
