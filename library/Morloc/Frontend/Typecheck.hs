{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Typecheck (typecheck, resolveTypes, evaluateAnnoSTypes, peakSExpr) where

import Morloc.Frontend.Namespace
import Morloc.Typecheck.Internal
import Morloc.Data.Doc
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

import qualified Data.Map as Map

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and everything else are abstracted away.
--
-- Check the general types, do nothing to the concrete types which may only be
-- solved after segregation. Later the concrete types will need to be checked
-- for type consistency and correctness of packers.
typecheck
  :: [AnnoS Int ManyPoly Int]
  -> MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheck = mapM run where
    run :: AnnoS Int ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
    run e0 = do

      -- s <- MM.gets stateSignatures
      -- MM.sayVVV $ "stateSignatures:\n  " <> pretty s

      -- standardize names for lambda bound variables (e.g., x0, x1 ...)
      let g0 = Gamma {gammaCounter = 0, gammaContext = []}
          ((_, g1), e1) = renameAnnoS (Map.empty, g0) e0
      (g2, _, e2) <- synthG g1 e1
      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g2:"
      seeGamma g2
      insetSay "========================================================"
      let e3 = mapAnnoSG (fmap normalizeType) . applyGen g2 $ e2

      resolveInstances g2 (applyGen g2 e3)

      -- s2 <- MM.gets stateSignatures
      -- MM.sayVVV $ "resolved stateSignatures:\n  " <> pretty s2
      --
      -- return e4

-- TypeU --> Type
resolveTypes :: AnnoS (Indexed TypeU) Many Int -> AnnoS (Indexed Type) Many Int
resolveTypes (AnnoS (Idx i t) ci e)
  = AnnoS (Idx i (typeOf t)) ci (f e) where
  f :: ExprS (Indexed TypeU) Many Int -> ExprS (Indexed Type) Many Int
  f (BndS x) = BndS x
  f (VarS v xs) = VarS v (fmap resolveTypes xs)
  f (CallS src) = CallS src
  f (AccS k x) = AccS k (resolveTypes x)
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

resolveInstances :: Gamma -> AnnoS (Indexed TypeU) ManyPoly Int -> MorlocMonad (AnnoS (Indexed TypeU) Many Int)
resolveInstances g (AnnoS gi@(Idx _ gt) ci e0) = AnnoS gi ci <$> f e0 where
  f :: ExprS (Indexed TypeU) ManyPoly Int -> MorlocMonad (ExprS (Indexed TypeU) Many Int)

  -- resolve instances
  f (VarS v (PolymorphicExpr _ _ _ rss)) = do
        -- collect all implementations and apply context
    let es = [AnnoS (Idx i (apply g t)) c e | (AnnoS (Idx i t) c e) <- concatMap snd rss]
        -- find the types of the most specific instances that are subtypes of the inferred type
        mostSpecificTypes = mostSpecificSubtypes gt [t | (AnnoS (Idx _ t) _ _) <- es]
        -- filter out the most specific subtype expressions
        es' = [AnnoS (Idx i t) c e | (AnnoS (Idx i t) c e) <- es, t `elem` mostSpecificTypes]
    VarS v . Many <$> mapM (resolveInstances g) es'

  f (VarS v (MonomorphicExpr _ xs)) = VarS v . Many <$> mapM (resolveInstances g) xs

  -- propagate
  f (AccS k e) = AccS k <$> resolveInstances g e
  f (AppS e es) = AppS <$> resolveInstances g e <*> mapM (resolveInstances g) es
  f (LamS vs e) = LamS vs <$> resolveInstances g e
  f (LstS es) = LstS <$> mapM (resolveInstances g) es
  f (TupS es) = TupS <$> mapM (resolveInstances g) es
  f (NamS rs) = NamS <$> mapM (secondM (resolveInstances g)) rs

  -- primitives
  f UniS = return UniS
  f (BndS v) = return $ BndS v
  f (RealS x) = return $ RealS x
  f (IntS x) = return $ IntS x
  f (LogS x) = return $ LogS x
  f (StrS x) = return $ StrS x
  f (CallS x) = return $ CallS x


-- prepare a general, indexed typechecking error
gerr :: Int -> TypeError -> MorlocMonad a
gerr i e = MM.throwError $ IndexedError i (GeneralTypeError e)


checkG
  :: Gamma
  -> AnnoS Int ManyPoly Int
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , AnnoS (Indexed TypeU) ManyPoly Int
       )
checkG g (AnnoS i j e) t = do
  (g', t', e') <- checkE' i g e t
  return (g', t', AnnoS (Idx i t') j e')

synthG
  :: Gamma
  -> AnnoS Int ManyPoly Int
  -> MorlocMonad
       ( Gamma
       , TypeU
       , AnnoS (Indexed TypeU) ManyPoly Int
       )
synthG g (AnnoS gi ci e) = do
  (g', t, e') <- synthE' gi g e
  return (g', t, AnnoS (Idx gi t) ci e')

synthE
  :: Int
  -> Gamma
  -> ExprS Int ManyPoly Int
  -> MorlocMonad
       ( Gamma
       , TypeU
       , ExprS (Indexed TypeU) ManyPoly Int
       )

synthE _ g UniS = return (g, BT.unitU, UniS)
synthE _ g (RealS x) = return (g, BT.realU, RealS x)
synthE _ g (IntS x) = return (g, BT.intU, IntS x)
synthE _ g (LogS x) = return (g, BT.boolU, LogS x)
synthE _ g (StrS x) = return (g, BT.strU, StrS x)

synthE i g0 (AccS k e) = do
  (g1, t1, e1) <- synthG g0 e
  (g2, valType) <- accessRecord g1 t1
  return (g2, valType, AccS k e1)
  where
    accessRecord :: Gamma -> TypeU -> MorlocMonad (Gamma, TypeU)
    accessRecord g t@(NamU _ _ _ rs) = case lookup k rs of
      Nothing -> gerr i (KeyError k t)
      (Just value) -> return (g, value)
    accessRecord g t@(ExistU v ps rs) = case lookup k rs of
      Nothing -> do
        let (g', value) = newvar (unTVar v <> "_" <> unKey k) g
        case access1 v (gammaContext g') of
          (Just (rhs, _, lhs)) -> return (g' { gammaContext = rhs <> [ExistG v ps ((k, value):rs)] <> lhs }, value)
          Nothing -> do
            MM.sayVVV $ "Case b"
                      <> "\n  rs:" <+> pretty rs
                      <> "\n  v:" <+> pretty v
            gerr i (KeyError k t)
      (Just value) -> return (g, value)
    accessRecord g t = do
      globalMap <- MM.gets stateGeneralTypedefs
      gscope <- case GMap.lookup i globalMap of
        GMapJust scope -> return scope
        _ -> return Map.empty
      case TE.evaluateStep gscope t of
        (Just t') -> accessRecord g t'
        Nothing -> gerr i (KeyError k t)

--   -->E0
synthE _ g (AppS f []) = do
  (g1, t1, f1) <- synthG g f
  return (g1, t1, AppS f1 [])

--   -->E
synthE i g0 (AppS f xs0) = do
  -- synthesize the type of the function
  (g1, funType0, funExpr0) <- synthG g0 f

  -- eta expand
  mayExpanded <- etaExpand g1 f xs0 funType0

  case mayExpanded of
    -- If the term was eta-expanded, retypecheck it
    (Just (g', x')) -> synthE' i g' x'
    -- Otherwise proceed
    Nothing -> do

      -- extend the function type with the type of the expressions it is applied to
      (g2, funType1, inputExprs) <- application' i g1 xs0 (normalizeType funType0)

      -- determine the type after application
      appliedType <- case funType1 of
        (FunU ts t) -> case drop (length inputExprs) ts of
          [] -> return t -- full application
          rs -> return $ FunU rs t -- partial application
        _ -> error "impossible"

      -- put the AppS back together with the synthesized function and input expressions
      return (g2, apply g2 appliedType, AppS (applyGen g2 funExpr0) inputExprs)

-- -->I==>
synthE i g0 f@(LamS vs x) = do
  (_, bodyType, _) <- synthG g0 x

  -- FIXME: Repeated inference here may lead to exponential runtime
  -- There must be a better way to handle eta reduction ...
  let n = nfargs bodyType
  if n > 0
    then do
      (g1, f2) <- expand n g0 f
      synthE' i g1 f2   -- <----- repeat inference -----------------------------
    else do
      -- create existentials for everything and pass it off to check
      let (g1, ts) = statefulMap (\g' v -> newvar (unEVar v <> "_x") g') g0 vs
          (g2, ft) = newvar "o_" g1
          finalType = FunU ts ft
      checkE' i g2 f finalType   -- <----- repeat inference --------------------
  where
    nfargs :: TypeU -> Int
    nfargs (FunU ts _) = length ts
    nfargs (ForallU _ f') = nfargs f'
    nfargs _ = 0


--   List
synthE _ g (LstS []) =
  let (g1, itemType) = newvar "itemType_" g
      listType = BT.listU itemType
  in return (g1, listType, LstS [])
synthE i g (LstS (e:es)) = do
  (g1, itemType, itemExpr) <- synthG g e
  (g2, listType, listExpr) <- checkE' i g1 (LstS es) (BT.listU itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr:es'))
    _ -> error "impossible"

--   Tuple
synthE _ g (TupS []) =
  let t = BT.tupleU []
  in return (g, t, TupS [])
synthE i g (TupS (e:es)) = do
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

  return (g2, t3, TupS (itemExpr:xs'))

synthE _ g0 (NamS rs) = do
  (g1, xs) <- statefulMapM (\s v -> synthG s v |>> (\(a,b,c) -> (a,(b,c)))) g0 (map snd rs)
  let (ts, es) = unzip xs
      ks = map fst rs
      (g2, t) = newvarRich [] (zip ks ts) "record_" g1
      e = NamS (zip ks es)
  return (g2, t, e)

-- Any morloc variables should have been expanded by treeify. Any bound
-- variables should be checked against. I think (this needs formalization).
synthE _ g0 (VarS v (MonomorphicExpr (Just t0) xs0)) = do
  let (g1, t1) = rename g0 (etype t0)
  (g2, t2, xs1) <- foldCheck g1 xs0 t1
  let xs2 = applyCon g2 $ VarS v (MonomorphicExpr (Just t0) xs1)
  return (g2, t2, xs2)

synthE _ g (VarS v (MonomorphicExpr Nothing (x:xs))) = do
  (g', t', x') <- synthG g x
  (g'', t'', xs') <- foldCheck g' xs t'
  let xs'' = applyCon g'' $ VarS v (MonomorphicExpr Nothing (x':xs'))
  return (g'', t'', xs'')

synthE _ g (VarS v (MonomorphicExpr Nothing [])) = do
  let (g', t) = newvar (unEVar v <> "_u") g
  return (g', t, VarS v (MonomorphicExpr Nothing []))

synthE i g0 (VarS v (PolymorphicExpr cls clsName t0 rs0)) = do
  let (g1, t1) = toExistential g0 (etype t0)
  rs' <- checkInstances g1 t1 rs0
  return (g1, t1, VarS v (PolymorphicExpr cls clsName t0 rs'))

  where

    -- check each instance
    -- do not return modified Gamma state
    checkInstances
      :: Gamma
      -> TypeU
      -> [(EType, [AnnoS Int ManyPoly Int])]
      -> MorlocMonad [(EType, [AnnoS (Indexed TypeU) ManyPoly Int])]
    checkInstances _ _ [] = return []
    checkInstances g10 genType ((instType, es):rs) = do
      rs' <- checkInstances g10 genType rs
      g11 <- subtype' i (etype instType) genType g10
      es' <- checkImplementations g11 genType es
      return ((instType, es'):rs')

    -- check each implementation within each instance
    -- do not return modified Gamma state
    checkImplementations
      :: Gamma
      -> TypeU
      -> [AnnoS Int ManyPoly Int]
      -> MorlocMonad [AnnoS (Indexed TypeU) ManyPoly Int]
    checkImplementations _ _ [] = return []
    checkImplementations g t (e:es) = do
      es' <- checkImplementations g t es
      (_, _, e') <- checkG g e t
      return (e':es')

-- This case will only be encountered in check, the existential generated here
-- will be subtyped against the type known from the VarS case.
synthE _ g (CallS src) = do
  let (g', t) = newvar "call_" g
  return (g', t, CallS src)

synthE _ g (BndS v) = do
  (g', t') <- case lookupE v g of
    -- yes, return the solved type
    (Just t) -> return (g, t)
    -- no, then I don't know what it is and will return an existential
    -- if this existential is never solved, then it will become universal later
    Nothing -> return $ newvar (unEVar v <> "_u") g
  return (g', t', BndS v)


etaExpand :: Gamma -> AnnoS Int f Int -> [AnnoS Int f Int] -> TypeU -> MorlocMonad (Maybe (Gamma, ExprS Int f Int))
etaExpand g0 f0 xs0@(length -> termSize) (normalizeType -> FunU (length -> typeSize) _)
    | termSize == typeSize = return Nothing
    | otherwise = Just <$> etaExpandE g0 (AppS f0 xs0)
    where

    etaExpandE :: Gamma -> ExprS Int f Int -> MorlocMonad (Gamma, ExprS Int f Int)
    etaExpandE g e@(AppS _ _) = tryExpand (typeSize - termSize) g e
    etaExpandE g e@(LamS vs _) = tryExpand (typeSize - termSize - length vs) g e
    etaExpandE g e = return (g, e)

    tryExpand n g e
        -- A partially applied term intended to return a function (e.g., `(\x y -> add x y) x |- Real -> Real`)
        -- A fully applied term
        | n <= 0 = return (g, e)
        | otherwise = expand n g e

etaExpand _ _ _ _ = return Nothing


expand :: Int -> Gamma -> ExprS Int f Int -> MorlocMonad (Gamma, ExprS Int f Int)
expand 0 g x = return (g, x)
expand n g e@(AppS _ _) = do
    newIndex <- MM.getCounter
    let (g', v') = evarname g "v"
    e' <- applyExistential v' e
    let x' = LamS [v'] (AnnoS newIndex newIndex e')
    expand (n-1) g' x'
expand n g (LamS vs' (AnnoS t ci e)) = do
    let (g', v') = evarname g "v"
    e' <- applyExistential v' e
    expand (n-1) g' (LamS (vs' <> [v']) (AnnoS t ci e'))
expand _ g x = return (g, x)


applyExistential :: EVar -> ExprS Int f Int -> MorlocMonad (ExprS Int f Int)
applyExistential v' (AppS f xs') = do
    newIndex <- MM.getCounter
    return $ AppS f (xs' <> [AnnoS newIndex newIndex (BndS v')])
-- possibly illegal application, will type check after expansion
applyExistential v' e = do
    appIndex <- MM.getCounter
    varIndex <- MM.getCounter
    return $ AppS (AnnoS appIndex appIndex e) [AnnoS varIndex varIndex (BndS v')]



application
  :: Int
  -> Gamma
  -> [AnnoS Int ManyPoly Int] -- the expressions that are passed to the function
  -> TypeU -- the function type
  -> MorlocMonad
      ( Gamma
      , TypeU -- output function type
      , [AnnoS (Indexed TypeU) ManyPoly Int] -- @e@, with type annotation
      )

--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
application i g0 es0 (FunU as0 b0) = do
  (g1, as1, es1, remainder) <- zipCheck i g0 es0 as0
  let es2 = map (applyGen g1) es1
      funType = apply g1 $ FunU (as1 <> remainder) b0
  insetSay $ "remainder:" <+> vsep (map pretty remainder)
  return (g1, funType, es2)

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
application i g0 es (ForallU v s) = application' i (g0 +> v) es (substitute v s)

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
application i g0 es (ExistU v@(TV s) [] _) =
  case access1 v (gammaContext g0) of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      let (g1, veas) = statefulMap (\g _ -> tvarname g "a_") g0 es
          (g2, vea) = tvarname g1 (s <> "o_")
          eas = [ExistU v' [] [] | v' <- veas]
          ea = ExistU vea [] []
          f = FunU eas ea
          g3 = g2 {gammaContext = rs <> [SolvedG v f] <> map index eas <> [index ea] <> ls}
      (g4, _, es', _) <- zipCheck i g3 es eas
      return (g4, apply g4 f, map (applyGen g4) es')
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g0 of
      (Just (FunU ts t)) -> do
        (g1, ts', es', _) <- zipCheck i g0 es ts
        return (g1, apply g1 (FunU ts' t), es')
      _ -> gerr i ApplicationOfNonFunction

application i _ _ _ = do
  gerr i ApplicationOfNonFunction



-- Tip together the arguments passed to an application
zipCheck
  :: Int
  -> Gamma
  -> [AnnoS Int ManyPoly Int]
  -> [TypeU]
  -> MorlocMonad
    ( Gamma
    , [TypeU]
    , [AnnoS (Indexed TypeU) ManyPoly Int]
    , [TypeU] -- remainder
    )
-- check the first elements, cdr down the remaining values
zipCheck i g0 (x0:xs0) (t0:ts0) = do
  (g1, t1, x1) <- checkG g0 x0 t0
  (g2, ts1, xs1, remainder) <- zipCheck i g1 xs0 ts0
  return (g2, t1:ts1, x1:xs1, remainder)
-- If there are fewer arguments than types, this may be OK, just partial application
zipCheck _ g0 [] ts = return (g0, [], [], ts)
-- If there are fewer types than arguments, then die
zipCheck i _ _ [] = gerr i TooManyArguments


foldCheck
  :: Gamma
  -> [AnnoS Int ManyPoly Int]
  -> TypeU
  -> MorlocMonad (Gamma, TypeU, [AnnoS (Indexed TypeU) ManyPoly Int])
foldCheck g [] t = return (g, t, [])
foldCheck g (x:xs) t = do
  (g', t', x') <- checkG g x t
  (g'', t'', xs') <- foldCheck g' xs t'
  return (g'', t'', x':xs')


checkE
  :: Int
  -> Gamma
  -> ExprS Int ManyPoly Int
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , ExprS (Indexed TypeU) ManyPoly Int
       )
checkE i g1 (LstS (e:es)) (AppU v [t]) = do
  (g2, t2, e') <- checkG g1 e t
  -- LstS [] will go to the normal Sub case
  (g3, t3, LstS es') <- checkE' i g2 (LstS es) (AppU v [t2])
  return (g3, t3, LstS (map (applyGen g3) (e':es')))

checkE i g0 e0@(LamS vs body) t@(FunU as b)
    | length vs == length as = do
        let g1 = g0 ++> zipWith AnnG vs as
        (g2, t2, e2) <- checkG g1 body b

        let t3 = apply g2 (FunU as t2)
            e3 = applyCon g2 (LamS vs e2)

        return (g2, t3, e3)

    | otherwise = do
        (g', e') <- expand (length as - length vs) g0 e0
        checkE' i g' e' t

checkE i g1 e1 (ForallU v a) = checkE' i (g1 +> v) e1 (substitute v a)

--   Sub
checkE i g1 e1 b = do
  (g2, a, e2) <- synthE' i g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  g3 <- subtype' i a' b' g2
  return (g3, apply g3 b', e2)

subtype' :: Int -> TypeU -> TypeU -> Gamma -> MorlocMonad Gamma
subtype' i a b g = do
  insetSay $ parens (pretty a) <+> "<:" <+> parens (pretty b)
  case subtype a b g of
    (Left err') -> gerr i err'
    (Right x) -> return x


-- helpers

-- apply context to a AnnoS
applyGen :: (Functor gf, Traversable f, Applicable g)
         => Gamma -> AnnoS (gf g) f c -> AnnoS (gf g) f c
applyGen g = mapAnnoSG (fmap (apply g))

applyCon :: (Functor gf, Traversable f, Applicable g)
         => Gamma -> ExprS (gf g) f c -> ExprS (gf g) f c
applyCon g = mapExprSG (fmap (apply g))

evaluateAnnoSTypes :: Traversable f => AnnoS (Indexed TypeU) f Int -> MorlocMonad (AnnoS (Indexed TypeU) f Int)
evaluateAnnoSTypes = mapAnnoSGM resolve where
  resolve :: Indexed TypeU -> MorlocMonad (Indexed TypeU)
  resolve (Idx m t) = do
    scope <- getScope m
    case TE.evaluateType scope t of
      (Left e) -> MM.throwError e
      (Right tu) -> return (Idx m tu)

  getScope :: Int -> MorlocMonad Scope
  getScope i= do
    globalMap <- MM.gets stateGeneralTypedefs
    case GMap.lookup i globalMap of
      GMapNoFst -> return Map.empty
      GMapNoSnd -> return Map.empty
      GMapJust scope -> return scope


---- debugging


synthE'
  :: Int
  -> Gamma
  -> ExprS Int ManyPoly Int
  -> MorlocMonad
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


checkE'
  :: Int
  -> Gamma
  -> ExprS Int ManyPoly Int
  -> TypeU
  -> MorlocMonad
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


application'
  :: Int
  -> Gamma
  -> [AnnoS Int ManyPoly Int]
  -> TypeU
  -> MorlocMonad
      ( Gamma
      , TypeU
      , [AnnoS (Indexed TypeU) ManyPoly Int]
      )
application' i g es t = do
  enter "application"
  seeType t
  insetSay $ "es:" <+> list [peakSExpr e | (AnnoS _ _ e) <- es]
  r@(g',t',_) <- application i g es t
  leave "application"
  seeGamma g'
  seeType t'
  return r

peakSExpr :: ExprS Int ManyPoly Int -> MDoc
peakSExpr UniS = "UniS"
peakSExpr (VarS v (MonomorphicExpr mayT _)) = "VarS" <+> pretty v <+> "::" <+> maybe "?" pretty mayT
peakSExpr (VarS v (PolymorphicExpr cls _ t _)) = "VarS" <+> pretty cls <+> " => " <+> pretty v <+> "::" <+> pretty t
peakSExpr (BndS v) = "BndS" <+> pretty v
peakSExpr (AccS k _) = "AccS" <> brackets (pretty k)
peakSExpr (AppS _ xs) = "AppS" <+> "nargs=" <> pretty (length xs)
peakSExpr (LamS vs _) = "LamS" <> tupled (map pretty vs)
peakSExpr (LstS xs) = "LstS" <> "n=" <> pretty (length xs)
peakSExpr (TupS xs) = "TupS" <> "n=" <> pretty (length xs)
peakSExpr (NamS rs) = "NamS" <> encloseSep "{" "}" "," (map (pretty . fst) rs)
peakSExpr (RealS x) = "RealS" <+> viaShow x
peakSExpr (IntS x) = "IntS" <+> pretty x
peakSExpr (LogS x) = "LogS" <+> pretty  x
peakSExpr (StrS x) = "StrS" <+> pretty x
peakSExpr (CallS src) = "CallS" <+> pretty src
