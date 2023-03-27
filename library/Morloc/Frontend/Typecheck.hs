{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Typecheck (typecheck, resolveTypes) where

import Morloc.Frontend.Namespace
import Morloc.Typecheck.Internal
import Morloc.Pretty
import Morloc.Data.Doc
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT

import qualified Control.Monad.State as CMS
import qualified Data.Map as Map
import Data.Bifunctor (first)

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and everything else are abstracted away.
--
-- Check the general types, do nothing to the concrete types which may only be
-- solved after segregation. Later the concrete types will need to be checked
-- for type consistency and correctness of packers.
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed TypeU) Many Int]
-- typecheck xs = error . MT.unpack . render . vsep . map (prettySAnno viaShow viaShow) $ xs
typecheck = mapM run where
    run :: SAnno Int Many Int -> MorlocMonad (SAnno (Indexed TypeU) Many Int)
    run e0 = do
      -- standardize names for lambda bound variables (e.g., x0, x1 ...)
      let g0 = Gamma {gammaCounter = 0, gammaContext = []}
          ((_, g1), e1) = renameSAnno (Map.empty, g0) e0
      (g2, _, e2) <- synthG' g1 e1
      insetSay "-------- leaving frontend typechecker ------------------"
      insetSay "g2:"
      seeGamma g2
      insetSay "e2:"
      -- peakGen e2
      insetSay "========================================================"
      return $ mapSAnno (fmap normalizeType) id . applyGen g2 $ e2

-- TypeU --> Type
resolveTypes :: SAnno (Indexed TypeU) Many Int -> SAnno (Indexed Type) Many Int
resolveTypes (SAnno (Many es) (Idx i t))
  = SAnno (Many (map (first f) es)) (Idx i (typeOf t)) where
  f :: SExpr (Indexed TypeU) Many Int -> SExpr (Indexed Type) Many Int
  f (AccS x k) = AccS (resolveTypes x) k
  f (AppS x xs) = AppS (resolveTypes x) (map resolveTypes xs) 
  f (LamS vs x) = LamS vs (resolveTypes x)
  f (LstS xs) = LstS (map resolveTypes xs)
  f (TupS xs) = TupS (map resolveTypes xs)
  f (NamS rs) = NamS (zip (map fst rs) (map (resolveTypes . snd) rs))
  f (RealS x) = RealS x
  f (IntS x) = IntS x
  f (LogS x) = LogS x
  f (StrS x) = StrS x
  f (CallS x) = CallS x
  f UniS = UniS
  f (VarS x) = VarS x

-- lookup a general type associated with an index
-- standardize naming of qualifiers
lookupType :: Int -> Gamma -> MorlocMonad (Maybe (Gamma, TypeU))
lookupType i g = do
  m <- CMS.gets stateSignatures
  return $ case GMap.lookup i m of
    GMapJust (TermTypes (Just (EType t _ _)) _ _) -> Just $ rename g t
    _ -> Nothing

-- prepare a general, indexed typechecking error
gerr :: Int -> TypeError -> MorlocMonad a
gerr i e = MM.throwError $ IndexedError i (GeneralTypeError e)

synthG
  :: Gamma
  -> SAnno Int Many Int
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
-- it is possible to export just a type signature
synthG g (SAnno (Many []) i) = do
  maybeType <- lookupType i g
  case maybeType of
    (Just (g', t)) -> return (g', t, SAnno (Many []) (Idx i t))
    -- if a term is associated with no expression or type
    Nothing -> do
        maybeName <- CMS.gets (Map.lookup i . stateName)
        case maybeName of
            -- This branch is entered for exported type definitions
            -- FIXME: return all definitions and their parameters, check parameter count
            (Just (EV v)) -> return (g, VarU (TV Nothing v), SAnno (Many []) (Idx i (VarU (TV Nothing v))))
            Nothing -> error "Indexing error, this should not occur, please message the maintainer"

synthG g0 (SAnno (Many ((e0, j):es)) i) = do

  -- Check for any existing type signature annotations
  maybeType <- lookupType i g0
  (g1, t1, e1) <- case maybeType of
    -- If there are no annotations, synthesize
    Nothing  -> synthE' i g0 e0
    -- If there are annotations ...
    (Just (g', t)) -> case e0 of
      -- If the annotation is of a variable name, return the annotation. Calling
      -- check would just re-synthesize the same type and check that it was
      -- equal to itself.
      (VarS v) -> return (g', t, VarS v)
      -- Otherwise check the annotation type
      _ -> checkE' i g' e0 t

  -- Check all other implementations against the first one
  (g2, t2, SAnno (Many es') _) <- checkG' g1 (SAnno (Many es) i) t1

  -- finally cons the head element back and apply everything we learned
  let finalExpr = applyGen g2 $ SAnno (Many ((e1, j):es')) (Idx i t2)

  return (g2, t2, finalExpr)

checkG
  :: Gamma
  -> SAnno Int Many Int
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
checkG g (SAnno (Many []) i) t = return (g, t, SAnno (Many []) (Idx i t)) 
checkG g0 (SAnno (Many ((e, j):es)) i) t0 = do 
  (g1, t1, e') <- checkE' i g0 e t0
  (g2, t2, SAnno (Many es') idType) <- checkG' g1 (SAnno (Many es) i) t1
  return (g2, t2, SAnno (Many ((e', j):es')) idType)


synthE
  :: Int
  -> Gamma
  -> SExpr Int Many Int
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )

synthE _ g UniS = return (g, MLD.defaultGeneralType UniS, UniS)
synthE _ g (RealS x) = return (g, MLD.defaultGeneralType (RealS x), RealS x)
synthE _ g (IntS x) = return (g, MLD.defaultGeneralType (IntS x), IntS x)
synthE _ g (LogS x) = return (g, MLD.defaultGeneralType (LogS x), LogS x)
synthE _ g (StrS x) = return (g, MLD.defaultGeneralType (StrS x), StrS x)

synthE i g (AccS e k) = do
  (g1, t1, e1) <- synthG' g e
  insetSay "accs"
  insetSay $ "t1:" <+> pretty t1
  seeGamma g1
  valType <- case t1 of
    (NamU _ _ _ rs) -> case lookup k rs of
      Nothing -> gerr i (KeyError k t1)
      (Just t) -> return t
    _ -> gerr i (KeyError k t1)
  return (g1, valType, AccS e1 k)

--   -->E0
synthE _ g (AppS f []) = do
  (g1, t1, f1) <- synthG' g f
  return (g1, t1, AppS f1 [])

--   -->E
synthE i g0 (AppS f xs0) = do
  -- synthesize the type of the function
  (g1, funType0, funExpr0) <- synthG g0 f

  -- eta expand
  mayExpanded <- etaExpand g1 f xs0 funType0

  case mayExpanded of
    -- If the term was eta-expanded, retypecheck it
    (Just (g', x')) -> synthE i g' x'
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

--   -->I==>
synthE i g0 f@(LamS vs x) = do
  (g1, bodyType, _) <- synthG g0 x

  let n = nfargs bodyType
  if n > 0
    then do
      (g2, f2) <- expand n g1 f
      insetSay $ "Expanded in -->I==>:" <+> prettySExpr (const "") (const "") f2
      synthE i g2 f2
    else do
      -- create existentials for everything and pass it off to check
      let (g2, ts) = statefulMap (\g' v -> newvar (unEVar v <> "_x") Nothing g') g1 vs
          (g3, ft) = newvar "o_" Nothing g2
          finalType = FunU ts ft
      checkE' i g3 f finalType
  where
    nfargs :: TypeU -> Int
    nfargs (FunU ts _) = length ts
    nfargs (ForallU _ f') = nfargs f'
    nfargs _ = 0

--   List
synthE _ g (LstS []) =
  let (g1, itemType) = newvar "itemType_" Nothing g
      listType = head $ MLD.defaultList Nothing itemType
  in return (g1, listType, LstS [])
synthE i g (LstS (e:es)) = do
  (g1, itemType, itemExpr) <- synthG' g e 
  (g2, listType, listExpr) <- checkE' i g1 (LstS es) (head $ MLD.defaultList Nothing itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr:es'))
    _ -> error "impossible"

--   Tuple
synthE _ g (TupS []) =
  let t = head $ MLD.defaultTuple Nothing []
  in return (g, t, TupS [])
synthE i g (TupS (e:es)) = do
  -- synthesize head
  (g1, itemType, itemExpr) <- synthG' g e

  -- synthesize tail
  (g2, tupleType, tupleExpr) <- synthE' i g1 (TupS es)

  -- merge the head and tail
  t3 <- case tupleType of
    (AppU _ ts) -> return . head $ MLD.defaultTuple Nothing (apply g2 itemType : ts)
    _ -> error "impossible" -- the general tuple will always be (AppU _ _)

  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t3, TupS (itemExpr:xs'))

--   Records
synthE _ g (NamS []) = return (g, head $ MLD.defaultRecord Nothing [], NamS [])
synthE i g0 (NamS ((k,x):rs)) = do
  insetSay $ "Entering synthE NamS (k=" <> pretty k <> ")"
  seeGamma g0
  insetSay "-------- syn"
  -- type the head
  (g1, headType, headExpr) <- synthG' g0 x

  -- type the tail
  (g2, tailType, tailExpr) <- synthE' i g1 (NamS rs)

  insetSay $ "Exiting synthE NamS (k=" <> pretty k <> ")"
  insetSay $ "  k type:" <+> pretty headType
  seeGamma g2
  insetSay "-------- syn"

  -- merge the head with tail
  t <- case tailType of
    (NamU o1 n1 ps1 rs1) -> return $ NamU o1 n1 ps1 ((k, apply g2 headType):rs1)
    _ -> error "impossible" -- the synthE on NamS will always return NamU type

  tailExprs <- case tailExpr of
    (NamS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t, NamS ((k, headExpr):tailExprs))

-- Sources are axiomatic. They are they type they are said to be.
synthE i g (CallS src) = do
  maybeType <- lookupType i g 
  (g', t) <- case maybeType of
    Just x -> return x
    -- no, then I don't know what it is and will return an existential
    -- if this existential is never solved, then it will become universal later 
    Nothing -> return $ newvar "src_"  Nothing g
  return (g', t, CallS src)

-- Any morloc variables should have been expanded by treeify. Any bound
-- variables should be checked against. I think (this needs formalization).
synthE i g (VarS v) = do
  -- is this a bound variable that has already been solved
  (g', t') <- case lookupE v g of 
    -- yes, return the solved type
    (Just t) -> return (g, t)
    Nothing -> do
    -- no, so is it a variable that has a type annotation?
      maybeType <- lookupType i g 
      case maybeType of
        Just x -> return x 
        -- no, then I don't know what it is and will return an existential
        -- if this existential is never solved, then it will become universal later 
        Nothing -> return $ newvar (unEVar v <> "_u")  Nothing g
  return (g', t', VarS v)


etaExpand :: Gamma -> SAnno Int Many Int -> [SAnno Int Many Int] -> TypeU -> MorlocMonad (Maybe (Gamma, SExpr Int Many Int))
etaExpand g0 f0 xs0@(length -> termSize) (normalizeType -> FunU (length -> typeSize) _)
    | termSize == typeSize = return Nothing 
    | otherwise = Just <$> etaExpandE g0 (AppS f0 xs0)
    where

    etaExpandE :: Gamma -> SExpr Int Many Int -> MorlocMonad (Gamma, SExpr Int Many Int)
    etaExpandE g e@(AppS _ _) = tryExpand (typeSize - termSize) g e
    etaExpandE g e@(LamS vs _) = tryExpand (typeSize - termSize - length vs) g e
    etaExpandE g e = return (g, e)

    tryExpand n g e
        -- A partially applied term intended to return a function (e.g., `(\x y -> add x y) x |- Real -> Real`)
        -- A fully applied term
        | n <= 0 = return (g, e)
        | otherwise = expand n g e

etaExpand _ _ _ _ = return Nothing


expand :: Int -> Gamma -> SExpr Int Many Int -> MorlocMonad (Gamma, SExpr Int Many Int)
expand 0 g x = return (g, x)
expand n g e@(AppS _ _) = do
    newIndex <- MM.getCounter
    let (g', v') = evarname g "v"
    e' <- applyExistential v' e
    let x' = LamS [v'] (SAnno (Many [(e', newIndex)]) newIndex)
    expand (n-1) g' x'
expand n g (LamS vs' (SAnno (Many es0') t)) = do
    let (g', v') = evarname g "v"
    es1' <- mapM (applyExistential v' . fst) es0'
    expand (n-1) g' (LamS (vs' <> [v']) (SAnno (Many (zip es1' (map snd es0'))) t))
expand _ g x = return (g, x)


applyExistential :: EVar -> SExpr Int Many Int -> MorlocMonad (SExpr Int Many Int)
applyExistential v' (AppS f xs') = do
    newIndex <- MM.getCounter  
    return $ AppS f (xs' <> [SAnno (Many [(VarS v', newIndex)]) newIndex])
-- possibly illegal application, will type check after expansion
applyExistential v' e = do
    appIndex <- MM.getCounter
    varIndex <- MM.getCounter
    return $ AppS (SAnno (Many [(e, appIndex)]) appIndex) [SAnno (Many [(VarS v', varIndex)]) varIndex]



application
  :: Int
  -> Gamma
  -> [SAnno Int Many Int] -- the expressions that are passed to the function
  -> TypeU -- the function type
  -> MorlocMonad
      ( Gamma
      , TypeU -- output function type
      , [SAnno (Indexed TypeU) Many Int] -- @e@, with type annotation
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
application i g0 es (ExistU v@(TV _ s) [] _) =
  case access1 v (gammaContext g0) of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      let (g1, veas) = statefulMap (\g _ -> tvarname g "a_" Nothing) g0 es
          (g2, vea) = tvarname g1 (s <> "o_") Nothing
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
  -> [SAnno Int Many Int]
  -> [TypeU]
  -> MorlocMonad
    ( Gamma
    , [TypeU]
    , [SAnno (Indexed TypeU) Many Int]
    , [TypeU] -- remainder
    )
-- check the first elements, cdr down the remaining values
zipCheck i g0 (x0:xs0) (t0:ts0) = do
  (g1, t1, x1) <- checkG' g0 x0 t0
  (g2, ts1, xs1, remainder) <- zipCheck i g1 xs0 ts0
  return (g2, t1:ts1, x1:xs1, remainder)
-- If there are fewer arguments than types, this may be OK, just partial application
zipCheck _ g0 [] ts = return (g0, [], [], ts)
-- If there are fewer types than arguments, then die
zipCheck i _ _ [] = gerr i TooManyArguments


checkE
  :: Int
  -> Gamma
  -> SExpr Int Many Int
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
checkE i g1 (LstS (e:es)) (AppU v [t]) = do
  (g2, t2, e') <- checkG' g1 e t 
  -- LstS [] will go to the normal Sub case
  (g3, t3, LstS es') <- checkE i g2 (LstS es) (AppU v [t2])
  return (g3, t3, LstS (map (applyGen g3) (e':es')))

checkE i g0 e0@(LamS vs body) t@(FunU as b)
    | length vs == length as = do
        let g1 = g0 ++> zipWith AnnG vs as
        (g2, t2, e2) <- checkG' g1 body b 

        let t3 = apply g2 (FunU as t2)
            e3 = applyCon g2 (LamS vs e2)

        return (g2, t3, e3)

    | otherwise = do 
        (g', e') <- expand (length as - length vs) g0 e0
        checkE i g' e' t

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

-- apply context to a SAnno
applyGen :: (Functor gf, Functor f, Applicable g)
         => Gamma -> SAnno (gf g) f c -> SAnno (gf g) f c
applyGen g = mapSAnno (fmap (apply g)) id

applyCon :: (Functor gf, Functor f, Applicable g)
         => Gamma -> SExpr (gf g) f c -> SExpr (gf g) f c
applyCon g = mapSExpr (fmap (apply g)) id


---- debugging

synthG' g x = do
  enter "synthG"
  r <- synthG g x
  leave "synthG"
  return r

checkG' g x t = do
  enter "checkG"
  r <- checkG g x t
  leave "checkG"
  return r

synthE' i g x = do
  enter "synthE"
  -- peak x
  seeGamma g
  r@(g', t, x') <- synthE i g x 
  leave "synthE"
  -- peak x'
  seeGamma g'
  seeType t
  return r

checkE' i g x t = do
  enter "checkE"
  -- peak x
  seeType t
  seeGamma g
  r@(g', t', x') <- checkE i g x t 
  leave "checkE"
  -- peak x'
  seeType t'
  seeGamma g'
  return r

application' i g es t = do
  enter "application"
  seeGamma g
  seeType t
  -- mapM_ peakGen es
  r@(g',t',es') <- application i g es t
  leave "application"
  seeGamma g'
  seeType t'
  -- mapM_ peakGen es'
  return r
