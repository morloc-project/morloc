{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Frontend.Valuecheck
Description : Check for contradictions between implementations
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Valuecheck (valuecheck, checkPair) where

import qualified Data.Set as Set
import qualified Data.Text as DT
import Morloc.Data.Doc
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM

-- Convert AnnoS objects to a simple intermediate type
toE :: AnnoS (Indexed Type) Many Int -> E
toE (AnnoS g _ UniS) = LitP g MUni
toE (AnnoS g _ (BndS v)) = BndP g v
toE (AnnoS g _ (VarS v (Many es))) = VarP g v (map toE es)
toE (AnnoS g _ (AppS e es)) = AppP g (toE e) (map toE es)
toE (AnnoS g _ (LamS vs e)) = LamP g vs (toE e)
toE (AnnoS g _ (LstS es)) = LstP g (map toE es)
toE (AnnoS g _ (TupS es)) = TupP g (map toE es)
toE (AnnoS g _ (NamS rs)) = NamP g (map (second toE) rs)
toE (AnnoS g _ (RealS x)) = LitP g (MNum x)
toE (AnnoS g _ (IntS x)) = LitP g (MInt x)
toE (AnnoS g _ (LogS x)) = LitP g (MLog x)
toE (AnnoS g _ (StrS x)) = LitP g (MStr x)
toE (AnnoS g _ (ExeS (SrcCall s))) = SrcP g s
toE (AnnoS g _ (ExeS (PatCall (PatternText s ss)))) =
  LitP g (MStr (s <> DT.concat ["#{}" <> s' | s' <- ss]))
toE (AnnoS g _ (ExeS (PatCall (PatternStruct s)))) = PatP g s
toE (AnnoS g _ (LetBndS v)) = BndP g v
toE (AnnoS _ _ (LetS _ _ body)) = toE body

indexOfE :: E -> Int
indexOfE (BndP (Idx i _) _) = i
indexOfE (VarP (Idx i _) _ _) = i
indexOfE (AppP (Idx i _) _ _) = i
indexOfE (LamP (Idx i _) _ _) = i
indexOfE (LstP (Idx i _) _) = i
indexOfE (TupP (Idx i _) _) = i
indexOfE (NamP (Idx i _) _) = i
indexOfE (LitP (Idx i _) _) = i
indexOfE (SrcP (Idx i _) _) = i
indexOfE (PatP (Idx i _) _) = i

-- Check the harmony of typed implementations.
--
-- A naive implementation of this functions (and mine is naive as heck) will run
-- in exponential time in some cases. This can be avoided with a touch of
-- memoization. But I will leave that as an exercise for my user (PR's accepted).
valuecheck :: AnnoS (Indexed Type) Many Int -> MorlocMonad (AnnoS (Indexed Type) Many Int)
valuecheck e0 = check (toE e0) >> return e0

-- walk through a tree
-- find the sets of implementations in VarS expressions
-- compare all pairs of implementations
check :: E -> MorlocMonad ()
check (VarP (Idx i _) _ es) = mapM_ (uncurry (checkPair i)) (pairwise es)
  where
    -- find all unique pairs
    pairwise :: [a] -> [(a, a)]
    pairwise xs = [(xs !! i', xs !! j') | i' <- [0 .. length xs - 1], j' <- [0 .. length xs - 1], j' > i']
check (AppP _ e es) = mapM_ check (e : es)
check (LamP _ _ e) = check e
check (LstP _ es) = mapM_ check es
check (TupP _ es) = mapM_ check es
check (NamP _ (map snd -> es)) = mapM_ check es
check _ = return ()

-- check for contradictions in one pair of expressions
checkPair :: Int -> E -> E -> MorlocMonad ()
-- These pass
--   foo x y = (x, y)
--   foo a b = (a, b)
--
-- These do not except in the special case where a=b
--   foo x y = (x, y)
--   foo a b = (b, a)
--
-- This requires unified names (see LamS case)
checkPair i e1@(BndP _ v1) e2@(BndP _ v2)
  | v1 == v2 = return ()
  | otherwise = valueError i e1 e2 "Non-equivalent variable patterns"
checkPair _ e1@(VarP g v1 es1) (VarP _ v2 es2)
  -- Same term, so es1 and es2 must be identical
  | v1 == v2 = check e1
  -- If the terms are different all the instances must still be the same and the
  -- type will be the same, so we can simply combine them.
  | otherwise = check (VarP g v1 (es1 <> es2))
-- evaluate all applications of lambdas
--  case #1 remove an empty lambda
checkPair i (AppP _ (LamP _ [] e1) _) e2 = checkPair i e1 e2
--  case #2 remove an empty application
checkPair i (AppP _ f@LamP {} []) e2 = checkPair i f e2
--  case #3 substitute on argument into the lambda
checkPair i (AppP g1 (LamP g2 (v : vs) e1) (x : xs)) e2 =
  let e1' = substituteExpr v x e1
   in checkPair i (AppP g1 (LamP g2 vs e1') xs) e2
--  if there is an applied lambda on the other side, reverse
checkPair i e1 e2@(AppP _ LamP {} _) = checkPair i e2 e1
-- No value checking is possible between applications
--
-- If the function applied is not the same in both terms, we can
-- say nothing without source analysis.
--
-- If the function applied is the same, we also need more information to make
-- any decisions. For example:
--
-- foo x y
-- foo y x
--
-- Here the applied function is the same, but the arguments are switched. But
-- whether this causes the terms to be non-equal depends on whether the function
-- commutes. For example, `add x y` == `add y x`.
--
-- In general, a function is free to map different inputs to the same
-- output. Without further information, we can conclude nothing. So all
-- applications must pass.
checkPair _ AppP {} AppP {} = return ()
checkPair i e1@AppP {} e2 = valueError i e1 e2 "Cannot check beyond source boundary"
checkPair i e1 e2@AppP {} = valueError i e1 e2 "Cannot check beyond source boundary"
-- Not that SrcP is something sourced, not necessarily a function, it may be a
-- constant.
checkPair i (SrcP (Idx _ t) src1) (SrcP _ src2) = compareForeignFunctions i t src1 src2
checkPair i e1@(SrcP _ _) e2 = checkPair i e2 e1
checkPair i e1 e2@SrcP {}
  | isSimple e1 = valueError i e1 e2 "Cannot compare source value to non-source expression"
  | otherwise = return ()
  where
    -- For VarP, simplicity of ANY instance indicates an error
    isSimple (VarP _ _ es) = any isSimple es
    -- For other expressions, only simplicity of ALL elements is error
    isSimple (LstP _ es) = all isSimple es
    isSimple (TupP _ es) = all isSimple es
    isSimple (NamP _ (map snd -> es)) = all isSimple es
    isSimple BndP {} = True
    isSimple LitP {} = True
    isSimple AppP {} = False
    isSimple LamP {} = False
    isSimple SrcP {} = False
    isSimple PatP {} = False

-- reduce empty lambdas
--
-- -- initial
-- \x y -> (\a b -> bar b a) x y
-- \k j -> bar k j
--
-- -- unify terms
-- \m n -> (\a b -> bar b a) m n
-- \m n -> bar m n
--
-- -- apply if not in canonical form
-- \m n -> bar n m
-- \m n -> bar m n
--
-- -- compare calls
-- bar
-- bar -- same function is called, so their arguments must be the comparable
--
-- -- compare arguments, starting with first
-- n
-- m
checkPair i (LamP _ vs1 s1) (LamP _ vs2 s2) = checkPair i s1' s2'
  where
    used =
      Set.unions
        [ freeTerms s1
        , freeTerms s2
        , Set.fromList (vs1 <> vs2)
        ]

    -- list of original variable names
    newvars =
      filter
        (\v -> not $ Set.member v used)
        [EV $ DT.pack ("x" <> show j) | j <- [(0 :: Int) ..]]

    s1' = foldr (\(v, r) s -> substituteEVar v r s) s1 (zip vs1 newvars)
    s2' = foldr (\(v, r) s -> substituteEVar v r s) s2 (zip vs2 newvars)
checkPair _ _ (LamP{}) = error "Illegal empty lambda"
checkPair _ (LamP{}) _ = error "Illegal empty lambda"
-- check all container elements
--  * their sizes must agree
--  * their pairwise elements must agree
checkPair i e1@(LstP _ xs) e2@(LstP _ ys)
  | length xs /= length ys = valueError i e1 e2 "Containers of unequal length"
  | otherwise = mapM_ (uncurry (checkPair i)) (zip xs ys)
checkPair i (TupP _ xs) (TupP _ ys) =
  mapM_ (uncurry (checkPair i)) (zip xs ys)
-- check records, no assumption of order
checkPair _ (NamP _ []) (NamP _ _) = return ()
checkPair i (NamP g1 ((k, x) : rs1)) (NamP g2 rs2) =
  case lookup k rs2 of
    (Just y) -> checkPair i x y >> checkPair i (NamP g1 rs1) (NamP g2 rs2)
    Nothing -> error "Unreachable if typechecker has passed"
-- Primitives must be equal
checkPair i e1@(LitP _ x) e2@(LitP _ y)
  | x == y = return ()
  | otherwise = valueError i e1 e2
    $ "Cannot equate non-equal primitives:\n"
    <> "a:" <+> pretty x
    <> "b:" <+> pretty y

-- All other cases should fail.
--
-- Actually, all other cases should already have failed while typechecking.
--
-- It should not be possible to reach this case, should it?
checkPair i e1 e2 = valueError i e1 e2 "Non-equivalent forms"

-- Currently we do not check the equivalence of sourced terms
--
-- This is the function to implement when we decided to start
-- checking source code
--
-- This operation may be expensive (if we are doing it right)
compareForeignFunctions :: Int -> Type -> Source -> Source -> MorlocMonad ()
compareForeignFunctions _ _ _ _ = return ()

valueError :: Int -> E -> E -> MDoc -> MorlocMonad ()
valueError i e1 e2 msg = MM.throwUnificationError (indexOfE e1) (indexOfE e2) i ("Error in value checker:" <+> msg)


substituteEVar :: EVar -> EVar -> E -> E
substituteEVar oldVar newVar e0
  | oldVar == newVar = e0
  | otherwise = f usedVars 0 e0
  where
    -- list of free term variables
    usedVars = Set.union (freeTerms e0) (Set.fromList [oldVar, newVar])

    f :: Set.Set EVar -> Int -> E -> E
    f _ _ e@(BndP g v)
      | v == oldVar = BndP g newVar
      | otherwise = e
    f used idx (LamP g vs e) =
      let (used', idx', vs', e') = relabelLam used idx vs e
       in LamP g vs' (f used' idx' e')
    f _ _ e@(VarP g v es)
      | v == oldVar = VarP g newVar es
      | otherwise = e
    f used idx (AppP g e es) = AppP g (f used idx e) $ map (f used idx) es
    f used idx (LstP g es) = LstP g $ map (f used idx) es
    f used idx (TupP g es) = TupP g $ map (f used idx) es
    f used idx (NamP g rs) = NamP g $ map (second (f used idx)) rs
    f _ _ e = e

    relabelLam :: Set.Set EVar -> Int -> [EVar] -> E -> (Set.Set EVar, Int, [EVar], E)
    relabelLam used idx [] e = (used, idx, [], e)
    relabelLam used idx (v : vs) e
      | Set.member v used =
          let (idx', v') = newvar used idx
           in let (used', idx'', vs', e') = relabelLam (Set.insert v' used) idx' vs (substituteEVar v v' e)
               in (used', idx'', v' : vs', e')
      | otherwise =
          let (used', idx', vs', e') = relabelLam used idx vs e
           in (used', idx', v : vs', e')

    newvar :: Set.Set EVar -> Int -> (Int, EVar)
    newvar used i =
      let x = EV (DT.pack $ "x" <> show i)
       in if Set.member x used
            then newvar used (i + 1)
            else (i, x)

-- Find all names in a term that are not bound under a lambda
freeTerms :: E -> Set.Set EVar
freeTerms = f Set.empty
  where
    f :: Set.Set EVar -> E -> Set.Set EVar
    f boundterms (BndP _ v)
      | Set.member v boundterms = Set.empty
      | otherwise = Set.singleton v
    f boundterms (VarP _ v es)
      | Set.member v boundterms = error "Bug found, somewhere Var and Bnd are getting mixed"
      | otherwise = Set.insert v . Set.unions . fmap (f boundterms) $ es
    f boundterms (LamP _ vs e) =
      let boundterms' = Set.union boundterms (Set.fromList vs)
       in f boundterms' e
    f boundterms (AppP _ e es) = Set.unions . map (f boundterms) $ (e : es)
    f boundterms (LstP _ es) = Set.unions . map (f boundterms) $ es
    f boundterms (TupP _ es) = Set.unions . map (f boundterms) $ es
    f boundterms (NamP _ (map snd -> es)) = Set.unions . map (f boundterms) $ es
    f _ _ = Set.empty

substituteExpr :: EVar -> E -> E -> E
substituteExpr oldVar replacementExpr = f
  where
    f e@(BndP _ v)
      | v == oldVar = replacementExpr
      | otherwise = e
    f e@(VarP _ v _)
      | v == oldVar = replacementExpr
      | otherwise = e
    f e@(LamP g vs body)
      | oldVar `elem` vs = e -- stop if term is shadowed
      | otherwise = LamP g vs (f body)
    f (AppP g e es) = AppP g (f e) (map f es)
    f (LstP g es) = LstP g (map f es)
    f (TupP g es) = TupP g (map f es)
    f (NamP g rs) = NamP g (map (second f) rs)
    f e@LitP {} = e
    f e@SrcP {} = e
    f e@PatP {} = e
