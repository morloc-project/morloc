{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Frontend.Valuecheck
Description : Check recursive bindings and alternative implementations
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Two checks on typed trees: that no binding needs its own value (BIND-7), and
that the alternative implementations of a term are not provably
contradictory, such as two constants of one type with different values.
-}
module Morloc.Frontend.Valuecheck (valuecheck, checkPair, checkRecursion) where

import Data.Foldable (toList)
import Morloc.Data.Zip (zipPrefix, zipSame, zipWithSame)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as DT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Scientific (toRealFloat)
import qualified Morloc.BaseTypes as BT
import qualified Morloc.TypeEval as TE
import Morloc.CodeGenerator.Value (etaParts, isLambda)
import Morloc.Data.Doc
import Morloc.Frontend.Rename (displayName)
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM

-- Convert AnnoS objects to a simple intermediate type
toE :: AnnoS (Indexed Type) Many Int -> E
toE (AnnoS g _ UniS) = LitP g MUni
toE (AnnoS g _ NullS) = LitP g MNull
toE (AnnoS g _ (BndS v)) = BndP g v
toE (AnnoS g _ (VarS v es)) = VarP g v (map toE (unMany es))
toE (AnnoS g _ (AppS e es)) = AppP g (toE e) (map toE es)
toE (AnnoS g _ (LamS vs e)) = LamP g vs (toE e)
toE (AnnoS g _ (LstS es)) = LstP g (map toE es)
toE (AnnoS g _ (TupS es)) = TupP g (map toE es)
toE (AnnoS g _ (NamS rs)) = NamP g (map (second toE) rs)
-- A literal carries its own source index: an inlined term's wrapping index
-- is that of the reference to it.
toE (AnnoS (Idx _ t) _ (RealS si x)) = LitP (Idx si t) (MNum x)
toE (AnnoS (Idx _ t) _ (IntS si x)) = LitP (Idx si t) (MInt x)
toE (AnnoS g _ (LogS x)) = LitP g (MLog x)
toE (AnnoS g _ (StrS x)) = LitP g (MStr x)
toE (AnnoS g _ (ConS _ n _ xs)) = ConP g n (map toE xs)
toE (AnnoS g _ (ExeS (SrcCall (Idx _ s)))) = SrcP g s
toE (AnnoS g _ (PatS fg _ _ app)) = AppP g (patE fg (patAppPattern app)) (map toE (toList app))
  where
    patE i (PatternText t ts) = LitP i (MStr (t <> DT.concat ["#{}" <> t' | t' <- ts]))
    patE i (PatternStruct sel) = PatP i sel
    patE i PatternBracketIndex = BracketIndexP i
    patE i PatternBracketSlice = BracketSliceP i
toE (AnnoS g _ (LetBndS v)) = BndP g v
toE (AnnoS g _ (CallS v)) = BndP g v
toE (AnnoS _ _ (LetS _ _ body)) = toE body
toE (AnnoS g _ (IfS c t e)) = IfP g (toE c) (toE t) (toE e)
toE (AnnoS g _ (DoBlockS e)) = DoBlockP g (toE e)
toE (AnnoS g _ (EvalS e)) = EvalP g (toE e)
toE (AnnoS g _ (CoerceS c e)) = CoerceP c g (toE e)
toE (AnnoS g _ (IntrinsicS intr es)) = IntrinsicP g intr (map toE es)
toE (AnnoS _ _ (LabelS _ e)) = toE e

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
indexOfE (BracketIndexP (Idx i _)) = i
indexOfE (BracketSliceP (Idx i _)) = i
indexOfE (IfP (Idx i _) _ _ _) = i
indexOfE (DoBlockP (Idx i _) _) = i
indexOfE (EvalP (Idx i _) _) = i
indexOfE (CoerceP _ (Idx i _) _) = i
indexOfE (IntrinsicP (Idx i _) _ _) = i
indexOfE (ConP (Idx i _) _ _) = i

typeOfE :: E -> Type
typeOfE (BndP (Idx _ t) _) = t
typeOfE (VarP (Idx _ t) _ _) = t
typeOfE (AppP (Idx _ t) _ _) = t
typeOfE (LamP (Idx _ t) _ _) = t
typeOfE (LstP (Idx _ t) _) = t
typeOfE (TupP (Idx _ t) _) = t
typeOfE (NamP (Idx _ t) _) = t
typeOfE (LitP (Idx _ t) _) = t
typeOfE (SrcP (Idx _ t) _) = t
typeOfE (PatP (Idx _ t) _) = t
typeOfE (BracketIndexP (Idx _ t)) = t
typeOfE (BracketSliceP (Idx _ t)) = t
typeOfE (IfP (Idx _ t) _ _ _) = t
typeOfE (DoBlockP (Idx _ t) _) = t
typeOfE (EvalP (Idx _ t) _) = t
typeOfE (CoerceP _ (Idx _ t) _) = t
typeOfE (IntrinsicP (Idx _ t) _ _) = t
typeOfE (ConP (Idx _ t) _ _) = t

-- | BIND-7. A 'CallS' back-edge closes the cycle of the open 'VarS'
-- alternatives above it, up to the 'VarS' of its name; a call of a shared
-- specialization is not a back-edge. On the way down, each member records
-- the demand on it of the member enclosing it, as a function of the demand
-- on that one.
checkRecursion ::
  Set.Set EVar -> (Int -> Maybe Int) -> Map.Map Int Int -> Map.Map Int Int -> AnnoS g Many Int -> MorlocMonad ()
checkRecursion specs termOf owners sites = go [] id
  where
    go :: [Member g] -> (Demand -> Demand) -> AnnoS g Many Int -> MorlocMonad ()
    go open here node@(AnnoS _ c e) = case e of
      VarS v alts ->
        mapM_ (\alt -> go (Member v (termOf c) alt here : open) id alt) alts
      CallS v
        | not (Set.member v specs)
        , (inner, target : _) <- break ((== v) . memberName) open ->
            checkCycle (target : reverse inner) here
      _ -> mapM_ (\(step, child) -> go open (step . here) child) (children node)

    -- the demand of each member on the next, and of the last on the first
    checkCycle :: [Member g] -> (Demand -> Demand) -> MorlocMonad ()
    checkCycle members final =
      let ring = zipSame members (map memberIncoming (drop 1 members) <> [final])
          rotations = take (length ring) (iterate rotate ring)
          rotate xs = drop 1 xs <> take 1 xs
       in case [m | r@((m, _) : _) <- rotations, needsItself m r] of
            (Member w k (AnnoS _ i _) _ : _) ->
              MM.throwSourcedError (fromMaybe i (k >>= (`Map.lookup` sites))) $
                "The bindings" <+> hsep (punctuate " ->" (names <> take 1 names))
                  <+> "form a cycle, and computing" <+> squotes (pretty (displayName w))
                  <+> "needs the value of" <+> squotes (pretty (displayName w)) <+> "itself"
              where
                names = [squotes (pretty (displayName (memberName m))) | m <- members]
            [] -> return ()

    -- computing the first member, as a value, comes round to demand it,
    -- unless a call on the way of a lambda holding it computes it anew
    needsItself start = walk (Applied 0)
      where
        walk Unforced _ = False
        walk _ [] = True
        walk d ((m, next) : rest)
          | runsBody d m && holds m = False
          | otherwise = walk (next d) rest
        holds (Member _ k _ _) = maybe False (`elem` ownerChain' start) k

    runsBody d (Member _ _ alt0 _) = go' alt0
      where
        go' alt@(AnnoS _ _ x) = case x of
          LamS vs _
            | isLambda alt -> case d of
                Called -> True
                Applied n -> n >= length vs
                Unforced -> False
          LabelS _ y -> go' y
          CoerceS _ y -> go' y
          _ -> False

    ownerChain' (Member _ k _ _) = maybe [] ownerChain k
    ownerChain k = case Map.lookup k owners of
      Just o | o /= k -> o : ownerChain o
      _ -> []

    children :: AnnoS g Many Int -> [(Demand -> Demand, AnnoS g Many Int)]
    children n@(AnnoS _ _ x) = case x of
      LamS vs b
        | Just (f, pre) <- etaParts n ->
            (etaHead (length pre) (length vs), f) : [(called, y) | y <- pre]
        | otherwise -> [(enter (length vs), b)]
      DoBlockS b -> [(force, b)]
      AppS f xs -> (supply (length xs), f) : [(called, y) | y <- xs]
      IfS cond t f -> [(evaluated, cond), (id, t), (id, f)]
      LetS _ y z -> [(called, y), (id, z)]
      LabelS _ y -> [(id, y)]
      CoerceS _ y -> [(id, y)]
      _ -> [(called, y) | y <- foldExprS (: []) x]

-- | How far evaluating an expression goes into a part of it: not at all, to
-- its value applied to some number of arguments (none: just its value), or
-- applied to any number of them.
data Demand = Unforced | Applied Int | Called
  deriving (Eq)

-- | The body of a lambda of @k@ parameters.
enter :: Int -> Demand -> Demand
enter k (Applied n)
  | n >= k = Applied (n - k)
  | otherwise = Unforced
enter _ d = d

-- | The head of an application of @k@ arguments.
supply :: Int -> Demand -> Demand
supply k (Applied n) = Applied (n + k)
supply _ d = d

-- | The head of an eta-expanded partial application holding @held@
-- arguments, waiting for @waiting@ more.
etaHead :: Int -> Int -> Demand -> Demand
etaHead held waiting (Applied n)
  | n >= waiting = Applied (held + n)
  | otherwise = Applied held
etaHead _ _ d = d

-- | The body of a do-block, which runs only when it is forced.
force :: Demand -> Demand
force Called = Called
force _ = Unforced

evaluated :: Demand -> Demand
evaluated Unforced = Unforced
evaluated _ = Applied 0

called :: Demand -> Demand
called Unforced = Unforced
called _ = Called

-- | An open alternative of a term on the path to a back-edge, with the
-- demand on it of the alternative that encloses it.
data Member g = Member
  { memberName :: EVar
  , _memberTerm :: Maybe Int
  , _memberAlt :: AnnoS g Many Int
  , memberIncoming :: Demand -> Demand
  }

-- | Reject provably contradictory implementations. Exponential in the
-- worst case; nothing is memoized.
valuecheck :: AnnoS (Indexed Type) Many Int -> MorlocMonad (AnnoS (Indexed Type) Many Int)
valuecheck e0 = do
  check (toE e0)
  return e0

check :: E -> MorlocMonad ()
check (VarP (Idx i _) _ es) = do
  -- alternatives of different types are instances, never both applied
  let typedPairs = [(a, b) | (a, b) <- pairwise es, typeOfE a == typeOfE b]
  sequence_ [compareForeignFunctions i t s1 s2 | (SrcP (Idx _ t) s1, SrcP _ s2) <- typedPairs]
  mapM_ (uncurry (checkPair i)) typedPairs
  mapM_ check es
  where
    pairwise :: [a] -> [(a, a)]
    pairwise xs = [(xs !! i', xs !! j') | i' <- [0 .. length xs - 1], j' <- [0 .. length xs - 1], j' > i']
check (AppP _ e es) = mapM_ check (e : es)
check (LamP _ _ e) = check e
check (LstP _ es) = mapM_ check es
check (TupP _ es) = mapM_ check es
check (NamP _ (map snd -> es)) = mapM_ check es
check (IfP _ c t e) = mapM_ check [c, t, e]
check (DoBlockP _ e) = check e
check (EvalP _ e) = check e
check (CoerceP _ _ e) = check e
check (IntrinsicP _ _ es) = mapM_ check es
check (ConP _ _ es) = mapM_ check es
check _ = return ()

-- | REAL-5: two implementations of one term that provably differ. Only data
-- can be told apart: literals, containers, records, constructors, and the
-- parameters of lambdas compared with their parameters renamed alike. A name
-- is compared through its own implementations; anything else, such as an
-- application or a branch, is accepted under REAL-1, as is a comparison
-- that has not been decided within 'comparisonBudget' steps.
checkPair :: Int -> E -> E -> MorlocMonad ()
checkPair i e10 e20 = do
  scope <- MM.gets stateGeneralTypedefs
  budget <- liftIO (newIORef comparisonBudget)
  go budget scope Set.empty e10 e20
  where
    -- @params@: the renamed lambda parameters, each standing for any value
    go :: IORef Int -> Scope -> Set.Set EVar -> E -> E -> MorlocMonad ()
    go budget scope params e1 e2 = do
      left <- liftIO (readIORef budget)
      when (left > 0) $ do
        liftIO (writeIORef budget (left - 1))
        step budget scope params e1 e2

    step budget scope params e1 e2 = case (e1, e2) of
      (VarP _ _ es1, VarP _ _ es2) -> sequence_ [next a b | a <- es1, b <- es2, typeOfE a == typeOfE b]
      (VarP _ _ es, _) -> sequence_ [next a e2 | a <- es, typeOfE a == typeOfE e2]
      (_, VarP {}) -> next e2 e1
      (AppP {}, _) | Just e1' <- betaStep e1 -> next e1' e2
      (_, AppP {}) | Just e2' <- betaStep e2 -> next e1 e2'
      (LamP g1 vs1 b1, LamP g2 vs2 b2) ->
        let k = min (length vs1) (length vs2)
            used = Set.unions [freeTerms b1, freeTerms b2, Set.fromList (vs1 <> vs2), params]
            fresh = take k [v | j <- [(0 :: Int) ..], let v = EV (DT.pack ("x" <> show j)), Set.notMember v used]
            rename vs body = foldr (\(v, r) acc -> substituteEVar v r acc) body (zipPrefix vs fresh)
            rest g vs body = case drop k vs of
              [] -> rename vs body
              more -> LamP g more (rename vs body)
         in go budget scope (Set.union params (Set.fromList fresh)) (rest g1 vs1 b1) (rest g2 vs2 b2)
      (BndP (Idx _ t) v1, BndP _ v2)
        | v1 /= v2 && isParam v1 && isParam v2 && manyValued t ->
            reject "Non-equivalent variable patterns"
      (BndP (Idx _ t) v, _)
        | isParam v && isConstant e2 && manyValued t ->
            reject "A parameter is compared with a constant"
      (_, BndP {}) | isConstant e1 -> next e2 e1
      (LitP (Idx _ t) x, LitP _ y)
        | not (sameLiteral t x y) ->
            reject $
              "Cannot equate non-equal primitives (the two operands disagree):" <> line
                <> "a:" <+> pretty x <> line
                <> "b:" <+> pretty y
      (LstP _ xs, LstP _ ys)
        | length xs /= length ys -> reject "Containers of unequal length"
        | otherwise -> sequence_ (zipWithSame next xs ys)
      (TupP _ xs, TupP _ ys)
        | length xs /= length ys -> MM.throwCompilerBug "valuecheck: two alternatives of one type are tuples of different sizes"
        | otherwise -> sequence_ (zipWithSame next xs ys)
      (NamP _ rs1, NamP _ rs2)
        | sort (map fst rs1) /= sort (map fst rs2) -> MM.throwCompilerBug "valuecheck: two alternatives of one type are records with different fields"
        | otherwise -> sequence_ [next x y | (k, x) <- rs1, Just y <- [lookup k rs2]]
      (ConP _ c1 xs, ConP _ c2 ys)
        | c1 /= c2 -> reject "Different constructors"
        | length xs /= length ys -> MM.throwCompilerBug "valuecheck: one constructor applied to different numbers of fields"
        | otherwise -> sequence_ (zipWithSame next xs ys)
      -- a value made optional is null only if it was itself null
      (CoerceP CoerceToOptional _ x, LitP _ MNull)
        | isConstant x && not (isNull x) -> reject "A value is compared with null"
      (LitP _ MNull, CoerceP CoerceToOptional _ _) -> next e2 e1
      (CoerceP c1 _ x, CoerceP c2 _ y) | c1 == c2 -> next x y
      (DoBlockP _ x, DoBlockP _ y) -> next x y
      (EvalP _ x, EvalP _ y) -> next x y
      -- `@show` is injective at one type, so its arguments are compared there
      (IntrinsicP _ IntrShow [x], IntrinsicP _ IntrShow [y])
        | typeOfE x == typeOfE y -> next x y
      _ -> return ()
      where
        next = go budget scope params
        reject = valueError i e1 e2
        isParam v = Set.member v params

        -- a type with more than one value, so two parameters of it may differ
        manyValued t = not (oneValued Set.empty (type2typeu t))

        oneValued :: Set.Set TVar -> TypeU -> Bool
        oneValued seen t0 = case TE.unWhnf (TE.whnf scope t0) of
          VarU v | v == BT.unit -> True
          AppU (VarU v) ts | v == BT.tuple (length ts) -> all (oneValued seen) ts
          NamU _ _ _ fs -> all (oneValued seen . snd) fs
          VarU v -> singleCtor seen v []
          AppU (VarU v) ts -> singleCtor seen v ts
          _ -> False
        -- a data type of one constructor whose fields each have one value
        singleCtor seen v ts
          | Set.member v seen = False
          | otherwise = case TE.dataCtorsOf scope v ts of
              Just [(_, fs)] -> all (oneValued (Set.insert v seen)) fs
              _ -> False

    -- a value fixed by the program, known without evaluating anything
    isConstant (LitP {}) = True
    isConstant (ConP _ _ xs) = all isConstant xs
    isConstant (LstP _ xs) = all isConstant xs
    isConstant (TupP _ xs) = all isConstant xs
    isConstant (NamP _ rs) = all (isConstant . snd) rs
    isConstant (CoerceP _ _ x) = isConstant x
    isConstant _ = False

    isNull (LitP _ MNull) = True
    isNull (CoerceP _ _ x) = isNull x
    isNull _ = False

    -- numbers are equal when they are one value of their type
    sameLiteral t a b = case (number a, number b) of
      (Just x, Just y)
        | t == VarT BT.f32 -> (toRealFloat x :: Float) == toRealFloat y
        | isRealType t -> (toRealFloat x :: Double) == toRealFloat y
        | otherwise -> x == y
      _ -> a == b
    number (MInt n) = Just (fromInteger n)
    number (MNum (RealFinite x)) = Just x
    number _ = Nothing
    isRealType t = t == VarT BT.real || t == VarT BT.f64

    -- one step of reducing an applied lambda
    betaStep (AppP g (LamP gl (v : vs) body) (x : xs)) = Just (AppP g (LamP gl vs (substituteExpr v x body)) xs)
    betaStep (AppP g (LamP _ [] body) xs@(_ : _)) = Just (AppP g body xs)
    betaStep (AppP _ (LamP _ [] body) []) = Just body
    betaStep (AppP _ f@LamP {} []) = Just f
    -- a name with one implementation stands for it
    betaStep (AppP g h xs) | Just f <- soleLambda h = Just (AppP g f xs)
    betaStep _ = Nothing
    soleLambda (VarP _ _ [f@LamP {}]) = Just f
    soleLambda (VarP _ _ [h]) = soleLambda h
    soleLambda _ = Nothing

-- | The comparison steps one pair of implementations may take before it is
-- accepted undecided.
comparisonBudget :: Int
comparisonBudget = 10000

-- | Two sources of one name and type in one language leave the realizer no
-- rule to choose between them; sources in different languages are
-- alternatives.
compareForeignFunctions :: Int -> Type -> Source -> Source -> MorlocMonad ()
compareForeignFunctions i _ src1 src2
  | src1 == src2 = return ()
  | srcLang src1 /= srcLang src2 = return ()
  | otherwise =
      MM.throwSourcedError i $
        "Ambiguous source declarations for"
          <+> squotes (pretty (srcAlias src1))
          <> ":" <> line <> indent 2 (pretty src1)
          <> line <> indent 2 (pretty src2)

valueError :: Int -> E -> E -> MDoc -> MorlocMonad ()
valueError i e1 e2 msg = MM.throwUnificationError (indexOfE e1) (indexOfE e2) i ("Error in value checker:" <+> msg)

substituteEVar :: EVar -> EVar -> E -> E
substituteEVar oldVar newVar e0
  | oldVar == newVar = e0
  | otherwise = f usedVars 0 e0
  where
    usedVars = Set.union (freeTerms e0) (Set.fromList [oldVar, newVar])

    f :: Set.Set EVar -> Int -> E -> E
    f _ _ e@(BndP g v)
      | v == oldVar = BndP g newVar
      | otherwise = e
    f used idx (LamP g vs e) =
      let (used', idx', vs', e') = relabelLam used idx vs e
       in LamP g vs' (f used' idx' e')
    f used idx (VarP g v es)
      | v == oldVar = VarP g newVar es
      | otherwise = VarP g v (map (f used idx) es)
    f used idx (ConP g c es) = ConP g c (map (f used idx) es)
    f used idx (AppP g e es) = AppP g (f used idx e) $ map (f used idx) es
    f used idx (LstP g es) = LstP g $ map (f used idx) es
    f used idx (TupP g es) = TupP g $ map (f used idx) es
    f used idx (NamP g rs) = NamP g $ map (second (f used idx)) rs
    f used idx (IfP g c t e) = IfP g (f used idx c) (f used idx t) (f used idx e)
    f used idx (DoBlockP g e) = DoBlockP g (f used idx e)
    f used idx (EvalP g e) = EvalP g (f used idx e)
    f used idx (CoerceP c g e) = CoerceP c g (f used idx e)
    f used idx (IntrinsicP g intr es) = IntrinsicP g intr (map (f used idx) es)
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
    f boundterms (IfP _ c t e) = Set.unions [f boundterms c, f boundterms t, f boundterms e]
    f boundterms (DoBlockP _ e) = f boundterms e
    f boundterms (EvalP _ e) = f boundterms e
    f boundterms (CoerceP _ _ e) = f boundterms e
    f boundterms (IntrinsicP _ _ es) = Set.unions . map (f boundterms) $ es
    f boundterms (ConP _ _ es) = Set.unions . map (f boundterms) $ es
    f _ _ = Set.empty

substituteExpr :: EVar -> E -> E -> E
substituteExpr oldVar replacementExpr = f
  where
    f e@(BndP _ v)
      | v == oldVar = replacementExpr
      | otherwise = e
    f (VarP g v es)
      | v == oldVar = replacementExpr
      | otherwise = VarP g v (map f es)
    f (ConP g c es) = ConP g c (map f es)
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
    f e@BracketIndexP {} = e
    f e@BracketSliceP {} = e
    f (IfP g c t e) = IfP g (f c) (f t) (f e)
    f (DoBlockP g e) = DoBlockP g (f e)
    f (EvalP g e) = EvalP g (f e)
    f (CoerceP c g e) = CoerceP c g (f e)
    f (IntrinsicP g intr es) = IntrinsicP g intr (map f es)
