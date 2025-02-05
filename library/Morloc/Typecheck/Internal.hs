{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

{-|
Module      : Morloc.Typecheck.Internal
Description : Functions for type checking and type manipulation
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module exports any typechecking machinery that can be shared between the
general typechecker in the frontend and the language specific typechecker(s) of
the backend.

-}

module Morloc.Typecheck.Internal
  ( (+>)
  , (++>)
  -- * accessing state
  , newvar
  , tvarname
  , newvarRich
  , evarname
  , qualify
  , unqualify
  -- * Typeclasses
  , Applicable(..)
  , GammaIndexLike(..)
  -- * manipulating context
  , access1
  , access2
  , lookupU
  , lookupE
  , cut
  , substitute
  , rename
  , renameAnnoS
  , occursCheck
  , toExistential
  -- * subtyping
  , subtype
  , isSubtypeOf2
  , equivalent2
  -- * debugging
  , seeGamma
  -- debugging
  , enter
  , insetSay
  , leave
  , peak
  , peakGen
  , seeType
  ) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import Morloc.Data.Doc
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

import qualified Data.Set as Set
import qualified Data.Map as Map

qualify :: [TVar] -> TypeU -> TypeU
qualify vs t = foldr ForallU t vs

unqualify :: TypeU -> ([TVar], TypeU)
unqualify (ForallU v (unqualify -> (vs, t))) = (v:vs, t)
unqualify t = ([], t)

toExistential :: Gamma -> TypeU -> (Gamma, TypeU)
toExistential g0 (unqualify -> (vs0, t0)) = f g0 vs0 t0 where
  f g [] t = (g, t)
  f g (v:vs) t = let (g', newVar) = newvar ("cls_" <> unTVar v) g
                 in f g' vs (substituteTVar v newVar t)

class Applicable a where
  apply :: Gamma -> a -> a

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable TypeU where
  -- [G]a = a
  apply g (VarU v) =
    -- FIXME: very wrong - only works because of my renaming scheme
    case lookupU v g of
      (Just t') -> t'
      Nothing -> VarU v

  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunU ts t) = FunU (map (apply g) ts) (apply g t)
  apply g (AppU t ts) = AppU (apply g t) (map (apply g) ts)
  -- [G]ForallU a.a = forall a. [G]a
  apply g (ForallU v a) =
    -- FIXME: VERY WRONG
    case lookupU v g of
      (Just _) -> apply g a
      Nothing -> ForallU v (apply g a)

  -- [G[a=t]]a = [G[a=t]]t
  apply g (ExistU v ts rs) =
    case lookupU v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistU v (map (apply g) ts) (map (second (apply g)) rs)
  apply g (NamU o n ps rs) = NamU o n ps [(k, apply g t) | (k, t) <- rs]

instance Applicable EType where
  apply g e = e { etype = apply g (etype e) }

instance Applicable Gamma where
  apply g1 g2 = g2 {gammaContext = map f (gammaContext g2)} where
    f :: GammaIndex -> GammaIndex
    f (AnnG v t) = AnnG v (apply g1 t)
    f (ExistG v ps rs) = ExistG v (map (apply g1) ps) (map (second (apply g1)) rs)
    f (SolvedG v t) = SolvedG v (apply g1 t)
    f x = x

class GammaIndexLike a where
  index :: a -> GammaIndex

instance GammaIndexLike GammaIndex where
  index = id

instance GammaIndexLike TypeU where
  index (ExistU t ts rs) = ExistG t ts rs
  index t = error $ "Can only index ExistT, found: " <> show t

instance GammaIndexLike TVar where
  index v = ExistG v [] []

(+>) :: GammaIndexLike a => Gamma -> a -> Gamma
(+>) g x = g {gammaContext = index x : gammaContext g}


(++>) :: GammaIndexLike a => Gamma -> [a] -> Gamma
(++>) g xs = g {gammaContext = map index (reverse xs) <> gammaContext g }


isSubtypeOf2 :: TypeU -> TypeU -> Bool
isSubtypeOf2 a b = case subtype Map.empty a b (Gamma 0 []) of
  (Left _) -> False
  (Right _) -> True

equivalent2 :: TypeU -> TypeU -> Bool
equivalent2 t1 t2 = isSubtypeOf2 t1 t2 && isSubtypeOf2 t2 t1


subtypeEvaluated :: Scope -> TypeU -> TypeU -> Gamma -> Either TypeError Gamma
subtypeEvaluated scope t1 t2 g =
    case (reduceType scope t1, reduceType scope t2) of
      (Just t1', _) -> subtype scope t1' t2 g
      (_, Just t2') -> subtype scope t1 t2' g
      (_, _) -> (Left . TypeEvaluationError . render) ("Type evaluation failed:" <+> viaShow (t1, t2))

-- evaluate a type one step, return nothing if no evaluation is possible
reduceType :: Scope -> TypeU -> Maybe TypeU
reduceType scope t0 =
    case TE.evaluateStep scope t0 of
        (Just t1) -> if t1 == t0 then Nothing else Just t1
        Nothing -> Nothing

-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: Scope -> TypeU -> TypeU -> Gamma -> Either TypeError Gamma

-- VarU vs VarT
subtype scope t1@(VarU a1) t2@(VarU a2) g
  -- If everything is the same, do nothing
  --
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l <: a_l -| G[a]
  | a1 == a2 = return g

  | otherwise = subtypeEvaluated scope t1 t2 g

subtype scope a@ExistU{} b@ExistU{} g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- E.a <: E.a -| G[E.a]
  | a == b = return g
  -- ----------------------------------------- <:InstantiateL/<:InstantiateR
  --  G[E.a] |- Ea <: Ea -| G[E.a]
  | otherwise = instantiate scope a b g
      -- formally, an `Ea notin FV(G)` check should be done here, but since the
      -- types involved are all existentials, it will always pass, so I omit
      -- it.

--  g1 |- B1 <: A1 -| g2
--  g2 |- [g2]A2 <: [g2]B2 -| g3
-- ----------------------------------------- <:-->
--  g1 |- A1 -> A2 <: B1 -> B2 -| g3
--
-- function subtypes are *contravariant* with respect to the input, that is,
-- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
subtype scope (FunU [] a2) (FunU [] b2) g = subtype scope a2 b2 g
subtype scope (FunU (a1:rs1) a2) (FunU (b1:rs2) b2) g1 = do
  g2 <- subtype scope b1 a1 g1
  subtype scope (apply g2 (FunU rs1 a2)) (apply g2 (FunU rs2 b2)) g2

--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype scope t1@(AppU v1@(ExistU _ _ _) vs1) t2@(AppU v2 vs2) g
  | length vs1 == length vs2 = zipSubtype t1 t2 scope (v1:vs1) (v2:vs2) g
  | otherwise = subtypeEvaluated scope t1 t2 g
subtype scope t1@(AppU v1 vs1) t2@(AppU v2@(ExistU _ _ _) vs2) g
  | length vs1 == length vs2 = zipSubtype t1 t2 scope (v1:vs1) (v2:vs2) g
  | otherwise = subtypeEvaluated scope t1 t2 g
subtype scope t1@(AppU v1 vs1) t2@(AppU v2 vs2) g
  | v1 == v2 && length vs1 == length vs2 = zipSubtype t1 t2 scope vs1 vs2 g
  | otherwise = subtypeEvaluated scope t1 t2 g

-- subtype unordered records
subtype scope (NamU _ v1 _ []) (NamU _ v2 _ []) g
    -- If one of the records is generic, allow promotion
    | v1 == BT.record || v2 == BT.record = return g
    -- Otherwise subtype the variable names
    | otherwise = subtype scope (VarU v1) (VarU v2) g

subtype _ t1@(NamU _ _ _ []) t2@(NamU _ _ _ _) _ =
  Left $ SubtypeError t1 t2 "NamU - Unequal number of fields"
subtype _ t1@(NamU _ _ _ _ ) t2@(NamU _ _ _ []) _ =
  Left $ SubtypeError t1 t2 "NamU - Unequal number of fields"
subtype scope t1@(NamU o1 v1 p1 ((k1,x1):rs1)) t2@(NamU o2 v2 p2 es2) g0 =
    case filterApart (\(k2, _) -> k2 == k1) es2 of
      (Nothing, _) -> Left $ SubtypeError t1 t2 "NamU - Unequal fields"
      (Just (_, x2), rs2)
        ->  subtype scope x1 x2 g0
        >>= subtype scope (NamU o1 v1 p1 rs1) (NamU o2 v2 p2 rs2)


--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype scope a b@(ExistU _ [] _) g = occursCheck b a "InstantiateR" >> instantiate scope a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype scope a@(ExistU _ [] _) b g = occursCheck a b "InstantiateL" >> instantiate scope a b g

subtype scope a@(AppU _ _) b@(ExistU _ _ _) g = subtype scope b a g

subtype scope t1@(ExistU v1 ps1 []) t2@(AppU _ ps2) g1
  | length ps1 /= length ps2 = Left $ SubtypeError t1 t2 "InstantiateL - Expected equal number of type parameters"
  | otherwise = do
    g2 <- foldM (\g (p1, p2) -> subtype scope p1 p2 g) g1 (zip ps1 ps2)
    case access1 v1 (gammaContext g2) of
      Just (rs, _, ls) -> do
        solved <- solve v1 t2
        return $ g2 { gammaContext = rs ++ [solved] ++ ls }
      Nothing -> return g2 -- it is already solved, so do nothing

--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
--
subtype scope (ForallU v a) b g0 = subtype scope (substitute v a) b (g0 +> v)
  -- NOTE: I am deviating from the rules here by not cutting. It is not
  -- necessary to do so since I rewrote all qualifiers to be globally unique.
  -- Also, when I cut here I lose my only link to v, and that caused `map fst`
  -- to not compile.

--  g1,a |- A <: B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype scope a (ForallU v b) g = subtype scope a b (g +> VarG v) >>= cut (VarG v)

-- fall through
subtype _ a b _ = Left $ SubtypeError a b "Type mismatch"


zipSubtype :: TypeU -> TypeU -> Scope -> [TypeU] -> [TypeU] -> Gamma -> Either TypeError Gamma
zipSubtype _ _ _ [] [] g' = return g'
zipSubtype a b scope (t1':ts1') (t2':ts2') g' = do
  g'' <- subtype scope t1' t2' g'
  zipSubtype a b scope ts1' ts2' g''
zipSubtype a b _ _ _ _ = Left $ SubtypeError a b "Parameter type mismatch"


-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: Scope -> TypeU -> TypeU -> Gamma -> Either TypeError Gamma

instantiate scope ta@(ExistU _ _ (_:_)) tb@(NamU _ _ _ _) g1 = instantiate scope tb ta g1
instantiate scope ta@(ExistU _ _ (_:_)) tb@(VarU _) g1 = instantiate scope tb ta g1
instantiate scope ta@(VarU _) tb@(ExistU _ _ (_:_)) g1 = do
  case reduceType scope ta of
    (Just ta') -> instantiate scope ta' tb g1
    Nothing -> Left $ InstantiationError ta tb "Error in VarU versus NamU with existential keys"
instantiate scope ta@(NamU _ _ _ rs1) tb@(ExistU v _ rs2@(_:_)) g1 = do
  g2 <- foldM (\g' (t1, t2) -> subtype scope t1 t2 g') g1 [(t1, t2) | (k1, t1) <- rs1, (k2, t2) <- rs2, k1 == k2]
  case access1 v (gammaContext g2) of
    (Just (rhs, _, lhs)) -> do
        solved <- solve v ta
        return $ g2 {gammaContext = rhs ++ [solved] ++ lhs}
    Nothing -> Left $ InstantiationError ta tb "Error in NamU with existential keys"

instantiate scope ta@(ExistU v [] _) tb@(FunU as b) g1 = do
  let (g2, veas) = statefulMap (\g _ -> tvarname g "ta") g1 as
      (g3, veb) = tvarname g2 "to"
      eas = [ExistU v' [] [] | v' <- veas]
      eb = ExistU veb [] []
  g4 <- case access1 v (gammaContext g3) of
      Just (rs, _, ls) -> do
        solved <- solve v (FunU eas eb)
        return $ g3 { gammaContext = rs ++ [solved] ++ (index eb : map index eas) ++ ls }
      Nothing -> Left $ InstantiationError ta tb "Error in InstLApp"
  g5 <- foldlM (\g (e, t) -> instantiate scope e t g) g4 (zip eas as)
  instantiate scope eb (apply g5 b) g5

--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRApp
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate scope ta@(FunU as b) tb@(ExistU v [] _) g1 = do
  let (g2, veas) = statefulMap (\g _ -> tvarname g "ta") g1 as
      (g3, veb) = tvarname g2 "to"
      eas = [ExistU v' [] [] | v' <- veas]
      eb = ExistU veb [] []
  g4 <- case access1 v (gammaContext g3) of
    Just (rs, _, ls) -> do
        solved <- solve v (FunU eas eb)
        return $ g3 { gammaContext = rs ++ [solved] ++ (index eb : map index eas) ++ ls }
    Nothing -> Left $ InstantiationError ta tb "Error in InstRApp"
  g5 <- foldlM (\g (e, t) -> instantiate scope t e g) g4 (zip eas as)
  instantiate scope eb (apply g5 b) g5



-- This is terrible kludge, I am not close to having considered all the edge
-- cases. I need to completely rewrite my type system. Argh. I also need to get
-- rid of all default types. Defaults should be set explicitly in morloc code.
instantiate _ ta@(ExistU _ _ (_:_)) tb@(ExistU v [] []) g1 =
  case access1 v (gammaContext g1) of
    (Just (ls, _, rs)) -> do
        solved <- solve v ta
        return $ g1 { gammaContext = ls ++ solved : rs }
    Nothing ->
      case lookupU v g1 of
        (Just _) -> return g1
        Nothing -> Left . InstantiationError ta tb . render
          $ "Error in recordInstRSolve with gamma:\n" <> tupled (map pretty (gammaContext g1))
instantiate _ ta@(ExistU v [] []) tb@(ExistU _ _ (_:_)) g1 =
  case access1 v (gammaContext g1) of
    (Just (ls, _, rs)) -> do
        solved <- solve v tb
        return $ g1 { gammaContext = ls ++ solved : rs }
    Nothing ->
      case lookupU v g1 of
        (Just _) -> return g1
        Nothing -> Left . InstantiationError ta tb . render
          $ "Error in recordInstLSolve:" <+> tupled (map pretty (gammaContext g1))


--
-- ----------------------------------------- InstLAllR
--
instantiate scope ta@(ExistU _ _ _) (ForallU v2 t2) g1
  = instantiate scope ta t2 (g1 +> VarG v2)
  >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate scope (ExistU v1 ps1 rs1) (ExistU v2 ps2 rs2) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype scope t1 t2 g) g1 (zip ps1 ps2)
  g3 <- foldM (\g' (t1, t2) -> subtype scope t1 t2 g') g2 [(t1, t2) | (k1, t1) <- rs1, (k2, t2) <- rs2, k1 == k2]
  let rs3 = rs1 <> [x | x <- rs2, fst x `notElem` map fst rs1]
      ta = ExistU v1 ps1 rs3
      tb = ExistU v2 ps2 rs3
  case access2 v1 v2 (gammaContext g3) of
    -- InstLReach
    (Just (ls, _, ms, x, rs)) -> do
        solved <- solve v1 tb
        return $ g3 { gammaContext = ls <> (solved : ms) <> (x : rs) }
    Nothing ->
      case access2 v2 v1 (gammaContext g3) of
      -- InstRReach
        (Just (ls, _, ms, x, rs)) -> do
          solved <- solve v2 ta
          return $ g3 { gammaContext = ls <> (solved : ms) <> (x : rs) }
        Nothing -> return g3

--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate scope (ForallU x b) tb@(ExistU _ [] _) g1
  = instantiate
      scope
      (substitute x b) -- [Eb/x]B
      tb -- Ea
      (g1 +> MarkG x +> ExistG x [] []) -- g1[Ea],>Eb,Eb
  >>= cut (MarkG x)
--  g1 |- t
-- ----------------------------------------- InstRSolve
--  g1,Ea,g2 |- t <=: Ea -| g1,Ea=t,g2
instantiate _ ta tb@(ExistU v [] []) g1 =
  case access1 v (gammaContext g1) of
    (Just (ls, _, rs)) -> do
        solved <- solve v ta
        return $ g1 { gammaContext = ls ++ solved : rs }
    Nothing ->
      case lookupU v g1 of
        (Just _) -> return g1
        Nothing -> Left . InstantiationError ta tb . render
          $ "Error in InstRSolve with gamma:\n" <> tupled (map pretty (gammaContext g1))


--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate _ ta@(ExistU v [] []) tb g1 =
  case access1 v (gammaContext g1) of
    (Just (ls, _, rs)) -> do
        solved <- solve v tb
        return $ g1 { gammaContext = ls ++ solved : rs }
    Nothing ->
      case lookupU v g1 of
        (Just _) -> return g1
        Nothing -> Left . InstantiationError ta tb . render
          $ "Error in InstLSolve:" <+> tupled (map pretty (gammaContext g1))

instantiate _ ta tb _ = Left $ InstantiationError ta tb "Unexpected types"

solve :: TVar -> TypeU -> Either TypeError GammaIndex
solve v t
    | v `elem` (mapMaybe toTVar . Set.toList . free $ t) = Left InfiniteRecursion
    | otherwise = Right (SolvedG v t)
    where
        toTVar :: TypeU -> Maybe TVar
        toTVar (ExistU v' _ _) = Just v'
        toTVar (VarU v') = Just v'
        toTVar _ = Nothing



occursCheck :: TypeU -> TypeU -> MT.Text -> Either TypeError ()
occursCheck t1 t2 place =
  if Set.member t1 (free t2)
  then Left $ OccursCheckFail t1 t2 place
  else Right ()



-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> TypeU -> TypeU
substitute v = substituteTVar v (ExistU v [] [])

access1 :: TVar -> [GammaIndex] -> Maybe ([GammaIndex], GammaIndex, [GammaIndex])
access1 v gs =
  case findIndex (exists v) gs of
    (Just 0) -> Just ([], head gs, tail gs)
    (Just i) -> Just (take i gs, gs !! i, drop (i + 1) gs)
    _ -> Nothing
  where
    exists :: TVar -> GammaIndex -> Bool
    exists v1 (ExistG v2 _ _) = v1 == v2
    exists _ _ = False


access2
  :: TVar
  -> TVar
  -> [GammaIndex]
  -> Maybe ([GammaIndex], GammaIndex, [GammaIndex], GammaIndex, [GammaIndex])
access2 lv rv gs =
  case access1 lv gs of
    Just (ls, x, rs) ->
      case access1 rv rs of
        Just (ls', y, rs') -> Just (ls, x, ls', y, rs')
        _ -> Nothing
    _ -> Nothing


-- | Look up a solved existential type variable
lookupU :: TVar -> Gamma -> Maybe TypeU
lookupU v (gammaContext -> gs0) = f gs0 where
  f [] = Nothing
  f ((SolvedG v' t):gs)
    | v == v' = Just t
    | otherwise = f gs
  f (_:gs) = f gs


-- | Look up a solved existential type variable
lookupE :: EVar -> Gamma -> Maybe TypeU
lookupE v (gammaContext -> gs0) = f gs0 where
  f :: [GammaIndex] -> Maybe TypeU
  f [] = Nothing
  f ((AnnG v' t):gs)
    | v == v' = Just t
    | otherwise = f gs
  f (_:gs) = f gs


-- | remove context up to a marker
cut :: GammaIndex -> Gamma -> Either TypeError Gamma
cut i g = do
  xs1 <- f (gammaContext g)
  return $ g { gammaContext = xs1 }
  where
  f [] = Left $ EmptyCut i
  f (x:xs)
    | i == x = return xs
    | otherwise = f xs


newvar :: MT.Text -> Gamma -> (Gamma, TypeU)
newvar = newvarRich [] []


newvarRich
  :: [TypeU] -- ^ type parameters
  -> [(Key, TypeU)] -- ^ key-value pairs
  -> MT.Text -- ^ prefix, just for readability
  -> Gamma
  -> (Gamma, TypeU)
newvarRich ps rs prefix g =
  let (g', v) = tvarname g prefix
  in (g' +> ExistG v ps rs, ExistU v ps rs)

-- | standardize quantifier names, for example, replace `a -> b` with `v0 -> v1`.
rename :: Gamma -> TypeU -> (Gamma, TypeU)
rename g0 (ForallU v@(TV s) t0) =
  let (g1, v') = tvarname g0 (s <> "___q")
      (g2, t1) = rename g1 t0
      t2 = substituteTVar v (VarU v') t1
  in (g2, ForallU v' t2)
-- Unless I add N-rank types, foralls can only be on top, so no need to recurse.
rename g t = (g, t)

renameAnnoS :: (Map.Map EVar EVar, Gamma) -> AnnoS g ManyPoly c -> ((Map.Map EVar EVar, Gamma), AnnoS g ManyPoly c)
renameAnnoS context (AnnoS gt ct e) =
  let (context', e') = renameSExpr context e
  in (context', AnnoS gt ct e')

renameSExpr :: (Map.Map EVar EVar, Gamma) -> ExprS g ManyPoly c -> ((Map.Map EVar EVar, Gamma), ExprS g ManyPoly c)
renameSExpr c0@(m, g) e0 = case e0 of
  (BndS v) -> case Map.lookup v m of
    (Just v') -> (c0, BndS v')
    Nothing -> (c0, BndS v)
  (VarS v (MonomorphicExpr t xs)) ->
    let (context', xs') = statefulMap renameAnnoS c0 xs
    in (context', VarS v (MonomorphicExpr t xs'))
  (VarS v (PolymorphicExpr cls clsName t rs)) ->
    let (ts, ass) = unzip rs
        (context', ass') = statefulMap (statefulMap renameAnnoS) c0 ass
        rs' = zip ts ass'
    in (context', VarS v $ PolymorphicExpr cls clsName t rs')
  (LamS vs x) ->
    let (g', vs') = statefulMap (\g'' (EV v) -> evarname g'' (v <> "___e")) g vs
        m' = foldr (uncurry Map.insert) m (zip vs vs')
        (c1, x') = renameAnnoS (m', g') x
    in (c1, LamS vs' x')
  (AccS k e) ->
    let (c1, e') = renameAnnoS c0 e
    in (c1, AccS k e')
  (AppS e es) ->
    let (c1, es') = statefulMap renameAnnoS c0 es
        (c2, e') = renameAnnoS c1 e -- order matters here, the arguments are bound under the PARENT
    in (c2, AppS e' es')
  (LstS es) ->
    let (c1, es') = statefulMap renameAnnoS c0 es
    in (c1, LstS es')
  (TupS es) ->
    let (c1, es') = statefulMap renameAnnoS c0 es
    in (c1, TupS es')
  (NamS rs) ->
    let (c1, es') = statefulMap renameAnnoS c0 (map snd rs)
    in (c1, NamS (zip (map fst rs) es'))
  e -> (c0, e)

tvarname :: Gamma -> MT.Text -> (Gamma, TVar)
tvarname g prefix =
  let i = gammaCounter g
  in (g {gammaCounter = i + 1}, TV (prefix <> MT.pack (show i)))

evarname :: Gamma -> MT.Text -> (Gamma, EVar)
evarname g prefix =
  let i = gammaCounter g
  in (g {gammaCounter = i + 1}, EV (prefix <> MT.pack (show i)))


-- debugging -------------------

enter :: MDoc -> MorlocMonad ()
enter d = do
  depth <- MM.incDepth
  insetSay $ "--" <> pretty depth <> "-->" <+> d

insetSay :: MDoc -> MorlocMonad ()
insetSay d = do
  MM.sayVVV $ " :" <+> d

seeType :: TypeU -> MorlocMonad ()
seeType t = insetSay $ pretty t

leave :: MDoc -> MorlocMonad ()
leave d = do
  depth <- MM.decDepth
  insetSay $ "<--" <> pretty (depth+1) <> "--" <+> d

seeGamma :: Gamma -> MorlocMonad ()
seeGamma g = MM.sayVVV $ nest 4 $ "Gamma:" <> line <> vsep (map pretty (gammaContext g))

peak :: ExprS g f c -> MorlocMonad ()
peak = insetSay . pretty

peakGen :: AnnoS g f c -> MorlocMonad ()
peakGen = insetSay . pretty

