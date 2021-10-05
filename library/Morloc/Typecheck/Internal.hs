{-|
Module      : Morloc.Typecheck.Internal
Description : Functions for type checking and type manipulation
Copyright   : (c) Zebulun Arendsee, 2021
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
  , newvarRich
  -- * Typeclasses
  , Applicable(..)
  , Indexable(..)
  -- * manipulating context
  , access1
  , access2
  , lookupU
  , lookupE
  , cut
  , substitute
  , occursCheck
  -- * subtyping
  , subtype
  ) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import Morloc.Data.Doc
import Morloc.Typecheck.Pretty

import qualified Data.Set as Set


class Applicable a where
  apply :: Gamma -> a -> a

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable TypeU where
  -- [G]a = a
  apply _ a@(VarU _) = a
  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunU ts t) = FunU (map (apply g) ts) (apply g t)
  -- [G]ForallU a.a = forall a. [G]a
  apply g (ForallU x a) = ForallU x (apply g a)
  -- [G[a=t]]a = [G[a=t]]t
  apply g (ExistU v ts ds) =
    case lookupU v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistU v (map (apply g) ts) (map (apply g) ds)
  apply g (AppU v ts) = AppU v (map (apply g) ts)
  apply g (NamU o n ps rs) = NamU o n ps [(k, apply g t) | (k, t) <- rs]

instance Applicable EType where
  apply g e = e { etype = apply g (etype e) }


class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable TypeU where
  index (ExistU t ts ds) = ExistG t ts ds
  index t = error $ "Can only index ExistT, found: " <> show t


(+>) :: Indexable a => Gamma -> a -> Gamma
(+>) g x = g {gammaContext = (index x) : gammaContext g}


(++>) :: Indexable a => Gamma -> [a] -> Gamma
(++>) g xs = g {gammaContext = map index (reverse xs) <> gammaContext g }


-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: TypeU -> TypeU -> Gamma -> Either TypeError Gamma

-- VarU vs VarT
subtype t1@(VarU (TV lang1 a1)) t2@(VarU (TV lang2 a2)) g
  -- If everything is the same, do nothing
  --
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l <: a_l -| G[a]
  | lang1 == lang2 && a1 == a2 = return g
  -- If languages are different, do nothing
  --  l1 != l2    b_l2 ~~> a_l1
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l1 <: b_l2 -| G[a]
  | lang1 /= lang2 = return $ g +> SerialConstraint t1 t2

  -- If languages are same, but types are different, raise error
  | lang1 == lang2 && a1 /= a2 = Left $ NotYetImplemented t1 t2 "Within language type conversion not yet implemented"

subtype a@(ExistU (TV l1 _) _ _) b@(ExistU (TV l2 _) _ _) g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- E.a <: E.a -| G[E.a]
  | a == b = return g
  --  l1 == l2
  -- ----------------------------------------- <:AlienExvar
  --  G[E.a,E.b] |- E.a <: E.b -| G[E.a,E.b], E.a ~~> E.b
  | l1 /= l2 = return $ g +> SerialConstraint a b
  --
  -- ----------------------------------------- <:InstantiateL/<:InstantiateR
  --  G[E.a] |- Ea <: Ea -| G[E.a]
  | otherwise
      -- formally, an `Ea notin FV(G)` check should be done here, but since the
      -- types involved are all existentials, it will always pass, so I omit
      -- it.
   = instantiate a b g

--  g1 |- B1 <: A1 -| g2
--  g2 |- [g2]A2 <: [g2]B2 -| g3
-- ----------------------------------------- <:-->
--  g1 |- A1 -> A2 <: B1 -> B2 -| g3
-- 
-- function subtypes are *contravariant* with respect to the input, that is,
-- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
subtype (FunU [] a2) (FunU [] b2) g = subtype a2 b2 g
subtype (FunU (a1:rs1) a2) (FunU (b1:rs2) b2) g1 = do
  g2 <- subtype b1 a1 g1
  subtype (apply g2 (FunU rs1 a2)) (apply g2 (FunU rs2 b2)) g2

--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype t1@(AppU v1 []) t2@(AppU v2 []) g
  | langOf v1 == langOf v2 = subtype (VarU v1) (VarU v2) g
  | otherwise = Left $ SubtypeError t1 t2 "<:App - Cannot compare between languages" 
subtype t1@(AppU v1@(TV l1 _) vs1) t2@(AppU v2@(TV l2 _) vs2) g
  | length vs1 /= length vs2 = Left $ SubtypeError t1 t2 "<:App - Cannot subtype types with unequal parameter count"
  | l1 /= l2 = return $ g +> SerialConstraint t1 t2
  | v1 == v2 = compareApp vs1 vs2 g
  | otherwise = Left $ SubtypeError t1 t2 "<:App - Unequal names in AppU of the same language" 
  where
    compareApp :: [TypeU] -> [TypeU] -> Gamma -> Either TypeError Gamma
    compareApp [] [] g' = return g'
    compareApp (t1':ts1') (t2':ts2') g' = do
      g'' <- subtype t1' t2' g'
      compareApp ts1' ts2' g''
    compareApp _ _ _ = Left $ SubtypeError t1 t2 "<:App - Type mismatch in AppU"

-- subtype unordered records
subtype (NamU _ v1 _ []) (NamU _ v2 _ []) g = subtype (VarU v1) (VarU v2) g
subtype t1@(NamU _ _ _ []) t2@(NamU _ _ _ _) _ =
  Left $ SubtypeError t1 t2 "NamU - Unequal number of fields"
subtype t1@(NamU _ _ _ _ ) t2@(NamU _ _ _ []) _ =
  Left $ SubtypeError t1 t2 "NamU - Unequal number of fields"
subtype t1@(NamU o1 v1 p1 ((k1,x1):rs1)) t2@(NamU o2 v2 p2 es2) g0
  | langOf v1 == langOf t2 =
    case filterApart (\(k2, _) -> k2 == k1) es2 of
      (Nothing, _) -> Left $ SubtypeError t1 t2 "NamU - Unequal fields"
      (Just (_, x2), rs2) -> do
          g1 <- subtype x1 x2 g0
          g2 <- subtype (NamU o1 v1 p1 rs1) (NamU o2 v2 p2 rs2) g1
          return g2
  | otherwise = return g0 -- incomparable

--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype a b@(ExistU _ [] _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck a b "InstantiateR" >> instantiate a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype a@(ExistU _ [] _) b g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck b a "InstantiateL" >> instantiate a b g

subtype a@(AppU v1 ps1) b@(ExistU v2 ps2 _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = subtype (AppU v1 ps1) (ExistU v2 ps2 []) g
subtype t1@(ExistU v1 ps1 _) t2@(AppU v2 ps2) g1
  | langOf v1 /= langOf v2 = return g1 -- incomparable
  | length ps1 /= length ps2 = Left $ SubtypeError t1 t2 "InstantiateL - Expected equal number of type parameters"
  | otherwise = do
    g2 <- foldM (\g (p1, p2) -> subtype p1 p2 g) g1 (zip ps1 ps2)
    case access1 v1 (gammaContext g2) of
      Just (rs, _, ls) ->
        return $ g2 { gammaContext = rs ++ [SolvedG v1 t2] ++ ls }
      Nothing -> return g2 -- it is already solved, so do nothing

--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
--
subtype (ForallU v@(TV lang _) a) b g0
  | lang /= langOf b = return g0
  | otherwise = do
      let (g1, a') = newvar lang g0
      g2 <- subtype (substituteTVar v a' a) b (g1 +> MarkG v +> a')
      cut (MarkG v) g2

--  g1,a |- A <: B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype a (ForallU v@(TV lang _) b) g
  | lang /= langOf a = return g
  | otherwise = subtype a b (g +> VarG v) >>= cut (VarG v)

-- fall through
subtype a b _ = Left $ SubtypeError a b "Type mismatch"



-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: TypeU -> TypeU -> Gamma -> Either TypeError Gamma

-- --  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- A1 <=: Ea1 -| g2
-- --  g2 |- Ea2 <=: [g2]A2 -| g3
-- -- ----------------------------------------- InstLApp
-- --  g1[Ea] |- Ea <=: A1 -> A2 -| g3
instantiate ta@(ExistU v@(TV lang _) [] _) tb@(FunU as b) g1 = do
  let (g2, eas) = statefulMap (\g _ -> newvar lang g) g1 as 
      (g3, eb) = newvar lang g2
  g4 <- case access1 v (gammaContext g3) of
      Just (rs, _, ls) ->
        return $ g3 { gammaContext = rs ++ [SolvedG v (FunU eas eb)] ++ map index eas ++ ls }
      Nothing -> Left $ InstantiationError ta tb "Error in InstLApp"
  g5 <- foldlM (\g (e, t) -> instantiate e t g) g4 (zip eas as)
  instantiate eb (apply g5 b) g5 



--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRApp
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate ta@(FunU as b) tb@(ExistU v@(TV lang _) [] _) g1 = do
  let (g2, eas) = statefulMap (\g _ -> newvar lang g) g1 as 
      (g3, eb) = newvar lang g2
  g4 <- case access1 v (gammaContext g3) of
    Just (rs, _, ls) ->
        return $ g3 { gammaContext = rs ++ [SolvedG v (FunU eas eb)] ++ map index eas ++ ls }
    Nothing -> Left $ InstantiationError ta tb "Error in InstRApp"
  g5 <- foldlM (\g (e, t) -> instantiate t e g) g4 (zip eas as)
  g6 <- instantiate eb (apply g5 b) g5
  return g6

--
-- ----------------------------------------- InstLAllR
--
instantiate ta@(ExistU _ _ _) tb@(ForallU v2 t2) g1
  | langOf ta /= langOf tb = return g1
  | otherwise = instantiate ta t2 (g1 +> VarG v2) >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate ta@(ExistU v1 ps1 []) tb@(ExistU v2 ps2 []) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- case access2 v1 v2 (gammaContext g2) of
    -- InstLReach
    (Just (ls, _, ms, x, rs)) -> return $ g2 { gammaContext = ls <> (SolvedG v1 tb : ms) <> (x : rs) }
    Nothing ->
      case access2 v2 v1 (gammaContext g2) of
      -- InstRReach
        (Just (ls, _, ms, x, rs)) ->
          return $ g2 { gammaContext = ls <> (SolvedG v2 ta : ms) <> (x : rs) }
        Nothing -> return g2
  return g3
--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate ta@(ForallU x b) tb@(ExistU _ [] _) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      instantiate
        (substitute x b) -- [Eb/x]B
        tb -- Ea
        (g1 +> MarkG x +> ExistG x [] []) -- g1[Ea],>Eb,Eb
      >>= cut (MarkG x)
--  g1 |- t
-- ----------------------------------------- InstRSolve
--  g1,Ea,g2 |- t <=: Ea -| g1,Ea=t,g2
instantiate ta tb@(ExistU v [] []) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v (gammaContext g1) of
        (Just (ls, _, rs)) -> return $ g1 { gammaContext = ls ++ (SolvedG v ta) : rs }
        Nothing ->
          case lookupU v g1 of
            (Just _) -> return g1
            Nothing -> Left . InstantiationError ta tb . render
              $ "Error in InstRSolve:" <+> tupled (map prettyGammaIndex (gammaContext g1))


--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate ta@(ExistU v [] []) tb g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v (gammaContext g1) of
        (Just (ls, _, rs)) -> return $ g1 { gammaContext = ls ++ (SolvedG v tb) : rs }
        Nothing ->
          case lookupU v g1 of
            (Just _) -> return g1
            Nothing -> Left . InstantiationError ta tb . render
              $ "Error in InstLSolve:" <+> tupled (map prettyGammaIndex (gammaContext g1))

-- if defaults are involved, no solving is done, but the subtypes of parameters
-- and defaults needs to be checked. 
instantiate (ExistU _ ps1 ds1) (ExistU _ ps2 ds2) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- foldM (\g d1 -> foldM (\g' d2 -> subtype d1 d2 g') g ds2) g2 ds1
  return g3

-- bad
instantiate _ _ g = return g


occursCheck :: TypeU -> TypeU -> MT.Text -> Either TypeError ()
occursCheck t1 t2 place = do
  case Set.member t1 (free t2) of
    True -> Left $ OccursCheckFail t1 t2 place
    False -> Right ()



-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> TypeU -> TypeU
substitute v t = substituteTVar v (ExistU v [] []) t


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


newvar :: Maybe Lang -> Gamma -> (Gamma, TypeU)
newvar = newvarRich [] []


newvarRich
  :: [TypeU] -- ^ type parameters
  -> [TypeU] -- ^ type defaults
  -> Maybe Lang
  -> Gamma
  -> (Gamma, TypeU)
newvarRich ps ds lang g =
  let i = gammaCounter g
      v = TV lang (newvars !! i)
  in (g {gammaCounter = i + 1} +> ExistG v ps ds, ExistU v ps ds)
  where
    newvars =
      zipWith (\x y -> MT.pack (x ++ show y)) (repeat "t") ([0 ..] :: [Integer])
