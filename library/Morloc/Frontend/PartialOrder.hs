{-|
Module      : Morloc.Frontend.PartialOrder
Description : Partial order implementation for types
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.PartialOrder (
    isSubtypeOf
  , equivalent
  , mostGeneral
  , mostSpecific
  , mostSpecificSubtypes
  , (<=)
) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import qualified Data.PartialOrd as P
import qualified Data.List as DL

-- Types are partially ordered, 'forall a . a' is lower (more generic) than
-- Int. But 'forall a . a -> a' cannot be compared to 'forall a . a', since
-- they are different kinds.
-- The order of types is used to choose the most specific serialization functions.
-- As far as serialization is concerned, properties and constraints do not matter.
instance P.PartialOrd TypeU where
  (<=) (VarU v1) (VarU v2) = v1 == v2
  (<=) (ExistU v1 [] _) (ExistU v2 [] _) = v1 == v2
  (<=) (ExistU v1 ts1 _) (ExistU v2 ts2 _)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
  (<=) (ForallU v t1) t2
    | (P.==) (ForallU v t1) t2 = True
    | otherwise = (P.<=) (substituteFirst v t1 t2) t2
  (<=) (FunU (t11:rs1) t12) (FunU (t21:rs2) t22) = t11 <= t21 && FunU rs1 t12 <= FunU rs2 t22
  (<=) (FunU [] t12) (FunU [] t22) = t12 <= t22 
  (<=) (AppU t1 (t11:rs1)) (AppU t2 (t21:rs2)) = t11 <= t21 && AppU t1 rs1 <= AppU t2 rs2
  (<=) (AppU t1 []) (AppU t2 []) = t1 <= t2
  -- the records do not need to be in the same order to be equivalent
  -- ---- do I need to sort on ps1/ps2 as well?
  (<=) (NamU o1 n1 ps1 ((k1,e1):rs1)) (NamU o2 n2 ps2 es2)
    = case DL.partition ((== k1) . fst) es2 of
       ([(_,e2)], rs2) -> e1 <= e2 && NamU o1 n1 ps1 rs1 <= NamU o2 n2 ps2 rs2
       _ -> False
  (<=) (NamU o1 n1 ps1 []) (NamU o2 n2 ps2 [])
    = o1 == o2 && n1 == n2 && length ps1 == length ps2
  (<=) _ _ = False

  (==) (ForallU v1@(TV _ _) t1) (ForallU v2 t2) =
    if Set.member (VarU v1) (free t2)
    then
      let v = newVariable t1 t2
      in (P.==) (substituteTVar v1 (VarU v) t1) (substituteTVar v2 (VarU v) t2)
    else (P.==) t1 (substituteTVar v2 (VarU v1) t2)
  (==) a b = a == b

-- Substitute all v for the first term in t2 that corresponds to v in t1. If v
-- does not occur in t1, then t1 is returned unchanged (e.g., `forall a . Int`).
substituteFirst :: TVar -> TypeU -> TypeU -> TypeU
substituteFirst v t1 t2 = case findFirst v t1 t2 of
  (Just t) -> substituteTVar v t t1
  Nothing -> t1

findFirst :: TVar -> TypeU -> TypeU -> Maybe TypeU
findFirst v = f where
  f (VarU v') t2
    | v == v' = Just t2
    | otherwise = Nothing
  f (ForallU v1 t1) (ForallU v2 t2)
    | v == v1 = Nothing
    | otherwise = f t1 (substituteTVar v2 (VarU v1) t2)
  f (ForallU v1 t1) t2
    | v == v1 = Nothing
    | otherwise = f (substituteTVar v1 (VarU v1) t1) t2
  f (FunU ts1 t1) (FunU ts2 t2)
    = foldl firstOf Nothing (zipWith f (ts1 <> [t1]) (ts2 <> [t2]))
  f (AppU t1 ts1) (AppU t2 ts2)
    = foldl firstOf Nothing (zipWith f (t1:ts1) (t2:ts2))
  f (NamU o1 n1 ps1 ((k1,e1):rs1)) (NamU o2 n2 ps2 es2)
    = case DL.partition ((== k1) . fst) es2 of
       ([(_,e2)], rs2) -> firstOf (f e1 e2) (f (NamU o1 n1 ps1 rs1) (NamU o2 n2 ps2 rs2))
       _ -> Nothing 
  f _ _ = Nothing

  firstOf :: Maybe a -> Maybe a -> Maybe a
  firstOf (Just x) _ = Just x
  firstOf _ (Just x) = Just x
  firstOf _ _ = Nothing


-- | is t1 a generalization of t2?
isSubtypeOf :: TypeU -> TypeU -> Bool
isSubtypeOf t1 t2 = case P.compare t1 t2 of
  (Just x) -> x <= EQ
  _ -> False

equivalent :: TypeU -> TypeU -> Bool
equivalent t1 t2 = isSubtypeOf t1 t2 && isSubtypeOf t2 t1

-- | find all types that are not greater than any other type
mostGeneral :: [TypeU] -> [TypeU]
mostGeneral ts = P.minima ts

-- | find all types that are not less than any other type
mostSpecific :: [TypeU] -> [TypeU]
mostSpecific ts = P.maxima ts

-- | find the most specific subtypes
mostSpecificSubtypes :: TypeU -> [TypeU] -> [TypeU]
mostSpecificSubtypes t ts = mostSpecific $ filter (\t2 -> isSubtypeOf t2 t) ts
