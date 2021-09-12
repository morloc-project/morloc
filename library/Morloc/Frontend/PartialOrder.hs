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
import qualified Morloc.Data.Text as MT
import qualified Data.Set as Set
import qualified Data.PartialOrd as P

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
  (<=) (CatU k1 t11 t12) (CatU k2 t21 t22)
    = k1 == k2
    && (P.<=) t11 t21
    && (P.<=) t22 t12
  (<=) (ForallU v t1) t2
    | (P.==) (ForallU v t1) t2 = True
    | otherwise = (P.<=) (substituteFirst v t1 t2) t2
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
findFirst v (VarU v') t2
  | v == v' = Just t2
  | otherwise = Nothing
findFirst v (ForallU v1 t1) (ForallU v2 t2)
  | v == v1 = Nothing
  | otherwise = findFirst v t1 (substituteTVar v2 (VarU v1) t2)
findFirst v (ForallU v1 t1) t2
  | v == v1 = Nothing
  | otherwise = findFirst v (substituteTVar v1 (VarU v1) t1) t2
findFirst v (CatU k1 t11 t12) (CatU k2 t21 t22)
  | k1 == k2 = case (findFirst v t11 t21, findFirst v t12 t22) of
    (Just t, _) -> Just t
    (_, Just t) -> Just t
    _ -> Nothing
findFirst _ _ _ = Nothing

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
