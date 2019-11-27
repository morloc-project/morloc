{-|
Module      : Morloc.TypeChecker.PartialOrder
Description : Partial order implementation for types
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.PartialOrder (
    substitute
  , isSubtypeOf
  , mostGeneral
  , mostSpecific
  , mostSpecificSubtypes
) where

import Morloc.Namespace
import Prelude hiding (compare)
import qualified Data.PartialOrd as P

-- | substitute all appearances of a given variable with a given new type
substitute :: TVar -> Type -> Type -> Type
substitute v r t = sub t
  where
    sub :: Type -> Type
    sub t'@(VarT v')
      | v == v' = r
      | otherwise = t'
    sub (FunT t1 t2) = FunT (sub t1) (sub t2)
    sub t'@(Forall x t'')
      | v /= x = Forall x (sub t'')
      | otherwise = t' -- allows shadowing of the variable
    sub (ArrT v' ts) = ArrT v' (map sub ts)
    sub (RecT rs) = RecT [(x, sub t') | (x, t') <- rs]
    sub t' = t'

-- Types are partially ordered, 'forall a . a' is lower (more generic) than
-- Int. But 'forall a . a -> a' cannot be compared to 'forall a . a', since
-- they are different kinds.
-- The order of types is used to choose the most specific serialization functions.
-- As far as serialization is concerned, properties and constraints do not matter.
instance P.PartialOrd Type where
  compare (VarT v1) (VarT v2)
    | v1 == v2 = Just EQ
    | otherwise = Nothing
  compare (ExistT v1 []) (ExistT v2 [])
    | v1 == v2 = Just EQ
    | otherwise = Nothing
  compare (ExistT v1 ts1) (ExistT v2 ts2) = Nothing
  compare (Forall v1 t1) (Forall v2 t2) = P.compare t1 (substitute v2 (VarT v1) t2)
  compare (FunT t11 t12) (FunT t21 t22)
    | ord1 <= ord2 = ord1
    | otherwise = Nothing
    where
      ord1 = P.compare t11 t21
      ord2 = P.compare t12 t22
  compare (ArrT v1 []) (ArrT v2 [])
    | v1 == v2 = Just EQ
    | otherwise = Nothing
  compare (ArrT v1 ts1) (ArrT v2 ts2)
    | v1 /= v2 || length ts1 /= length ts2 = Nothing
    | not (all isJust order) = Nothing
    | all (== Just EQ) order = Just EQ
    | all (<= Just EQ) order = Just LT
    | all (>= Just EQ) order = Just GT
    where
      order = zipWith P.compare ts1 ts2
  compare (RecT es1) (RecT es2)
    | length ts1 /= length ts2 = Nothing
    | not (all isJust order) = Nothing
    | all (== Just EQ) order = Just EQ
    | all (<= Just EQ) order = Just LT
    | all (>= Just EQ) order = Just GT
    where
      ts2 = catMaybes $ map (\(k,_) -> lookup k es2) es1
      ts1 = map snd es1
      order = zipWith P.compare ts1 ts2
  compare (Forall _ t) _ = Just LT
  compare _ (Forall _ t) = Just GT
  compare (ExistT _ _) _ = Just LT
  compare _ (ExistT _ _) = Just GT
  compare _ _ = Nothing

-- | is t1 a generalization of t2?
isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf t1 t2 = case P.compare t1 t2 of
  (Just x) -> x <= EQ
  _ -> False

-- | find all types that are not greater than any other type
mostGeneral :: [Type] -> [Type]
mostGeneral ts = P.minima ts

-- | find all types that are not less than any other type
mostSpecific :: [Type] -> [Type]
mostSpecific ts = P.maxima ts

-- | find the most specific subtypes
mostSpecificSubtypes :: Type -> [Type] -> [Type]
mostSpecificSubtypes t ts = mostSpecific $ filter (\t2 -> P.compare t2 t <= Just EQ) ts
