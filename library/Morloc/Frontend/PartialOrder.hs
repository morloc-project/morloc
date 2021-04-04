{-|
Module      : Morloc.Frontend.PartialOrder
Description : Partial order implementation for types
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.PartialOrder (
    substitute
  , free
  , isSubtypeOf
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

-- | substitute all appearances of a given variable with a given new type
substitute :: TVar -> UnresolvedType -> UnresolvedType -> UnresolvedType
substitute v@(TV _ _) (ForallU q r) t = 
  if Set.member (VarU q) (free t)
  then
    let q' = getNewVariable r t -- get unused variable name from [a, ..., z, aa, ...]
        r' = substitute q (VarU q') r -- substitute the new variable into the unqualified type
    in ForallU q' (substitute v r' t)
  else
    ForallU q (substitute v r t)
substitute v r t = sub t
  where
    sub :: UnresolvedType -> UnresolvedType
    sub t'@(VarU v')
      | v == v' = r
      | otherwise = t'
    sub (FunU t1 t2) = FunU (sub t1) (sub t2)
    sub t'@(ForallU x t'')
      | v /= x = ForallU x (sub t'')
      | otherwise = t' -- allows shadowing of the variable
    sub (ArrU v' ts) = ArrU v' (map sub ts)
    sub (NamU namType v' ts rs) = NamU namType v' (map sub ts) [(x, sub t') | (x, t') <- rs]
    sub (ExistU v' ps ds) = ExistU v' (map sub ps) (map sub ds)

free :: UnresolvedType -> Set.Set UnresolvedType
free v@(VarU _) = Set.singleton v
free v@(ExistU _ [] _) = Set.singleton v
free (ExistU v ts _) = Set.unions $ Set.singleton (ArrU v ts) : map free ts
free (FunU t1 t2) = Set.union (free t1) (free t2)
free (ForallU v t) = Set.delete (VarU v) (free t)
free (ArrU _ xs) = Set.unions (map free xs)
free (NamU _ _ _ rs) = Set.unions [free t | (_, t) <- rs]

-- Types are partially ordered, 'forall a . a' is lower (more generic) than
-- Int. But 'forall a . a -> a' cannot be compared to 'forall a . a', since
-- they are different kinds.
-- The order of types is used to choose the most specific serialization functions.
-- As far as serialization is concerned, properties and constraints do not matter.
instance P.PartialOrd UnresolvedType where
  (<=) (VarU v1) (VarU v2) = v1 == v2
  (<=) (ExistU v1 [] _) (ExistU v2 [] _) = v1 == v2
  (<=) (ExistU v1 ts1 _) (ExistU v2 ts2 _)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
  (<=) (FunU t11 t12) (FunU t21 t22)
    =  (P.<=) t11 t21
    && (P.<=) t22 t12
  (<=) (ArrU v1 []) (ArrU v2 []) = v1 == v2
  (<=) (ArrU v1 ts1) (ArrU v2 ts2)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
  (<=) (NamU _ v1 _ es1) (NamU _ v2 _ es2)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
    where
      ts1 = map snd es1
      ts2 = catMaybes $ map (\(k,_) -> lookup k es2) es1
  (<=) (ForallU v t1) t2
    | (P.==) (ForallU v t1) t2 = True
    | otherwise = (P.<=) (substituteFirst v t1 t2) t2
  (<=) _ _ = False

  (==) (ForallU v1@(TV _ _) t1) (ForallU v2 t2) =
    if Set.member (VarU v1) (free t2)
    then
      let v = getNewVariable t1 t2
      in (P.==) (substitute v1 (VarU v) t1) (substitute v2 (VarU v) t2)
    else (P.==) t1 (substitute v2 (VarU v1) t2)
  (==) a b = a == b

-- Substitute all v for the first term in t2 that corresponds to v in t1. If v
-- does not occur in t1, then t1 is returned unchanged (e.g., `forall a . Int`).
substituteFirst :: TVar -> UnresolvedType -> UnresolvedType -> UnresolvedType
substituteFirst v t1 t2 = case findFirst v t1 t2 of
  (Just t) -> substitute v t t1
  Nothing -> t1

-- | get a fresh variable name that is not used in t1 or t2, it reside in the same namespace as the first type
getNewVariable :: UnresolvedType -> UnresolvedType -> TVar
getNewVariable t1 t2 = findNew variables (Set.union (allVars t1) (allVars t2))
  where 
    variables = [1 ..] >>= flip replicateM ['a' .. 'z']

    findNew :: [String] -> Set.Set UnresolvedType -> TVar
    findNew [] _ = error "Could not fresh variable in an infinite list ... odd"
    findNew (x:xs) ts
      | Set.member (VarU v) ts = findNew xs ts 
      | otherwise = v
      where
        v = TV (langOf t1) (MT.pack x)

    allVars :: UnresolvedType -> Set.Set UnresolvedType
    allVars (ForallU v t) = Set.union (Set.singleton (VarU v)) (allVars t)
    allVars t = free t


findFirst :: TVar -> UnresolvedType -> UnresolvedType -> Maybe UnresolvedType
findFirst v (VarU v') t2
  | v == v' = Just t2
  | otherwise = Nothing
findFirst v (ForallU v1 t1) (ForallU v2 t2)
  | v == v1 = Nothing
  | otherwise = findFirst v t1 (substitute v2 (VarU v1) t2)
findFirst v (ForallU v1 t1) t2
  | v == v1 = Nothing
  | otherwise = findFirst v (substitute v1 (VarU v1) t1) t2
findFirst v (FunU t11 t12) (FunU t21 t22)
  = case (findFirst v t11 t21, findFirst v t12 t22) of
    (Just t, _) -> Just t
    (_, Just t) -> Just t
    _ -> Nothing
findFirst v (ArrU _ ts1) (ArrU _ ts2)
  = listToMaybe . catMaybes $ zipWith (findFirst v) ts1 ts2
findFirst v (NamU _ _ _ es1) (NamU _ _ _ es2)
  = listToMaybe . catMaybes $ zipWith (findFirst v) ts1 ts2
    where
      ts1 = map snd es1
      ts2 = catMaybes $ map (\(k,_) -> lookup k es2) es1
findFirst _ _ _ = Nothing

-- | is t1 a generalization of t2?
isSubtypeOf :: UnresolvedType -> UnresolvedType -> Bool
isSubtypeOf t1 t2 = case P.compare t1 t2 of
  (Just x) -> x <= EQ
  _ -> False

equivalent :: UnresolvedType -> UnresolvedType -> Bool
equivalent t1 t2 = isSubtypeOf t1 t2 && isSubtypeOf t2 t1

-- | find all types that are not greater than any other type
mostGeneral :: [UnresolvedType] -> [UnresolvedType]
mostGeneral ts = P.minima ts

-- | find all types that are not less than any other type
mostSpecific :: [UnresolvedType] -> [UnresolvedType]
mostSpecific ts = P.maxima ts

-- | find the most specific subtypes
mostSpecificSubtypes :: UnresolvedType -> [UnresolvedType] -> [UnresolvedType]
mostSpecificSubtypes t ts = mostSpecific $ filter (\t2 -> isSubtypeOf t2 t) ts
