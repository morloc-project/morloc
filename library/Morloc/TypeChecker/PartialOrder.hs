{-|
Module      : Morloc.TypeChecker.PartialOrder
Description : Partial order implementation for types
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.PartialOrder (
    substitute
  , free
  , isSubtypeOf
  , mostGeneral
  , mostSpecific
  , mostSpecificSubtypes
) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Data.Set as Set
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
    sub (NamT v' rs) = NamT v' [(x, sub t') | (x, t') <- rs]
    sub (ExistT v' ps ds) = ExistT v' (map sub ps) (map (DefaultType . sub . unDefaultType) ds)
    sub t' = t'

-- | TODO: document
free :: Type -> Set.Set Type
free v@(VarT _) = Set.singleton v
free v@(ExistT _ [] _) = Set.singleton v
free (ExistT v ts _) = Set.unions $ Set.singleton (ArrT v ts) : map free ts
free (FunT t1 t2) = Set.union (free t1) (free t2)
free (Forall v t) = Set.delete (VarT v) (free t)
free (ArrT _ xs) = Set.unions (map free xs)
free (NamT _ rs) = Set.unions [free t | (_, t) <- rs]

-- Types are partially ordered, 'forall a . a' is lower (more generic) than
-- Int. But 'forall a . a -> a' cannot be compared to 'forall a . a', since
-- they are different kinds.
-- The order of types is used to choose the most specific serialization functions.
-- As far as serialization is concerned, properties and constraints do not matter.
instance P.PartialOrd Type where
  (<=) (VarT v1) (VarT v2) = v1 == v2
  (<=) (ExistT v1 [] _) (ExistT v2 [] _) = v1 == v2
  (<=) (ExistT v1 ts1 _) (ExistT v2 ts2 _)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
  (<=) (Forall v1 t1) (Forall v2 t2) = (P.<=) t1 (substitute v2 (VarT v1) t2)
  (<=) (FunT t11 t12) (FunT t21 t22)
    =  (P.<=) t11 t21
    && (P.<=) t22 t12
  (<=) (ArrT v1 []) (ArrT v2 []) = v1 == v2
  (<=) (ArrT v1 ts1) (ArrT v2 ts2)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
  (<=) (NamT v1 es1) (NamT v2 es2)
    =  v1 == v2
    && length ts1 == length ts2
    && foldl (&&) True (zipWith (P.<=) ts1 ts2)
    where
      ts1 = map snd es1
      ts2 = catMaybes $ map (\(k,_) -> lookup k es2) es1
  (<=) (Forall v@(TV lang _) t1) t2 = case substituteFirst v t1 t2 of
    (Just t1') -> (P.<=) t1' t2
    Nothing -> (P.<=) (substitute v (getNewVariable lang t1 t2) t1) t2
  (<=) _ _ = False

substituteFirst :: TVar -> Type -> Type -> Maybe Type
substituteFirst v t1 t2 = substitute v <$> findFirst v t1 t2 <*> pure t1

getNewVariable :: Maybe Lang -> Type -> Type -> Type 
getNewVariable lang t1 t2 = VarT $ findNew variables (Set.union (free t1) (free t2))
  where 
    variables = [1 ..] >>= flip replicateM ['a' .. 'z']

    findNew :: [String] -> Set.Set Type -> TVar
    findNew (x:xs) ts
      | Set.member (VarT v) ts = findNew xs ts 
      | otherwise = v
      where
        v = TV lang (MT.pack x)

findFirst :: TVar -> Type -> Type -> Maybe Type
findFirst v (VarT v') t2
  | v == v' = Just t2
  | otherwise = Nothing
findFirst v (Forall v1 t1) (Forall v2 t2)
  | v == v1 = Nothing
  | otherwise = findFirst v t1 (substitute v2 (VarT v1) t2)
findFirst v (FunT t11 t12) (FunT t21 t22)
  = case (findFirst v t11 t21, findFirst v t12 t22) of
    (Just t, _) -> Just t  
    (_, Just t) -> Just t
    _ -> Nothing
findFirst v (ArrT _ ts1) (ArrT _ ts2)
  = listToMaybe . catMaybes $ zipWith (findFirst v) ts1 ts2
findFirst v (NamT _ es1) (NamT _ es2)
  = listToMaybe . catMaybes $ zipWith (findFirst v) ts1 ts2
    where
      ts1 = map snd es1
      ts2 = catMaybes $ map (\(k,_) -> lookup k es2) es1
findFirst _ _ _ = Nothing

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
mostSpecificSubtypes t ts = mostSpecific $ filter (\t2 -> isSubtypeOf t2 t) ts
