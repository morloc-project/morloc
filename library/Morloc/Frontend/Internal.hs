{-|
Module      : Morloc.Frontend.Internal
Description : Utilities for type checking
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Internal
  ( generalize
  ) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT


-- | Deal with existentials.
-- This function is used to resolve remaining existentials when no further
-- inferences about their type can be made. If the existentials have a default
-- type, then that type can be used to replace the existential. Otherwise, the
-- existential can be cast as generic (ForallU).
generalize :: TypeU -> TypeU
generalize t0 = generalize' (existentialMap t0) t0 where
  generalize' :: [(TVar, Name)] -> TypeU -> TypeU
  generalize' [] t = t
  generalize' ((e, r):xs) t = generalize' xs (generalizeOne e r t)

  variables = [1 ..] >>= flip replicateM ['a' .. 'z']

  existentialMap t =
    zip (Set.toList (findExistentials t)) (map (Name . MT.pack) variables)

  findExistentials :: TypeU -> Set.Set TVar
  findExistentials (VarU _) = Set.empty
  findExistentials (ExistU v ts rs) =
    Set.unions
      $ [Set.singleton v]
      ++ map findExistentials ts
      ++ map (findExistentials . snd) rs
  findExistentials (ForallU v t) = Set.delete v (findExistentials t)
  findExistentials (FunU ts t) = Set.unions (findExistentials t : map findExistentials ts)
  findExistentials (AppU t ts) = Set.unions (findExistentials t : map findExistentials ts)
  findExistentials (NamU _ _ ps rs) = Set.unions (map findExistentials (map snd rs <> ps))

  generalizeOne :: TVar -> Name -> TypeU -> TypeU
  generalizeOne v0@(TV lang0 _) r0 t0 = ForallU (TV lang0 (unName r0)) (f v0 t0)
    where
     -- the type term that is being substitute in
      replacementTerm = TV lang0 (unName r0)

      f :: TVar -> TypeU -> TypeU
      f v t1@(ExistU v' [] _)
        | v == v' = VarU replacementTerm -- substitute
        | otherwise = t1
      f v (ExistU v' ts [])
        | v == v' = AppU (VarU replacementTerm) (map (f v) ts) -- substitute
        | otherwise = AppU (VarU v) (map (f v) ts)
      f _ (ExistU _ _ _) = error "Bad thing happen"

      f v t1@(ForallU x t2)
        | v /= x = ForallU x (f v t2)
        | otherwise = t1
      f v (FunU ts t) = FunU (map (f v) ts) (f v t)
      f v (AppU t ts) = AppU (f v t) (map (f v) ts)
      f v (NamU o n ps rs) = NamU o n (map (f v) ps) [(k, f v t) | (k, t) <- rs]
      f _ t@(VarU _) = t
