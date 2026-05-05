{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Typecheck.SetSolver
Description : Type-level unordered set solver
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Solves equality and basic constraint problems on Set-kinded type
expressions (Stage 8 of the tables refactor — foundational kind layer).

The canonical form is a sorted, dedup'd list of element 'TypeU' values
plus an optional set-tail variable. Two sets are equal when their
canonical forms agree.

Decidability:

- Two ground sets compare element-wise after sorting.
- A ground set equated to a set var solves the var.
- Two set vars on either side defer.
- Union (@+@) and intersect (@&@) reduce on ground operands; otherwise
  defer.
- Difference (@-@) is a no-op for absent keys when the LHS is ground;
  otherwise defer.
-}
module Morloc.Typecheck.SetSolver
  ( SetExpr (..)
  , SetCanon (..)
  , SetError (..)
  , normalize
  , setEqual
  , solveSet
  , canonToTypeU
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import qualified Data.Set as Set
import Data.Text (Text)
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Namespace.Type (TypeU (..))

-- | The structural form a SetExpr can take.
data SetExpr
  = SetVar TVar
  | SetEmpty
  | SetLit [TypeU]              -- ^ unordered set of literal elements
  | SetUnion SetExpr SetExpr
  | SetInter SetExpr SetExpr
  | SetDiff SetExpr SetExpr
  deriving (Eq, Ord, Show)

-- | Canonical form: a sorted-and-dedup'd element list plus an optional
-- set-tail variable.
data SetCanon = SetCanon
  { setElems :: ![TypeU]
  , setTail  :: !(Maybe TVar)
  } deriving (Eq, Ord, Show)

-- | Errors from set-solver attempts.
data SetError
  = SetContradiction Text
  | SetDeferred
  deriving (Eq, Show)

-- | Reduce a SetExpr to canonical form.
normalize :: SetExpr -> Either SetError SetCanon
normalize SetEmpty = Right (SetCanon [] Nothing)
normalize (SetVar v) = Right (SetCanon [] (Just v))
normalize (SetLit es) = Right (SetCanon (sortDedup es) Nothing)
normalize (SetUnion a b) = do
  SetCanon ea ta <- normalize a
  SetCanon eb tb <- normalize b
  case (ta, tb) of
    (Nothing, Nothing) -> Right (SetCanon (sortDedup (ea ++ eb)) Nothing)
    (Just _, Nothing) -> Left SetDeferred
    (Nothing, Just _) -> Left SetDeferred
    (Just _, Just _) -> Left SetDeferred
normalize (SetInter a b) = do
  SetCanon ea ta <- normalize a
  SetCanon eb tb <- normalize b
  case (ta, tb) of
    (Nothing, Nothing) ->
      Right (SetCanon (sortDedup [x | x <- ea, x `elem` eb]) Nothing)
    _ -> Left SetDeferred
normalize (SetDiff a b) = do
  SetCanon ea ta <- normalize a
  SetCanon eb tb <- normalize b
  case (ta, tb) of
    (Nothing, Nothing) ->
      Right (SetCanon [x | x <- ea, x `notElem` eb] Nothing)
    _ -> Left SetDeferred

sortDedup :: [TypeU] -> [TypeU]
sortDedup = nub . sort

-- | Equality on Set expressions, after normalization.
setEqual :: SetExpr -> SetExpr -> Bool
setEqual a b = case (normalize a, normalize b) of
  (Right ca, Right cb) -> ca == cb
  _ -> False

-- | Attempt to solve @a == b@. Outcomes mirror the Rec / List solvers.
solveSet :: SetExpr -> SetExpr -> Either SetError (Map TVar SetExpr)
solveSet lhs rhs = do
  l <- normalize lhs
  r <- normalize rhs
  solveCanon l r

solveCanon :: SetCanon -> SetCanon -> Either SetError (Map TVar SetExpr)
solveCanon (SetCanon ea Nothing) (SetCanon eb Nothing)
  | ea == eb = Right Map.empty
  | otherwise =
      Left (SetContradiction $ "Set elements differ")
solveCanon (SetCanon ea (Just v)) (SetCanon eb Nothing)
  -- LHS has tail var, RHS is ground. The LHS literal elements must be a
  -- subset of RHS, and v is solved to (RHS minus LHS elements).
  | all (`elem` eb) ea = Right (Map.singleton v (SetLit [x | x <- eb, x `notElem` ea]))
  | otherwise =
      Left (SetContradiction "Set LHS literal exceeds RHS")
solveCanon ca@(SetCanon _ Nothing) cb@(SetCanon _ (Just _)) = solveCanon cb ca
-- Both sides carry a tail variable. When the literal element sets are
-- equal we can unify the two tail variables (a no-op if they are the
-- same name, otherwise bind one to the other). Without this, two
-- distinct fresh set variables introduced at different call sites
-- never get linked even when subtype tries to equate them, breaking
-- constraint propagation. Mirrors the analogous fix in RecSolver.
solveCanon (SetCanon ea (Just va)) (SetCanon eb (Just vb))
  | ea == eb =
      if va == vb
      then Right Map.empty
      else Right (Map.singleton vb (SetVar va))
  | otherwise = Left SetDeferred

-- | Lift a canonical Set back to a TypeU.
canonToTypeU :: SetCanon -> TypeU
canonToTypeU (SetCanon es tail_) =
  let lit = if null es then SetEmptyU else SetUnionU SetEmptyU (litsToTypeU es)
      tailType = maybe SetEmptyU SetVarU tail_
   in case (es, tail_) of
        ([], Nothing) -> SetEmptyU
        (_, Nothing) -> litsToTypeU es
        ([], Just _) -> tailType
        (_, Just _) -> SetUnionU tailType (litsToTypeU es)
  where
    -- Surface representation of a ground set is a SetUnion-chain, since
    -- there is no SetLitU constructor (the surface uses ListToSet of a
    -- list literal). For solver-internal use this nested SetUnionU form
    -- is fine because solveCanon only inspects normalize's output.
    litsToTypeU [] = SetEmptyU
    litsToTypeU [x] = SetUnionU SetEmptyU (singletonU x)
    litsToTypeU (x : xs) = SetUnionU (singletonU x) (litsToTypeU xs)
    -- Best-effort singleton: wraps a single element. The caller should
    -- be using the surface ListToSet form anyway.
    singletonU x = SetUnionU SetEmptyU (SetUnionU x SetEmptyU)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- The compiler isn't using these helper imports yet; suppress unused-
-- import warnings when the SetSolver isn't fully exercised.
_unused :: ()
_unused = let _ = Set.empty :: Set.Set Int in ()
