{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Typecheck.StrSolver
Description : Type-level string equality solver (literals-only)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Solves equality constraints between Str-kinded type expressions. Per the
Stage 0 design memo (plans/tables/04-str-solver-scope.md) this solver is
intentionally restricted: equations with a free Str variable on either
side defer indefinitely. Word equations (Makanin's algorithm) are NOT
attempted. Only literal-vs-literal equality and literal-fold of concat
chains are solved.

The natural use of Str-kinded variables is for column names: a Stdlib
function like 'add :: f:Str -> Vector n a -> Table n r -> ...' takes a
literal column name at every call site. The constraint 'f = "age"' is
satisfied by the standard variable-assignment path (handled in the
typechecker's gamma), not by this solver. The solver only fires when an
equation pins down a Str variable to a literal that conflicts with
another literal.
-}
module Morloc.Typecheck.StrSolver
  ( StrExpr(..)
  , StrError(..)
  , normalize
  , strEqual
  , solveStr
  , isGround
  , freeStrVars
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.Namespace.Prim (TVar (..))

-- | A type-level string expression.
data StrExpr
  = StrLit Text             -- ^ literal: "foo"
  | StrVar TVar             -- ^ type variable of kind Str
  | StrConcat StrExpr StrExpr
  deriving (Eq, Ord, Show)

-- | Errors from str-solver attempts.
data StrError
  = StrContradiction Text Text  -- ^ literal-vs-literal mismatch
  | StrDeferred                 -- ^ cannot solve without more info
  deriving (Eq, Show)

-- | Canonicalize a StrExpr by folding adjacent literals through concat.
-- The result is either a single literal, a single variable, or a
-- left-leaning concat chain whose leaves are alternating literals and
-- variables (with no two literals adjacent).
normalize :: StrExpr -> StrExpr
normalize (StrLit s) = StrLit s
normalize (StrVar v) = StrVar v
normalize (StrConcat a b) = case (normalize a, normalize b) of
  (StrLit "", x) -> x
  (x, StrLit "") -> x
  (StrLit s1, StrLit s2) -> StrLit (s1 <> s2)
  (StrLit s1, StrConcat (StrLit s2) y) -> normalize (StrConcat (StrLit (s1 <> s2)) y)
  (StrConcat x (StrLit s1), StrLit s2) -> normalize (StrConcat x (StrLit (s1 <> s2)))
  (x, y) -> StrConcat x y

-- | True iff a StrExpr has no free variables.
isGround :: StrExpr -> Bool
isGround (StrLit _) = True
isGround (StrVar _) = False
isGround (StrConcat a b) = isGround a && isGround b

-- | The set of free Str variables in an expression.
freeStrVars :: StrExpr -> Set.Set TVar
freeStrVars (StrLit _) = Set.empty
freeStrVars (StrVar v) = Set.singleton v
freeStrVars (StrConcat a b) = Set.union (freeStrVars a) (freeStrVars b)

-- | Equality on Str expressions, after normalization. Returns True iff
-- both expressions reduce to the same canonical form. Variables compare
-- structurally; this does NOT solve word equations.
strEqual :: StrExpr -> StrExpr -> Bool
strEqual a b = normalize a == normalize b

-- | Attempt to solve an equation 'a == b'. Three outcomes:
--
-- - 'Right Map.empty' (or with substitutions): equation is satisfiable.
--   For the literals-only solver, the only substitution case is when one
--   side is a single free variable and the other side is a ground literal.
-- - 'Left (StrContradiction ...)': literals on both sides differ.
-- - 'Left StrDeferred': equation involves variables in a way the solver
--   cannot decide. The caller should defer the constraint.
solveStr :: StrExpr -> StrExpr -> Either StrError (Map TVar StrExpr)
solveStr lhs rhs =
  let l = normalize lhs
      r = normalize rhs
  in case (l, r) of
        (StrLit s1, StrLit s2)
          | s1 == s2 -> Right Map.empty
          | otherwise -> Left (StrContradiction s1 s2)
        -- Solve a single variable to a ground value.
        (StrVar v, _) | isGround r -> Right (Map.singleton v r)
        (_, StrVar v) | isGround l -> Right (Map.singleton v l)
        -- Same expression on both sides (after normalization).
        _ | l == r -> Right Map.empty
        -- Anything else with a free variable: defer.
        _ -> Left StrDeferred
