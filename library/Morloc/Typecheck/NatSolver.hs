{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Typecheck.NatSolver
Description : Type-level natural number arithmetic solver
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Normalizes type-level Nat expressions to Sum-of-Products (SOP) canonical form
and solves equality constraints between Nat expressions. Based on the
approach in ghc-typelits-natnormalise by Christiaan Baaij.
-}
module Morloc.Typecheck.NatSolver
  ( NatExpr(..)
  , NatSOP(..)
  , NatProduct(..)
  , NatError(..)
  , normalize
  , natEqual
  , solveNat
  , substituteNat
  , isGround
  , freeNatVars
  , sopToNatExpr
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Morloc.Namespace.Prim (TVar(..))

-- | A type-level natural number expression
data NatExpr
  = NatLit Integer        -- ^ literal: 0, 1, 2, ...
  | NatVar TVar           -- ^ type variable of kind Nat
  | NatAdd NatExpr NatExpr -- ^ addition
  | NatMul NatExpr NatExpr -- ^ multiplication
  | NatSub NatExpr NatExpr -- ^ subtraction (a - b = a + negate b in SOP)
  | NatDiv NatExpr NatExpr -- ^ division (ground-only or constant-divisor)
  deriving (Eq, Ord, Show)

-- | Sum-of-Products canonical form for Nat expressions.
-- Represents: sum of (coefficient * product-of-variable-powers)
newtype NatSOP = NatSOP { unNatSOP :: [NatProduct] }
  deriving (Eq, Ord, Show)

-- | A single product term: coefficient * (v1^e1 * v2^e2 * ...)
-- Invariants: exponents > 0, zero-coefficient terms removed
data NatProduct = NatProduct
  { npCoeff :: !Integer
  , npVars  :: !(Map TVar Integer)
  } deriving (Show)

-- Custom Eq/Ord: full comparison including coefficient
instance Eq NatProduct where
  (NatProduct c1 v1) == (NatProduct c2 v2) = c1 == c2 && v1 == v2

instance Ord NatProduct where
  compare (NatProduct c1 v1) (NatProduct c2 v2) =
    compare (Map.size v1, v1, c1) (Map.size v2, v2, c2)

-- | Result of attempting to solve a Nat constraint
data NatError
  = Contradiction
  | Deferred NatSOP  -- ^ cannot solve yet, keep as deferred constraint
  deriving (Eq, Show)

-- | Normalize a NatExpr to canonical SOP form
normalize :: NatExpr -> NatSOP
normalize (NatLit n)   = NatSOP [NatProduct n Map.empty]
normalize (NatVar v)   = NatSOP [NatProduct 1 (Map.singleton v 1)]
normalize (NatAdd a b) = addSOP (normalize a) (normalize b)
normalize (NatMul a b) = mulSOP (normalize a) (normalize b)
normalize (NatSub a b) = addSOP (normalize a) (negateSOP (normalize b))
normalize (NatDiv a b) = divSOP (normalize a) (normalize b)

-- | Add two SOPs by merging and combining like terms
addSOP :: NatSOP -> NatSOP -> NatSOP
addSOP (NatSOP ps1) (NatSOP ps2) = NatSOP (mergeLikeTerms (ps1 ++ ps2))

-- | Multiply two SOPs by distributing (cross-product of terms)
mulSOP :: NatSOP -> NatSOP -> NatSOP
mulSOP (NatSOP ps1) (NatSOP ps2) =
  NatSOP (mergeLikeTerms [mulProduct p1 p2 | p1 <- ps1, p2 <- ps2])

-- | Multiply two product terms
mulProduct :: NatProduct -> NatProduct -> NatProduct
mulProduct (NatProduct c1 vs1) (NatProduct c2 vs2) =
  NatProduct (c1 * c2) (Map.unionWith (+) vs1 vs2)

-- | Merge like terms: group by variable-power maps, sum coefficients,
-- remove zero-coefficient products, sort canonically
mergeLikeTerms :: [NatProduct] -> [NatProduct]
mergeLikeTerms =
    filter (\p -> npCoeff p /= 0)
  . map mergeGroup
  . groupBy ((==) `on` npVars)
  . sortBy (comparing npVars)
  where
    mergeGroup :: [NatProduct] -> NatProduct
    mergeGroup [] = error "impossible: groupBy produces non-empty groups"
    mergeGroup ps@(p:_) = NatProduct (sum (map npCoeff ps)) (npVars p)

-- | Check if two Nat expressions are equal (via SOP normalization)
natEqual :: NatExpr -> NatExpr -> Bool
natEqual e1 e2 = normalize e1 == normalize e2

-- | Solve the constraint e1 ~ e2, returning variable substitutions
solveNat :: NatExpr -> NatExpr -> Either NatError (Map TVar NatExpr)
solveNat e1 e2 =
  let sop1 = normalize e1
      sop2 = normalize e2
      diff = subSOP sop1 sop2
  in solveSOP diff

-- | Subtract two SOPs: sop1 - sop2
subSOP :: NatSOP -> NatSOP -> NatSOP
subSOP (NatSOP ps1) (NatSOP ps2) =
  addSOP (NatSOP ps1) (NatSOP (map negateProduct ps2))

-- | Negate a product term
negateProduct :: NatProduct -> NatProduct
negateProduct (NatProduct c vs) = NatProduct (negate c) vs

-- | Negate an entire SOP
negateSOP :: NatSOP -> NatSOP
negateSOP (NatSOP ps) = NatSOP (map negateProduct ps)

-- | Divide two SOPs. Only handles ground division or constant divisor.
-- For ground: compute directly. For constant divisor: divide each coefficient.
-- Otherwise: return the original forms unchanged (will be Deferred by solver).
divSOP :: NatSOP -> NatSOP -> NatSOP
divSOP (NatSOP ps1) (NatSOP [NatProduct d vs2])
  | Map.null vs2, d /= 0
  , all (\p -> npCoeff p `mod` d == 0) ps1
  = NatSOP (mergeLikeTerms [NatProduct (npCoeff p `div` d) (npVars p) | p <- ps1])
divSOP (NatSOP ps1) (NatSOP ps2)
  -- Both ground: compute directly
  | all (\p -> Map.null (npVars p)) ps1
  , all (\p -> Map.null (npVars p)) ps2
  , let n = sum (map npCoeff ps1)
  , let d = sum (map npCoeff ps2)
  , d /= 0
  , n `mod` d == 0
  = NatSOP [NatProduct (n `div` d) Map.empty]
  -- Cannot simplify: return a sentinel that won't match anything useful.
  -- The solver will see non-matching SOPs and return Deferred.
  | otherwise = NatSOP [NatProduct 0 (Map.singleton (TV "__div__") 1)]

-- | Solve sop = 0
solveSOP :: NatSOP -> Either NatError (Map TVar NatExpr)
solveSOP (NatSOP []) = Right Map.empty  -- 0 = 0
solveSOP (NatSOP [NatProduct c vs])
  | Map.null vs && c /= 0 = Left Contradiction  -- c = 0 where c /= 0
  | Map.null vs           = Right Map.empty      -- 0 = 0
  | Map.size vs == 1, [(v, 1)] <- Map.toList vs =
      -- c*v = 0, only solution is v = 0 (but only if c divides 0, which it does)
      if c == 0
        then Right Map.empty
        else Right (Map.singleton v (NatLit 0))
  | otherwise = Left (Deferred (NatSOP [NatProduct c vs]))
solveSOP (NatSOP prods)
  | Just (v, a, b) <- extractLinearVar prods =
      if b `mod` a == 0
        then Right (Map.singleton v (NatLit (negate b `div` a)))
        else Left Contradiction
  | otherwise = Left (Deferred (NatSOP prods))

-- | Find a variable that appears linearly (exponent 1, alone in its product)
-- Returns (variable, coefficient, sum of constant terms)
extractLinearVar :: [NatProduct] -> Maybe (TVar, Integer, Integer)
extractLinearVar prods =
  let -- Find products with exactly one variable at exponent 1
      linearSingles = [ (v, npCoeff p)
                       | p <- prods
                       , Map.size (npVars p) == 1
                       , [(v, 1)] <- [Map.toList (npVars p)]
                       ]
      -- Check which linear variables appear only once AND all other
      -- products are constant (no other variables). Without this guard,
      -- expressions like i*j - n would incorrectly solve n = 0.
      candidates = [ (v, c, constSum)
                    | (v, c) <- linearSingles
                    , length [() | p <- prods, Map.member v (npVars p)] == 1
                    , let others = [p | p <- prods, not (Map.member v (npVars p))]
                    , all (\p -> Map.null (npVars p)) others
                    , let constSum = sum (map npCoeff others)
                    ]
  in case candidates of
       ((v, c, s) : _) -> Just (v, c, s)
       [] -> Nothing

-- | Apply substitutions to a NatExpr
substituteNat :: Map TVar NatExpr -> NatExpr -> NatExpr
substituteNat m = go
  where
    go (NatLit n) = NatLit n
    go (NatVar v) = case Map.lookup v m of
      Just e  -> e
      Nothing -> NatVar v
    go (NatAdd a b) = NatAdd (go a) (go b)
    go (NatMul a b) = NatMul (go a) (go b)
    go (NatSub a b) = NatSub (go a) (go b)
    go (NatDiv a b) = NatDiv (go a) (go b)

-- | Check if a NatExpr has no free variables
isGround :: NatExpr -> Bool
isGround (NatLit _) = True
isGround (NatVar _) = False
isGround (NatAdd a b) = isGround a && isGround b
isGround (NatMul a b) = isGround a && isGround b
isGround (NatSub a b) = isGround a && isGround b
isGround (NatDiv a b) = isGround a && isGround b

-- | Get all free variables in a NatExpr
freeNatVars :: NatExpr -> Set.Set TVar
freeNatVars (NatLit _) = Set.empty
freeNatVars (NatVar v) = Set.singleton v
freeNatVars (NatAdd a b) = Set.union (freeNatVars a) (freeNatVars b)
freeNatVars (NatMul a b) = Set.union (freeNatVars a) (freeNatVars b)
freeNatVars (NatSub a b) = Set.union (freeNatVars a) (freeNatVars b)
freeNatVars (NatDiv a b) = Set.union (freeNatVars a) (freeNatVars b)

-- | Convert a SOP back to a NatExpr (for error messages and further processing)
sopToNatExpr :: NatSOP -> NatExpr
sopToNatExpr (NatSOP []) = NatLit 0
sopToNatExpr (NatSOP prods) = foldl1 NatAdd (map productToExpr prods)
  where
    productToExpr :: NatProduct -> NatExpr
    productToExpr (NatProduct c vs)
      | Map.null vs = NatLit c
      | c == 1      = varsToExpr (Map.toList vs)
      | otherwise   = NatMul (NatLit c) (varsToExpr (Map.toList vs))

    varsToExpr :: [(TVar, Integer)] -> NatExpr
    varsToExpr [] = NatLit 1
    varsToExpr pairs = foldl1 NatMul (concatMap expandVar pairs)

    expandVar :: (TVar, Integer) -> [NatExpr]
    expandVar (v, n)
      | n <= 0    = []
      | otherwise = replicate (fromIntegral n) (NatVar v)
