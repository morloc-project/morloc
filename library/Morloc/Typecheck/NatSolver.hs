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
  , naturalSolution
  , hasOpaqueDivision
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Morloc.Namespace.Prim (TVar(..))
import qualified Data.Text as T
import Data.Maybe (isJust)

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
  = NatSOP (mergeLikeTerms [NatProduct (n `quot` d) Map.empty])
  -- Cannot simplify: an opaque variable named for the operands, so that
  -- equal quotients are equal and the solver never solves one.
  | otherwise = NatSOP [NatProduct 1 (Map.singleton (divVar ps1 ps2) 1)]

divVar :: [NatProduct] -> [NatProduct] -> TVar
divVar ps1 ps2 = TV (T.pack (divPrefix <> show (ps1, ps2)))

divPrefix :: String
divPrefix = "__div__"

isDivVar :: TVar -> Bool
isDivVar (TV v) = T.pack divPrefix `T.isPrefixOf` v

-- | Solve sop = 0
solveSOP :: NatSOP -> Either NatError (Map TVar NatExpr)
solveSOP sop@(NatSOP ps)
  | any (any isDivVar . Map.keys . npVars) ps = Left (Deferred sop)
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
      if b `mod` a == 0 && negate b `div` a >= 0
        then Right (Map.singleton v (NatLit (negate b `div` a)))
        else Left Contradiction
  | Just (v, e) <- extractLinearVarPoly prods =
      Right (Map.singleton v e)
  | otherwise = Left (Deferred (NatSOP prods))

-- | Solve @c*v + P(others) = 0@ as @v := -P/c@ when @c@ divides every
-- coefficient in @P@ and the result has no negative coefficient, so that
-- it is a natural number for every value of the other variables. Distinct
-- from 'extractLinearVar', which requires @P@ to be a constant.
extractLinearVarPoly :: [NatProduct] -> Maybe (TVar, NatExpr)
extractLinearVarPoly prods =
  case candidates of
    ((v, c, others) : _) ->
      Just (v, sopToNatExpr (NatSOP [NatProduct (negate (npCoeff p) `div` c) (npVars p) | p <- others]))
    [] -> Nothing
  where
    candidates =
      [ (v, c, others)
      | p0 <- prods
      , [(v, 1)] <- [Map.toList (npVars p0)]
      , let c = npCoeff p0
      , c /= 0
      , length [() | p <- prods, Map.member v (npVars p)] == 1
      , let others = [p | p <- prods, not (Map.member v (npVars p))]
      , all (\p -> npCoeff p `mod` c == 0 && negate (npCoeff p) `div` c >= 0) others
      ]

-- | Whether the equations @a = b@ have a common solution in the naturals:
-- @Just True@ when one is found, @Just False@ when there is none, @Nothing@
-- when neither is shown. A single linear equation is decided exactly; other
-- systems are searched for a small witness. A difference that goes below
-- zero is not a natural (KIND-4), so a witness never passes through one.
naturalSolution :: [(NatExpr, NatExpr)] -> Maybe Bool
naturalSolution eqs
  | any signed sops = Just False
  | [NatSOP ps] <- sops, Just answer <- linear ps = Just answer
  | any holds candidates = Just True
  | otherwise = Nothing
  where
    sops = [normalize (NatSub a b) | (a, b) <- eqs]
    vs = Set.toList (Set.unions [freeNatVars a <> freeNatVars b | (a, b) <- eqs])
    bound = 1 + maximum (0 : [abs (npCoeff p) | NatSOP ps <- sops, p <- ps])
    width = last (0 : takeWhile (\w -> (w + 1) ^ length vs <= (40000 :: Integer)) [1 .. bound])
    candidates = [Map.fromList (zip vs xs) | xs <- mapM (const [0 .. width]) vs]
    holds env = all (\(a, b) -> isJust (eval env a) && eval env a == eval env b) eqs
    eval env e = case e of
      NatLit n -> Just n
      NatVar v -> Map.lookup v env
      NatAdd a b -> (+) <$> eval env a <*> eval env b
      NatMul a b -> (*) <$> eval env a <*> eval env b
      NatSub a b -> case (-) <$> eval env a <*> eval env b of
        Just x | x >= 0 -> Just x
        _ -> Nothing
      NatDiv a b -> case (eval env a, eval env b) of
        (Just x, Just y) | y /= 0 -> Just (x `quot` y)
        _ -> Nothing
    -- every product of naturals is non-negative, so a sum of terms whose
    -- coefficients share one sign and include a nonzero constant is never 0;
    -- a quotient of an expression with subtraction may be negative, so an
    -- equation holding one says nothing
    signed (NatSOP ps)
      | hasOpaqueDivision (NatSOP ps) = False
      | otherwise =
          let constant = sum [npCoeff p | p <- ps, Map.null (npVars p)]
              varCoeffs = [npCoeff p | p <- ps, not (Map.null (npVars p))]
          in (constant > 0 && all (> 0) varCoeffs) || (constant < 0 && all (< 0) varCoeffs)
    -- the equation came from expressions without subtraction, so its
    -- normal form is the whole constraint
    noSubtraction = not (any (\(a, b) -> hasSub a || hasSub b) eqs)
    hasSub e = case e of
      NatSub _ _ -> True
      NatAdd a b -> hasSub a || hasSub b
      NatMul a b -> hasSub a || hasSub b
      NatDiv a b -> hasSub a || hasSub b
      _ -> False
    linear ps
      | noSubtraction
      , not (hasOpaqueDivision (NatSOP ps))
      , all (\p -> Map.size (npVars p) <= 1 && all (== 1) (Map.elems (npVars p))) ps =
          let k = negate (sum [npCoeff p | p <- ps, Map.null (npVars p)])
              cs = [npCoeff p | p <- ps, not (Map.null (npVars p))]
          in linearNatural cs k
      | otherwise = Nothing

-- | Whether @sum (zipWith (*) cs xs) == k@ has a solution with every
-- @xs@ a natural. With coefficients of both signs it has one exactly when
-- their gcd divides @k@: the vector @|c_q| e_p + c_p e_q@ for @c_p > 0 > c_q@
-- leaves the sum unchanged, so any integer solution can be shifted until it
-- is natural. With one sign it is the coin problem: past the Frobenius bound
-- every multiple of the gcd is reachable, and below it the least reachable
-- sum in each residue class decides; @Nothing@ when that table is too large.
linearNatural :: [Integer] -> Integer -> Maybe Bool
linearNatural cs0 k0
  | null cs0 = Just (k0 == 0)
  | k0 `mod` g /= 0 = Just False
  | any (> 0) cs0 && any (< 0) cs0 = Just True
  | k < 0 = Just False
  | k >= (a - 1) * (maximum cs - 1) = Just True
  | a > 100000 = Nothing
  | otherwise = Just (maybe False (<= k) (Map.lookup (k `mod` a) smallest))
  where
    g = foldr1 gcd cs0
    cs = map (abs . (`div` g)) cs0
    k = (if all (> 0) cs0 then k0 else negate k0) `div` g
    a = minimum cs
    -- the least reachable sum in each residue class modulo the smallest
    -- coefficient; a sum is reachable when it is at least that least one
    smallest = dijkstra (Set.singleton (0, 0)) Map.empty
    dijkstra frontier done = case Set.minView frontier of
      Nothing -> done
      Just ((d, r), rest)
        | Map.member r done -> dijkstra rest done
        | otherwise ->
            let next = Set.fromList [(d + c, (r + c) `mod` a) | c <- cs]
            in dijkstra (Set.union rest next) (Map.insert r d done)

-- | Whether a normal form holds a quotient that could not be reduced.
hasOpaqueDivision :: NatSOP -> Bool
hasOpaqueDivision (NatSOP ps) = any (any isDivVar . Map.keys . npVars) ps

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
