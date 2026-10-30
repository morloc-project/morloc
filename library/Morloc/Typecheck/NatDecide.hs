{- |
Module      : Morloc.Typecheck.NatDecide
Description : The decision procedure for Nat conditions (DIM-7)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

'decide' answers whether a set of Nat conditions holds: for every natural
value of the rigid variables (POLY-7), some natural value of the other
variables satisfies every condition (DIM-6). A subtraction is natural
subtraction, defined only when it does not go below zero (KIND-4), and a
division is truncating, defined only for a nonzero divisor; each adds its
side condition.

Inferred variables are first eliminated where an equation defines one as a
polynomial over the rest. The conditions left are split into groups that
share no variable, and each group is decided on its own. A group is decided
exactly when it has no variables, when it is one condition over rigid
variables with neither subtraction nor division (an equation by polynomial
identity, a linear inequality by its coefficients), and when it is one
linear equation or inequality over inferred variables with neither
subtraction nor division. Otherwise a group holds when a short list of
candidate witnesses proves it, is refuted by a bounded search for a
counterexample, and is 'Unknown' when neither succeeds.
-}
module Morloc.Typecheck.NatDecide
  ( Condition (..)
  , Decision (..)
  , decide
  , evalNat
  , sideConditions
  ) where

import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Typecheck.NatSolver

data Condition
  = NatEq NatExpr NatExpr
  | NatGe NatExpr NatExpr
  deriving (Eq, Ord, Show)

data Decision
  = -- | The conditions hold. The map gives values found for inferred
    -- variables, as expressions over the rigid ones; it need not name every
    -- inferred variable.
    Holds (Map TVar NatExpr)
  | -- | A value of the rigid variables for which no value of the others
    -- satisfies the conditions; rigid variables it leaves out may take any
    -- value.
    Refuted (Map TVar Integer)
  | Unknown
  deriving (Eq, Show)

-- | Decide @conditions@ with @rigid@ universally quantified and every other
-- variable existentially quantified.
decide :: Set TVar -> [Condition] -> Decision
decide rigid conds0 =
  case eliminate rigid (withSides conds0) of
    Left counter -> Refuted counter
    Right (solved, conds) ->
      combine solved (map (decideGroup rigid) (groups (filter (not . proven) conds)))

combine :: Map TVar NatExpr -> [Decision] -> Decision
combine solved ds = case [env | Refuted env <- ds] of
  (env : _) -> Refuted env
  []
    | all isHolds ds ->
        let found = Map.unions [w | Holds w <- ds]
         in Holds (Map.union found (Map.map (substituteNat found) solved))
    | otherwise -> Unknown
  where
    isHolds (Holds _) = True
    isHolds _ = False

withSides :: [Condition] -> [Condition]
withSides cs = Set.toList (Set.fromList (cs <> concatMap sideConditions cs))

-- | The conditions under which every subtraction and division in a
-- condition is defined.
sideConditions :: Condition -> [Condition]
sideConditions (NatEq a b) = exprSides a <> exprSides b
sideConditions (NatGe a b) = exprSides a <> exprSides b

exprSides :: NatExpr -> [Condition]
exprSides e = case e of
  NatLit _ -> []
  NatVar _ -> []
  NatAdd a b -> exprSides a <> exprSides b
  NatMul a b -> exprSides a <> exprSides b
  NatSub a b -> NatGe a b : exprSides a <> exprSides b
  NatDiv a b -> NatGe b (NatLit 1) : exprSides a <> exprSides b

condVars :: [Condition] -> Set TVar
condVars = Set.unions . map vars
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

-- | Conditions grouped so that no two groups share a variable; each
-- condition without variables is a group of its own.
groups :: [Condition] -> [[Condition]]
groups [] = []
groups (c : cs) = let (g, rest) = grow [c] cs in g : groups rest
  where
    grow g pool =
      let vs = condVars g
          (joined, rest) = partition (\x -> not (Set.null vs) && not (Set.disjoint vs (condVars [x]))) pool
       in if null joined then (g, rest) else grow (g <> joined) rest

-- | Solve inferred variables from equations that define them, substituting
-- each into the rest. A definition @v := p - n@ adds @p >= n@, since @v@ is
-- a natural. A ground condition that is false refutes at once.
eliminate :: Set TVar -> [Condition] -> Either (Map TVar Integer) (Map TVar NatExpr, [Condition])
eliminate rigid = go Map.empty
  where
    go solved cs = case [(v, d, c) | c@(NatEq a b) <- cs, Just (v, d) <- [definition rigid a b]] of
      ((v, (pos, neg), c) : _) ->
        let e = case neg of
              NatLit 0 -> pos
              _ -> NatSub pos neg
            sub = substituteNat (Map.singleton v e)
            rest = [substCond sub c' | c' <- cs, c' /= c] <> [NatGe pos neg]
            solved' = Map.insert v e (Map.map sub solved)
         in go solved' (Set.toList (Set.fromList rest))
      []
        | any groundFalse cs -> Left Map.empty
        | otherwise -> Right (solved, cs)
    groundFalse c = Set.null (condVars [c]) && holdsAt Map.empty c /= Just True

substCond :: (NatExpr -> NatExpr) -> Condition -> Condition
substCond f (NatEq a b) = NatEq (f a) (f b)
substCond f (NatGe a b) = NatGe (f a) (f b)

-- | @a = b@ read as @v := p - n@ for an inferred @v@ that occurs once,
-- linearly, with a coefficient dividing every other coefficient; @p@ and
-- @n@ have no subtraction. The equation must be free of division, whose
-- normal form is not exact.
definition :: Set TVar -> NatExpr -> NatExpr -> Maybe (TVar, (NatExpr, NatExpr))
definition rigid a b
  | hasOpaqueDivision sop = Nothing
  | otherwise = listToMaybe candidates
  where
    sop@(NatSOP ps) = normalize (NatSub a b)
    candidates =
      [ (v, split [NatProduct (negate (npCoeff p) `div` c) (npVars p) | p <- others])
      | p0 <- ps
      , [(v, 1)] <- [Map.toList (npVars p0)]
      , not (Set.member v rigid)
      , let c = npCoeff p0
      , c /= 0
      , length [() | p <- ps, Map.member v (npVars p)] == 1
      , let others = [p | p <- ps, not (Map.member v (npVars p))]
      , all (\p -> npCoeff p `mod` c == 0) others
      ]
    split qs =
      ( sopToNatExpr (NatSOP [q | q <- qs, npCoeff q > 0])
      , sopToNatExpr (NatSOP [NatProduct (negate (npCoeff q)) (npVars q) | q <- qs, npCoeff q < 0])
      )

-- | A condition proven for every value of its variables by its normal form.
proven :: Condition -> Bool
proven c = case c of
  NatEq _ _ -> not (hasOpaqueDivision d) && null (unNatSOP d)
  NatGe _ _ -> not (hasOpaqueDivision d) && all ((>= 0) . npCoeff) (unNatSOP d)
  where
    d = condSOP c

condSOP :: Condition -> NatSOP
condSOP (NatEq a b) = normalize (NatSub a b)
condSOP (NatGe a b) = normalize (NatSub a b)

-- | No subtraction or division, so the normal form is the whole condition.
partialFree :: Condition -> Bool
partialFree c = case c of
  NatEq a b -> free a && free b
  NatGe a b -> free a && free b
  where
    free e = case e of
      NatLit _ -> True
      NatVar _ -> True
      NatAdd x y -> free x && free y
      NatMul x y -> free x && free y
      NatSub _ _ -> False
      NatDiv _ _ -> False

linearCond :: Condition -> Bool
linearCond c = all (\p -> sum (Map.elems (npVars p)) <= 1) (unNatSOP (condSOP c))

-- | Decide one group of unproven conditions that shares no variable with
-- any other group.
decideGroup :: Set TVar -> [Condition] -> Decision
decideGroup rigid conds
  | Set.null vs = if all ((== Just True) . holdsAt Map.empty) conds then Holds Map.empty else Refuted Map.empty
  | Set.null inferred = case mapMaybe counterexample conds of
      (env : _) -> Refuted env
      [] -> searched
  | null rs = case satisfiable searchBudget conds of
      Sat found -> Holds (Map.map NatLit found)
      Unsat -> Refuted Map.empty
      Undecided -> Unknown
  | Just w <- witness rs (Set.toList inferred) conds = Holds w
  | otherwise = case [env | c <- conds, Set.null (condVars [c] `Set.difference` rigid), Just env <- [counterexample c]] of
      (env : _) -> Refuted env
      [] -> searched
  where
    vs = condVars conds
    rs = Set.toList (Set.intersection rigid vs)
    inferred = vs `Set.difference` rigid
    searched = maybe Unknown Refuted (searchRigid rs conds)

-- | A counterexample to an unproven condition over rigid variables, by an
-- exact method: a linear inequality @c0 + sum ci xi >= 0@ fails at zero when
-- @c0 < 0@, and otherwise at @xi = c0 / |ci| + 1@ for a negative @ci@; a
-- nonzero polynomial is nonzero somewhere on the grid of @degree + 1@
-- values per variable, searched when that grid is small enough.
counterexample :: Condition -> Maybe (Map TVar Integer)
counterexample c
  | not (partialFree c) = Nothing
  | otherwise = case c of
      NatGe _ _
        | linearCond c, c0 < 0 -> Just zeros
        | linearCond c, ((v, ci) : _) <- negs -> Just (Map.insert v (c0 `div` negate ci + 1) zeros)
      NatEq _ _
        | product [degree v + 1 | v <- vs] <= searchBudget ->
            listToMaybe [env | env <- grid, holdsAt env c == Just False]
      _ -> Nothing
  where
    NatSOP ps = condSOP c
    c0 = sum [npCoeff p | p <- ps, Map.null (npVars p)]
    negs = [(v, npCoeff p) | p <- ps, [(v, 1)] <- [Map.toList (npVars p)], npCoeff p < 0]
    vs = Set.toList (condVars [c])
    zeros = Map.fromList [(v, 0) | v <- vs]
    degree v = maximum (1 : [Map.findWithDefault 0 v (npVars p) | p <- ps])
    grid = [Map.fromList (zip vs xs) | xs <- mapM (\v -> [0 .. degree v]) vs]

-- | Witnesses tried for the inferred variables of a group that also has
-- rigid ones: small constants, each rigid variable times one or two, and
-- their sum. A witness counts only if every condition is then 'proven'.
witness :: [TVar] -> [TVar] -> [Condition] -> Maybe (Map TVar NatExpr)
witness rs inferred conds
  | length inferred > 3 = Nothing
  | otherwise = listToMaybe [w | w <- choices, all (proven . substCond (substituteNat w)) conds]
  where
    sumRigid = foldr1 NatAdd (map NatVar rs)
    candidates =
      map NatLit [0, 1, 2]
        <> concat [[NatVar r, NatMul (NatLit 2) (NatVar r)] | r <- rs]
        <> [sumRigid | length rs > 1]
    choices = [Map.fromList (zip inferred es) | es <- mapM (const candidates) inferred]

data Satisfiable = Sat (Map TVar Integer) | Unsat | Undecided
  deriving (Eq)

-- | Assignments a search may try.
searchBudget :: Integer
searchBudget = 20000

-- | Whether some natural assignment satisfies every condition, with the
-- assignment when a search found one (empty when only shown to exist).
-- Exact for one linear equation or inequality without subtraction or
-- division, and when a condition without variables fails; otherwise a
-- bounded search.
satisfiable :: Integer -> [Condition] -> Satisfiable
satisfiable budget conds
  | any (\c -> Set.null (condVars [c]) && holdsAt Map.empty c /= Just True) conds = Unsat
  | (env : _) <- filter holdsAll candidates = Sat env
  | [c@(NatEq a b)] <- conds, partialFree c, linearCond c, Just answer <- naturalSolution [(a, b)] =
      if answer then Sat Map.empty else Unsat
  | [c@(NatGe _ _)] <- conds, partialFree c, linearCond c =
      let NatSOP ps = condSOP c
       in if any ((> 0) . npCoeff) [p | p <- ps, not (Map.null (npVars p))]
            || sum [npCoeff p | p <- ps, Map.null (npVars p)] >= 0
            then Sat Map.empty
            else Unsat
  | otherwise = Undecided
  where
    holdsAll env = all ((== Just True) . holdsAt env) conds
    candidates = assignments vs (searchWidth budget (length vs) conds)
    vs = Set.toList (condVars conds)

-- | A value of the rigid variables at which a group has no solution, from a
-- bounded search over a few rigid samples. A sample whose instance is
-- undecided is skipped.
searchRigid :: [TVar] -> [Condition] -> Maybe (Map TVar Integer)
searchRigid rs conds =
  listToMaybe
    [ env
    | env <- assignments rs (searchWidth 64 (length rs) conds)
    , satisfiable 2000 (map (substCond (substituteNat (Map.map NatLit env))) conds) == Unsat
    ]

-- | The widest box of values per variable, up to the largest coefficient,
-- within @budget@ points.
searchWidth :: Integer -> Int -> [Condition] -> Integer
searchWidth budget n conds =
  let bound = 1 + maximum (0 : concatMap (map (abs . npCoeff) . unNatSOP . condSOP) conds)
   in last (0 : takeWhile (\w -> (w + 1) ^ n <= budget) [1 .. max 4 bound])

assignments :: [TVar] -> Integer -> [Map TVar Integer]
assignments vs w = [Map.fromList (zip vs xs) | xs <- mapM (const [0 .. w]) vs]

-- | A condition at an assignment; Nothing when a variable is unassigned or
-- an operation is undefined.
holdsAt :: Map TVar Integer -> Condition -> Maybe Bool
holdsAt env c = case c of
  NatEq a b -> (==) <$> evalNat env a <*> evalNat env b
  NatGe a b -> (>=) <$> evalNat env a <*> evalNat env b

-- | Evaluate with natural subtraction and truncating division; Nothing where
-- either is undefined or a variable is unassigned.
evalNat :: Map TVar Integer -> NatExpr -> Maybe Integer
evalNat env e = case e of
  NatLit n -> Just n
  NatVar v -> Map.lookup v env
  NatAdd a b -> (+) <$> evalNat env a <*> evalNat env b
  NatMul a b -> (*) <$> evalNat env a <*> evalNat env b
  NatSub a b -> do
    x <- evalNat env a
    y <- evalNat env b
    if x >= y then Just (x - y) else Nothing
  NatDiv a b -> do
    x <- evalNat env a
    y <- evalNat env b
    if y /= 0 then Just (x `div` y) else Nothing
