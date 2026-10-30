{- |
Module      : Morloc.Typecheck.NatDecide
Description : The decision procedure for Nat conditions (DIM-7)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

'decide' answers whether a set of Nat conditions holds: for every value of
the rigid variables (POLY-7), some value of the other variables satisfies
every condition (DIM-6). A Nat variable takes natural values and an Int
variable integer ones; each Int variable is decided as the difference of two
natural ones. 'decideAssuming' quantifies the
rigid variables only over the values that satisfy its assumptions. The
arithmetic is integer arithmetic (KIND-3): a difference may be negative, and
a division truncates toward zero and is defined only for a positive divisor,
which adds that side condition.

Inferred variables are first eliminated where an equation defines one as a
polynomial over the rest. The conditions left are split into groups that
share no variable, and each group is decided on its own. A group is decided
exactly when it has no variables, when it is one condition over rigid
variables without division (an equation by polynomial identity, a linear
inequality by its coefficients), and when it is one linear equation or
inequality over inferred variables without division. Otherwise a group holds when a short list of
candidate witnesses proves it, is refuted by a bounded search for a
counterexample, and is 'Unknown' when neither succeeds.
-}
module Morloc.Typecheck.NatDecide
  ( Condition (..)
  , Decision (..)
  , decide
  , decideAssuming
  , substCond
  , reaching
  , condVars
  , holdsAt
  , evalNat
  , sideConditions
  ) where

import Control.Applicative ((<|>))
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
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
decide rigid = decideAssuming rigid []

-- | Decide @conditions@ for the values of @rigid@ that satisfy
-- @assumptions@, which are over rigid variables only. A refutation then
-- gives a value of every rigid variable of its group, and that value
-- satisfies the assumptions.
decideAssuming :: Set TVar -> [Condition] -> [Condition] -> Decision
decideAssuming rigid asm conds0
  | Set.null ints = case decideNatural rigid asm conds of
      Holds w -> Holds (withDefs w)
      d -> d
  | otherwise = case decideNatural rigid' (map split asm) (map split conds) of
      Holds w
        -- A witness is kept only when it names both halves of every Int it
        -- names one of, so that the values it gives stay consistent.
        | all (\i -> all (`Map.member` w) (pair i) || not (any (`Map.member` w) (pair i))) (Set.toList ints)
        , let joined = Map.union (Map.mapMaybe id (Map.fromList [(i, joinW w i) | i <- Set.toList ints])) (Map.withoutKeys w halvesSet)
        , all (Set.disjoint halvesSet . freeNatVars) (Map.elems joined) ->
            Holds (withDefs joined)
        | otherwise -> Holds Map.empty
      Refuted env -> Refuted (Map.union (Map.fromList [(i, joinE env i) | i <- Set.toList ints, any (`Map.member` env) (pair i)]) (Map.withoutKeys env halvesSet))
      Unknown -> Unknown
  where
    (defs, conds1) = defineFirst conds0
    -- A definition may name variables the witness gives values to.
    withDefs w = Map.union w (Map.map (substituteNat w) defs)
    -- A condition without division that holds for every integer value of
    -- its Int variables (an even power, as in @h * h >= 0@) is settled
    -- before the split hides the sign of each.
    conds = filter (not . settled) conds1
    settled c@(NatGe a b) = null (sideConditions c) && nonNegativeEverywhere (NatSub a b)
    settled _ = False
    ints = Set.unions [intVars a <> intVars b | c <- asm <> conds, let (a, b) = sides c]
    halves = intHalves
    pair i = let (p, n) = halves i in [p, n]
    halvesSet = Set.fromList (concatMap pair (Set.toList ints))
    rigid' = Set.union (Set.difference rigid ints) (Set.fromList (concatMap pair (Set.toList (Set.intersection rigid ints))))
    split = substCond splitIntVars
    joinE env i = let (p, n) = halves i in Map.findWithDefault 0 p env - Map.findWithDefault 0 n env
    joinW w i = let (p, n) = halves i in NatSub <$> Map.lookup p w <*> Map.lookup n w

    -- An inferred Nat written equal to an expression (@v = e@) is replaced
    -- by it, leaving @e >= 0@, before the split, so that this condition is
    -- still read with its Int variables whole.
    defineFirst cs = case [(v, e, c) | c@(NatEq x y) <- cs, (NatVar v, e) <- [(x, y), (y, x)], not (Set.member v rigid), not (Set.member v (freeNatVars e))] of
      ((v, e, c) : _) ->
        let sub = substituteNat (Map.singleton v e)
            (ds, rest) = defineFirst (NatGe e (NatLit 0) : [substCond sub c' | c' <- cs, c' /= c])
         in (Map.insert v (substituteNat ds e) ds, rest)
      [] -> (Map.empty, cs)

sides :: Condition -> (NatExpr, NatExpr)
sides (NatEq a b) = (a, b)
sides (NatGe a b) = (a, b)

-- | 'decideAssuming' over Nat variables alone.
decideNatural :: Set TVar -> [Condition] -> [Condition] -> Decision
decideNatural rigid asm0 conds0
  | any (\a -> Set.null (condVars [a]) && holdsAt Map.empty a /= Just True) asm = Holds Map.empty
  | otherwise = case eliminate rigid (withSides conds0) of
    Left counter
      | null asm -> Refuted counter
      | otherwise -> maybe Unknown Refuted (inDomain asm counter)
    Right (solved, conds) ->
      combine solved [decideGroup rigid a cs | (a, cs) <- groupsWith asm (filter (not . discharged asm) conds)]
  where
    asm = withSides asm0

-- | An assignment of the assumptions' variables, extending @env@, at which
-- every assumption holds, from a bounded search.
inDomain :: [Condition] -> Map TVar Integer -> Maybe (Map TVar Integer)
inDomain asm env =
  listToMaybe
    [ Map.union env found
    | found <- assignments free (searchWidth 4096 (length free) asm)
    , all ((== Just True) . holdsAt (Map.union env found)) asm
    ]
  where
    free = Set.toList (condVars asm `Set.difference` Map.keysSet env)

-- | The conditions connected to @vs@ through shared variables.
reaching :: Set TVar -> [Condition] -> [Condition]
reaching vs cs =
  let (near, far) = partition (\c -> not (Set.disjoint vs (condVars [c]))) cs
   in if null near then [] else near <> reaching (Set.union vs (condVars near)) far

-- | A condition that follows from the assumptions: its normal form is
-- 'proven' or is an assumption's; it is a quotient's non-negativity that
-- follows from its dividend's (a quotient truncates toward zero, and its
-- divisor is positive by its side condition); or it is an inequality whose
-- normal form exceeds a small multiple of one assumption, or the sum of
-- two, by a polynomial with non-negative coefficients.
discharged :: [Condition] -> Condition -> Bool
discharged asm0 c0 = proven c || any (sameAs c) asm || ineq
  where
    c = abstractQuotients asm0 c0
    asm = map (abstractQuotients asm0) asm0
    sameAs (NatGe _ _) a@(NatGe _ _) = condSOP a == d
    sameAs (NatEq _ _) a@(NatEq _ _) = condSOP a == d || condSOP a == negateSOP' d
    sameAs _ _ = False
    negateSOP' (NatSOP ps) = NatSOP [p {npCoeff = negate (npCoeff p)} | p <- ps]
    ineq = case c of
      NatGe _ _ ->
        not (hasOpaqueDivision d)
          && any (\a -> nonNegative (subtractSOP d a)) combos
      NatEq _ _ -> False
    d = condSOP c
    lower = [condSOP a | a@(NatGe _ _) <- asm]
    combos =
      [scaleSOP k a | a <- lower, k <- [1, 2, 3]]
        <> [addSOPs a b | (i, a) <- zip [0 :: Int ..] lower, (j, b) <- zip [0 ..] lower, i < j]
    nonNegative x = not (hasOpaqueDivision x) && all ((>= 0) . npCoeff) (unNatSOP x)
    scaleSOP k (NatSOP ps) = NatSOP [p {npCoeff = k * npCoeff p} | p <- ps]
    addSOPs (NatSOP a) (NatSOP b) = normalize (NatAdd (sopToNatExpr (NatSOP a)) (sopToNatExpr (NatSOP b)))
    subtractSOP x y = normalize (NatSub (sopToNatExpr x) (sopToNatExpr y))

-- | Each quotient whose dividend is non-negative under the assumptions
-- becomes a variable named for it: it is then a natural, since the quotient
-- truncates toward zero and its divisor is positive by its side condition.
abstractQuotients :: [Condition] -> Condition -> Condition
abstractQuotients asm c = case c of
  NatEq a b -> NatEq (go a) (go b)
  NatGe a b -> NatGe (go a) (go b)
  where
    go e = case e of
      NatAdd x y -> NatAdd (go x) (go y)
      NatSub x y -> NatSub (go x) (go y)
      NatMul x y -> NatMul (go x) (go y)
      NatDiv x y
        | discharged asm (NatGe x (NatLit 0)) -> NatVar (TV (T.pack ("(" <> show (go x, go y) <> ")")))
        | otherwise -> NatDiv (go x) (go y)
      _ -> e

-- | Conditions grouped so that no two groups share a variable, with the
-- assumptions joining the groups whose variables they connect; each group
-- carries the assumptions over its variables. A condition without variables
-- is a group of its own.
groupsWith :: [Condition] -> [Condition] -> [([Condition], [Condition])]
groupsWith asm conds =
  [ ([a | Right a <- g], cs)
  | g <- tagged
  , let cs = [c | Left c <- g]
  , not (null cs)
  ]
  where
    tagged = groupBy' (map Left conds <> map Right asm)
    groupBy' [] = []
    groupBy' (x : xs) = let (g, rest) = grow [x] xs in g : groupBy' rest
    grow g pool =
      let vs = condVars (map untag g)
          (joined, rest) = partition (\x -> not (Set.null vs) && not (Set.disjoint vs (condVars [untag x]))) pool
       in if null joined then (g, rest) else grow (g <> joined) rest
    untag = either id id

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

-- | The conditions under which every division in a condition is defined.
sideConditions :: Condition -> [Condition]
sideConditions (NatEq a b) = exprSides a <> exprSides b
sideConditions (NatGe a b) = exprSides a <> exprSides b

exprSides :: NatExpr -> [Condition]
exprSides e = case e of
  NatLit _ -> []
  NatVar _ -> []
  IntVar _ -> []
  NatAdd a b -> exprSides a <> exprSides b
  NatMul a b -> exprSides a <> exprSides b
  NatSub a b -> exprSides a <> exprSides b
  NatDiv a b -> NatGe b (NatLit 1) : exprSides a <> exprSides b

condVars :: [Condition] -> Set TVar
condVars = Set.unions . map vars
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

-- | Solve inferred variables from equations that define them, substituting
-- each into the rest. A definition @v := p - n@ adds @p >= n@, since @v@ is
-- a natural; one written @v = e@ adds @e >= 0@. A ground condition that is
-- false refutes at once.
eliminate :: Set TVar -> [Condition] -> Either (Map TVar Integer) (Map TVar NatExpr, [Condition])
eliminate rigid = go Map.empty
  where
    go solved cs = case [(v, d, c) | c@(NatEq a b) <- cs, Just (v, d) <- [literal a b <|> literal b a <|> definition rigid a b]] of
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
    -- @v = e@ as written defines an inferred @v@ not in @e@ exactly, even
    -- where @e@ divides.
    literal (NatVar v) e
      | not (Set.member v rigid), not (Set.member v (freeNatVars e)) = Just (v, (e, NatLit 0))
    literal _ _ = Nothing

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

-- | No division, so the normal form is the whole condition.
partialFree :: Condition -> Bool
partialFree c = case c of
  NatEq a b -> free a && free b
  NatGe a b -> free a && free b
  where
    free e = case e of
      NatLit _ -> True
      NatVar _ -> True
      IntVar _ -> True
      NatAdd x y -> free x && free y
      NatMul x y -> free x && free y
      NatSub x y -> free x && free y
      NatDiv _ _ -> False

linearCond :: Condition -> Bool
linearCond c = all (\p -> sum (Map.elems (npVars p)) <= 1) (unNatSOP (condSOP c))

-- | Decide one group of unproven conditions, with the assumptions over its
-- variables, that shares no variable with any other group.
decideGroup :: Set TVar -> [Condition] -> [Condition] -> Decision
decideGroup rigid asm conds
  | not (null asm) = assuming
  | Set.null vs = if all ((== Just True) . holdsAt Map.empty) conds then Holds Map.empty else Refuted Map.empty
  | Set.null inferred = case mapMaybe counterexample conds of
      (env : _) -> Refuted env
      [] -> searched
  | null rs = case satisfiable searchBudget conds of
      Sat found -> Holds (Map.map NatLit found)
      Unsat -> Refuted Map.empty
      Undecided -> Unknown
  | Just w <- witness (const False) rs (Set.toList inferred) conds = Holds w
  | otherwise = case [env | c <- conds, Set.null (condVars [c] `Set.difference` rigid), Just env <- [counterexample c]] of
      (env : _) -> Refuted env
      [] -> searched
  where
    vs = condVars conds
    rs = Set.toList (Set.intersection rigid vs)
    inferred = vs `Set.difference` rigid
    searched = maybe Unknown Refuted (searchRigid (const True) rs conds)
    -- Every refutation must lie inside the assumptions, so an exact
    -- counterexample counts only when it does.
    allRs = Set.toList (Set.intersection rigid (vs <> condVars asm))
    inAsm env = all ((== Just True) . holdsAt env) asm
    exact =
      [ env'
      | c <- conds
      , Set.null (condVars [c] `Set.difference` rigid)
      , Just env <- [counterexample c]
      , Just env' <- [inDomain asm env]
      ]
    assuming
      | not (Set.null inferred), Just w <- witness (discharged asm) rs (Set.toList inferred) conds = Holds w
      | (env : _) <- exact = Refuted env
      | otherwise = maybe Unknown Refuted (searchRigid inAsm allRs (conds <> asm))

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
-- their sum. A witness counts only if every condition is then 'proven' or
-- follows by @follows@.
witness :: (Condition -> Bool) -> [TVar] -> [TVar] -> [Condition] -> Maybe (Map TVar NatExpr)
witness follows rs inferred conds
  | length inferred > 3 = Nothing
  | null rs = Nothing
  | otherwise = listToMaybe [w | w <- choices, all ((\c -> proven c || follows c) . substCond (substituteNat w)) conds]
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
-- Exact for one linear equation or inequality without division,
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

-- | A value of the rigid variables, accepted by @admit@, at which a group
-- has no solution, from a bounded search over a few rigid samples. A sample
-- whose instance is undecided is skipped.
searchRigid :: (Map TVar Integer -> Bool) -> [TVar] -> [Condition] -> Maybe (Map TVar Integer)
searchRigid admit rs conds =
  listToMaybe
    [ env
    | env <- assignments rs (searchWidth 64 (length rs) conds)
    , admit env
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
