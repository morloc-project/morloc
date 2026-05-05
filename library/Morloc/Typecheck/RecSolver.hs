{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Typecheck.RecSolver
Description : Type-level row-polymorphic record solver
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Solves equality and constraint problems on Rec-kinded type expressions
(column maps for Tables in the Stage 3 tables refactor). The canonical
form is a pair of:

- a Map from field name to field type (the ground fields)
- an optional row-tail variable (the polymorphic tail)

Decidability follows the rules in plans/tables/10-rec-solver-decidability.md:

- Equations between two ground-tail Recs solve by alignment.
- Equations with one ground tail and one row-var tail solve the row var.
- Equations with two row-var tails defer.
- Union (@+@) requires disjoint keys; conflicts raise an error. Disjointness
  involving a row-var tail defers as a Lacks constraint.
- Difference (@-@) is a no-op when the named key is not in the Rec; this
  enables the @((r - f) + f=a)@ construction idiom (memo 12).
- Intersection (@&@) defers when either side has a row-var tail.

Type-level field types are 'TypeU' values. Per-field type unification is the
responsibility of the calling typechecker — this solver only checks structural
equality of the field set.
-}
module Morloc.Typecheck.RecSolver
  ( RecExpr (..)
  , RecCanon (..)
  , RecError (..)
  , normalize
  , recEqual
  , solveRec
  , isGround
  , freeRecVars
  , canonToTypeU
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Namespace.Type (TypeU (..))

-- | The structural form a RecExpr can take. Mirrors the TypeU Rec
-- constructors; the bridge to TypeU happens in Typecheck.Internal.
data RecExpr
  = RecVar TVar
  | RecEmpty
  | RecExtend Text TypeU RecExpr
  | RecUnion RecExpr RecExpr
  | RecDiff RecExpr [Text]
  | RecIntersect RecExpr RecExpr
  deriving (Eq, Ord, Show)

-- | Canonical form: a sorted Map of field-type pairs plus an optional
-- row-tail variable. Two RecExprs that normalize to the same RecCanon
-- represent the same row-polymorphic Rec.
data RecCanon = RecCanon
  { recFields :: !(Map Text TypeU)
  , recTail   :: !(Maybe TVar)
  } deriving (Eq, Ord, Show)

-- | Errors from rec-solver attempts.
data RecError
  = -- | Two grounds disagree on a key's type, OR a Lacks constraint is
    -- violated, OR a key set conflict in a strict union. Carries a short
    -- diagnostic message.
    RecContradiction Text
    -- | The equation cannot be decided without more information; caller
    -- should defer the constraint.
  | RecDeferred
    -- | Difference / Intersect / Union encountered a non-canonicalizable
    -- subterm (e.g. a non-Rec TypeU).
  | RecMalformed Text
  deriving (Eq, Show)

-- | Reduce a RecExpr to its canonical (RecCanon, ground?) form. Returns
-- Left on type-level conflicts (e.g. union of overlapping ground keys).
-- Returns Right RecCanon on successful normalization.
normalize :: RecExpr -> Either RecError RecCanon
normalize RecEmpty = Right (RecCanon Map.empty Nothing)
normalize (RecVar v) = Right (RecCanon Map.empty (Just v))
normalize (RecExtend k t rest) = do
  RecCanon fs tl <- normalize rest
  case Map.lookup k fs of
    Just _ -> Left (RecContradiction $ "Duplicate field in Rec extension: " <> k)
    Nothing -> Right (RecCanon (Map.insert k t fs) tl)
normalize (RecUnion a b) = do
  RecCanon fa tla <- normalize a
  RecCanon fb tlb <- normalize b
  let overlap = Set.intersection (Map.keysSet fa) (Map.keysSet fb)
  case (Set.null overlap, tla, tlb) of
    (True, Nothing, Nothing) -> Right (RecCanon (Map.union fa fb) Nothing)
    (True, Just t, Nothing) -> Right (RecCanon (Map.union fa fb) (Just t))
    (True, Nothing, Just t) -> Right (RecCanon (Map.union fa fb) (Just t))
    (True, Just _, Just _) -> Left RecDeferred
    (False, _, _) ->
      Left (RecContradiction $ "Rec union has overlapping keys: " <>
            commaSep (Set.toList overlap))
normalize (RecDiff a ks) = do
  RecCanon fa tla <- normalize a
  -- Per memo 10: removing an absent key is a no-op.
  let fa' = foldr Map.delete fa ks
  case tla of
    Nothing -> Right (RecCanon fa' Nothing)
    -- A row-tail variable could in principle contain one of the dropped
    -- keys; we cannot resolve until the tail is solved. For Stage 3 we
    -- leave the diff symbolic by carrying the tail; downstream Lacks
    -- constraints record the dropped key set against the tail var.
    Just _ -> Right (RecCanon fa' tla)
normalize (RecIntersect a b) = do
  RecCanon fa tla <- normalize a
  RecCanon fb tlb <- normalize b
  case (tla, tlb) of
    (Nothing, Nothing) -> do
      -- Both ground: take fields present in both with matching types.
      let common = Map.intersectionWith (,) fa fb
          shared = Map.mapMaybe (\(t1, t2) -> if t1 == t2 then Just t1 else Nothing) common
          mismatched = Map.filterWithKey (\k _ -> Map.member k common && not (Map.member k shared)) fa
      if Map.null mismatched
        then Right (RecCanon shared Nothing)
        else Left (RecContradiction $ "Rec intersection has type mismatch on: " <>
                   commaSep (Map.keys mismatched))
    _ -> Left RecDeferred

-- | True iff the canonical Rec has no row-tail variable.
isGround :: RecCanon -> Bool
isGround (RecCanon _ Nothing) = True
isGround _ = False

-- | The free row-variables in a RecExpr.
freeRecVars :: RecExpr -> Set.Set TVar
freeRecVars (RecVar v) = Set.singleton v
freeRecVars RecEmpty = Set.empty
freeRecVars (RecExtend _ _ rest) = freeRecVars rest
freeRecVars (RecUnion a b) = Set.union (freeRecVars a) (freeRecVars b)
freeRecVars (RecDiff a _) = freeRecVars a
freeRecVars (RecIntersect a b) = Set.union (freeRecVars a) (freeRecVars b)

-- | Equality on Rec expressions, after normalization.
recEqual :: RecExpr -> RecExpr -> Bool
recEqual a b = case (normalize a, normalize b) of
  (Right ca, Right cb) -> ca == cb
  _ -> False

-- | Attempt to solve an equation @a == b@. Outcomes:
--
-- - 'Right Map.empty': equation already satisfied (both sides equal after
--   normalization).
-- - 'Right subs': a row-tail variable was solved; subs maps it to a
--   RecExpr.
-- - 'Left RecContradiction msg': structural conflict (overlapping keys
--   in strict union, type mismatch on shared key in intersection, etc.).
-- - 'Left RecDeferred': cannot decide without more info; caller defers.
solveRec :: RecExpr -> RecExpr -> Either RecError (Map TVar RecExpr)
solveRec lhs rhs = do
  l <- normalize lhs
  r <- normalize rhs
  solveCanon l r

solveCanon :: RecCanon -> RecCanon -> Either RecError (Map TVar RecExpr)
solveCanon (RecCanon fa Nothing) (RecCanon fb Nothing)
  -- Both ground tails: must have identical field maps.
  | fa == fb = Right Map.empty
  | Map.keysSet fa /= Map.keysSet fb =
      Left (RecContradiction $ "Rec key sets differ: " <>
            keyDelta (Map.keysSet fa) (Map.keysSet fb))
  | otherwise =
      -- Same keys but at least one type differs. Per-field type
      -- equality is checked here; cross-type unification (e.g. existential
      -- propagation) belongs to the calling typechecker.
      Left (RecContradiction $ "Rec field types differ for key(s): " <>
            commaSep [k | (k, ta) <- Map.toList fa
                        , Just tb <- [Map.lookup k fb]
                        , ta /= tb])
solveCanon (RecCanon fa (Just v)) (RecCanon fb Nothing)
  -- LHS row var, RHS ground. The LHS fields must be a subset of RHS, and
  -- v is solved to (RHS minus LHS fields).
  | Map.isSubmapOfBy (==) fa fb =
      let resid = Map.difference fb fa
       in Right (Map.singleton v (canonToRecExpr (RecCanon resid Nothing)))
  | otherwise =
      Left (RecContradiction $ "Rec LHS extension exceeds RHS or has type mismatch: " <>
            keyDelta (Map.keysSet fa) (Map.keysSet fb))
solveCanon ca@(RecCanon _ Nothing) cb@(RecCanon _ (Just _)) =
  -- Symmetric case
  case solveCanon cb ca of
    Right subs -> Right subs
    Left e -> Left e
-- Both sides are pure row variables (no fixed fields). Unify them by
-- binding the RHS variable to the LHS variable (or no-op if same).
-- Without this, two distinct fresh row variables introduced at
-- different call sites would never get linked even when subtype tries
-- to equate them, breaking constraint propagation: an obligation
-- carrying r2 stays separate from an assumption carrying r1 even
-- after the function call links them.
solveCanon (RecCanon fa (Just va)) (RecCanon fb (Just vb))
  | Map.null fa && Map.null fb =
      if va == vb
      then Right Map.empty
      else Right (Map.singleton vb (RecVar va))
  | fa == fb =
      if va == vb
      then Right Map.empty
      else Right (Map.singleton vb (RecVar va))
  | otherwise = Left RecDeferred

-- | Lift a canonical Rec back to a RecExpr (for substitution into TypeU).
canonToRecExpr :: RecCanon -> RecExpr
canonToRecExpr (RecCanon fs tail_) =
  let tailExpr = maybe RecEmpty RecVar tail_
   in Map.foldrWithKey RecExtend tailExpr fs

-- | Lift a canonical Rec back to a TypeU. Iterated extension on the
-- optional tail variable (or RecEmptyU for ground records).
canonToTypeU :: RecCanon -> TypeU
canonToTypeU (RecCanon fs tail_) =
  let tailType = maybe RecEmptyU RecVarU tail_
   in Map.foldrWithKey RecExtendU tailType fs

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

commaSep :: [Text] -> Text
commaSep = Map.foldrWithKey (\_ b acc -> if acc == "" then b else b <> ", " <> acc) ""
         . Map.fromList . zip [(0 :: Int) ..]

keyDelta :: Set.Set Text -> Set.Set Text -> Text
keyDelta a b =
  let missing = Set.toList (Set.difference a b)
      extra = Set.toList (Set.difference b a)
   in (if null missing then "" else "missing in RHS=" <> commaSep missing)
      <> (if null missing || null extra then "" else "; ")
      <> (if null extra then "" else "extra in RHS=" <> commaSep extra)
