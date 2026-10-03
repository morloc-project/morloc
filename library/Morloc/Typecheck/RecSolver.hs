{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Typecheck.RecSolver
Description : Type-level row-polymorphic record solver
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Solves equality and constraint problems on Rec-kinded type expressions
(column schemas for Tables, and record field sets).

A Rec is an /ordered row/: a sequence of (field, type) pairs with pairwise
distinct labels, interleaved with row variables standing for unknown blocks.
Order is part of type identity, so @{a = Int, b = Str}@ and
@{b = Str, a = Int}@ are distinct rows. They remain isomorphic -- a
permutation converts either to the other -- but the conversion is explicit,
never inserted by the solver.

The deliberate omission is the scoped-labels swap rule
(@{l1 | {l2 | r}} == {l2 | {l1 | r}}@ when @l1 /= l2@). Admitting it would
quotient rows by permutation and collapse this into an unordered theory;
omitting it is what makes column order observable and lets a signature
describe where a column moves.

The canonical form is a /sequence of segments/, not a block of fields with a
trailing variable. A row variable can sit anywhere, because the rows that
actually occur put it anywhere:

@
  {a = Int, b = Str}              [Ground [a, b]]
  {a = Int | r}                   [Ground [a], Tail r]
  r + {z = Real}                  [Tail r, Ground [z]]
  l + {f = a} + r                 [Tail l, Ground [f], Tail r]
@

The third and fourth shapes are the ones the stdlib uses -- @setCol@ and
@renameCol@ replace a column in place -- and a (fields, tail) pair cannot
express them. Representing them as a prefix would move the ground block
across the variable, which breaks the property an ordered theory rests on:
normalizing and substituting must commute.

Decidability. Matching walks the two segment sequences left to right:

- A ground segment must align with the same fields, in the same order.
- A row variable followed by a known label is pinned: the label occurs at
  most once, so the split point is unique and the variable takes everything
  before it.
- A row variable at the end takes the remainder.
- Two adjacent row variables are ambiguous, and defer.

Type-level field types are 'TypeU' values. Per-field type unification is the
responsibility of the calling typechecker -- this solver only checks
structural equality of the field sequence.
-}
module Morloc.Typecheck.RecSolver
  ( RecExpr (..)
  , RecSeg (..)
  , RecCanon (..)
  , RecSolution (..)
  , RecError (..)
  , normalize
  , solveRec
  , isGround
  , groundFields
  , freeRecVars
  , canonToRecExpr
  , mkCanon
  ) where

import Control.Monad (foldM)
import Data.List (foldl')
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

-- | One run of a canonical row: a block of known fields in written order, or
-- a row variable standing for an unknown block.
data RecSeg
  = RecGround ![(Text, TypeU)]
  | RecTail !TVar
  deriving (Eq, Ord, Show)

-- | Canonical form: the segments of the row, left to right. Two RecExprs
-- represent the same row exactly when they normalize to the same RecCanon,
-- which includes agreeing on field order.
--
-- Invariant, established by 'mkCanon': no empty ground segment, and no two
-- adjacent ground segments. Without it a row would have several canonical
-- forms and 'Eq' would be wrong.
newtype RecCanon = RecCanon { recSegs :: [RecSeg] }
  deriving (Eq, Ord, Show)

-- | What solving an equation produced: row-variable bindings, plus the
-- field types that must agree. Aligning two rows is a structural question
-- and this module answers only that; whether @Int@ matches an existential
-- is the calling typechecker's business, so matched fields whose types are
-- not already identical come back as obligations for it to discharge.
data RecSolution = RecSolution
  { recSubs     :: Map TVar RecExpr
  , recFieldEqs :: [(TypeU, TypeU)]
  } deriving (Eq, Show)

instance Semigroup RecSolution where
  RecSolution a x <> RecSolution b y = RecSolution (Map.union a b) (x <> y)

instance Monoid RecSolution where
  mempty = RecSolution Map.empty []

-- | Errors from rec-solver attempts.
data RecError
  = -- | Two rows disagree on a key, a key order, or a key's type; OR a
    -- Lacks constraint is violated; OR a key set conflict in a strict union.
    -- Carries a short diagnostic message.
    RecContradiction Text
    -- | The equation cannot be decided without more information; caller
    -- should defer the constraint.
  | RecDeferred
    -- | Difference / Intersect / Union encountered a non-canonicalizable
    -- subterm (e.g. a non-Rec TypeU).
  | RecMalformed Text
  deriving (Eq, Show)

-- | Build a canonical segment sequence: drop empty ground runs and merge
-- adjacent ones, so that each row has exactly one canonical form.
mkCanon :: [RecSeg] -> RecCanon
mkCanon = RecCanon . foldr step []
  where
    step (RecGround []) acc = acc
    step (RecGround a) (RecGround b : rest) = RecGround (a <> b) : rest
    step seg acc = seg : acc

-- | Reduce a RecExpr to its canonical (ordered) form. Returns Left on
-- type-level conflicts (e.g. union of overlapping ground keys).
normalize :: RecExpr -> Either RecError RecCanon
normalize RecEmpty = Right (mkCanon [])
normalize (RecVar v) = Right (mkCanon [RecTail v])
normalize (RecExtend k t rest) = do
  RecCanon segs <- normalize rest
  -- The extension is the head of the spine, so it leads the sequence.
  -- Only the known fields can be checked here; a duplicate hidden inside a
  -- row variable is a Lacks obligation for the caller.
  if any ((== k) . fst) (concatMap segFields segs)
    then Left (RecContradiction $ "Duplicate field in Rec extension: " <> k)
    else Right (mkCanon (RecGround [(k, t)] : segs))
normalize (RecUnion a b) = do
  RecCanon sa <- normalize a
  RecCanon sb <- normalize b
  let ka = keySet (concatMap segFields sa)
      kb = keySet (concatMap segFields sb)
      overlap = Set.intersection ka kb
  if Set.null overlap
    -- Concatenation, not merging: the left operand's segments lead. Two row
    -- variables meeting here is representable; whether the result can be
    -- solved is 'solveCanon''s problem, not normalization's.
    then Right (mkCanon (sa <> sb))
    else Left (RecContradiction $ "Rec union has overlapping keys: " <>
               commaSep (Set.toList overlap))
normalize (RecDiff a ks) = do
  RecCanon sa <- normalize a
  -- Removing an absent key is a no-op; the survivors keep their order. A row
  -- variable could in principle hold one of the dropped keys, so the diff
  -- stays symbolic over the tail; downstream Lacks constraints record the
  -- dropped key set against it.
  let drop_ = Set.fromList ks
      keep (RecGround fs) = RecGround (filter (\(k, _) -> not (Set.member k drop_)) fs)
      keep s = s
  Right (mkCanon (map keep sa))
normalize (RecIntersect a b) = do
  ca <- normalize a
  cb <- normalize b
  case (groundFields ca, groundFields cb) of
    (Just fa, Just fb) -> do
      -- Both ground: keep the left operand's fields that appear in the right
      -- at the same type, in the left operand's order.
      let rhs = Map.fromList fb
          shared = [(k, t) | (k, t) <- fa, Map.lookup k rhs == Just t]
          mismatched = [k | (k, t) <- fa, maybe False (/= t) (Map.lookup k rhs)]
      if null mismatched
        then Right (mkCanon [RecGround shared])
        else Left (RecContradiction $ "Rec intersection has type mismatch on: " <>
                   commaSep mismatched)
    _ -> Left RecDeferred

segFields :: RecSeg -> [(Text, TypeU)]
segFields (RecGround fs) = fs
segFields (RecTail _) = []

-- | The whole field sequence, when the row has no row variable.
groundFields :: RecCanon -> Maybe [(Text, TypeU)]
groundFields (RecCanon segs) = concat <$> traverse only segs
  where
    only (RecGround fs) = Just fs
    only (RecTail _) = Nothing

-- | True iff the canonical Rec has no row variable.
isGround :: RecCanon -> Bool
isGround c = case groundFields c of
  Just _ -> True
  Nothing -> False

-- | The free row-variables in a RecExpr.
freeRecVars :: RecExpr -> Set.Set TVar
freeRecVars (RecVar v) = Set.singleton v
freeRecVars RecEmpty = Set.empty
freeRecVars (RecExtend _ _ rest) = freeRecVars rest
freeRecVars (RecUnion a b) = Set.union (freeRecVars a) (freeRecVars b)
freeRecVars (RecDiff a _) = freeRecVars a
freeRecVars (RecIntersect a b) = Set.union (freeRecVars a) (freeRecVars b)

-- | Attempt to solve an equation @a == b@. Outcomes:
--
-- - 'Right': the rows align. The solution carries any row-variable
--   bindings and any field types the caller must still unify.
-- - 'Left RecContradiction msg': structural conflict (differing key sets,
--   differing key order, overlapping keys in a strict union).
-- - 'Left RecDeferred': cannot decide without more info; caller defers.
solveRec :: RecExpr -> RecExpr -> Either RecError RecSolution
solveRec lhs rhs = do
  l <- normalize lhs
  r <- normalize rhs
  solveCanon l r

solveCanon :: RecCanon -> RecCanon -> Either RecError RecSolution
solveCanon ca cb = case (groundFields ca, groundFields cb) of
  (Just fa, Just fb) -> groundEq fa fb
  -- One side is fully known: pin the other side's variables against it.
  -- 'matchAgainst' emits each obligation as (segment side, known side), so
  -- when the known side is the left operand the pairs come back reversed.
  -- subtype is directional, so they have to be put back.
  (Nothing, Just fb) -> matchAgainst (recSegs ca) fb
  (Just fa, Nothing) -> flipEqs <$> matchAgainst (recSegs cb) fa
  -- Both sides carry row variables. Link them when the shapes already agree;
  -- anything else needs more information. Without the linking case two fresh
  -- row variables introduced at different call sites would never connect,
  -- and an obligation carrying r2 would stay separate from an assumption
  -- carrying r1 even after the call equates them.
  (Nothing, Nothing) -> linkSegs (recSegs ca) (recSegs cb)

groundEq :: [(Text, TypeU)] -> [(Text, TypeU)] -> Either RecError RecSolution
groundEq fa fb
  | fa == fb = Right mempty
  | keySet fa /= keySet fb =
      Left (RecContradiction $ "Rec key sets differ: " <>
            keyDelta (keySet fa) (keySet fb))
  | map fst fa /= map fst fb =
      -- Same fields in a different order. These rows are isomorphic but not
      -- equal; the caller must reorder explicitly.
      Left (RecContradiction $ "Rec field order differs: " <>
            commaSep (map fst fa) <> " versus " <> commaSep (map fst fb))
  | otherwise =
      -- Same keys in the same order. Any field whose types are not already
      -- identical becomes an obligation: one of them may be an existential
      -- that only the typechecker can solve.
      Right mempty { recFieldEqs = [(ta, tb) | ((_, ta), (_, tb)) <- zip fa fb, ta /= tb] }

-- | Walk a segment sequence against a fully known field sequence, solving
-- each row variable from the fields it must span.
matchAgainst :: [RecSeg] -> [(Text, TypeU)] -> Either RecError RecSolution
matchAgainst = go mempty
  where
    go acc [] [] = Right acc
    go _ [] leftover =
      Left (RecContradiction $ "Rec has unmatched field(s): " <>
            commaSep (map fst leftover))
    go acc (RecGround fs : segs) target = do
      (rest, eqs) <- alignPrefix fs target
      go acc { recFieldEqs = recFieldEqs acc <> eqs } segs rest
    -- A trailing row variable absorbs everything that is left.
    go acc [RecTail v] target = bind acc v target
    -- A row variable delimited on the right by a known label: the label is
    -- unique, so the split is determined.
    go acc (RecTail v : segs@(RecGround ((k, _) : _) : _)) target =
      case break ((== k) . fst) target of
        (_, []) ->
          Left (RecContradiction $ "Rec is missing the field that pins a row \
                                   \variable: " <> k)
        (before, rest) -> do
          acc' <- bind acc v before
          go acc' segs rest
    -- Two row variables in a row: the split between them is unconstrained.
    go _ (RecTail _ : RecTail _ : _) _ = Left RecDeferred
    go _ (RecTail _ : RecGround [] : _) _ = Left RecDeferred

    bind acc v fs =
      let sol = canonToRecExpr (mkCanon [RecGround fs])
       in case Map.lookup v (recSubs acc) of
            Just prior
              | prior /= sol ->
                  Left (RecContradiction $ "Row variable " <> unTVar v <>
                        " is solved two different ways in one equation")
            _ -> Right acc { recSubs = Map.insert v sol (recSubs acc) }

-- | A known block must align with the same fields, in the same order.
alignPrefix :: [(Text, TypeU)] -> [(Text, TypeU)]
            -> Either RecError ([(Text, TypeU)], [(TypeU, TypeU)])
alignPrefix [] rest = Right (rest, [])
alignPrefix ((k, _) : _) [] =
  Left (RecContradiction $ "Rec has no field left to match key: " <> k)
alignPrefix ((k1, t1) : as) ((k2, t2) : bs)
  | k1 /= k2 =
      Left (RecContradiction $ "Rec field order differs: expected " <> k1 <>
            " at this position, found " <> k2)
  | otherwise = do
      (rest, eqs) <- alignPrefix as bs
      return (rest, if t1 == t2 then eqs else (t1, t2) : eqs)

-- | Both sides still hold row variables. Only a structural match is decided
-- here; anything else waits for one side to ground out.
linkSegs :: [RecSeg] -> [RecSeg] -> Either RecError RecSolution
linkSegs sa sb
  | length sa /= length sb = Left RecDeferred
  | otherwise = foldM step mempty (zip sa sb)
  where
    step acc (RecGround fa, RecGround fb)
      -- Differing known blocks are not a contradiction: a row variable
      -- elsewhere in the sequence can still absorb the difference.
      | map fst fa == map fst fb =
          Right acc { recFieldEqs = recFieldEqs acc <>
                        [(ta, tb) | ((_, ta), (_, tb)) <- zip fa fb, ta /= tb] }
      | otherwise = Left RecDeferred
    step acc (RecTail va, RecTail vb)
      | va == vb = Right acc
      | otherwise = Right acc { recSubs = Map.insert vb (RecVar va) (recSubs acc) }
    step _ _ = Left RecDeferred

-- | Lift a canonical Rec back to a RecExpr (for substitution into TypeU).
-- The sequence is rebuilt as-is: this function must never sort.
canonToRecExpr :: RecCanon -> RecExpr
canonToRecExpr (RecCanon segs) = case segs of
  [] -> RecEmpty
  _ -> foldr1 RecUnion (map fromSeg segs)
  where
    fromSeg (RecTail v) = RecVar v
    fromSeg (RecGround fs) = foldr (\(k, t) rest -> RecExtend k t rest) RecEmpty fs

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Put field obligations back into the caller's argument order.
flipEqs :: RecSolution -> RecSolution
flipEqs sol = sol { recFieldEqs = [(b, a) | (a, b) <- recFieldEqs sol] }

keySet :: [(Text, TypeU)] -> Set.Set Text
keySet = Set.fromList . map fst

commaSep :: [Text] -> Text
commaSep [] = ""
commaSep (x : xs) = foldl' (\acc b -> acc <> ", " <> b) x xs

keyDelta :: Set.Set Text -> Set.Set Text -> Text
keyDelta a b =
  let missing = Set.toList (Set.difference a b)
      extra = Set.toList (Set.difference b a)
   in (if null missing then "" else "missing in RHS=" <> commaSep missing)
      <> (if null missing || null extra then "" else "; ")
      <> (if null extra then "" else "extra in RHS=" <> commaSep extra)
