{- |
Module      : Morloc.Data.Usefulness
Description : Exhaustiveness and redundancy of pattern matrices
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Usefulness of a pattern vector with respect to a pattern matrix, after
Maranget, "Warnings for pattern matching" (JFP 2007). A clause list is
exhaustive when the all-wildcard vector is not useful against it, and a
clause is redundant when it is not useful against the clauses before it.

The algorithm sees only constructor heads and their arities. The caller
describes its types through a 'Signature': a head must identify the type it
belongs to, so that the heads present in a column decide whether they cover
it.
-}
module Morloc.Data.Usefulness
  ( Pat (..)
  , Witness (..)
  , Coverage (..)
  , Signature (..)
  , useful
  , missing
  , redundant
  ) where

import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)

data Pat c = Wild | Con c [Pat c]
  deriving (Show, Eq, Ord)

-- | A set of values no row matches.
data Witness c
  = Any
  -- | A value whose head is none of these.
  | Other [c]
  | WCon c [Witness c]
  deriving (Show, Eq, Ord)

-- | What the distinct heads present in a column say about its type.
data Coverage c
  = Complete
  -- | The type is finite and these of its heads are absent.
  | Absent [c]
  -- | No finite set of heads covers the type, as for literals.
  | Unbounded
  deriving (Show, Eq)

data Signature c = Signature
  { arity :: c -> Int
  , coverage :: [c] -> Coverage c
  -- ^ called only with a non-empty list of distinct heads of one type
  }

-- | A witness vector of values that match @q@ and no row, if any exist.
useful :: Eq c => Signature c -> [[Pat c]] -> [Pat c] -> Maybe [Witness c]
useful _ rows [] = if null rows then Just [] else Nothing
useful sig rows (Con c ps : qs) =
  rebuild c (length ps) <$> useful sig (specialize sig c rows) (ps <> qs)
useful sig rows (Wild : qs) = case columnCoverage sig rows of
  Nothing -> (Any :) <$> rest
  Just (hs, Complete) ->
    listToMaybe (mapMaybe (\c -> rebuild c (arity sig c) <$> useful sig (specialize sig c rows) (wilds sig c <> qs)) hs)
  Just (_, Absent (c : _)) -> (WCon c (anys sig c) :) <$> rest
  Just (hs, _) -> (Other hs :) <$> rest
  where
    rest = useful sig (defaults rows) qs

-- | Up to @limit@ witness vectors of width @n@ that together, untruncated,
-- hold exactly the value vectors no row matches; none when the rows are
-- exhaustive. The witnesses are disjoint.
missing :: Eq c => Signature c -> Int -> Int -> [[Pat c]] -> [[Witness c]]
missing sig limit n rows = take limit (witnesses sig n rows)

witnesses :: Eq c => Signature c -> Int -> [[Pat c]] -> [[Witness c]]
witnesses _ 0 rows = [[] | null rows]
witnesses sig n rows = case columnCoverage sig rows of
  Nothing -> map (Any :) rest
  Just (hs, cov) ->
    let present = concat [map (rebuild c (arity sig c)) (witnesses sig (arity sig c + n - 1) (specialize sig c rows)) | c <- hs]
     in case cov of
          Complete -> present
          Absent cs -> [WCon c (anys sig c) : w | c <- cs, w <- rest] <> present
          Unbounded -> map (Other hs :) rest <> present
  where
    rest = witnesses sig (n - 1) (defaults rows)

-- | The indices of the rows that match no value the rows before them leave
-- unmatched. A guarded row (flagged True) may fail its guard, so it covers
-- nothing for the rows after it, though it may itself be redundant.
redundant :: Eq c => Signature c -> [(Bool, [Pat c])] -> [Int]
redundant sig = go 0 []
  where
    go _ _ [] = []
    go i seen ((guarded, row) : rest) =
      [i | null (useful sig seen row)]
        <> go (i + 1) (if guarded then seen else seen <> [row]) rest

-- | The distinct heads of the first column and what they cover, if it has
-- any heads.
columnCoverage :: Eq c => Signature c -> [[Pat c]] -> Maybe ([c], Coverage c)
columnCoverage sig rows = case nub [c | Con c _ : _ <- rows] of
  [] -> Nothing
  hs -> Just (hs, case coverage sig hs of
    Absent [] -> Complete
    cov -> cov)

specialize :: Eq c => Signature c -> c -> [[Pat c]] -> [[Pat c]]
specialize sig c = mapMaybe row
  where
    row (Con c' ps : rest)
      | c' == c = Just (ps <> rest)
      | otherwise = Nothing
    row (Wild : rest) = Just (wilds sig c <> rest)
    row [] = Nothing

defaults :: [[Pat c]] -> [[Pat c]]
defaults rows = [rest | Wild : rest <- rows]

wilds :: Signature c -> c -> [Pat c]
wilds sig c = replicate (arity sig c) Wild

anys :: Signature c -> c -> [Witness c]
anys sig c = replicate (arity sig c) Any

rebuild :: c -> Int -> [Witness c] -> [Witness c]
rebuild c a w = WCon c (take a w) : drop a w
