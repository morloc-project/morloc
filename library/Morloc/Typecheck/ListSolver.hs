{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Typecheck.ListSolver
Description : Type-level ordered list solver
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Solves equality and basic constraint problems on List-kinded type
expressions (Stage 8 of the tables refactor - foundational kind layer).

The canonical form is a flat sequence of "chunks" where each chunk is
either a ground literal element ('Lit') or a list-tail variable ('Var').
Two lists are equal when their flattened chunks agree on the boundary
elements; a variable on one side and a ground residue on the other
solves the variable.

Decidability:

- Two ground literal lists are compared element-wise (ordered).
- A single variable equated to a ground list solves the variable.
- A ground prefix or suffix on each side is stripped before solving.
- Two variables in the middle defer.

Element-level equality (e.g. between two 'StrLitU' values) is checked by
this solver structurally; per-element type unification is the calling
typechecker's responsibility - this solver only checks structural
equality of the chunk sequence.
-}
module Morloc.Typecheck.ListSolver
  ( ListExpr (..)
  , ListChunk (..)
  , ListError (..)
  , normalize
  , listEqual
  , solveList
  , canonToTypeU
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Namespace.Type (TypeU (..))

-- | The structural form a ListExpr can take. Mirrors the TypeU List
-- constructors; the bridge to TypeU happens in Typecheck.Internal.
data ListExpr
  = ListVar TVar
  | ListLit [TypeU]
  | ListApp ListExpr ListExpr
  deriving (Eq, Ord, Show)

-- | A chunk in the flattened canonical form: either a contiguous
-- run of ground elements or a polymorphic tail variable.
data ListChunk
  = Lit [TypeU]
  | Var TVar
  deriving (Eq, Ord, Show)

-- | Errors from list-solver attempts.
data ListError
  = -- | The two lists cannot be reconciled (length mismatch on grounds,
    -- element-level mismatch, etc.).
    ListContradiction Text
  | -- | The equation cannot be decided without more information.
    ListDeferred
  deriving (Eq, Show)

-- | Flatten a ListExpr into a list of chunks. ListApp is left-recursed
-- so the chunk sequence reads in left-to-right order. Adjacent literal
-- chunks are merged.
normalize :: ListExpr -> [ListChunk]
normalize = mergeLits . flatten
  where
    flatten (ListVar v) = [Var v]
    flatten (ListLit es) = [Lit es]
    flatten (ListApp a b) = flatten a ++ flatten b

    mergeLits [] = []
    mergeLits (Lit xs : Lit ys : rest) = mergeLits (Lit (xs ++ ys) : rest)
    mergeLits (Lit [] : rest) = mergeLits rest
    mergeLits (c : rest) = c : mergeLits rest

-- | Equality on List expressions, after normalization.
listEqual :: ListExpr -> ListExpr -> Bool
listEqual a b = normalize a == normalize b

-- | Attempt to solve an equation @a == b@.
--
-- - @Right Map.empty@: equation already satisfied.
-- - @Right subs@: list variable(s) solved.
-- - @Left ListContradiction msg@: structural mismatch.
-- - @Left ListDeferred@: cannot decide.
solveList :: ListExpr -> ListExpr -> Either ListError (Map TVar ListExpr)
solveList a b = solveChunks (normalize a) (normalize b)

solveChunks :: [ListChunk] -> [ListChunk] -> Either ListError (Map TVar ListExpr)
solveChunks ca cb
  | ca == cb = Right Map.empty
  | otherwise = do
      -- Strip matching ground prefixes element-by-element.
      let (ca', cb') = stripPrefix ca cb
      let (ca'', cb'') = stripSuffix ca' cb'
      case (ca'', cb'') of
        ([], []) -> Right Map.empty
        ([Var v], rest) -> Right (Map.singleton v (chunksToListExpr rest))
        (rest, [Var v]) -> Right (Map.singleton v (chunksToListExpr rest))
        ([], _) -> Left (ListContradiction "List length mismatch (RHS longer)")
        (_, []) -> Left (ListContradiction "List length mismatch (LHS longer)")
        _ -> Left ListDeferred

-- | Strip element-by-element matching ground prefixes from two chunk lists.
stripPrefix :: [ListChunk] -> [ListChunk] -> ([ListChunk], [ListChunk])
stripPrefix (Lit (x : xs) : as) (Lit (y : ys) : bs)
  | x == y = stripPrefix (consLit xs as) (consLit ys bs)
  where
    consLit [] rest = rest
    consLit zs rest = Lit zs : rest
stripPrefix as bs = (as, bs)

-- | Strip element-by-element matching ground suffixes.
stripSuffix :: [ListChunk] -> [ListChunk] -> ([ListChunk], [ListChunk])
stripSuffix as bs =
  let (as', bs') = stripPrefix (reverseChunks as) (reverseChunks bs)
   in (reverseChunks as', reverseChunks bs')
  where
    reverseChunks = reverse . map reverseChunk
    reverseChunk (Lit es) = Lit (reverse es)
    reverseChunk c@(Var _) = c

-- | Lift a chunk sequence back to a ListExpr.
chunksToListExpr :: [ListChunk] -> ListExpr
chunksToListExpr [] = ListLit []
chunksToListExpr [Lit es] = ListLit es
chunksToListExpr [Var v] = ListVar v
chunksToListExpr (c : cs) = foldl ListApp (chunkToExpr c) (map chunkToExpr cs)
  where
    chunkToExpr (Lit es) = ListLit es
    chunkToExpr (Var v) = ListVar v

-- | Lift a chunk sequence back to a TypeU.
canonToTypeU :: [ListChunk] -> TypeU
canonToTypeU = listExprToTypeU . chunksToListExpr

listExprToTypeU :: ListExpr -> TypeU
listExprToTypeU (ListVar v) = ListVarU v
listExprToTypeU (ListLit es) = ListLitU es
listExprToTypeU (ListApp a b) = ListAppU (listExprToTypeU a) (listExprToTypeU b)
