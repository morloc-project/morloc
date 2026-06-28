{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.IFile
Description : Shared helpers for IFile pattern-walker codegen.
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The IFile pattern walker (@mlc_ifile_walk@) is invoked from two places:

  * 'Morloc.CodeGenerator.Express' (pool-side codegen)
  * 'Morloc.CodeGenerator.Nexus' (pure-runtime nexus codegen)

Both paths share three concerns:

  1. Validating that a 'Selector' chain is a single-field walk
     (multi-field groups like @.(.1, .2) f@ are not yet supported by the
     runtime walker).
  2. Converting a 'Selector' into a list of 'WalkStep's.
  3. Encoding 'WalkStep's as the path string the runtime walker consumes.

This module owns those three concerns so the two callers can share the
encoding and never drift.
-}

module Morloc.CodeGenerator.IFile
  ( WalkStep (..)
  , selectorToWalkSteps
  , walkStepsToPath
  , bracketIndexSteps
  , bracketSliceSteps
  , typeHeadIsIFile
  ) where

import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Namespace
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

-- | One step in an IFile walk. Field steps consume no runtime args;
-- bracket steps consume runtime args (1 for BracketIndex, 3 for
-- BracketSlice) in DFS order at evaluation time. Group steps fan out
-- into multiple sibling sub-walks whose results pack into a tuple.
data WalkStep
  = WalkField Int          -- ^ Tuple-slot step (e.g. @.1@); offset into
                           -- the struct on disk.
  | WalkKey Text           -- ^ Record-field step (e.g. @.foo@);
                           -- resolved via the value schema's field
                           -- table.
  | WalkBracketIndex       -- ^ @.[i]@; consumes one runtime arg.
  | WalkBracketSlice       -- ^ @.[s:e:p]@; consumes three runtime args.
  | WalkGroup [[WalkStep]] -- ^ Multi-field selector (e.g.
                           -- @.(.x, .y) f@); each inner @[WalkStep]@
                           -- is a sibling sub-walk run at the same
                           -- file position. Result is a tuple of the
                           -- sibling values. Group children may
                           -- themselves contain bracket steps or
                           -- nested groups; runtime args flow in DFS
                           -- order across all children.
  deriving (Show, Ord, Eq)

-- | Flatten a 'Selector' into a walk-step chain. Single-sibling
-- field steps pass through as a flat sequence; multi-sibling groups
-- become a 'WalkGroup'; bracket-index / bracket-slice steps map to
-- their walk-step analogs.
selectorToWalkSteps :: Int -> Selector -> MorlocMonad [WalkStep]
selectorToWalkSteps _    SelectorEnd                  = return []
selectorToWalkSteps midx (SelectorIdx (i, s) [])      = (WalkField i :) <$> selectorToWalkSteps midx s
selectorToWalkSteps midx (SelectorKey (k, s) [])      = (WalkKey   k :) <$> selectorToWalkSteps midx s
selectorToWalkSteps midx (SelectorBracketIndex s)     = (WalkBracketIndex :) <$> selectorToWalkSteps midx s
selectorToWalkSteps _    SelectorBracketSlice         = return [WalkBracketSlice]
selectorToWalkSteps midx (SelectorIdx hd others)      = oneGroup midx (hd : others) WalkField
selectorToWalkSteps midx (SelectorKey hd others)      = oneGroup midx (hd : others) WalkKey

oneGroup
  :: Int                          -- ^ midx for sourced errors
  -> [(a, Selector)]              -- ^ siblings: head + tail-of-each
  -> (a -> WalkStep)              -- ^ how to wrap the sibling's access step
  -> MorlocMonad [WalkStep]
oneGroup midx siblings mkHead = do
  chains <- mapM (\(h, sub) -> (mkHead h :) <$> selectorToWalkSteps midx sub) siblings
  return [WalkGroup chains]

-- | Encode walk steps as the path string the runtime walker parses.
--
-- @
--   [WalkField 1]                                 -> ".1"
--   [WalkKey "foo"]                               -> ".foo"
--   [WalkBracketIndex]                            -> ".[]"
--   [WalkBracketSlice]                            -> ".[:]"
--   [WalkField 1, WalkBracketIndex]               -> ".1.[]"
--   [WalkKey "foo", WalkBracketSlice]             -> ".foo.[:]"
--   [WalkGroup [[WalkKey "x"], [WalkKey "y"]]]    -> ".(.x;.y)"
--   [WalkField 0, WalkGroup [[WalkKey "x"], [WalkKey "y"]]]
--                                                  -> ".0.(.x;.y)"
-- @
--
-- Steps emit their leading '.' so the empty step list maps to the empty
-- string (a pure pass-through walk). Inside a group, sibling chains
-- are separated by ';' (field names are @[A-Za-z_][A-Za-z0-9_]*@ so
-- ';' is unambiguous as a delimiter).
walkStepsToPath :: [WalkStep] -> Text
walkStepsToPath = T.concat . map step1
  where
    step1 (WalkField i)        = "." <> T.pack (show i)
    step1 (WalkKey k)          = "." <> k
    step1 WalkBracketIndex     = ".[]"
    step1 WalkBracketSlice     = ".[:]"
    step1 (WalkGroup chains)   =
      ".(" <> T.intercalate ";" (map walkStepsToPath chains) <> ")"

-- | Convenience: a single bracket-index step.
bracketIndexSteps :: [WalkStep]
bracketIndexSteps = [WalkBracketIndex]

-- | Convenience: a single bracket-slice step.
bracketSliceSteps :: [WalkStep]
bracketSliceSteps = [WalkBracketSlice]

-- | True if @t@'s outermost head is @IFile@ after walking alias chains
-- and peeling @EffectT@ / @AppT@ wrappers. Used by both codegen paths
-- to decide whether a pattern application routes to the walker.
typeHeadIsIFile :: Int -> Type -> MorlocMonad Bool
typeHeadIsIFile midx originalType = do
  scope <- MM.getGeneralScope midx
  go scope (type2typeu (peel originalType))
  where
    peel (EffectT _ inner) = peel inner
    peel other = other
    go scope t
      | extractKey t == BT.ifileVar = return True
      | otherwise = case TE.reduceType scope t of
          Just t' | t' /= t -> go scope t'
          _ -> return False

