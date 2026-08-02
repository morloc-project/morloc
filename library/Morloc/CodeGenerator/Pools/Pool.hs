{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Pools.Pool
Description : The Pool/Member interface for code-generation targets
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A pool is an ABI/runtime family that compiles to one daemon process; a member
is a language within a pool. This module defines the language-agnostic Member
interface. A member's codegen internals (its 'LowerConfig', 'SerialAST'
handling, and packers) live behind 'memberEmit' and never surface here, so the
interface stays small and uniform across languages.

The concrete members live under "Morloc.CodeGenerator.Pools.CAbi.Members": the
bespoke C++ translator and the generic (Python/R/...) translator both plug into
this shared seam through 'memberEmit'.
-}
module Morloc.CodeGenerator.Pools.Pool
  ( Member (..)
  ) where

import Morloc.CodeGenerator.Namespace

-- | A code-generation member: a language that emits a pool's source for a set
-- of serialized manifolds. 'memberEmit' has the same shape as the former
-- per-language 'TranslateFn' callback; everything else it needs it reads from
-- 'MorlocMonad' state.
data Member = Member
  { memberEmit :: [Source] -> [SerialManifold] -> MorlocMonad Script
  }
