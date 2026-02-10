{- |
Module      : Morloc
Description : Top-level compiler layout
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc
  ( writeProgram
  , typecheck
  , typecheckFrontend
  , generatePools
  ) where

import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import Morloc.Namespace.State

import Morloc.CodeGenerator.Docstrings (processDocstrings)
import Morloc.CodeGenerator.Emit (pool, emit)
import Morloc.CodeGenerator.Namespace (SerialManifold)
import Morloc.CodeGenerator.Express (express)
import Morloc.CodeGenerator.LambdaEval (applyLambdas)
import Morloc.CodeGenerator.Parameterize (parameterize)
import Morloc.CodeGenerator.Realize (realityCheck)
import Morloc.CodeGenerator.Segment (segment)
import Morloc.CodeGenerator.Serialize (serialize)
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Morloc.Frontend.API as F
import Morloc.Frontend.Restructure (restructure)
import Morloc.Frontend.Treeify (treeify)
import qualified Morloc.Monad as MM
import Morloc.ProgramBuilder.Build (buildProgram)

-- | Check the general types only
typecheckFrontend ::
  Maybe Path ->
  Code ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheckFrontend path code =
  -- Maybe Path -> Text -> [Module]
  -- parse code into unannotated modules
  F.parse path code
    -- resolve type aliases and such
    >>= restructure
    -- convert to Sanno
    >>= treeify
    -- add type annotations to sub-expressions and raise type errors
    >>= F.typecheck

-- | Check general types and also resolve implementations
typecheck ::
  Maybe Path ->
  Code ->
  MorlocMonad
    ( [AnnoS (Indexed Type) One ()]
    , [AnnoS (Indexed Type) One (Indexed Lang)]
    )
typecheck path code =
  typecheckFrontend path code
    -- resolve all TypeU types to Type
    |>> map F.resolveTypes
    -- resolve all TypeU types to Type
    >>= mapM F.valuecheck
    -- check for value contradictions between implementations
    >>= realityCheck

-- | Do everything except language specific code generation.
generatePools :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad [(Lang, [SerialManifold])]
generatePools rASTs =
  mapM parameterize rASTs
    >>= mapM express
    >>= mapM segment |>> concat
    >>= mapM serialize
    |>> pool

-- | Build a program as a local executable
writeProgram ::
  -- | source code filename (for debugging messages)
  Maybe Path ->
  -- | source code text
  Code ->
  MorlocMonad ()
writeProgram path code =
  typecheck path code
    -- evaluate all applied lambdas in rasts and gasts
    >>= bimapM (mapM applyLambdas) (mapM applyLambdas)
    -- process docstrings to determine how to build CLI
    >>= bimapM (mapM processDocstrings) (mapM processDocstrings)
    -- generate nexus and pools
    >>= \(gASTs, rASTs) -> do
      nexus <- Nexus.generate gASTs rASTs
      MM.startCounter
      pools <-
        mapM parameterize (map fst rASTs)
          >>= mapM express
          >>= mapM segment |>> concat
          >>= mapM serialize
          |>> pool
          >>= mapM (uncurry emit)
      return (nexus, pools)
    -- write the code and compile as needed
    >>= buildProgram
