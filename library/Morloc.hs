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
  ) where

import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import Morloc.Namespace.State

import Morloc.CodeGenerator.Docstrings (processDocstrings)
import Morloc.CodeGenerator.Generate (generate)
import Morloc.CodeGenerator.LambdaEval (applyLambdas)
import Morloc.CodeGenerator.Realize (realityCheck)
import qualified Morloc.Frontend.API as F
import Morloc.Frontend.Restructure (restructure)
import Morloc.Frontend.Treeify (treeify)
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
    -- prepare scripts
    >>= uncurry generate
    -- (Script, [Script]) -> IO ()
    -- write the code and compile as needed
    >>= buildProgram
