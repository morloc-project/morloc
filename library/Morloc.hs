{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc
Description : Top-level compiler pipeline: parse, typecheck, generate, build
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Entry point for the morloc compiler library. Orchestrates the full pipeline:
parsing source into a module DAG, typechecking, code generation (pools +
manifest), and building executables. The 'writeProgram' function is the
main API consumed by the CLI.
-}
module Morloc
  ( writeProgram
  , typecheck
  , typecheckFrontend
  , generatePools
  ) where

import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.Namespace.Type

import Morloc.Data.Doc (pretty)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Morloc.CodeGenerator.Docstrings (processDocstrings)
import Morloc.CodeGenerator.Emit (TranslateFn, emit, pool)
import Morloc.CodeGenerator.Express (express, addCacheWraps, addDebugWraps)
import Morloc.CodeGenerator.LambdaEval (applyLambdas)
import Morloc.CodeGenerator.Namespace (SerialManifold)
import qualified Morloc.CodeGenerator.Nexus as Nexus
import Morloc.CodeGenerator.Parameterize (parameterize)
import Morloc.CodeGenerator.Realize (realityCheck)
import Morloc.CodeGenerator.Segment (segment)
import Morloc.CodeGenerator.Reduce (reduce)
import Morloc.CodeGenerator.Serialize (serialize)
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Frontend.API as F
import qualified Morloc.Frontend.AST as AST
import Morloc.Frontend.Restructure (restructure)
import Morloc.Frontend.Treeify (treeify)
import qualified Morloc.Data.PoolHash as PoolHash
import qualified Morloc.Monad as MM
import Morloc.ProgramBuilder.Build (buildProgram)

-- | Check the general types only
typecheckFrontend ::
  Maybe Path ->
  Code ->
  MorlocMonad [AnnoS (Indexed TypeU) Many Int]
typecheckFrontend path code = do
  dag <- F.parse path code
  evalMode <- MM.gets stateEvalMode
  if evalMode then checkEvalRestrictions dag else return ()
  case DAG.roots dag of
    (r : _) -> MM.modify (\s -> s {stateModuleName = Just r})
    _ -> return ()
  restructure dag
    >>= treeify
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
generatePools rASTs = do
  paramRASTs <- mapM parameterize rASTs
  let langMap = Map.fromList
        [(midx, lang) | AnnoS (Idx midx _) (Idx _ lang, _) _ <- paramRASTs]
  MM.modify (\s -> s { stateManifoldLang = langMap })
  mapM express paramRASTs
    >>= mapM segment |>> concat
    >>= mapM serialize
    >>= mapM reduce
      |>> pool

-- | Build a program as a local executable
writeProgram ::
  -- | language-specific translator callback
  TranslateFn ->
  -- | source code filename (for debugging messages)
  Maybe Path ->
  -- | source code text
  Code ->
  MorlocMonad ()
writeProgram translateFn path code = do
  typecheck path code
    -- evaluate all applied lambdas in rasts and gasts
    >>= bimapM (mapM applyLambdas) (mapM applyLambdas)
    -- process docstrings to determine how to build CLI
    >>= bimapM (mapM processDocstrings) (mapM processDocstrings)
    -- generate nexus and pools
    >>= \(gASTs, rASTs) ->
      do
        -- Filter out generic (polymorphic) exports -- they can't become CLI subcommands
        let isConcreteExport (AnnoS (Idx _ t) _ _, _) = not (containsUnk t)
            (concreteGASTs, genericGASTs) = partition isConcreteExport gASTs
            (concreteRASTs, genericRASTs) = partition isConcreteExport rASTs
            warnSkip (AnnoS (Idx i _) _ _) = do
              name <- MM.metaName i
              case name of
                Just (EV n) -> MM.say $ "Warning: skipping generic export '" <> pretty n <> "'"
                Nothing -> return ()
        mapM_ (warnSkip . fst) genericGASTs
        mapM_ (warnSkip . fst) genericRASTs
        -- Only exports become commands, but helper rASTs also flow to
        -- 'Nexus.generate' so it can see their languages for pool
        -- enumeration -- the export tree ends at a bare 'CallS' back-edge
        -- after 'extractRecursiveHelpers' and hides them otherwise.
        exports <- MM.gets stateExports
        let exportSet = Set.fromList exports
            isExported (AnnoS (Idx midx _) _ _, _) = Set.member midx exportSet
            exportedRASTs = filter isExported concreteRASTs
            helperRASTs = map fst (filter (not . isExported) concreteRASTs)
        nexus <- Nexus.generate concreteGASTs exportedRASTs helperRASTs
        MM.startCounter
        paramRASTs <- mapM parameterize (map fst concreteRASTs)
        let langMap = Map.fromList
              [(midx, lang) | AnnoS (Idx midx _) (Idx _ lang, _) _ <- paramRASTs]
        MM.modify (\s -> s { stateManifoldLang = langMap })
        pools <-
          mapM express paramRASTs
            -- Wrap each cache:true manifold's body in a 'PolyCacheBody'.
            >>= mapM addCacheWraps
            -- When 'stateDebugTrace' (--debug), wrap every foreign-call
            -- manifold body in 'PolyDebugWrap'. No-op when the flag is off.
            >>= mapM addDebugWraps
            >>= mapM segment |>> concat
            >>= mapM serialize
            >>= mapM reduce
              |>> pool
            >>= mapM (uncurry (emit translateFn))
        -- Fingerprint each pool's emitted source and substitute the
        -- hex hashes into the @<MORLOC_POOL_HASH:lang>@ placeholders
        -- the nexus manifest carries. The hashes are computed AFTER
        -- pool emission so they cover the final source bytes, and
        -- BEFORE 'buildProgram' so the resolved manifest is what hits
        -- disk. @hash-include@ files from the program YAML fold into
        -- every pool's hash so they invalidate the cache too.
        hashIncludes <- MM.gets stateHashIncludePaths
        poolHashes <- MM.liftIO $ PoolHash.computePoolHashes hashIncludes pools
        let nexusPatched = PoolHash.patchManifestPoolHashes poolHashes nexus
        buildProgram (nexusPatched, pools)

-- | An eval input is a single expression, not a module: it may import
-- installed modules and use let/where/do, but may not define types,
-- typeclasses, instances, or source foreign code. The check is fully
-- recursive so a forbidden construct cannot hide inside a nested
-- let/where/do block (the only grammatical path is a forced
-- expression). Imported modules are not checked: eval mode also
-- disables local-module resolution, so every import is genuinely
-- pre-existing installed code.
checkEvalRestrictions :: DAG MVar Import ExprI -> MorlocMonad ()
checkEvalRestrictions dag =
  case DAG.roots dag of
    [] -> return ()
    (root : _) -> case Map.lookup root dag of
      Nothing -> return ()
      Just (ExprI _ (ModE _ body), _) -> mapM_ (AST.checkExprI checkExpr) body
      Just _ -> return ()
  where
    checkExpr :: ExprI -> MorlocMonad ()
    checkExpr (ExprI i (SrcE _)) =
      MM.throwSourcedError i "source statements are not allowed in eval mode"
    checkExpr (ExprI i (ClsE _)) =
      MM.throwSourcedError i "class declarations are not allowed in eval mode"
    checkExpr (ExprI i (IstE _ _ _)) =
      MM.throwSourcedError i "instance declarations are not allowed in eval mode"
    checkExpr (ExprI i (TypE _)) =
      MM.throwSourcedError i "type declarations are not allowed in eval mode"
    checkExpr _ = return ()
