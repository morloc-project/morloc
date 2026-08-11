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

import Morloc.Data.Doc (pretty, (<+>), squotes)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Morloc.Build.Params as BP
import qualified Data.Set as Set

import Morloc.CodeGenerator.Docstrings (processDocstrings)
import Morloc.CodeGenerator.EffectBoundary (checkEffectBoundaries, insertEffectBoundaries)
import Morloc.CodeGenerator.Emit (TranslateFn, emit, pool)
import Morloc.CodeGenerator.Express (express, addCacheWraps, addDebugWraps, addLoopWraps)
import Morloc.CodeGenerator.LambdaEval (applyLambdas)
import Morloc.CodeGenerator.Namespace (SerialManifold)
import qualified Morloc.CodeGenerator.Nexus as Nexus
import Morloc.CodeGenerator.Parameterize (parameterize)
import Morloc.CodeGenerator.Guest.Pass (lowerGuests)
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
    -- lower guest-language sources (e.g. Futhark) into host glue
    >>= lowerGuests
    -- resolve all TypeU types to Type
    |>> map F.resolveTypes
    -- resolve all TypeU types to Type
    >>= mapM F.valuecheck
    -- check for value contradictions between implementations
    >>= realityCheck

-- | Do everything except language specific code generation.
--
-- Runs 'applyLambdas' first, matching the pool path in 'writeProgram': the
-- 'express' pipeline assumes lambdas have been reduced/lifted, so skipping it
-- lets a lambda reach 'expressPolyApp' as an un-handled head ("unexpected
-- LamS"). 'False' keeps a multiply-used lambda as a shared native closure
-- (rASTs become pools, not the pure nexus evaluator).
generatePools :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad [(Lang, [SerialManifold])]
generatePools rASTs0 = do
  reg <- MM.gets stateLangRegistry
  rASTs <- mapM (applyLambdas False) rASTs0
  paramRASTs <- mapM parameterize rASTs
  let langMap = Map.fromList
        [(midx, lang) | AnnoS (Idx midx _) (Idx _ lang, _) _ <- paramRASTs]
  MM.modify (\s -> s { stateManifoldLang = langMap })
  mapM express paramRASTs
    -- Boundary reconciliation + invariant check. Insertion installs
    -- 'PolyEval' / 'PolyDoBlock' at every boundary whose declared type
    -- disagrees with its calling convention; the checker asserts the
    -- resulting tree satisfies the invariant. A checker failure means
    -- 'EffectBoundary' is missing a table row.
    >>= mapM (\ph -> do
                 ph' <- insertEffectBoundaries ph
                 checkEffectBoundaries ph'
                 return ph')
    >>= mapM segment |>> concat
    >>= mapM serialize
    >>= mapM reduce
      |>> pool reg

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
    -- Evaluate applied lambdas. gASTs run in the pure nexus evaluator, which
    -- cannot hold a native closure, so let-bound lambdas are always inlined
    -- there (True); rASTs become pools, where a multiply-used lambda is kept
    -- as a shared native closure to avoid exponential inlining (False).
    >>= bimapM (mapM (applyLambdas True)) (mapM (applyLambdas False))
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
        (nexus, wrappers) <- Nexus.generate concreteGASTs exportedRASTs helperRASTs
        MM.startCounter
        paramRASTs <- mapM parameterize (map fst concreteRASTs)
        let langMap = Map.fromList
              [(midx, lang) | AnnoS (Idx midx _) (Idx _ lang, _) _ <- paramRASTs]
        MM.modify (\s -> s { stateManifoldLang = langMap })
        reg <- MM.gets stateLangRegistry
        pools <-
          mapM express paramRASTs
            -- Wrap each cache:true manifold's body in a 'PolyCacheBody'.
            >>= mapM addCacheWraps
            -- When 'stateDebugTrace' (--debug), wrap every foreign-call
            -- manifold body in 'PolyDebugWrap'. No-op when the flag is off.
            >>= mapM addDebugWraps
            -- Lower eligible tail-recursive helpers to native 'PolyLoop's.
            >>= mapM addLoopWraps
            -- Boundary reconciliation + invariant check; see 'generatePools'.
            >>= mapM (\ph -> do
                         ph' <- insertEffectBoundaries ph
                         checkEffectBoundaries ph'
                         return ph')
            >>= mapM segment |>> concat
            >>= mapM serialize
            >>= mapM reduce
              |>> pool reg
            >>= mapM (uncurry (emit translateFn))
        -- Fingerprint each pool's emitted source and substitute the
        -- hex hashes into the @<MORLOC_POOL_HASH:lang>@ placeholders
        -- the nexus manifest carries. The hashes are computed AFTER
        -- pool emission so they cover the final source bytes, and
        -- BEFORE 'buildProgram' so the resolved manifest is what hits
        -- disk. @hash-include@ files from the program YAML fold into
        -- every pool's hash so they invalidate the cache too.
        hashIncludes <- MM.gets stateHashIncludePaths
        -- Build parameters and guest object files are inputs that change the
        -- emitted binary without changing the emitted pool source, so they must
        -- enter the cache key too (see 'PoolHash.computePoolHashes'). The guest
        -- objects are hashed once here (not re-read per pool) and their
        -- fingerprint folded into the salt.
        params <- MM.gets stateLangParams
        guestObjects <-
          MM.gets $ \s ->
            [ T.unpack f
            | f <- concatMap packageCxxFlags (statePackageMeta s)
            , ".o" `T.isSuffixOf` f
            ]
        objHash <- MM.liftIO $ PoolHash.hashFiles guestObjects
        let buildSalt
              | null guestObjects = BP.renderSalt params
              | otherwise = BP.renderSalt params <> "|obj:" <> PoolHash.poolHashHex objHash
        poolHashes <- MM.liftIO $ PoolHash.computePoolHashes buildSalt hashIncludes pools
        let nexusPatched = PoolHash.patchManifestPoolHashes poolHashes nexus
        buildProgram (nexusPatched, wrappers, pools)

-- | An eval input is a single expression, not a module: it may import
-- installed modules and use let/where/do, but may not define types,
-- typeclasses, instances, or source foreign code. The check is fully
-- recursive so a forbidden construct cannot hide inside a nested
-- let/where/do block (the only grammatical path is a forced
-- expression). It walks only the root DAG node -- the user-written code
-- -- so imported modules (separate nodes) are never inspected.
--
-- In sandbox mode ('stateEvalSandbox' = Just), two further gates apply
-- to the untrusted expression: no directly-written IO intrinsic (Gate 1)
-- and no top-level import outside the allow-list (Gate 2). Both are
-- inert for trusted dev eval (Nothing), where local IO and any installed
-- module remain available.
checkEvalRestrictions :: DAG MVar Import ExprI -> MorlocMonad ()
checkEvalRestrictions dag = do
  sandbox <- MM.gets stateEvalSandbox
  case DAG.roots dag of
    [] -> return ()
    (root : _) -> case Map.lookup root dag of
      Nothing -> return ()
      Just (ExprI _ (ModE _ body), edges) -> do
        mapM_ (AST.checkExprI (checkExpr sandbox)) body
        -- Gate 2: match on the resolved target MVar (the DAG child key),
        -- so `import M as N` is still checked against M. A vetted facade's
        -- own imports are edges from its node, never reached here, so the
        -- facade may re-export terms drawn from non-listed modules. Inert
        -- (Maybe is Foldable) when not sandboxed.
        mapM_ (\allowed -> mapM_ (checkImport allowed) edges) sandbox
      Just _ -> return ()
  where
    checkExpr :: Maybe (Set.Set MVar) -> ExprI -> MorlocMonad ()
    checkExpr _ (ExprI i (SrcE _)) =
      MM.throwSourcedError i "source statements are not allowed in eval mode"
    checkExpr _ (ExprI i (ClsE _)) =
      MM.throwSourcedError i "class declarations are not allowed in eval mode"
    checkExpr _ (ExprI i (IstE _ _ _)) =
      MM.throwSourcedError i "instance declarations are not allowed in eval mode"
    checkExpr _ (ExprI i (TypE _)) =
      MM.throwSourcedError i "type declarations are not allowed in eval mode"
    -- Gate 1: a directly-written IO intrinsic is refused in sandbox mode.
    -- Calling an exported function that uses one internally is still fine:
    -- that intrinsic lives in another DAG node this walk never visits.
    checkExpr (Just _) (ExprI i (IntrinsicE intr _))
      | intrinsicIsIO intr =
          MM.throwSourcedError i
            "IO intrinsics may not be used directly in a sandboxed eval expression; \
            \wrap the intrinsic in an exported function of an allow-listed module instead"
    checkExpr _ _ = return ()

    checkImport :: Set.Set MVar -> (MVar, Import) -> MorlocMonad ()
    checkImport allowed (childMV, _)
      | Set.member childMV allowed = return ()
      | otherwise = MM.throwSystemError $
          "module" <+> squotes (pretty childMV) <+> "is not in the eval allow-list"
