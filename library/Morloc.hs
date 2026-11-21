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
  , emitPoolScripts
  , typecheck
  , typecheckFrontend
  , typecheckModule
  , generatePools
  ) where

import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.Namespace.Type

import Morloc.Data.Doc (pretty, render, (<+>), squotes)
import Morloc.Frontend.Typecheck (isPattern)
import Morloc.Typecheck.Internal (mapTypeUChildren, prettyTypeU)
import qualified Morloc.BaseTypes as BT
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Morloc.Data.Text as MT
import qualified Morloc.Build.Params as BP
import qualified Data.Set as Set

import Morloc.CodeGenerator.Docstrings (processDocstrings)
import Morloc.CodeGenerator.EffectBoundary (checkEffectBoundaries, insertEffectBoundaries)
import Morloc.CodeGenerator.Emit (TranslateFn, checkManifoldIds, emit, pool)
import Morloc.CodeGenerator.StaticArgs (specializeStaticArgs)
import Morloc.CodeGenerator.Express (express, addCacheWraps, addDebugWraps, addLoopWraps, addNativeRecEntries, etaReduceForwarders, bareFoldSteps)
import Morloc.CodeGenerator.LambdaEval (applyNexusLambdas, applyPoolLambdas)
import Morloc.CodeGenerator.Namespace (Arg, PolyHead (..), SerialManifold)
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Morloc.CodeGenerator.SystemConfig as SC
import Morloc.CodeGenerator.Parameterize (parameterize)
import Morloc.CodeGenerator.Guest.Pass (lowerGuests)
import Morloc.CodeGenerator.Realize (placeNormalized, realityCheck)
import qualified Morloc.CodeGenerator.Infer as Infer
import Morloc.CodeGenerator.Segment (checkScopes, segment)
import Morloc.CodeGenerator.Serial (checkPackerCoherence)
import Morloc.CodeGenerator.Reduce (reduce)
import Morloc.CodeGenerator.Serialize (headsById, serialize)
import Morloc.CodeGenerator.Suspension (lowerSuspensions)
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Frontend.API as F
import Morloc.Frontend.Labels (checkLabels)
import Morloc.Frontend.AutoRequire (autoRequire)
import qualified Morloc.Frontend.AST as AST
import Morloc.Frontend.Restructure (restructure)
import Morloc.Frontend.Link (checkRsize)
import Morloc.Frontend.PatternCheck (checkPatterns)
import Morloc.Frontend.Treeify (contractTrees, treeify, Validation (..))
import Morloc.Frontend.Share (shareBindings)
import Morloc.Frontend.Generalize (generalize)
import Morloc.Frontend.Specialize (isGenericExport, specialize)
import qualified Morloc.Data.PoolHash as PoolHash
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Version
import qualified Morloc.Monad as MM
import Control.Monad.Except (catchError, throwError)
import Morloc.ProgramBuilder.Build (buildProgram, discardStagingDir, withStagingCleanup)

-- | Typecheck an installed module's entry file, giving each exported name
-- with its type. The state is left as it was. A rejection comes back with
-- its location already rendered, since the state that locates it is gone.
typecheckModule :: Path -> MorlocMonad [(T.Text, T.Text)]
typecheckModule mainFile = do
  code <- liftIO $ MT.readFile mainFile
  savedState <- MM.get
  r <- (Right <$> typecheckFrontend (Just mainFile) (Code code)) `catchError` (return . Left)
  st <- MM.get
  MM.put savedState
  case r of
    Left e@(Rejected _ _)
      | not (MM.isDefect st e) -> MM.throwRejected (MM.makeMorlocError st e)
    Left e@(EnvFailed _) -> throwError e
    Left e -> throwError (Fault Nothing (MM.makeMorlocError st e))
    Right xs ->
      return
        [ (render (pretty v), render (prettyTypeU t))
        | AnnoS (Idx i t) _ _ <- xs
        , Just v <- [Map.lookup i (stateName st)]
        ]

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
    (r : _) -> MM.modify' (\s -> s {stateModuleName = Just r})
    _ -> return ()
  (treeified, exports) <- restructure dag >>= treeify
  checkRsize
  -- CLS-20: a type's Packable instances agree on its wire form, so the
  -- instance a use selects may fix the wire type.
  checkPackerCoherence
  checks <- generalize treeified
  -- Collecting copies index-keyed state (labels, annotations); the trees are
  -- discarded, so the state is restored.
  beforeValidation <- MM.get
  contractTrees (validationInferred checks) (validationModules checks) >>= F.validate
  MM.put beforeValidation
  es <- specialize checks exports
  specNames <- MM.gets stateSpecNames
  Specs specTrees <- MM.gets stateSpecs
  GMap idmap _ <- MM.gets stateSignatures
  owners <- MM.gets stateWhereOwner
  sites <- MM.gets stateDefinitionSite
  mapM_ (F.checkRecursion specNames (`Map.lookup` idmap) owners sites) (es <> specTrees)
  checkPatterns (es <> specTrees)
  MM.modify' (\s -> s {stateExportTypes = Map.fromList [(i, t) | AnnoS (Idx i t) _ _ <- es]})
  return es

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
    >>= skipGenericExports
    -- the shared specializations the exports call are trees of their own
    >>= (\es -> MM.gets stateSpecs >>= \(Specs specs) -> return (es <> specs))
    -- a streaming command's `Collect a` is unit from here on (STRM-7)
    >>= eraseCollect
    -- lower guest-language sources (e.g. Futhark) into host glue
    >>= lowerGuests
    -- resolve all TypeU types to Type
    |>> map F.resolveTypes
    >>= mapM checkLabels
    >>= (\es -> Infer.recordUndeterminedSites es >> return es)
    -- fail fast on a discarded Try (types are concrete here, instances
    -- still Many, so one insertion covers every realization)
    >>= mapM autoRequire
    -- check for value contradictions between implementations
    >>= mapM F.valuecheck
    -- evaluate each named value once, where its uses need it
    >>= mapM shareBindings
    >>= realityCheck

-- | Replace every @Collect a@ with @()@ and @closeCollect with @close, in
-- the trees and in the export types codegen reads. What a command streams
-- was recorded from its signature before typechecking ('stateStreamElems').
eraseCollect :: [AnnoS (Indexed TypeU) Many Int] -> MorlocMonad [AnnoS (Indexed TypeU) Many Int]
eraseCollect es = do
  MM.modify' (\s -> s {stateExportTypes = Map.map eraseU (stateExportTypes s)})
  return (map eraseTree es)
  where
    eraseTree (AnnoS (Idx i t) c e) = AnnoS (Idx i (eraseU t)) c (eraseE (mapExprS eraseTree e))
    eraseE (IntrinsicS IntrCloseCollect xs) = IntrinsicS IntrClose xs
    eraseE x = x
    eraseU (AppU (VarU v) [_]) | v == BT.collectVar = BT.unitU
    eraseU t = mapTypeUChildren eraseU t

-- | Set aside the exports whose type keeps a type variable: they are not
-- commands and are never realized (SIG-5, CMD-2). Each is warned about and
-- recorded, so a use of one that needs it realized can name it. The
-- alternatives each export gives directly are still compared (REAL-5); its
-- references are left as their signatures, so what they reach is not.
skipGenericExports :: [AnnoS (Indexed TypeU) Many Int] -> MorlocMonad [AnnoS (Indexed TypeU) Many Int]
skipGenericExports es = do
  let (generic, concrete) = partition (\(AnnoS (Idx _ t) _ _) -> isGenericExport t) es
  mapM_ refusePatternExport [(i, t) | AnnoS (Idx i t) _ _ <- generic, isPattern t]
  mapM_ (F.valuecheck . F.resolveTypes) generic
  mapM_ warnSkippedExport [i | AnnoS (Idx i _) _ _ <- generic]
  recordSkippedGeneric [(i, typeOf t) | AnnoS (Idx i t) _ _ <- generic]
  return concrete

-- | POLY-9: a pattern the root module defines is not a command. One it
-- imports and exports is skipped as any generic export is.
refusePatternExport :: (Int, TypeU) -> MorlocMonad ()
refusePatternExport (i, t) = do
  name <- MM.metaName i
  defined <- MM.gets stateRootUnsigned
  case name of
    Just v@(EV n)
      | Set.member v defined ->
          MM.throwSourcedError i $
            squotes (pretty n) <+> "is a pattern, of type" <+> prettyTypeU t
              <> ", so it cannot be a command: give it a signature that names its tuple or record, or apply it to"
              <+> "data with 'morloc-nexus view --pattern'"
    _ -> return ()

warnSkippedExport :: Int -> MorlocMonad ()
warnSkippedExport i = do
  name <- MM.metaName i
  case name of
    Just (EV n) -> MM.warnAt i $ "skipping generic export" <+> squotes (pretty n) <> ": its type keeps a type variable, so it is not a command"
    Nothing -> return ()

-- | Record trees left unrealized because their type is generic, keeping the
-- first entry for a name.
recordSkippedGeneric :: [(Int, Type)] -> MorlocMonad ()
recordSkippedGeneric ts = do
  entries <- catMaybes <$> mapM (\(i, t) -> fmap (\v -> (v, (i, t))) <$> MM.metaName i) ts
  MM.modify' (\st -> st {stateSkippedGeneric = Map.union (stateSkippedGeneric st) (Map.fromListWith (\_ old -> old) entries)})

-- | Do everything except language specific code generation.
--
-- Runs 'applyPoolLambdas' first, matching the pool path in 'writeProgram':
-- the 'express' pipeline assumes lambdas have been reduced/lifted, so skipping
-- it lets a lambda reach 'expressPolyApp' as an un-handled head ("unexpected
-- LamS"). A multiply-used lambda is kept as a shared native closure (rASTs
-- become pools, not the pure nexus evaluator).
generatePools :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad [(Lang, [SerialManifold])]
generatePools rASTs = mapM applyPoolLambdas rASTs >>= specializeStaticArgs >>= placeNormalized >>= lowerPools

-- | Lower realized pool trees to per-language serial manifolds. This is the
-- whole path from parameterization to pool assembly, shared by 'writeProgram'
-- and 'generatePools' so what @morloc dump@ shows is what gets built.
lowerPools :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad [(Lang, [SerialManifold])]
lowerPools rASTs = do
  paramRASTs <- mapM parameterize rASTs
  let langMap = Map.fromList
        [(midx, lang) | AnnoS (Idx midx _) (Idx _ lang, _) _ <- paramRASTs]
  MM.modify' (\s -> s { stateManifoldLang = langMap })
  reg <- MM.gets stateLangRegistry
  mapM (\r -> expressRoot r >>= checkHeadLang langMap) paramRASTs
    -- Wrap each cache:true manifold's body in a 'PolyCacheBody'.
    >>= mapM addCacheWraps
    -- When 'stateDebugTrace' (--debug), wrap every foreign-call
    -- manifold body in 'PolyDebugWrap'. No-op when the flag is off.
    >>= mapM addDebugWraps
    -- Lower eligible tail-recursive helpers to native 'PolyLoop's.
    >>= mapM addLoopWraps
    -- Give a recursion its own pool reaches a native entry point.
    >>= addNativeRecEntries
    -- Boundary reconciliation + invariant check. Insertion installs
    -- 'PolyEval' / 'PolyDoBlock' at every boundary whose declared type
    -- disagrees with its calling convention; the checker asserts the
    -- resulting tree satisfies the invariant. A checker failure means
    -- 'EffectBoundary' is missing a table row.
    >>= mapM (\ph -> do
                 ph' <- insertEffectBoundaries ph
                 checkEffectBoundaries ph'
                 lowerSuspensions (bareFoldSteps (etaReduceForwarders ph')))
    >>= mapM segment |>> concat
    -- every variable a generated manifold refers to is bound in it
    >>= mapM checkScopes
    >>= (\hs -> mapM (serialize (headsById hs)) hs)
    >>= mapM reduce
    >>= (\ms -> checkManifoldIds reg ms >> return (pool reg ms))

-- | Callers of a root read its language from 'stateManifoldLang', so its head
-- must be built there.
checkHeadLang :: Map.Map Int Lang -> PolyHead -> MorlocMonad PolyHead
checkHeadLang langMap h@(PolyHead lang midx _ _)
  | Map.lookup midx langMap == Just lang = return h
  | otherwise =
      MM.throwCompilerBug $
        "the manifold of root" <+> pretty midx <+> "is built in" <+> pretty lang
          <+> "but its tree is realized in" <+> pretty (Map.lookup midx langMap)

-- | Express a root. A root of function type is called with every input of
-- its type (the nexus sends a command's arguments by its type), so its
-- manifold must take exactly those; anything else would read a closure as
-- a result.
expressRoot :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressRoot e@(AnnoS (Idx midx t) _ _) = do
  h@(PolyHead _ _ args _) <- express e
  case t of
    FunT ts _
      | length args /= length ts ->
          MM.throwCompilerBug $
            "the manifold of root" <+> pretty midx <+> "takes" <+> pretty (length args)
              <+> "arguments, but its type" <+> squotes (pretty t) <+> "has" <+> pretty (length ts)
    _ -> return h

-- | Build a program as a local executable
writeProgram ::
  -- | language-specific translator callback
  TranslateFn ->
  -- | source code filename (for debugging messages)
  Maybe Path ->
  -- | source code text
  Code ->
  MorlocMonad ()
writeProgram translateFn path code =
  -- Guest lowering lands artifacts in the build's staging tree before the
  -- program builder runs, so a failure anywhere in between must discard that
  -- tree or an aborted build leaves a .tmp.<pid> directory beside the program.
  withStagingCleanup $ do
    (nexus, envspec, wrappers, pools) <- emitProgram translateFn path code
    buildEmitted nexus envspec wrappers pools

-- | The pool scripts of a program, generated but not built: what
-- @morloc types@ takes the generated type files from (GEN-13).
emitPoolScripts :: TranslateFn -> Maybe Path -> Code -> MorlocMonad [Script]
emitPoolScripts translateFn path code = withStagingCleanup $ do
  (_, _, _, pools) <- emitProgram translateFn path code
  discardStagingDir
  return pools

-- | Record the prefix of a managed build's environment, refusing a managed
-- build whose prefix is unusable (PKG-13).
resolveEnvPrefix :: MorlocMonad ()
resolveEnvPrefix =
  MM.liftIO SC.managedPrefix
    >>= either (MM.throwFailed . pretty) (\p -> MM.modify' (\s -> s {stateEnvPrefix = p}))

-- | Typecheck, realize and emit the nexus and every pool.
emitProgram :: TranslateFn -> Maybe Path -> Code -> MorlocMonad (Script, Script, [WrapperFile], [Script])
emitProgram translateFn path code =
  resolveEnvPrefix >> typecheck path code
    -- Evaluate applied lambdas. gASTs run in the pure nexus evaluator, which
    -- cannot hold a native closure, so let-bound lambdas are always inlined
    -- there; rASTs become pools, where a multiply-used lambda is kept as a
    -- shared native closure to avoid exponential inlining.
    >>= bimapM (mapM applyNexusLambdas) (mapM applyPoolLambdas)
    -- Give a recursive helper its own copy for each closed function it is
    -- passed at a position every recursion passes through unchanged, so the
    -- function is referenced directly rather than carried as a closure value.
    -- It creates trees, so it runs before the nexus sees the pool trees and
    -- before the counter is reused for code generation.
    >>= (\(g, r) -> (,) g <$> specializeStaticArgs r)
    -- Place languages again on the normalized pool trees.
    >>= (\(g, r) -> (,) g <$> placeNormalized r)
    -- process docstrings to determine how to build CLI
    >>= bimapM (mapM processDocstrings) (mapM processDocstrings)
    -- generate nexus and pools
    >>= \(gASTs, rASTs) ->
      do
        -- A tree whose type keeps a type variable has no command form. The
        -- generic exports were set aside before realization (SIG-5); a
        -- generic tree here is recorded too, so that a use of it can be
        -- reported as undetermined.
        exportIds <- Set.fromList <$> MM.gets stateExports
        let isConcreteExport (AnnoS (Idx _ t) _ _, _) = not (containsUnk t)
            (concreteGASTs, genericGASTs) = partition isConcreteExport gASTs
            (concreteRASTs, genericRASTs) = partition isConcreteExport rASTs
            generic =
              [(i, t) | (AnnoS (Idx i t) _ _, _) <- genericGASTs]
                <> [(i, t) | (AnnoS (Idx i t) _ _, _) <- genericRASTs]
        mapM_ warnSkippedExport (nubOrd [i | (i, _) <- generic, Set.member i exportIds])
        recordSkippedGeneric generic
        -- Only exports become commands, but helper rASTs also flow to
        -- 'Nexus.generate' so it can see their languages for pool
        -- enumeration -- the export tree ends at a bare 'CallS' back-edge
        -- after 'extractRecursiveHelpers' and hides them otherwise.
        exports <- MM.gets stateExports
        let exportSet = Set.fromList exports
            isExported (AnnoS (Idx midx _) _ _, _) = Set.member midx exportSet
            exportedRASTs = filter isExported concreteRASTs
            helperRASTs = map fst (filter (not . isExported) concreteRASTs)
        (nexus, envspec, wrappers) <- Nexus.generate concreteGASTs exportedRASTs helperRASTs
        MM.startCounter
        pools <-
          lowerPools (map fst concreteRASTs)
            >>= mapM (uncurry (emit translateFn))
        return (nexus, envspec, wrappers, pools)

-- | Hash the emitted pools into the manifest, then write and build them.
buildEmitted :: Script -> Script -> [WrapperFile] -> [Script] -> MorlocMonad ()
buildEmitted nexus envspec wrappers pools = do
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
        guestObjects <- MM.gets stateGuestObjects
        objHash <- MM.liftIO $ PoolHash.hashFiles guestObjects
        -- How native pools are compiled and linked, and the environment every
        -- pool runs in, change results without changing any pool source.
        native <-
          MM.gets $ \s ->
            let metas = statePackageMeta s
             in ( concatMap packageCxxFlags metas
                , concatMap (concat . Map.elems . packageCppLibs) metas
                , stateGuestLinkFlags s
                , stateEnvPrefix s
                )
        let objSalt
              | null guestObjects = BP.renderSalt params
              | otherwise = BP.renderSalt params <> "|obj:" <> PoolHash.poolHashHex objHash
            buildSalt = case native of
              ([], [], [], Nothing) -> objSalt
              _ -> objSalt <> "|native:" <> T.pack (show native)
        sources <- MM.gets (concat . GMap.elems . stateSources)
        let sourcedFiles lang = Set.toAscList (Set.fromList [p | s <- sources, srcLang s == lang, Just p <- [srcPath s]])
            salt = buildSalt <> "|morloc:" <> T.pack Morloc.Version.versionStr
        poolHashes <- MM.liftIO $ PoolHash.computePoolHashes salt hashIncludes sourcedFiles pools
        let nexusPatched = PoolHash.patchManifestPoolHashes poolHashes nexus
        -- The envspec Script carries no make commands; it is written to the
        -- build dir alongside manifest.json and the pool sources.
        buildProgram (nexusPatched, wrappers, envspec : pools)

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
    checkExpr _ (ExprI i (IstE Declared _ _ _ _)) =
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
      | otherwise = MM.throwRejected $
          "module" <+> squotes (pretty childMV) <+> "is not in the eval allow-list"
