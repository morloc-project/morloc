{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Namespace
Description : Re-exports of core namespace types plus frontend-specific helpers
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Aggregates all core namespace modules ('Prim', 'Type', 'Expr', 'State') into
a single import for frontend code, and provides state-index copying
('copyState').
-}
module Morloc.Frontend.Namespace
  ( module Morloc.Namespace.Prim
  , module Morloc.Namespace.Type
  , module Morloc.Namespace.Expr
  , module Morloc.Namespace.State
  , isGeneric
  , copyState
  , newIndex
  ) where

import Data.Text (Text)
import Morloc.Data.Doc (pretty)
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.Namespace.Type

-- | Determine if a type term is generic
isGeneric :: Text -> Bool
isGeneric = isTypeVariableName

{- | Copy all index-keyed state entries from @oldIndex@ to @newIndex@.
Used when a module is re-indexed (e.g., after merging duplicate imports).
-}
copyState :: Int -> Int -> MorlocMonad ()
copyState oldIdx newIdx = do
  s <- MM.get

  -- Could be defined more succinctly, but it is IMPERATIVE that every index
  -- is copied. Listing all fields will ensure that an error is raised if a
  -- new MorlocState field is added but included in this function.
  MM.put $
    MorlocState
      { statePackageMeta = statePackageMeta s
      , stateVerbosity = stateVerbosity s
      , stateCounter = stateCounter s
      , stateManifoldCounter = stateManifoldCounter s
      , stateDepth = stateDepth s
      , stateSignatures = updateGMap (stateSignatures s)
      , stateTypeclasses = stateTypeclasses s
      , stateInstances = stateInstances s
      , stateConcreteTypedefs = stateConcreteTypedefs s
      , stateGeneralTypedefs = stateGeneralTypedefs s
      , stateNewtypeHeaders = stateNewtypeHeaders s
      , stateExportTypes = stateExportTypes s
      , stateSources = updateGMap (stateSources s)
      , stateAnnotations = updateMap (stateAnnotations s)
      , stateOutfile = stateOutfile s
      , stateExports = updateList (stateExports s)
      , stateName = updateMap (stateName s)
      , stateTermDocs = stateTermDocs s
      , stateStreamElems = stateStreamElems s
      , stateModuleCommands = stateModuleCommands s
      , stateParseSlots = stateParseSlots s
      , stateReplayPlans = stateReplayPlans s
      , stateErrorNotes = updateMap (stateErrorNotes s)
      , stateManifoldConfig = updateMap (stateManifoldConfig s)
      , stateLogTemplate = stateLogTemplate s
      , stateBenchTemplate = stateBenchTemplate s
      , stateRunLog = stateRunLog s
      , stateHashIncludePaths = stateHashIncludePaths s
      , stateSourceMap = updateMap (stateSourceMap s)
      , stateSourceText = stateSourceText s
      , stateBuildConfig = stateBuildConfig s
      , stateLangParams = stateLangParams s
      , stateModuleName = stateModuleName s
      , stateInstall = stateInstall s
      , stateInstallForce = stateInstallForce s
      , stateInstallDir = stateInstallDir s
      , stateBuildRoot = stateBuildRoot s
      , stateStagingDir = stateStagingDir s
      , stateProgramKey = stateProgramKey s
      , stateWrapperSpecs = stateWrapperSpecs s
      , stateRebuild = stateRebuild s
      , stateBuildParentDir = stateBuildParentDir s
      , stateClassDefs = stateClassDefs s
      , stateEffects = stateEffects s
      , stateLangRegistry = stateLangRegistry s
      , stateExportGroups = stateExportGroups s
      , stateManifoldLang = stateManifoldLang s
      , stateNativeRecEntries = stateNativeRecEntries s
      , stateRecursionTargets = stateRecursionTargets s
      , stateWhereOwner = stateWhereOwner s
      , stateMatchSites = stateMatchSites s
      , stateConstantOrigin = updateMap (stateConstantOrigin s)
      , stateHoldCache = stateHoldCache s
      , stateUndeterminedSites = stateUndeterminedSites s
      , stateSkippedGeneric = stateSkippedGeneric s
      , stateStageEntries = stateStageEntries s
      , stateStageContext = stateStageContext s
      , stateRecStages = stateRecStages s
      , stateSpecs = stateSpecs s
      , stateSpecNames = stateSpecNames s
      , statePlacement = statePlacement s
      , stateNamedGasts = stateNamedGasts s
      , stateHostFunctionVars = stateHostFunctionVars s
      , stateHostFunctionArgs = stateHostFunctionArgs s
      , stateHostOriginClosures = stateHostOriginClosures s
      , stateArgTypes = stateArgTypes s
      , stateManifoldEffects = stateManifoldEffects s
      , stateDefinitionSite = stateDefinitionSite s
      , stateNameSource = stateNameSource s
      , stateDebugCallee = stateDebugCallee s
      , stateProjectRoot = stateProjectRoot s
      , stateEnvSpecLangs = stateEnvSpecLangs s
      , stateEvalMode = stateEvalMode s
      , stateAllowLocalModules = stateAllowLocalModules s
      , stateAutoInstall = stateAutoInstall s
      , stateSnapshot = stateSnapshot s
      , stateEvalSandbox = stateEvalSandbox s
      , stateUnsafeSkipNullCheck = stateUnsafeSkipNullCheck s
      , stateInlineSize = stateInlineSize s
      , stateNoShm = stateNoShm s
      , stateTmpdir = stateTmpdir s
      , stateDebugTrace = stateDebugTrace s
      , stateModuleDoc = stateModuleDoc s
      , stateModuleEpilogues = stateModuleEpilogues s
      , stateVariantAncestors = stateVariantAncestors s
      , stateVariantArgsResolving = stateVariantArgsResolving s
      }
  where
    updateGMap g = case GMap.yIsX oldIdx newIdx g of
      (Just g') -> g'
      Nothing -> g

    updateMap m = case Map.lookup oldIdx m of
      (Just x) -> Map.insert newIdx x m
      Nothing -> m

    updateList xs = if oldIdx `elem` xs then newIdx : xs else xs

-- | Allocate a fresh index that inherits all index-keyed state from @parent@.
-- FIXME: when line-number linking is added, this map must be updated too and
-- the trace should be recorded.
newIndex :: Int -> MorlocMonad Int
newIndex parent = do
  i <- MM.getCounter
  copyState parent i
  MM.sayVVV $ "Set indices " <> pretty parent <> " = " <> pretty i
  return i
