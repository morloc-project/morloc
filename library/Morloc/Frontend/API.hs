{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.API
Description : Entry point for the frontend pipeline (parse, typecheck, valuecheck)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Orchestrates the full frontend pipeline: parsing source files into a module
DAG, recursively resolving imports, and re-exporting the typechecker and
valuechecker entry points. This is the primary interface consumed by the
top-level compiler driver ('Morloc').
-}
module Morloc.Frontend.API
  ( parse
  , Parser.readType
  , Typecheck.typecheck
  , Typecheck.resolveTypes
  , Valuecheck.valuecheck
  ) where

import qualified Control.Monad.State.Strict as State
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.Config as Config
import qualified Morloc.Data.DAG as MDD
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Frontend.Desugar as Desugar
import Morloc.Frontend.Namespace
import Morloc.Frontend.Parser (PState (..), emptyPState)
import qualified Morloc.Frontend.Parser as Parser
import qualified Morloc.Frontend.Typecheck as Typecheck
import qualified Morloc.Frontend.Valuecheck as Valuecheck
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.ProgramBuilder.Install as Install
import qualified Morloc.System as MS
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

{- | Parse a morloc source file and all its imports into a module DAG.
Recursively discovers and parses imported modules.
-}
parse ::
  -- | path to the current module (if we are reading from a file)
  Maybe Path ->
  -- | code of the current module
  Code ->
  MorlocMonad (DAG MVar Import ExprI)
parse f (Code code) = do
  moduleConfig <- Config.loadModuleConfig f
  langMap <- buildLangMap'

  -- The main module's @log-template@ becomes the program-wide default
  -- log message template. Per-label overrides win over this; the
  -- built-in defaults in 'Morloc.CodeGenerator.LogTemplate' fill any
  -- subfield neither this nor the per-label config supplies.
  MM.modify (\st -> st {stateLogTemplate = moduleConfigLogTemplate moduleConfig})

  -- The main module's run-scope @prologue@ / @epilogue@ templates are
  -- rendered by the nexus at run boundaries. Sub-modules cannot
  -- contribute to these (a workflow has exactly one entry-point, so
  -- run-scope events have exactly one source of truth).
  MM.modify
    ( \st -> st
        { stateRunLog =
            case (moduleConfigPrologue moduleConfig, moduleConfigEpilogue moduleConfig) of
              (Nothing, Nothing) -> Nothing
              (mp, me) ->
                Just
                  RunLogTemplate
                    { runLogPrologue = mp
                    , runLogEpilogue = me
                    }
        }
    )

  -- Compute project root from entry-point file path
  let projectRoot = fmap MS.takeDirectory f

  -- Resolve the main module's @hash-include@ patterns into a concrete
  -- list of files. Their contents will be folded into every pool's
  -- cache hash so that editing a foreign source file (or a runtime
  -- data file the user explicitly lists) invalidates the cache. Scope
  -- validation rejects absolute paths and @..@ traversals; the same
  -- contract as @packageInclude@.
  case moduleConfigHashInclude moduleConfig of
    Just patterns | not (null patterns) -> do
      MM.liftIO $ Install.validateIncludeScope patterns
      case projectRoot of
        Just root -> do
          paths <- MM.liftIO $ Install.resolveIncludePatterns root patterns
          MM.modify (\st -> st {stateHashIncludePaths = paths})
        Nothing -> return ()
    _ -> return ()

  let parserState =
        emptyPState
          { psModuleConfig = moduleConfig
          , psLangMap = langMap
          , psProjectRoot = projectRoot
          }

  -- store source text, project root, and load package metadata for the main file
  case f of
    Just path -> do
      MM.modify (\st -> st
        { stateSourceText = Map.insert path code (stateSourceText st)
        , stateProjectRoot = projectRoot
        })
      Mod.loadModuleMetadata path
    Nothing -> return ()

  case Parser.readProgram Nothing f code parserState mempty of
    (Left e) -> MM.throwSystemError $ pretty e
    (Right (mainDag, mainState)) -> do
      -- capture module-level docs from the main module before imports overwrite them
      MM.modify (\st -> st
        { stateModuleDoc = psModuleDoc mainState
        , stateModuleEpilogues = psModuleEpilogues mainState
        })
      parseImports mainDag mainState Map.empty
  where
    -- descend recursively into imports
    parseImports ::
      DAG MVar Import ExprI ->
      PState ->
      Map.Map MVar Path ->
      MorlocMonad (DAG MVar Import ExprI)
    parseImports d s m = case unimported of
      [] -> do
        -- transfer source positions from parser state into MorlocState
        MM.modify (\st -> st
          { stateSourceMap = psSourceMap s <> stateSourceMap st
          , stateTermDocs = psTermDocs s <> stateTermDocs st
          })
        -- emit any docstring warnings accumulated during desugar
        case psWarnings s of
          [] -> return ()
          ws -> MM.tell ws
        -- Synthesize `--' with:` terminal-action commands and expand @collect
        -- now that the whole import DAG is available, so offset/IFile handler
        -- detection can see signatures imported from other modules.
        finalizeCollectActions d
      ((mainModule, importedModule) : _) -> do
        when (mainModule == importedModule) . MM.throwSystemError $
          "Module" <+> pretty importedModule <+> "imports itself"
        importPath <- case Map.lookup mainModule m of
          (Just mainPath) -> Mod.findModule (Just mainPath, mainModule) importedModule
          Nothing -> Mod.findModule (Nothing, mainModule) importedModule

        -- Load the <main>.yaml file associated with the main morloc package file
        moduleConfig <- Config.loadModuleConfig (Just importPath)
        let newState = s {psModuleConfig = moduleConfig}

        Mod.loadModuleMetadata importPath
        (childPath, code') <- openLocalModule importPath
        case Parser.readProgram (Just importedModule) childPath code' newState d of
          (Left e) -> MM.throwSystemError $ pretty e
          (Right (d', s')) ->
            -- The parsed module may have a different internal name than the
            -- import edge target (e.g., file declares "module units" but
            -- import edge targets ".units"). Reconcile by renaming the DAG
            -- entry to match the import name.
            let d'' = reconcileModuleName importedModule d d'
            in parseImports d'' s' (maybe m (\v -> Map.insert importedModule v m) childPath)
      where
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- find all (module to module) edges in the graph where the imported
        -- module has not yet been parsed
        unimported = filter (\(_, importMod) -> not (Set.member importMod parsed)) (MDD.edgelist d)

    -- If readProgram added a key that doesn't match importedModule, rename it.
    -- This happens when a local import (".units") parses a file that declares
    -- "module units (...)". We rename the DAG key and the ModE name to match
    -- the import edge target.
    reconcileModuleName :: MVar -> DAG MVar Import ExprI -> DAG MVar Import ExprI -> DAG MVar Import ExprI
    reconcileModuleName importName dOld dNew =
      case newKeys of
        [actualName] | actualName /= importName ->
          -- Rename: delete the old key, insert under the import name with
          -- the ModE name rewritten to match
          case Map.lookup actualName dNew of
            Just (ExprI i (ModE _ es), edges) ->
              let renamed = Map.delete actualName dNew
              in Map.insert importName (ExprI i (ModE importName es), edges) renamed
            _ -> dNew  -- shouldn't happen
        _ -> dNew  -- zero or multiple new keys: nothing to reconcile
      where
        newKeys = filter (`Map.notMember` dOld) (Map.keys dNew)

-- | Post-parse pass over the full module DAG: for every module, synthesize
-- its `--' with:` terminal-action commands and expand @collect nodes. This
-- runs here (rather than in per-module desugar) because offset (@U64 -> ...@)
-- and IFile handler detection reads the handler's signature, which may be
-- imported from another module -- unavailable until the whole DAG is parsed.
--
-- A single 'Desugar.DState' is threaded across modules so synthesized nodes
-- get globally-unique indices (seeded past every existing index) and their
-- source locations accumulate into one map. Reserved-prefix / flag-collision
-- errors from the synthesis surface without a source caret (the rare cost of
-- deferring past the per-module parse context); their messages are explicit.
finalizeCollectActions :: DAG MVar Import ExprI -> MorlocMonad (DAG MVar Import ExprI)
finalizeCollectActions dag = do
  srcMap0 <- MM.gets stateSourceMap
  let visibleSigs = moduleVisibleSigs dag
      idx0 = maximum (0 : map ((+ 1) . AST.maxIndex) (MDD.nodes dag))
      ds0 = mkFinalizeDState idx0 srcMap0
      -- 'D' threads the shared DState (fresh indices, accumulated source
      -- locations) left-to-right through the module list, so a single
      -- 'runStateT' over 'mapM' does the plumbing.
      finalizeModule (m, (node, edges)) = do
        let imported = Map.findWithDefault Map.empty m visibleSigs
        node' <- Desugar.injectTerminalActionsWithSigs imported node
                   >>= Desugar.expandCollectE
        return (m, (node', edges))
  case State.runStateT (mapM finalizeModule (Map.toList dag)) ds0 of
    Left err ->
      MM.throwSystemError . pretty $
        Desugar.showParseError "<terminal-action synthesis>" err
    Right (entries, dsFinal) -> do
      MM.modify (\st -> st {stateSourceMap = Desugar.dsSourceMap dsFinal})
      case Desugar.dsWarnings dsFinal of
        [] -> return ()
        ws -> MM.tell ws
      return (Map.fromList entries)

-- | Construct a minimal 'Desugar.DState' for the post-parse synthesis pass.
-- Only the index counter, source map, and warning accumulator carry live
-- data; the remaining fields are unused by terminal-action / @collect
-- synthesis and are left empty.
mkFinalizeDState :: Int -> Map.Map Int SrcLoc -> Desugar.DState
mkFinalizeDState idx srcMap = Desugar.DState
  { Desugar.dsExpIndex = idx
  , Desugar.dsSourceMap = srcMap
  , Desugar.dsDocMap = Map.empty
  , Desugar.dsModulePath = Nothing
  , Desugar.dsModuleConfig = defaultValue
  , Desugar.dsSourceLines = []
  , Desugar.dsLangMap = Map.empty
  , Desugar.dsProjectRoot = Nothing
  , Desugar.dsTermDocs = Map.empty
  , Desugar.dsWarnings = []
  , Desugar.dsModuleDoc = []
  , Desugar.dsModuleEpilogues = []
  }

-- | For each module, the term signatures visible to it: its own top-level
-- signatures plus, transitively through imports (honoring include / exclude /
-- alias, and skipping qualified imports whose terms are not in bare scope),
-- the exported signatures of imported modules.
--
-- Computed by the shared memoized bottom-up DAG fold 'DAG.synthesizeNodes'
-- (each module resolved once, not once-per-import-path). Each node yields a
-- @(visible, exported)@ pair; importers consume a child's @exported@ subset.
-- A module-import cycle stalls the fold ('Nothing'); such a program is
-- rejected by 'resolveImports' immediately after this pass, so degrading to
-- local-only visibility here is harmless.
moduleVisibleSigs :: DAG MVar Import ExprI -> Map.Map MVar (Map.Map EVar TypeU)
moduleVisibleSigs dag =
  case runIdentity (MDD.synthesizeNodes resolve dag) of
    Just resolved -> Map.map (fst . fst) resolved
    Nothing -> Map.empty
  where
    resolve
      :: MVar
      -> ExprI
      -> [(MVar, Import, (Map.Map EVar TypeU, Map.Map EVar TypeU))]
      -> Identity (Map.Map EVar TypeU, Map.Map EVar TypeU)
    resolve _ node children =
      let visible = Map.union
            (localSigs node)
            (Map.unions [ applyImport imp exported | (_, imp, (_, exported)) <- children ])
          exported = case AST.findExport node of
            ExportAll -> visible
            _ ->
              let names = AST.findExportSet node
               in Map.filterWithKey (\k _ -> Set.member (TermSymbol k) names) visible
       in pure (visible, exported)

    localSigs :: ExprI -> Map.Map EVar TypeU
    localSigs node = Map.fromList [ (v, etype et) | (v, _, et) <- AST.findSignatures node ]

    -- Apply an import edge to a child's exported sigs: rename by alias /
    -- restrict by include list, drop excluded terms. Qualified imports
    -- (namespace) bring no bare names.
    applyImport :: Import -> Map.Map EVar TypeU -> Map.Map EVar TypeU
    applyImport (Import _ include excludes namespace) srcExports
      | isJust namespace = Map.empty
      | otherwise =
          let base = case include of
                Nothing -> srcExports
                Just syms -> Map.fromList
                  [ (alias, sig)
                  | AliasedTerm orig alias <- syms
                  , Just sig <- [Map.lookup orig srcExports]
                  ]
              excluded = Set.fromList [ e | TermSymbol e <- excludes ]
           in Map.filterWithKey (\k _ -> not (Set.member k excluded)) base

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  MM.modify (\st -> st {stateSourceText = Map.insert filename code (stateSourceText st)})
  return (Just filename, code)

{- | Build a map from language aliases to Lang values, combining the
registry (built-in languages) with filesystem-discovered plugins.
-}
buildLangMap' :: MorlocMonad (Map.Map T.Text Lang)
buildLangMap' = do
  -- Get the registry-based lang map (all built-in languages)
  reg <- MM.gets stateLangRegistry
  let registryMap = LR.buildLangMap reg

  -- Discover additional plugin languages on the filesystem
  home <- MM.asks configHome
  let langDir = home </> "lang"
  exists <- liftIO $ doesDirectoryExist langDir
  pluginMap <-
    if not exists
      then return Map.empty
      else do
        dirs <- liftIO $ listDirectory langDir
        results <- liftIO $ mapM (scanLangDir langDir) dirs
        return $ Map.fromList [(n, lang) | Just (n, lang) <- results]

  -- Registry entries take precedence over filesystem discoveries
  return $ Map.union registryMap pluginMap
  where
    scanLangDir :: FilePath -> String -> IO (Maybe (T.Text, Lang))
    scanLangDir langDir dirName = do
      let descPath = langDir </> dirName </> "lang.yaml"
      hasDesc <- doesFileExist descPath
      if not hasDesc
        then return Nothing
        else do
          result <- LR.parseLangYamlFile descPath
          case result of
            Left _ -> return Nothing
            Right (name, ext) -> return $ Just (name, Lang name ext)
