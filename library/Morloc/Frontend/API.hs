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
  , Typecheck.validate
  , Typecheck.resolveTypes
  , Valuecheck.valuecheck
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.Config as Config
import qualified Morloc.Data.DAG as MDD
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
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
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)

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

  -- Load the env's module-pin snapshot only when auto-install can actually run
  -- (`morloc make`, not --offline). Read-only commands (typecheck, dump) and
  -- eval never install, so they must neither pay the directory scan nor be
  -- aborted by a conflicting snapshot they will not consult. `morloc install`
  -- loads it separately (Subcommands.cmdInstall).
  autoInstall <- MM.gets stateAutoInstall
  when autoInstall $ do
    snapshot <- Mod.loadSnapshot
    MM.modify (\st -> st {stateSnapshot = snapshot})

  -- The main module's @log-template@ becomes the program-wide default
  -- log message template. Per-label overrides win over this; the
  -- built-in defaults in 'Morloc.CodeGenerator.LogTemplate' fill any
  -- subfield neither this nor the per-label config supplies.
  MM.modify (\st -> st {stateLogTemplate = moduleConfigLogTemplate moduleConfig})

  -- Likewise the program-wide @benchmark-template@: one row shape for the
  -- end-of-run summary the nexus renders from every label's timings.
  MM.modify (\st -> st {stateBenchTemplate = moduleConfigBenchTemplate moduleConfig})

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
      Mod.loadModuleMetadata True path
    Nothing -> return ()

  case Parser.readProgram Nothing f code parserState mempty of
    (Left e) -> MM.throwSystemError $ pretty e
    (Right (mainDag, mainState)) -> do
      -- capture module-level docs from the main module before imports overwrite them
      MM.modify (\st -> st
        { stateModuleDoc = psModuleDoc mainState
        , stateModuleEpilogues = psModuleEpilogues mainState
        })
      entryPath <- case f of
        Just path -> Just <$> MM.liftIO (canonicalizePath path)
        Nothing -> return Nothing
      parseImports entryPath mainDag mainState Map.empty Map.empty Map.empty
  where
    -- Resolve an import to a file path. Local (.dot) imports are project
    -- files and are never downloaded. A missing bare/namespaced import is
    -- auto-downloaded only when stateAutoInstall is set (make, unless
    -- --offline) and we are not in eval mode. Resolution itself always goes
    -- through findModule, which reports ambiguity errors and the
    -- local-resolution warning; findBareModuleMaybe is only a presence probe
    -- that decides whether an install is needed.
    resolveImport :: Maybe Path -> MVar -> MVar -> MorlocMonad Path
    resolveImport mainPath mainModule importedModule
      | Mod.isLocalImport importedModule =
          Mod.findModule (mainPath, mainModule) importedModule
      | otherwise = do
          present <- Mod.findBareModuleMaybe importedModule
          case present of
            Just _ -> Mod.findModule (mainPath, mainModule) importedModule
            Nothing -> do
              autoInstall <- MM.gets stateAutoInstall
              evalMode <- MM.gets stateEvalMode
              when (autoInstall && not evalMode) $
                Mod.autoInstallDep importedModule
              Mod.findModule (mainPath, mainModule) importedModule

    -- descend recursively into imports
    --
    -- A module's local imports resolve from the root of its own package
    -- (MOD-6): the entry file's directory for the program, the directory of
    -- a package reached by a bare import for that package. In such a
    -- package, a local import `.x` becomes the module `p.x` (p the package),
    -- so two packages' local modules of one name stay distinct. @pkgs@ holds
    -- each module's package, absent for the program's own modules; @rw@
    -- holds each renamed import's package root and written name.
    parseImports ::
      Maybe Path ->
      DAG MVar Import ExprI ->
      PState ->
      Map.Map MVar Path ->
      Map.Map MVar (MVar, Path) ->
      Map.Map MVar (Path, MVar) ->
      MorlocMonad (DAG MVar Import ExprI)
    parseImports entryPath d s m pkgs rw = case unimported of
      [] -> do
        -- transfer source positions from parser state into MorlocState
        MM.modify (\st -> st
          { stateSourceMap = psSourceMap s <> stateSourceMap st
          , stateTermDocs = psTermDocs s <> stateTermDocs st
          , stateStreamElems = psStreamElems s <> stateStreamElems st
          })
        -- emit any docstring warnings accumulated during desugar
        case psWarnings s of
          [] -> return ()
          ws -> MM.tell ws
        case MDD.findCycle d of
          Nothing -> return d
          Just cyc -> MM.throwSystemError $
            "Import cycle:" <+> hsep (punctuate " ->" (map pretty cyc))
      ((mainModule, importedModule) : _) -> do
        importPath <- case Map.lookup importedModule rw of
          Just (root, written) -> Mod.findLocalModuleAt mainModule root written
          Nothing -> resolveImport (Map.lookup mainModule m) mainModule importedModule

        -- Every loaded module is reached from the entry file, so importing
        -- the entry file closes a cycle.
        canonPath <- MM.liftIO $ canonicalizePath importPath
        when (Just canonPath == entryPath) . MM.throwSystemError $
          "Import cycle: module" <+> pretty mainModule <+> "imports the entry file"
            <+> pretty importPath <+> "as" <+> pretty importedModule

        -- Load the <main>.yaml file associated with the main morloc package file
        moduleConfig <- Config.loadModuleConfig (Just importPath)
        -- the package this module belongs to, absent for the program's own
        let pkg = case Map.lookup importedModule rw of
              Just _ -> Map.lookup mainModule pkgs
              Nothing
                | Mod.isLocalImport importedModule -> Map.lookup mainModule pkgs
                | otherwise -> Just (importedModule, MS.takeDirectory importPath)
            newState = s {psModuleConfig = moduleConfig, psOwnModule = isNothing pkg}

        Mod.loadModuleMetadata False importPath
        (childPath, code') <- openLocalModule importPath
        case Parser.readProgram (Just importedModule) childPath code' newState d of
          (Left e) -> MM.throwSystemError $ pretty e
          (Right (d', s')) ->
            -- The parsed module may have a different internal name than the
            -- import edge target (e.g., file declares "module units" but
            -- import edge targets ".units"). Reconcile by renaming the DAG
            -- entry to match the import name.
            let d'' = reconcileModuleName importedModule d d'
                (d3, renamed) = case pkg of
                  Just (p, root) -> renameLocalImports p root importedModule d''
                  Nothing -> (d'', [])
                pkgs' = maybe pkgs (\v -> Map.insert importedModule v pkgs) pkg
                rw' = Map.union rw (Map.fromList renamed)
            in if Map.member importedModule d3
                 then parseImports entryPath d3 s' (maybe m (\v -> Map.insert importedModule v m) childPath) pkgs' rw'
                 else MM.throwSystemError $
                   "Loading" <+> pretty importPath <+> "for the import of" <+> pretty importedModule
                     <+> "defined no module of that name;"
                     <+> case newModules d d' of
                           [] -> "every module it declares was already loaded"
                           ms -> "it declares" <+> hsep (punctuate "," (map pretty ms))
      where
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- find all (module to module) edges in the graph where the imported
        -- module has not yet been parsed
        unimported = filter (\(_, importMod) -> not (Set.member importMod parsed)) (MDD.edgelist d)

    -- Give a package module's local imports their package-qualified names,
    -- returning each new name's package root and written name.
    renameLocalImports ::
      MVar -> Path -> MVar -> DAG MVar Import ExprI -> (DAG MVar Import ExprI, [(MVar, (Path, MVar))])
    renameLocalImports (MV p) root k dag = case Map.lookup k dag of
      Nothing -> (dag, [])
      Just (ExprI i e, edges) ->
        let locals = [v | (v, _) <- edges, Mod.isLocalImport v]
            qualified v@(MV t) = if Mod.isLocalImport v then MV (p <> t) else v
            renameImport imp = imp {importModuleName = qualified (importModuleName imp)}
            renameExpr (ExprI j (ModE n es)) = ExprI j (ModE n (map renameExpr es))
            renameExpr (ExprI j (ImpE imp)) = ExprI j (ImpE (renameImport imp))
            renameExpr x = x
            edges' = [(qualified v, renameImport imp) | (v, imp) <- edges]
         in ( Map.insert k (renameExpr (ExprI i e), edges') dag
            , [(qualified v, (root, v)) | v <- locals]
            )

    -- If readProgram added a key that doesn't match importedModule, rename it.
    -- This happens when a local import (".units") parses a file that declares
    -- "module units (...)". We rename the DAG key and the ModE name to match
    -- the import edge target.
    newModules :: DAG MVar Import ExprI -> DAG MVar Import ExprI -> [MVar]
    newModules dOld dNew = filter (`Map.notMember` dOld) (Map.keys dNew)

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
