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

  -- Compute project root from entry-point file path
  let projectRoot = fmap MS.takeDirectory f

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
        return d
      ((mainModule, importedModule) : _) -> do
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
