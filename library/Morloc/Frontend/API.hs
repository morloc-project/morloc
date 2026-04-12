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
    (Right (mainDag, mainState)) -> parseImports mainDag mainState Map.empty
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

        -- Strip the "." prefix from local imports. The prefix is only needed
        -- for findModule to know to resolve locally. After resolution, the
        -- canonical module name is the bare name (matching the file's own
        -- module declaration).
        let canonicalName = stripLocalPrefix importedModule
            d1 = if canonicalName /= importedModule
                   then rewriteEdgeTarget importedModule canonicalName d
                   else d

        -- Load the <main>.yaml file associated with the main morloc package file
        moduleConfig <- Config.loadModuleConfig (Just importPath)
        let newState = s {psModuleConfig = moduleConfig}

        Mod.loadModuleMetadata importPath
        (childPath, code') <- openLocalModule importPath
        case Parser.readProgram (Just canonicalName) childPath code' newState d1 of
          (Left e) -> MM.throwSystemError $ pretty e
          (Right (d', s')) -> parseImports d' s' (maybe m (\v -> Map.insert canonicalName v m) childPath)
      where
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- find all (module to module) edges in the graph where the imported
        -- module has not yet been parsed
        unimported = filter (\(_, importMod) -> not (Set.member importMod parsed)) (MDD.edgelist d)

    -- Strip the "." prefix that marks local imports
    stripLocalPrefix :: MVar -> MVar
    stripLocalPrefix (MV x)
      | T.pack "." `T.isPrefixOf` x = MV (T.drop 1 x)
      | otherwise = MV x

    -- Rewrite all DAG edges that target oldName to target newName
    rewriteEdgeTarget :: MVar -> MVar -> DAG MVar Import ExprI -> DAG MVar Import ExprI
    rewriteEdgeTarget oldName newName = Map.map (\(n, edges) ->
      (n, [(if k == oldName then newName else k, e) | (k, e) <- edges]))

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
