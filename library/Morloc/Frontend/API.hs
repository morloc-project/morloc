{- |
Module      : Morloc.Frontend.API
Description : Morloc frontend API
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
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
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import Morloc.Frontend.Parser (PState (..), emptyPState)
import Morloc.Frontend.Namespace
import qualified Morloc.Frontend.Parser as Parser
import qualified Morloc.Frontend.Typecheck as Typecheck
import qualified Morloc.Frontend.Valuecheck as Valuecheck
import qualified Morloc.CodeGenerator.LanguageDescriptor as LD
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import Morloc.Data.Doc
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath ((</>))

parse ::
  -- | path to the current module (if we are reading from a file)
  Maybe Path ->
  -- | code of the current module
  Code ->
  MorlocMonad (DAG MVar Import ExprI)
parse f (Code code) = do
  moduleConfig <- Config.loadModuleConfig f
  plugins <- discoverPluginLangs

  let parserState = emptyPState { psModuleConfig = moduleConfig
                                , psPluginLangs = plugins }

  -- store source text for the main file
  case f of
    Just path -> MM.modify (\st -> st { stateSourceText = Map.insert path code (stateSourceText st) })
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
        MM.modify (\st -> st { stateSourceMap = psSourceMap s <> stateSourceMap st })
        return d
      ((mainModule, importedModule) : _) -> do
        importPath <- case Map.lookup mainModule m of
          (Just mainPath) -> Mod.findModule (Just mainPath, mainModule) importedModule
          Nothing -> Mod.findModule (Nothing, mainModule) importedModule

        -- Load the <main>.yaml file associated with the main morloc package file
        moduleConfig <- Config.loadModuleConfig (Just importPath)
        let newState = s { psModuleConfig = moduleConfig }

        Mod.loadModuleMetadata importPath
        (childPath, code') <- openLocalModule importPath
        case Parser.readProgram (Just importedModule) childPath code' newState d of
          (Left e) -> MM.throwSystemError $ pretty e
          (Right (d', s')) -> parseImports d' s' (maybe m (\v -> Map.insert importedModule v m) childPath)
      where
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- find all (module to module) edges in the graph where the imported
        -- module has not yet been parsed
        unimported = filter (\(_, importMod) -> not (Set.member importMod parsed)) (MDD.edgelist d)

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  MM.modify (\st -> st { stateSourceText = Map.insert filename code (stateSourceText st) })
  return (Just filename, code)

-- | Scan configHome/lang/ for plugin language descriptors (lang.yaml files).
-- Returns a map from language name to file extension.
discoverPluginLangs :: MorlocMonad (Map.Map T.Text String)
discoverPluginLangs = do
  home <- MM.asks configHome
  let langDir = home </> "lang"
  exists <- liftIO $ doesDirectoryExist langDir
  if not exists
    then return Map.empty
    else do
      dirs <- liftIO $ listDirectory langDir
      results <- liftIO $ mapM (scanLangDir langDir) dirs
      return $ Map.fromList [(n, e) | Just (n, e) <- results]
  where
    scanLangDir :: FilePath -> String -> IO (Maybe (T.Text, String))
    scanLangDir langDir dirName = do
      let descPath = langDir </> dirName </> "lang.yaml"
      hasDesc <- doesFileExist descPath
      if not hasDesc
        then return Nothing
        else do
          result <- LD.loadLangDescriptor descPath
          case result of
            Left _ -> return Nothing
            Right desc -> return $ Just (T.toLower (LD.ldName desc), LD.ldExtension desc)
