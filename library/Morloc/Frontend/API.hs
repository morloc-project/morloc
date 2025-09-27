{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.API
Description : Morloc frontend API
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.API
  ( parse
  , Parser.readType
  , Typecheck.typecheck
  , Typecheck.resolveTypes
  , Valuecheck.valuecheck
  ) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Lexer (ParserState(..), emptyState)
import qualified Data.Set as Set
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.Parser as Parser
import qualified Morloc.Frontend.Typecheck as Typecheck
import qualified Morloc.Frontend.Valuecheck as Valuecheck
import qualified Morloc.Config as Config

parse ::
     Maybe Path -- ^ path to the current module (if we are reading from a file)
  -> Code -- ^ code of the current module
  -> MorlocMonad (DAG MVar Import ExprI)
parse f (Code code) = do

  moduleConfig <- Config.loadModuleConfig f

  let parserState = emptyState 

  -- MM.say $ "Parsing" <+> maybe "<stdin>" MD.viaShow f
  case Parser.readProgram Nothing f code (parserState { stateModuleConfig = moduleConfig }) mempty of
    (Left e) -> MM.throwError $ SyntaxError e
    (Right (mainDag, mainState)) -> parseImports mainDag mainState Map.empty
  where
    -- descend recursively into imports
    parseImports
      :: DAG MVar Import ExprI
      -> ParserState
      -> Map.Map MVar Path
      -> MorlocMonad (DAG MVar Import ExprI)
    parseImports d s m = case unimported of
      [] -> return d
      ((mainModule, importedModule):_) -> do
          importPath <- case Map.lookup mainModule m of
              (Just mainPath) -> Mod.findModule (Just mainPath, mainModule) importedModule
              Nothing -> Mod.findModule (Nothing, mainModule) importedModule

          -- Load the <main>.yaml file associated with the main morloc package file
          moduleConfig <- Config.loadModuleConfig (Just importPath)
          let newState = s { stateModuleConfig = moduleConfig }

          Mod.loadModuleMetadata importPath
          (childPath, code') <- openLocalModule importPath
          case Parser.readProgram (Just importedModule) childPath code' newState d of
            (Left e) -> MM.throwError $ SyntaxError e
            (Right (d', s')) -> parseImports d' s' (maybe m (\v -> Map.insert importedModule v m) childPath)
      where
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- find all (module to module) edges in the graph where the imported
        -- module has not yet been parsed
        unimported = filter (\(_, importMod) -> not (Set.member importMod parsed)) (MDD.edgelist d)

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  return (Just filename, code)
