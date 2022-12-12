{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.API
Description : Morloc frontend API
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.API
  ( parse
  , Parser.readType
  , Typecheck.typecheck
  , Typecheck.resolveTypes
  ) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.Doc as MD
import Morloc.Data.Doc ((<+>))
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.Parser as Parser
import qualified Morloc.Frontend.Lexer as Lexer
import qualified Morloc.Frontend.Typecheck as Typecheck

parse ::
     Maybe Path -- ^ path to the current module (if we are reading from a file)
  -> Code -- ^ code of the current module
  -> MorlocMonad (DAG MVar Import ExprI)
parse f (Code code) = do
  -- MM.say $ "Parsing" <+> maybe "<stdin>" MD.viaShow f
  case Parser.readProgram Nothing f code Lexer.emptyState mempty of
    (Left e) -> MM.throwError $ SyntaxError e
    (Right (mainDag, mainState)) -> parseImports mainDag  mainState
  where
    -- descend recursively into imports
    parseImports
      :: DAG MVar Import ExprI
      -> Lexer.ParserState
      -> MorlocMonad (DAG MVar Import ExprI)
    parseImports d s = case unimported of
      [] -> return d
      (child:_) -> do
          importPath <- Mod.findModule child
          Mod.loadModuleMetadata importPath
          (childPath, code') <- openLocalModule importPath
          -- MM.say $ "Parsing module" <+> (MD.viaShow . unMVar) child <+> "from" <+>  MD.viaShow importPath
          case Parser.readProgram (Just child) childPath code' s d of
            (Left e) -> MM.throwError $ SyntaxError e
            (Right (d', s')) -> parseImports d' s'
      where
        -- all modules that are imported
        imported = Set.fromList (map snd (MDD.edgelist d))
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- the modules that have not been parsed yet
        unimported = Set.toList $ Set.difference imported parsed

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  return (Just filename, code)
