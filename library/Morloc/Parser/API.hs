{-|
Module      : Morloc.Parser.API
Description : IO functions for the parser
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Parser.API (parse) where

import Morloc.Namespace
import qualified Morloc.Parser.Parser as Parser
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Data.DAG as MDD
import qualified Data.Set as Set
import qualified Data.Map as Map

parse ::
     Maybe Path
  -> Code -- ^ code of the current module
  -> MorlocMonad (DAG MVar Import ParserNode)
parse f (Code code) = parseImports (Parser.readProgram f code mempty)
  where
    parseImports
      :: DAG MVar Import ParserNode
      -> MorlocMonad (DAG MVar Import ParserNode)
    parseImports dag@(DAG g d)
      | length unimported == 0 = return dag
      | otherwise = do
          importPath <- Mod.findModule (head imports)
          Mod.loadModuleMetadata importPath
          (path', code') <- openLocalModule importPath
          parseImports (Parser.readProgram path' code' dag)
      where
        unimported = Set.difference (MDD.keysSet dag) (Map.keysSet d)
        imports = [k | (k, _) <- concat (Map.elems g), Set.member k unimported]

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile (MT.unpack . unPath $ filename)
  return (Just filename, code)
