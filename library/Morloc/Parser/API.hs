{-|
Module      : Morloc.Parser.API
Description : IO functions for the parser
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Parser.API
( 
    parse
  , cute
  , ugly
) where

import Morloc.Parser.Parser
import Morloc.Namespace (Path, Name, MorlocMonad)
import Morloc.TypeChecker.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM

import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import qualified Control.Monad as CM
import qualified Data.Map as Map
import qualified System.FilePath.Posix as SFP

parse
  :: Maybe Path
  -> MT.Text -- ^ code of the current module
  -> MorlocMonad [Module]
parse f code = do
  mods <- fmap Map.elems $ parse' Map.empty (f, code)
  return mods
  where
    parse' :: (Map.Map MVar Module) -> (Maybe Path, MT.Text) -> MorlocMonad (Map.Map MVar Module)
    parse' visited (f', code') = CM.foldM parse'' visited (readProgram f' code')

    parse'' :: (Map.Map MVar Module) -> Module -> MorlocMonad (Map.Map MVar Module)
    parse'' visited m
      | Map.member (moduleName m) visited = return visited
      | otherwise = do
          -- for now I only support local modules
          imports <- mapM (\(MV v) -> Mod.findModule v >>= openLocalModule)
                          (map importModuleName (moduleImports m))
          mapM Mod.loadModuleMetadata (map snd imports)
          mods <- CM.foldM parse' (Map.insert (moduleName m) m visited) imports
          return mods

cute :: [Module] -> IO ()
cute ms = mapM_ (\m -> putDoc (prettyModule m) >> putStrLn "") ms

ugly :: [Module] -> IO ()
ugly ms = print ms

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- MM.liftIO $ MT.readFile (MT.unpack filename)
  return (Just filename, code)
