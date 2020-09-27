{-|
Module      : Morloc.Parser.API
Description : IO functions for the parser
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Parser.API
  ( parse
  , cute
  , ugly
  ) where

import Morloc.Namespace
import Morloc.Parser.Parser
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Pretty as Pretty

import qualified Control.Monad as CM
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)

parse ::
     Maybe Path
  -> Code -- ^ code of the current module
  -> MorlocMonad [Module]
parse f (Code code) = do
  mods <- fmap Map.elems $ parse' Map.empty (f, code)
  return mods
  where
    parse' ::
         (Map.Map MVar Module)
      -> (Maybe Path, MT.Text)
      -> MorlocMonad (Map.Map MVar Module)

    parse' visited (f', code') = do
      -- check for multiple declarations within a file, NOT global
      ms <- checkForMultipleDeclarations (readProgram f' code') 
      CM.foldM parse'' visited ms

    parse'' ::
         (Map.Map MVar Module) -> Module -> MorlocMonad (Map.Map MVar Module)
    parse'' visited m
      | Map.member (moduleName m) visited = return visited
      | otherwise = do
        -- these are modules that are not defined in the input file
        let nonlocalModules = filter (\v -> not (Map.member v visited))
                            . map importModuleName . moduleImports $ m

        -- FIXME: for local modules, where two or more modules are defined in
        -- one file, how is module/package metadata entered?

        -- load metadata for all non-local imported modules
        mapM (\v -> Mod.findModule v >>= Mod.loadModuleMetadata) nonlocalModules
        -- for now I only support local modules
        imports <- mapM (\v -> Mod.findModule v >>= openLocalModule) nonlocalModules
        mods <- CM.foldM parse' (Map.insert (moduleName m) m visited) imports
        return mods

checkForMultipleDeclarations :: [Module] -> MorlocMonad [Module] 
checkForMultipleDeclarations ms = case duplicates (map moduleName ms) of
  [] -> return ms
  mvars -> MM.throwError $ MultipleModuleDeclarations mvars

cute :: [Module] -> IO ()
cute ms = mapM_ (\m -> putDoc (Pretty.prettyModule m) >> putStrLn "") ms

ugly :: [Module] -> IO ()
ugly ms = print ms

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile (MT.unpack . unPath $ filename)
  return (Just filename, code)
