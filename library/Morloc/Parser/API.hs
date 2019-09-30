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
  , ignoreSource
  , localModules
  , checkSources
) where

import Morloc.Parser.Parser
import Morloc.Global (Path)
import Morloc.Operators
import Morloc.TypeChecker.Namespace
import qualified Morloc.Data.Text as MT

import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import qualified Control.Monad as CM
import qualified Data.Map as Map
import qualified System.FilePath.Posix as SFP

parse
  :: (Path -> IO ()) -- ^ check existence of a source (file, URL, or whatever)
  -> (MVar -> IO (Maybe Path, MT.Text)) -- ^ open a module (file, URL, or whatever)
  -> Maybe Path
  -> MT.Text -- ^ code of the current module
  -> IO [Module]
parse checkSource loadModule f code = fmap Map.elems $ parse' Map.empty (f, code) where
  parse' :: (Map.Map MVar Module) -> (Maybe Path, MT.Text) -> IO (Map.Map MVar Module)
  parse' visited (f', code') = CM.foldM parse'' visited (readProgram f' code')

  parse'' :: (Map.Map MVar Module) -> Module -> IO (Map.Map MVar Module)
  parse'' visited m
    | Map.member (moduleName m) visited = return visited
    | otherwise = do
        checkSources checkSource m
        imports <- mapM (loadModule . importModuleName) (moduleImports m)
        CM.foldM parse' (Map.insert (moduleName m) m visited) imports

cute :: Either TypeError [Module] -> IO ()
cute (Right es) = mapM_ (\e -> putDoc (prettyModule e) >> putStrLn "") es
cute (Left err) = print err

ugly :: Either TypeError [Module] -> IO ()
ugly (Right es) = print es
ugly (Left err) = print err

-- do not check existence of source files
ignoreSource :: MT.Text -> IO () 
ignoreSource _ = return ()

localModules :: Maybe String -> MVar -> IO (Maybe Path, MT.Text)
localModules (Just filename) (MV f) = do
  code <- MT.readFile . SFP.replaceFileName filename $ (MT.unpack f <> ".loc")
  return (Just (MT.pack filename), code)
localModules Nothing (MV f) = do
  let filename = MT.unpack f <> ".loc"
  code <- MT.readFile filename
  return (Just . MT.pack $ filename, code)

-- assert that all sourced resources exist
checkSources :: (Path -> IO ()) -> Module -> IO ()
checkSources f m = chk' (moduleBody m) where
  chk' ((SrcE _ (Just filename) _):es) = f filename >> chk' es
  chk' (_:es) = chk' es
  chk' [] = return ()
