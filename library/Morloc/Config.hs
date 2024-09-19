{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Config
Description : Handle local configuration
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Config
  ( Config(..)
  , loadMorlocConfig
  , loadDefaultMorlocConfig
  , setupServerAndSocket
  , getDefaultConfigFilepath
  , getDefaultMorlocLibrary
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Language as ML
import qualified Data.Yaml.Config as YC
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import qualified Data.Aeson.KeyMap as K


getDefaultConfigFilepath :: IO Path
getDefaultConfigFilepath = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc/config"

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $
    Config
      (MT.unpack . fromJust $ defaults K.!? "home")
      (MT.unpack . fromJust $ defaults K.!? "source")
      (MT.unpack . fromJust $ defaults K.!? "plane")
      (MT.unpack . fromJust $ defaults K.!? "tmpdir")
      "python3" -- lang_python3
      "Rscript" -- lang_R
      "perl" -- lang_perl

-- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
-- default configuration will be used.
loadMorlocConfig :: Maybe Path -> IO Config
loadMorlocConfig Nothing = do
  defaults <- defaultFields
  MS.loadYamlConfig
    Nothing
    (YC.useCustomEnv defaults)
    loadDefaultMorlocConfig
loadMorlocConfig (Just configFile) = do
  configExists <- MS.doesFileExist configFile
  defaults <- defaultFields
  if configExists
    then
      MS.loadYamlConfig
        (Just [configFile])
        (YC.useCustomEnv defaults)
        loadDefaultMorlocConfig
    else
      loadMorlocConfig Nothing

-- -- | Create the base call to a pool (without arguments)
-- -- For example:
-- --   ./pool.R 1 --
-- --   ./pool.py 1 --
-- --   ./pool-cpp.out 1 --
-- --   ./pool.R 1 [1,2,3] true
-- buildPoolCallBase
--   :: Config
--   -> Maybe Lang
--   -> Int
--   -> Maybe [MDoc]
-- buildPoolCallBase _ (Just CLang) i =
--   Just ["./" <> pretty (ML.makeExecutableName CLang "pool"), pretty i]
-- buildPoolCallBase _ (Just CppLang) i =
--   Just ["./" <> pretty (ML.makeExecutableName CppLang "pool"), pretty i]
-- buildPoolCallBase c (Just RLang) i =
--   Just [pretty (configLangR c), pretty (ML.makeExecutableName RLang "pool"), pretty i]
-- buildPoolCallBase c (Just Python3Lang) i =
--   Just [pretty (configLangPython3 c), pretty (ML.makeExecutableName Python3Lang "pool"), pretty i]
-- buildPoolCallBase _ _ _ = Nothing -- FIXME: add error handling

setupServerAndSocket
  :: Config
  -> Lang
  -> Socket 
setupServerAndSocket c lang = Socket lang (args <> [socket]) socket where
  args = case lang of
    CLang -> ["./" <> pretty (ML.makeExecutableName CLang "pool")]
    CppLang -> ["./" <> pretty (ML.makeExecutableName CppLang "pool")]
    RLang -> [pretty (configLangR c), pretty (ML.makeExecutableName RLang "pool")]
    Python3Lang -> [pretty (configLangPython3 c), pretty (ML.makeExecutableName Python3Lang "pool")]
    _ -> undefined 

  socket = "pipe-" <> pretty (ML.showLangName lang)


-- A key value map
defaultFields :: IO (K.KeyMap MT.Text)
defaultFields = do
  home <- MT.pack <$> getDefaultMorlocHome
  lib <- MT.pack <$> getDefaultMorlocSource
  tmp <- MT.pack <$> getDefaultMorlocTmpDir
  return $ K.fromList [("home", home), ("source", lib), ("plane", "morloclib"), ("tmpdir", tmp)]

-- | Get the Morloc home directory (absolute path)
getDefaultMorlocHome :: IO Path
getDefaultMorlocHome = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc"

-- | Get the Morloc source directory (absolute path). Usually this will be a
-- folder inside the home directory. This is the path to the source data (often
-- a get repo).
getDefaultMorlocSource :: IO Path
getDefaultMorlocSource = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc/src/morloc"

-- | Get the path to the morloc shared libraries folder
getDefaultMorlocLibrary :: IO Path
getDefaultMorlocLibrary = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc/lib"

-- | Get the Morloc default temporary directory. This will store generated
-- SPARQL queries and rdf dumps that can be used in debugging.
getDefaultMorlocTmpDir :: IO Path
getDefaultMorlocTmpDir = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc/tmp"
