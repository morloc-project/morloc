{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Config
Description : Handle local configuration
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Config
  ( Config(..)
  , loadMorlocConfig
  , loadModuleConfig
  , loadDefaultMorlocConfig
  , loadBuildConfig
  , setupServerAndSocket
  , getDefaultConfigFilepath
  , getDefaultMorlocLibrary
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Language as ML
import qualified Data.Yaml as Y
import qualified Data.Yaml.Config as YC
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import qualified Morloc.Monad as MM
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
      (MT.unpack . fromJust $ defaults K.!? "build-config")
      "python3" -- lang_python3
      "Rscript" -- lang_R

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

loadModuleConfig :: Maybe Path -> MorlocMonad ModuleConfig
loadModuleConfig Nothing = return defaultValue
loadModuleConfig (Just configFile) = do
  let moduleConfigFile = MS.dropExtension configFile <> ".yaml"
  configExists <- liftIO $ MS.doesFileExist moduleConfigFile
  if configExists
    then do
      result <- liftIO $ Y.decodeFileEither moduleConfigFile
      case result of
        Left errMsg -> MM.throwError . OtherError . MT.pack $
            "Failed to parse module config file '" <> configFile <> "': " <> Y.prettyPrintParseException errMsg
        Right config -> return config
    else
        return defaultValue

loadBuildConfig :: Config -> IO BuildConfig
loadBuildConfig config = do
  let configFile = configBuildConfig config
  configExists <- MS.doesFileExist configFile
  if configExists
    then do
      result <- Y.decodeFileEither configFile
      case result of
        Left errMsg -> error $ "Failed to parse build config file '" <> configFile <> "': " <> Y.prettyPrintParseException errMsg
        Right buildConfig -> return buildConfig
    else
        return defaultValue

setupServerAndSocket
  :: Config
  -> Lang
  -> Socket 
setupServerAndSocket c lang = Socket lang args socket where
  args = case lang of
    CLang -> ["./" <> pretty (ML.makeExecutablePoolName CLang)]
    CppLang -> ["./" <> pretty (ML.makeExecutablePoolName CppLang)]
    RLang -> [pretty (configLangR c), pretty (ML.makeExecutablePoolName RLang)]
    Python3Lang -> [pretty (configLangPython3 c), pretty (ML.makeExecutablePoolName Python3Lang)]

  socket = "pipe-" <> pretty (ML.showLangName lang)


-- A key value map
defaultFields :: IO (K.KeyMap MT.Text)
defaultFields = do
  home <- MT.pack <$> getDefaultMorlocHome
  lib <- MT.pack <$> getDefaultMorlocSource
  tmp <- MT.pack <$> getDefaultMorlocTmpDir
  buildConfig <- MT.pack <$> getDefaultMorlocBuildConfig
  return $ K.fromList [("home", home), ("source", lib), ("plane", "morloclib"), ("tmpdir", tmp), ("build-config", buildConfig)]

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

-- | Get the Morloc default temporary directory.
getDefaultMorlocTmpDir :: IO Path
getDefaultMorlocTmpDir = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc/tmp"

-- | Get the Morloc default build config. This will store `morloc init` flags
-- that affect all builds
getDefaultMorlocBuildConfig :: IO Path
getDefaultMorlocBuildConfig = MS.combine <$> MS.getHomeDirectory <*> pure ".morloc/.build-config.yaml"
