{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Config
Description : Configuration loading and default paths
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Loads the morloc configuration from @~\/.local\/share\/morloc\/config@ (YAML),
module-level configs from @\<module\>.yaml@, and build configs. Also sets up
per-language server sockets for IPC during pool execution.
-}
module Morloc.Config
  ( Config (..)
  , loadMorlocConfig
  , loadModuleConfig
  , loadDefaultMorlocConfig
  , loadBuildConfig
  , setupServerAndSocket
  , getDefaultConfigFilepath
  , getDefaultMorlocLibrary
  ) where

import qualified Data.Aeson.KeyMap as K
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Yaml as Y
import qualified Data.Yaml.Config as YC
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim
import Morloc.Namespace.Expr
import Morloc.Namespace.State
import qualified Morloc.System as MS

getDefaultConfigFilepath :: IO Path
getDefaultConfigFilepath = MS.combine <$> getDefaultMorlocHome <*> pure "config"

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $
    Config
      (MT.unpack . fromJust $ defaults K.!? "home")
      (MT.unpack . fromJust $ defaults K.!? "source")
      (MT.unpack . fromJust $ defaults K.!? "plane")
      (MT.unpack . fromJust $ defaults K.!? "plane-core")
      (MT.unpack . fromJust $ defaults K.!? "tmpdir")
      (MT.unpack . fromJust $ defaults K.!? "build-config")
      Map.empty -- configLangOverrides

{- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
default configuration will be used.
-}
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
        Left errMsg ->
          MM.throwSystemError $
            "Failed to parse module config file '" <> pretty configFile <> "': " <> pretty (Y.prettyPrintParseException errMsg)
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
        Left errMsg ->
          error $
            "Failed to parse build config file '" <> configFile <> "': " <> Y.prettyPrintParseException errMsg
        Right buildConfig -> return buildConfig
    else
      return defaultValue

setupServerAndSocket ::
  Config ->
  LangRegistry ->
  Lang ->
  Socket
setupServerAndSocket c reg lang = Socket lang args socket
  where
    name = ML.langName lang
    -- Look up run command: config overrides take precedence over registry defaults
    runCmd = case Map.lookup name (configLangOverrides c) of
      Just cmd -> cmd
      Nothing -> LR.registryRunCommand reg name
    isCompiled = LR.registryIsCompiled reg name
    poolExe = ML.makeExecutablePoolName lang

    args
      | isCompiled = ["./" <> pretty poolExe]
      | null runCmd = [pretty name, pretty poolExe]
      | otherwise = map pretty runCmd ++ [pretty poolExe]

    socket = "pipe-" <> pretty (ML.showLangName lang)

-- This is where the default file organization of morloc is set
defaultFields :: IO (K.KeyMap Text)
defaultFields = do
  home <- MT.pack <$> getDefaultMorlocHome
  lib <- MT.pack <$> getDefaultMorlocSource
  tmp <- MT.pack <$> getDefaultMorlocTmpDir
  buildConfig <- MT.pack <$> getDefaultMorlocBuildConfig
  return $
    K.fromList
      [ ("home", home)
      , ("source", lib)
      , ("plane", "default")
      , ("plane-core", "morloclib")
      , ("tmpdir", tmp)
      , ("build-config", buildConfig)
      ]

-- | Get the Morloc home directory (absolute path)
getDefaultMorlocHome :: IO Path
getDefaultMorlocHome = MS.combine <$> MS.getHomeDirectory <*> pure ".local/share/morloc"

{- | Get the Morloc source directory (absolute path). Usually this will be a
folder inside the home directory. This is the path to the source data (often
a get repo).
-}
getDefaultMorlocSource :: IO Path
getDefaultMorlocSource = MS.combine <$> getDefaultMorlocHome <*> pure "src/morloc/plane"

-- | Get the path to the morloc shared libraries folder
getDefaultMorlocLibrary :: IO Path
getDefaultMorlocLibrary = MS.combine <$> getDefaultMorlocHome <*> pure "lib"

-- | Get the Morloc default temporary directory.
getDefaultMorlocTmpDir :: IO Path
getDefaultMorlocTmpDir = MS.combine <$> getDefaultMorlocHome <*> pure "tmp"

{- | Get the Morloc default build config. This will store `morloc init` flags
that affect all builds
-}
getDefaultMorlocBuildConfig :: IO Path
getDefaultMorlocBuildConfig = MS.combine <$> getDefaultMorlocHome <*> pure ".build-config.yaml"
