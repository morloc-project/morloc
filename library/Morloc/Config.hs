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
  , writeBuildConfig
  , setupServerAndSocket
  , getDefaultConfigFilepath
  , getDefaultMorlocLibrary
  -- State-rooted (mutable) path helpers -- see 'configState'.
  , exeDir
  , fdbDir
  , moduleDir
  , rustBuildDir
  , snapshotDir
  ) where

import qualified Data.Aeson.KeyMap as K
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Yaml as Y
import qualified Data.Yaml.Config as YC
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import qualified Morloc.System as MS
import System.Environment (lookupEnv)

getDefaultConfigFilepath :: IO Path
getDefaultConfigFilepath = MS.combine <$> getDefaultMorlocHome <*> pure "config"

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $
    Config
      (MT.unpack . fromJust $ defaults K.!? "home")
      (MT.unpack . fromJust $ defaults K.!? "state")
      (MT.unpack . fromJust $ defaults K.!? "source")
      (MT.unpack . fromJust $ defaults K.!? "plane")
      (MT.unpack . fromJust $ defaults K.!? "plane-core")
      (MT.unpack . fromJust $ defaults K.!? "tmpdir")
      (MT.unpack . fromJust $ defaults K.!? "build-config")
      Map.empty -- configLangOverrides
      Nothing   -- configRegistry

{- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
default configuration will be used.
-}
loadMorlocConfig :: Maybe Path -> IO Config
loadMorlocConfig mfile =
  loadMorlocConfig' mfile >>= applyMorlocHomeOverride >>= resolveStateRoots

{- | Resolve the mutable-STATE root. @$MORLOC_STATE@ overrides; it defaults to
'configHome' (a plain host install stays single-directory). @configLibrary@ (the
installed-module plane source) is re-derived under the state root because
installed modules are state, not runtime. Runs AFTER 'applyMorlocHomeOverride'
so it sees the final home. In a container, the manager sets MORLOC_HOME to the
image-baked runtime and MORLOC_STATE to the mounted state dir, so init's runtime
under home is never shadowed by the state mount.
-}
resolveStateRoots :: Config -> IO Config
resolveStateRoots config = do
  envState <- lookupEnv "MORLOC_STATE"
  let state = case envState of
        Just s | not (null s) -> s
        _ -> configHome config
  return
    config
      { configState = state
      , configLibrary = MS.combine state "src/morloc/plane"
      }

-- | State root (mutable): built programs, module db, installed module code.
exeDir :: Config -> Path
exeDir c = MS.combine (configState c) "exe"

-- | Module database directory (state).
fdbDir :: Config -> Path
fdbDir c = MS.combine (configState c) "fdb"

-- | Per-language installed-module directory under the state root
-- (@sub@ = "include" | "python" | "R").
moduleDir :: Config -> String -> Path
moduleDir c sub = MS.combine (MS.combine (configState c) "modules") sub

-- | Regenerable rustmorloc/pool cargo build cache, under the state root.
rustBuildDir :: Config -> Path
rustBuildDir c = MS.combine (configState c) "cache/rust-build"

-- | Module-pin snapshot directory (state). Holds zero or more snapshot files
-- (one @name hash@ per line) that pin module git hashes for on-demand install.
-- Read lazily on the make/install path, never at config load.
snapshotDir :: Config -> Path
snapshotDir c = MS.combine (configState c) "snapshots"

loadMorlocConfig' :: Maybe Path -> IO Config
loadMorlocConfig' Nothing = do
  defaults <- defaultFields
  MS.loadYamlConfig
    Nothing
    (YC.useCustomEnv defaults)
    loadDefaultMorlocConfig
loadMorlocConfig' (Just configFile) = do
  configExists <- MS.doesFileExist configFile
  defaults <- defaultFields
  if configExists
    then
      MS.loadYamlConfig
        (Just [configFile])
        (YC.useCustomEnv defaults)
        loadDefaultMorlocConfig
    else
      loadMorlocConfig' Nothing

{- | When MORLOC_HOME is set, it is authoritative for the home-rooted layout,
overriding any @home@ (and its derived paths) baked into a loaded config file.
Without this, an absolute @home@ persisted by @morloc init@ -- e.g. a config
initialized on a host and later bind-mounted into a container under a different
MORLOC_HOME -- would shadow the environment variable, so installs land outside
the mounted tree. The re-rooted subpaths mirror the getDefaultMorloc* helpers.
-}
applyMorlocHomeOverride :: Config -> IO Config
applyMorlocHomeOverride config = do
  envHome <- lookupEnv "MORLOC_HOME"
  case envHome of
    Just h | not (null h) ->
      return
        config
          { configHome = h
          , configLibrary = MS.combine h "src/morloc/plane"
          , configTmpDir = MS.combine h "tmp"
          , configBuildConfig = MS.combine h ".build-config.yaml"
          }
    _ -> return config

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
            "Failed to parse module config file '"
              <> pretty configFile
              <> "': "
              <> pretty (Y.prettyPrintParseException errMsg)
        Right config -> validateLabels moduleConfigFile config
    else
      return defaultValue

-- | Reject labels containing characters that would break the code emitted
-- around them. Labels become string literals in Python/R/C++ wrap shims and
-- are used unquoted in shell glob patterns; restricting them to a portable
-- alphanumeric set avoids quoting/escaping bugs and shell-injection holes
-- at codegen time. Allowed characters: A-Z, a-z, 0-9, '_', '-'. The label
-- must also be non-empty.
validateLabels :: Path -> ModuleConfig -> MorlocMonad ModuleConfig
validateLabels path mc = do
  let bad = [lbl | lbl <- Map.keys (moduleConfigLabeledGroups mc), not (validLabel lbl)]
  case bad of
    [] -> return mc
    xs ->
      MM.throwSystemError $
        "Invalid label name(s) in '" <> pretty path <> "': "
          <> hsep (punctuate "," (map (dquotes . pretty) xs))
          <> ". Labels must match [A-Za-z0-9_-]+."
  where
    validLabel t = not (MT.null t) && MT.all isOk t
    isOk c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9') || c == '_' || c == '-'

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

-- | Serialize the build config to its file. This writes the whole value, so to
-- preserve existing fields a caller must start from a loaded config and modify
-- it (a read-modify-write). Nothing-valued fields are omitted by the ToJSON
-- instance, keeping the file minimal.
writeBuildConfig :: Config -> BuildConfig -> IO ()
writeBuildConfig config = Y.encodeFile (configBuildConfig config)

setupServerAndSocket ::
  LangRegistry ->
  Lang ->
  Socket
setupServerAndSocket reg lang0 = Socket lang socket
  where
    -- A guest member (e.g. futhark) has no pool of its own; its socket
    -- resolves to its host pool (cpp). Centralised here so no caller can emit
    -- a phantom guest socket by forgetting to map. Identity for non-members.
    lang = LR.poolOf reg lang0
    socket = "pipe-" <> pretty (ML.showLangName lang)

-- This is where the default file organization of morloc is set
defaultFields :: IO (K.KeyMap Text)
defaultFields = do
  home <- MT.pack <$> getDefaultMorlocHome
  state <- MT.pack <$> getDefaultMorlocState
  lib <- MT.pack <$> getDefaultMorlocSource
  tmp <- MT.pack <$> getDefaultMorlocTmpDir
  buildConfig <- MT.pack <$> getDefaultMorlocBuildConfig
  return $
    K.fromList
      [ ("home", home)
      , ("state", state)
      , ("source", lib)
      , ("plane", "default")
      , ("plane-core", "morloclib")
      , ("tmpdir", tmp)
      , ("build-config", buildConfig)
      ]

-- | Get the Morloc home directory (absolute path).
-- Respects MORLOC_HOME env var, falling back to ~/.local/share/morloc.
getDefaultMorlocHome :: IO Path
getDefaultMorlocHome = do
  envHome <- lookupEnv "MORLOC_HOME"
  case envHome of
    Just p | not (null p) -> return p
    _ -> MS.combine <$> MS.getHomeDirectory <*> pure ".local/share/morloc"

-- | The mutable-state root: @$MORLOC_STATE@, else 'getDefaultMorlocHome'.
getDefaultMorlocState :: IO Path
getDefaultMorlocState = do
  envState <- lookupEnv "MORLOC_STATE"
  case envState of
    Just p | not (null p) -> return p
    _ -> getDefaultMorlocHome

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
