{- |
Module      : MorlocManager.Version
Description : Morloc version installation and management
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Manages multiple installed morloc versions. Each version corresponds to
a container image (@morloc-full:\<version\>@) pulled from GHCR and a
host-side data directory that persists morloc state across container runs.

@
  Version lifecycle:

  install           select            run
  +--------+       +--------+       +--------+
  | pull   |------>| set    |------>| start  |
  | image  |       | active |       | cont.  |
  | create |       | in cfg |       | with   |
  | dirs   |       +--------+       | ver's  |
  +--------+                        | image  |
                                    +--------+

  Data layout per version:
    ~/.local/share/morloc/versions/0.67.0/
      bin/         -- morloc binaries (mapped to ~/.local/bin inside container)
      lib/         -- installed morloc modules
      fdb/         -- program manifests
      include/     -- C/C++ headers
      opt/         -- optional data
      tmp/         -- build temporaries
@
-}

module MorlocManager.Version
  ( -- * Installation
    installVersion
  , installLatest
    -- * Selection
  , selectVersion
    -- * Removal
  , uninstallVersion
    -- * Discovery
  , listVersions
  , resolveActiveVersion
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeDirectoryRecursive
  )
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr, hFlush)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import MorlocManager.Types
import MorlocManager.Config
import MorlocManager.Container

-- | Install a morloc version: pull the container image and create the
-- host-side data directory structure.
--
-- If the version is already installed, this is a no-op unless the image
-- needs to be re-pulled.
installVersion :: ContainerEngine -> Scope -> Version -> IO (Either ManagerError ())
installVersion engine scope ver = do
  let imageRef = "ghcr.io/morloc-project/morloc/morloc-full:" <> showVersion ver
  -- Pull image
  (code, _stdout, stderr') <- containerPull engine imageRef
  case code of
    ExitFailure n -> pure (Left (EngineError engine n stderr'))
    ExitSuccess -> do
      -- Create version data directories
      vDataDir <- versionDataDir scope ver
      mapM_ (\d -> createDirectoryIfMissing True (vDataDir </> d))
        ["bin", "lib", "fdb", "include", "opt", "tmp"]
      -- Write version config
      let vc = VersionConfig
            { vcImage   = imageRef
            , vcHostDir = vDataDir
            , vcShmSize = "512m"
            , vcEngine  = engine
            }
      writeVersionConfig scope ver vc

-- | Install the latest morloc version using the "edge" container tag.
--
-- Pulls the @morloc-full:edge@ image, runs @morloc --version@ inside it
-- to discover the actual version number, then performs a normal versioned
-- install. Returns the installed 'Version' on success.
installLatest :: ContainerEngine -> Scope -> IO (Either ManagerError Version)
installLatest engine scope = do
  let edgeImage = "ghcr.io/morloc-project/morloc/morloc-full:edge" :: Text
  -- Pull the edge image
  (pullCode, _stdout, pullErr) <- containerPull engine edgeImage
  case pullCode of
    ExitFailure n -> pure (Left (EngineError engine n pullErr))
    ExitSuccess -> do
      -- Query the morloc version from inside the edge container
      let exe = engineExecutable engine
      (verCode, verOut, verErr) <- readProcessWithExitCode exe
        ["run", "--rm", Text.unpack edgeImage, "morloc", "--version"] ""
      case verCode of
        ExitFailure _ -> pure (Left (InstallError
          ("Failed to detect morloc version from edge image: " <> verErr)))
        ExitSuccess -> do
          -- Parse version from output like "morloc 0.68.0" or just "0.68.0"
          let verStr = lastWord (strip verOut)
          case parseVersion verStr of
            Nothing -> pure (Left (InstallError
              ("Could not parse version from edge image output: " <> verOut)))
            Just ver -> do
              -- Tag the edge image with the actual version for consistency
              let versionedImage = "ghcr.io/morloc-project/morloc/morloc-full:" <> showVersion ver
              _ <- readProcessWithExitCode exe
                ["tag", Text.unpack edgeImage, Text.unpack versionedImage] ""
              result <- installVersion engine scope ver
              case result of
                Left err -> pure (Left err)
                Right () -> pure (Right ver)
  where
    strip = reverse . dropWhile isNewline . reverse . dropWhile isNewline
    isNewline c = c == '\n' || c == '\r' || c == ' '
    lastWord s = case words s of
      [] -> s
      ws -> Prelude.last ws

-- | Set a version as the active version in the top-level config.
selectVersion :: Scope -> Version -> IO (Either ManagerError ())
selectVersion scope ver = do
  -- Verify the version is installed
  path <- versionConfigPath scope ver
  vcResult <- readConfig path
  case (vcResult :: Either ManagerError VersionConfig) of
    Left _ -> pure (Left (VersionNotInstalled ver))
    Right _ -> do
      -- Read current config or use defaults
      cfgPath <- configPath scope
      cfg <- readConfig cfgPath
      let baseCfg = case (cfg :: Either ManagerError Config) of
            Right c -> c
            Left _  -> defaultConfig
      -- Update with selected version
      let newCfg = baseCfg
            { configActiveVersion = Just ver
            , configActiveScope = scope
            }
      writeConfig cfgPath newCfg

-- | List all installed versions for a given scope.
--
-- Scans the @versions\/@ directory for subdirectories that parse as
-- version numbers AND contain a valid @config.json@.
listVersions :: Scope -> IO [Version]
listVersions scope = do
  dir <- configDir scope
  let versionsDir = dir </> "versions"
  exists <- doesDirectoryExist versionsDir
  if not exists
    then pure []
    else do
      entries <- listDirectory versionsDir
      let candidates = [ (e, v) | e <- entries, Just v <- [parseVersion e] ]
      -- Only include versions that have a config file (fully installed)
      installed <- filterM' (\(e, _) -> do
        let cfgFile = versionsDir </> e </> "config.json"
        doesFileExist cfgFile
        ) candidates
      pure (map snd installed)
  where
    filterM' _ [] = pure []
    filterM' p (x:xs) = do
      ok <- p x
      rest <- filterM' p xs
      pure (if ok then x : rest else rest)

-- | Resolve the currently active version by checking local scope first,
-- then system scope. Returns 'NoActiveVersion' if nothing is selected.
resolveActiveVersion :: IO (Either ManagerError (Version, Scope))
resolveActiveVersion = do
  mCfg <- readActiveConfig
  case mCfg of
    Nothing -> pure (Left NoActiveVersion)
    Just cfg -> case configActiveVersion cfg of
      Nothing  -> pure (Left NoActiveVersion)
      Just ver -> pure (Right (ver, configActiveScope cfg))

-- | Remove an installed version. Removes:
--
--   * The container image (e.g., @ghcr.io\/morloc-project\/morloc\/morloc-full:0.68.0@)
--   * The version config directory (e.g., @~\/.config\/morloc\/versions\/0.68.0\/@)
--   * The version data directory (e.g., @~\/.local\/share\/morloc\/versions\/0.68.0\/@)
--   * Clears the active version in config if this was the selected version
--
-- The container engine is read from the version config (no need to pass it).
uninstallVersion :: Scope -> Version -> IO (Either ManagerError ())
uninstallVersion scope ver = do
  -- Verify version is installed and read its config (need image + engine)
  vcPath <- versionConfigPath scope ver
  vcResult <- readConfig vcPath
  case (vcResult :: Either ManagerError VersionConfig) of
    Left _ -> pure (Left (VersionNotInstalled ver))
    Right vc -> do
      -- Remove the container image (before deleting config)
      let engine = vcEngine vc
          imageRef = vcImage vc
          exe = engineExecutable engine
      (rmiCode, _, _) <- readProcessWithExitCode exe ["rmi", Text.unpack imageRef] ""
      case rmiCode of
        ExitSuccess   -> hPutStrLn stderr $ "  Removed image: " <> Text.unpack imageRef
        ExitFailure _ -> hPutStrLn stderr $ "  Warning: could not remove image " <> Text.unpack imageRef <> " (may have been removed manually)"
      -- If this is the active version, clear it
      mCfg <- readActiveConfig
      case mCfg of
        Just cfg | configActiveVersion cfg == Just ver -> do
          cfgP <- configPath scope
          let newCfg = cfg { configActiveVersion = Nothing }
          _ <- writeConfig cfgP newCfg
          hPutStrLn stderr "  Cleared active version"
        _ -> pure ()
      -- Remove version config directory
      vcDir <- versionConfigDir scope ver
      vcDirExists <- doesDirectoryExist vcDir
      if vcDirExists
        then do
          removeDirectoryRecursive vcDir
          hPutStrLn stderr $ "  Removed config: " <> vcDir
        else pure ()
      -- Remove version data directory
      vdDir <- versionDataDir scope ver
      vdDirExists <- doesDirectoryExist vdDir
      if vdDirExists
        then do
          removeDirectoryRecursive vdDir
          hPutStrLn stderr $ "  Removed data: " <> vdDir
        else pure ()
      hFlush stderr
      pure (Right ())
