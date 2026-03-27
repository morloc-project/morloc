{- |
Module      : MorlocManager.Config
Description : Atomic JSON configuration with file locking
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Reads and writes morloc-manager configuration as JSON files with atomic
write semantics and advisory file locking to prevent concurrent corruption.

Atomic writes use the temp-file-and-rename pattern: data is written to a
temporary file in the same directory, then renamed over the target. Since
@rename(2)@ is atomic on POSIX filesystems, readers never see partial writes.

Advisory locking via @flock(2)@ prevents two morloc-manager processes from
writing the same config file simultaneously. Readers do not acquire locks
(they may see the old or new version, but never a corrupt intermediate).

@
  Scope resolution order:

  readActiveConfig
    |
    +--> try local:  ~/.config/morloc/config.json
    |      |
    |      +--> found? return it
    |      |
    |      +--> not found? fall through
    |
    +--> try system: /etc/morloc/config.json
           |
           +--> found? return it
           |
           +--> not found? return Nothing
@

@
  Config file layout:

  Local scope:
    ~/.config/morloc/
      config.json                           -- top-level config
      versions/
        0.67.0/
          config.json                       -- per-version config
          environments/
            base.json                       -- default environment
            ml.json                         -- custom environment
        0.58.3/
          config.json
          ...

  System scope:
    /etc/morloc/
      config.json
      versions/
        ...
@
-}

module MorlocManager.Config
  ( -- * Reading configuration
    readConfig
  , readActiveConfig
  , readVersionConfig
  , readEnvironmentConfig
    -- * Writing configuration
  , writeConfig
  , writeVersionConfig
  , writeEnvironmentConfig
    -- * Reading workspace configuration
  , readWorkspaceConfig
  , writeWorkspaceConfig
    -- * Path utilities
  , configDir
  , configPath
  , versionConfigDir
  , versionConfigPath
  , environmentConfigPath
  , dataDir
  , versionDataDir
  , depsDir
    -- * Workspace paths
  , workspaceConfigDir
  , workspaceConfigPath
  , workspaceDataDir
  , listWorkspaces
    -- * Flags files
  , readFlagsFile
  , globalFlagsPath
  , envFlagsPath
    -- * Scope utilities
  , findInstalledScope
  , findWorkspaceScope
  , requireScopeConfig
  , fixSystemPerms
  ) where

import Control.Exception (bracket, IOException, try)
import System.IO.Error (isPermissionError, isDoesNotExistError)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getXdgDirectory
  , listDirectory
  , XdgDirectory(..)
  , renameFile
  )
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openTempFile)
import System.IO (SeekMode(..))
import System.Posix.IO
  ( openFd, closeFd
  , OpenMode(..)
  , defaultFileFlags
  , OpenFileFlags(..)
  , setLock
  , LockRequest(..)
  )
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

import MorlocManager.Types

-- ======================================================================
-- Path utilities
-- ======================================================================

-- | Root configuration directory for a given scope.
--
-- @
--   Local:  ~/.config/morloc/
--   System: /etc/morloc/
-- @
configDir :: Scope -> IO FilePath
configDir Local  = getXdgDirectory XdgConfig "morloc"
configDir System = pure "/etc/morloc"

-- | Path to the top-level config file for a given scope.
configPath :: Scope -> IO FilePath
configPath scope = do
  dir <- configDir scope
  pure (dir </> "config.json")

-- | Directory for a specific version's configuration.
--
-- @
--   ~/.config/morloc/versions/0.67.0/
-- @
versionConfigDir :: Scope -> Version -> IO FilePath
versionConfigDir scope ver = do
  dir <- configDir scope
  pure (dir </> "versions" </> versionStr ver)
  where
    versionStr v = show (vMajor v) <> "." <> show (vMinor v) <> "." <> show (vPatch v)

-- | Path to a version's config file.
versionConfigPath :: Scope -> Version -> IO FilePath
versionConfigPath scope ver = do
  dir <- versionConfigDir scope ver
  pure (dir </> "config.json")

-- | Path to an environment's config file within a version.
environmentConfigPath :: Scope -> Version -> String -> IO FilePath
environmentConfigPath scope ver envName = do
  dir <- versionConfigDir scope ver
  pure (dir </> "environments" </> envName <> ".json")

-- | Root data directory for a given scope.
--
-- @
--   Local:  ~/.local/share/morloc/
--   System: /usr/local/share/morloc/
-- @
dataDir :: Scope -> IO FilePath
dataDir Local  = getXdgDirectory XdgData "morloc"
dataDir System = pure "/usr/local/share/morloc"

-- | Data directory for a specific installed version.
--
-- @
--   ~/.local/share/morloc/versions/0.67.0/
-- @
versionDataDir :: Scope -> Version -> IO FilePath
versionDataDir scope ver = do
  dir <- dataDir scope
  pure (dir </> "versions" </> versionStr ver)
  where
    versionStr v = show (vMajor v) <> "." <> show (vMinor v) <> "." <> show (vPatch v)

-- | Directory for custom environment Dockerfiles.
--
-- @
--   ~/.local/share/morloc/deps/
-- @
depsDir :: Scope -> IO FilePath
depsDir scope = do
  dir <- dataDir scope
  pure (dir </> "deps")

-- ======================================================================
-- Workspace paths
-- ======================================================================

-- | Configuration directory for a named workspace.
--
-- @
--   ~/.config/morloc/workspaces/foo/
-- @
workspaceConfigDir :: Scope -> Text -> IO FilePath
workspaceConfigDir scope name = do
  dir <- configDir scope
  pure (dir </> "workspaces" </> Text.unpack name)

-- | Path to a workspace's config file.
workspaceConfigPath :: Scope -> Text -> IO FilePath
workspaceConfigPath scope name = do
  dir <- workspaceConfigDir scope name
  pure (dir </> "config.json")

-- | Data directory for a named workspace.
--
-- @
--   ~/.local/share/morloc/workspaces/foo/
-- @
workspaceDataDir :: Scope -> Text -> IO FilePath
workspaceDataDir scope name = do
  dir <- dataDir scope
  pure (dir </> "workspaces" </> Text.unpack name)

-- | List all workspaces for a given scope.
--
-- Scans the @workspaces\/@ directory for subdirectories that
-- contain a valid @config.json@.
listWorkspaces :: Scope -> IO [Text]
listWorkspaces scope = do
  dir <- configDir scope
  let wsDir = dir </> "workspaces"
  exists <- doesDirectoryExist wsDir
  if not exists
    then pure []
    else do
      entries <- listDirectory wsDir
      installed <- filterM' (\e -> do
        let cfgFile = wsDir </> e </> "config.json"
        doesFileExist cfgFile
        ) entries
      pure (map Text.pack installed)
  where
    filterM' _ [] = pure []
    filterM' p (x:xs) = do
      ok <- p x
      rest <- filterM' p xs
      pure (if ok then x : rest else rest)

-- ======================================================================
-- Reading configuration
-- ======================================================================

-- | Read a JSON config file. Distinguishes between:
--
--   * File not found -> 'ConfigNotFound'
--   * Permission denied -> 'ConfigPermissionDenied'
--   * Invalid JSON -> 'ConfigParseError'
readConfig :: FromJSON a => FilePath -> IO (Either ManagerError a)
readConfig path = do
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case result of
    Left err
      | isPermissionError err   -> pure (Left (ConfigPermissionDenied path))
      | isDoesNotExistError err -> pure (Left (ConfigNotFound path))
      | otherwise               -> pure (Left (ConfigNotFound path))
    Right bytes -> case Aeson.eitherDecode bytes of
      Left msg  -> pure (Left (ConfigParseError path msg))
      Right val -> pure (Right val)

-- | Read the active configuration by checking local scope first, then system.
--
-- This implements the scope resolution order: local config takes priority
-- over system config. Returns 'Nothing' if no config exists in either scope.
readActiveConfig :: IO (Maybe Config)
readActiveConfig = do
  localPath <- configPath Local
  result <- readConfig localPath
  case result of
    Right cfg -> pure (Just cfg)
    Left _ -> do
      systemPath <- configPath System
      result' <- readConfig systemPath
      case result' of
        Right cfg -> pure (Just cfg)
        Left _    -> pure Nothing

-- | Read the configuration for a specific installed version.
readVersionConfig :: Scope -> Version -> IO (Either ManagerError VersionConfig)
readVersionConfig scope ver = do
  path <- versionConfigPath scope ver
  readConfig path

-- | Read the configuration for a specific environment within a version.
readEnvironmentConfig :: Scope -> Version -> String -> IO (Either ManagerError EnvironmentConfig)
readEnvironmentConfig scope ver envName = do
  path <- environmentConfigPath scope ver envName
  readConfig path

-- | Read the configuration for a named workspace.
readWorkspaceConfig :: Scope -> Text -> IO (Either ManagerError WorkspaceConfig)
readWorkspaceConfig scope name = do
  path <- workspaceConfigPath scope name
  readConfig path

-- | Write a workspace config file, creating parent directories as needed.
writeWorkspaceConfig :: Scope -> Text -> WorkspaceConfig -> IO (Either ManagerError ())
writeWorkspaceConfig scope name wc = do
  path <- workspaceConfigPath scope name
  writeConfig path wc

-- ======================================================================
-- Writing configuration
-- ======================================================================

-- | Atomically write a JSON value to a config file.
--
-- Uses the temp-file-and-rename pattern:
--
-- 1. Create a temporary file in the same directory as the target
-- 2. Write the JSON content to the temporary file
-- 3. Rename the temporary file over the target (atomic on POSIX)
--
-- An advisory @flock(2)@ write lock is held during the operation to prevent
-- concurrent writers from interleaving. The lock file is @\<target\>.lock@.
writeConfig :: ToJSON a => FilePath -> a -> IO (Either ManagerError ())
writeConfig path val = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  bestEffortChmod 0o755 dir  -- world-readable+executable for system scope
  result <- try (withFileLock (path <> ".lock") $ do
    (tmpPath, tmpHandle) <- openTempFile dir "config.tmp"
    BL.hPut tmpHandle (Aeson.encode val)
    hClose tmpHandle
    renameFile tmpPath path
    bestEffortChmod 0o644 path  -- world-readable for system-scope multi-user access
    ) :: IO (Either IOException ())
  case result of
    Left err -> pure (Left (ConfigParseError path (show err)))
    Right () -> pure (Right ())

-- | Write a version config file, creating parent directories as needed.
writeVersionConfig :: Scope -> Version -> VersionConfig -> IO (Either ManagerError ())
writeVersionConfig scope ver vc = do
  path <- versionConfigPath scope ver
  writeConfig path vc

-- | Write an environment config file, creating parent directories as needed.
writeEnvironmentConfig :: Scope -> Version -> String -> EnvironmentConfig -> IO (Either ManagerError ())
writeEnvironmentConfig scope ver envName ec = do
  path <- environmentConfigPath scope ver envName
  writeConfig path ec

-- ======================================================================
-- File locking (internal)
-- ======================================================================

-- | Acquire an advisory write lock on the given path for the duration of
-- an action. The lock file is created if it does not exist.
--
-- Uses POSIX @fcntl(2)@ file locking (via 'setLock') which is released
-- automatically when the file descriptor is closed.
withFileLock :: FilePath -> IO a -> IO a
withFileLock lockPath action = do
  createDirectoryIfMissing True (takeDirectory lockPath)
  bracket
    (do fd <- openFd lockPath WriteOnly defaultFileFlags
                { creat = Just lockMode }
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
        pure fd
    )
    closeFd
    (\_ -> action)
  where
    -- World-readable so non-root users can acquire locks on system-scope
    -- config files. The lock is advisory (fcntl), so read permission is
    -- sufficient for other users to observe the lock state.
    lockMode :: FileMode
    lockMode = 0o644

-- ======================================================================
-- Flags files
-- ======================================================================

-- | Read a flags file, returning extra CLI flags as a list of strings.
--
-- Flags files contain one flag per line. Lines starting with @#@ are
-- comments. Leading/trailing whitespace is stripped. Empty lines are
-- skipped.
--
-- @
--   # morloc.flags example
--   --gpus all
--   -v /data/models:/models
-- @
readFlagsFile :: FilePath -> IO [String]
readFlagsFile path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      contents <- readFile path
      pure [ stripped
           | line <- lines contents
           , let stripped = strip line
           , not (null stripped)
           , not (startsWith '#' stripped)
           ]
  where
    strip = reverse . dropWhile isSpace' . reverse . dropWhile isSpace'
    isSpace' c = c == ' ' || c == '\t'
    startsWith _ [] = False
    startsWith c (x:_) = c == x

-- | Path to the global flags file for a scope.
--
-- @
--   ~/.local/share/morloc/morloc.flags
-- @
globalFlagsPath :: Scope -> IO FilePath
globalFlagsPath scope = do
  dir <- dataDir scope
  pure (dir </> "morloc.flags")

-- | Path to an environment-specific flags file within a version.
--
-- @
--   ~/.config/morloc/versions/0.67.0/environments/ml.flags
-- @
envFlagsPath :: Scope -> Version -> String -> IO FilePath
envFlagsPath scope ver envName = do
  dir <- versionConfigDir scope ver
  pure (dir </> "environments" </> envName <> ".flags")

-- ======================================================================
-- Scope utilities
-- ======================================================================

-- | Find which scope has a version installed, checking Local first.
-- Returns 'Nothing' if the version is not installed in either scope.
findInstalledScope :: Version -> IO (Maybe Scope)
findInstalledScope ver = do
  localPath <- versionConfigPath Local ver
  localResult <- try (BL.readFile localPath) :: IO (Either IOException BL.ByteString)
  case localResult of
    Right _ -> pure (Just Local)
    Left _ -> do
      sysPath <- versionConfigPath System ver
      sysResult <- try (BL.readFile sysPath) :: IO (Either IOException BL.ByteString)
      case sysResult of
        Right _ -> pure (Just System)
        Left _  -> pure Nothing

-- | Find which scope has a workspace, checking Local first.
findWorkspaceScope :: Text -> IO (Maybe Scope)
findWorkspaceScope name = do
  localPath <- workspaceConfigPath Local name
  localResult <- try (BL.readFile localPath) :: IO (Either IOException BL.ByteString)
  case localResult of
    Right _ -> pure (Just Local)
    Left _ -> do
      sysPath <- workspaceConfigPath System name
      sysResult <- try (BL.readFile sysPath) :: IO (Either IOException BL.ByteString)
      case sysResult of
        Right _ -> pure (Just System)
        Left _  -> pure Nothing

-- | Require that a scope's config exists. Returns 'SetupNotComplete'
-- if the config file is not found (distinguishing from permission or
-- parse errors which are returned as-is).
requireScopeConfig :: Scope -> IO (Either ManagerError Config)
requireScopeConfig scope = do
  path <- configPath scope
  result <- readConfig path
  case result of
    Right cfg              -> pure (Right cfg)
    Left (ConfigNotFound _) -> pure (Left (SetupNotComplete scope))
    Left err               -> pure (Left err)

-- | Set world-readable permissions on a system-scope config file.
-- Call this after 'writeConfig' for any System-scope path so that
-- non-root users can read the configuration.
fixSystemPerms :: FilePath -> IO ()
fixSystemPerms = bestEffortChmod 0o644

-- | Best-effort chmod. Silently ignores failures (e.g., non-root user
-- cannot change permissions on files they don't own).
bestEffortChmod :: FileMode -> FilePath -> IO ()
bestEffortChmod mode path = do
  result <- try (setFileMode path mode) :: IO (Either IOException ())
  case result of
    Right () -> pure ()
    Left _   -> pure ()
