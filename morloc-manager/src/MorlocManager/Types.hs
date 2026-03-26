{- |
Module      : MorlocManager.Types
Description : Core algebraic data types for morloc-manager
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Defines the core domain types used throughout morloc-manager: semantic
versions, installation scopes, container engines, configuration records,
and freeze manifests.

All types derive 'Generic' and have 'ToJSON'/'FromJSON' instances for
JSON configuration persistence. Version ordering follows semantic
versioning (major, then minor, then patch).

@
  Type hierarchy:

  Config                     -- top-level manager config (one per scope)
    +-- Version              -- semantic version (major.minor.patch)
    +-- Scope                -- Local | System
    +-- ContainerEngine      -- Docker | Podman

  VersionConfig              -- per-installed-version config
    +-- image reference
    +-- host data directory
    +-- shared memory size

  EnvironmentConfig          -- per-custom-environment config
    +-- image tag
    +-- Dockerfile path
    +-- content hash

  FreezeManifest             -- auditable frozen state record
    +-- [ModuleEntry]        -- installed modules with checksums
    +-- [ProgramEntry]       -- compiled programs with commands
@
-}

module MorlocManager.Types
  ( -- * Core enumerations
    Scope(..)
  , ContainerEngine(..)
    -- * Version
  , Version(..)
  , showVersion
  , parseVersion
    -- * Configuration
  , Config(..)
  , defaultConfig
  , VersionConfig(..)
  , EnvironmentConfig(..)
    -- * Freeze manifest
  , FreezeManifest(..)
  , ModuleEntry(..)
  , ProgramEntry(..)
    -- * Errors
  , ManagerError(..)
  , renderError
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

-- ======================================================================
-- Core enumerations
-- ======================================================================

-- | Installation scope: per-user or system-wide.
--
-- Local scope stores configuration under @~\/.config\/morloc\/@ and data
-- under @~\/.local\/share\/morloc\/@. System scope uses @\/etc\/morloc\/@
-- and @\/usr\/local\/share\/morloc\/@.
--
-- When resolving the active version, local scope is checked first. If no
-- local version is active, the system scope is consulted as a fallback.
data Scope
  = Local
    -- ^ Per-user installation (default). No elevated privileges required.
  | System
    -- ^ System-wide installation. Requires sudo for writes, but regular
    -- users can read and run.
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Scope where
  toJSON Local  = Aeson.String "local"
  toJSON System = Aeson.String "system"

instance FromJSON Scope where
  parseJSON = Aeson.withText "Scope" $ \t -> case t of
    "local"  -> pure Local
    "system" -> pure System
    _        -> fail $ "Unknown scope: " <> Text.unpack t

-- | Supported container engines. Podman is preferred (rootless by default).
--
-- The manager auto-detects which engine is available at startup, preferring
-- Podman. The user can override via @--container-engine@ or the config file.
-- Engine-specific differences (e.g., Podman's @--userns=keep-id@ for
-- rootless operation) are handled in "MorlocManager.Container".
data ContainerEngine
  = Docker
    -- ^ Docker engine. Requires the Docker daemon and typically group
    -- membership (@docker@ group) for non-root access.
  | Podman
    -- ^ Podman engine (preferred). Rootless by default, no daemon required.
  deriving (Eq, Show, Generic)

instance ToJSON ContainerEngine where
  toJSON Docker = Aeson.String "docker"
  toJSON Podman = Aeson.String "podman"

instance FromJSON ContainerEngine where
  parseJSON = Aeson.withText "ContainerEngine" $ \t -> case t of
    "docker" -> pure Docker
    "podman" -> pure Podman
    _        -> fail $ "Unknown container engine: " <> Text.unpack t

-- ======================================================================
-- Version
-- ======================================================================

-- | Semantic version with major.minor.patch components.
--
-- Ordering follows semantic versioning: major takes precedence over minor,
-- which takes precedence over patch.
--
-- >>> parseVersion "0.67.0"
-- Just (Version 0 67 0)
--
-- >>> showVersion (Version 0 67 0)
-- "0.67.0"
data Version = Version
  { vMajor :: !Int  -- ^ Major version component
  , vMinor :: !Int  -- ^ Minor version component
  , vPatch :: !Int  -- ^ Patch version component
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Version where
  toJSON v = Aeson.String (showVersion v)

instance FromJSON Version where
  parseJSON = Aeson.withText "Version" $ \t ->
    case parseVersion (Text.unpack t) of
      Just v  -> pure v
      Nothing -> fail $ "Invalid version: " <> Text.unpack t

-- | Render a version as @"major.minor.patch"@.
showVersion :: Version -> Text
showVersion (Version ma mi pa) =
  Text.pack (show ma <> "." <> show mi <> "." <> show pa)

-- | Parse a @"major.minor.patch"@ string into a 'Version'.
-- Returns 'Nothing' if the string does not match the expected format.
parseVersion :: String -> Maybe Version
parseVersion s = case break (== '.') s of
  (ma, '.':rest) -> case break (== '.') rest of
    (mi, '.':pa) -> Version <$> readMaybe ma <*> readMaybe mi <*> readMaybe pa
    _ -> Nothing
  _ -> Nothing

-- ======================================================================
-- Configuration
-- ======================================================================

-- | Top-level manager configuration. One file per scope (local or system).
--
-- @
--   Config file locations:
--   ~/.config/morloc/config.json          (local scope)
--   /etc/morloc/config.json               (system scope)
-- @
--
-- When no version is selected, 'configActiveVersion' is 'Nothing' and most
-- commands will fail with a helpful error message.
data Config = Config
  { configActiveVersion :: Maybe Version
    -- ^ Currently selected morloc version, or 'Nothing' if none installed
  , configActiveScope :: Scope
    -- ^ Whether using local (per-user) or system-wide installation
  , configActiveEnv :: Text
    -- ^ Active dependency environment name (@"base"@ or a custom name)
  , configEngine :: ContainerEngine
    -- ^ Container engine to use for all operations
  } deriving (Show, Generic)

instance ToJSON Config where
  toJSON c = Aeson.object
    [ "active_version" .= configActiveVersion c
    , "active_scope"   .= configActiveScope c
    , "active_env"     .= configActiveEnv c
    , "engine"         .= configEngine c
    ]

instance FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o -> Config
    <$> o .:? "active_version"
    <*> o .:? "active_scope"  .!= Local
    <*> o .:? "active_env"    .!= "base"
    <*> o .:? "engine"        .!= Podman

-- | Default configuration for a fresh installation.
defaultConfig :: Config
defaultConfig = Config
  { configActiveVersion = Nothing
  , configActiveScope   = Local
  , configActiveEnv     = "base"
  , configEngine        = Podman
  }

-- | Per-version configuration. One file per installed morloc version.
--
-- @
--   ~/.config/morloc/versions/\<ver\>/config.json
-- @
data VersionConfig = VersionConfig
  { vcImage :: Text
    -- ^ Container image reference
    -- (e.g., @ghcr.io\/morloc-project\/morloc\/morloc-full:0.67.0@)
  , vcHostDir :: FilePath
    -- ^ Host-side data directory for this version's persistent state
  , vcShmSize :: Text
    -- ^ Shared memory size for containers (default @"512m"@)
  , vcEngine :: ContainerEngine
    -- ^ Container engine used when this version was installed
  } deriving (Show, Generic)

instance ToJSON VersionConfig where
  toJSON vc = Aeson.object
    [ "image"    .= vcImage vc
    , "host_dir" .= vcHostDir vc
    , "shm_size" .= vcShmSize vc
    , "engine"   .= vcEngine vc
    ]

instance FromJSON VersionConfig where
  parseJSON = Aeson.withObject "VersionConfig" $ \o -> VersionConfig
    <$> o .:  "image"
    <*> o .:  "host_dir"
    <*> o .:? "shm_size" .!= "512m"
    <*> o .:? "engine"   .!= Podman

-- | Per-custom-environment configuration.
--
-- Custom environments are dependency layers built on top of the base morloc
-- image. Each environment has a Dockerfile and produces a tagged image.
--
-- @
--   ~/.config/morloc/versions/\<ver\>/environments/\<name\>.json
-- @
data EnvironmentConfig = EnvironmentConfig
  { ecImage :: Text
    -- ^ Built image tag (e.g., @morloc-env:0.67.0-ml@)
  , ecDockerfile :: FilePath
    -- ^ Path to the environment's Dockerfile
  , ecContentHash :: Maybe Text
    -- ^ SHA-256 hash of the Dockerfile at last build time. Used to detect
    -- changes and skip unnecessary rebuilds. 'Nothing' if never built.
  } deriving (Show, Generic)

instance ToJSON EnvironmentConfig where
  toJSON ec = Aeson.object
    [ "image"        .= ecImage ec
    , "dockerfile"   .= ecDockerfile ec
    , "content_hash" .= ecContentHash ec
    ]

instance FromJSON EnvironmentConfig where
  parseJSON = Aeson.withObject "EnvironmentConfig" $ \o -> EnvironmentConfig
    <$> o .:  "image"
    <*> o .:  "dockerfile"
    <*> o .:? "content_hash"

-- ======================================================================
-- Freeze manifest
-- ======================================================================

-- | Auditable record of a frozen morloc state.
--
-- Written alongside the state tarball during freeze. Captures enough
-- information to reproduce the exact same serve image and to audit what
-- functions are available at runtime.
--
-- @
--   Builder Container          Frozen Artifact          Serve Image
--   +----------------+        +----------------+      +----------------+
--   | lib/  (modules)|--tar-->| state.tar.gz   |--  ->| read-only      |
--   | fdb/ (manifest)|        | manifest.json  | COPY | fdb + lib      |
--   | bin/ (programs)|        +----------------+      | nexus router   |
--   | pool files     |                                +----------------+
--   +----------------+
-- @
data FreezeManifest = FreezeManifest
  { fmMorlocVersion :: Version
    -- ^ Morloc compiler version used during the build phase
  , fmFrozenAt :: UTCTime
    -- ^ Timestamp when the freeze was performed
  , fmModules :: [ModuleEntry]
    -- ^ Installed modules with names, versions, and SHA-256 checksums
  , fmPrograms :: [ProgramEntry]
    -- ^ Compiled programs with their exported command names
  , fmBaseImage :: Text
    -- ^ Container base image reference used during build
  , fmEnvLayer :: Maybe Text
    -- ^ Custom dependency layer name, if one was active during build
  } deriving (Show, Generic)

instance ToJSON FreezeManifest where
  toJSON fm = Aeson.object
    [ "morloc_version" .= fmMorlocVersion fm
    , "frozen_at"      .= fmFrozenAt fm
    , "modules"        .= fmModules fm
    , "programs"       .= fmPrograms fm
    , "base_image"     .= fmBaseImage fm
    , "env_layer"      .= fmEnvLayer fm
    ]

instance FromJSON FreezeManifest where
  parseJSON = Aeson.withObject "FreezeManifest" $ \o -> FreezeManifest
    <$> o .:  "morloc_version"
    <*> o .:  "frozen_at"
    <*> o .:  "modules"
    <*> o .:  "programs"
    <*> o .:  "base_image"
    <*> o .:? "env_layer"

-- | An installed morloc module recorded in a freeze manifest.
data ModuleEntry = ModuleEntry
  { meName :: Text
    -- ^ Module name (e.g., @"root"@, @"math"@)
  , meVersion :: Maybe Text
    -- ^ Module version string, if available
  , meSha256 :: Text
    -- ^ SHA-256 hash of the module's source directory
  } deriving (Show, Generic)

instance ToJSON ModuleEntry where
  toJSON me = Aeson.object
    [ "name"    .= meName me
    , "version" .= meVersion me
    , "sha256"  .= meSha256 me
    ]

instance FromJSON ModuleEntry where
  parseJSON = Aeson.withObject "ModuleEntry" $ \o -> ModuleEntry
    <$> o .:  "name"
    <*> o .:? "version"
    <*> o .:  "sha256"

-- | A compiled morloc program recorded in a freeze manifest.
data ProgramEntry = ProgramEntry
  { peName :: Text
    -- ^ Program name (becomes the executable name)
  , peCommands :: [Text]
    -- ^ Exported command names (each becomes a CLI subcommand)
  } deriving (Show, Generic)

instance ToJSON ProgramEntry where
  toJSON pe = Aeson.object
    [ "name"     .= peName pe
    , "commands" .= peCommands pe
    ]

instance FromJSON ProgramEntry where
  parseJSON = Aeson.withObject "ProgramEntry" $ \o -> ProgramEntry
    <$> o .: "name"
    <*> o .: "commands"

-- ======================================================================
-- Errors
-- ======================================================================

-- | Errors that can occur during morloc-manager operations.
data ManagerError
  = ConfigNotFound FilePath
    -- ^ Expected config file does not exist
  | ConfigPermissionDenied FilePath
    -- ^ Config file exists but is not readable (e.g., 0600 owned by root)
  | ConfigParseError FilePath String
    -- ^ Config file exists but contains invalid JSON
  | NoActiveVersion
    -- ^ No morloc version is currently selected
  | VersionNotInstalled Version
    -- ^ Requested version is not installed in any scope
  | InvalidVersion String
    -- ^ Version string does not match MAJOR.MINOR.PATCH format
  | NoCommand
    -- ^ No command specified for @run@ (need --shell or a command)
  | EngineNotFound
    -- ^ Neither docker nor podman found on PATH
  | EngineError ContainerEngine Int Text
    -- ^ Container engine command failed with exit code and stderr
  | EnvironmentNotFound Text
    -- ^ Named environment does not exist
  | InstallError String
    -- ^ Error during version installation (pull, init, version detection)
  | UninstallError String
    -- ^ Error during version uninstallation
  | EnvError String
    -- ^ Error during environment management
  | FreezeError String
    -- ^ Error during state freeze (tar, checksum, etc.)
  | SELinuxError String
    -- ^ SELinux-related error (unsafe path, relabeling failure)
  deriving (Show, Eq)


-- | Render a 'ManagerError' as a user-facing error message.
renderError :: ManagerError -> Text
renderError (ConfigNotFound path) =
  "Configuration not found: " <> Text.pack path
renderError (ConfigPermissionDenied path) =
  "Permission denied: " <> Text.pack path <> ". Check file ownership and permissions."
renderError (ConfigParseError path msg) =
  "Invalid configuration in " <> Text.pack path <> ": " <> Text.pack msg
renderError NoActiveVersion =
  "No morloc version selected. Run: morloc-manager select <version>"
renderError (VersionNotInstalled v) =
  "Version " <> showVersion v <> " is not installed in any scope. Run: morloc-manager info"
renderError (InvalidVersion s) =
  "Invalid version: " <> Text.pack s <> ". Expected format: MAJOR.MINOR.PATCH"
renderError NoCommand =
  "No command specified. Use --shell or provide a command after --."
renderError EngineNotFound =
  "No container engine found. Install podman or docker."
renderError (EngineError engine code stderr') =
  "Container engine (" <> Text.pack (show engine) <> ") failed with exit code "
  <> Text.pack (show code) <> ":\n" <> stderr'
renderError (EnvironmentNotFound name) =
  "Environment not found: " <> name
renderError (InstallError msg) =
  "Install failed: " <> Text.pack msg
renderError (UninstallError msg) =
  "Uninstall failed: " <> Text.pack msg
renderError (EnvError msg) =
  "Environment error: " <> Text.pack msg
renderError (FreezeError msg) =
  "Freeze failed: " <> Text.pack msg
renderError (SELinuxError msg) =
  "SELinux error: " <> Text.pack msg
