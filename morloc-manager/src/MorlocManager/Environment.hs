{- |
Module      : MorlocManager.Environment
Description : Custom dependency layer management
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Manages custom dependency environments, which are container image layers
built on top of the base @morloc-full@ image. Each environment is defined
by a user-edited Dockerfile that installs additional system packages,
Python libraries, R packages, etc.

@
  Layer architecture:

  +-------------------------------+
  | Custom environment layer      |  morloc-env:0.67.0-ml
  | (pip install scikit-learn)    |  Built from user's Dockerfile
  +-------------------------------+
  | morloc-full:0.67.0            |  Base image from GHCR
  | (compiler + nexus + Python +  |
  |  R + C++ toolchains)          |
  +-------------------------------+

  Environment lifecycle:

  init           edit              build           activate
  +--------+    +--------+       +--------+       +--------+
  | create |    | user   |       | docker |       | set in |
  | stub   |--->| edits  |------>| build  |------>| version|
  | Dfile  |    | Dfile  |       | image  |       | config |
  +--------+    +--------+       +--------+       +--------+

  Files per environment:
    ~/.local/share/morloc/deps/ml.Dockerfile
    ~/.config/morloc/versions/0.67.0/environments/ml.json
@

Rebuild detection uses SHA-256 hashing of the Dockerfile contents. If the
hash matches the last build, the build step is skipped. This prevents
unnecessary image rebuilds when the user runs @morloc-manager env ml@
without editing the Dockerfile.
-}

module MorlocManager.Environment
  ( -- * Initialization
    initEnvironment
    -- * Building
  , buildEnvironment
    -- * Activation
  , activateEnvironment
    -- * Discovery
  , listEnvironments
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import System.Exit (ExitCode(..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , doesDirectoryExist
  , listDirectory
  )
import System.FilePath ((</>), takeBaseName)

import MorlocManager.Types
import MorlocManager.Config
import MorlocManager.Container

-- | Create a new environment by writing a stub Dockerfile.
--
-- The stub Dockerfile uses @ARG CONTAINER_BASE=scratch@ and @FROM $CONTAINER_BASE@
-- so that morloc-manager can inject the correct base image at build time.
-- The user edits the Dockerfile to add their dependencies.
initEnvironment :: Scope -> String -> IO (Either ManagerError FilePath)
initEnvironment scope envName = do
  deps <- depsDir scope
  createDirectoryIfMissing True deps
  let dockerfilePath = deps </> envName <> ".Dockerfile"
  exists <- doesFileExist dockerfilePath
  if exists
    then pure (Left (FreezeError ("Dockerfile already exists: " <> dockerfilePath)))
    else do
      writeFile dockerfilePath $ unlines
        [ "# morloc-manager environment: " <> envName
        , "# Edit this file to add your dependencies, then run:"
        , "#   morloc-manager env " <> envName
        , ""
        , "ARG CONTAINER_BASE=scratch"
        , "FROM ${CONTAINER_BASE}"
        , ""
        , "# Example: install Python packages"
        , "# RUN pip install scikit-learn pandas"
        , ""
        , "# Example: install R packages"
        , "# RUN R -e \"install.packages('ggplot2', repos='https://cloud.r-project.org')\""
        , ""
        , "# Example: install system packages"
        , "# RUN apt-get update && apt-get install -y libfoo-dev"
        ]
      pure (Right dockerfilePath)

-- | Build an environment image from its Dockerfile.
--
-- Skips the build if the Dockerfile has not changed since the last build
-- (detected via SHA-256 hash comparison). The image is tagged as
-- @morloc-env:\<version\>-\<envName\>@.
buildEnvironment
  :: ContainerEngine -> Scope -> Version -> String
  -> IO (Either ManagerError ())
buildEnvironment engine scope ver envName = do
  deps <- depsDir scope
  let dockerfilePath = deps </> envName <> ".Dockerfile"
  exists <- doesFileExist dockerfilePath
  if not exists
    then pure (Left (EnvironmentNotFound (Text.pack envName)))
    else do
      -- Check if rebuild is needed via content hash
      currentHash <- hashFile dockerfilePath
      ecResult <- readEnvironmentConfig scope ver envName
      let needsRebuild = case ecResult of
            Right ec -> ecContentHash ec /= Just currentHash
            Left _   -> True
      if not needsRebuild
        then pure (Right ())
        else do
          -- Determine base image for this version
          vcResult <- readVersionConfig scope ver
          case vcResult of
            Left err -> pure (Left err)
            Right vc -> do
              let tag = "morloc-env:" <> showVersion ver <> "-" <> Text.pack envName
                  buildCfg = BuildConfig
                    { bcDockerfile = dockerfilePath
                    , bcContext    = deps
                    , bcTag        = tag
                    , bcBuildArgs  = [("CONTAINER_BASE", vcImage vc)]
                    }
              (code, _stdout, stderr') <- containerBuild engine buildCfg
              case code of
                ExitFailure n -> pure (Left (EngineError engine n stderr'))
                ExitSuccess -> do
                  let ec = EnvironmentConfig
                        { ecImage       = tag
                        , ecDockerfile  = dockerfilePath
                        , ecContentHash = Just currentHash
                        }
                  writeEnvironmentConfig scope ver envName ec

-- | Set an environment as active for the current version.
--
-- Updates the top-level config's @active_env@ field. The environment
-- must have been built first (its config file must exist).
activateEnvironment :: Scope -> Version -> String -> IO (Either ManagerError ())
activateEnvironment scope ver envName = do
  -- Verify environment exists
  ecResult <- readEnvironmentConfig scope ver envName
  case (ecResult :: Either ManagerError EnvironmentConfig) of
    Left _ -> pure (Left (EnvironmentNotFound (Text.pack envName)))
    Right _ -> do
      cfgPath <- configPath scope
      cfgResult <- readConfig cfgPath
      case (cfgResult :: Either ManagerError Config) of
        Left err -> pure (Left err)
        Right cfg -> do
          let newCfg = cfg { configActiveEnv = Text.pack envName }
          writeConfig cfgPath newCfg

-- | List all environments available for a version.
--
-- Scans both the environments config directory (for built environments with
-- @.json@ config files) and the deps directory (for initialized-but-not-yet-built
-- environments with @.Dockerfile@ files). The @"base"@ environment is always
-- implicitly available.
listEnvironments :: Scope -> Version -> IO [String]
listEnvironments scope ver = do
  -- Built environments (have .json config)
  dir <- versionConfigDir scope ver
  let envDir = dir </> "environments"
  builtEnvs <- do
    exists <- doesDirectoryExist envDir
    if not exists
      then pure []
      else do
        entries <- listDirectory envDir
        pure [ takeBaseName e | e <- entries, hasSuffix ".json" e ]
  -- Initialized environments (have .Dockerfile in deps dir)
  deps <- depsDir scope
  initEnvs <- do
    exists <- doesDirectoryExist deps
    if not exists
      then pure []
      else do
        entries <- listDirectory deps
        pure [ takeBaseName e | e <- entries, hasSuffix ".Dockerfile" e ]
  -- Merge, deduplicate, keep order: base first, then sorted unique names
  let allEnvs = nub (builtEnvs <> initEnvs)
  pure ("base" : allEnvs)
  where
    hasSuffix suffix str = drop (length str - length suffix) str == suffix

-- ======================================================================
-- Internal
-- ======================================================================

-- | Compute the SHA-256 hash of a file's contents, returned as a hex string.
hashFile :: FilePath -> IO Text
hashFile path = do
  contents <- BS.readFile path
  let digest = SHA256.hash contents
  pure (Text.pack (concatMap byteToHex (BS.unpack digest)))
  where
    byteToHex :: Word8 -> String
    byteToHex b =
      let (hi, lo) = b `divMod` 16
      in [hexChar hi, hexChar lo]
    hexChar :: Word8 -> Char
    hexChar n
      | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
