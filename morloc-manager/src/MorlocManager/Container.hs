{- |
Module      : MorlocManager.Container
Description : Typed abstraction over docker/podman CLI
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides a typed interface to container operations (run, build, pull, stop,
etc.) that shells out to the @docker@ or @podman@ CLI. Each operation takes
a configuration record and constructs the CLI invocation from its fields --
no raw string assembly is exposed.

The docker and podman CLIs are the most stable public interface for container
engines (backwards-compatible for over a decade). Engine-specific differences
are handled internally:

  * Podman rootless: adds @--userns=keep-id@ so files created inside the
    container are owned by the invoking user, not root.
  * Docker: adds @--user UID:GID@ for the same effect.
  * SELinux: volume mounts receive a @:z@ suffix on SELinux-enforcing
    systems (delegated to "MorlocManager.SELinux").

@
  Typed config to CLI invocation:

  RunConfig                     docker/podman run
  +---------+                   +------------------------------------------+
  | image   |---+-------------->| podman run                               |
  | mounts  |---+  construct    |   --userns=keep-id                       |
  | ports   |---+  CLI args     |   -v /host/data:/container/data:z        |
  | env     |---+-------------->|   -p 8080:8080                           |
  | readOnly|---+               |   -e MORLOC_HOME=/home/morloc            |
  | command |---+               |   --read-only                            |
  +---------+                   |   ghcr.io/.../morloc-full:0.67.0         |
                                |   morloc make -o svc service.loc         |
                                +------------------------------------------+
@
-}

module MorlocManager.Container
  ( -- * Configuration records
    RunConfig(..)
  , defaultRunConfig
  , BuildConfig(..)
    -- * Operations
  , containerRun
  , containerRunPassthrough
  , containerBuild
  , containerPull
  , containerStop
  , containerRemove
  , containerImages
    -- * Engine detection
  , detectEngine
  , engineExecutable
    -- * Internals (for testing)
  , engineSpecificRunFlags
  , buildRunArgs
  , buildBuildArgs
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hPutStrLn, hFlush, stderr)
import System.Process
  ( readProcessWithExitCode
  , createProcess
  , proc
  , waitForProcess
  , StdStream(..)
  , std_in, std_out, std_err
  , delegate_ctlc
  )
import System.Directory (findExecutable)
import System.Posix.User (getEffectiveUserID, getEffectiveGroupID)

import MorlocManager.Types (ContainerEngine(..), ManagerError(..))

-- ======================================================================
-- Configuration records
-- ======================================================================

-- | Configuration for @docker\/podman run@. Every field maps to one or more
-- CLI flags. No flags are constructed from raw strings -- the typed fields
-- prevent malformed invocations.
data RunConfig = RunConfig
  { rcImage :: Text
    -- ^ Container image to run (e.g., @ghcr.io\/.../morloc-full:0.67.0@)
  , rcBindMounts :: [(FilePath, FilePath)]
    -- ^ Volume bind mounts as @(host_path, container_path)@ pairs.
    -- SELinux @:z@ suffix is appended automatically when needed.
  , rcPorts :: [(Int, Int)]
    -- ^ Port mappings as @(host_port, container_port)@ pairs.
  , rcEnv :: [(Text, Text)]
    -- ^ Environment variables to set inside the container.
  , rcReadOnly :: Bool
    -- ^ If 'True', mount the root filesystem as read-only (@--read-only@).
  , rcInteractive :: Bool
    -- ^ If 'True', allocate a pseudo-TTY and keep stdin open (@-it@).
  , rcRemoveAfter :: Bool
    -- ^ If 'True', remove the container after it exits (@--rm@).
  , rcName :: Maybe Text
    -- ^ Optional container name (@--name@).
  , rcShmSize :: Maybe Text
    -- ^ Shared memory size (e.g., @"512m"@). Maps to @--shm-size@.
  , rcCommand :: Maybe [Text]
    -- ^ Command and arguments to execute inside the container.
    -- If 'Nothing', the image's default entrypoint runs.
  , rcWorkDir :: Maybe FilePath
    -- ^ Working directory inside the container (@-w@).
  , rcSELinuxSuffix :: String
    -- ^ SELinux volume suffix, either @":z"@ or @""@. Applied to all
    -- bind mounts. Determined by 'MorlocManager.SELinux.volumeSuffix'.
  , rcExtraFlags :: [Text]
    -- ^ Escape hatch: additional CLI flags passed verbatim.
    -- Use sparingly -- prefer typed fields.
  } deriving (Show)

-- | Sensible defaults: no mounts, no ports, non-interactive, auto-remove.
defaultRunConfig :: Text -> RunConfig
defaultRunConfig image = RunConfig
  { rcImage       = image
  , rcBindMounts  = []
  , rcPorts       = []
  , rcEnv         = []
  , rcReadOnly    = False
  , rcInteractive = False
  , rcRemoveAfter = True
  , rcName          = Nothing
  , rcShmSize       = Nothing
  , rcCommand       = Nothing
  , rcWorkDir       = Nothing
  , rcSELinuxSuffix = ""
  , rcExtraFlags    = []
  }

-- | Configuration for @docker\/podman build@.
data BuildConfig = BuildConfig
  { bcDockerfile :: FilePath
    -- ^ Path to the Dockerfile
  , bcContext :: FilePath
    -- ^ Build context directory (usually the directory containing the Dockerfile)
  , bcTag :: Text
    -- ^ Image tag to apply (e.g., @morloc-env:0.67.0-ml@)
  , bcBuildArgs :: [(Text, Text)]
    -- ^ Build-time variables passed via @--build-arg KEY=VALUE@
  } deriving (Show)

-- ======================================================================
-- Engine detection
-- ======================================================================

-- | Auto-detect which container engine is available.
--
-- Prefers Podman (rootless by default). Falls back to Docker.
-- Returns 'EngineNotFound' if neither is on PATH.
detectEngine :: IO (Either ManagerError ContainerEngine)
detectEngine = do
  podman <- findExecutable "podman"
  case podman of
    Just _  -> pure (Right Podman)
    Nothing -> do
      docker <- findExecutable "docker"
      case docker of
        Just _  -> pure (Right Docker)
        Nothing -> pure (Left EngineNotFound)

-- | Get the CLI executable name for an engine.
engineExecutable :: ContainerEngine -> String
engineExecutable Docker = "docker"
engineExecutable Podman = "podman"

-- ======================================================================
-- Operations
-- ======================================================================

-- | Run a container with the given configuration.
--
-- Constructs the full CLI invocation from 'RunConfig' fields, handling
-- engine-specific differences (Podman rootless, Docker user mapping).
-- Returns stdout and stderr as 'Text'.
containerRun :: ContainerEngine -> RunConfig -> IO (ExitCode, Text, Text)
containerRun engine cfg = do
  let exe = engineExecutable engine
  extraEngineFlags <- engineSpecificRunFlagsIO engine
  let args = buildRunArgs engine extraEngineFlags cfg
  runProcess exe args

-- | Run a container with stdin/stdout/stderr passed through to the terminal.
--
-- Unlike 'containerRun', this does not capture output — it inherits the
-- parent process's file descriptors. Use this for interactive commands
-- (@--shell@) and for commands whose output should stream to the user.
--
-- Uses 'createProcess' with 'Inherit' delegates for all three handles
-- and 'delegate_ctlc = True' so that Ctrl-C reaches the container
-- process rather than killing morloc-manager.
containerRunPassthrough :: ContainerEngine -> Bool -> RunConfig -> IO ExitCode
containerRunPassthrough engine verbose cfg = do
  let exe = engineExecutable engine
  extraEngineFlags <- engineSpecificRunFlagsIO engine
  let args = buildRunArgs engine extraEngineFlags cfg
  -- Print the command when verbose mode is enabled
  whenIO' verbose $ do
    hPutStrLn stderr $ "[morloc-manager] " <> exe <> " " <> unwords (map quote args)
    hFlush stderr
  let cp = (proc exe args)
        { std_in  = Inherit
        , std_out = Inherit
        , std_err = Inherit
        , delegate_ctlc = True
        }
  (_, _, _, ph) <- createProcess cp
  waitForProcess ph
  where
    -- Quote arguments that contain spaces for readability
    quote s | ' ' `elem` s = "'" <> s <> "'"
            | otherwise     = s

-- | Build a container image from a Dockerfile.
-- Stderr passes through to the user (build progress).
containerBuild :: ContainerEngine -> BuildConfig -> IO (ExitCode, Text, Text)
containerBuild engine cfg = do
  let exe = engineExecutable engine
      args = buildBuildArgs cfg
  runProcessPassStderr exe args

-- | Pull a container image from a registry.
-- Stderr passes through to the user (pull progress).
containerPull :: ContainerEngine -> Text -> IO (ExitCode, Text, Text)
containerPull engine image = do
  let exe = engineExecutable engine
  runProcessPassStderr exe ["pull", Text.unpack image]

-- | Stop a running container by name or ID.
containerStop :: ContainerEngine -> Text -> IO ExitCode
containerStop engine nameOrId = do
  let exe = engineExecutable engine
  (code, _, _) <- runProcess exe ["stop", Text.unpack nameOrId]
  pure code

-- | Remove a container by name or ID.
containerRemove :: ContainerEngine -> Text -> IO ExitCode
containerRemove engine nameOrId = do
  let exe = engineExecutable engine
  (code, _, _) <- runProcess exe ["rm", "-f", Text.unpack nameOrId]
  pure code

-- | List images matching an optional filter.
-- Returns the raw output text (one image per line in default format).
containerImages :: ContainerEngine -> Maybe Text -> IO (ExitCode, Text, Text)
containerImages engine mFilter = do
  let exe = engineExecutable engine
      args = ["images"] ++ maybe [] (\f -> ["--filter", Text.unpack f]) mFilter
  runProcess exe args

-- ======================================================================
-- CLI argument construction (internal)
-- ======================================================================

-- | Build the argument list for @docker\/podman run@.
--
-- @
--   buildRunArgs Podman cfg
--   ==> ["run", "--userns=keep-id", "--rm", "-it",
--        "--shm-size", "512m", "-w", "/home/user/project",
--        "-v", "/host/data:/container/data:z",
--        "-e", "HOME=/home/user",
--        "ghcr.io/.../morloc-full:0.67.0",
--        "morloc", "make", "-o", "svc", "svc.loc"]
-- @
buildRunArgs :: ContainerEngine -> [String] -> RunConfig -> [String]
buildRunArgs _engine extraEngineFlags cfg = concat
  [ ["run"]
  , extraEngineFlags
  , if rcRemoveAfter cfg then ["--rm"] else []
  , if rcReadOnly cfg then ["--read-only"] else []
  , if rcInteractive cfg then ["-it"] else []
  , maybe [] (\n -> ["--name", Text.unpack n]) (rcName cfg)
  , maybe [] (\s -> ["--shm-size", Text.unpack s]) (rcShmSize cfg)
  , maybe [] (\w -> ["-w", w]) (rcWorkDir cfg)
  , concatMap mountArg (rcBindMounts cfg)
  , concatMap portArg (rcPorts cfg)
  , concatMap envArg (rcEnv cfg)
  , map Text.unpack (rcExtraFlags cfg)
  , [Text.unpack (rcImage cfg)]
  , maybe [] (map Text.unpack) (rcCommand cfg)
  ]
  where
    suffix = rcSELinuxSuffix cfg
    mountArg (host, container) =
      ["-v", host <> ":" <> container <> suffix]
    portArg (hostPort, containerPort) =
      ["-p", show hostPort <> ":" <> show containerPort]
    envArg (key, val) =
      ["-e", Text.unpack key <> "=" <> Text.unpack val]

-- | Engine-specific flags added to every @run@ invocation (IO version).
--
-- Podman rootless: @--userns=keep-id@ maps the container's root user to
-- the invoking user, so files written inside the container are owned by
-- the host user (not root). Skipped when running as root (euid 0) because
-- rootful podman does not support @--userns=keep-id@.
--
-- Docker: @--user UID:GID@ achieves the same effect so files created
-- inside the container are owned by the invoking user. Skipped when
-- running as root (UID 0 mapping is the default).
engineSpecificRunFlagsIO :: ContainerEngine -> IO [String]
engineSpecificRunFlagsIO Podman = do
  uid <- getEffectiveUserID
  if uid == 0
    then pure []  -- rootful podman: --userns=keep-id is unsupported
    else pure ["--userns=keep-id"]
engineSpecificRunFlagsIO Docker = do
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  if uid == 0
    then pure []  -- running as root: no user mapping needed
    else pure ["--user", show uid <> ":" <> show gid]

-- | Pure version for testing. Uses static Podman flags; Docker returns empty
-- (since UID/GID require IO). Real code uses 'engineSpecificRunFlagsIO'.
engineSpecificRunFlags :: ContainerEngine -> [String]
engineSpecificRunFlags Podman = ["--userns=keep-id"]
engineSpecificRunFlags Docker = []

-- | Build the argument list for @docker\/podman build@.
buildBuildArgs :: BuildConfig -> [String]
buildBuildArgs cfg = concat
  [ ["build"]
  , ["-f", bcDockerfile cfg]
  , ["-t", Text.unpack (bcTag cfg)]
  , concatMap buildArgFlag (bcBuildArgs cfg)
  , [bcContext cfg]
  ]
  where
    buildArgFlag (key, val) =
      ["--build-arg", Text.unpack key <> "=" <> Text.unpack val]

-- ======================================================================
-- Process execution (internal)
-- ======================================================================

-- | Run an external process, capturing stdout and stderr as 'Text'.
-- Neither stream is shown to the user.
runProcess :: String -> [String] -> IO (ExitCode, Text, Text)
runProcess exe args = do
  (code, stdout', stderr') <- readProcessWithExitCode exe args ""
  pure (code, Text.pack stdout', Text.pack stderr')

-- | Run an external process, capturing stdout but letting stderr pass
-- through to the terminal. Use this for operations where the user needs
-- to see progress (image pulls, builds) but the caller still needs
-- stdout for programmatic use.
runProcessPassStderr :: String -> [String] -> IO (ExitCode, Text, Text)
runProcessPassStderr exe args = do
  let cp = (proc exe args)
        { std_in  = Inherit
        , std_out = CreatePipe
        , std_err = Inherit
        }
  (_, mStdout, _, ph) <- createProcess cp
  stdout' <- case mStdout of
    Just h  -> Text.pack <$> hGetContents h
    Nothing -> pure ""
  code <- waitForProcess ph
  pure (code, stdout', "")

whenIO' :: Bool -> IO () -> IO ()
whenIO' True  act = act
whenIO' False _   = pure ()
