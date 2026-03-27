{- |
Module      : Main
Description : CLI entry point for morloc-manager
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Parses command-line arguments via optparse-applicative and dispatches to
the appropriate subcommand handler.

@
  CLI structure:

  morloc-manager [--engine docker|podman] [-v|--verbose] \<subcommand\>

  Setup:
    install   [--no-init] [--force] [--system|--local] [VERSION]
    uninstall [--system|--local] [--all] VERSION...
    select    [--system|--local] \<VERSION\>
    info      [--system|--local]

  Development:
    run       [--system|--local] [--shell] [--] COMMAND...
    env       [--system|--local] [--init NAME|--list|--reset|NAME]

  Deployment:
    start     IMAGE [-p HOST:CONTAINER]
    stop      NAME
    freeze    [--system|--local] [-o PATH]
    unfreeze  --from TARBALL --tag TAG [--base IMAGE]
    status
    logs      NAME [-f|--follow]
@
-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Version as DV
import Options.Applicative
import qualified Paths_morloc_manager as PMM
import Control.Monad (when)
import Control.Exception (SomeException, IOException, try, displayException)
import Control.Concurrent (threadDelay)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable, getCurrentDirectory, getHomeDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (exitWith, ExitCode(..), exitFailure)
import System.FilePath (normalise, addTrailingPathSeparator, (</>))
import System.IO (Handle, hPutStr, hPutStrLn, hFlush, hIsTerminalDevice, stdin, stdout, stderr)

import MorlocManager.Types
import MorlocManager.Config
  ( readConfig
  , readActiveConfig
  , readVersionConfig
  , readEnvironmentConfig
  , readWorkspaceConfig
  , writeWorkspaceConfig
  , configDir
  , configPath
  , dataDir
  , versionDataDir
  , workspaceDataDir
  , listWorkspaces
  , readFlagsFile
  , globalFlagsPath
  , envFlagsPath
  , findInstalledScope
  , findWorkspaceScope
  , requireScopeConfig
  , writeConfig
  )
import MorlocManager.Container
  ( RunConfig(..)
  , defaultRunConfig
  , containerRunPassthrough
  )
import MorlocManager.Freeze (freeze, freezeFromDir)
import MorlocManager.Serve (buildServeImage, runServeContainer, stopServeContainer, listServeContainers, streamServeLogs)
import MorlocManager.SELinux (SELinuxMode(..), detectSELinux, volumeSuffix, validateMountPath)
import MorlocManager.Version
  ( installVersion'
  , installLatest
  , isVersionInstalled
  , selectVersion
  , uninstallVersion
  , listVersions
  , resolveActiveVersion
  , resolveActiveTarget
  )
import MorlocManager.Environment
  ( initEnvironment
  , buildEnvironment
  , activateEnvironment
  , listEnvironments
  )

-- ======================================================================
-- CLI types
-- ======================================================================

-- | Top-level CLI options parsed before the subcommand.
data GlobalOpts = GlobalOpts
  { globalVerbose :: Bool
    -- ^ Print container commands to stderr before executing
  } deriving (Show)

-- | Parsed subcommand with its arguments.
data Command
  = CmdSetup (Maybe Scope) (Maybe ContainerEngine)
    -- ^ Configure engine for a scope (interactive or via --engine flag)
  | CmdInstall (Maybe Scope) (Maybe String) Bool Bool
    -- ^ Install a version (scope, version, noInit, force)
  | CmdUninstall (Maybe Scope) Bool [String]
    -- ^ Uninstall versions (scope, removeAll, versions)
  | CmdSelect String
    -- ^ Select an installed version or workspace as active
  | CmdInfo
    -- ^ Show installed versions and configuration
  | CmdNew (Maybe Scope) String (Maybe String) Bool
    -- ^ Create a workspace (scope, name, fromVersion, copy)
  | CmdRun Bool [String]
    -- ^ Run a command (shell, args)
  | CmdEnv EnvAction
    -- ^ Environment management
  | CmdStart Text [(Int,Int)]
    -- ^ Start a serve container (image tag, port mappings)
  | CmdStop Text
    -- ^ Stop a running serve container
  | CmdFreeze (Maybe FilePath)
    -- ^ Export installed state as frozen artifact (output path)
  | CmdUnfreeze FilePath Text (Maybe Text)
    -- ^ Build a serve image from frozen state (tarball, tag, base image)
  | CmdStatus
    -- ^ List running serve containers
  | CmdLogs Text Bool
    -- ^ Show logs from a serve container (name, follow)
  deriving (Show)

-- | Sub-actions for the @env@ subcommand.
data EnvAction
  = EnvActivate String
    -- ^ Activate (and build if needed) an environment
  | EnvInit String
    -- ^ Create a new environment Dockerfile stub
  | EnvList
    -- ^ List available environments
  | EnvReset
    -- ^ Reset to the base environment
  deriving (Show)

-- ======================================================================
-- Parser definitions
-- ======================================================================

optsParser :: ParserInfo (GlobalOpts, Command)
optsParser = info (helper <*> versionFlag <*> ((,) <$> globalOptsParser <*> commandParser))
  ( fullDesc
  <> header "morloc-manager - container lifecycle manager for Morloc"
  <> progDesc "Manage containerized Morloc installations, dependency layers, and deployments"
  )

versionFlag :: Parser (a -> a)
versionFlag = infoOption ("morloc-manager v" <> DV.showVersion PMM.version)
  ( long "version" <> help "Print version and exit" )

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
        ( long "verbose" <> short 'v'
        <> help "Print container commands to stderr before executing" )

-- | Optional scope override for admin commands (install, uninstall, setup).
scopeOverride :: Parser (Maybe Scope)
scopeOverride = optional (option scopeReader
  ( long "scope" <> metavar "SCOPE"
  <> help "Target scope: local or system (default: local)" ))

scopeReader :: ReadM Scope
scopeReader = eitherReader $ \s -> case s of
  "local"  -> Right Local
  "system" -> Right System
  _        -> Left ("Unknown scope: " <> s <> ". Use 'local' or 'system'.")

commandParser :: Parser Command
commandParser = setupCmds <|> devCmds <|> deployCmds
  where
    setupCmds = hsubparser
      ( commandGroup "Setup"
      <> command "setup" (info setupParser
          (progDesc "Configure engine for a scope"))
      <> command "install" (info installParser
          (progDesc "Install a morloc version"))
      <> command "uninstall" (info uninstallParser
          (progDesc "Remove a version"))
      <> command "select" (info selectParser
          (progDesc "Set the active version or workspace"))
      <> command "info" (info (pure CmdInfo)
          (progDesc "Show configuration and installed versions"))
      )
    devCmds = hsubparser
      ( metavar ""
      <> commandGroup "Development"
      <> command "new" (info newParser
          (progDesc "Create a named workspace"))
      <> command "run" (info runParser
          (progDesc "Run a command in the active container"))
      <> command "env" (info envParser
          (progDesc "Manage dependency environments"))
      )
    deployCmds = hsubparser
      ( metavar ""
      <> commandGroup "Deployment"
      <> command "start" (info startParser
          (progDesc "Start a serve container"))
      <> command "stop" (info stopParser
          (progDesc "Stop a running serve container"))
      <> command "freeze" (info freezeParser
          (progDesc "Export installed state as a frozen artifact"))
      <> command "unfreeze" (info unfreezeParser
          (progDesc "Build a serve image from frozen state"))
      <> command "status" (info (pure CmdStatus)
          (progDesc "List running serve containers"))
      <> command "logs" (info logsParser
          (progDesc "Show logs from a serve container"))
      )

setupParser :: Parser Command
setupParser = CmdSetup <$> scopeOverride <*> optional (option engineReader
  ( long "engine" <> metavar "ENGINE"
  <> help "Container engine: podman or docker (skip interactive prompt)" ))

installParser :: Parser Command
installParser = CmdInstall
  <$> scopeOverride
  <*> optional (argument str (metavar "VERSION" <> help "Version to install (default: latest)"))
  <*> switch (long "no-init" <> help "Skip morloc init after install")
  <*> switch (long "force" <> short 'f' <> help "Force re-install even if version is already installed")

selectParser :: Parser Command
selectParser = CmdSelect
  <$> argument str (metavar "TARGET" <> help "Version or workspace name to select")

uninstallParser :: Parser Command
uninstallParser = CmdUninstall
  <$> scopeOverride
  <*> switch (long "all" <> short 'a' <> help "Remove all installed versions")
  <*> many (argument str (metavar "VERSION..." <> help "Version(s) to remove"))

runParser :: Parser Command
runParser = CmdRun
  <$> switch (long "shell" <> help "Start an interactive shell")
  <*> many (argument str (metavar "COMMAND..." <> help "Command to run inside the container"))

envParser :: Parser Command
envParser = CmdEnv
  <$> (envInitFlag <|> envListFlag <|> envResetFlag <|> envActivateArg)
  where
    envInitFlag = EnvInit <$> option str
      (long "init" <> metavar "NAME" <> help "Create a new environment Dockerfile")
    envListFlag = EnvList <$ switch (long "list" <> help "List available environments")
    envResetFlag = EnvReset <$ switch (long "reset" <> help "Reset to base environment")
    envActivateArg = EnvActivate <$> argument str
      (metavar "NAME" <> help "Activate (and build if needed) an environment")

newParser :: Parser Command
newParser = CmdNew
  <$> scopeOverride
  <*> argument str (metavar "NAME" <> help "Workspace name")
  <*> optional (option str
        ( long "from" <> metavar "VERSION"
        <> help "Base version (default: currently active version)" ))
  <*> switch (long "copy" <> help "Copy lib/fdb/bin from base version")

freezeParser :: Parser Command
freezeParser = CmdFreeze
  <$> optional (option str
        ( long "output" <> short 'o' <> metavar "PATH"
        <> help "Output directory for frozen state (default: ./morloc-freeze/)" ))

unfreezeParser :: Parser Command
unfreezeParser = CmdUnfreeze
  <$> option str (long "from" <> metavar "TARBALL" <> help "Path to state.tar.gz from freeze")
  <*> option (Text.pack <$> str) (long "tag" <> short 't' <> metavar "TAG" <> help "Image tag")
  <*> optional (option (Text.pack <$> str)
        (long "base" <> metavar "IMAGE" <> help "Base image override (default: morloc-serve:<ver>)"))

startParser :: Parser Command
startParser = CmdStart
  <$> argument (Text.pack <$> str) (metavar "IMAGE" <> help "Serve image tag")
  <*> many (option portReader
        ( long "port" <> short 'p' <> metavar "HOST:CONTAINER"
        <> help "Port mapping (e.g., 8080:8080)" ))

stopParser :: Parser Command
stopParser = CmdStop
  <$> argument (Text.pack <$> str) (metavar "NAME" <> help "Container name to stop")

logsParser :: Parser Command
logsParser = CmdLogs
  <$> argument (Text.pack <$> str) (metavar "NAME" <> help "Container name")
  <*> switch (long "follow" <> short 'f' <> help "Follow log output")

portReader :: ReadM (Int, Int)
portReader = eitherReader $ \s -> case break (== ':') s of
  (h, ':':c) -> case (,) <$> readMaybeInt h <*> readMaybeInt c of
    Just pair -> Right pair
    Nothing   -> Left ("Invalid port mapping: " <> s)
  _ -> Left ("Expected HOST:CONTAINER format, got: " <> s)
  where
    readMaybeInt :: String -> Maybe Int
    readMaybeInt s' = case reads s' of { [(n, "")] -> Just n; _ -> Nothing }

engineReader :: ReadM ContainerEngine
engineReader = eitherReader $ \s -> case s of
  "docker" -> Right Docker
  "podman" -> Right Podman
  _        -> Left ("Unknown engine: " <> s <> ". Use 'docker' or 'podman'.")

-- ======================================================================
-- Main dispatch
-- ======================================================================

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser optsParser
  result <- try (dispatch (globalVerbose globalOpts) cmd)
    :: IO (Either SomeException (Either ManagerError ()))
  case result of
    Left exc -> do
      TextIO.hPutStrLn stderr $ "Error: " <> Text.pack (displayException exc)
      exitWith (ExitFailure 1)
    Right (Right ()) -> pure ()
    Right (Left err) -> do
      TextIO.hPutStrLn stderr (renderError err)
      case err of
        EngineError _ n _ -> exitWith (ExitFailure n)
        _                 -> exitWith (ExitFailure 1)

-- | Resolve scope with default Local for admin commands.
resolveScope :: Maybe Scope -> Scope
resolveScope (Just s) = s
resolveScope Nothing  = Local

-- | Resolve the active version, falling back to a workspace's base version.
resolveActiveVersionOrWorkspace :: IO (Either ManagerError (Version, Scope))
resolveActiveVersionOrWorkspace = do
  result <- resolveActiveVersion
  case result of
    Right v -> pure (Right v)
    Left NoActiveVersion -> do
      targetResult <- resolveActiveTarget
      case targetResult of
        Right (TargetWorkspace name, scope) -> do
          wcResult <- readWorkspaceConfig scope name
          case wcResult of
            Right wc -> pure (Right (wsBaseVersion wc, wsBaseScope wc))
            Left err -> pure (Left err)
        _ -> pure (Left NoActiveVersion)
    Left err -> pure (Left err)

-- | Get engine from active config (local-first, system fallback).
-- Auto-triggers interactive setup if no config exists and stdin is a TTY.
ensureEngine :: IO (Either ManagerError ContainerEngine)
ensureEngine = do
  mCfg <- readActiveConfig
  case mCfg of
    Just cfg -> pure (Right (configEngine cfg))
    Nothing -> do
      isTTY <- hIsTerminalDevice stdin
      if isTTY
        then do
          hPutStrLn stderr "No configuration found. Let's set up.\n"
          result <- runInteractiveSetup Local
          case result of
            Left err -> pure (Left err)
            Right () -> do
              mCfg' <- readActiveConfig
              case mCfg' of
                Just cfg -> pure (Right (configEngine cfg))
                Nothing  -> pure (Left (SetupNotComplete Local))
        else pure (Left (SetupNotComplete Local))

-- | Get engine for a specific scope. Auto-triggers setup for that scope.
ensureEngineForScope :: Scope -> IO (Either ManagerError ContainerEngine)
ensureEngineForScope scope = do
  result <- requireScopeConfig scope
  case result of
    Right cfg -> pure (Right (configEngine cfg))
    Left (SetupNotComplete s) -> do
      isTTY <- hIsTerminalDevice stdin
      if isTTY
        then do
          let scopeStr = case s of { Local -> "local"; System -> "system" }
          hPutStrLn stderr $ "No " <> scopeStr <> " configuration found. Let's set up.\n"
          setupResult <- runInteractiveSetup s
          case setupResult of
            Left err -> pure (Left err)
            Right () -> do
              result' <- requireScopeConfig s
              case result' of
                Right cfg -> pure (Right (configEngine cfg))
                Left err  -> pure (Left err)
        else pure (Left (SetupNotComplete s))
    Left err -> pure (Left err)

-- | Interactive setup: detect engines, prompt user, write config.
runInteractiveSetup :: Scope -> IO (Either ManagerError ())
runInteractiveSetup scope = do
  engineResult <- interactiveEngineChoice
  case engineResult of
    Left err -> pure (Left err)
    Right eng -> do
      cfgP <- configPath scope
      mExisting <- readConfig cfgP
      let baseCfg = case (mExisting :: Either ManagerError Config) of
            Right c -> c
            Left _  -> defaultConfig
      result <- writeConfig cfgP (baseCfg { configEngine = eng, configActiveScope = scope })
      case result of
        Left err -> pure (Left err)
        Right () -> do
          info' stderr $ "  Engine: " <> Text.unpack (displayEngine eng)
          info' stderr $ "  Config: " <> cfgP
          info' stderr ""
          pure (Right ())

-- | Non-interactive setup: write config with the given engine directly.
runNonInteractiveSetup :: Scope -> ContainerEngine -> IO (Either ManagerError ())
runNonInteractiveSetup scope eng = do
  cfgP <- configPath scope
  mExisting <- readConfig cfgP
  let baseCfg = case (mExisting :: Either ManagerError Config) of
        Right c -> c
        Left _  -> defaultConfig
  result <- writeConfig cfgP (baseCfg { configEngine = eng, configActiveScope = scope })
  case result of
    Left err -> pure (Left err)
    Right () -> do
      info' stderr $ "  Engine: " <> Text.unpack (displayEngine eng)
      info' stderr $ "  Config: " <> cfgP
      info' stderr ""
      pure (Right ())

-- | Detect available engines and prompt user to choose if both found.
interactiveEngineChoice :: IO (Either ManagerError ContainerEngine)
interactiveEngineChoice = do
  podman <- findExecutable "podman"
  docker <- findExecutable "docker"
  case (podman, docker) of
    (Nothing, Nothing) -> pure (Left EngineNotFound)
    (Just _, Nothing)  -> do
      info' stderr "Detected: podman"
      pure (Right Podman)
    (Nothing, Just _)  -> do
      info' stderr "Detected: docker"
      pure (Right Docker)
    (Just _, Just _)   -> do
      hPutStrLn stderr "Both podman and docker detected."
      hPutStrLn stderr "  [1] podman (recommended)"
      hPutStrLn stderr "  [2] docker"
      hPutStr stderr "Choose [1]: "
      hFlush stderr
      lineResult <- try getLine :: IO (Either IOException String)
      case lineResult of
        Left _ -> do
          hPutStrLn stderr ""
          hPutStrLn stderr "Non-interactive input detected, defaulting to podman."
          pure (Right Podman)
        Right "2" -> pure (Right Docker)
        Right _   -> pure (Right Podman)

-- | Dispatch a parsed command to its handler.
dispatch :: Bool -> Command -> IO (Either ManagerError ())
dispatch verbose cmd = case cmd of

  -- ---- setup ----
  CmdSetup mScope mEngine -> do
    let scope = resolveScope mScope
    case mEngine of
      Just eng -> runNonInteractiveSetup scope eng
      Nothing  -> runInteractiveSetup scope

  -- ---- install ----
  CmdInstall mScope mVerStr noInit force -> do
    let scope = resolveScope mScope
    engineResult <- ensureEngineForScope scope
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> do
        -- Install the version
        installResult <- case mVerStr of
          Nothing       -> installLatest engine scope
          Just "latest" -> installLatest engine scope
          Just verStr -> case parseVersion verStr of
            Nothing  -> pure (Left (InvalidVersion verStr))
            Just ver -> do
              r <- installVersion' force engine scope ver
              case r of
                Left err -> pure (Left err)
                Right () -> pure (Right ver)
        case installResult of
          Left err -> pure (Left err)
          Right ver -> do
            -- Check if this was a fresh install or a no-op (already installed)
            -- installVersion' prints its own message for the no-op case
            -- Auto-select if no version is currently active
            activeResult <- resolveActiveVersion
            case activeResult of
              Left NoActiveVersion -> do
                _ <- selectVersion scope ver
                info' stderr $ "Auto-selected version " <> Text.unpack (showVersion ver)
              _ -> pure ()
            -- Run morloc init unless --no-init or version was already installed
            if noInit
              then pure (Right ())
              else if not force
                then do
                  -- Check if already installed (same check installVersion' does)
                  let imageRef = "ghcr.io/morloc-project/morloc/morloc-full:" <> showVersion ver
                  wasAlready <- isVersionInstalled engine scope ver imageRef
                  if wasAlready
                    then pure (Right ())
                    else do
                      runMorlocInit engine verbose
                else do
                  info' stderr "Running morloc init -f..."
                  runInContainer engine verbose False ["morloc", "init", "-f"]

  -- ---- select ----
  CmdSelect verStr -> do
    case parseVersion verStr of
      Just ver -> do
        -- Treat as version — auto-find which scope has it
        found <- findInstalledScope ver
        let scope = maybe Local id found
        result <- selectVersion scope ver
        case result of
          Left err -> pure (Left err)
          Right () -> do
            info' stderr $ "Selected version " <> Text.unpack (showVersion ver)
            pure (Right ())
      Nothing -> do
        -- Treat as workspace name — auto-find which scope has it
        let wsName = Text.pack verStr
        found <- findWorkspaceScope wsName
        let scope = maybe Local id found
        wcResult <- readWorkspaceConfig scope wsName
        case wcResult of
          Left _ -> pure (Left (InvalidVersion ("Not found as version or workspace: " <> verStr)))
          Right _ -> do
            cfgP <- configPath scope
            mCfg <- readActiveConfig
            let cfg = maybe defaultConfig id mCfg
            _ <- writeConfig cfgP (cfg
              { configActiveTarget = Just (TargetWorkspace wsName)
              , configActiveScope = scope
              })
            info' stderr $ "Selected workspace " <> verStr
            pure (Right ())

  -- ---- uninstall ----
  CmdUninstall mScope removeAll verStrs -> do
    if not removeAll && null verStrs
      then pure (Left (UninstallError "No versions specified. Use VERSION... or --all."))
      else do
        let scope = resolveScope mScope
        versions <- if removeAll
          then listVersions scope
          else case mapM parseVersion verStrs of
            Nothing -> pure []
            Just vs -> pure vs
        if null versions && not removeAll
          then pure (Left (InvalidVersion (unwords verStrs)))
          else do
            results <- mapM (\ver -> do
              info' stderr $ "Uninstalling " <> Text.unpack (showVersion ver) <> "..."
              uninstallVersion scope ver
              ) versions
            case [e | Left e <- results] of
              (err:_) -> pure (Left err)
              []      -> do
                info' stderr $ "Uninstalled " <> show (length versions) <> " version(s)"
                pure (Right ())

  -- ---- run ----
  CmdRun shell args -> do
    engineResult <- ensureEngine
    case engineResult of
      Left err -> pure (Left err)
      Right engine ->
        if not shell && null args
          then pure (Left NoCommand)
          else runInContainer engine verbose shell args

  -- ---- env ----
  CmdEnv envAction -> do
    -- Resolve scope from active config
    mCfgForScope <- readActiveConfig
    let scope = maybe Local configActiveScope mCfgForScope
    case envAction of
      EnvInit name -> do
        result <- initEnvironment scope name
        case result of
          Left err -> pure (Left err)
          Right path -> do
            info' stderr $ "Created environment Dockerfile: " <> path
            info' stderr "Edit the Dockerfile, then run: morloc-manager env <name>"
            pure (Right ())

      EnvActivate name
        -- Detect common positional-arg mistakes and give correction hints
        | name `elem` ["list", "ls"] ->
            pure (Left (EnvError "Unknown argument. Did you mean: morloc-manager env --list"))
        | name `elem` ["reset"] ->
            pure (Left (EnvError "Unknown argument. Did you mean: morloc-manager env --reset"))
        | name `elem` ["init"] ->
            pure (Left (EnvError "Unknown argument. Did you mean: morloc-manager env --init <NAME>"))
        | otherwise -> do
        engineResult <- ensureEngine
        case engineResult of
          Left err -> pure (Left err)
          Right engine -> do
            verResult <- resolveActiveVersionOrWorkspace
            case verResult of
              Left err -> pure (Left err)
              Right (ver, _) -> do
                buildResult <- buildEnvironment engine scope ver name
                case buildResult of
                  Left err -> pure (Left err)
                  Right () -> do
                    actResult <- activateEnvironment scope ver name
                    case actResult of
                      Left err -> pure (Left err)
                      Right () -> do
                        info' stderr $ "Activated environment: " <> name
                        pure (Right ())

      EnvList -> do
        verResult <- resolveActiveVersionOrWorkspace
        case verResult of
          Left err -> pure (Left err)
          Right (ver, _) -> do
            envs <- listEnvironments scope ver
            mapM_ putStrLn envs
            pure (Right ())

      EnvReset -> do
        mCfg <- readActiveConfig
        case mCfg of
          Nothing -> pure (Right ())
          Just cfg -> do
            cfgP <- configPath scope
            let newCfg = cfg { configActiveEnv = "base" }
            _ <- writeConfig cfgP newCfg
            info' stderr "Reset to base environment"
            pure (Right ())

  -- ---- new ----
  CmdNew mScope name mFromVer doCopy -> do
    let scope = resolveScope mScope
    engineResult <- ensureEngineForScope scope
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> do
        -- Resolve base version
        baseResult <- case mFromVer of
          Just verStr -> case parseVersion verStr of
            Nothing  -> pure (Left (InvalidVersion verStr))
            Just ver -> do
              mFoundScope <- findInstalledScope ver
              case mFoundScope of
                Nothing -> pure (Left (VersionNotInstalled ver))
                Just foundScope -> pure (Right (ver, foundScope))
          Nothing -> resolveActiveVersion
        case baseResult of
          Left err -> pure (Left err)
          Right (baseVer, baseScope) -> do
            let wsName = Text.pack name
            wDataDir <- workspaceDataDir scope wsName
            exists <- doesDirectoryExist wDataDir
            if exists
              then pure (Left (InstallError ("Workspace already exists: " <> name)))
              else do
                mapM_ (\d -> createDirectoryIfMissing True (wDataDir </> d))
                  ["bin", "lib", "fdb", "include", "opt", "tmp"]
                when doCopy $ do
                  srcDir <- versionDataDir baseScope baseVer
                  mapM_ (\d -> do
                    let src = srcDir </> d
                    let dst = wDataDir </> d
                    srcExists <- doesDirectoryExist src
                    when srcExists $ do
                      _ <- readProcessWithExitCode "cp" ["-a", src <> "/.", dst] ""
                      pure ()
                    ) ["lib", "fdb", "bin"]
                  info' stderr $ "Copied state from " <> Text.unpack (showVersion baseVer)
                let wc = WorkspaceConfig
                      { wsBaseVersion = baseVer
                      , wsBaseScope = baseScope
                      , wsEngine = engine
                      }
                _ <- writeWorkspaceConfig scope wsName wc
                cfgP <- configPath scope
                mCfg <- readActiveConfig
                let cfg = maybe defaultConfig id mCfg
                _ <- writeConfig cfgP (cfg
                  { configActiveTarget = Just (TargetWorkspace wsName)
                  , configActiveScope = scope
                  })
                info' stderr $ "Created workspace: " <> name
                info' stderr $ "Based on version " <> Text.unpack (showVersion baseVer)
                initResult <- runMorlocInit engine verbose
                case initResult of
                  Left err -> pure (Left err)
                  Right () -> do
                    info' stderr $ "Activated workspace: " <> name
                    info' stderr "Note: 'freeze' is not yet supported from workspaces."
                    pure (Right ())

  -- ---- info ----
  CmdInfo -> do
    mCfg <- readActiveConfig
    seMode <- detectSELinux
    let activeScope = maybe Local configActiveScope mCfg
        seStr = case seMode of
          SELinuxEnforcing  -> "enforcing"
          SELinuxPermissive -> "permissive"
          SELinuxDisabled   -> "not detected"
        scopeStr = case activeScope of { Local -> "local"; System -> "system" }
    let engineStr = case mCfg of
          Just cfg -> displayEngine (configEngine cfg)
          Nothing  -> "not configured"
    cfgRoot <- configDir activeScope
    datRoot <- dataDir activeScope
    let activeTarget = mCfg >>= configActiveTarget
    case mCfg of
      Nothing -> do
        putStrLn   "Active:         none"
        putStrLn   "Active env:     base"
        putStrLn $ "Engine:         " <> Text.unpack engineStr
      Just cfg -> do
        let activeStr = case configActiveTarget cfg of
              Nothing                  -> "none"
              Just (TargetVersion v)   -> Text.unpack (showVersion v)
              Just (TargetWorkspace w) -> Text.unpack w <> " (workspace)"
        putStrLn $ "Active:         " <> activeStr
        putStrLn $ "Active env:     " <> Text.unpack (configActiveEnv cfg)
        putStrLn $ "Engine:         " <> Text.unpack (displayEngine (configEngine cfg))
    putStrLn $ "Scope:          " <> scopeStr
    putStrLn $ "SELinux:        " <> seStr
    putStrLn $ "Config root:    " <> cfgRoot
    putStrLn $ "Data root:      " <> datRoot
    -- List versions
    localVersions <- listVersions Local
    putStrLn ""
    putStrLn "Local versions:"
    if null localVersions
      then putStrLn "  (none)"
      else mapM_ (\v -> putStrLn $ "  " <> Text.unpack (showVersion v)
            <> if activeTarget == Just (TargetVersion v) && activeScope == Local then " (active)" else ""
          ) localVersions
    systemVersions <- listVersions System
    putStrLn ""
    putStrLn "System versions:"
    if null systemVersions
      then putStrLn "  (none)"
      else mapM_ (\v -> putStrLn $ "  " <> Text.unpack (showVersion v)
            <> if activeTarget == Just (TargetVersion v) && activeScope == System then " (active)" else ""
          ) systemVersions
    -- List workspaces
    localWorkspaces <- listWorkspaces Local
    putStrLn ""
    putStrLn "Local workspaces:"
    if null localWorkspaces
      then putStrLn "  (none)"
      else mapM_ (\w -> do
            wcResult <- readWorkspaceConfig Local w
            let baseStr = case wcResult of
                  Right wc -> " (based on " <> Text.unpack (showVersion (wsBaseVersion wc)) <> ")"
                  Left _   -> ""
            putStrLn $ "  " <> Text.unpack w <> baseStr
              <> if activeTarget == Just (TargetWorkspace w) && activeScope == Local then " (active)" else ""
          ) localWorkspaces
    systemWorkspaces <- listWorkspaces System
    when (not (null systemWorkspaces)) $ do
      putStrLn ""
      putStrLn "System workspaces:"
      mapM_ (\w -> do
            wcResult <- readWorkspaceConfig System w
            let baseStr = case wcResult of
                  Right wc -> " (based on " <> Text.unpack (showVersion (wsBaseVersion wc)) <> ")"
                  Left _   -> ""
            putStrLn $ "  " <> Text.unpack w <> baseStr
              <> if activeTarget == Just (TargetWorkspace w) && activeScope == System then " (active)" else ""
          ) systemWorkspaces
    pure (Right ())

  -- ---- freeze ----
  CmdFreeze mOutput -> do
    let outputDir = maybe "./morloc-freeze" id mOutput
    targetResult <- resolveActiveTarget
    case targetResult of
      Left err -> pure (Left err)
      Right (target, activeScope) -> case target of
        TargetVersion ver -> freeze activeScope ver outputDir
        TargetWorkspace name -> do
          wcResult <- readWorkspaceConfig activeScope name
          case wcResult of
            Left err -> pure (Left err)
            Right wc -> do
              wsDir <- workspaceDataDir activeScope name
              freezeFromDir activeScope (wsBaseVersion wc) wsDir outputDir

  -- ---- unfreeze ----
  CmdUnfreeze tarball tag mBase -> do
    engineResult <- ensureEngine
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> do
        targetResult <- resolveActiveTarget
        case targetResult of
          Left err -> pure (Left err)
          Right (target, _) -> do
            let ver = case target of
                  TargetVersion v -> v
                  TargetWorkspace _ -> Version 0 0 0
            buildServeImage engine tarball tag ver mBase

  -- ---- start ----
  CmdStart image ports -> do
    engineResult <- ensureEngine
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> do
        let containerName = "morloc-serve-" <> Text.replace ":" "-" image
            portMappings = if null ports then [(8080, 8080)] else ports
        runServeContainer engine image containerName portMappings

  -- ---- stop ----
  CmdStop name -> do
    engineResult <- ensureEngine
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> do
        result <- stopServeContainer engine name
        case result of
          Left err -> pure (Left err)
          Right () -> do
            info' stderr $ "Stopped container " <> Text.unpack name
            pure (Right ())

  -- ---- status ----
  CmdStatus -> do
    engineResult <- ensureEngine
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> listServeContainers engine

  -- ---- logs ----
  CmdLogs name follow -> do
    engineResult <- ensureEngine
    case engineResult of
      Left err -> pure (Left err)
      Right engine -> streamServeLogs engine name follow

-- ======================================================================
-- Container run
-- ======================================================================

-- | Run a command inside the active morloc container.
runInContainer
  :: ContainerEngine -> Bool -> Bool -> [String]
  -> IO (Either ManagerError ())
runInContainer engine verbose shell args = do
  targetResult <- resolveActiveTarget
  case targetResult of
    Left err -> pure (Left err)
    Right (target, activeScope) -> do
      -- Resolve version config (for image) and data dir based on target type
      resolution <- case target of
        TargetVersion ver -> do
          vcResult <- readVersionConfig activeScope ver
          case vcResult of
            Left err -> pure (Left err)
            Right vc -> do
              dDir <- versionDataDir activeScope ver
              pure (Right (vc, dDir, activeScope, ver))
        TargetWorkspace name -> do
          wcResult <- readWorkspaceConfig activeScope name
          case wcResult of
            Left err -> pure (Left err)
            Right wc -> do
              vcResult <- readVersionConfig (wsBaseScope wc) (wsBaseVersion wc)
              case vcResult of
                Left err -> pure (Left err)
                Right vc -> do
                  dDir <- workspaceDataDir activeScope name
                  pure (Right (vc, dDir, wsBaseScope wc, wsBaseVersion wc))
      case resolution of
        Left err -> pure (Left err)
        Right (vc, vDataDir, verScope, ver) -> do
          mCfg <- readActiveConfig
          image <- resolveImage verScope ver vc mCfg
          seMode <- detectSELinux
          let suffix = volumeSuffix seMode
          home <- getHomeDirectory
          cwd <- getCurrentDirectory
          let envName = maybe "base" (Text.unpack . configActiveEnv) mCfg
          globalFp <- globalFlagsPath verScope
          globalFlags <- readFlagsFile globalFp
          envFlags <- if envName == "base"
            then pure []
            else do
              efp <- envFlagsPath verScope ver envName
              readFlagsFile efp
          let extraFlags = map Text.pack (globalFlags <> envFlags)
              isInit = case args of
                ("morloc":"init":_) -> True
                _                   -> False
          let isHomeDir = addTrailingPathSeparator (normalise cwd)
                       == addTrailingPathSeparator (normalise home)
          if not isInit && not (null suffix) && not isHomeDir
            then do
              validation <- validateMountPath cwd
              case validation of
                Left err -> pure (Left err)
                Right () -> runWithConfig engine verbose image vDataDir home cwd
                              suffix shell args isInit (vcShmSize vc) extraFlags
            else do
              -- When running from home with SELinux, skip the work mount
              -- (home is unsafe to relabel with :z) and use home as workDir
              let cwd' = if isHomeDir && not (null suffix) then home else cwd
                  skipWorkMount = isHomeDir && not (null suffix) && not isInit
              when skipWorkMount $ do
                hPutStrLn stderr "Warning: running from home directory with SELinux; working directory mount skipped."
                hPutStrLn stderr "Workaround: create a project subdirectory and work from there:"
                hPutStrLn stderr "  mkdir ~/myproject && cd ~/myproject"
              runWithConfig engine verbose image vDataDir home cwd'
                   suffix shell args (isInit || skipWorkMount) (vcShmSize vc) extraFlags

-- | Resolve which container image to use, considering custom environments.
resolveImage :: Scope -> Version -> VersionConfig -> Maybe Config -> IO Text
resolveImage scope ver vc mCfg = do
  let baseImage = vcImage vc
  case mCfg of
    Nothing -> pure baseImage
    Just cfg -> do
      let envName = configActiveEnv cfg
      if envName == "base"
        then pure baseImage
        else do
          ecResult <- readEnvironmentConfig scope ver (Text.unpack envName)
          case ecResult of
            Right ec -> pure (ecImage ec)
            Left _   -> pure baseImage

-- | Construct the RunConfig and execute the container.
runWithConfig
  :: ContainerEngine -> Bool -> Text -> FilePath -> FilePath -> FilePath
  -> String -> Bool -> [String] -> Bool -> Text -> [Text]
  -> IO (Either ManagerError ())
runWithConfig engine verbose image vDataDir home cwd suffix shell args isInit shmSize extraFlags = do
  whenIO shell $ do
    isTTYin  <- hIsTerminalDevice stdin
    isTTYout <- hIsTerminalDevice stdout
    if not isTTYin || not isTTYout
      then do
        hPutStrLn stderr "Error: --shell requires an interactive terminal (TTY)."
        hPutStrLn stderr "If connecting over SSH, use: ssh -t <host> morloc-manager run --shell"
        exitWith (ExitFailure 1)
      else pure ()
  let containerHome = home
      baseMounts =
        [ (vDataDir, containerHome <> "/.local/share/morloc")
        , (vDataDir <> "/bin", containerHome <> "/.local/bin")
        ]
      workMount = if isInit then [] else [(cwd, cwd)]
      allMounts = baseMounts <> workMount
      workDir = if isInit then containerHome else cwd
      envVars =
        [ ("HOME", Text.pack containerHome)
        , ("PATH", Text.pack containerHome <> "/.local/bin:" <> Text.pack containerHome <> "/.local/share/morloc/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
        ]
      cmd' = if shell then Just ["/bin/bash"]
            else if null args then Nothing
            else Just (map Text.pack args)
      cfg = (defaultRunConfig image)
        { rcBindMounts    = allMounts
        , rcEnv           = envVars
        , rcInteractive   = shell
        , rcShmSize       = Just shmSize
        , rcWorkDir       = Just workDir
        , rcSELinuxSuffix = suffix
        , rcCommand       = cmd'
        , rcExtraFlags    = extraFlags
        }
  code <- containerRunPassthrough engine verbose cfg
  case code of
    ExitSuccess   -> pure (Right ())
    ExitFailure n -> pure (Left (EngineError engine n "Container exited with error"))

-- ======================================================================
-- Helpers
-- ======================================================================

-- | Run morloc init inside the container. Uses -q (quiet) unless verbose.
runMorlocInit :: ContainerEngine -> Bool -> IO (Either ManagerError ())
runMorlocInit engine verbose = do
  let initArgs = if verbose
        then ["morloc", "init", "-f"]
        else ["morloc", "init", "-f", "-q"]
  info' stderr "Initializing morloc..."
  runInContainer engine verbose False initArgs

-- | User-facing display for ContainerEngine.
displayEngine :: ContainerEngine -> Text
displayEngine Docker = "docker"
displayEngine Podman = "podman"

-- | Print an info message to stderr.
info' :: Handle -> String -> IO ()
info' h msg = hPutStrLn h msg >> hFlush h

-- | Execute an IO action only when the condition is true.
whenIO :: Bool -> IO () -> IO ()
whenIO True  act = act
whenIO False _   = pure ()
