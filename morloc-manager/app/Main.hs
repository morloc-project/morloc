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

  Subcommands:
    install [--no-init] [--system|--local] [VERSION]
    select  [--system|--local] \<VERSION\>
    uninstall [--system|--local] [--all] VERSION...
    run     [--system|--local] [--shell] [--] COMMAND...
    env     [--system|--local] [--init NAME|--list|--reset|NAME]
    info    [--system|--local]
    build   [--system|--local] [--shell] [--script FILE]
    freeze  [--system|--local] [-o PATH]
    serve-image --from TARBALL --tag TAG [--base IMAGE]
    serve   IMAGE [-p HOST:CONTAINER]
@
-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Options.Applicative
import Control.Monad (when)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Exit (exitWith, ExitCode(..), exitFailure)
import System.FilePath (normalise, addTrailingPathSeparator)
import System.IO (Handle, hPutStrLn, hFlush, hIsTerminalDevice, stdin, stdout, stderr)

import MorlocManager.Types
import MorlocManager.Config
  ( readActiveConfig
  , readVersionConfig
  , readEnvironmentConfig
  , configDir
  , configPath
  , versionConfigPath
  , dataDir
  , versionDataDir
  , readFlagsFile
  , globalFlagsPath
  , envFlagsPath
  , findInstalledScope
  , fixSystemPerms
  , writeConfig
  )
import MorlocManager.Container
  ( RunConfig(..)
  , defaultRunConfig
  , detectEngine
  , containerRunPassthrough
  )
import MorlocManager.Freeze (freeze)
import MorlocManager.Serve (buildServeImage, runServeContainer)
import MorlocManager.SELinux (SELinuxMode(..), detectSELinux, volumeSuffix, validateMountPath)
import MorlocManager.Version
  ( installVersion
  , installLatest
  , selectVersion
  , uninstallVersion
  , listVersions
  , resolveActiveVersion
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
  { globalEngine :: Maybe ContainerEngine
    -- ^ Override container engine (default: auto-detect)
  , globalVerbose :: Bool
    -- ^ Print container commands to stderr before executing
  } deriving (Show)

-- | Parsed subcommand with its arguments.
data Command
  = CmdInstall (Maybe Scope) (Maybe String) Bool
    -- ^ Install a version (scope, version, noInit)
  | CmdSelect (Maybe Scope) String
    -- ^ Select an installed version as active
  | CmdUninstall (Maybe Scope) Bool [String]
    -- ^ Uninstall versions (scope, removeAll, versions)
  | CmdRun (Maybe Scope) Bool [String]
    -- ^ Run a command (scope, shell, args)
  | CmdEnv (Maybe Scope) EnvAction
    -- ^ Environment management
  | CmdInfo (Maybe Scope)
    -- ^ Show installed versions and configuration
  | CmdBuild (Maybe Scope) Bool (Maybe FilePath)
    -- ^ Start a mutable builder container (scope, shell, script)
  | CmdFreeze (Maybe Scope) (Maybe FilePath)
    -- ^ Export installed state as frozen artifact (scope, output path)
  | CmdServeImage FilePath Text (Maybe Text)
    -- ^ Build a serve image (tarball, tag, base image override)
  | CmdServe Text [(Int,Int)]
    -- ^ Run a serve container (image tag, port mappings)
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
versionFlag = infoOption "morloc-manager v0.11.0"
  ( long "version" <> help "Print version and exit" )

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> optional (option engineReader
        ( long "engine" <> metavar "ENGINE"
        <> help "Container engine: docker or podman (default: auto-detect)" ))
  <*> switch
        ( long "verbose" <> short 'v'
        <> help "Print container commands to stderr before executing" )

-- | Parse --system / --local as mutually exclusive scope flags.
-- Defaults to Local when neither is specified.
scopeFlags :: Parser (Maybe Scope)
scopeFlags =
      (flag' (Just System) (long "system" <> help "Use system-wide installation"))
  <|> (flag' (Just Local)  (long "local"  <> help "Use per-user installation (default)"))
  <|> pure Nothing

commandParser :: Parser Command
commandParser = hsubparser
  ( command "install" (info installParser
      (progDesc "Install a morloc version"))
  <> command "select" (info selectParser
      (progDesc "Set an installed version as the active version"))
  <> command "uninstall" (info uninstallParser
      (progDesc "Remove version config, data, and container image"))
  <> command "run" (info runParser
      (progDesc "Run a command inside the active morloc container"))
  <> command "env" (info envParser
      (progDesc "Manage custom dependency environments"))
  <> command "info" (info (CmdInfo <$> scopeFlags)
      (progDesc "Show installed versions and current configuration"))
  <> command "build" (info buildParser
      (progDesc "Start a mutable builder container"))
  <> command "freeze" (info freezeParser
      (progDesc "Export installed state as a frozen artifact"))
  <> command "serve-image" (info serveImageParser
      (progDesc "Build an immutable serve image from frozen state"))
  <> command "serve" (info serveParser
      (progDesc "Run a serve container with the nexus router"))
  )

installParser :: Parser Command
installParser = CmdInstall
  <$> scopeFlags
  <*> optional (argument str (metavar "VERSION" <> help "Version to install (default: latest)"))
  <*> switch (long "no-init" <> help "Skip morloc init after install")

selectParser :: Parser Command
selectParser = CmdSelect
  <$> scopeFlags
  <*> argument str (metavar "VERSION" <> help "Version to select")

uninstallParser :: Parser Command
uninstallParser = CmdUninstall
  <$> scopeFlags
  <*> switch (long "all" <> short 'a' <> help "Remove all installed versions")
  <*> many (argument str (metavar "VERSION..." <> help "Version(s) to remove"))

runParser :: Parser Command
runParser = CmdRun
  <$> scopeFlags
  <*> switch (long "shell" <> help "Start an interactive shell")
  <*> many (argument str (metavar "COMMAND..." <> help "Command to run inside the container"))

envParser :: Parser Command
envParser = CmdEnv
  <$> scopeFlags
  <*> (envInitFlag <|> envListFlag <|> envResetFlag <|> envActivateArg)
  where
    envInitFlag = EnvInit <$> option str
      (long "init" <> metavar "NAME" <> help "Create a new environment Dockerfile")
    envListFlag = EnvList <$ switch (long "list" <> help "List available environments")
    envResetFlag = EnvReset <$ switch (long "reset" <> help "Reset to base environment")
    envActivateArg = EnvActivate <$> argument str
      (metavar "NAME" <> help "Activate (and build if needed) an environment")

buildParser :: Parser Command
buildParser = CmdBuild
  <$> scopeFlags
  <*> switch (long "shell" <> help "Start an interactive shell (default: run --script)")
  <*> optional (option str
        ( long "script" <> metavar "FILE"
        <> help "Build script to run inside the container" ))

freezeParser :: Parser Command
freezeParser = CmdFreeze
  <$> scopeFlags
  <*> optional (option str
        ( long "output" <> short 'o' <> metavar "PATH"
        <> help "Output directory for frozen state (default: ./morloc-freeze/)" ))

serveImageParser :: Parser Command
serveImageParser = CmdServeImage
  <$> option str (long "from" <> metavar "TARBALL" <> help "Path to state.tar.gz from freeze")
  <*> option (Text.pack <$> str) (long "tag" <> short 't' <> metavar "TAG" <> help "Image tag")
  <*> optional (option (Text.pack <$> str)
        (long "base" <> metavar "IMAGE" <> help "Base image override (default: morloc-serve:<ver>)"))

serveParser :: Parser Command
serveParser = CmdServe
  <$> argument (Text.pack <$> str) (metavar "IMAGE" <> help "Serve image tag")
  <*> many (option portReader
        ( long "port" <> short 'p' <> metavar "HOST:CONTAINER"
        <> help "Port mapping (e.g., 8080:8080)" ))

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
  result <- dispatch (globalEngine globalOpts) (globalVerbose globalOpts) cmd
  case result of
    Right () -> pure ()
    Left err -> do
      TextIO.hPutStrLn stderr (renderError err)
      case err of
        EngineError _ n _ -> exitWith (ExitFailure n)
        _                 -> exitWith (ExitFailure 1)

-- | Resolve the container engine, failing if unavailable.
requireEngine :: Maybe ContainerEngine -> IO ContainerEngine
requireEngine (Just e) = pure e
requireEngine Nothing = do
  result <- detectEngine
  case result of
    Right e  -> pure e
    Left err -> do
      TextIO.hPutStrLn stderr (renderError err)
      exitFailure

-- | Resolve scope. Defaults to Local when not specified.
resolveScope :: Maybe Scope -> Scope
resolveScope (Just s) = s
resolveScope Nothing  = Local

-- | Dispatch a parsed command to its handler.
dispatch :: Maybe ContainerEngine -> Bool -> Command -> IO (Either ManagerError ())
dispatch mEngine verbose cmd = case cmd of

  -- ---- install ----
  CmdInstall mScope mVerStr noInit -> do
    engine <- requireEngine mEngine
    let scope = resolveScope mScope
    -- Install the version
    installResult <- case mVerStr of
      Nothing       -> installLatest engine scope
      Just "latest" -> installLatest engine scope
      Just verStr -> case parseVersion verStr of
        Nothing  -> pure (Left (InvalidVersion verStr))
        Just ver -> do
          r <- installVersion engine scope ver
          case r of
            Left err -> pure (Left err)
            Right () -> pure (Right ver)
    case installResult of
      Left err -> pure (Left err)
      Right ver -> do
        info' stderr $ "Installed version " <> Text.unpack (showVersion ver)
        -- Fix system permissions if needed
        whenIO (scope == System) $ do
          cfgP <- configPath scope
          fixSystemPerms cfgP
          vcP <- versionConfigPath scope ver
          fixSystemPerms vcP
        -- Auto-select if no version is currently active
        activeResult <- resolveActiveVersion
        case activeResult of
          Left NoActiveVersion -> do
            _ <- selectVersion scope ver
            info' stderr $ "Auto-selected version " <> Text.unpack (showVersion ver)
          _ -> pure ()
        -- Run morloc init unless --no-init
        if noInit
          then pure (Right ())
          else do
            info' stderr "Running morloc init -f..."
            runInContainer engine verbose False ["morloc", "init", "-f"]

  -- ---- select ----
  CmdSelect mScope verStr -> do
    case parseVersion verStr of
      Nothing  -> pure (Left (InvalidVersion verStr))
      Just ver -> do
        -- Find which scope has this version
        scope <- case mScope of
          Just s  -> pure s
          Nothing -> do
            found <- findInstalledScope ver
            case found of
              Just s  -> pure s
              Nothing -> pure Local  -- will fail with VersionNotInstalled
        result <- selectVersion scope ver
        case result of
          Left err -> pure (Left err)
          Right () -> do
            info' stderr $ "Selected version " <> Text.unpack (showVersion ver)
            pure (Right ())

  -- ---- uninstall ----
  CmdUninstall mScope removeAll verStrs -> do
    if not removeAll && null verStrs
      then pure (Left (InstallError "No versions specified. Use VERSION... or --all."))
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
  CmdRun _mScope shell args -> do
    engine <- requireEngine mEngine
    if not shell && null args
      then pure (Left NoCommand)
      else runInContainer engine verbose shell args

  -- ---- env ----
  CmdEnv mScope envAction -> do
    let scope = resolveScope mScope
    case envAction of
      EnvInit name -> do
        result <- initEnvironment scope name
        case result of
          Left err -> pure (Left err)
          Right path -> do
            info' stderr $ "Created environment Dockerfile: " <> path
            info' stderr "Edit the Dockerfile, then run: morloc-manager env <name>"
            pure (Right ())

      EnvActivate name -> do
        engine <- requireEngine mEngine
        verResult <- resolveActiveVersion
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
        verResult <- resolveActiveVersion
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

  -- ---- info ----
  CmdInfo _mScope -> do
    case mEngine of
      Just _ -> hPutStrLn stderr "Warning: --engine flag has no effect on the info command."
      Nothing -> pure ()
    mCfg <- readActiveConfig
    seMode <- detectSELinux
    let activeScope = maybe Local configActiveScope mCfg
        seStr = case seMode of
          SELinuxEnforcing  -> "enforcing"
          SELinuxPermissive -> "permissive"
          SELinuxDisabled   -> "not detected"
        scopeStr = case activeScope of { Local -> "local"; System -> "system" }
    engineStr <- case mEngine of
      Just e  -> pure (displayEngine e)
      Nothing -> do
        eResult <- detectEngine
        pure $ case eResult of { Right e -> displayEngine e; Left _ -> "not found" }
    cfgRoot <- configDir activeScope
    datRoot <- dataDir activeScope
    case mCfg of
      Nothing -> do
        putStrLn   "Active version: none"
        putStrLn   "Active env:     base"
        putStrLn $ "Engine:         " <> Text.unpack engineStr
      Just cfg -> do
        putStrLn $ "Active version: " <> Text.unpack (maybe "none" showVersion (configActiveVersion cfg))
        putStrLn $ "Active env:     " <> Text.unpack (configActiveEnv cfg)
        putStrLn $ "Engine:         " <> Text.unpack (displayEngine (configEngine cfg))
    putStrLn $ "Scope:          " <> scopeStr
    putStrLn $ "SELinux:        " <> seStr
    putStrLn $ "Config root:    " <> cfgRoot
    putStrLn $ "Data root:      " <> datRoot
    let activeVer = mCfg >>= configActiveVersion
    localVersions <- listVersions Local
    putStrLn ""
    putStrLn "Local versions:"
    if null localVersions
      then putStrLn "  (none)"
      else mapM_ (\v -> putStrLn $ "  " <> Text.unpack (showVersion v)
            <> if Just v == activeVer && activeScope == Local then " (active)" else ""
          ) localVersions
    systemVersions <- listVersions System
    putStrLn ""
    putStrLn "System versions:"
    if null systemVersions
      then putStrLn "  (none)"
      else mapM_ (\v -> putStrLn $ "  " <> Text.unpack (showVersion v)
            <> if Just v == activeVer && activeScope == System then " (active)" else ""
          ) systemVersions
    pure (Right ())

  -- ---- build ----
  CmdBuild _mScope shell mScript -> do
    engine <- requireEngine mEngine
    let args' = case mScript of { Just s -> ["bash", s]; Nothing -> [] }
    runInContainer engine verbose (shell || null args') args'

  -- ---- freeze ----
  CmdFreeze mScope mOutput -> do
    let scope = resolveScope mScope
        outputDir = maybe "./morloc-freeze" id mOutput
    verResult <- resolveActiveVersion
    case verResult of
      Left err -> pure (Left err)
      Right (ver, _) -> freeze scope ver outputDir

  -- ---- serve-image ----
  CmdServeImage tarball tag mBase -> do
    engine <- requireEngine mEngine
    verResult <- resolveActiveVersion
    case verResult of
      Left err -> pure (Left err)
      Right (ver, _) -> buildServeImage engine tarball tag ver mBase

  -- ---- serve ----
  CmdServe image ports -> do
    engine <- requireEngine mEngine
    let containerName = "morloc-serve-" <> image
        portMappings = if null ports then [(8080, 8080)] else ports
    runServeContainer engine image containerName portMappings

-- ======================================================================
-- Container run
-- ======================================================================

-- | Run a command inside the active morloc container.
runInContainer
  :: ContainerEngine -> Bool -> Bool -> [String]
  -> IO (Either ManagerError ())
runInContainer engine verbose shell args = do
  verResult <- resolveActiveVersion
  case verResult of
    Left err -> pure (Left err)
    Right (ver, activeScope) -> do
      let verScope = activeScope
      vcResult <- readVersionConfig verScope ver
      case vcResult of
        Left err -> pure (Left err)
        Right vc -> do
          mCfg <- readActiveConfig
          image <- resolveImage verScope ver vc mCfg
          seMode <- detectSELinux
          let suffix = volumeSuffix seMode
          vDataDir <- versionDataDir verScope ver
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
              when (skipWorkMount) $
                hPutStrLn stderr "Warning: running from home directory with SELinux; working directory mount skipped."
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
        , ("PATH", Text.pack containerHome <> "/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
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
