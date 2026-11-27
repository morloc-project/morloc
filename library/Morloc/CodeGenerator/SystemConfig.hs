{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Morloc.CodeGenerator.SystemConfig
Description : Write runtime files and compile shared libraries during @morloc init@
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Handles the @morloc init@ system setup: writing embedded C library sources,
compiling @libmorloc.so@, building the static nexus binary, and running
per-language @init.sh@ scripts to compile language extensions.
-}
module Morloc.CodeGenerator.SystemConfig
  ( configure
  , configureAll
  , withInitLock
  , pathIsWithin
  , Incoherence (..)
  , compilerIncoherence
  , BuildTool (..)
  , PoolToolchain (..)
  , ToolchainHost (..)
  , toolsOutsidePrefix
  , strictToolchainError
  , managedPrefix
  , prefixFlags
  , prefixRustLinkArgs
  , rustPoolRustflags
  , runtimeInstalled
  , runtimeInstallScript
  , cargoPackageVersion
  , keptRuntimeLink
  , findRuntimeDir
  , nexusAgrees
  , langMarkerText
  , installedRuntimeMismatches
  , Mismatch (..)
  , BindingState (..)
  , bindingState
  , bindingsReportJson
  , checkBindings
  , effectiveInitFlags
  , interpreterIdentity
  , bindingLanguages
  , installRustBinding
  ) where

import qualified System.Info as SI
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.Platform as P
import qualified Morloc.Completion as Completion
import qualified Morloc.Config as Config
import qualified Morloc.DataFiles as DF
import qualified Morloc.System as MS
import Morloc.Module (OverwriteProtocol (..), initPlaneLayout)

import qualified Data.List as DL
import qualified Data.Text as T
import Data.Char (isSpace, toUpper)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8', decodeUtf8Lenient, encodeUtf8)
import Morloc.Abi (abiVersion, headerAbiVersion)
import Morloc.Version (versionStr)
import Morloc.RustBinding (rustBindingDir, rustBindingFiles)
import System.Posix.Process (getProcessID)

import Control.Exception (IOException, SomeAsyncException, SomeException, bracket, catch, displayException, fromException, onException, throwIO, try)
import System.IO.Error (ioeGetErrorString)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, executable, findExecutable, findExecutablesInDirectories, getHomeDirectory, getPermissions, listDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile, renameDirectory, renameFile)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.FilePath (splitSearchPath, takeDirectory, takeFileName)
import System.IO (IOMode (ReadMode), hClose, hIsTerminalDevice, hPutStrLn, stderr, withFile)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hLock, hTryLock)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, readCreateProcessWithExitCode, readProcessWithExitCode, waitForProcess, withCreateProcess)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import System.Timeout (timeout)
import Morloc.Data.PoolHash (hashText)
import Morloc.Data.Json (jsonArr, jsonBool, jsonInt, jsonObj, jsonStr)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

configureAll :: Bool -> OverwriteProtocol -> Bool -> Bool -> Config -> IO Bool
configureAll verbose force slurmSupport sanitize config = do
  result <- try (withInitLock verbose (configHome config)
                   (configureAllSteps verbose force slurmSupport sanitize config))
              :: IO (Either SomeException ())
  case result of
    Left e -> do
      -- Strip the "user error (...)" wrapper from IOError messages
      let msg = case fromException e :: Maybe IOError of
            Just ioe -> ioeGetErrorString ioe
            Nothing -> displayException e
      sayError $ "Configuration failed: " ++ msg
      return False
    Right _ -> return True

-- | Serialize this @morloc init@ against any other sharing the same
-- @MORLOC_HOME@. Init is an install: it removes and rewrites artifacts at
-- fixed shared paths, so two concurrent runs interleave destructive steps and
-- one fails on a file the other has already deleted. An advisory lock on the
-- home directory makes the second run wait and then take its own pass. The
-- lock lives in an open descriptor rather than the filesystem, so it is
-- released even if the process is killed, leaving nothing stale to clear by
-- hand.
withInitLock :: Bool -> FilePath -> IO a -> IO a
withInitLock verbose homeDir act = do
  createDirectoryIfMissing True homeDir
  bracket (MS.openLockFile (homeDir </> ".init.lock")) hClose $ \h -> do
    held <- hTryLock h ExclusiveLock
    unless held $ do
      sayInfo verbose "Waiting for another morloc init to finish"
      hLock h ExclusiveLock
    act

-- | The SLURM and sanitize settings an init builds with: an incremental init
-- (the on-demand language add) keeps what the home records and can only turn
-- a setting on; a full init takes its flags as given.
effectiveInitFlags :: Bool -> (Bool, Bool) -> (Maybe Bool, Maybe Bool) -> (Bool, Bool)
effectiveInitFlags incremental (slurmFlag, sanitizeFlag) (slurmStored, sanitizeStored)
  | incremental = (slurmFlag || slurmStored == Just True, sanitizeFlag || sanitizeStored == Just True)
  | otherwise = (slurmFlag, sanitizeFlag)

configureAllSteps :: Bool -> OverwriteProtocol -> Bool -> Bool -> Config -> IO ()
configureAllSteps verbose force slurmFlag sanitizeFlag config = do
  let homeDir = configHome config
      srcLibrary = configLibrary config
      includeDir = homeDir </> "include"
      tmpDir = configTmpDir config
      optDir = homeDir </> "opt"
      libDir = homeDir </> "lib"

  -- The active strict-mode conda prefix, resolved once and shared by the
  -- coherence guard and the env-aware language-setup loop below.
  mStrictPrefix <- strictCondaPrefix

  -- Incremental mode (@MORLOC_INIT_INCREMENTAL@, set by the in-env agent on the
  -- on-demand shim-build path): skip the already-built runtime and language shims
  -- (marker-driven) and build only what is missing. A bare @morloc init@ (flag
  -- unset) rebuilds everything, so it can never silently keep an ABI-stale runtime
  -- after a compiler upgrade.
  incremental <- envFlag "MORLOC_INIT_INCREMENTAL"
  existingBuildConfig <- Config.loadBuildConfig config
  let (slurmSupport, sanitize) =
        effectiveInitFlags incremental (slurmFlag, sanitizeFlag)
          (buildConfigSlurmSupport existingBuildConfig, buildConfigSanitize existingBuildConfig)

  -- Admission control FIRST, before any destructive step: a failed coherence
  -- check must abort before the runtime build and the force-clean below,
  -- otherwise a rejected env leaves MORLOC_HOME gutted (no libmorloc, no nexus).
  checkCondaCoherence verbose mStrictPrefix

  ensureDirectory verbose "morloc home directory" homeDir
  ensureDirectory verbose "morloc lib directory" libDir
  ensureDirectory verbose "morloc include directory" includeDir
  -- Installed-module resources are STATE, under the state root (not the runtime
  -- lib/); pre-create the per-language dirs there.
  ensureDirectory verbose "morloc python module directory" (Config.moduleDir config "python")
  ensureDirectory verbose "morloc R module directory" (Config.moduleDir config "R")
  ensureDirectory verbose "morloc cpp module include directory" (Config.moduleDir config "include")
  ensureDirectory verbose "morloc cpp module lib directory" (Config.moduleDir config "lib")
  ensureDirectory verbose "morloc tmp directory" tmpDir
  ensureDirectory verbose "morloc opt directory" optDir
  ensureDirectory verbose "morloc module directory" srcLibrary
  -- PLANE-1: a plane of an older layout stops init with the command to remove it
  initPlaneLayout config

  sayInfo verbose $ "Slurm support ... " <> show slurmSupport

  sayInfo verbose $ "Sanitize ... " <> show sanitize

  sayInfo verbose "Writing build config file"
  -- Read-modify-write: preserve any existing fields (e.g. `lang-params` set via
  -- `morloc config`) and only override the two flags `morloc init` owns.
  Config.writeBuildConfig
    config
    existingBuildConfig
      { buildConfigSlurmSupport = Just slurmSupport
      , buildConfigSanitize = Just sanitize
      }

  -- Clean and create build directory
  let buildDir = tmpDir </> "libmorloc-build"
  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir
  createDirectoryIfMissing True buildDir

  requireTool "gcc" "gcc (or the compiler named by $CC) is required to compile language extensions (C++ pools, Python/R bindings)"
  -- Export $AR/$STRIP for the cargo/cc-rs builds and the shim init scripts. A
  -- conda toolchain ships these only under a target-prefixed name and sets them
  -- on activation, but not every launch path activates; derive them from the C
  -- compiler so the builds do not fall back to a bare `ar`/`strip` that conda
  -- does not provide.
  exportArchiveTools

  -- Build libmorloc.so + morloc-nexus (and the language shims)
  -- FROM SOURCE with the active toolchain. This is the single provisioning path:
  -- the runtime that pools link and load MUST be ABI-coherent with the toolchain
  -- that compiles those pools, so a prebuilt binary from a foreign libc/toolchain
  -- world is not an option. Only the morloc compiler and mim are
  -- shipped prebuilt (they never link into a pool).
  let soPath = libDir </> P.sharedLibName P.hostPlatform "morloc"
      nexusBinDir = homeDir </> "bin"
      nexusBinPath = nexusBinDir </> "morloc-nexus"
  createDirectoryIfMissing True nexusBinDir
  -- Resolve the user's home for the legacy ~/.local/bin/ symlink fallback
  -- (see the MORLOC_BIN_LINK_DIR handling below).
  userHome <- getHomeDirectory

  -- Build the Rust runtime (libmorloc.so + morloc-nexus) when it is missing, when the installed one does not hold together
  -- or does not match this compiler (see nexusAgrees), or when a full
  -- rebuild is forced. An in-environment on-demand language add re-runs
  -- `morloc init` WITHOUT force to build a newly provisioned language's shim;
  -- an intact runtime is not relinked (and cargo is not required) on that
  -- path. The manager's provision always uses force.
  keptRuntimeLink homeDir >>= either (ioError . userError) return
  runtimeBuilt <- runtimeInstalled homeDir
  agrees <- if runtimeBuilt && force /= ForceOverwrite then nexusAgrees homeDir else return (Left "")
  case agrees of
    Left why | not (null why) -> sayWarning ("Rebuilding the runtime: " <> why)
    _ -> return ()
  let rebuildRuntime = force == ForceOverwrite || either (const True) (const False) agrees || not incremental
  if rebuildRuntime
    then do
      provisionRustRuntime verbose config homeDir
      nexusAgrees homeDir >>= either (ioError . userError) return
    else sayInfo verbose "Runtime (libmorloc + morloc-nexus + morloc.h) already built; skipping"

  when (force == ForceOverwrite) $ do
    sayInfo verbose "Force rebuild: cleaning init-owned artifacts"
    forM_ (filter (/= takeFileName soPath) initOwnedLibPaths) $ \p -> removePathIfExists (libDir </> p)
    forM_ (filter (/= "morloc.h") initOwnedIncludePaths) $ \p -> removePathIfExists (includeDir </> p)
    cleanDirectory optDir

  -- Symlink the newly installed binaries into a "user bin" directory so
  -- they end up on PATH. The directory is selected by the
  -- MORLOC_BIN_LINK_DIR environment variable:
  --
  --   Just <non-empty path>  - create that directory if missing and
  --                            symlink into it (caller-controlled target)
  --   Just ""                - explicit skip, no symlinks created
  --   Nothing                - default: symlink into ~/.local/bin/
  --                            iff that directory already exists
  --
  -- The skip-on-empty case lets a caller (e.g. mim when running
  -- this init inside a container) suppress symlinking without baking
  -- container-awareness into this module. The container harness sets
  -- MORLOC_BIN_LINK_DIR to a morloc-owned subtree such as
  -- $HOME/.local/share/morloc/bin so the resulting symlinks land in a
  -- non-shared location instead of clobbering files in the user's
  -- general-purpose ~/.local/bin/.
  -- Linking binaries onto PATH is a convenience: the binaries are always
  -- reachable under $MORLOC_HOME/bin regardless. A failure here (e.g. the link
  -- dir is an unwritable host path under a non-root container UID) must warn,
  -- not abort init.
  optionalStep "Could not link morloc binaries onto PATH (they remain available under $MORLOC_HOME/bin): " $ do
    linkDirEnv <- lookupEnv "MORLOC_BIN_LINK_DIR"
    mLinkDir <- case linkDirEnv of
      Just "" -> return Nothing
      Just dir -> do
        createDirectoryIfMissing True dir
        return (Just dir)
      Nothing -> do
        let defaultBin = userHome </> ".local" </> "bin"
        exists <- doesDirectoryExist defaultBin
        return $ if exists then Just defaultBin else Nothing
    case mLinkDir of
      Nothing -> return ()
      Just linkDir -> do
        symlinkBinary nexusBinPath (linkDir </> "morloc-nexus")
        let managerSrc = nexusBinDir </> "mim"
        managerExists <- doesFileExist managerSrc
        when managerExists $
          symlinkBinary managerSrc (linkDir </> "mim")

  -- Compiled programs start the nexus by name, so it has to be found on PATH,
  -- and be this install's. Nothing above guarantees either: the link
  -- directory may not exist (a fresh macOS has no ~/.local/bin) or may not be
  -- on PATH, and another install may come first.
  onPath <- findExecutable "morloc-nexus"
  installed <- canonicalizePath nexusBinPath
  case onPath of
    Nothing ->
      sayWarning $ "morloc-nexus is not on PATH, so compiled programs cannot start;"
        <> " add " <> nexusBinDir <> " to PATH"
    Just found -> do
      resolved <- canonicalizePath found
      when (resolved /= installed) $
        sayWarning $ "morloc-nexus on PATH is " <> found <> ", not this install's "
          <> nexusBinPath <> "; compiled programs will run the former"

  -- Create exe/ and fdb/ directories (STATE, under the state root)
  let exeDir = Config.exeDir config
      fdbDir = Config.fdbDir config
  ensureDirectory verbose "morloc exe directory" exeDir
  ensureDirectory verbose "morloc fdb directory" fdbDir

  -- Configure each language via its init.sh script.
  --
  -- Language SELECTION is environment-aware in strict mode: a language is set up
  -- only when its toolchain resolves INSIDE the conda prefix -- the conda env IS
  -- the intended language set. A host tool for a language the env does not
  -- include (e.g. a stray /usr/bin/R on a CI runner) is NOT built, so we
  -- never compile a shim that mixes a host toolchain with conda's libmorloc.
  -- In non-strict mode, selection stays PATH-based (any tool on PATH), preserving
  -- lenient system-toolchain dev installs.
  --
  -- Shim BUILD is fail-closed in strict mode: a compile failure ABORTS init with
  -- the error, rather than the best-effort warn that silently leaves a half-
  -- configured env whose failure only surfaces later as a runtime import error.
  --
  -- SHIM-1: a per-language marker records what a shim was built from and
  -- against. In incremental mode (the on-demand agent path) a language whose
  -- binding is current is skipped and every other one is rebuilt. A bare init
  -- (non-incremental) rebuilds every available language. The markers live
  -- under opt/, which the force-clean above wipes, so `-f` also rebuilds every
  -- available language.
  let langMarkerDir = optDir </> "lang-configured"
  mBuildId <- installedBuildId homeDir
  createDirectoryIfMissing True langMarkerDir
  forM_ DF.langSetups $ \ls -> do
    let tools = DF.lsRequiredTools ls
        marker = langMarkerDir </> DF.lsName ls
    unavailable <- filterM (fmap not . toolAvailableInEnv mStrictPrefix) tools
    identity <- if null unavailable then resolveManagedIdentity ls else return (Left BindingAbsent)
    alreadyBuilt <- (== BindingOk) <$> bindingState homeDir sanitize mBuildId ls (Just identity)
    if not (null unavailable)
      then
        let reason = case mStrictPrefix of
              Just _ -> "not provided by the conda environment"
              Nothing -> "missing"
        in sayWarning $ "Skipping " <> DF.lsName ls <> " setup (" <> reason <> ": " <> unwords unavailable <> ")"
      else if either (const True) (const False) identity
        then
          let why = case identity of
                Left (BindingUnavailable w) -> w
                Left (BindingOutsidePrefix w) -> w
                _ -> "its interpreter cannot be used"
          in case mStrictPrefix of
               Just _ -> ioError (userError ("cannot set up " <> DF.lsName ls <> ": " <> why))
               Nothing -> sayWarning $ "Skipping " <> DF.lsName ls <> " setup (" <> why <> ")"
      else if alreadyBuilt && force /= ForceOverwrite && incremental
        then sayInfo verbose $ DF.lsName ls <> " extension already built; skipping"
        else do
          hPutStrLn stderr $ "Building " <> DF.lsName ls <> " extension ..."
          sayInfo verbose $ "Configuring " <> DF.lsName ls <> " language support"
          -- Write data files to build dir
          forM_ (DF.lsFiles ls) $ \ef ->
            TIO.writeFile (buildDir </> DF.embededFileName ef) (DF.embededFileText ef)
          -- Write and run init script
          let initPath = buildDir </> "init.sh"
          TIO.writeFile initPath (DF.embededFileText (DF.lsInitScript ls))
          let sanitizeFlagsStr = sanitizeFlags sanitize
          -- Run the setup, and only on success record the marker + clean up.
          -- Grouping the marker write with the build inside `optionalStep` keeps
          -- a non-strict failure from marking the language as built.
          let buildShim = do
                run verbose "bash" [initPath, homeDir, buildDir, sanitizeFlagsStr]
                removeFileSafe initPath
                forM_ (DF.lsFiles ls) $ \ef ->
                  removeFileSafe (buildDir </> DF.embededFileName ef)
                case (mBuildId, identity) of
                  (Just bid, Right ident) -> writeMarker marker (langMarkerText bid ident sanitize ls)
                  _ -> return ()
          case mStrictPrefix of
            Just _ -> buildShim
            Nothing -> optionalStep (DF.lsName ls <> " setup failed: ") buildShim

  -- SHIM-6: install this compiler's Rust binding (`-f` rewrites it), then
  -- warm the pool build cache when the runtime under it is new or the cache
  -- was never warmed for it. Rust is a capability, not a core install step:
  -- a failed install leaves Rust unavailable with a warning, and `morloc
  -- make` refuses Rust programs until it is repaired.
  optionalStep "Rust binding install failed (Rust pools will not be available): " $
    void (installRustBinding verbose (force == ForceOverwrite) homeDir)
  warmed <- doesFileExist (rustBindingDir homeDir </> ".warmed")
  when (rebuildRuntime || not warmed) $
    optionalStep "Rust build cache not warmed (the first Rust pool build will compile its dependencies): " $ do
      warmRustBinding verbose config homeDir
      BS.writeFile (rustBindingDir homeDir </> ".warmed") BS.empty

  -- Generate shell completions
  sayInfo verbose "Generating shell completions"
  Completion.regenerateCompletions verbose (Config.exeDir config) (homeDir </> "completions")

-- | Whether a home holds a runtime this compiler can use as it is:
-- libmorloc, the nexus, their recorded build id, and the header they were
-- installed with, which must be the header this compiler was built with.
runtimeInstalled :: FilePath -> IO Bool
runtimeInstalled homeDir = do
  let header = homeDir </> "include" </> "morloc.h"
  present <- and <$> mapM doesFileExist
    [ homeDir </> "lib" </> P.sharedLibName P.hostPlatform "morloc"
    , homeDir </> "bin" </> "morloc-nexus"
    , buildIdFile homeDir
    , header
    ]
  if present
    then (== encodeUtf8 DF.libmorlocHeader) <$> BS.readFile header
    else return False

-- | The install script of a runtime source directory, or why it cannot be
-- used (SHIM-7). Init installs the source's morloc.h for the bindings and
-- pools to compile against, so it must be the header this compiler was built
-- with; and the nexus refuses manifests from another morloc version (ART-5),
-- so the source's crates must carry this compiler's version.
runtimeInstallScript :: FilePath -> IO (Either String FilePath)
runtimeInstallScript runtimeDir =
  either
    (\why -> Left (why <> "; set MORLOC_RUNTIME_DIR to the runtime/ directory this compiler was built from and re-run `morloc init -f`"))
    Right
    <$> runtimeSource runtimeDir

-- | 'runtimeInstallScript' without the remedy.
runtimeSource :: FilePath -> IO (Either String FilePath)
runtimeSource runtimeDir = do
  let script = runtimeDir </> "install.sh"
      header = runtimeDir </> "include" </> "morloc.h"
      crateFile c = runtimeDir </> c </> "Cargo.toml"
      crates = ["morloc-manifest", "morloc-nexus", "morloc-runtime", "morloc-runtime-types"]
      refuse why = Left ("the runtime source at " <> runtimeDir <> " " <> why)
  hasCargo <- doesFileExist (runtimeDir </> "Cargo.toml")
  hasScript <- doesFileExist script
  hasHeader <- doesFileExist header
  missingCrates <- filterM (fmap not . doesFileExist . crateFile) crates
  if not hasCargo
    then return (refuse "is not a runtime source (it has no Cargo.toml)")
    else if not hasScript
      then return (refuse "predates this compiler (it has no install.sh)")
      else if not hasHeader
        then return (refuse ("has no " <> header))
        else case missingCrates of
          c : _ -> return (refuse ("has no " <> crateFile c))
          [] -> do
            bytes <- BS.readFile header
            versions <- forM crates $ \c ->
              (,) c . either (const Nothing) cargoPackageVersion . decodeUtf8' <$> BS.readFile (crateFile c)
            let wrong = [(c, v) | (c, v) <- versions, v /= Just (T.pack versionStr)]
            return $
              if bytes /= encodeUtf8 DF.libmorlocHeader
                then refuse $ case decodeUtf8' bytes of
                  Right text | Just n <- headerAbiVersion text, n /= abiVersion ->
                    "declares ABI " <> show n <> " in include/morloc.h, but this compiler emits code for ABI "
                      <> show abiVersion
                  _ -> "has an include/morloc.h that differs from the header this compiler was built with"
                else case wrong of
                  (c, v) : _ -> refuse $
                    "has " <> c <> " " <> maybe "of an unreadable version" T.unpack v
                      <> ", but this compiler is morloc " <> versionStr
                  [] -> Right script

-- | Whether a home's @runtime@, when it is a link, reaches a source this
-- compiler accepts. Init never replaces the link, and Rust pools and the Rust
-- binding build against it (SHIM-7).
keptRuntimeLink :: FilePath -> IO (Either String ())
keptRuntimeLink homeDir = do
  let link = homeDir </> "runtime"
  isLink <- pathIsSymbolicLink link `catch` \(_ :: IOException) -> return False
  if not isLink
    then return (Right ())
    else do
      target <- canonicalizePath link `catch` \(_ :: IOException) -> return link
      either
        (\why -> Left $
          link <> " is a link, which init keeps and Rust pools build against, but " <> why
            <> "; point it at the runtime/ directory this compiler was built from, or remove it, and re-run `morloc init -f`")
        (const (Right ()))
        <$> runtimeSource target

-- | The version in a Cargo.toml's @[package]@ table: a quoted string, with
-- any comment after it.
cargoPackageVersion :: T.Text -> Maybe T.Text
cargoPackageVersion text =
  case [ v
       | l <- takeWhile (not . isTable) (drop 1 (dropWhile (/= "[package]") (map uncomment (T.lines text))))
       , Just rest <- [T.stripPrefix "version" l]
       , Just v0 <- [T.stripPrefix "=" (T.strip rest)]
       , Just v <- [quoted '"' (T.strip v0) <|> quoted '\'' (T.strip v0)]
       ] of
    [v] -> Just v
    _ -> Nothing
  where
    isTable l = "[" `T.isPrefixOf` l
    -- A version holds no '#', so everything from one on is a comment.
    uncomment = T.strip . T.takeWhile (/= '#')
    quoted q t = T.stripPrefix (T.singleton q) t >>= T.stripSuffix (T.singleton q)

-- | Whether the installed nexus accepts the installed libmorloc (LIFE-6),
-- reports the build id install.sh recorded for them, speaks the ABI this
-- compiler emits code for and is this compiler's version (SHIM-7), or why
-- not.
nexusAgrees :: FilePath -> IO (Either String ())
nexusAgrees homeDir = do
  let nexus = homeDir </> "bin" </> "morloc-nexus"
  -- The nexus must load the libmorloc beside it, found through its rpath.
  env0 <- getEnvironment
  let cleared = [kv | kv@(k, _) <- env0, k `notElem` ["LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"]]
      ask arg = try (readCreateProcessWithExitCode ((proc nexus [arg]) {env = Just cleared}) "")
        :: IO (Either SomeException (ExitCode, String, String))
  built <- ask "--build-id"
  case built of
    Left e -> return (Left ("the installed " <> nexus <> " cannot run: " <> displayException e))
    Right (ExitFailure code, _, err) -> return . Left $
      "the installed " <> nexus <> " fails its build check (exit " <> show code
        <> "; it refuses a libmorloc from another build, or cannot load one): " <> trimEnd err
    Right (ExitSuccess, out, _) -> do
      recorded <- installedBuildId homeDir
      reported <- ask "--abi-version"
      version <- ask "--version"
      let expectedVersion = "morloc-nexus " <> versionStr
      return $ case (reported, version) of
        _ | recorded /= Just (T.strip (T.pack out)) -> Left $
          "the build id recorded at " <> buildIdFile homeDir <> " is not the installed nexus's ("
            <> trimEnd out <> "); the last runtime install did not finish"
        (Right (ExitSuccess, abi, _), _) | readMaybe (trimEnd abi) /= Just abiVersion -> Left $
          "the installed " <> nexus <> " speaks ABI " <> trimEnd abi
            <> ", but this compiler emits code for ABI " <> show abiVersion
        (Right (ExitSuccess, _, _), Right (ExitSuccess, v, _))
          | trimEnd v == expectedVersion -> Right ()
          | otherwise -> Left $
              "the installed " <> nexus <> " is " <> trimEnd v <> ", but this compiler is morloc " <> versionStr
        (Right (ExitFailure _, _, err), _) -> Left ("the installed " <> nexus <> " cannot report its ABI: " <> trimEnd err)
        (Left e, _) -> Left ("the installed " <> nexus <> " cannot run: " <> displayException e)
        (_, Right (ExitFailure _, _, err)) -> Left ("the installed " <> nexus <> " cannot report its version: " <> trimEnd err)
        (_, Left e) -> Left ("the installed " <> nexus <> " cannot run: " <> displayException e)

-- | The build id install.sh recorded for the installed runtime.
buildIdFile :: FilePath -> FilePath
buildIdFile homeDir = homeDir </> "lib" </> "morloc.build-id"

installedBuildId :: FilePath -> IO (Maybe T.Text)
installedBuildId homeDir = do
  let f = buildIdFile homeDir
  exists <- doesFileExist f
  if exists
    then either (const Nothing) (Just . T.strip) . decodeUtf8' <$> BS.readFile f
    else return Nothing

-- | The compiler flags a binding is built with under the sanitize setting.
sanitizeFlags :: Bool -> String
sanitizeFlags sanitize = if sanitize then "-fsanitize=alignment -fno-sanitize-recover=alignment" else ""

-- | SHIM-1: what a binding was built from and against, as its marker
-- records it: the runtime build id, the interpreter's identity, the sanitize
-- setting, and a hash of the binding's sources (the header, its init script
-- and files).
data BindingRecord = BindingRecord
  { brBuildId :: T.Text
  , brIdentity :: T.Text
  , brSanitize :: Bool
  , brSources :: T.Text
  }
  deriving (Eq, Show)

bindingSources :: DF.LangSetup -> T.Text
bindingSources ls =
  hashText . T.concat $
    DF.libmorlocHeader
      : concat [[T.pack (DF.embededFileName ef), "\0", DF.embededFileText ef, "\0"] | ef <- DF.lsInitScript ls : DF.lsFiles ls]

renderRecord :: BindingRecord -> String
renderRecord r =
  T.unpack . T.unlines $
    [ "morloc-binding 1"
    , "build-id " <> brBuildId r
    , "identity " <> brIdentity r
    , "sanitize " <> (if brSanitize r then "1" else "0")
    , "sources " <> brSources r
    ]

parseRecord :: BS.ByteString -> Maybe BindingRecord
parseRecord bytes = do
  text <- either (const Nothing) Just (decodeUtf8' bytes)
  let fields = [(k, T.drop 1 v) | l <- T.lines text, let (k, v) = T.breakOn " " l]
  version <- lookup "morloc-binding" fields
  if version /= "1" then Nothing else do
    sanitize <- lookup "sanitize" fields
    BindingRecord
      <$> lookup "build-id" fields
      <*> lookup "identity" fields
      <*> pure (sanitize == "1")
      <*> lookup "sources" fields

-- | The marker text for a binding built now with the given inputs.
langMarkerText :: T.Text -> T.Text -> Bool -> DF.LangSetup -> String
langMarkerText buildId identity sanitize ls = renderRecord (BindingRecord buildId identity sanitize (bindingSources ls))

-- | Write a marker beside its destination and rename it into place.
writeMarker :: FilePath -> String -> IO ()
writeMarker marker text = do
  let tmp = marker <> ".tmp"
  writeFile tmp text
  renameFile tmp marker

-- | Whether a language's installed binding is the one this compiler would
-- build now. Ok, absent (never built), stale (built by another compiler,
-- against another runtime or interpreter, with other settings, or its
-- artifact is gone), unavailable (its interpreter cannot be found or run, so
-- building it is pointless), or outside-prefix (in a managed environment, the
-- interpreter on PATH is not the environment's).
data BindingState
  = BindingOk
  | BindingAbsent
  | BindingStale String
  | BindingUnavailable String
  | BindingOutsidePrefix String
  deriving (Eq, Show)

-- | The canonical names of the languages that have a binding.
bindingLanguages :: [T.Text]
bindingLanguages = map DF.lsLang DF.langSetups ++ ["rust"]

-- | SHIM-6: whether this compiler's Rust binding is installed in the home.
rustBindingState :: FilePath -> IO BindingState
rustBindingState homeDir = do
  done <- doesFileExist (rustBindingDir homeDir </> ".complete")
  return (if done then BindingOk else BindingAbsent)

-- | The interpreter a language's binding is built against and loaded by, if
-- it has one.
langInterpreter :: DF.LangSetup -> Maybe String
langInterpreter ls = case T.unpack (DF.lsLang ls) of
  "py" -> Just "python3"
  "r" -> Just "R"
  _ -> Nothing

-- | The file a configured binding must have, relative to the home, given the
-- identity it was built for (Python's module is named by its extension
-- suffix, the identity's first word).
langArtifact :: DF.LangSetup -> T.Text -> Maybe FilePath
langArtifact ls identity = case T.unpack (DF.lsLang ls) of
  "py" -> Just ("opt" </> ("pymorloc" <> T.unpack (T.takeWhile (not . isSpace) identity)))
  "r" -> Just ("lib" </> "librmorloc.so")
  "cpp" -> Just ("lib" </> "libcppmorloc.a")
  _ -> Nothing

-- | The identity of an interpreter as a binding depends on it: for Python its
-- extension suffix (version and ABI flags, which also name the built module)
-- and numpy's major version (the binding is built against numpy's C API);
-- for R its major and minor version.
interpreterIdentity :: DF.LangSetup -> FilePath -> IO (Either String T.Text)
interpreterIdentity ls exe = case T.unpack (DF.lsLang ls) of
  "py" ->
    fmap firstLine <$> probe ["-I", "-c", "import sysconfig, numpy; print(sysconfig.get_config_var('EXT_SUFFIX'), 'numpy', numpy.__version__.split('.')[0])"]
  "r" -> (>>= rMinor) <$> probe ["--version"]
  other -> return (Left ("no interpreter identity is defined for " <> other))
  where
    firstLine out = T.strip (T.takeWhile (/= '\n') out)
    -- R's front end can print a warning about R_HOME before the version.
    rMinor out = case [ws | l <- T.lines out, ws@("R" : "version" : _) <- [T.words l]] of
      ((_ : _ : v : _) : _) | (major : minor : _) <- T.splitOn "." v -> Right ("R " <> major <> "." <> minor)
      _ -> Left (exe <> " --version printed no R version line")
    probe args = do
      result <- try (timeout 30000000 (runBytes exe args))
                  :: IO (Either IOException (Maybe (ExitCode, BS.ByteString, BS.ByteString)))
      return $ case result of
        Left e -> Left (exe <> " cannot run: " <> displayException e)
        Right Nothing -> Left (exe <> " " <> unwords args <> " did not finish within 30 seconds")
        Right (Just (ExitSuccess, out, _))
          | T.all isSpace (decodeUtf8Lenient out) -> Left (exe <> " " <> unwords args <> " printed nothing")
          | otherwise -> Right (decodeUtf8Lenient out)
        Right (Just (ExitFailure code, _, err)) ->
          Left (exe <> " " <> unwords args <> " failed (exit " <> show code <> "): " <> trimEnd (T.unpack (decodeUtf8Lenient err)))

-- | Run a program with no input and R_HOME unset, and read its output as
-- bytes, whatever the locale.
runBytes :: FilePath -> [String] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runBytes exe args = do
  env0 <- getEnvironment
  withFile "/dev/null" ReadMode $ \devNull ->
    withCreateProcess
      (proc exe args)
        { std_in = UseHandle devNull
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env = Just [kv | kv@(k, _) <- env0, k /= "R_HOME"]
        }
      $ \_ mOut mErr ph ->
        case (mOut, mErr) of
          (Just hOut, Just hErr) -> do
            errVar <- newEmptyMVar
            _ <- forkIO ((try (BS.hGetContents hErr) :: IO (Either IOException BS.ByteString)) >>= putMVar errVar)
            out <- BS.hGetContents hOut
            err <- either (const BS.empty) id <$> takeMVar errVar
            code <- waitForProcess ph
            return (code, out, err)
          _ -> ioError (userError ("could not read the output of " <> exe))

-- | Find a language's interpreter on PATH and take its identity, under the
-- managed environment's prefix rule ('managedPrefix'): an interpreter outside
-- the prefix is reported rather than used, and a managed environment that
-- cannot be resolved makes every binding with an interpreter unavailable.
resolveManagedIdentity :: DF.LangSetup -> IO (Either BindingState T.Text)
resolveManagedIdentity ls = case langInterpreter ls of
  Nothing -> return (Right T.empty)
  Just _ -> do
    prefix <- managedPrefix
    case prefix of
      Left err -> return (Left (BindingUnavailable err))
      Right mPrefix -> resolveIdentity mPrefix ls

-- | Find a language's interpreter on PATH and take its identity. With a
-- prefix, an interpreter outside it is reported rather than used.
resolveIdentity :: Maybe FilePath -> DF.LangSetup -> IO (Either BindingState T.Text)
resolveIdentity mPrefix ls = case langInterpreter ls of
  Nothing -> return (Right T.empty)
  Just tool -> do
    loc <- resolveToolLocation tool
    case loc of
      Nothing -> return (Left (BindingUnavailable (tool <> " is not on PATH")))
      Just (exe, dirAbs)
        | Just prefix <- mPrefix, not (pathIsWithin prefix dirAbs) ->
            return . Left . BindingOutsidePrefix $
              "the " <> tool <> " on PATH (" <> exe <> ") is not the environment's (" <> prefix </> "bin" <> ")"
        | otherwise -> either (Left . BindingUnavailable) Right <$> interpreterIdentity ls exe

-- | The state of a language's installed binding. With a probe (init and
-- @init --check@, SHIM-2) the interpreter found now is compared with the one
-- the binding was built for; without one (@morloc make@, SHIM-3) only the
-- recorded facts are checked, and the binding checks its interpreter when it
-- loads (SHIM-5).
bindingState :: FilePath -> Bool -> Maybe T.Text -> DF.LangSetup -> Maybe (Either BindingState T.Text) -> IO BindingState
bindingState homeDir sanitize mBuildId ls probe = do
  let marker = homeDir </> "opt" </> "lang-configured" </> DF.lsName ls
  configured <- doesFileExist marker
  if not configured
    then return BindingAbsent
    else do
      record <- parseRecord <$> BS.readFile marker
      case (record, mBuildId, probe) of
        (Nothing, _, _) -> return (BindingStale "its marker was written by another compiler")
        (_, Nothing, _) -> return (BindingStale ("no runtime build is recorded at " <> buildIdFile homeDir))
        (_, _, Just (Left st)) -> return st
        (Just r, Just buildId, _)
          | brBuildId r /= buildId ->
              return . BindingStale $
                "it was built against runtime build " <> T.unpack (brBuildId r)
                  <> ", and the installed runtime is build " <> T.unpack buildId
          | brSources r /= bindingSources ls ->
              return (BindingStale "it was built from another compiler's binding sources")
          | brSanitize r /= sanitize ->
              return . BindingStale $
                "it was built with sanitizing " <> onOff (brSanitize r) <> ", and this home builds with it " <> onOff sanitize
          | Just (Right identity) <- probe, identity /= brIdentity r ->
              return . BindingStale $
                "it was built for " <> T.unpack (brIdentity r) <> ", and the interpreter found now is " <> T.unpack identity
          | otherwise -> do
              let artifact = maybe (homeDir </> "unknown binding artifact") (homeDir </>) (langArtifact ls (brIdentity r))
              present <- doesFileExist artifact
              return (if present then BindingOk else BindingStale (artifact <> " is missing"))
  where
    onOff b = if b then "on" else "off"

-- | How the runtime and bindings installed in a home differ from what this
-- compiler, under the given sanitize setting, emits code for (SHIM-3): the
-- header, the runtime's recorded build id, and each of the given languages
-- (canonical names) whose recorded binding is not current. Reads files
-- only; runs no interpreter. Empty when they agree.
installedRuntimeMismatches :: Bool -> [T.Text] -> FilePath -> IO [Mismatch]
installedRuntimeMismatches sanitize langNames homeDir = do
  mBuildId <- installedBuildId homeDir
  headerOk <- installedHeaderOk homeDir
  langs <- case mBuildId of
    Nothing -> return []
    Just _ -> fmap concat . forM [ls | ls <- DF.langSetups, DF.lsLang ls `elem` langNames] $ \ls -> do
      st <- bindingState homeDir sanitize mBuildId ls Nothing
      return [Mismatch True (describeBinding ls st <> " (in " <> homeDir <> ")") | st /= BindingOk]
  rust <- if "rust" `elem` langNames then rustBindingState homeDir else return BindingOk
  return $
    [Mismatch True (homeDir </> "include" </> "morloc.h" <> " is not the header this compiler was built with") | not headerOk]
      ++ [Mismatch True ("no runtime build is recorded at " <> buildIdFile homeDir) | isNothing mBuildId]
      ++ langs
      ++ [Mismatch True ("the Rust binding (rustmorloc) is not installed at " <> rustBindingDir homeDir) | rust /= BindingOk]

-- | One way an installed home does not suit this compiler, and whether an
-- init repairs it (or the interpreters or environment must be fixed first).
data Mismatch = Mismatch {mismatchInitRepairs :: Bool, mismatchText :: String}
  deriving (Eq, Show)

installedHeaderOk :: FilePath -> IO Bool
installedHeaderOk homeDir = do
  let header = homeDir </> "include" </> "morloc.h"
  exists <- doesFileExist header
  if exists then (== encodeUtf8 DF.libmorlocHeader) <$> BS.readFile header else return False

describeBinding :: DF.LangSetup -> BindingState -> String
describeBinding ls st = "the " <> DF.lsName ls <> " binding " <> case st of
  BindingOk -> "is current"
  BindingAbsent -> "is not built"
  BindingStale why -> "is out of date: " <> why
  BindingUnavailable why -> "cannot be built: " <> why <> maybe "" (const " (put the interpreter on PATH, or fix the environment)") (langInterpreter ls)
  BindingOutsidePrefix why -> "cannot be checked: " <> why <> " (run in the environment, with its bin first on PATH)"

-- | What @morloc init --check@ prints: whether the runtime holds together
-- and matches this compiler, and each language's binding state. Exits 0;
-- reads only, so it runs in a read-only home.
checkBindings :: Config -> [T.Text] -> IO T.Text
checkBindings config langNames =
  checkBindingsUnguarded config langNames `catch` \(e :: SomeException) ->
    case fromException e :: Maybe SomeAsyncException of
      Just a -> throwIO a
      Nothing -> return (bindingsReportJson (Left ("the check could not run: " <> displayException e)) [])

checkBindingsUnguarded :: Config -> [T.Text] -> IO T.Text
checkBindingsUnguarded config langNames = do
  let homeDir = configHome config
  buildConfig <- Config.loadBuildConfig config
  let sanitize = buildConfigSanitize buildConfig == Just True
      chosen = [ls | ls <- DF.langSetups, null langNames || DF.lsLang ls `elem` langNames]
      wantRust = null langNames || "rust" `elem` langNames
  mBuildId <- installedBuildId homeDir
  headerOk <- installedHeaderOk homeDir
  runtime <-
    if not headerOk
      then return (Left (homeDir </> "include" </> "morloc.h" <> " is not the header this compiler was built with"))
      else do
        present <- runtimeInstalled homeDir
        if present
          then nexusAgrees homeDir
          else do
            missing <- filterM (fmap not . doesFileExist)
              [ homeDir </> "lib" </> P.sharedLibName P.hostPlatform "morloc"
              , homeDir </> "bin" </> "morloc-nexus"
              , buildIdFile homeDir
              ]
            return (Left ("the runtime is not installed: missing " <> DL.intercalate ", " missing))
  states <- forM chosen $ \ls -> do
    probe <- resolveManagedIdentity ls
    st <- bindingState homeDir sanitize mBuildId ls (Just probe)
    return (DF.lsLang ls, st)
  rust <- if wantRust then (\st -> [("rust", st)]) <$> rustBindingState homeDir else return []
  return (bindingsReportJson runtime (states ++ rust))

bindingsReportJson :: Either String () -> [(T.Text, BindingState)] -> T.Text
bindingsReportJson runtime states =
  jsonObj
    [ ("bindings_check", jsonInt 1)
    , ("runtime", jsonObj (("ok", jsonBool (either (const False) (const True) runtime)) : [("reason", jsonStr (T.pack r)) | Left r <- [runtime]]))
    , ("languages", jsonArr [jsonObj [("lang", jsonStr l), ("state", jsonStr (bindingStateName st)), ("reason", jsonStr (T.pack (bindingStateReason st)))] | (l, st) <- states])
    ]
  where
    bindingStateName BindingOk = "ok"
    bindingStateName BindingAbsent = "absent"
    bindingStateName (BindingStale _) = "stale"
    bindingStateName (BindingUnavailable _) = "unavailable"
    bindingStateName (BindingOutsidePrefix _) = "outside-prefix"
    bindingStateReason (BindingStale r) = r
    bindingStateReason (BindingUnavailable r) = r
    bindingStateReason (BindingOutsidePrefix r) = r
    bindingStateReason _ = ""

-- | Build and install libmorloc, morloc-nexus and morloc.h from the runtime
-- source with the active toolchain (through the runtime's own install.sh),
-- then persist the source.
-- 'configureAllSteps' calls this when the runtime is missing or does not hold
-- together ('nexusAgrees'), or a rebuild is forced; an on-demand language add
-- over an intact runtime skips it.
provisionRustRuntime :: Bool -> Config -> FilePath -> IO ()
provisionRustRuntime verbose config homeDir = do
  -- Resolve the Rust workspace source + cargo up front. Search order:
  -- MORLOC_RUNTIME_DIR (set by mim / dev); the persisted copy at
  -- $MORLOC_HOME/runtime; then next to the morloc binary.
  runtimeDirEnv <- lookupEnv "MORLOC_RUNTIME_DIR"
  runtimeDir <- case runtimeDirEnv of
    Just d -> return d
    Nothing -> do
      morlocBin <- findExecutable "morloc"
          -- The persisted copy is kept AHEAD of the binary-relative candidates
          -- so a custom $MORLOC_HOME is not shadowed by a stale default-home
          -- ($HOME/.local/share/morloc/runtime) workspace. Staleness is surfaced by
          -- a warning below (set MORLOC_RUNTIME_DIR to rebuild from fresh source).
      let homeCandidate = [homeDir </> "runtime"]
          binCandidates = case morlocBin of
            Just binPath ->
              [ takeDirectory (takeDirectory binPath) </> "share" </> "morloc" </> "runtime"
              , takeDirectory (takeDirectory binPath) </> "runtime"
              ]
            Nothing -> []
      findRuntimeDir (homeCandidate ++ binCandidates)
  hasCargo <- findExecutable "cargo"
  -- Build libmorloc.so + morloc-nexus from source (needs cargo + the Rust
  -- workspace). mim is NOT built here: it is the downloaded static
  -- bootstrap binary, ABI-isolated from the runtime. This guard aborts init
  -- unless the source + cargo are present, so the Rust steps below all run.
  when (null runtimeDir || hasCargo == Nothing) $
    ioError . userError $ unlines
      [ "morloc init builds the runtime (libmorloc.so + morloc-nexus) from"
      , "source and requires cargo plus the Rust workspace."
      , ""
      , "Provision via mim, or for development set MORLOC_RUNTIME_DIR"
      , "to the runtime/ directory and put cargo on PATH:"
      , "  export MORLOC_RUNTIME_DIR=/path/to/morloc/runtime"
      , "  morloc init -f"
      ]

  -- Everything init builds, installs and persists comes from one snapshot of
  -- the source, taken here: libmorloc, the nexus, the installed morloc.h, and
  -- the copy at $MORLOC_HOME/runtime that Rust pools later build against. The
  -- resolved source may change while init runs; the snapshot does not.
  let persistedRuntimeDir = homeDir </> "runtime"
      snapshot = homeDir </> "runtime.staging"
  srcAbs <- canonicalizePath runtimeDir
  persistedExists <- doesDirectoryExist persistedRuntimeDir
  persistedIsLink <- if persistedExists then pathIsSymbolicLink persistedRuntimeDir else return False
  persistedAbs <- if persistedExists then canonicalizePath persistedRuntimeDir else return persistedRuntimeDir
  -- The source itself (no fresher workspace found, or a read-only image
  -- layer provisioned there) and a link to a development tree are left as
  -- they are.
  let replacePersisted = srcAbs /= persistedAbs && not persistedIsLink
  -- Checked here too, so a refusal names the source rather than the snapshot.
  runtimeInstallScript srcAbs >>= either (ioError . userError) (const (return ()))
  when (srcAbs == persistedAbs) $
    sayWarning $
      "Using the persisted runtime source at $MORLOC_HOME/runtime (no fresher "
        <> "workspace found). If you changed the compiler's runtime/, set "
        <> "MORLOC_RUNTIME_DIR to it and re-run `morloc init -f`."
  removePathIfExists snapshot
  flip onException (removePathIfExists snapshot) $ do
    sayInfo verbose $ "Snapshotting the runtime source " <> srcAbs <> " to " <> snapshot
    copyTree verbose srcAbs snapshot
    installFromSnapshot snapshot
  if replacePersisted
    then do
      sayInfo verbose "Persisting the runtime source to $MORLOC_HOME/runtime"
      let previous = homeDir </> "runtime.old"
      removePathIfExists previous
      when persistedExists $ renameDirectory persistedRuntimeDir previous
      renameDirectory snapshot persistedRuntimeDir
      removePathIfExists previous
    else removePathIfExists snapshot

  where
    installFromSnapshot snapshot = do
      script <- runtimeInstallScript snapshot >>= either (ioError . userError) return
      -- Build into a WRITABLE target dir (the shared rust-build cache under
      -- MORLOC_STATE), NOT the default `<source>/target`: in a container the
      -- Rust source is a read-only image layer, so cargo cannot write a
      -- target beside it.
      sayInfo verbose "Building and installing the runtime (libmorloc, morloc-nexus, morloc.h)"
      run verbose "env" (["MORLOC_INIT_VERBOSE=1" | verbose] ++ ["bash", script, homeDir, Config.rustBuildDir config])
        `catch` \(e :: IOError) -> ioError . userError $
          "building the runtime from " <> snapshot <> " failed (see the output above): " <> ioeGetErrorString e

-- | SHIM-6: write this compiler's rustmorloc into the home, beside the
-- other bindings, in a directory named for its contents; a complete one is
-- left as it is, and other compilers' copies are not touched. True when it
-- was written now.
installRustBinding :: Bool -> Bool -> FilePath -> IO Bool
installRustBinding verbose rewrite homeDir = do
  let dir = rustBindingDir homeDir
      parent = takeDirectory dir
      stamp = dir </> ".complete"
  done <- (&& not rewrite) <$> doesFileExist stamp
  unless done $ do
    sayInfo verbose ("Installing the Rust binding (rustmorloc) at " <> dir)
    pid <- getProcessID
    let staging = parent </> ("." <> takeFileName dir <> ".staging." <> show pid)
    createDirectoryIfMissing True parent
    removePathIfExists staging
    forM_ rustBindingFiles $ \(name, bytes) -> do
      createDirectoryIfMissing True (takeDirectory (staging </> name))
      BS.writeFile (staging </> name) bytes
    BS.writeFile (staging </> ".complete") BS.empty
    removePathIfExists dir
    renameDirectory staging dir
  return (not done)

-- | Build the installed rustmorloc into the shared pool target dir with the
-- default pool release profile, so that a first Rust pool build finds its
-- dependencies (Arrow and the rest) built and compiles only rustmorloc and
-- itself.
warmRustBinding :: Bool -> Config -> FilePath -> IO ()
warmRustBinding verbose config homeDir = do
  sayInfo verbose "Warming the Rust pool build cache (rustmorloc's dependencies)"
  run verbose "env" $
    [ "MORLOC_HOME=" <> homeDir, "RUSTC_BOOTSTRAP=1", "cargo", "-Zprofile-rustflags"
    , "build", "--release", "--locked"
    , "--manifest-path", rustBindingDir homeDir </> "Cargo.toml"
    , "--target-dir", Config.rustBuildDir config
    ]
      ++ concatMap (\c -> ["--config", c]) (rustPoolProfileConfig "2" "\"thin\"")

-- | The pool release profile (Members/Rust.hs makeCargoDocs) as cargo
-- @--config@ settings, for an opt-level and lto given as TOML literals.
rustPoolProfileConfig :: String -> String -> [String]
rustPoolProfileConfig optLevel lto =
  [ "profile.release.opt-level=" <> optLevel
  , "profile.release.lto=" <> lto
  , "profile.release.panic=\"unwind\""
  , "profile.release.debug=\"line-tables-only\""
  , "profile.release.rustflags=" <> show rustPoolRustflags
  ]
    ++ ["profile.release.split-debuginfo=\"packed\"" | SI.os == "darwin"]

-- | PANIC-9: the rustc flags a Rust pool crate and its dependencies build
-- with. The panic hook tells a user panic from a runtime one by the debug
-- information of the code that raised it, and code merged with an identical
-- function, by LLVM or by the macOS linker, carries the other function's.
-- The rustc flag is unstable; RUSTC_BOOTSTRAP enables it on a stable
-- toolchain.
rustPoolRustflags :: [String]
rustPoolRustflags = "-Zmerge-functions=disabled" : ["-Clink-arg=-Wl,-no_deduplicate" | SI.os == "darwin"]

-- | The first candidate runtime source that 'runtimeInstallScript' accepts;
-- else the first with a Cargo.toml, which it then refuses by name; else "".
findRuntimeDir :: [FilePath] -> IO FilePath
findRuntimeDir ds = do
  workspaces <- filterM (\d -> doesFileExist (d </> "Cargo.toml")) ds
  usable <- filterM (fmap (either (const False) (const True)) . runtimeInstallScript) workspaces
  return $ case usable ++ workspaces of
    d : _ -> d
    [] -> ""

-- ANSI color wrapping, disabled when stderr is not a terminal
withColor :: String -> String -> IO String
withColor code msg = do
  isTty <- hIsTerminalDevice stderr
  return $ if isTty then code <> msg <> "\ESC[0m" else msg

sayInfo :: Bool -> String -> IO ()
sayInfo verbose message = when verbose $ do
  line <- withColor "\ESC[34m" ("[INFO] " <> message)
  hPutStrLn stderr line

sayWarning :: String -> IO ()
sayWarning message = do
  line <- withColor "\ESC[33m" ("[WARNING] " <> message)
  hPutStrLn stderr line

sayError :: String -> IO ()
sayError message = do
  line <- withColor "\ESC[31m" ("[ERROR] " <> message)
  hPutStrLn stderr line

-- | Run an optional configuration step: on an I/O failure (a failed `run`,
-- mkdir, symlink, ...), warn with the given prefix and continue rather than
-- aborting the whole `morloc init`. Catches only synchronous IOErrors, so
-- asynchronous exceptions -- notably a user Ctrl-C during a long `cargo build`
-- -- still propagate and abort. Core install steps must NOT use this -- only
-- capabilities that may legitimately be unavailable (a language toolchain, a
-- convenience symlink).
optionalStep :: String -> IO () -> IO ()
optionalStep prefix act =
  act `catch` \(e :: IOError) -> sayWarning (prefix <> displayException e)

run :: Bool -> String -> [String] -> IO ()
run verbose cmd args = do
  when verbose $ do
    line <- withColor "\ESC[2m" (cmd <> " " <> unwords args)
    hPutStrLn stderr line
  let cp = (proc cmd args) { std_out = UseHandle stderr }
  (_, _, _, ph) <- createProcess cp
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> ioError . userError $ cmd <> " exited with code " <> show code

-- | Copy a source tree to a new directory, leaving out build output and
-- version control (any @target@ or @.git@).
copyTree :: Bool -> FilePath -> FilePath -> IO ()
copyTree verbose src dest = do
  createDirectoryIfMissing True dest
  run verbose "sh"
    [ "-c"
    , "tar cf - -C \"$1\" --exclude=target --exclude=.git . | tar xf - -C \"$2\""
    , "sh", src, dest
    ]

ensureDirectory :: Bool -> String -> FilePath -> IO ()
ensureDirectory verbose description path = do
  exists <- doesDirectoryExist path
  if exists
    then sayInfo verbose $ description ++ " ... " ++ path
    else do
      createDirectoryIfMissing True path
      sayInfo verbose $ description ++ " ... created " ++ path

-- | Remove a file, ignoring errors if it doesn't exist
removeFileSafe :: FilePath -> IO ()
removeFileSafe path = do
  result <- try (removeFile path) :: IO (Either SomeException ())
  case result of
    Left _ -> return ()
    Right _ -> return ()

-- | Remove all contents of a directory (but keep the directory itself)
cleanDirectory :: FilePath -> IO ()
cleanDirectory dir = do
  exists <- doesDirectoryExist dir
  when exists $ do
    removeDirectoryRecursive dir
    createDirectoryIfMissing True dir

-- | Remove a path that may be a file, directory, or symlink. No-op if absent.
removePathIfExists :: FilePath -> IO ()
removePathIfExists path = do
  isFile <- doesFileExist path
  if isFile
    then removeFile path
    else do
      isDir <- doesDirectoryExist path
      when isDir $ removeDirectoryRecursive path

-- | Names (relative to $MORLOC_HOME/lib/) written by morloc init itself.
-- Must stay in sync with the per-language init.sh scripts under
-- binders/*/init.sh and with runtime/install.sh (libmorloc). Anything
-- not in this list (notably lib/python/<modName>/ and lib/R/<modName>/
-- populated by `morloc install` via Module.runExpose) is preserved across
-- `morloc init -f`.
initOwnedLibPaths :: [FilePath]
initOwnedLibPaths =
  [ P.sharedLibName P.hostPlatform "morloc"  -- libmorloc.so | libmorloc.dylib
  , "libcppmorloc.a"
  , "librmorloc.so"  -- R loadable modules keep the .so name even on macOS
  ]

-- | Names (relative to $MORLOC_HOME/include/) written by morloc init itself.
-- Must stay in sync with binders/cpp/init.sh and runtime/install.sh
-- (morloc.h). `mlccpptypes/` is omitted
-- because cpp/init.sh git-clones it on demand and reuses it if present.
-- Anything not in this list (notably include/<modName>/ populated by
-- `morloc install`) is preserved across `morloc init -f`.
initOwnedIncludePaths :: [FilePath]
initOwnedIncludePaths =
  [ "morloc.h"
  , "cppmorloc.hpp"
  , "cppmorloc_api.hpp"
  , "mlc_arrow.hpp"
  , "mlc_rec.hpp"
  , "morloc_pch.hpp"
  , "morloc_pch.hpp.gch"
  , "nanoarrow"
  ]

-- | The conventional environment variable that overrides a build tool's name.
-- A conda toolchain (e.g. a pixi env) exports these pointing at target-prefixed
-- tools such as @x86_64-conda-linux-gnu-g++@ / @-ar@ rather than plain
-- @gcc@/@g++@/@ar@. Tools not listed are used as given.
toolEnvVar :: String -> Maybe String
toolEnvVar "gcc"   = Just "CC"
toolEnvVar "g++"   = Just "CXX"
toolEnvVar "ar"    = Just "AR"
toolEnvVar "strip" = Just "STRIP"
toolEnvVar _       = Nothing

-- | Resolve a build tool name: the environment override if set, else the tool
-- name as given (whether or not it is on PATH).
resolveToolName :: String -> IO String
resolveToolName tool = do
  mEnv <- maybe (return Nothing) lookupEnv (toolEnvVar tool)
  return (fromMaybe tool mEnv)

-- | Ask the C compiler for the absolute path of a companion tool
-- (e.g. @cc -print-prog-name=ar@). Returns Nothing when the compiler cannot
-- resolve it (it echoes the bare name back, which has no path separator).
compilerToolPath :: String -> String -> IO (Maybe FilePath)
compilerToolPath cc tool = do
  result <- try (readProcessWithExitCode cc ["-print-prog-name=" <> tool] "")
              :: IO (Either SomeException (ExitCode, String, String))
  return $ case result of
    Right (ExitSuccess, out, _) ->
      let path = reverse (dropWhile (`elem` (" \t\r\n" :: String)) (reverse out))
      in if '/' `elem` path then Just path else Nothing
    _ -> Nothing

-- | Export $AR and $STRIP for the cargo/cc-rs builds and the shim init scripts
-- when unset, derived from the resolved C compiler. Idempotent: an already-set
-- value (e.g. from a conda activation) is left untouched. conda ships these
-- tools only under a target-prefixed name, so a bare @ar@/@strip@ fallback
-- fails inside a conda env; asking the compiler resolves the prefixed tool.
exportArchiveTools :: IO ()
exportArchiveTools = do
  cc <- resolveToolName "gcc"
  mapM_ (ensure cc) [("AR", "ar"), ("STRIP", "strip")]
  where
    ensure cc (var, tool) = do
      existing <- lookupEnv var
      case existing of
        Just v | not (null v) -> return ()
        _ -> do
          mp <- compilerToolPath cc tool
          maybe (return ()) (setEnv var) mp

-- | Native-backend admission control. When strict mode is requested
-- (@MORLOC_STRICT_CONDA@, set by the native backend and by the native CI),
-- every ABI-bearing build tool that resolves MUST live under @$CONDA_PREFIX@.
-- A tool resolving outside it (e.g. a host @\/usr\/bin\/gcc@ while python is
-- conda's) silently mixes ABIs: artifacts built by different-world toolchains
-- share one address space at pool-load time and can crash. Fail loud at init
-- instead of at a user segfault.
--
-- The guard is OPT-IN, not "any @CONDA_PREFIX@": many developers keep a conda
-- base environment activated while building morloc with the host toolchain, and
-- that must not become a hard error. Absent tools are ignored (their language is
-- simply skipped by the setup loop); only found-but-outside tools are errors.
--
-- The tool's DIRECTORY (canonicalized) is compared, not the file: pixi may link
-- @$CONDA_PREFIX\/bin\/<tool>@ into a shared package store, so canonicalizing the
-- file would follow that link out of the prefix and false-positive; @bin\/@
-- itself is a real directory under the prefix.
checkCondaCoherence :: Bool -> Maybe FilePath -> IO ()
checkCondaCoherence verbose mPrefix =
  case mPrefix of
    Nothing -> return ()
    Just prefixAbs -> do
      -- ABI-bearing tools only (compilers, interpreters, cargo); git/make are
      -- not ABI-bearing and may legitimately come from the host.
      checks <- mapM (locateTool prefixAbs) abiBearingTools
      let incoherent = [x | Just x <- checks]
          shadowed = [(t, p) | (t, p, Shadowed) <- incoherent]
          unactivated = [(t, p) | (t, p, Unactivated) <- incoherent]
      if null incoherent
        then sayInfo verbose $ "Toolchain coherent with CONDA_PREFIX=" <> prefixAbs
        else
          ioError . userError . unlines $
            [ "Toolchain is incoherent with the active conda environment (CONDA_PREFIX="
                <> prefixAbs
                <> ")."
            ]
              ++ shadowedReport shadowed
              ++ unactivatedReport unactivated
  where
    shadowedReport [] = []
    shadowedReport ts =
      [ "These build tools resolve to a HOST copy that shadows the conda"
      , "environment's, which mixes ABIs and can crash at pool-load time:"
      ]
        ++ ["  " <> t <> " -> " <> p | (t, p) <- ts]
        ++ [ "Ensure the conda environment's tool comes first on PATH"
           , "(re-activate), or use a container backend."
           ]

    -- The env has no compiler of its own AND exported no $CC/$CXX, so the only
    -- C compiler in sight is the host's. Nothing is shadowing anything and no
    -- amount of PATH reordering helps: the environment's compiler activation
    -- never ran (or failed), which is what the user has to be told.
    unactivatedReport [] = []
    unactivatedReport ts =
      [ "The conda environment activated no C/C++ compiler: it provides no copy of"
      , "these tools and exported no $CC/$CXX, so they resolve to a HOST compiler,"
      , "which mixes ABIs and can crash at pool-load time:"
      ]
        ++ ["  " <> t <> " -> " <> p | (t, p) <- ts]
        ++ [ "conda ships its compilers under target-prefixed names"
           , "(x86_64-conda-linux-gnu-gcc, arm64-apple-darwin20.0.0-clang) and names"
           , "them in $CC/$CXX from the environment's activate.d scripts. Unset means"
           , "that activation did not run or failed; re-provision the environment and"
           , "read the output of its activation scripts."
           ]

    -- Just (tool, hostPath, why) if the tool is INCOHERENT; Nothing if it is
    -- coherent (resolves inside the env) or simply not part of this env. A tool
    -- resolving OUTSIDE the prefix is only a problem when the env actually
    -- provides it and a host copy is shadowing it -- NOT when the env never
    -- included that language (e.g. a stray host /usr/bin/R in an env with no
    -- R). The C/C++ compiler (gcc/g++, via $CC/$CXX) is the exception: it is
    -- always required, so a host compiler is always a fatal mix.
    locateTool :: FilePath -> String -> IO (Maybe (String, FilePath, Incoherence))
    locateTool prefixAbs tool = do
      loc <- resolveToolLocation tool
      case loc of
        Nothing -> return Nothing
        Just (path, dirAbs)
          | pathIsWithin prefixAbs dirAbs -> return Nothing
          | otherwise -> do
              inEnv <- doesFileExist (prefixAbs </> "bin" </> tool)
              if tool `elem` ["gcc", "g++"]
                then do
                  named <- (/= tool) <$> resolveToolName tool
                  return (Just (tool, path, compilerIncoherence inEnv named))
                else return (if inEnv then Just (tool, path, Shadowed) else Nothing)

-- | Why an ABI-bearing tool resolved outside the active conda prefix.
data Incoherence
  = Shadowed    -- ^ the env provides this tool, but a host copy comes first
  | Unactivated -- ^ the env provides no such tool and exported no $CC/$CXX
  deriving (Eq, Show)

-- | Classify a C/C++ compiler that resolved OUTSIDE the conda prefix, given
-- whether the prefix holds a @bin/gcc@ (@inEnv@) and whether @$CC@/@$CXX@ named
-- a compiler at all (@named@). A named compiler resolving outside the env is a
-- foreign compiler, and an env copy losing the PATH race is shadowed: both are
-- fixable by pointing at the env's tool. Neither holding means the env simply
-- has no C compiler to point at -- conda ships compilers under target-prefixed
-- names and reaches them only through @$CC@/@$CXX@, so an unset pair with no
-- @bin/gcc@ means the toolchain activation never ran.
compilerIncoherence :: Bool -> Bool -> Incoherence
compilerIncoherence inEnv named
  | inEnv || named = Shadowed
  | otherwise = Unactivated

-- | Is @child@ the same as, or nested under, @parent@? Both are expected to be
-- absolute, normalized paths. The trailing-separator comparison avoids matching
-- a sibling whose name merely shares a prefix (@\/opt\/env@ vs @\/opt\/env2@).
pathIsWithin :: FilePath -> FilePath -> Bool
pathIsWithin parent child = (parent <> "/") `DL.isPrefixOf` (child <> "/")

-- | The active strict-mode conda prefix (canonicalized), or Nothing when strict
-- mode (@MORLOC_STRICT_CONDA@) is off. Strict mode is opt-in -- set by the native
-- backend and the native CI -- so ordinary system-toolchain installs are
-- unaffected. Errors if strict mode is requested without an active @CONDA_PREFIX@.
strictCondaPrefix :: IO (Maybe FilePath)
strictCondaPrefix = strictMode >>= either (ioError . userError) return

-- | 'strictCondaPrefix', with the error as a value.
strictMode :: IO (Either String (Maybe FilePath))
strictMode = do
  enabled <- envFlag "MORLOC_STRICT_CONDA"
  if not enabled
    then return (Right Nothing)
    else do
      mPrefix <- lookupEnv "CONDA_PREFIX"
      case mPrefix of
        Just prefix | not (all isSpace prefix) -> Right . Just <$> canonicalizePath prefix
        _ ->
          return . Left $
            "MORLOC_STRICT_CONDA is set but CONDA_PREFIX is not: strict mode "
              <> "requires an active conda/pixi environment."

-- | The prefix P of a managed build (PKG-13): Nothing when @MORLOC_ENV@ is
-- unset or empty, else the canonical @CONDA_PREFIX@. A managed build whose
-- prefix is unset, not a directory, or holds a libmorloc (which a run path into
-- it would load in place of the one the nexus chooses) is an error.
managedPrefix :: IO (Either String (Maybe FilePath))
managedPrefix = do
  mEnv <- lookupEnv "MORLOC_ENV"
  case mEnv of
    Just e | not (all isSpace e) -> do
      mPrefix <- lookupEnv "CONDA_PREFIX"
      case mPrefix of
        Just prefix | not (all isSpace prefix) -> do
          isDir <- doesDirectoryExist prefix
          if not isDir
            then return . Left $ "MORLOC_ENV is set but CONDA_PREFIX (" <> prefix <> ") is not a directory, so the environment's packages cannot be found"
            else do
              p <- canonicalizePath prefix
              let lib = p <> "/lib"
              hasLib <- doesDirectoryExist lib
              own <- if hasLib then filter ("libmorloc" `DL.isPrefixOf`) <$> listDirectory lib else return []
              return $ case own of
                _ | ':' `elem` p -> Left ("the environment prefix " <> p <> " holds ':', so its bin directory cannot be put on PATH")
                [] -> Right (Just p)
                f : _ -> Left ("the environment prefix " <> p <> " holds a libmorloc (" <> lib <> "/" <> f <> "), which its run path would load in place of morloc's own")
        _ -> return . Left $ "MORLOC_ENV is set but CONDA_PREFIX is unset or empty, so the environment's packages cannot be found"
    _ -> return (Right Nothing)

-- | The flags a native C or C++ build in prefix P adds (PKG-13): P's headers
-- after morloc's, P's libraries, and P/lib as a run path. On Linux the run
-- path is DT_RPATH, which a caller's LD_LIBRARY_PATH does not override.
prefixFlags :: P.Platform -> FilePath -> [T.Text]
prefixFlags platform p =
  [ "-isystem", q (p <> "/include")
  , "-L", q (p <> "/lib")
  , "-Xlinker", "-rpath", "-Xlinker", q (p <> "/lib")
  ]
    ++ case platform of
      P.Linux -> ["-Xlinker", "--disable-new-dtags"]
      P.Darwin -> []
  where
    q = T.pack . MS.shellQuote

-- | The linker arguments a Rust pool built in prefix P adds (PKG-13), one
-- argument each, unquoted: cargo passes each to the linker as is.
prefixRustLinkArgs :: P.Platform -> FilePath -> [T.Text]
prefixRustLinkArgs platform p =
  ["-Xlinker", "-rpath", "-Xlinker", T.pack (p <> "/lib")]
    ++ case platform of
      P.Linux -> ["-Xlinker", "--disable-new-dtags"]
      P.Darwin -> []

-- | Is an on/off environment variable on? Unset, empty, @0@, @false@, @no@ and
-- @off@, in any case, are off.
envFlag :: String -> IO Bool
envFlag k = maybe False ((`notElem` ["", "0", "false", "no", "off"]) . map toLower) <$> lookupEnv k

-- | A program a pool build runs, as the build resolves it: the variable that
-- names it (Nothing for a default name looked up on PATH), that value, and the
-- executable found.
data BuildTool = BuildTool
  { btRole :: String
  , btLinker :: Bool
  , btVar :: Maybe String
  , btValue :: String
  , btFound :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | What the check needs to know about the host.
data ToolchainHost = ToolchainHost
  { thSystemLinker :: Maybe FilePath
  -- ^ a host program rustc may link with: on macOS @/usr/bin/cc@, which
  -- conda's activation never replaces and which links Rust and libSystem only
  , thTargetTriple :: FilePath -> IO (Maybe String)
  -- ^ the host triple reported by the rustc at this path, which is the
  -- prefix's
  }

-- | In strict mode, the error for a pool build that would run a program from
-- outside @$CONDA_PREFIX@, or that names one that does not exist; Nothing when
-- strict mode is off or every program is the environment's. This is the
-- make-time half of 'checkCondaCoherence': it sees the environment cargo and
-- the C++ compiler actually run in, which @morloc init@ never does.
strictToolchainError :: PoolToolchain -> IO (Maybe String)
strictToolchainError kind = do
  mode <- strictMode
  case mode of
    Left err -> return (Just err)
    Right Nothing -> return Nothing
    Right (Just prefixAbs) -> do
      vars <- getEnvironment
      bad <- toolsOutsidePrefix host prefixAbs vars kind
      return $ if null bad then Nothing else Just (report prefixAbs bad)
  where
    host =
      ToolchainHost
        { thSystemLinker = if SI.os == "darwin" then Just "/usr/bin/cc" else Nothing
        , thTargetTriple = rustcHost
        }
    rustcHost rustc = do
      result <- try (readProcessWithExitCode rustc ["-vV"] "")
                  :: IO (Either SomeException (ExitCode, String, String))
      return $ case result of
        Right (ExitSuccess, out, _) ->
          listToMaybe [h | l <- lines out, Just h <- [DL.stripPrefix "host: " l]]
        _ -> Nothing
    report prefixAbs bad =
      unlines $
        [ "This build would run programs that are missing, broken, or not the conda"
        , "environment's (CONDA_PREFIX=" <> prefixAbs <> "):"
        ]
          ++ [ "  " <> btRole t <> ": " <> maybe "" (<> "=") (btVar t) <> btValue t
                 <> maybe " (not found)" (" -> " <>) (btFound t)
             | t <- bad
             ]
          ++ [ "In a mim environment, run `mim update` to record its toolchain again, and"
             , "check that your shell startup files put no other compiler or Rust ahead"
             , "of the environment on PATH."
             ]

-- | How a build reads a program from a variable's value.
data Split
  = Whole  -- ^ the value is one program (cargo's rustc and linker)
  | CcRs   -- ^ the whole value if it is a file, else its words, behind a compiler cache
  | Shell  -- ^ its words, as an unquoted shell expansion splits them

-- | The programs a pool build would run that are missing or lie outside the
-- (canonical) prefix, resolved against @vars@ (its @PATH@ included) the way the
-- build resolves them. A C++ pool runs @${CXX:-g++}@ and a C guest compile
-- @${CC:-cc}@. A Rust pool runs cargo and rustc, compiles C and C++ through
-- cc-rs (@CC@, @CXX@ and @AR@, each in the form cc-rs reads for a native
-- build) and links through cargo's linker. The cc-rs and linker forms are
-- named by the target triple, which comes from rustc once it is known to be
-- the prefix's; a prefix rustc that reports none is itself reported.
toolsOutsidePrefix :: ToolchainHost -> FilePath -> [(String, String)] -> PoolToolchain -> IO [BuildTool]
toolsOutsidePrefix host prefixAbs vars kind = do
  (tools, broken) <- case kind of
    CppPoolToolchain -> (\t -> ([t], [])) <$> fromVars "C++ compiler" Shell ["CXX"] "g++"
    CGuestToolchain -> (\t -> ([t], [])) <$> fromVars "C compiler" Shell ["CC"] "cc"
    RustPoolToolchain -> do
      cargo <- fromVars "cargo" Whole [] "cargo"
      rustc <- fromVars "rustc" Whole ["RUSTC", "CARGO_BUILD_RUSTC"] "rustc"
      rustcInPrefix <- not <$> isOutside rustc
      triple <-
        if rustcInPrefix
          then maybe (return Nothing) (thTargetTriple host) (btFound rustc)
          else return Nothing
      let ccRsVars base = maybe [] (\t -> [base <> "_" <> t, base <> "_" <> map underscore t]) triple ++ ["HOST_" <> base, base]
      cc <- mapM (\(role, base, def) -> fromVars role CcRs (ccRsVars base) def)
        [("C compiler for cc-rs", "CC", "cc"), ("C++ compiler for cc-rs", "CXX", "c++"), ("archiver for cc-rs", "AR", "ar")]
      -- With no triple the linker variable is unknown: rustc itself is
      -- reported, as a foreign rustc or as one that names no host.
      case triple of
        Just t -> do
          linker <- fromVars "linker" Whole [linkerVar t] "cc"
          return ([cargo, rustc] ++ cc ++ [linker {btLinker = True}], [])
        Nothing
          | rustcInPrefix -> return ([cargo] ++ cc, [rustc {btRole = "rustc, which reports no host triple"}])
          | otherwise -> return ([cargo, rustc] ++ cc, [])
  bad <- filterM isOutside tools
  return (bad ++ broken)
  where
    var k = lookup k vars
    pathDirs = maybe [] splitSearchPath (var "PATH")

    -- The first variable set names the program; with none, or with a value the
    -- build reads as none, the default name. cc-rs takes the first variable
    -- set, even a blank one, and falls back when it trims to nothing; the
    -- shell's @${X:-def}@ falls back only on an empty value; cargo is taken to
    -- run any value set, so an empty one names nothing.
    fromVars role how ks def =
      case [(k, v) | k <- ks, Just v <- [var k], keeps how v] of
        ((k, v) : _) | not (fallsBack how v) -> do
          found <- locate how v
          spaced <- case how of
            Shell | length (shellWords v) > 1 -> isExecutableFile v
            _ -> return False
          let role' = if spaced then role <> " (the build's shell splits its path at the space)" else role
          return (BuildTool role' False (Just k) v found)
        _ -> BuildTool role False Nothing def <$> locate Whole def

    keeps Shell v = not (null v)
    keeps _ _ = True
    fallsBack CcRs v = all isSpace v
    fallsBack _ _ = False

    underscore c = if c == '-' then '_' else c
    linkerVar t = "CARGO_TARGET_" <> map (toUpper . underscore . dot) t <> "_LINKER"
    dot c = if c == '.' then '_' else c

    locate how value = case how of
      Whole -> program value
      Shell -> firstWord (shellWords value)
      CcRs -> do
        let trimmed = DL.dropWhileEnd isSpace (dropWhile isSpace value)
        whole <- if '/' `elem` trimmed then isExecutableFile trimmed else return False
        if whole then return (Just trimmed) else firstWord (words trimmed)

    -- The default IFS: space, tab and newline.
    shellWords str = case dropWhile ifs str of
      "" -> []
      rest -> let (w, more) = break ifs rest in w : shellWords more
    ifs c = c `elem` (" \t\n" :: String)

    -- A compiler cache in front of the compiler is not the compiler.
    firstWord (w : prog : _) | takeFileName w `elem` compilerCaches = program prog
    firstWord (prog : _) = program prog
    firstWord [] = return Nothing

    program p
      | '/' `elem` p = (\ok -> if ok then Just p else Nothing) <$> isExecutableFile p
      | otherwise = listToMaybe <$> findExecutablesInDirectories pathDirs p

    -- The directory is canonicalized, not the file: see 'checkCondaCoherence'.
    isOutside t = case btFound t of
      Nothing -> return True
      Just p -> do
        systemLinker <- case thSystemLinker host of
          Just sys | btLinker t -> (==) <$> canonicalizePath p <*> canonicalizePath sys
          _ -> return False
        if systemLinker
          then return False
          else not . pathIsWithin prefixAbs <$> canonicalizePath (takeDirectory p)

    -- Wrappers that run the named compiler locally. Remote compilers (distcc,
    -- icecc) are judged as themselves.
    compilerCaches = ["ccache", "sccache", "cachepot", "buildcache"]

isExecutableFile :: FilePath -> IO Bool
isExecutableFile p = do
  isFile <- doesFileExist p
  if isFile then executable <$> getPermissions p else return False

-- | ABI-bearing tools: compilers and language interpreters whose provenance
-- determines the ABI of what they build/run. In strict mode these MUST come from
-- the conda env. Auxiliary tools (git, make, pkg-config) are not ABI-bearing and
-- may come from the host even in strict mode.
abiBearingTools :: [String]
abiBearingTools = ["gcc", "g++", "python3", "R", "cargo"]

-- | Is a language's tool available for THIS environment? The tool must resolve
-- (findExecutable, honoring $CC/$CXX for gcc/g++). In strict mode an ABI-bearing
-- tool must also resolve INSIDE the conda prefix: a host compiler/interpreter
-- means the language is not part of the env, so its shim must not be built (that
-- would mix a host toolchain with conda's libmorloc). Non-ABI auxiliary tools
-- (e.g. git for cloning C++ headers) may be host-provided. In non-strict mode any
-- tool on PATH counts.
toolAvailableInEnv :: Maybe FilePath -> String -> IO Bool
toolAvailableInEnv mPrefix tool = do
  loc <- resolveToolLocation tool
  case loc of
    Nothing -> return False
    Just (_, dirAbs) -> case mPrefix of
      Nothing -> return True
      Just prefixAbs
        | tool `notElem` abiBearingTools -> return True
        | otherwise -> return (pathIsWithin prefixAbs dirAbs)

-- | Resolve a tool (honoring $CC/$CXX for gcc/g++), locate it on PATH, and
-- canonicalize its directory. Nothing if the tool is absent. Shared by
-- 'requireTool', the coherence guard, and env-aware language selection so the
-- resolve-and-locate rule lives in one place.
resolveToolLocation :: String -> IO (Maybe (FilePath, FilePath))
resolveToolLocation tool = do
  resolved <- resolveToolName tool
  found <- findExecutable resolved
  traverse (\p -> (,) p <$> canonicalizePath (takeDirectory p)) found

-- | Check that a tool exists on PATH (honoring $CC/$CXX for gcc/g++), error if not
requireTool :: String -> String -> IO ()
requireTool tool msg = do
  found <- resolveToolLocation tool
  case found of
    Nothing -> ioError . userError $ tool <> " not found on PATH. " <> msg
    Just _ -> return ()

-- | Create a symlink at dst pointing to src, removing any existing file at dst.
symlinkBinary :: FilePath -> FilePath -> IO ()
symlinkBinary src dst = do
  -- Remove existing file or symlink at destination
  isLink <- pathIsSymbolicLink dst `catch` (\(_ :: SomeException) -> return False)
  when isLink $ removeFile dst
  isFile <- doesFileExist dst
  when isFile $ removeFile dst
  createFileLink src dst

