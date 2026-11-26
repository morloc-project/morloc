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
  , findRustDir
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
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Morloc.Abi (abiVersion, headerAbiVersion)

import Control.Exception (SomeException, bracket, catch, displayException, fromException, onException, try)
import System.IO.Error (ioeGetErrorString)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, executable, findExecutable, findExecutablesInDirectories, getHomeDirectory, getPermissions, listDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.FilePath (splitSearchPath, takeDirectory, takeFileName)
import System.IO (hClose, hIsTerminalDevice, hPutStrLn, stderr)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hLock, hTryLock)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, readProcessWithExitCode, waitForProcess)

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

configureAllSteps :: Bool -> OverwriteProtocol -> Bool -> Bool -> Config -> IO ()
configureAllSteps verbose force slurmSupport sanitize config = do
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
  existingBuildConfig <- Config.loadBuildConfig config
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

  -- Build libmorloc.so + morloc-nexus (and rustmorloc + the language shims)
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

  -- Build the Rust runtime (libmorloc.so + morloc-nexus + the rustmorloc pool
  -- cache) only when it is missing, or when a full rebuild is forced. An
  -- in-environment on-demand language add (see the language loop below) re-runs
  -- `morloc init` WITHOUT force purely to build a newly provisioned language's
  -- shim; the already-built runtime must not be relinked (and cargo must not be
  -- required) on that path. The manager's provision always uses force, so a
  -- compiler/version change still rebuilds the runtime.
  runtimeBuilt <- runtimeInstalled homeDir
  if force == ForceOverwrite || not runtimeBuilt || not incremental
    then provisionRustRuntime verbose config homeDir
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
  -- A per-language marker records that a shim was built successfully. In
  -- incremental mode (the on-demand agent path), an already-built language is
  -- skipped: this makes an on-demand init (run to add one newly provisioned
  -- language) build ONLY that language's shim and leave every other language's
  -- working shim untouched -- crucial because rebuilding a shim against a moved
  -- interpreter would break its ABI. A bare init (non-incremental) rebuilds every
  -- available language. The markers live under opt/, which the force-clean above
  -- wipes, so `-f` also rebuilds every available language.
  let langMarkerDir = optDir </> "lang-configured"
  createDirectoryIfMissing True langMarkerDir
  forM_ DF.langSetups $ \ls -> do
    let tools = DF.lsRequiredTools ls
        marker = langMarkerDir </> DF.lsName ls
    unavailable <- filterM (fmap not . toolAvailableInEnv mStrictPrefix) tools
    alreadyBuilt <- doesFileExist marker
    if not (null unavailable)
      then
        let reason = case mStrictPrefix of
              Just _ -> "not provided by the conda environment"
              Nothing -> "missing"
        in sayWarning $ "Skipping " <> DF.lsName ls <> " setup (" <> reason <> ": " <> unwords unavailable <> ")"
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
          let sanitizeFlagsStr = if sanitize then "-fsanitize=alignment -fno-sanitize-recover=alignment" else ""
          -- Run the setup, and only on success record the marker + clean up.
          -- Grouping the marker write with the build inside `optionalStep` keeps
          -- a non-strict failure from marking the language as built.
          let buildShim = do
                run verbose "bash" [initPath, homeDir, buildDir, sanitizeFlagsStr]
                removeFileSafe initPath
                forM_ (DF.lsFiles ls) $ \ef ->
                  removeFileSafe (buildDir </> DF.embededFileName ef)
                TIO.writeFile marker ""
          case mStrictPrefix of
            Just _ -> buildShim
            Nothing -> optionalStep (DF.lsName ls <> " setup failed: ") buildShim

  -- Generate shell completions
  sayInfo verbose "Generating shell completions"
  Completion.regenerateCompletions verbose (Config.exeDir config) (homeDir </> "completions")

-- | Whether a home holds a runtime this compiler can use as it is:
-- libmorloc, the nexus, and the header they were installed with, which must
-- be the header this compiler was built with.
runtimeInstalled :: FilePath -> IO Bool
runtimeInstalled homeDir = do
  let header = homeDir </> "include" </> "morloc.h"
  present <- and <$> mapM doesFileExist
    [ homeDir </> "lib" </> P.sharedLibName P.hostPlatform "morloc"
    , homeDir </> "bin" </> "morloc-nexus"
    , header
    ]
  if present
    then (== encodeUtf8 DF.libmorlocHeader) <$> BS.readFile header
    else return False

-- | The install script of a runtime source directory, or why it cannot be
-- used. Init installs the source's morloc.h for the bindings and pools to
-- compile against, so it must be the header this compiler was built with.
runtimeInstallScript :: FilePath -> IO (Either String FilePath)
runtimeInstallScript rustDir = do
  let script = rustDir </> "install.sh"
      header = rustDir </> "include" </> "morloc.h"
      refuse why = Left $
        "the runtime source at " <> rustDir <> " " <> why
          <> "; set MORLOC_RUST_DIR to the data/rust this compiler was built from and re-run `morloc init -f`"
  hasCargo <- doesFileExist (rustDir </> "Cargo.toml")
  hasScript <- doesFileExist script
  hasHeader <- doesFileExist header
  if not hasCargo
    then return (refuse "is not a runtime source (it has no Cargo.toml)")
    else if not hasScript
      then return (refuse "predates this compiler (it has no install.sh)")
      else if not hasHeader
        then return (refuse ("has no " <> header))
        else do
          bytes <- BS.readFile header
          return $
            if bytes == encodeUtf8 DF.libmorlocHeader
              then Right script
              else refuse $ case decodeUtf8' bytes of
                Right text | Just n <- headerAbiVersion text, n /= abiVersion ->
                  "declares ABI " <> show n <> " in include/morloc.h, but this compiler emits code for ABI "
                    <> show abiVersion
                _ -> "has an include/morloc.h that differs from the header this compiler was built with"

-- | Build and install libmorloc, morloc-nexus and morloc.h from the runtime
-- source with the active toolchain (through the runtime's own install.sh),
-- then persist the source and warm the rustmorloc pool cache.
-- 'configureAllSteps' calls this only when the runtime is missing or a rebuild is
-- forced, so an in-env, no-force, on-demand language add skips it.
provisionRustRuntime :: Bool -> Config -> FilePath -> IO ()
provisionRustRuntime verbose config homeDir = do
  -- Resolve the Rust workspace source + cargo up front. Search order:
  -- MORLOC_RUST_DIR (set by mim / dev); the persisted copy at
  -- $MORLOC_HOME/rust; then next to the morloc binary.
  rustDirEnv <- lookupEnv "MORLOC_RUST_DIR"
  rustDir <- case rustDirEnv of
    Just d -> return d
    Nothing -> do
      morlocBin <- findExecutable "morloc"
          -- The persisted copy is kept AHEAD of the binary-relative candidates
          -- so a custom $MORLOC_HOME is not shadowed by a stale default-home
          -- ($HOME/.local/share/morloc/rust) workspace. Staleness is surfaced by
          -- a warning below (set MORLOC_RUST_DIR to rebuild from fresh source).
      let homeCandidate = [homeDir </> "rust"]
          binCandidates = case morlocBin of
            Just binPath ->
              [ takeDirectory (takeDirectory binPath) </> "share" </> "morloc" </> "rust"
              , takeDirectory (takeDirectory binPath) </> "data" </> "rust"
              ]
            Nothing -> []
      findRustDir (homeCandidate ++ binCandidates)
  hasCargo <- findExecutable "cargo"
  -- Build libmorloc.so + morloc-nexus from source (needs cargo + the Rust
  -- workspace). mim is NOT built here: it is the downloaded static
  -- bootstrap binary, ABI-isolated from the runtime. This guard aborts init
  -- unless the source + cargo are present, so the Rust steps below all run.
  when (null rustDir || hasCargo == Nothing) $
    ioError . userError $ unlines
      [ "morloc init builds the runtime (libmorloc.so + morloc-nexus) from"
      , "source and requires cargo plus the Rust workspace."
      , ""
      , "Provision via mim, or for development set MORLOC_RUST_DIR"
      , "to the data/rust/ directory and put cargo on PATH:"
      , "  export MORLOC_RUST_DIR=/path/to/morloc/data/rust"
      , "  morloc init -f"
      ]

  -- Everything init builds, installs and persists comes from one snapshot of
  -- the source, taken here: libmorloc, the nexus, the installed morloc.h, and
  -- the copy at $MORLOC_HOME/rust that Rust pools later build against. The
  -- resolved source may change while init runs; the snapshot does not.
  let persistedRustDir = homeDir </> "rust"
      snapshot = homeDir </> "rust.staging"
  srcAbs <- canonicalizePath rustDir
  persistedExists <- doesDirectoryExist persistedRustDir
  persistedIsLink <- if persistedExists then pathIsSymbolicLink persistedRustDir else return False
  persistedAbs <- if persistedExists then canonicalizePath persistedRustDir else return persistedRustDir
  -- The source itself (no fresher workspace found, or a read-only image
  -- layer provisioned there) and a link to a development tree are left as
  -- they are.
  let replacePersisted = srcAbs /= persistedAbs && not persistedIsLink
  when (srcAbs == persistedAbs) $
    sayWarning $
      "Using the persisted Rust source at $MORLOC_HOME/rust (no fresher "
        <> "workspace found). If you changed the compiler's data/rust, set "
        <> "MORLOC_RUST_DIR to it and re-run `morloc init -f`."
  removePathIfExists snapshot
  flip onException (removePathIfExists snapshot) $ do
    sayInfo verbose $ "Snapshotting the runtime source " <> srcAbs <> " to " <> snapshot
    copyTree verbose srcAbs snapshot
    installFromSnapshot snapshot
  if replacePersisted
    then do
      sayInfo verbose "Persisting the runtime source to $MORLOC_HOME/rust"
      let previous = homeDir </> "rust.old"
      removePathIfExists previous
      when persistedExists $ renameDirectory persistedRustDir previous
      renameDirectory snapshot persistedRustDir
      removePathIfExists previous
    else removePathIfExists snapshot

  -- Warm the shared pool build cache. A Rust pool is a Cargo project
  -- (Members/Rust.hs) that path-depends on rustmorloc in the persisted source,
  -- compiled per pool by cargo with the local rustc; pre-building rustmorloc
  -- into the shared pool target-dir spares the first `morloc make` that
  -- compile. Warming is a Rust capability, not a core install step: a failure
  -- leaves Rust unavailable with a warning rather than aborting init, and the
  -- per-pool build at `morloc make` still enforces cargo via makeTheMaker.
  optionalStep "Rust configuration failed (Rust pools will not be available): " $ do
    let poolTargetDir = Config.rustBuildDir config
    sayInfo verbose "Warming rustmorloc build cache (Rust pool marshaller)"
    -- The warm-up builds with the pool profile so that pool builds reuse
    -- these artifacts: its rustc flags (PANIC-9) and, on macOS, its packed
    -- debug information (PANIC-12).
    run verbose "env" $
      [ "MORLOC_HOME=" <> homeDir, "RUSTC_BOOTSTRAP=1", "cargo", "-Zprofile-rustflags"
      , "build", "--release"
      , "--manifest-path", persistedRustDir </> "Cargo.toml"
      , "-p", "rustmorloc"
      , "--target-dir", poolTargetDir
      , "--config", "profile.release.rustflags=" <> show rustPoolRustflags
      ]
      ++ (if SI.os == "darwin" then ["--config", "profile.release.split-debuginfo=\"packed\""] else [])
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
findRustDir :: [FilePath] -> IO FilePath
findRustDir ds = do
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
-- data/lang/*/init.sh and with data/rust/install.sh (libmorloc). Anything
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
-- Must stay in sync with data/lang/cpp/init.sh and data/rust/install.sh
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

