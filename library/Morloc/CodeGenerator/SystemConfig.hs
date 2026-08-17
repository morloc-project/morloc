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
  , pathIsWithin
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Completion as Completion
import qualified Morloc.Config as Config
import qualified Morloc.DataFiles as DF
import Morloc.Module (OverwriteProtocol (..))

import qualified Data.List as DL
import qualified Data.Text.IO as TIO

import Control.Exception (SomeException, catch, displayException, fromException, try)
import System.IO.Error (ioeGetErrorString)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

configureAll :: Bool -> OverwriteProtocol -> Bool -> Bool -> Config -> IO Bool
configureAll verbose force slurmSupport sanitize config = do
  result <- try (configureAllSteps verbose force slurmSupport sanitize config) :: IO (Either SomeException ())
  case result of
    Left e -> do
      -- Strip the "user error (...)" wrapper from IOError messages
      let msg = case fromException e :: Maybe IOError of
            Just ioe -> ioeGetErrorString ioe
            Nothing -> displayException e
      sayError $ "Configuration failed: " ++ msg
      return False
    Right _ -> return True

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

  -- Admission control FIRST, before any destructive step: a failed coherence
  -- check must abort before the force-clean below wipes opt/ + init-owned libs,
  -- otherwise a rejected env leaves MORLOC_HOME gutted (no libmorloc, no nexus).
  checkCondaCoherence verbose mStrictPrefix

  -- When force is set, clean stale init-owned artifacts. Package-installed
  -- subtrees (see Module.exposeDirsFor) live in include/<modName>/,
  -- lib/python/<modName>/ and lib/R/<modName>/; those must not be wiped
  -- here or every `morloc init -f` would force a reinstall of every module
  -- that uses the `expose:` field. opt/ has no expose target, so it can
  -- still be fully wiped.
  when (force == ForceOverwrite) $ do
    sayInfo verbose "Force rebuild: cleaning init-owned artifacts"
    forM_ initOwnedLibPaths $ \p -> removePathIfExists (libDir </> p)
    forM_ initOwnedIncludePaths $ \p -> removePathIfExists (includeDir </> p)
    cleanDirectory optDir

  ensureDirectory verbose "morloc home directory" homeDir
  ensureDirectory verbose "morloc lib directory" libDir
  ensureDirectory verbose "morloc include directory" includeDir
  ensureDirectory verbose "morloc python lib directory" (libDir </> "python")
  ensureDirectory verbose "morloc R lib directory" (libDir </> "R")
  ensureDirectory verbose "morloc tmp directory" tmpDir
  ensureDirectory verbose "morloc opt directory" optDir
  ensureDirectory verbose "morloc module directory" srcLibrary

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
  -- Install morloc.h (the C ABI contract for language extensions and pool templates)
  sayInfo verbose "Installing morloc.h"
  TIO.writeFile (includeDir </> "morloc.h") DF.libmorlocHeader

  -- Install libmorloc.so and morloc-nexus.
  --
  -- Strategy (in priority order):
  --   1. MORLOC_RUST_BIN: directory with pre-built libmorloc.so + morloc-nexus
  --      (used for release installs with portable musl-linked binaries)
  --   2. MORLOC_RUST_DIR: Cargo workspace source - build from source via cargo
  --      (used for development and container builds)
  --   3. Auto-detect Cargo workspace relative to morloc binary
  let soPath = libDir </> "libmorloc.so"
  -- Primary install goes to $MORLOC_HOME/bin/
  let nexusBinDir = homeDir </> "bin"
      nexusBinPath = nexusBinDir </> "morloc-nexus"
  createDirectoryIfMissing True nexusBinDir
  -- Resolve the user's home for the legacy ~/.local/bin/ symlink fallback
  -- (see the MORLOC_BIN_LINK_DIR handling below).
  userHome <- getHomeDirectory

  -- Resolve the Rust workspace source + cargo up front. Needed by the
  -- from-source libmorloc build AND -- unconditionally -- by the rustmorloc
  -- build below: rustmorloc is an rlib locked to the LOCAL rustc that
  -- `morloc make` uses, so it can never come from a prebuilt bundle (unlike
  -- the C-ABI libmorloc.so) and must be compiled locally in every init path.
  -- Search order: MORLOC_RUST_DIR; the prebuilt bundle's `rust/` subdir; the
  -- persisted copy at $MORLOC_HOME/rust; then next to the morloc binary.
  rustBinEnv <- lookupEnv "MORLOC_RUST_BIN"
  rustDirEnv <- lookupEnv "MORLOC_RUST_DIR"
  rustDir <- case rustDirEnv of
    Just d -> return d
    Nothing -> do
      morlocBin <- findExecutable "morloc"
      let bundleCandidate = case rustBinEnv of Just b -> [b </> "rust"]; Nothing -> []
          -- Order: an explicit MORLOC_RUST_DIR (handled above) wins; then a
          -- prebuilt bundle; then the persisted copy at $MORLOC_HOME/rust; then
          -- locations relative to the morloc binary. The persisted copy is kept
          -- AHEAD of the binary-relative candidates so a custom $MORLOC_HOME is
          -- not shadowed by a stale default-home ($HOME/.local/share/morloc/rust)
          -- workspace. Staleness of the persisted copy is surfaced by a warning
          -- below (set MORLOC_RUST_DIR to rebuild from fresh source).
          homeCandidate = [homeDir </> "rust"]
          binCandidates = case morlocBin of
            Just binPath ->
              [ takeDirectory (takeDirectory binPath) </> "share" </> "morloc" </> "rust"
              , takeDirectory (takeDirectory binPath) </> "data" </> "rust"
              ]
            Nothing -> []
      findRustDir (bundleCandidate ++ homeCandidate ++ binCandidates)
  hasCargo <- findExecutable "cargo"
  case rustBinEnv of
    Just binDir -> do
      -- Pre-built binaries (release path)
      let prebuiltSo = binDir </> "libmorloc.so"
          prebuiltNexus = binDir </> "morloc-nexus"
          prebuiltManager = binDir </> "morloc-manager"
          managerBinPath = nexusBinDir </> "morloc-manager"
      sayInfo verbose $ "Installing pre-built libmorloc.so from " <> binDir
      run verbose "cp" [prebuiltSo, soPath]
      run verbose "cp" [prebuiltNexus, nexusBinPath]
      run verbose "chmod" ["+x", soPath]
      run verbose "chmod" ["+x", nexusBinPath]
      -- Install morloc-manager if present in pre-built binaries
      managerExists <- doesFileExist prebuiltManager
      when managerExists $ do
        sayInfo verbose "Installing pre-built morloc-manager"
        run verbose "cp" [prebuiltManager, managerBinPath]
        run verbose "chmod" ["+x", managerBinPath]
    Nothing -> do
      -- Build libmorloc.so + morloc-nexus/manager from source (needs cargo +
      -- the Rust workspace, resolved above).
      when (null rustDir || hasCargo == Nothing) $
        ioError . userError $ unlines
          [ "morloc init requires pre-built libmorloc.so and morloc-nexus binaries."
          , ""
          , "Download them from: https://github.com/morloc-project/morloc/releases"
          , ""
          , "Then set MORLOC_RUST_BIN to the directory containing them:"
          , "  export MORLOC_RUST_BIN=/path/to/binaries"
          , "  morloc init -f"
          , ""
          , "For development, you can build from source instead:"
          , "  1. Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
          , "  2. Set MORLOC_RUST_DIR to the data/rust/ directory in the compiler repo"
          , "  3. Run: morloc init -f"
          ]

      sayInfo verbose "Compiling libmorloc.so (Rust)"
      run verbose "cargo"
        [ "build", "--release"
        , "--manifest-path", rustDir </> "Cargo.toml"
        , "-p", "morloc-runtime"
        ]
      -- Build the .so from the staticlib using gcc --whole-archive.
      -- This exports ALL symbols, which is required because the Rust runtime's
      -- internal state (SHM globals, allocator) must be visible to language
      -- extensions (pymorloc, rmorloc, cppmorloc).
      -- We cannot use the cdylib directly because Rust's cdylib only exports
      -- #[no_mangle] pub extern "C" symbols, and adding a version script to
      -- override this conflicts with Rust's own version script on ARM/aarch64.
      let rustStaticLib = rustDir </> "target" </> "release" </> "libmorloc_runtime.a"
      -- Link libmorloc.so with $CC when set (conda toolchain) so the runtime and
      -- the shims that link it share one compiler/libc world; fall back to gcc.
      cc <- resolveToolName "gcc"
      run verbose cc
        [ "-shared", "-o", soPath
        , "-Wl,--whole-archive", rustStaticLib, "-Wl,--no-whole-archive"
        , "-lpthread", "-lrt", "-ldl", "-lm"
        ]
      hasStrip <- findExecutable "strip"
      case hasStrip of
        Just stripPath -> run verbose stripPath [soPath]
        Nothing -> return ()

      sayInfo verbose "Compiling morloc-nexus (Rust)"
      run verbose "cargo"
        [ "build", "--release"
        , "--manifest-path", rustDir </> "Cargo.toml"
        , "-p", "morloc-nexus"
        ]
      let rustNexus = rustDir </> "target" </> "release" </> "morloc-nexus"
      run verbose "cp" [rustNexus, nexusBinPath]
      case hasStrip of
        Just stripPath -> run verbose stripPath [nexusBinPath]
        Nothing -> return ()

      sayInfo verbose "Compiling morloc-manager (Rust)"
      run verbose "cargo"
        [ "build", "--release"
        , "--manifest-path", rustDir </> "Cargo.toml"
        , "-p", "morloc-manager"
        ]
      let rustManager = rustDir </> "target" </> "release" </> "morloc-manager"
          managerBinPath = nexusBinDir </> "morloc-manager"
      run verbose "cp" [rustManager, managerBinPath]
      case hasStrip of
        Just stripPath -> run verbose stripPath [managerBinPath]
        Nothing -> return ()

      return ()

  -- Persist the Rust workspace SOURCE at $MORLOC_HOME/rust and warm the shared
  -- pool build cache. A Rust pool is now a real Cargo project (Members/Rust.hs)
  -- that path-depends on rustmorloc, so its source (and its sibling
  -- morloc-runtime-types + the workspace Cargo.toml/Cargo.lock) must be present
  -- at `morloc make` time -- unlike the C-ABI libmorloc.so, rustmorloc is
  -- locked to the LOCAL rustc and is compiled per-pool by cargo. We pre-build
  -- rustmorloc into the shared pool target-dir so the first `morloc make`
  -- doesn't pay that compile.
  case (null rustDir, hasCargo) of
    (False, Just _) -> do
      -- Persisting the Rust workspace and warming the rustmorloc cache are Rust
      -- capabilities, not core install steps: a failure (e.g. a cargo build
      -- error) must leave Rust unavailable with a warning, not abort the whole
      -- init. The per-pool build at `morloc make` still enforces cargo via
      -- makeTheMaker, so a broken warm-up cannot silently ship a bad pool.
      optionalStep "Rust configuration failed (Rust pools will not be available): " $ do
        let persistedRustDir = homeDir </> "rust"
            poolTargetDir = libDir </> "rust-build"
        -- If the resolved source IS the persisted copy (no fresher workspace
        -- found), warn instead of silently reusing possibly-stale source; a dev
        -- who edited data/rust must point MORLOC_RUST_DIR at it. Otherwise
        -- refresh the persisted copy from the resolved source.
        srcAbs <- canonicalizePath rustDir
        destExists <- doesDirectoryExist persistedRustDir
        destAbs <- if destExists then canonicalizePath persistedRustDir else return persistedRustDir
        if srcAbs == destAbs
          then sayWarning $
            "Using the persisted Rust source at $MORLOC_HOME/rust (no fresher "
              <> "workspace found). If you changed the compiler's data/rust, set "
              <> "MORLOC_RUST_DIR to it and re-run `morloc init -f`."
          else do
            sayInfo verbose "Persisting Rust workspace source to $MORLOC_HOME/rust"
            persistRustSource verbose srcAbs persistedRustDir destExists
        sayInfo verbose "Warming rustmorloc build cache (Rust pool marshaller)"
        run verbose "cargo"
          [ "build", "--release"
          , "--manifest-path", persistedRustDir </> "Cargo.toml"
          , "-p", "rustmorloc"
          , "--target-dir", poolTargetDir
          ]
    (True, _) ->
      sayWarning $
        "Rust workspace not persisted (Rust pools will not compile): the Rust "
          <> "workspace was not found. Set MORLOC_RUST_DIR to the compiler's "
          <> "data/rust/ directory, then re-run `morloc init -f`."
    (_, Nothing) ->
      sayWarning $
        "Rust workspace not persisted (Rust pools will not compile): `cargo` was "
          <> "not found on PATH. Install Rust (https://rustup.rs) or add cargo to "
          <> "PATH, then re-run `morloc init -f`."

  -- Symlink the newly installed binaries into a "user bin" directory so
  -- they end up on PATH. The directory is selected by the
  -- MORLOC_BIN_LINK_DIR environment variable:
  --
  --   Just <non-empty path>  - create that directory if missing and
  --                            symlink into it (caller-controlled target)
  --   Just ""                - explicit skip, no symlinks created
  --   Nothing                - legacy default: symlink into ~/.local/bin/
  --                            iff that directory already exists
  --
  -- The skip-on-empty case lets a caller (e.g. morloc-manager when running
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
        let managerSrc = nexusBinDir </> "morloc-manager"
        managerExists <- doesFileExist managerSrc
        when managerExists $
          symlinkBinary managerSrc (linkDir </> "morloc-manager")

  -- Create exe/ and fdb/ directories
  let exeDir = homeDir </> "exe"
      fdbDir = homeDir </> "fdb"
  ensureDirectory verbose "morloc exe directory" exeDir
  ensureDirectory verbose "morloc fdb directory" fdbDir

  -- Configure each language via its init.sh script.
  --
  -- Language SELECTION is environment-aware in strict mode: a language is set up
  -- only when its toolchain resolves INSIDE the conda prefix -- the conda env IS
  -- the intended language set. A host tool for a language the env does not
  -- include (e.g. a stray /usr/bin/julia on a CI runner) is NOT built, so we
  -- never compile a shim that mixes a host toolchain with conda's libmorloc.
  -- In non-strict mode, selection stays PATH-based (any tool on PATH), preserving
  -- lenient system-toolchain dev installs.
  --
  -- Shim BUILD is fail-closed in strict mode: a compile failure ABORTS init with
  -- the error, rather than the best-effort warn that silently leaves a half-
  -- configured env whose failure only surfaces later as a runtime import error.
  forM_ DF.langSetups $ \ls -> do
    let tools = DF.lsRequiredTools ls
    avail <- mapM (toolAvailableInEnv mStrictPrefix) tools
    let unavailable = [t | (t, False) <- zip tools avail]
    if null unavailable
      then do
        hPutStrLn stderr $ "Building " <> DF.lsName ls <> " extension ..."
        sayInfo verbose $ "Configuring " <> DF.lsName ls <> " language support"
        -- Write data files to build dir
        forM_ (DF.lsFiles ls) $ \ef ->
          TIO.writeFile (buildDir </> DF.embededFileName ef) (DF.embededFileText ef)
        -- Write and run init script
        let initPath = buildDir </> "init.sh"
        TIO.writeFile initPath (DF.embededFileText (DF.lsInitScript ls))
        let sanitizeFlagsStr = if sanitize then "-fsanitize=alignment -fno-sanitize-recover=alignment" else ""
        let runSetup = run verbose "bash" [initPath, homeDir, buildDir, sanitizeFlagsStr]
        case mStrictPrefix of
          Just _ -> runSetup
          Nothing -> optionalStep (DF.lsName ls <> " setup failed: ") runSetup
        -- Clean up
        removeFileSafe initPath
        forM_ (DF.lsFiles ls) $ \ef ->
          removeFileSafe (buildDir </> DF.embededFileName ef)
      else
        let reason = case mStrictPrefix of
              Just _ -> "not provided by the conda environment"
              Nothing -> "missing"
        in sayWarning $ "Skipping " <> DF.lsName ls <> " setup (" <> reason <> ": " <> unwords unavailable <> ")"

  -- Generate shell completions
  sayInfo verbose "Generating shell completions"
  Completion.regenerateCompletions verbose homeDir

-- | Search for a Rust workspace directory containing Cargo.toml
findRustDir :: [FilePath] -> IO FilePath
findRustDir [] = return ""
findRustDir (d:ds) = do
  exists <- doesFileExist (d </> "Cargo.toml")
  if exists then return d else findRustDir ds

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

-- | Copy a Rust workspace source tree (already resolved to an absolute path) to
-- a stable location under $MORLOC_HOME so generated pool Cargo projects can
-- path-depend on rustmorloc at make time. Build artifacts (any @target@ dir)
-- are excluded. The caller decides whether to persist (it owns the
-- source-vs-destination comparison), so there is no guard here.
persistRustSource :: Bool -> FilePath -> FilePath -> Bool -> IO ()
persistRustSource verbose srcAbs destDir destExists = do
  when destExists $ removeDirectoryRecursive destDir
  createDirectoryIfMissing True destDir
  -- tar-pipe excluding build/VCS dirs; portable across dev machines with cargo.
  run verbose "sh"
    [ "-c"
    , "tar cf - -C '" <> srcAbs <> "' --exclude=target --exclude=.git . | tar xf - -C '" <> destDir <> "'"
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
-- data/lang/*/init.sh and with the libmorloc.so install above. Anything
-- not in this list (notably lib/python/<modName>/ and lib/R/<modName>/
-- populated by `morloc install` via Module.runExpose) is preserved across
-- `morloc init -f`.
initOwnedLibPaths :: [FilePath]
initOwnedLibPaths =
  [ "libmorloc.so"
  , "libcppmorloc.a"
  , "librmorloc.so"
  ]

-- | Names (relative to $MORLOC_HOME/include/) written by morloc init itself.
-- Must stay in sync with data/lang/cpp/init.sh. `mlccpptypes/` is omitted
-- because cpp/init.sh git-clones it on demand and reuses it if present.
-- Anything not in this list (notably include/<modName>/ populated by
-- `morloc install`) is preserved across `morloc init -f`.
initOwnedIncludePaths :: [FilePath]
initOwnedIncludePaths =
  [ "morloc.h"
  , "cppmorloc.hpp"
  , "mlc_arrow.hpp"
  , "morloc_pch.hpp"
  , "morloc_pch.hpp.gch"
  , "nanoarrow"
  ]

-- | Resolve a compiler tool name against the environment. A conda toolchain
-- (e.g. a pixi env) exports $CC/$CXX pointing at prefixed compilers such as
-- @x86_64-conda-linux-gnu-g++@ rather than a plain @gcc@/@g++@, so honor those
-- for the C/C++ compiler names. All other tools are used as given.
resolveToolName :: String -> IO String
resolveToolName tool = do
  mEnv <- case tool of
    "gcc" -> lookupEnv "CC"
    "g++" -> lookupEnv "CXX"
    _     -> return Nothing
  return (fromMaybe tool mEnv)

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
      if null incoherent
        then sayInfo verbose $ "Toolchain coherent with CONDA_PREFIX=" <> prefixAbs
        else
          ioError . userError . unlines $
            [ "Toolchain is incoherent with the active conda environment (CONDA_PREFIX="
                <> prefixAbs
                <> ")."
            , "These build tools resolve to a HOST copy that shadows the conda"
            , "environment's, which mixes ABIs and can crash at pool-load time:"
            ]
              ++ ["  " <> t <> " -> " <> p | (t, p) <- incoherent]
              ++ [ "Ensure the conda environment's tool comes first on PATH"
                 , "(re-activate), or use a container backend."
                 ]
  where
    -- Just (tool, hostPath) if the tool is INCOHERENT; Nothing if it is coherent
    -- (resolves inside the env) or simply not part of this env. A tool resolving
    -- OUTSIDE the prefix is only a problem when the env actually provides it and
    -- a host copy is shadowing it -- NOT when the env never included that
    -- language (e.g. a stray host /usr/bin/julia in an env with no julia). The
    -- C/C++ compiler (gcc/g++, via $CC/$CXX) is the exception: it is always
    -- required, so a host compiler is always a fatal mix.
    locateTool :: FilePath -> String -> IO (Maybe (String, FilePath))
    locateTool prefixAbs tool = do
      loc <- resolveToolLocation tool
      case loc of
        Nothing -> return Nothing
        Just (path, dirAbs)
          | pathIsWithin prefixAbs dirAbs -> return Nothing
          | otherwise -> do
              envProvides <-
                if tool `elem` ["gcc", "g++"]
                  then return True
                  else doesFileExist (prefixAbs </> "bin" </> tool)
              return (if envProvides then Just (tool, path) else Nothing)

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
strictCondaPrefix = do
  strict <- lookupEnv "MORLOC_STRICT_CONDA"
  let enabled = maybe False (`notElem` ["", "0", "false", "no"]) strict
  if not enabled
    then return Nothing
    else do
      mPrefix <- lookupEnv "CONDA_PREFIX"
      case mPrefix of
        Nothing ->
          ioError . userError $
            "MORLOC_STRICT_CONDA is set but CONDA_PREFIX is not: strict mode "
              <> "requires an active conda/pixi environment."
        Just prefix -> Just <$> canonicalizePath prefix

-- | ABI-bearing tools: compilers and language interpreters whose provenance
-- determines the ABI of what they build/run. In strict mode these MUST come from
-- the conda env. Auxiliary tools (git, make, pkg-config) are not ABI-bearing and
-- may come from the host even in strict mode.
abiBearingTools :: [String]
abiBearingTools = ["gcc", "g++", "python3", "R", "julia", "cargo"]

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

