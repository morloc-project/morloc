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
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Completion as Completion
import qualified Morloc.DataFiles as DF
import Morloc.Module (OverwriteProtocol (..))

import qualified Data.Text.IO as TIO

import Control.Exception (SomeException, catch, displayException, fromException, try)
import System.IO.Error (ioeGetErrorString)
import System.Directory (createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory, listDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
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

  -- When force is set, clean build output directories
  when (force == ForceOverwrite) $ do
    sayInfo verbose "Force rebuild: cleaning stale artifacts"
    cleanDirectory libDir
    cleanDirectoryExcept includeDir ["mlccpptypes"]
    cleanDirectory optDir

  ensureDirectory verbose "morloc home directory" homeDir
  ensureDirectory verbose "morloc lib directory" libDir
  ensureDirectory verbose "morloc include directory" includeDir
  ensureDirectory verbose "morloc tmp directory" tmpDir
  ensureDirectory verbose "morloc opt directory" optDir
  ensureDirectory verbose "morloc module directory" srcLibrary

  sayInfo verbose $ "Slurm support ... " <> show slurmSupport

  sayInfo verbose $ "Sanitize ... " <> show sanitize

  sayInfo verbose "Writing build config file"
  let sanitizeLine = if sanitize then "\nsanitize: true" else "\nsanitize: false"
  TIO.writeFile
    (configBuildConfig config)
    ((if slurmSupport then "slurm-support: true" else "slurm-support: false") <> sanitizeLine)

  -- Clean and create build directory
  let buildDir = tmpDir </> "libmorloc-build"
  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir
  createDirectoryIfMissing True buildDir

  requireTool "gcc" "gcc is required to compile language extensions (C++ pools, Python/R bindings)"
  -- Install morloc.h (the C ABI contract for language extensions and pool templates)
  sayInfo verbose "Installing morloc.h"
  TIO.writeFile (includeDir </> "morloc.h") DF.libmorlocHeader

  -- Install libmorloc.so and morloc-nexus.
  --
  -- Strategy (in priority order):
  --   1. MORLOC_RUST_BIN: directory with pre-built libmorloc.so + morloc-nexus
  --      (used for release installs with portable musl-linked binaries)
  --   2. MORLOC_RUST_DIR: Cargo workspace source — build from source via cargo
  --      (used for development and container builds)
  --   3. Auto-detect Cargo workspace relative to morloc binary
  let soPath = libDir </> "libmorloc.so"
  -- Primary install goes to $MORLOC_HOME/bin/
  let nexusBinDir = homeDir </> "bin"
      nexusBinPath = nexusBinDir </> "morloc-nexus"
  createDirectoryIfMissing True nexusBinDir
  -- Symlink to ~/.local/bin/ if that directory exists
  userHome <- getHomeDirectory
  let userBinDir = userHome </> ".local" </> "bin"

  rustBinEnv <- lookupEnv "MORLOC_RUST_BIN"
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
      -- Try to build from source: need both cargo and the Rust workspace
      rustDirEnv <- lookupEnv "MORLOC_RUST_DIR"
      rustDir <- case rustDirEnv of
        Just d -> return d
        Nothing -> do
          morlocBin <- findExecutable "morloc"
          let searchDirs = case morlocBin of
                Just binPath ->
                  [ takeDirectory (takeDirectory binPath) </> "share" </> "morloc" </> "rust"
                  , takeDirectory (takeDirectory binPath) </> "data" </> "rust"
                  ]
                Nothing -> []
          findRustDir searchDirs

      hasCargo <- findExecutable "cargo"

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
      run verbose "gcc"
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


  -- Symlink binaries to ~/.local/bin/ if it exists
  userBinExists <- doesDirectoryExist userBinDir
  when userBinExists $ do
    symlinkBinary nexusBinPath (userBinDir </> "morloc-nexus")
    let managerSrc = nexusBinDir </> "morloc-manager"
    managerExists <- doesFileExist managerSrc
    when managerExists $
      symlinkBinary managerSrc (userBinDir </> "morloc-manager")

  -- Create exe/ and fdb/ directories
  let exeDir = homeDir </> "exe"
      fdbDir = homeDir </> "fdb"
  ensureDirectory verbose "morloc exe directory" exeDir
  ensureDirectory verbose "morloc fdb directory" fdbDir

  -- Configure each language via its init.sh script
  forM_ DF.langSetups $ \ls -> do
    missing <- checkTools (DF.lsRequiredTools ls)
    if null missing
      then do
        sayInfo verbose $ "Configuring " <> DF.lsName ls <> " language support"
        -- Write data files to build dir
        forM_ (DF.lsFiles ls) $ \ef ->
          TIO.writeFile (buildDir </> DF.embededFileName ef) (DF.embededFileText ef)
        -- Write and run init script
        let initPath = buildDir </> "init.sh"
        TIO.writeFile initPath (DF.embededFileText (DF.lsInitScript ls))
        let sanitizeFlagsStr = if sanitize then "-fsanitize=alignment -fno-sanitize-recover=alignment" else ""
        result <- try (run verbose "bash" [initPath, homeDir, buildDir, sanitizeFlagsStr]) :: IO (Either SomeException ())
        case result of
          Left e -> sayWarning $ DF.lsName ls <> " setup failed: " <> displayException e
          Right _ -> return ()
        -- Clean up
        removeFileSafe initPath
        forM_ (DF.lsFiles ls) $ \ef ->
          removeFileSafe (buildDir </> DF.embededFileName ef)
      else
        sayWarning $ "Skipping " <> DF.lsName ls <> " setup (missing: " <> unwords missing <> ")"

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

-- | Remove all contents of a directory except entries in the keep list
cleanDirectoryExcept :: FilePath -> [String] -> IO ()
cleanDirectoryExcept dir keep = do
  exists <- doesDirectoryExist dir
  when exists $ do
    entries <- listDirectory dir
    forM_ entries $ \entry -> do
      unless (entry `elem` keep) $ do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir
          then removeDirectoryRecursive path
          else removeFile path

-- | Check that a tool exists on PATH, error if not
requireTool :: String -> String -> IO ()
requireTool tool msg = do
  found <- findExecutable tool
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

-- | Check which tools from a list are missing. Returns list of missing tool names.
checkTools :: [String] -> IO [String]
checkTools tools = do
  results <- forM tools $ \tool -> do
    found <- findExecutable tool
    return (tool, found)
  return [t | (t, Nothing) <- results]
