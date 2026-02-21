{-# LANGUAGE OverloadedStrings #-}

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

import Control.Exception (SomeException, displayException, try)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable, getHomeDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (replaceExtension, takeFileName)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

configureAll :: Bool -> OverwriteProtocol -> Bool -> Config -> IO Bool
configureAll verbose force slurmSupport config = do
  result <- try (configureAllSteps verbose force slurmSupport config) :: IO (Either SomeException ())
  case result of
    Left e -> do
      sayError $ "Configuration failed: " ++ displayException e
      return False
    Right _ -> return True

configureAllSteps :: Bool -> OverwriteProtocol -> Bool -> Config -> IO ()
configureAllSteps verbose force slurmSupport config = do
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

  sayInfo verbose "Writing build config file"
  TIO.writeFile
    (configBuildConfig config)
    (if slurmSupport then "slurm-support: true" else "slurm-support: false")

  -- Clean and create build directory
  let buildDir = tmpDir </> "libmorloc-build"
  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir
  createDirectoryIfMissing True buildDir

  -- Check for gcc (required for core)
  requireTool "gcc" "gcc is required to compile libmorloc.so and morloc-nexus"

  sayInfo verbose "Writing morloc C library source files"
  forM_ DF.libmorlocFiles $ \ef ->
    TIO.writeFile (buildDir </> DF.embededFileName ef) (DF.embededFileText ef)

  sayInfo verbose "Compiling libmorloc.so"
  let morlocOptions = if slurmSupport then ["-DSLURM_SUPPORT"] else []
      cFiles =
        [ buildDir </> DF.embededFileName ef | ef <- DF.libmorlocFiles, ".c" `isSuffixOf` DF.embededFileName ef
        ]
      soPath = libDir </> "libmorloc.so"
      objPaths = [buildDir </> replaceExtension (takeFileName f) "o" | f <- cFiles]
  forM_ (zip cFiles objPaths) $ \(cFile, objPath) -> do
    let args =
          ["-c", "-Wall", "-Werror", "-O2", "-fPIC", "-I" <> buildDir, "-o", objPath, cFile] <> morlocOptions
    run verbose "gcc" args
  let soArgs = ["-shared", "-o", soPath] <> objPaths <> ["-lpthread"]
  run verbose "gcc" soArgs
  forM_ DF.libmorlocFiles $ \ef -> removeFile (buildDir </> DF.embededFileName ef)
  forM_ objPaths removeFile

  sayInfo verbose "Installing morloc.h"
  TIO.writeFile (includeDir </> "morloc.h") DF.libmorlocHeader

  -- Compile morloc-nexus (installed to ~/.local/bin alongside the morloc binary,
  -- since wrapper scripts invoke it by bare name via PATH)
  sayInfo verbose "Compiling morloc-nexus"
  userHome <- getHomeDirectory
  let nexusBinDir = userHome </> ".local" </> "bin"
      nexusSrcPath = buildDir </> "nexus.c"
      nexusBinPath = nexusBinDir </> "morloc-nexus"
  createDirectoryIfMissing True nexusBinDir
  TIO.writeFile nexusSrcPath (DF.embededFileText DF.nexusSource)
  run
    verbose
    "gcc"
    [ "-O2"
    , "-I" <> includeDir
    , "-o"
    , nexusBinPath
    , nexusSrcPath
    , "-L" <> libDir
    , "-Wl,-rpath," <> libDir
    , "-lmorloc"
    , "-lpthread"
    ]
  removeFile nexusSrcPath

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
        result <- try (run verbose "bash" [initPath, homeDir, buildDir]) :: IO (Either SomeException ())
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

-- | Check which tools from a list are missing. Returns list of missing tool names.
checkTools :: [String] -> IO [String]
checkTools tools = do
  results <- forM tools $ \tool -> do
    found <- findExecutable tool
    return (tool, found)
  return [t | (t, Nothing) <- results]
