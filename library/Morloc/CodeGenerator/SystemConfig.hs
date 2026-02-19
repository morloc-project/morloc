{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.SystemConfig
Description : Configure the system as needed for the given composition
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.SystemConfig
  ( configure
  , configureAll
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Completion as Completion
import qualified Morloc.DataFiles as DF
import Morloc.Module (OverwriteProtocol(..))

import qualified Data.Text.IO as TIO

import Control.Exception (SomeException, displayException, try)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory, removeFile)
import System.FilePath (takeFileName, replaceExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

configureAll :: Bool -> OverwriteProtocol -> Bool -> Config -> IO Bool
configureAll verbose _force slurmSupport config = do
  result <- try (configureAllSteps verbose slurmSupport config) :: IO (Either SomeException ())
  case result of
    Left e -> do
      hPutStrLn stderr $ "\ESC[31mConfiguration failed: " ++ displayException e ++ "\ESC[0m"
      return False
    Right _ -> return True

configureAllSteps :: Bool -> Bool -> Config -> IO ()
configureAllSteps verbose slurmSupport config = do
  let homeDir = configHome config
      srcLibrary = configLibrary config
      includeDir = homeDir </> "include"
      tmpDir = configTmpDir config
      optDir = homeDir </> "opt"
      libDir = homeDir </> "lib"

  createDirectoryWithDescription verbose "morloc home directory" homeDir
  createDirectoryWithDescription verbose "morloc lib directory" libDir
  createDirectoryWithDescription verbose "morloc include directory" includeDir
  createDirectoryWithDescription verbose "morloc tmp directory" tmpDir
  createDirectoryWithDescription verbose "morloc opt directory" optDir
  createDirectoryWithDescription verbose "morloc module directory" srcLibrary

  say verbose $ "Slurm support ... " <> show slurmSupport

  say verbose "Writing build config file"
  TIO.writeFile
    (configBuildConfig config)
    (if slurmSupport then "slurm-support: true" else "slurm-support: false")

  -- Build libmorloc.so
  let buildDir = tmpDir </> "libmorloc-build"
  createDirectoryIfMissing True buildDir

  say verbose "Writing morloc C library source files"
  forM_ DF.libmorlocFiles $ \ef ->
    TIO.writeFile (buildDir </> DF.embededFileName ef) (DF.embededFileText ef)

  say verbose "Compiling libmorloc.so"
  let morlocOptions = if slurmSupport then ["-DSLURM_SUPPORT"] else []
      cFiles = [buildDir </> DF.embededFileName ef | ef <- DF.libmorlocFiles, ".c" `isSuffixOf` DF.embededFileName ef]
      soPath = libDir </> "libmorloc.so"
      objPaths = [buildDir </> replaceExtension (takeFileName f) "o" | f <- cFiles]
  forM_ (zip cFiles objPaths) $ \(cFile, objPath) -> do
    let args = ["-c", "-Wall", "-Werror", "-O2", "-fPIC", "-I" <> buildDir, "-o", objPath, cFile] <> morlocOptions
    run verbose "gcc" args
  let soArgs = ["-shared", "-o", soPath] <> objPaths <> ["-lpthread"]
  run verbose "gcc" soArgs
  forM_ DF.libmorlocFiles $ \ef -> removeFile (buildDir </> DF.embededFileName ef)
  forM_ objPaths removeFile

  say verbose "Installing morloc.h"
  TIO.writeFile (includeDir </> "morloc.h") DF.libmorlocHeader

  -- Compile mim (morloc install manager)
  say verbose "Compiling mim"
  userHome <- getHomeDirectory
  let localBinDir = userHome </> ".local" </> "bin"
      nexusSrcPath = buildDir </> "nexus.c"
      mimBinPath = localBinDir </> "mim"
  createDirectoryIfMissing True localBinDir
  TIO.writeFile nexusSrcPath (DF.embededFileText DF.nexusSource)
  run verbose "gcc" ["-O2", "-I" <> includeDir, "-o", mimBinPath, nexusSrcPath,
             "-L" <> libDir, "-Wl,-rpath," <> libDir, "-lmorloc", "-lpthread"]
  removeFile nexusSrcPath

  -- Create exe/ and fdb/ directories
  let exeDir = homeDir </> "exe"
      fdbDir = homeDir </> "fdb"
  createDirectoryWithDescription verbose "morloc exe directory" exeDir
  createDirectoryWithDescription verbose "morloc fdb directory" fdbDir

  -- Configure each language via its init.sh script
  forM_ DF.langSetups $ \ls -> do
    say verbose $ "Configuring " <> DF.lsName ls <> " morloc API libraries"
    -- Write data files to build dir
    forM_ (DF.lsFiles ls) $ \ef ->
      TIO.writeFile (buildDir </> DF.embededFileName ef) (DF.embededFileText ef)
    -- Write and run init script
    let initPath = buildDir </> "init.sh"
    TIO.writeFile initPath (DF.embededFileText (DF.lsInitScript ls))
    run verbose "bash" [initPath, homeDir, buildDir]
    -- Clean up
    removeFile initPath
    forM_ (DF.lsFiles ls) $ \ef ->
      removeFileSafe (buildDir </> DF.embededFileName ef)

  -- Generate shell completions
  say verbose "Generating shell completions"
  Completion.regenerateCompletions verbose homeDir

say :: Bool -> String -> IO ()
say verbose message =
  when verbose $ hPutStrLn stderr ("\ESC[32m" <> message <> "\ESC[0m")

run :: Bool -> String -> [String] -> IO ()
run verbose cmd args = do
  when verbose $ hPutStrLn stderr (cmd <> " " <> unwords args)
  callProcess cmd args

createDirectoryWithDescription :: Bool -> String -> FilePath -> IO ()
createDirectoryWithDescription verbose description path = do
  exists <- doesDirectoryExist path
  if exists
    then
      when verbose $
        putStrLn $ "Checking " ++ description ++ " ... using existing path " ++ path
    else do
      createDirectoryIfMissing True path
      when verbose $
        putStrLn $ "Checking " ++ description ++ " ... missing, creating at " ++ path

-- | Remove a file, ignoring errors if it was already cleaned up by init.sh
removeFileSafe :: FilePath -> IO ()
removeFileSafe path = do
  result <- try (removeFile path) :: IO (Either SomeException ())
  case result of
    Left _ -> return ()
    Right _ -> return ()
