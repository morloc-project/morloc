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
import qualified Morloc.DataFiles as DF
import Morloc.Module (OverwriteProtocol (..))

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Morloc.Data.Text as MT

import Control.Exception (SomeException, displayException, try)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile)
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)
import System.Process (callCommand, callProcess)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

configureAll :: Bool -> OverwriteProtocol -> Bool -> Config -> IO Bool
configureAll verbose force slurmSupport config = do
  -- Wrap the entire configuration process in exception handling
  result <- try (configureAllSteps verbose force slurmSupport config) :: IO (Either SomeException ())
  case result of
    Left e -> do
      hPutStrLn stderr $ "\ESC[31mConfiguration failed: " ++ displayException e ++ "\ESC[0m"
      return False
    Right _ -> return True

-- | Configure for all languages
configureAllSteps :: Bool -> OverwriteProtocol -> Bool -> Config -> IO ()
configureAllSteps verbose force slurmSupport config = do
  -- Setup Morloc home directory structure
  let homeDir = configHome config
      srcLibrary = configLibrary config
      includeDir = configHome config </> "include"
      tmpDir = configTmpDir config
      optDir = configHome config </> "opt"
      libDir = configHome config </> "lib"

  createDirectoryWithDescription "morloc home directory" homeDir
  createDirectoryWithDescription "morloc lib directory" libDir
  createDirectoryWithDescription "morloc include directory" includeDir
  createDirectoryWithDescription "morloc tmp directory" tmpDir
  createDirectoryWithDescription "morloc opt directory" optDir
  createDirectoryWithDescription "morloc module directory" srcLibrary

  say $ "Slurm support ... " <> show slurmSupport

  say $ "Writing build config file"

  -- currently SLURM support is the only build option
  TIO.writeFile
    (configBuildConfig config)
    (if slurmSupport then "slurm-support: true" else "slurm-support: false")

  say "Installing C++ extra types"

  let mlccpptypesPath = includeDir </> "mlccpptypes"
  mlccpptypesExists <- doesDirectoryExist mlccpptypesPath

  unless mlccpptypesExists $ do
    let mlccpptypesRepoUrl = "https://github.com/morloclib/mlccpptypes"
        cmd = unwords ["git clone", mlccpptypesRepoUrl, mlccpptypesPath]

    say "installing mlccpptypes"
    callCommand cmd

  let libmorlocHPath = includeDir </> DF.embededFileName (DF.libMorlocH DF.libmorloc)
      libmorlocCPath = includeDir </> DF.embededFileName (DF.libMorlocC DF.libmorloc)
      libhashPath = includeDir </> DF.embededFileName (DF.libHashH DF.libmorloc)

  say "Writing morloc.h, morloc.c, xxhash.h"
  TIO.writeFile libmorlocHPath $ DF.embededFileText (DF.libMorlocH DF.libmorloc)
  TIO.writeFile libmorlocCPath $ DF.embededFileText (DF.libMorlocC DF.libmorloc)
  TIO.writeFile libhashPath $ DF.embededFileText (DF.libHashH DF.libmorloc)

  say "Compiling libmorloc.a and libmorloc.so"
  let morlocOptions = (if slurmSupport then ["-DSLURM_SUPPORT"] else [])
      objPath = tmpDir </> "morloc.o"
      soPath = libDir </> "libmorloc.so"
      aPath = libDir </> "libmorloc.a"
  -- Compile morloc.c to object file
  callProcess "gcc" $ ["-c", "-O2", "-fPIC", "-I" <> includeDir, "-o", objPath, libmorlocCPath] <> morlocOptions
  -- Create static library
  callProcess "ar" ["rcs", aPath, objPath]
  -- Create shared library
  callProcess "gcc" ["-shared", "-o", soPath, objPath]
  -- Clean up object file
  removeFile objPath

  say "Configuring C++ morloc api header"
  TIO.writeFile (includeDir </> DF.embededFileName DF.libcpplang) (DF.embededFileText DF.libcpplang)

  say "Configuring python MessagePack libraries"
  let libpyPath = optDir </> DF.embededFileName DF.libpylang
      libpySetupPath = optDir </> DF.embededFileName DF.libpylangSetup
      libpyMakePath = optDir </> DF.embededFileName DF.libpylangMakefile

  TIO.writeFile libpyPath $ DF.embededFileText DF.libpylang
  TIO.writeFile libpySetupPath $ DF.embededFileText DF.libpylangSetup
  TIO.writeFile libpyMakePath $ DF.embededFileText DF.libpylangMakefile

  say "Generating libpymorloc.so"
  callCommand ("make -C " <> optDir <> " -f " <> DF.embededFileName DF.libpylangMakefile)

  say "Configuring R morloc API libraries"
  compileCCodeIfNeeded libDir
    (DF.embededFileText DF.librlang)
    (includeDir </> DF.embededFileName DF.librlang)
    (libDir </> "librmorloc.so")
    (includeDir </> "librmorloc.o")
  where
    ok msg = "\ESC[32m" <> msg <> "\ESC[0m"

    say :: String -> IO ()
    say message =
      if verbose
        then do
          hPutStrLn stderr (ok message)
        else
          return ()

    createDirectoryWithDescription :: String -> FilePath -> IO ()
    createDirectoryWithDescription description path = do
      exists <- doesDirectoryExist path
      if exists
        then
          when verbose $
            putStrLn $
              "Checking " ++ description ++ " ... using existing path " ++ path
        else do
          createDirectoryIfMissing True path
          when verbose $
            putStrLn $
              "Checking " ++ description ++ " ... missing, creating at " ++ path

    compileCCodeIfNeeded :: Path -> Text -> Path -> Path -> Path -> IO ()
    compileCCodeIfNeeded libDir' codeText sourcePath libPath objPath = do
      alreadyExists <- doesFileExist libPath
      if (alreadyExists && force == DoNotOverwrite)
        then return ()
        else do
          -- Write the code to the temporary file
          withFile sourcePath WriteMode $ \tempHandle -> do
            MT.hPutStr tempHandle codeText

          -- Compile the C code, will generate a .so file with same path and
          -- basename as the source .c file
          let compileCommand = "R CMD SHLIB " ++ sourcePath ++ " -o " ++ libPath
                ++ " -L" ++ libDir' ++ " -Wl,-rpath," ++ libDir' ++ " -lmorloc -lpthread"

          callCommand compileCommand

          -- Delete the source .c file
          sourcePathExists <- doesFileExist sourcePath
          when (sourcePathExists) (removeFile sourcePath)

          -- Delete the source .o file
          objPathExists <- doesFileExist objPath
          when (objPathExists) (removeFile objPath)
