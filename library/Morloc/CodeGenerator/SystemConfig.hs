{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.SystemConfig
Description : Configure the system as needed for the given composition
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.SystemConfig
(
    configure
  , configureAll
) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.DataFiles as DF

import qualified Morloc.Data.Text as MT
import qualified Data.Text.IO as TIO

import System.Process (callCommand, callProcess)
import System.Directory (doesFileExist, removeFile, doesDirectoryExist, createDirectoryIfMissing)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn, stderr)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

-- | Configure for all languages
configureAll :: Bool -> Bool -> Bool -> Config -> IO ()
configureAll verbose force slurmSupport config = do 

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
  TIO.writeFile (configBuildConfig config) ( if slurmSupport then "slurm-support: true" else "slurm-support: false" )

  say "Installing C++ extra types"

  let mlccpptypesPath = includeDir </> "mlccpptypes"
  mlccpptypesExists <- doesDirectoryExist mlccpptypesPath

  unless mlccpptypesExists $ do
    let mlccpptypesRepoUrl = "https://github.com/morloclib/mlccpptypes"
        cmd = unwords ["git clone", mlccpptypesRepoUrl, mlccpptypesPath]

    say "installing mlccpptypes"
    callCommand cmd

  say "Configuring R socket library"
  let srcpath = configHome config </> "lib" </> "socketr.c"
      objpath = configHome config </> "lib" </> "socketr.o"
      libpath = configHome config </> "lib" </> "libsocketr.so"
  compileCCodeIfNeeded (DF.embededFileText DF.rSocketLib) srcpath libpath objpath

  let libmorlocPath = includeDir </> DF.embededFileName (DF.libMorlocH DF.libmorloc)
      libhashPath = includeDir </> DF.embededFileName (DF.libHashH DF.libmorloc)

  say "Creating morloc.h"
  TIO.writeFile libmorlocPath $ DF.embededFileText (DF.libMorlocH DF.libmorloc)
  TIO.writeFile libhashPath $ DF.embededFileText (DF.libHashH DF.libmorloc)

  say "Generating libmorloc.so"
  -- this is a stupid hack to make gcc compile a header to a shared object
  let tmpCFile = tmpDir </> "x.c"

      soPath = libDir </> "libmorloc.so"
  TIO.writeFile tmpCFile (
      "#include \"" <> MT.pack libmorlocPath <> "\"" <> "\n" <>
      "#include \"" <> MT.pack libhashPath <> "\""
    )
  -- special system-wide morloc build options
  let morlocOptions = ( if slurmSupport then ["-DSLURM_SUPPORT"] else [] )
  let gccArgs = [ "-O", "-shared", "-o", soPath, "-fPIC", tmpCFile ] <> morlocOptions
  callProcess "gcc" gccArgs
  removeFile tmpCFile

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
  compileCCodeIfNeeded
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
          then when verbose $
              putStrLn $ "Checking " ++ description ++ " ... using existing path " ++ path
          else do
              createDirectoryIfMissing True path
              when verbose $
                  putStrLn $ "Checking " ++ description ++ " ... missing, creating at " ++ path

  compileCCodeIfNeeded :: MT.Text -> Path -> Path -> Path -> IO ()
  compileCCodeIfNeeded codeText sourcePath libPath objPath = do
      alreadyExists <- doesFileExist libPath
      if (alreadyExists && not force)
          then return ()
          else do
              -- Write the code to the temporary file
              withFile sourcePath WriteMode $ \tempHandle -> do
                  MT.hPutStr tempHandle codeText

             -- Compile the C code, will generate a .so file with same path and
             -- basename as the source .c file
              let compileCommand = "R CMD SHLIB " ++ sourcePath ++ " -o " ++ libPath 

              callCommand compileCommand

              -- Delete the source .c file
              sourcePathExists <- doesFileExist sourcePath
              when (sourcePathExists) (removeFile sourcePath)

              -- Delete the source .o file
              objPathExists <- doesFileExist objPath
              when (objPathExists) (removeFile objPath)
