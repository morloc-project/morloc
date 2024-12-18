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
import Morloc.DataFiles (rSocketLib, pympack, msgpackSource, rmpack, cppmpack)

import qualified Morloc.Data.Text as MT
import qualified Data.Text.IO as TIO

import System.Process (callCommand, callProcess, readCreateProcessWithExitCode, shell)
import System.Directory (doesFileExist, removeFile, doesDirectoryExist, createDirectoryIfMissing)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn, stderr)

import System.Exit (ExitCode(..), exitWith)

configure :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad ()
configure _ = return ()

-- | Configure for all languages
configureAll :: Bool -> Bool -> Config -> IO ()
configureAll verbose force config = do 

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

  say "Installing C++ extra types"

  let mlccpptypesPath = includeDir </> "mlccpptypes"
  mlccpptypesExists <- doesDirectoryExist mlccpptypesPath

  unless mlccpptypesExists $ do
    let mlccpptypesRepoUrl = "https://github.com/morloclib/mlccpptypes"
        cmd = unwords ["git clone", mlccpptypesRepoUrl, mlccpptypesPath]
    runCommand "installing mlccpptypes" cmd

  say "Configuring R socket library"
  let srcpath = configHome config </> "lib" </> "socketr.c"
      objpath = configHome config </> "lib" </> "socketr.o"
      libpath = configHome config </> "lib" </> "libsocketr.so"
  compileCCodeIfNeeded (snd $ rSocketLib) srcpath libpath objpath

  let mlcmsgpackHeader = includeDir </> fst msgpackSource

  say "Creating mlcmpack header"
  TIO.writeFile mlcmsgpackHeader (snd msgpackSource)

  -- Check if mlcmpack.so exists
  let soPath = libDir </> "libmlcmpack.so"
  soExists <- doesFileExist soPath

  -- if the library doesn't exist, make it
  unless (soExists && not force) $ do
    say "Generating libmlcmpack.so"
    -- this is a stupid hack to make gcc compile a header to a shared object
    let tmpCFile = tmpDir </> "x.c"
    TIO.writeFile tmpCFile ("#include \"" <> MT.pack mlcmsgpackHeader <> "\"")
    let gccArgs = [ "-shared", "-o", soPath, "-fPIC", tmpCFile ]
    callProcess "gcc" gccArgs
    removeFile tmpCFile

  say "Configuring C++ MessagePack header"
  TIO.writeFile (includeDir </> fst cppmpack) (snd cppmpack)

  say "Configuring python MessagePack libraries"
  let (pympackFilename, pympackCode) = pympack
  let pymackPath = optDir </> pympackFilename
  TIO.writeFile pymackPath pympackCode


  say "Configuring R MessagePack libraries"
  compileCCodeIfNeeded
    (snd rmpack)
    (includeDir </> "mpackr.c")
    (libDir </> "libmpackr.so")
    (includeDir </> "mpackr.o")
  where

  ok msg = "\ESC[32m" <> msg <> "\ESC[0m"
  bad msg = "\ESC[31m" <> msg <> "\ESC[0m"

  say :: String -> IO ()
  say message =
    if verbose
    then do
      hPutStrLn stderr (ok message)
    else
      return ()

  warn :: String -> IO ()
  warn message =
    if verbose
    then do
      hPutStrLn stderr (bad message)
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

  runCommand ::
       String -- description of action
    -> String -- system command
    -> IO ()
  runCommand runMsg cmd = do
    say runMsg
    putStrLn $ cmd
    (exitCode, _, err') <-
      readCreateProcessWithExitCode (shell cmd) []
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure code -> do
        warn "  command failed - exiting"
        putStrLn err'
        exitWith (ExitFailure code)
