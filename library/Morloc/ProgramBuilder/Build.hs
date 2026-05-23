{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.ProgramBuilder.Build
Description : Compile pool source files and assemble the final executable
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Orchestrates the @morloc make@ build step: writes generated pool source
files, compiles them with the appropriate language toolchain, copies the
pre-compiled nexus binary, and writes the manifest file.
-}
module Morloc.ProgramBuilder.Build
  ( buildProgram
  ) where

import Control.Monad.Except (catchError, throwError)
import Morloc.Data.Doc ((<+>), line, vsep, pretty)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import qualified Morloc.System as MS
import qualified System.Directory as SD
import System.IO.Error (ioeGetFileName)
import System.Process (callProcess)

buildProgram :: (Script, [Script]) -> MorlocMonad ()
buildProgram (nexus, pools) = do
  installDir <- MM.gets stateInstallDir
  case installDir of
    Just dir -> do
      -- When installing, copy includes and the morloc script into the install
      -- directory, cd there, and build as normal. This avoids leaving artifacts
      -- in CWD and ensures the installed pools are a fresh build.
      force <- MM.gets stateInstallForce
      dirExists <- liftIO $ SD.doesDirectoryExist dir
      when (dirExists && not force) $ do
        contents <- liftIO $ SD.listDirectory dir
        unless (null contents) $
          MM.throwSystemError $ "Install directory already exists: " <> pretty dir
            <> ". Use --force to overwrite."
      when (dirExists && force) $
        liftIO $ SD.removeDirectoryRecursive dir
      liftIO $ SD.createDirectoryIfMissing True dir
      origDir <- liftIO SD.getCurrentDirectory
      liftIO $ SD.setCurrentDirectory dir
      mapM_ build (nexus : pools) `finally` liftIO (SD.setCurrentDirectory origDir)
    Nothing ->
      mapM_ build (nexus : pools)

-- | catch/finally for MorlocMonad
finally :: MorlocMonad a -> MorlocMonad () -> MorlocMonad a
finally action cleanup = do
  result <- catchError (fmap Right action) (return . Left)
  cleanup
  case result of
    Right a -> return a
    Left e -> throwError e

build :: Script -> MorlocMonad ()
build s = do
  (_ :/ tree) <- liftIO $ MS.writeDirectoryWith (\f c -> MT.writeFile f (unCode c)) (scriptCode s)
  case failures tree of
    [] -> return ()
    errs -> do
      msgs <- liftIO (mapM describeWriteFailure errs)
      MM.throwSystemError (vsep msgs)
  mapM_ runSysCommand (scriptMake s)

-- | Turn a directory-tree write failure into an actionable message.
-- A directory occupying the output path is the common, recoverable
-- mistake (the chosen binary name collides with an existing
-- directory); name the conflict and point at @-o@ instead of leaking
-- the raw 'IOError' text to the user.
describeWriteFailure :: DirTree a -> IO MDoc
describeWriteFailure (Failed _ e) = do
  let mpath = ioeGetFileName e
  isDir <- maybe (return False) SD.doesDirectoryExist mpath
  return $
    if isDir
      then "Cannot write the output file" <+> maybe "(the output path)" pretty mpath
             <> ": a directory of that name already exists."
             <> line
             <> "Choose a different output name with -o, or remove the directory."
      else "Failed to write generated file:" <+> pretty (show e)
describeWriteFailure _ = return "Failed to write generated files."

runSysCommand :: SysCommand -> MorlocMonad ()
runSysCommand (SysExe path) = liftIO $ callProcess "chmod" ["755", path]
runSysCommand (SysRun (Code cmd)) = MM.runCommand "runSysCommand" cmd
runSysCommand other =
  MM.throwSystemError $ "Unsupported SysCommand: " <> pretty (show other)
