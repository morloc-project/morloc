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
import Morloc.Data.Doc ((<+>), vsep, pretty)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import qualified Morloc.System as MS
import qualified System.Directory as SD

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
    errs -> MM.throwSystemError $ "Failed to write generated files:" <+> vsep
      [pretty (show e) | Failed _ e <- errs]
  mapM_ runSysCommand (scriptMake s)

runSysCommand :: SysCommand -> MorlocMonad ()
runSysCommand (SysExe path) = do
  p <- liftIO $ SD.getPermissions path
  liftIO $ SD.setPermissions path (p {SD.executable = True})
runSysCommand (SysRun (Code cmd)) = MM.runCommand "runSysCommand" cmd
runSysCommand other =
  MM.throwSystemError $ "Unsupported SysCommand: " <> pretty (show other)
