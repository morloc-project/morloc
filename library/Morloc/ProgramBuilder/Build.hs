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

import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import qualified Morloc.System as MS
import qualified System.Directory as SD

buildProgram :: (Script, [Script]) -> MorlocMonad ()
buildProgram (nexus, pools) = mapM_ build (nexus : pools)

build :: Script -> MorlocMonad ()
build s = do
  -- write the required file structure
  _ <- liftIO $ MS.writeDirectoryWith (\f c -> MT.writeFile f (unCode c)) (scriptCode s)
  -- execute all make commands
  mapM_ runSysCommand (scriptMake s)

runSysCommand :: SysCommand -> MorlocMonad ()
runSysCommand (SysExe path) = do
  p <- liftIO $ SD.getPermissions path
  liftIO $ SD.setPermissions path (p {SD.executable = True})
runSysCommand (SysMove _ _) = undefined
runSysCommand (SysRun (Code cmd)) = MM.runCommand "runSysCommand" cmd
runSysCommand (SysInstall _) = undefined
runSysCommand (SysUnlink _) = undefined
