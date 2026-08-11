{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.ProgramBuilder.Build
Description : Compile pool source files and assemble the final executable
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Orchestrates the @morloc make@ build step: writes @manifest.json@ and the
per-language pool sources into a staging directory, compiles the pools with
the appropriate language toolchain, then atomically swaps the staging tree
into the final @<key>-build@ directory. Finally it writes the thin shell
launcher wrappers that point at the built @manifest.json@.
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
import Morloc.ProgramBuilder.Paths (buildMarker)
import qualified Morloc.System as MS
import qualified System.Directory as SD
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Error (ioeGetFileName)
import System.Process (callProcess, getCurrentPid)

buildProgram :: (Script, [WrapperFile], [Script]) -> MorlocMonad ()
buildProgram (manifest, wrappers, pools) = do
  mBuildDir <- MM.gets stateInstallDir
  buildDir <- case mBuildDir of
    Just d -> return d
    Nothing -> liftIO SD.getCurrentDirectory
  isInstall <- MM.gets stateInstall
  mRoot <- MM.gets stateBuildRoot
  force <- MM.gets stateInstallForce

  -- The atomically-swapped, marker-owned unit. For install it is the ROOT
  -- (exe/<key>): swapping it wholesale means a --force reinstall replaces
  -- stale mirrored sources too, not just the nested build. `make` owns only
  -- <key>-build and must never swap the working directory.
  let swapTarget = if isInstall then maybe buildDir id mRoot else buildDir

  -- Install-mode guard: refuse to clobber a populated install root without
  -- --force, so a failed reinstall can never destroy an existing program
  -- before its bin/ entry is checked.
  when isInstall $ do
    dirExists <- liftIO $ SD.doesDirectoryExist swapTarget
    when dirExists $ do
      contents <- liftIO $ SD.listDirectory swapTarget
      when (not (null contents) && not force) $
        MM.throwSystemError $ "Install directory already exists: " <> pretty swapTarget
          <> ". Use --force to overwrite."

  -- Build into a sibling staging directory (same parent, so the later
  -- rename is atomic), then swap it into place. A crash mid-build leaves
  -- the previous good build untouched. The manifest + pools tree is written
  -- into `dst`: the staging root for make (= <key>-build), or the nested
  -- <key>-build/ under the staging root for install (whose root also holds
  -- the source mirror that installProgram lands post-swap).
  pid <- liftIO getCurrentPid
  let parent = takeDirectory swapTarget
      staging = swapTarget <> ".tmp." <> show pid
      dst = if isInstall then staging </> takeFileName buildDir else staging
  liftIO $ SD.createDirectoryIfMissing True parent
  liftIO $ removeDirIfExists staging
  liftIO $ SD.createDirectoryIfMissing True staging
  origDir <- liftIO SD.getCurrentDirectory

  ( do
      liftIO $ MT.writeFile (staging </> buildMarker) ""
      liftIO $ SD.createDirectoryIfMissing True dst
      liftIO $ SD.setCurrentDirectory dst
      buildAll (manifest : pools)
      liftIO $ SD.setCurrentDirectory origDir
      liftIO $ swapIn staging swapTarget
    ) `catchError` \e -> do
      liftIO $ SD.setCurrentDirectory origDir
      liftIO $ removeDirIfExists staging
      throwError e

  -- Launcher wrappers land at their absolute targets (the root: CWD for make,
  -- exe/<key> for install) and are made executable.
  liftIO $ mapM_ writeWrapper wrappers
  where
    -- Land every pool's source files on disk BEFORE running any make
    -- commands. If one pool's compile fails, the other pools' sources
    -- still exist on disk for inspection. Without this split, a make
    -- failure in an earlier pool aborts the mapM_ before later pools'
    -- files ever get written.
    buildAll ss = mapM_ writeScript ss >> mapM_ runMakes ss

-- | Atomically replace @dest@ with @staging@ (same parent, so the rename is
-- atomic). An existing @dest@ is removed only when it is a real directory
-- (not a symlink) carrying the 'buildMarker', so a stray or user-owned
-- directory is never destroyed.
swapIn :: FilePath -> FilePath -> IO ()
swapIn staging dest = do
  destExists <- SD.doesDirectoryExist dest
  when destExists $ do
    ok <- safeToDelete dest
    if ok
      then SD.removeDirectoryRecursive dest
      else ioError . userError $
        "Refusing to overwrite '" <> dest <> "': not a morloc build directory "
          <> "(missing " <> buildMarker <> " marker) or it is a symbolic link."
  SD.renameDirectory staging dest

-- | A directory may be deleted on rebuild only if it is a real directory
-- (not a symlink) and carries the 'buildMarker'.
safeToDelete :: FilePath -> IO Bool
safeToDelete dir = do
  isDir <- SD.doesDirectoryExist dir
  isSym <- SD.pathIsSymbolicLink dir
  hasMarker <- SD.doesFileExist (dir </> buildMarker)
  return (isDir && not isSym && hasMarker)

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists dir = do
  exists <- SD.doesDirectoryExist dir
  when exists $ SD.removeDirectoryRecursive dir

writeWrapper :: WrapperFile -> IO ()
writeWrapper (WrapperFile path body) = do
  -- A directory occupying the launcher path is the common recoverable
  -- mistake (the chosen output name collides with an existing directory);
  -- name the conflict instead of leaking a raw "is a directory" IOError.
  isDir <- SD.doesDirectoryExist path
  when isDir . ioError . userError $
    "Cannot write the launcher '" <> path <> "': a directory of that name already exists. "
      <> "Choose a different output name with --cli-out/-o, or remove the directory."
  SD.createDirectoryIfMissing True (takeDirectory path)
  MT.writeFile path body
  callProcess "chmod" ["755", path]

writeScript :: Script -> MorlocMonad ()
writeScript s = do
  (_ :/ tree) <- liftIO $ MS.writeDirectoryWith (\f c -> MT.writeFile f (unCode c)) (scriptCode s)
  case failures tree of
    [] -> return ()
    errs -> do
      msgs <- liftIO (mapM describeWriteFailure errs)
      MM.throwSystemError (vsep msgs)

runMakes :: Script -> MorlocMonad ()
runMakes s = mapM_ runSysCommand (scriptMake s)

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
