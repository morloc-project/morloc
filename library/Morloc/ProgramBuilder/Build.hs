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
  , BuildDirs (..)
  , resolveBuildDirs
  , ensureStagingDir
  , stagingPoolsDir
  , discardStagingDir
  , withStagingCleanup
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (catchError, throwError)
import qualified Data.Map as Map
import qualified Morloc.Config as MC
import Morloc.Data.Doc ((<+>), line, vsep, pretty)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Build.CargoLock as CL
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.ProgramBuilder.Paths (buildDirName, buildMarker)
import qualified Morloc.System as MS
import qualified System.Directory as SD
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.IO.Error (ioeGetFileName)
import System.Process (CreateProcess (env), callProcess, createProcess, getCurrentPid, proc, waitForProcess)

-- | The build layout, resolved once by 'resolveBuildDirs'.
data BuildDirs = BuildDirs
  { bdKey :: String
  -- ^ directory identity: the launcher name and the @<key>@ of @<key>-build@
  , bdRoot :: Path
  -- ^ the source/install ROOT (see 'stateBuildRoot')
  , bdBuildDir :: Path
  -- ^ @root </> <key>-build@ (see 'stateInstallDir')
  }

-- | Resolve the build layout, caching it in 'stateBuildRoot' /
-- 'stateInstallDir' so every consumer (the nexus manifest, the staging tree,
-- guest-language artifacts) agrees on one location.
--
-- Directory identity: for @make@ it is the build key (--name / -o / source
-- basename), so several sources built in one working directory get distinct
-- <key>-build dirs. For a real install it is the MODULE name: the source file
-- (conventionally main.loc) is not the program's identity, so exe/<module>,
-- its nested <module>-build, and the bin launcher share that one name. Eval is
-- the exception: its module is a synthetic "main" (an anonymous expression has
-- no declaration), so its identity is the build key -- the --save name, else
-- the ephemeral "eval" -- NOT the module, or every --save would collide on
-- exe/main. Both modes nest the build dir one level below its root, so a
-- pool's sources are always at ../../.. .
--
-- The root is the source/install ROOT: exe/<module> for install (a mirror of
-- the working directory), the working directory (or --build-dir) for make.
resolveBuildDirs :: MorlocMonad BuildDirs
resolveBuildDirs = do
  st <- MM.get
  config <- MM.ask
  programName <- MM.getModuleName
  programKey <- MM.getProgramKey
  let dirKey = if stateInstall st && not (stateEvalMode st) then programName else programKey
  case (stateBuildRoot st, stateInstallDir st) of
    (Just root, Just buildDir) -> return (BuildDirs dirKey root buildDir)
    _ -> do
      root <-
        if stateInstall st
          then return (MC.exeDir config </> dirKey)
          else do
            cwd <- liftIO SD.getCurrentDirectory
            liftIO $ SD.makeAbsolute (fromMaybe cwd (stateBuildParentDir st))
      let buildDir = root </> buildDirName dirKey
      MM.modify (\s -> s {stateInstallDir = Just buildDir, stateBuildRoot = Just root})
      return (BuildDirs dirKey root buildDir)

-- | The unit 'buildProgram' atomically swaps into place. For install it is the
-- ROOT (exe/<key>): swapping it wholesale means a --force reinstall replaces
-- stale mirrored sources too, not just the nested build. @make@ owns only
-- <key>-build and must never swap the working directory.
swapTargetOf :: MorlocMonad Path
swapTargetOf = do
  dirs <- resolveBuildDirs
  isInstall <- MM.gets stateInstall
  return (if isInstall then bdRoot dirs else bdBuildDir dirs)

-- | The staging directory the build assembles in, created (with its marker) on
-- first call and cached in 'stateStagingDir'. Returns @(staging, dst)@: the
-- staging root, and the directory inside it where the manifest + pools tree
-- lands -- the staging root itself for make (= <key>-build), or the nested
-- <key>-build/ under it for install (whose root also holds the source mirror
-- that installProgram lands post-swap). Staging is a sibling of the swap
-- target (same parent) so the final rename is atomic; a crash mid-build leaves
-- the previous good build untouched.
--
-- A pass that runs before 'buildProgram' but must land artifacts in the build
-- tree (a guest language's compiled objects) calls this and writes under
-- @dst@; the build then finds the directory already prepared.
ensureStagingDir :: MorlocMonad (Path, Path)
ensureStagingDir = do
  swapTarget <- swapTargetOf
  mStaging <- MM.gets stateStagingDir
  staging <- case mStaging of
    Just s -> return s
    Nothing -> do
      pid <- liftIO getCurrentPid
      let staging = swapTarget <> ".tmp." <> show pid
      liftIO $ SD.createDirectoryIfMissing True (takeDirectory swapTarget)
      liftIO $ removeDirIfExists staging
      liftIO $ SD.createDirectoryIfMissing True staging
      liftIO $ MT.writeFile (staging </> buildMarker) ""
      MM.modify (\s -> s {stateStagingDir = Just staging})
      return staging
  dst <- stagingDst staging
  liftIO $ SD.createDirectoryIfMissing True dst
  return (staging, dst)

-- | Where the manifest + pools tree lands inside a staging root.
stagingDst :: Path -> MorlocMonad Path
stagingDst staging = do
  buildDir <- bdBuildDir <$> resolveBuildDirs
  isInstall <- MM.gets stateInstall
  return (if isInstall then staging </> takeFileName buildDir else staging)

-- | The @pools/@ directory of the staging tree, if one has been created.
-- Reads state only: nothing is created.
stagingPoolsDir :: MorlocMonad (Maybe Path)
stagingPoolsDir = do
  mStaging <- MM.gets stateStagingDir
  case mStaging of
    Nothing -> return Nothing
    Just staging -> Just . (</> "pools") <$> stagingDst staging

-- | Remove the staging directory, if one was created, and forget it. Called
-- on every failure path so an aborted build never leaves a @.tmp.<pid>@ tree
-- beside the program.
discardStagingDir :: MorlocMonad ()
discardStagingDir = do
  mStaging <- MM.gets stateStagingDir
  case mStaging of
    Nothing -> return ()
    Just staging -> do
      liftIO $ removeDirIfExists staging
      MM.modify (\s -> s {stateStagingDir = Nothing})

-- | Run an action, discarding the staging directory if it fails.
withStagingCleanup :: MorlocMonad a -> MorlocMonad a
withStagingCleanup action = action `catchError` \e -> discardStagingDir >> throwError e

buildProgram :: (Script, [WrapperFile], [Script]) -> MorlocMonad ()
buildProgram (manifest, wrappers, pools) = do
  isInstall <- MM.gets stateInstall
  force <- MM.gets stateInstallForce
  swapTarget <- swapTargetOf
  origDir <- liftIO SD.getCurrentDirectory

  -- Resolve the project root while the working directory is still the one it was
  -- parsed relative to. 'stateProjectRoot' is the entry file's directory as typed,
  -- so `morloc make main.loc` makes it "."; resolved after the chdir below it would
  -- name the staging directory instead of the project.
  mProjectRoot <- MM.gets stateProjectRoot
  projectRoot <- liftIO $ SD.makeAbsolute (fromMaybe "." mProjectRoot)

  ( do
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
      (staging, dst) <- ensureStagingDir
      liftIO $ SD.setCurrentDirectory dst
      buildAll projectRoot (manifest : pools)
      liftIO $ SD.setCurrentDirectory origDir
      liftIO $ swapIn staging swapTarget
      MM.modify (\s -> s {stateStagingDir = Nothing})
    ) `catchError` \e -> do
      liftIO $ SD.setCurrentDirectory origDir
      discardStagingDir
      throwError e

  -- Launcher wrappers land at their absolute targets (the root: CWD for make,
  -- exe/<key> for install) and are made executable.
  liftIO $ mapM_ writeWrapper wrappers
  where
    -- Land every pool's source files on disk BEFORE running any make
    -- commands. If one pool's compile fails, the other pools' sources
    -- still exist on disk for inspection. Without this split, a make
    -- failure in an earlier pool aborts the mapM_ before later pools'
    -- files ever get written. Between the two, provision the program's
    -- declared dependencies (a no-op outside a managed environment), so the
    -- pool compiles find the required headers/libraries/interpreters.
    buildAll projectRoot ss = do
      mapM_ writeScript ss
      syncEnvDeps projectRoot
      mapM_ runMakes ss

-- | Provision a program's declared package dependencies before its pools are
-- compiled, by invoking the environment's build hook: an external program named
-- by @MORLOC_BUILD_HOOK@ (the in-environment dependency agent, e.g. @mim-env@),
-- run as @<hook> sync --name <key> --spec envspec.json --root <projectRoot>@.
-- @projectRoot@ is the absolute directory the hook resolves local (filesystem
-- path) dependencies against; the caller resolves it before the build changes
-- directory, since this runs from inside the staging tree. Runs only when BUILDING
-- (not eval -- the manager provisions that) INSIDE a managed environment
-- (@MORLOC_ENV@ set) a program that declares dependencies or has a pool; every
-- other case is a silent no-op. CWD is the staging build dir, so the freshly
-- written @envspec.json@ is at a relative path.
--
-- An install build (@morloc make --install@, @morloc install --build@) syncs
-- too, with @--installed@ so the hook records the program as part of the
-- environment's installed baseline rather than as a transient scratch build.
-- When the manager drives the install it has already provisioned the module
-- closure, and the hook's cached solve is a near-no-op; when a user runs the
-- install directly inside the environment, this is the only provisioning step.
--
-- The compiler's own path is passed to the hook as @MORLOC_BIN@ so the hook's
-- reverse @morloc lang-support@ call resolves the exact driving compiler without
-- relying on PATH.
--
-- A solve failure (e.g. an unsatisfiable conflict) aborts the build with the
-- hook's message. A managed environment with no hook set is an ERROR: it
-- provably expects provisioning but has no provisioner, so the build fails now
-- with an actionable message rather than at pool compile on missing deps.
syncEnvDeps :: FilePath -> MorlocMonad ()
syncEnvDeps projectRoot = do
  metas <- MM.gets statePackageMeta
  mKey <- MM.gets stateProgramKey
  isEval <- MM.gets stateEvalMode
  isInstall <- MM.gets stateInstall
  usesLangs <- MM.gets stateEnvSpecLangs
  mEnv <- liftIO $ lookupEnv "MORLOC_ENV"
  let declaresDeps = any packageHasDeps metas
      -- A program that merely USES a language (has a pool) needs that language's
      -- toolchain/runtime provisioned on demand, even with no declared package
      -- deps; a program that declares deps needs them regardless. Either makes
      -- the build's provisioning hook fire. The hook is a near-no-op (its solve
      -- is cached) when the world is already up to date.
      needsSync = declaresDeps || not (null usesLangs)
  case (mEnv, mKey) of
    (Just envVal, Just key)
      | not (null envVal) && needsSync && not isEval ->
          runSync declaresDeps isInstall key projectRoot
    _ -> return ()
  where
    packageHasDeps pm =
      not (Map.null (packagePyDeps pm))
        || not (Map.null (packageRDeps pm))
        || not (Map.null (packageCppDeps pm))
        || not (Map.null (packageRustDeps pm))
        || not (Map.null (packageJuliaDeps pm))
        || not (Map.null (packageLocalDeps pm))

    -- @requireHook@ is @declaresDeps@: a program that DECLARES package
    -- dependencies cannot build without them, so a managed env with no
    -- provisioner is a hard error. A language-only trigger degrades to a warning
    -- instead -- the languages may already be provisioned (by `mim new`/init), so
    -- a missing hook must not brick a build that would otherwise succeed.
    runSync requireHook installed key root = do
      mhook <- liftIO $ lookupEnv "MORLOC_BUILD_HOOK"
      case mhook of
        Just hook | not (null hook) -> do
          MM.say $ "Provisioning environment dependencies (" <> pretty hook <> " sync)..."
          self <- liftIO getExecutablePath
          result <- liftIO (runHook hook self installed key root)
          case result of
            Left e ->
              MM.throwSystemError $ "could not run " <> pretty hook <> ": " <> pretty (show e)
            Right ExitSuccess -> return ()
            Right (ExitFailure _) ->
              MM.throwSystemError
                "environment dependency provisioning failed (see the output above)."
        -- MORLOC_ENV is set (a managed env that expects provisioning) but no build
        -- hook is named.
        _ | requireHook ->
              MM.throwSystemError . vsep $
                [ "This program declares package dependencies and MORLOC_ENV is set, but"
                , "MORLOC_BUILD_HOOK names no provisioning program. A managed environment"
                , "must export MORLOC_BUILD_HOOK (the dependency agent, e.g. mim) so"
                , "'morloc make' can provision declared dependencies before the pools are"
                , "compiled. Re-provision the environment (e.g. 'mim update --env <env>')."
                ]
          | otherwise ->
              MM.say . vsep $
                [ "Warning: MORLOC_ENV is set but MORLOC_BUILD_HOOK names no provisioning"
                , "program, so on-demand language provisioning is skipped. If a pool fails"
                , "to compile on a missing toolchain, re-provision the environment"
                , "(e.g. 'mim update --env <env>')."
                ]

    -- Inherit the terminal so the hook and the pixi it spawns stream progress
    -- live; capturing would silence a successful install. MORLOC_BIN carries THIS
    -- compiler's path so the hook's reverse `morloc lang-support` call resolves
    -- the driving compiler without relying on PATH. `try` keeps a spawn failure
    -- (e.g. a non-executable hook) inside the error monad.
    runHook :: FilePath -> FilePath -> Bool -> String -> FilePath -> IO (Either IOException ExitCode)
    runHook hook self installed key root = try $ do
      baseEnv <- getEnvironment
      let childEnv = ("MORLOC_BIN", self) : filter ((/= "MORLOC_BIN") . fst) baseEnv
          -- --root is the project root (the entry module's directory), against
          -- which local (filesystem-path) dependency paths are resolved.
          args =
            ["sync", "--name", key, "--spec", "envspec.json", "--root", root]
              ++ ["--installed" | installed]
          spec = (proc hook args) { env = Just childEnv }
      (_, _, _, ph) <- createProcess spec
      waitForProcess ph

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
runSysCommand (SysMergeCargoLock base envLock pool) = liftIO $ CL.mergeLockFiles base envLock pool
runSysCommand other =
  MM.throwSystemError $ "Unsupported SysCommand: " <> pretty (show other)
