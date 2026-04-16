{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.ProgramBuilder.Install
Description : Install compiled morloc programs system-wide
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.ProgramBuilder.Install
  ( installProgram
  , validateIncludeScope
  , validateIncludeCoverage
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.Completion as Completion
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getPermissions
  , listDirectory
  , removeFile
  , setOwnerExecutable
  , setPermissions
  )
import System.Environment (lookupEnv)
import System.FilePath
  ( isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )
import System.IO (hPutStrLn, stderr)

-- | Finalize an installed program. The build step has already written pools
-- and the wrapper script directly into installDir. This function copies the
-- wrapper to bin/, extracts the manifest to fdb/, and copies include files.
installProgram ::
  -- | configHome (e.g. ~/.local/share/morloc)
  String ->
  -- | installDir (e.g. <configHome>/exe/<name>)
  String ->
  -- | installName
  String ->
  -- | include patterns
  [Text] ->
  -- | force overwrite
  Bool ->
  IO ()
installProgram configHome installDir installName includes force = do
  let binDir = configHome </> "bin"
      binPath = binDir </> installName
      installedWrapper = installDir </> installName

  -- Check for existing bin entry (installDir is already populated by build)
  binExists <- doesFileExist binPath
  when (binExists && not force) $
    throwIO . userError $ "'" <> installName <> "' is already installed. Use --force to overwrite."
  when (binExists && force) $
    removeFile binPath

  -- Copy include-matched files from CWD to installDir
  mapM_ (\pat -> copyIncludePattern (T.unpack pat) "." installDir) includes

  -- Copy wrapper from installDir to bin/
  createDirectoryIfMissing True binDir
  copyFile installedWrapper binPath
  makeExecutable binPath

  -- Copy manifest to fdb/ for daemon discovery
  let fdbDir = configHome </> "fdb"
      fdbPath = fdbDir </> (installName ++ ".manifest")
  createDirectoryIfMissing True fdbDir
  extractAndWriteManifest binPath fdbPath

  -- Check if bin dir is on PATH and print hint if not
  pathEnv <- lookupEnv "PATH"
  let pathStr = fromMaybe "" pathEnv
  when (not (binDir `isInfixOf` pathStr)) $
    hPutStrLn stderr $ "Note: add " <> binDir <> " to your PATH"

  hPutStrLn stderr $ "Installed '" <> installName <> "' to " <> binPath

  -- Regenerate shell completions
  Completion.regenerateCompletions False configHome

-- | Recursively copy a directory
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  mapM_
    ( \entry -> do
        let srcPath = src </> entry
            dstPath = dst </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir
          then copyDirectoryRecursive srcPath dstPath
          else copyFile srcPath dstPath
    )
    entries

{- | Copy files matching an include pattern, preserving relative paths.
Trailing "/" means copy a directory recursively.
"*" in a pattern means glob match.
Otherwise treated as an exact file/directory path.
-}
copyIncludePattern :: String -> FilePath -> FilePath -> IO ()
copyIncludePattern pattern srcRoot dstRoot
  | "/" `isSuffixOf` pattern = do
      let dirName = init pattern
          srcDir = srcRoot </> dirName
      exists <- doesDirectoryExist srcDir
      if exists
        then copyDirectoryRecursive srcDir (dstRoot </> dirName)
        else return ()
  | '*' `elem` pattern = do
      files <- listDirectoryRecursive srcRoot
      let matching = filter (matchGlob pattern . makeRelative srcRoot) files
      mapM_
        ( \f -> do
            let rel = makeRelative srcRoot f
                dst = dstRoot </> rel
            createDirectoryIfMissing True (takeDirectory dst)
            copyFile f dst
        )
        matching
  | otherwise = do
      let srcPath = srcRoot </> pattern
      isFile <- doesFileExist srcPath
      isDir <- doesDirectoryExist srcPath
      if isFile
        then do
          let dst = dstRoot </> pattern
          createDirectoryIfMissing True (takeDirectory dst)
          copyFile srcPath dst
        else
          if isDir
            then
              copyDirectoryRecursive srcPath (dstRoot </> pattern)
            else
              return ()

-- | Recursively list all files in a directory
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
  entries <- listDirectory dir
  paths <-
    mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listDirectoryRecursive path
            else return [path]
      )
      entries
  return (concat paths)

{- | Simple glob pattern matching supporting * (any sequence within a segment)
and ** (any path segments). Matches against relative paths.
-}
matchGlob :: String -> FilePath -> Bool
matchGlob [] [] = True
matchGlob ('*' : '*' : '/' : rest) path =
  matchGlob rest path || case break (== '/') path of
    (_, '/' : remaining) -> matchGlob ('*' : '*' : '/' : rest) remaining
    _ -> False
matchGlob ('*' : rest) path = any (\i -> matchGlob rest (drop i path)) [0 .. length segment]
  where
    segment = takeWhile (/= '/') path
matchGlob (p : rest) (c : cs) | p == c = matchGlob rest cs
matchGlob _ _ = False

-- | Make a file executable
makeExecutable :: FilePath -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (setOwnerExecutable True p)

{- | Reject include patterns that escape the package root.

An include path is only valid if it resolves to a location inside the
directory that contains package.yaml (and the main .loc script). Absolute
paths are rejected outright; relative paths are rejected if normalising
them produces a leading @..@ segment.

This closes the obvious footgun of a package referencing files outside the
project directory, which would break reproducibility and make installs
depend on ambient filesystem layout.
-}
validateIncludeScope :: [Text] -> IO ()
validateIncludeScope patterns =
  case filter (not . inScope . T.unpack) patterns of
    [] -> return ()
    bad ->
      throwIO . userError $
        "Invalid `include` in package.yaml: the following entries escape the "
          <> "package directory (absolute paths and `..` are not allowed):\n"
          <> unlines (map (("  " <>) . T.unpack) bad)
  where
    inScope :: String -> Bool
    inScope pat
      | isAbsolute pat = False
      | otherwise =
          case splitDirectories (normalise pat) of
            (".." : _) -> False
            segs -> not (any (== "..") segs)

{- | Verify that every directly-sourced file in the compiled program is
covered by at least one include pattern.

This catches the common mistake of writing @source Py from "helper.py"@ in
a .loc file but forgetting to add @helper.py@ to @package.yaml@'s @include@
field. Without this check, @morloc make --install@ silently produces a
broken install: the pool cannot import @helper@ at runtime.

Note: we only check /directly/ sourced files, not files that those sources
in turn import (e.g., one R file calling @source()@ on another). Transitive
dependencies cannot be discovered without executing the source language.
The user is still responsible for listing every runtime asset in
@include@; this validator is a targeted safety net for the direct case.

Paths that resolve outside the package root are skipped: those come from
library modules (e.g., root-py), not the user's project, and are not the
user's responsibility to declare.
-}
validateIncludeCoverage ::
  -- | package root (directory containing the main .loc script / package.yaml)
  FilePath ->
  -- | include patterns as written in package.yaml
  [Text] ->
  -- | absolute filesystem paths of every directly-sourced file
  [FilePath] ->
  IO ()
validateIncludeCoverage packageRoot patterns sourcePaths = do
  let patStrs = map T.unpack patterns
      relPaths = [ rel
                 | p <- sourcePaths
                 , let rel = makeRelative packageRoot (normalise p)
                 , not (isAbsolute rel)
                 , not ("../" `isPrefixOf` rel)
                 , rel /= ".."
                 ]
      uncovered = filter (not . isCovered patStrs) relPaths
  case uncovered of
    [] -> return ()
    missing ->
      throwIO . userError $
        "The following source files are referenced from your morloc program "
          <> "but not listed in `include` in package.yaml:\n"
          <> unlines (map ("  " <>) missing)
          <> "\nAdd them to `include` so they are copied into the install:\n"
          <> "  include:\n"
          <> unlines (map (("    - " <>)) missing)

-- | True if any include pattern matches the given relative path.
isCovered :: [String] -> FilePath -> Bool
isCovered patterns relPath = any (coversOne relPath) patterns
  where
    coversOne :: FilePath -> String -> Bool
    coversOne rel pat
      -- `src/` matches anything inside the src/ directory
      | "/" `isSuffixOf` pat =
          let dir = init pat
          in (dir ++ "/") `isPrefixOf` rel
      -- glob pattern
      | '*' `elem` pat = matchGlob pat rel
      -- exact path
      | otherwise = pat == rel

{- | Extract the manifest JSON from a wrapper script and write it to a file.
The wrapper script has the format:
  #!/bin/sh
  exec morloc-nexus "$0" "$@"
  ### MANIFEST ###
  <json>
-}
extractAndWriteManifest :: FilePath -> FilePath -> IO ()
extractAndWriteManifest wrapperPath manifestPath = do
  contents <- readFile wrapperPath
  let marker = "### MANIFEST ###"
      afterMarker = drop 1 $ dropWhile (/= marker) (lines contents)
  case afterMarker of
    [] -> return () -- no manifest found, skip silently
    _ -> writeFile manifestPath (unlines afterMarker)
