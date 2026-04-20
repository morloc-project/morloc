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
  , copyAllFiltered
  , cleanIgnoredFiles
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM_, when, unless)
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
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv)
import System.Process (callProcess)
import System.FilePath
  ( isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , takeFileName
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
  -- | include patterns (Nothing = copy everything, Just [] = copy nothing)
  Maybe [Text] ->
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

  -- Copy files from CWD to installDir
  case includes of
    Nothing -> copyAllFiltered "." installDir
    Just pats -> mapM_ (\pat -> copyIncludePattern (T.unpack pat) "." installDir) pats

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

-- ======================================================================
-- Copy-everything mode (default)
-- ======================================================================

-- | Always-excluded patterns, applied even without a .morlocignore file.
defaultIgnorePatterns :: [String]
defaultIgnorePatterns =
  [ ".git/"
  , ".morlocignore"
  , "*.manifest"
  ]

-- | Copy all files from srcRoot to dstRoot, excluding files that match
-- .morlocignore patterns and always-excluded patterns. Preserves
-- relative directory structure.
copyAllFiltered :: FilePath -> FilePath -> IO ()
copyAllFiltered srcRoot dstRoot = do
  userPatterns <- readMorlocIgnore (srcRoot </> ".morlocignore")
  let allPatterns = defaultIgnorePatterns ++ userPatterns
  files <- listDirectoryRecursive srcRoot
  let relFiles = map (makeRelative srcRoot) files
      kept = filter (not . isIgnored allPatterns) relFiles
  forM_ kept $ \rel -> do
    let dst = dstRoot </> rel
    createDirectoryIfMissing True (takeDirectory dst)
    -- Skip if dst already exists (build artifacts placed by the compiler)
    dstExists <- doesFileExist dst
    unless dstExists $
      copyFile (srcRoot </> rel) dst

-- | Remove files matching .morlocignore and always-excluded patterns
-- from an already-populated directory. Used after git clone for module
-- install to clean up ignored files in-place.
cleanIgnoredFiles :: FilePath -> IO ()
cleanIgnoredFiles dir = do
  userPatterns <- readMorlocIgnore (dir </> ".morlocignore")
  let allPatterns = defaultIgnorePatterns ++ userPatterns
  files <- listDirectoryRecursive dir
  let relFiles = map (makeRelative dir) files
      ignored = filter (isIgnored allPatterns) relFiles
  forM_ ignored $ \rel ->
    removeFile (dir </> rel)
  -- Clean up empty directories left behind
  cleanEmptyDirs dir

-- | Remove empty directories recursively (bottom-up).
cleanEmptyDirs :: FilePath -> IO ()
cleanEmptyDirs dir = do
  entries <- listDirectory dir
  forM_ entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    when isDir $ do
      cleanEmptyDirs path
      subEntries <- listDirectory path
      when (null subEntries) $
        removeDirectoryRecursive path

-- ======================================================================
-- .morlocignore parsing
-- ======================================================================

-- | Read and parse a .morlocignore file. Returns empty list if the file
-- does not exist.
readMorlocIgnore :: FilePath -> IO [String]
readMorlocIgnore path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      return $ parseIgnorePatterns content
    else return []

-- | Parse .morlocignore content into a list of patterns.
-- Supports: blank lines, # comments, negation with !, trailing / for dirs.
parseIgnorePatterns :: String -> [String]
parseIgnorePatterns = filter (not . null) . map clean . lines
  where
    clean line =
      let trimmed = dropWhile (== ' ') line
      in if null trimmed || head trimmed == '#'
           then ""
           else trimmed

-- | Check if a relative path should be ignored based on ignore patterns.
-- Supports: glob patterns, directory patterns (trailing /), negation (!).
isIgnored :: [String] -> FilePath -> Bool
isIgnored patterns relPath = foldl apply False patterns
  where
    apply acc pat
      | "!" `isPrefixOf` pat =
          if matchIgnorePattern (drop 1 pat) relPath then False else acc
      | otherwise =
          if matchIgnorePattern pat relPath then True else acc

-- | Match a single ignore pattern against a relative path.
matchIgnorePattern :: String -> FilePath -> Bool
matchIgnorePattern pat relPath
  -- Directory pattern: "build/" matches any path under build/
  | "/" `isSuffixOf` pat =
      let dir = init pat
      in dir == relPath
           || (dir ++ "/") `isPrefixOf` relPath
           || ("/" ++ dir ++ "/") `isInfixOf` ("/" ++ relPath)
           || takeFileName (takeDirectory relPath) == dir
  -- Pattern contains / → match against full relative path
  | '/' `elem` pat = matchGlob pat relPath
  -- Pattern without / → match against filename only
  | otherwise = matchGlob pat (takeFileName relPath)

-- ======================================================================
-- Allowlist mode (explicit include patterns)
-- ======================================================================

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
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
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

-- ======================================================================
-- Validation
-- ======================================================================

-- | Make a file executable (0755 so group/other can execute too)
makeExecutable :: FilePath -> IO ()
makeExecutable path = callProcess "chmod" ["755", path]

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

Only runs in strict mode (when include patterns are explicitly specified).
In default mode (include everything), this check is skipped since all
files are copied.

Note: we only check /directly/ sourced files, not files that those sources
in turn import (e.g., one R file calling @source()@ on another). Transitive
dependencies cannot be discovered without executing the source language.
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

-- ======================================================================
-- Manifest extraction
-- ======================================================================

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
