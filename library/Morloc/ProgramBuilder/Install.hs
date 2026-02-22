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
  ) where

import Data.List (isInfixOf, isSuffixOf)
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
  , removeDirectoryRecursive
  , removeFile
  , setOwnerExecutable
  , setPermissions
  )
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath (makeRelative, takeDirectory, (</>))

-- | After a successful local build, copy artifacts to the install location.
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
      localWrapper = installName

  -- Check for existing install
  binExists <- doesFileExist binPath
  exeExists <- doesDirectoryExist installDir
  if (binExists || exeExists) && not force
    then die $ "'" <> installName <> "' is already installed. Use --force to overwrite."
    else do
      -- Remove old install if forcing
      if force
        then do
          if exeExists then removeDirectoryRecursive installDir else return ()
          if binExists then removeFile binPath else return ()
        else return ()

      -- Create install directory and copy pools
      createDirectoryIfMissing True installDir
      poolsExist <- doesDirectoryExist "pools"
      if poolsExist
        then copyDirectoryRecursive "pools" (installDir </> "pools")
        else return ()

      -- Copy include-matched files (preserving relative paths)
      mapM_ (\pat -> copyIncludePattern (T.unpack pat) "." installDir) includes

      -- Move wrapper to bin (already has correct build_dir from Nexus.generate)
      createDirectoryIfMissing True binDir
      copyFile localWrapper binPath
      makeExecutable binPath

      -- Copy manifest to fdb/ for daemon discovery
      let fdbDir = configHome </> "fdb"
          fdbPath = fdbDir </> (installName ++ ".manifest")
      createDirectoryIfMissing True fdbDir
      extractAndWriteManifest binPath fdbPath

      -- Clean up local build artifacts
      if poolsExist then removeDirectoryRecursive "pools" else return ()
      removeFile localWrapper

      -- Check if bin dir is on PATH and print hint if not
      pathEnv <- lookupEnv "PATH"
      let pathStr = maybe "" id pathEnv
      if not (binDir `isInfixOf` pathStr)
        then putStrLn $ "Note: add " <> binDir <> " to your PATH"
        else return ()

      putStrLn $ "Installed '" <> installName <> "' to " <> binPath

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
