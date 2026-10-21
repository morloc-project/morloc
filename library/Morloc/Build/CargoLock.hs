{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Build.CargoLock
Description : The environment's Cargo.lock: coverage checks and merging
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A Rust pool is a Cargo project whose dependencies should resolve the same way
every time in one environment, so every pool starts from one lock: the
workspace lock persisted with the runtime (the authority for rustmorloc and
everything it pulls in) extended by the crates earlier pools in this
environment resolved from @rust-deps@. Cargo prunes the entries a pool does not
use and pins the rest, so a pool whose crates are all in the lock needs no
network; a pool that adds a crate resolves it once and its pins are merged
back, after which every later pool finds it pinned -- unless the crate was
resolved against a newer version of one the base pins, in which case it is
not merged back and each pool resolves it again.

Cargo's lock format is a header, one @[[package]]@ table per crate, and
optionally trailing tables such as @[[patch.unused]]@, all separated by blank
lines. A crate legitimately appears more than once when several major
versions coexist (syn 2 and syn 3, say), so a lock is treated as a set of
tables keyed by name: a merge adds only the tables whose crate name the base
does not already carry. An added table is kept only while each of its
dependencies names the same package in the merged lock as in the lock it
came from: a pool may have bumped a crate the base pins, and cargo refuses a
lock whose tables were resolved against a version it no longer holds.
-}
module Morloc.Build.CargoLock
  ( mergeCargoLocks
  , mergeLockFiles
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (onException)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openTempFileWithDefaultPermissions)

-- | A lock split into its header lines, its @[[package]]@ tables (crate name
-- paired with the table's lines) and every other table, in file order.
data Lock = Lock
  { lockHeader :: [Text]
  , lockPackages :: [(Text, [Text])]
  , lockTrailer :: [[Text]]
  }

-- | Extend @base@ with the package tables of @extra@ whose crate name @base@
-- lacks and whose dependencies still name the same packages in the result.
-- The header and trailing tables are @base@'s; packages come out sorted by
-- name then version, as cargo writes them, so merging a lock into itself is
-- the identity.
mergeCargoLocks :: Text -> Text -> Text
mergeCargoLocks base extra =
  renderLock b {lockPackages = sortOn key (lockPackages b ++ consistent candidates)}
  where
    b = parseLock base
    e = lockPackages (parseLock extra)
    known = Set.fromList (map fst (lockPackages b))
    candidates = [p | p@(name, _) <- e, not (Set.member name known)]
    key (name, ls) = (name, tableField "version" ls)
    -- Drop the tables whose dependencies no longer name what they named in
    -- @extra@, until none is dropped: a dropped table strands its dependents.
    consistent kept =
      let merged = lockPackages b ++ kept
          kept' = [p | p <- kept, keeps merged p]
       in if length kept' == length kept then kept else consistent kept'
    resolved = [(p, (\ds -> [(d, packageId e d) | d <- ds]) <$> tableDependencies ls) | p@(_, ls) <- candidates]
    keeps merged p = case lookup p resolved of
      Just (Just deps) -> all (\(d, i) -> isJust i && i == packageId merged d) deps
      _ -> False

-- | The (name, version, source) a lock's dependency entry names: a bare
-- name is the one table of that name, @name version@ the one of that
-- version, @name version (source)@ the one from that source. Nothing when
-- no single table matches.
packageId :: [(Text, [Text])] -> Text -> Maybe (Text, Text, Text)
packageId pkgs entry = case [ident t | t@(n, _) <- pkgs, n == name, matches t] of
  [i] -> Just i
  _ -> Nothing
  where
    (name, rest) = T.breakOn " " entry
    (ver, src) = T.breakOn " " (T.strip rest)
    source = withoutPrecise (fromMaybe (T.strip src) (T.stripPrefix "(" (T.strip src) >>= T.stripSuffix ")"))
    -- A table's git source carries the resolved commit after '#'; a
    -- dependency entry names the source without it.
    withoutPrecise = fst . T.breakOn "#"
    ident (n, ls) = (n, tableField "version" ls, withoutPrecise (tableField "source" ls))
    matches t@(_, ls) =
      (T.null ver || tableField "version" ls == ver)
        && (T.null source || let (_, _, s') = ident t in s' == source)

-- | The entries of a table's @dependencies@ array, as cargo writes it (one
-- quoted entry per line); Nothing when the array is written another way.
tableDependencies :: [Text] -> Maybe [Text]
tableDependencies ls = case break (T.isPrefixOf "dependencies") (map T.strip ls) of
  (_, []) -> Just []
  (_, "dependencies = [" : body) -> entries body
  _ -> Nothing
  where
    entries ("]" : _) = Just []
    entries (l : more) = do
      inner <- T.stripPrefix "\"" l >>= T.stripSuffix "\","
      (inner :) <$> entries more
    entries [] = Nothing

-- | @mergeLockFiles base env pool@ rewrites the environment lock @env@ as
-- @base@ extended by the crates already in @env@ and then by those in the
-- freshly built pool's lock. Re-basing on @base@ each time means an upgraded
-- runtime lock takes effect at once and the environment lock never keeps a
-- pin the runtime has moved past. Each merge stages its own file and renames
-- it into place, so a concurrent build reads either the old or the new lock,
-- never a torn one; two builds merging at once may lose one's additions,
-- which costs that crate one more online resolution and nothing else.
mergeLockFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeLockFiles basePath envPath poolPath = do
  base <- TIO.readFile basePath
  env <- readIfPresent envPath
  pool <- readIfPresent poolPath
  let merged = mergeCargoLocks (mergeCargoLocks base env) pool
  createDirectoryIfMissing True (takeDirectory envPath)
  (tmp, h) <- openTempFileWithDefaultPermissions (takeDirectory envPath) (takeFileName envPath <> ".tmp")
  (TIO.hPutStr h merged >> hClose h) `onException` (hClose h >> removeFile tmp)
  renameFile tmp envPath `onException` removeFile tmp
  where
    readIfPresent p = do
      exists <- doesFileExist p
      if exists then TIO.readFile p else return ""

parseLock :: Text -> Lock
parseLock txt = Lock (dropTrailingBlanks header) packages others
  where
    (header, rest) = break isTableStart (T.lines txt)
    tables = map dropTrailingBlanks (splitTables rest)
    packages = [(tableField "name" t, t) | t <- tables, take 1 t == ["[[package]]"]]
    others = [t | t <- tables, take 1 t /= ["[[package]]"]]

-- | Cut the lines following the header at every table start.
splitTables :: [Text] -> [[Text]]
splitTables [] = []
splitTables (l : ls) = (l : body) : splitTables rest
  where
    (body, rest) = break isTableStart ls

isTableStart :: Text -> Bool
isTableStart = T.isPrefixOf "[["

dropTrailingBlanks :: [Text] -> [Text]
dropTrailingBlanks = reverse . dropWhile (T.null . T.strip) . reverse

-- | The value of a @key = "value"@ line in a table; empty when absent.
tableField :: Text -> [Text] -> Text
tableField key ls =
  case [v | l <- ls, Just rest <- [T.stripPrefix (key <> " = \"") (T.strip l)], Just v <- [T.stripSuffix "\"" rest]] of
    (v : _) -> v
    [] -> ""

-- | Tables separated by one blank line, a single newline at the end.
renderLock :: Lock -> Text
renderLock lock =
  T.intercalate "\n" (map T.unlines (headerPart ++ map snd (lockPackages lock) ++ lockTrailer lock))
  where
    headerPart = [lockHeader lock | not (null (lockHeader lock))]
