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
use and pins the rest, so a pool whose crates are all in the lock builds
offline; a pool that adds a crate resolves it once and its pins are merged
back, after which every later pool finds it pinned.

Cargo's lock format is a header, one @[[package]]@ table per crate, and
optionally trailing tables such as @[[patch.unused]]@, all separated by blank
lines. A crate legitimately appears more than once when several major
versions coexist (syn 2 and syn 3, say), so a lock is treated as a set of
tables keyed by name: a merge adds only the tables whose crate name the base
does not already carry. A pool table that refers to a version the merged lock
lacks is not a hazard: cargo treats it as stale and re-resolves that crate.
-}
module Morloc.Build.CargoLock
  ( lockCoversCrates
  , mergeCargoLocks
  , mergeLockFiles
  ) where

import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory, (<.>))

-- | A lock split into its header lines, its @[[package]]@ tables (crate name
-- paired with the table's lines) and every other table, in file order.
data Lock = Lock
  { lockHeader :: [Text]
  , lockPackages :: [(Text, [Text])]
  , lockTrailer :: [[Text]]
  }

-- | Does the lock pin every one of the given crates? A pinned crate is a
-- @[[package]]@ table; a crate that appears only inside another package's
-- @dependencies@ list is not pinned.
lockCoversCrates :: Text -> [Text] -> Bool
lockCoversCrates lockText crates = all (`Set.member` pinned) crates
  where
    pinned = Set.fromList (map fst (lockPackages (parseLock lockText)))

-- | Extend @base@ with every package table of @extra@ whose crate name @base@
-- lacks. The header and trailing tables are @base@'s; packages come out sorted
-- by name then version, as cargo writes them, so merging a lock into itself
-- is the identity.
mergeCargoLocks :: Text -> Text -> Text
mergeCargoLocks base extra =
  renderLock b {lockPackages = sortOn key (lockPackages b ++ added)}
  where
    b = parseLock base
    known = Set.fromList (map fst (lockPackages b))
    added = [p | p@(name, _) <- lockPackages (parseLock extra), not (Set.member name known)]
    key (name, ls) = (name, tableField "version" ls)

-- | @mergeLockFiles base env pool@ rewrites the environment lock @env@ as
-- @base@ extended by the crates already in @env@ and then by those in the
-- freshly built pool's lock. Re-basing on @base@ each time means an upgraded
-- runtime lock takes effect at once and the environment lock never keeps a
-- pin the runtime has moved past. The file is replaced by rename, so a
-- concurrent build reads either the old or the new lock, never a torn one;
-- two builds merging at once may lose one's additions, which costs that
-- crate one more online resolution and nothing else.
mergeLockFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeLockFiles basePath envPath poolPath = do
  base <- TIO.readFile basePath
  env <- readIfPresent envPath
  pool <- readIfPresent poolPath
  let merged = mergeCargoLocks (mergeCargoLocks base env) pool
      tmp = envPath <.> "tmp"
  createDirectoryIfMissing True (takeDirectory envPath)
  TIO.writeFile tmp merged
  renameFile tmp envPath
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
