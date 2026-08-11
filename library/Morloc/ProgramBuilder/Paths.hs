{- |
Module      : Morloc.ProgramBuilder.Paths
Description : Single source of truth for the build-directory layout
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Both @morloc make@ and @morloc install@ produce the same layout:

>  <root>/<key>-build/{manifest.json, pools/<lang>/pool.*}

with the program's sourced files living at @<root>/@ (a pool's own directory
is therefore always three levels below the source root). For @make@ the root
is the working directory; for @install@ it is @exe/<key>@, a mirror of the
working directory. This module is the ONE place that knows the @-build@ suffix,
the build-dir marker, where a program's @manifest.json@ sits, and how a
@datafile path resolves, so build-time formation and every consumer (discovery,
datafile resolution) cannot drift.
-}
module Morloc.ProgramBuilder.Paths
  ( buildDirSuffix
  , buildDirName
  , buildMarker
  , installedManifestPath
  , resolveDatafileAgainstRoot
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName, (</>))

-- | Suffix appended to a program key to name its build directory.
buildDirSuffix :: String
buildDirSuffix = "-build"

-- | The build-directory basename for a program key (e.g. @foo@ -> @foo-build@).
buildDirName :: String -> String
buildDirName key = key <> buildDirSuffix

-- | Marker file written at the root of every morloc build directory (make's
-- @<key>-build@, install's @exe/<key>@). Its presence gates deletion on
-- rebuild and identifies stale build dirs to skip when mirroring sources.
buildMarker :: FilePath
buildMarker = ".morloc-build"

-- | Given a program's root directory @exe/<name>@, the path to its
-- @manifest.json@ inside the nested @<name>-build/@ tree. Used by every
-- discovery site so they agree with build-time formation.
installedManifestPath :: FilePath -> FilePath
installedManifestPath progDir =
  progDir </> buildDirName (takeFileName progDir) </> "manifest.json"

-- | Resolve a @datafile relative path against the source ROOT (where data is
-- mirrored beside sources). Shared by the nexus- and pool-side codegen so both
-- resolve identically; a @Nothing@ root passes the path through unchanged.
resolveDatafileAgainstRoot :: Maybe FilePath -> Text -> Text
resolveDatafileAgainstRoot mRoot rel =
  maybe rel (\d -> T.pack (d </> T.unpack rel)) mRoot
