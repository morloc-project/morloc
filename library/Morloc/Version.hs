{- |
Module      : Morloc.Version
Description : The morloc version and the cross-repo contract schema versions
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The schema versions the compiler emits across a process boundary to the
environment manager (mim) are defined HERE, once, and referenced everywhere else,
so a version can never drift between its definition and its use. The manager holds
the matching values in @morloc-deps@ (its @version@ module); the conformance test
guards the two sides against drift.
-}
module Morloc.Version
  ( versionStr
  , envspecVersion
  , langSupportSchemaVersion
  ) where

import Data.Version (showVersion)
import qualified Paths_morloc (version)
import qualified Morloc.Data.Text as MT

versionStr :: String
versionStr = showVersion Paths_morloc.version

-- | @envspec.json@ @envspec_version@ (integer): the single schema version the
-- compiler emits and the manager accepts. v2 adds the @local@ package source
-- (local filesystem-path dependencies) and makes @source@ a discriminated tag.
envspecVersion :: Int
envspecVersion = 2

-- | @morloc lang-support@ @schema_version@ (semver "MAJOR.MINOR"). A new language
-- or field is a MINOR bump; a breaking change is a MAJOR bump.
langSupportSchemaVersion :: MT.Text
langSupportSchemaVersion = MT.pack "1.0"
