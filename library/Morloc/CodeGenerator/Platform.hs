{- |
Module      : Morloc.CodeGenerator.Platform
Description : Host-platform build differences (ELF/Linux vs Mach-O/macOS)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Native morloc builds target the host (there is no cross-compilation), so the
platform here is the host OS. This module centralizes the ELF-vs-Mach-O
differences the build layer cares about -- shared-library naming and the
dynamic-loader "origin" token -- so the build steps consult one authority
rather than sprouting scattered per-site conditionals.
-}
module Morloc.CodeGenerator.Platform
  ( Platform(..)
  , hostPlatform
  , sharedLibExt
  , sharedLibName
  , rpathOrigin
  ) where

import qualified System.Info as SI

-- | The platform morloc is building for (= the host, since native builds are
-- not cross-compiled).
data Platform = Linux | Darwin
  deriving (Eq, Show)

-- | The current host platform. Anything that is not macOS is treated as Linux
-- (the only two native backends morloc targets; other hosts route to a
-- container upstream of code generation).
hostPlatform :: Platform
hostPlatform = case SI.os of
  "darwin" -> Darwin
  _        -> Linux

-- | Shared-library filename extension: @so@ on ELF, @dylib@ on Mach-O.
sharedLibExt :: Platform -> String
sharedLibExt Darwin = "dylib"
sharedLibExt Linux  = "so"

-- | Canonical shared-library filename for a base name, e.g.
-- @sharedLibName Darwin "morloc" == "libmorloc.dylib"@.
sharedLibName :: Platform -> String -> String
sharedLibName p base = "lib" <> base <> "." <> sharedLibExt p

-- | The dynamic loader's "origin" token for a loader-relative rpath:
-- @$ORIGIN@ on ELF, @\@loader_path@ on Mach-O.
rpathOrigin :: Platform -> String
rpathOrigin Darwin = "@loader_path"
rpathOrigin Linux  = "$ORIGIN"
