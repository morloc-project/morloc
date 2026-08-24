{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Abi
Description : The morloc ABI / wire-format contract version
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The ABI/wire-format version is distinct from the release version
('Morloc.Version.versionStr'): it identifies the C ABI declared in
@data/morloc/morloc.h@ and the wire packet format, and is bumped only when
those change. The single source of truth is the @#define MORLOC_ABI_VERSION@
line in @morloc.h@; the Rust runtime mirrors it
(@morloc_runtime_types::MORLOC_ABI_VERSION@, kept in lockstep by a test) and
this module parses it from the embedded header.

A prebuilt @libmorloc.so@ reports its version via the @morloc_abi_version()@ C
function and @morloc-nexus@ via @morloc-nexus --abi-version@. Provisioning
compares those against 'abiVersion' and refuses a mismatched binary rather than
running it, preventing silent cross-pool struct/offset corruption from mixing
components built against different ABIs.
-}
module Morloc.Abi
  ( abiVersion
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Morloc.DataFiles as DF
import Text.Read (readMaybe)

-- | The ABI/wire-format contract version this compiler expects, parsed from the
-- @#define MORLOC_ABI_VERSION@ line of the embedded @morloc.h@. Errors only if
-- the header is malformed (an invariant the Rust @abi_version_matches_header@
-- test also guards).
abiVersion :: Int
abiVersion =
  case listToMaybe (mapMaybe parseDefine (T.lines DF.libmorlocHeader)) of
    Just n -> n
    Nothing -> error "morloc.h does not define MORLOC_ABI_VERSION as an integer"
  where
    parseDefine line =
      case T.words (T.strip line) of
        ("#define" : "MORLOC_ABI_VERSION" : val : _) -> readMaybe (T.unpack val)
        _ -> Nothing
