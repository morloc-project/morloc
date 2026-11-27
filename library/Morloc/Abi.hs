{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Abi
Description : The morloc ABI / wire-format contract version
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The ABI/wire-format version is distinct from the release version
('Morloc.Version.versionStr'): it identifies the C ABI declared in
@runtime/include/morloc.h@ and the wire packet format. The single source of
truth is the @#define MORLOC_ABI_VERSION@ line in @morloc.h@; the Rust runtime
reads it from the same header at build time
(@morloc_runtime_types::MORLOC_ABI_VERSION@) and this module parses it from
the embedded header.

@morloc init@ rebuilds a runtime whose @morloc-nexus --abi-version@ is not
'abiVersion', and fails if the rebuilt one still reports another. Whether a built program may run on an installed runtime is
decided by the morloc version it was built with (ART-5), not by this number.
-}
module Morloc.Abi
  ( abiVersion
  , headerAbiVersion
  , headerDefine
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Morloc.DataFiles as DF
import Text.Read (readMaybe)

-- | The ABI/wire-format contract version this compiler expects, parsed from the
-- @#define MORLOC_ABI_VERSION@ line of the embedded @morloc.h@. Errors only if
-- the header is malformed, which also fails the Rust build.
abiVersion :: Int
abiVersion =
  case headerAbiVersion DF.libmorlocHeader of
    Just n -> n
    Nothing -> error "morloc.h does not define MORLOC_ABI_VERSION as an integer"

-- | The @MORLOC_ABI_VERSION@ a @morloc.h@ declares, if it declares one.
headerAbiVersion :: T.Text -> Maybe Int
headerAbiVersion = headerDefine "MORLOC_ABI_VERSION"

-- | The integer a @morloc.h@ @#define@s under a name, if it defines one.
headerDefine :: T.Text -> T.Text -> Maybe Int
headerDefine name = listToMaybe . mapMaybe parseDefine . T.lines
  where
    parseDefine line =
      case T.words (T.strip line) of
        ("#define" : n : val : _) | n == name -> readMaybe (T.unpack val)
        _ -> Nothing
