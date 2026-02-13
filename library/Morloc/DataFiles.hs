{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Morloc.DataFiles
Description : Handle non-Haskell files such as foreign language sources files and configs
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.DataFiles
  ( EmbededFile (..)
  , libmorlocFiles
  , libmorlocHeader
  , nexusSource
  , poolTemplate
  , libcpplang
  , libcpplangImpl
  , libcpplangPch
  , libpylang
  , libpylangMakefile
  , libpylangSetup
  , librlang
  ) where

import Data.FileEmbed (embedFileRelative)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Morloc.Namespace.Prim (Lang(..))

data EmbededFile = EmbededFile
  { embededFileName :: String -- basename for the file
  , embededFileText :: Text -- full text the file contained at compile time
  }

-- C library split into modular source files
libmorlocFiles :: [EmbededFile]
libmorlocFiles =
  [ EmbededFile "morloc.h"     (decodeUtf8 $(embedFileRelative "data/morloc/morloc.h"))
  , EmbededFile "macros.h"     (decodeUtf8 $(embedFileRelative "data/morloc/macros.h"))
  , EmbededFile "memory.h"     (decodeUtf8 $(embedFileRelative "data/morloc/memory.h"))
  , EmbededFile "call.h"       (decodeUtf8 $(embedFileRelative "data/morloc/call.h"))
  , EmbededFile "packet.h"     (decodeUtf8 $(embedFileRelative "data/morloc/packet.h"))
  , EmbededFile "schema.h"     (decodeUtf8 $(embedFileRelative "data/morloc/schema.h"))
  , EmbededFile "eval.h"       (decodeUtf8 $(embedFileRelative "data/morloc/eval.h"))
  , EmbededFile "slurm.h"      (decodeUtf8 $(embedFileRelative "data/morloc/slurm.h"))
  , EmbededFile "utility.h"    (decodeUtf8 $(embedFileRelative "data/morloc/utility.h"))
  , EmbededFile "mpack.h"      (decodeUtf8 $(embedFileRelative "data/morloc/mpack.h"))
  , EmbededFile "json.h"       (decodeUtf8 $(embedFileRelative "data/morloc/json.h"))
  , EmbededFile "cache.h"      (decodeUtf8 $(embedFileRelative "data/morloc/cache.h"))
  , EmbededFile "xxhash.h"     (decodeUtf8 $(embedFileRelative "data/morloc/xxhash.h"))
  , EmbededFile "cache.c"      (decodeUtf8 $(embedFileRelative "data/morloc/cache.c"))
  , EmbededFile "cli.c"        (decodeUtf8 $(embedFileRelative "data/morloc/cli.c"))
  , EmbededFile "eval.c"       (decodeUtf8 $(embedFileRelative "data/morloc/eval.c"))
  , EmbededFile "ipc.c"        (decodeUtf8 $(embedFileRelative "data/morloc/ipc.c"))
  , EmbededFile "json.c"       (decodeUtf8 $(embedFileRelative "data/morloc/json.c"))
  , EmbededFile "mpack.c"      (decodeUtf8 $(embedFileRelative "data/morloc/mpack.c"))
  , EmbededFile "packet.c"     (decodeUtf8 $(embedFileRelative "data/morloc/packet.c"))
  , EmbededFile "schema.c"     (decodeUtf8 $(embedFileRelative "data/morloc/schema.c"))
  , EmbededFile "serialize.c"  (decodeUtf8 $(embedFileRelative "data/morloc/serialize.c"))
  , EmbededFile "shm.c"        (decodeUtf8 $(embedFileRelative "data/morloc/shm.c"))
  , EmbededFile "slurm.c"      (decodeUtf8 $(embedFileRelative "data/morloc/slurm.c"))
  , EmbededFile "utility.c"    (decodeUtf8 $(embedFileRelative "data/morloc/utility.c"))
  , EmbededFile "manifest.h"  (decodeUtf8 $(embedFileRelative "data/morloc/manifest.h"))
  , EmbededFile "manifest.c"  (decodeUtf8 $(embedFileRelative "data/morloc/manifest.c"))
  ]

-- | Produce a single self-contained morloc.h by recursively inlining local includes
libmorlocHeader :: Text
libmorlocHeader =
  let fileMap = Map.fromList [(T.pack (embededFileName ef), embededFileText ef) | ef <- libmorlocFiles]
  in inlineIncludes fileMap Set.empty (fileMap Map.! "morloc.h")

-- Replace each #include "xxx.h" with the file contents, tracking visited files
inlineIncludes :: Map Text Text -> Set Text -> Text -> Text
inlineIncludes fileMap seen content = T.unlines $ concatMap processLine (T.lines content)
  where
    processLine line = case parseLocalInclude line of
      Just filename
        | filename `Set.member` seen -> []
        | Just fileContent <- Map.lookup filename fileMap ->
            T.lines $ inlineIncludes fileMap (Set.insert filename seen) fileContent
        | otherwise -> [line]
      Nothing -> [line]

    parseLocalInclude line =
      let stripped = T.stripStart line
      in case T.stripPrefix "#include \"" stripped of
        Just rest -> case T.stripSuffix "\"" rest of
          Just filename -> Just filename
          Nothing -> Nothing
        Nothing -> Nothing

-- The static nexus source (compiled once during morloc init)
nexusSource :: EmbededFile
nexusSource = EmbededFile "nexus.c" (decodeUtf8 $ $(embedFileRelative "data/nexus.c"))

-- Pool templates for all supported languages
poolTemplate :: Lang -> EmbededFile
poolTemplate CppLang = EmbededFile "pool.cpp" (decodeUtf8 $ $(embedFileRelative "data/pools/pool.cpp"))
poolTemplate Python3Lang = EmbededFile "pool.py" (decodeUtf8 $ $(embedFileRelative "data/pools/pool.py"))
poolTemplate RLang = EmbededFile "pool.R" (decodeUtf8 $ $(embedFileRelative "data/pools/pool.R"))
poolTemplate _ = undefined

-- R interface to morloc.h
librlang :: EmbededFile
librlang = EmbededFile "rmorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/r/rmorloc.c"))

-- C++ interface to morloc.h
libcpplang :: EmbededFile
libcpplang = EmbededFile "cppmorloc.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.hpp"))

-- C++ wrapper implementations (compiled once during morloc init)
libcpplangImpl :: EmbededFile
libcpplangImpl = EmbededFile "cppmorloc.cpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.cpp"))

-- Precompiled header (compiled once during morloc init)
libcpplangPch :: EmbededFile
libcpplangPch = EmbededFile "morloc_pch.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/morloc_pch.hpp"))

-- Python interface to morloc.h
-- built as a module and imported into python pools and the nexus
-- requires libmlcmpack.so
libpylang :: EmbededFile
libpylang = EmbededFile "pymorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/py/pymorloc.c"))

libpylangSetup :: EmbededFile
libpylangSetup = EmbededFile "setup.py" (decodeUtf8 $ $(embedFileRelative "data/lang/py/setup.py"))

libpylangMakefile :: EmbededFile
libpylangMakefile = EmbededFile "Makefile" (decodeUtf8 $ $(embedFileRelative "data/lang/py/Makefile"))
