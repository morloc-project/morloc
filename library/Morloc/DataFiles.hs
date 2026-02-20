{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Morloc.DataFiles
Description : Template-Haskell-embedded data files for runtime and codegen
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

All non-Haskell data files (C library sources, pool templates, init scripts,
lang.yaml configs, nexus source) are embedded at compile time via
'Data.FileEmbed.embedFileRelative'. This module provides typed access to
these files for use by 'Morloc.CodeGenerator.SystemConfig' (init) and the
translators (codegen).
-}
module Morloc.DataFiles
  ( EmbededFile (..)
  , LangSetup (..)
  , libmorlocFiles
  , libmorlocHeader
  , nexusSource
  , poolTemplate
  , poolTemplateGeneric
  , langSetups
  , langRegistryFiles
  , languagesYaml
  ) where

import Data.FileEmbed (embedFileRelative)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

data EmbededFile = EmbededFile
  { embededFileName :: String -- basename for the file
  , embededFileText :: Text -- full text the file contained at compile time
  }

-- | Per-language init setup: an init script and associated data files.
data LangSetup = LangSetup
  { lsName :: String
  , lsInitScript :: EmbededFile
  , lsFiles :: [EmbededFile]
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
  , EmbededFile "daemon.h"    (decodeUtf8 $(embedFileRelative "data/morloc/daemon.h"))
  , EmbededFile "daemon.c"    (decodeUtf8 $(embedFileRelative "data/morloc/daemon.c"))
  , EmbededFile "pool.h"      (decodeUtf8 $(embedFileRelative "data/morloc/pool.h"))
  , EmbededFile "pool.c"      (decodeUtf8 $(embedFileRelative "data/morloc/pool.c"))
  , EmbededFile "http.h"      (decodeUtf8 $(embedFileRelative "data/morloc/http.h"))
  , EmbededFile "http.c"      (decodeUtf8 $(embedFileRelative "data/morloc/http.c"))
  , EmbededFile "router.h"    (decodeUtf8 $(embedFileRelative "data/morloc/router.h"))
  , EmbededFile "router.c"    (decodeUtf8 $(embedFileRelative "data/morloc/router.c"))
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

-- | Pool template lookup by canonical language name
poolTemplate :: Text -> EmbededFile
poolTemplate "cpp" = EmbededFile "pool.cpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/pool.cpp"))
poolTemplate name = error $ "No embedded pool template for " <> T.unpack name

-- | 3-section pool templates for the generic translator (sources, manifolds, dispatch)
poolTemplateGeneric :: Text -> EmbededFile
poolTemplateGeneric "py" = EmbededFile "pool.py" (decodeUtf8 $ $(embedFileRelative "data/lang/py/pool.py"))
poolTemplateGeneric "r" = EmbededFile "pool.R" (decodeUtf8 $ $(embedFileRelative "data/lang/r/pool.R"))
poolTemplateGeneric name = poolTemplate name

-- | Per-language init setups. Each bundles an init.sh script with
-- the data files that should be written to the build dir before running it.
langSetups :: [LangSetup]
langSetups = [cppSetup, pythonSetup, rSetup, juliaSetup]

cppSetup :: LangSetup
cppSetup = LangSetup "C++"
  (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/init.sh")))
  [ EmbededFile "cppmorloc.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.hpp"))
  , EmbededFile "cppmorloc.cpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.cpp"))
  , EmbededFile "morloc_pch.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/morloc_pch.hpp"))
  ]

pythonSetup :: LangSetup
pythonSetup = LangSetup "python"
  (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/py/init.sh")))
  [ EmbededFile "pymorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/py/pymorloc.c"))
  , EmbededFile "setup.py" (decodeUtf8 $ $(embedFileRelative "data/lang/py/setup.py"))
  , EmbededFile "Makefile" (decodeUtf8 $ $(embedFileRelative "data/lang/py/Makefile"))
  ]

rSetup :: LangSetup
rSetup = LangSetup "R"
  (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/r/init.sh")))
  [ EmbededFile "rmorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/r/rmorloc.c"))
  ]

juliaSetup :: LangSetup
juliaSetup = LangSetup "Julia"
  (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/init.sh")))
  [ EmbededFile "juliabridge.c" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/juliabridge.c"))
  , EmbededFile "MorlocRuntime.jl" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/MorlocRuntime.jl"))
  , EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/lang.yaml"))
  , EmbededFile "pool.jl" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/pool.jl"))
  ]

-- | Per-language lang.yaml files keyed by canonical name
langRegistryFiles :: [(String, EmbededFile)]
langRegistryFiles =
  [ ("c",   EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/c/lang.yaml")))
  , ("cpp", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/lang.yaml")))
  , ("py",  EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/py/lang.yaml")))
  , ("r",   EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/r/lang.yaml")))
  , ("jl",  EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/lang.yaml")))
  ]

-- | Shared languages.yaml with pairwise costs
languagesYaml :: EmbededFile
languagesYaml = EmbededFile "languages.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/languages.yaml"))
