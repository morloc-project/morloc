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
  , libmorlocHeader
  , poolTemplate
  , poolTemplateGeneric
  , langSetups
  , langRegistryFiles
  , languagesYaml
  ) where

import Data.FileEmbed (embedFileRelative)
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
  , lsRequiredTools :: [String]
  , lsInitScript :: EmbededFile
  , lsFiles :: [EmbededFile]
  }

-- | The single self-contained morloc.h header (the ABI contract for libmorloc.so).
-- Language extensions and pool templates #include this to call into the Rust library.
libmorlocHeader :: Text
libmorlocHeader = decodeUtf8 $(embedFileRelative "data/morloc/morloc.h")

-- | Pool template lookup by canonical language name
poolTemplate :: Text -> EmbededFile
poolTemplate "cpp" = EmbededFile "pool.cpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/pool.cpp"))
poolTemplate name = error $ "No embedded pool template for " <> T.unpack name

-- | 3-section pool templates for the generic translator (sources, manifolds, dispatch)
poolTemplateGeneric :: Text -> EmbededFile
poolTemplateGeneric "py" = EmbededFile "pool.py" (decodeUtf8 $ $(embedFileRelative "data/lang/py/pool.py"))
poolTemplateGeneric "r" = EmbededFile "pool.R" (decodeUtf8 $ $(embedFileRelative "data/lang/r/pool.R"))
poolTemplateGeneric name = poolTemplate name

{- | Per-language init setups. Each bundles an init.sh script with
the data files that should be written to the build dir before running it.
-}
langSetups :: [LangSetup]
langSetups = [cppSetup, pythonSetup, rSetup, juliaSetup]

cppSetup :: LangSetup
cppSetup =
  LangSetup
    "C++"
    ["g++", "git"]
    (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/init.sh")))
    [ EmbededFile "cppmorloc.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.hpp"))
    , EmbededFile "cppmorloc.cpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.cpp"))
    , EmbededFile "morloc_pch.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/morloc_pch.hpp"))
    , EmbededFile "mlc_arrow.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/mlc_arrow.hpp"))
    , EmbededFile "mlc_tensor.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/mlc_tensor.hpp"))
    , EmbededFile "nanoarrow.h" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/nanoarrow/nanoarrow.h"))
    , EmbededFile "nanoarrow.c" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/nanoarrow/nanoarrow.c"))
    ]

pythonSetup :: LangSetup
pythonSetup =
  LangSetup
    "python"
    ["python3"]
    (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/py/init.sh")))
    [ EmbededFile "pymorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/py/pymorloc.c"))
    , EmbededFile "setup.py" (decodeUtf8 $ $(embedFileRelative "data/lang/py/setup.py"))
    , EmbededFile "Makefile" (decodeUtf8 $ $(embedFileRelative "data/lang/py/Makefile"))
    ]

rSetup :: LangSetup
rSetup =
  LangSetup
    "R"
    ["R"]
    (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/r/init.sh")))
    [ EmbededFile "rmorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/r/rmorloc.c"))
    ]

juliaSetup :: LangSetup
juliaSetup =
  LangSetup
    "Julia"
    ["julia"]
    (EmbededFile "init.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/init.sh")))
    [ EmbededFile "juliabridge.c" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/juliabridge.c"))
    , EmbededFile
        "MorlocRuntime.jl"
        (decodeUtf8 $ $(embedFileRelative "data/lang/julia/MorlocRuntime.jl"))
    , EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/lang.yaml"))
    , EmbededFile "pool.jl" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/pool.jl"))
    ]

-- | Per-language lang.yaml files keyed by canonical name
langRegistryFiles :: [(String, EmbededFile)]
langRegistryFiles =
  [ ("c", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/c/lang.yaml")))
  , ("cpp", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/lang.yaml")))
  , ("py", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/py/lang.yaml")))
  , ("r", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/r/lang.yaml")))
  , ("jl", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/julia/lang.yaml")))
  ]

-- | Shared languages.yaml with pairwise costs
languagesYaml :: EmbededFile
languagesYaml = EmbededFile "languages.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/languages.yaml"))
