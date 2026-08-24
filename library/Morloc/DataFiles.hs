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
  , poolHostTemplate
  , poolTemplateGeneric
  , langSetups
  , langRegistryFiles
  , languagesYaml
  , requirementsCore
  , requirementsFiles
  , installScriptFiles
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
poolTemplate "rust" = EmbededFile "pool.rs" (decodeUtf8 $ $(embedFileRelative "data/lang/rust/pool.rs"))
poolTemplate name = error $ "No embedded pool template for " <> T.unpack name

-- | Member-agnostic pool host translation unit for the CAbi family. Owns
-- main()/pool_main; the member (pool.cpp) provides the registration hook it calls.
poolHostTemplate :: Text -> EmbededFile
poolHostTemplate "cpp" = EmbededFile "pool_host.cpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/pool_host.cpp"))
poolHostTemplate name = error $ "No embedded pool host template for " <> T.unpack name

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
  , ("futhark", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/futhark/lang.yaml")))
  , ("rust", EmbededFile "lang.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/rust/lang.yaml")))
  ]

-- | Shared languages.yaml with pairwise costs
languagesYaml :: EmbededFile
languagesYaml = EmbededFile "languages.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/languages.yaml"))

-- | Core build-toolchain requirements (rust/c-compiler/make/pkg-config), shared
-- across languages, beside languages.yaml.
requirementsCore :: EmbededFile
requirementsCore = EmbededFile "requirements.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/requirements.yaml"))

-- | Per-language binder requirements (conda packages + supported runtime
-- version), keyed by canonical name. Kept separate from lang.yaml (grammar).
requirementsFiles :: [(Text, EmbededFile)]
requirementsFiles =
  [ ("py", EmbededFile "requirements.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/py/requirements.yaml")))
  , ("r", EmbededFile "requirements.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/r/requirements.yaml")))
  , ("cpp", EmbededFile "requirements.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/requirements.yaml")))
  , ("rust", EmbededFile "requirements.yaml" (decodeUtf8 $ $(embedFileRelative "data/lang/rust/requirements.yaml")))
  ]

-- | Per-language container install scripts, for languages NOT on conda-forge
-- (their upstream binary is fetched by a script at OCI image build). Keyed by
-- canonical name. Keep in lockstep with `layout::SCRIPT_LANGUAGES` in morloc-deps
-- (morloc-project/morloc-manager).
installScriptFiles :: [(Text, EmbededFile)]
installScriptFiles =
  [ ("futhark", EmbededFile "install.sh" (decodeUtf8 $ $(embedFileRelative "data/lang/futhark/install.sh")))
  ]
