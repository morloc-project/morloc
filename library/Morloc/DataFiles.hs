{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.DataFiles
Description : Handle non-Haskell files such as foreign language sources files and configs
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.DataFiles
  ( EmbededFile(..)
  , LibMorloc(..)
  , libmorloc
  , nexusTemplate
  , poolTemplate
  , rSocketLib
  , libcpplang
  , libpylang
  , libpylangMakefile
  , libpylangSetup
  , librlang
  ) where

import Morloc.Namespace
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text) 

data EmbededFile = EmbededFile
  { embededFileName :: String -- basename for the file
  , embededFileText :: Text -- full text the file contained at compile time
  }

-- C library for universal binary formatting, serialization, and everything
data LibMorloc = LibMorloc
  { libMorlocH :: EmbededFile
  , libHashH :: EmbededFile
  }
libmorloc = LibMorloc
  { libMorlocH = EmbededFile "morloc.h" (decodeUtf8 $ $(embedFileRelative "data/morloc.h"))
  , libHashH = EmbededFile "xxhash.h" (decodeUtf8 $ $(embedFileRelative "data/third-party/xxhash.h"))
  }

-- The nexus template
nexusTemplate :: EmbededFile
nexusTemplate = EmbededFile "nexus.py" (decodeUtf8 $ $(embedFileRelative "data/nexus.py"))


-- Pool templates for all supported languages
poolTemplate :: Lang -> EmbededFile
poolTemplate CppLang     = EmbededFile "pool.cpp" (decodeUtf8 $ $(embedFileRelative "data/pools/pool.cpp"))
poolTemplate Python3Lang = EmbededFile "pool.py" (decodeUtf8 $ $(embedFileRelative "data/pools/pool.py"))
poolTemplate RLang       = EmbededFile "pool.R" (decodeUtf8 $ $(embedFileRelative "data/pools/pool.R"))
poolTemplate _ = undefined

-- C file describing socket bindings needed for R
rSocketLib :: EmbededFile
rSocketLib = EmbededFile "socketr.c" (decodeUtf8 $ $(embedFileRelative "data/misc/socketr.c"))

-- R interface to morloc.h
librlang :: EmbededFile
librlang = EmbededFile "rmorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/r/rmorloc.c"))

-- C++ interface to morloc.h
libcpplang :: EmbededFile
libcpplang = EmbededFile "cppmorloc.hpp" (decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.hpp"))

-- Python interface to morloc.h
-- built as a module and imported into python pools and the nexus
-- requires libmlcmpack.so
libpylang :: EmbededFile
libpylang = EmbededFile "pymorloc.c" (decodeUtf8 $ $(embedFileRelative "data/lang/py/pymorloc.c"))

libpylangSetup :: EmbededFile
libpylangSetup = EmbededFile "setup.py" (decodeUtf8 $ $(embedFileRelative "data/lang/py/setup.py"))

libpylangMakefile :: EmbededFile
libpylangMakefile = EmbededFile "Makefile" (decodeUtf8 $ $(embedFileRelative "data/lang/py/Makefile"))
