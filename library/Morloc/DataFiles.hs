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
  ( poolTemplate
  , nexusTemplate
  , rSocketLib
  , msgpackSource
  , rmpack
  , pympack
  , pympackSetup
  , pympackMakefile
  , cppmpack
  ) where

import Morloc.Namespace
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text) 

-- Pool templates for all supported languages
poolTemplate :: Lang -> Text
poolTemplate CppLang     = decodeUtf8 $ $(embedFileRelative "data/pools/pool.cpp")
poolTemplate Python3Lang = decodeUtf8 $ $(embedFileRelative "data/pools/pool.py")
poolTemplate RLang       = decodeUtf8 $ $(embedFileRelative "data/pools/pool.R")
poolTemplate _ = undefined

-- The nexus template
nexusTemplate :: Text
nexusTemplate = decodeUtf8 $ $(embedFileRelative "data/nexus.py")

-- C file describing socket bindings needed for R
rSocketLib :: (Text, Text)
rSocketLib = ("socketr.c", decodeUtf8 $ $(embedFileRelative "data/misc/socketr.c"))

-- The main header and required source files for the mlcmpack library
-- Required, by all MessagePack-based pools
msgpackSource :: (String, Text)
msgpackSource = ("mlcmpack.h", decodeUtf8 $ $(embedFileRelative "data/msgpack/src/mlcmpack.h"))

-- A C file that defines the R binding to the mlcmpack MessagePack library
-- compiled into a shared library loaded by R pools
-- requires mlcmpack.h
rmpack :: (String, Text)
rmpack = ("mpackr.c", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/r/mpackr.c"))

-- header used in C++ pools
cppmpack :: (String, Text)
cppmpack = ("cppmpack.hpp", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/cpp/cppmpack.hpp"))

-- A Python C API program binding to the mlcmpack MessagePack library
-- will be built as a module and imported into python pools and the nexus
-- requires libmlcmpack.so
pympack :: (String, Text)
pympack = ("pympack.c", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/py/pympack.c"))

-- A setup script for building pympack.c
pympackSetup :: (String, Text)
pympackSetup = ("setup.py", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/py/setup.py"))

-- A Makefile for building pympack.c
pympackMakefile :: (String, Text)
pympackMakefile = ("Makefile", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/py/Makefile"))
