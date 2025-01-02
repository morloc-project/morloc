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
  , libmorloc
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


-- C library for universal binary formatting, serialization, and everything
libmorloc :: (String, Text)
libmorloc = ("morloc.h", decodeUtf8 $ $(embedFileRelative "data//morloc.h"))

-- R interface to morloc.h
librlang :: (String, Text)
librlang = ("rmorloc.c", decodeUtf8 $ $(embedFileRelative "data/lang/r/rmorloc.c"))

-- C++ interface to morloc.h
libcpplang :: (String, Text)
libcpplang = ("cppmorloc.hpp", decodeUtf8 $ $(embedFileRelative "data/lang/cpp/cppmorloc.hpp"))

-- Python interface to morloc.h
-- built as a module and imported into python pools and the nexus
-- requires libmlcmpack.so
libpylang :: (String, Text)
libpylang = ("pymorloc.c", decodeUtf8 $ $(embedFileRelative "data/lang/py/pymorloc.c"))

libpylangSetup :: (String, Text)
libpylangSetup = ("setup.py", decodeUtf8 $ $(embedFileRelative "data/lang/py/setup.py"))

libpylangMakefile :: (String, Text)
libpylangMakefile = ("Makefile", decodeUtf8 $ $(embedFileRelative "data/lang/py/Makefile"))
