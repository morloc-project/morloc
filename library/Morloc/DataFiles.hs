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
msgpackSource :: ((String, Text), [(String, Text)])
msgpackSource =
  ( ("mlcmpack.h", decodeUtf8 $ $(embedFileRelative "data/msgpack/src/mlcmpack.h"))
  , [ ("mlcmpack.c", decodeUtf8 $ $(embedFileRelative "data/msgpack/src/mlcmpack.c"))
    , ("mpack.h",   decodeUtf8 $ $(embedFileRelative "data/msgpack/src/mpack.h")   )
    , ("mpack.c",   decodeUtf8 $ $(embedFileRelative "data/msgpack/src/mpack.c")   )
    ]
  )

-- A C file that defines the R binding to the mlcmpack MessagePack library
rmpack :: (String, Text)
rmpack = ("rmpack.c", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/r/rmpack.c"))

-- A python code defining the python binding to the mlcmpack MessagePack library
pympack :: (String, Text)
pympack = ("pympack.py", decodeUtf8 $ $(embedFileRelative "data/msgpack/lang/py/pympack.py"))
