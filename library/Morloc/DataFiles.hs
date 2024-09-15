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
  ( languageFiles
  ) where

import Morloc.Namespace
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFileRelative)

import Morloc.Data.Doc as MD

-- | Language-specific source code that is inserted into the generated pools
languageFiles :: Lang -> LanguageSource
languageFiles CppLang = LanguageSource
  (pretty . decodeUtf8 $ $(embedFileRelative "data/interop.cpp"))
  (pretty . decodeUtf8 $ $(embedFileRelative "data/serialization.cpp"))
languageFiles Python3Lang = LanguageSource
  (pretty . decodeUtf8 $ $(embedFileRelative "data/interop.py"))
  (pretty . decodeUtf8 $ $(embedFileRelative "data/serialization.py"))
languageFiles RLang = LanguageSource
  (pretty . decodeUtf8 $ $(embedFileRelative "data/interop.R"))
  (pretty . decodeUtf8 $ $(embedFileRelative "data/serialization.R"))
languageFiles _ = LanguageSource "" ""
