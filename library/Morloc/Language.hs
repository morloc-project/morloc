{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Language
Description : Language type and utilities
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The Lang type is a simple name+extension record. All language metadata
lives in the LangRegistry (loaded from lang.yaml files).
-}
module Morloc.Language
  ( Lang (..)
  , makeExtension
  , showLangName
  , makeExecutablePoolName
  , makeSourcePoolName
  , pairwiseCost
  , languageCost
  , serialType
  -- * Smart constructors for built-in languages
  , pyLang
  , rLang
  , cLang
  , cppLang
  -- * Construction from name + extension
  , makeLang
  ) where

import Data.Text (Text)
import Morloc.Data.Doc

-- | A programming language in the Morloc ecosystem.
-- Identity is determined solely by the canonical name.
data Lang = Lang
  { langName :: !Text        -- canonical lowercase name: "py", "r", "cpp", etc.
  , langExtension :: !String -- file extension: "py", "R", "cpp", etc.
  } deriving (Show)

instance Eq Lang where
  a == b = langName a == langName b

instance Ord Lang where
  compare a b = compare (langName a) (langName b)

instance Pretty Lang where
  pretty = pretty . langName

-- Smart constructors for built-in languages
pyLang :: Lang
pyLang = Lang "py" "py"

rLang :: Lang
rLang = Lang "r" "R"

cLang :: Lang
cLang = Lang "c" "c"

cppLang :: Lang
cppLang = Lang "cpp" "cpp"

-- | Construct a Lang from canonical name and extension
makeLang :: Text -> String -> Lang
makeLang = Lang

-- | Get the file extension for a language
makeExtension :: Lang -> String
makeExtension = langExtension

-- | Get the canonical name of a language
showLangName :: Lang -> Text
showLangName = langName

makeSourceName :: Lang -> String -> String
makeSourceName lang base = base ++ "." ++ makeExtension lang

makeExecutableName :: Lang -> String -> String
makeExecutableName lang base
  | langName lang == "c"   = base <> "-c.out"
  | langName lang == "cpp" = base <> "-cpp.out"
  | otherwise = makeSourceName lang base

makeExecutablePoolName :: Lang -> String
makeExecutablePoolName lang = makeExecutableName lang "pool"

makeSourcePoolName :: Lang -> String
makeSourcePoolName lang = makeSourceName lang "pool"

-- | Pairwise cost of calling from one language to another.
-- These are used by the optimizer to select implementations.
pairwiseCost :: Lang -> Lang -> Int
pairwiseCost from to
  | from == to = sameLangCost (langName from)
  | langName from == "cpp" && langName to == "c" = 1
  | otherwise = crossLangCost (langName to)
  where
    sameLangCost "c"   = 1
    sameLangCost "cpp" = 1
    sameLangCost "py"  = 10
    sameLangCost "r"   = 100
    sameLangCost _     = 10

    crossLangCost "c"   = 5000
    crossLangCost "cpp" = 5000
    crossLangCost "py"  = 10000
    crossLangCost "r"   = 40000
    crossLangCost _     = 10000

-- | Preference cost for tie-breaking between languages
languageCost :: Lang -> Int
languageCost lang = case langName lang of
  "cpp" -> 0
  "c"   -> 1
  "py"  -> 3
  "r"   -> 4
  _     -> 5

-- | The serialization type for a language
serialType :: Lang -> Text
serialType lang = case langName lang of
  "cpp" -> "uint8_t*"
  "r"   -> "character"
  "py"  -> "str"
  "c"   -> error "C is not yet supported as a pool language"
  _     -> "bytes"
