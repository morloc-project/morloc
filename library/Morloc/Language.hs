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
