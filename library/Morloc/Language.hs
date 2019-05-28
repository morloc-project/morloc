{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language
Description : Handling for specific languages
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Language
(
  Lang(..)
  , parseExtension
  , makeExtension
  , showLangName
  , readLangName
  , makeExecutableName
  , makeSourceName
  , standardizeLangName
) where

import Data.Monoid ((<>))
import Data.Text (Text)
-- Don't import anything from Morloc here. This module should be VERY lowest
-- in the hierarchy, to avoid circular dependencies, since the lexer needs to
-- access it.

-- | Programming language
data Lang
  = MorlocLang
  | Python3Lang
  | RLang
  | CLang
  | PerlLang
  deriving(Ord, Eq, Show)

-- | Try to determine the source language for a file from its extension
parseExtension :: Text -> Maybe Lang
parseExtension "loc" = Just MorlocLang
parseExtension "py"  = Just Python3Lang
parseExtension "R"   = Just RLang
parseExtension "c"   = Just CLang
parseExtension "h"   = Just CLang
parseExtension "pl"  = Just PerlLang
parseExtension _ = Nothing 

-- | Create an extension for a given language
makeExtension :: Lang -> Text
makeExtension MorlocLang  = "loc"
makeExtension Python3Lang = "py"
makeExtension RLang       = "R"
makeExtension CLang       = "c"
makeExtension PerlLang    = "pl"

-- | Creat the name of a given language
showLangName :: Lang -> Text
showLangName MorlocLang  = "morloc"
showLangName Python3Lang = "python3"
showLangName RLang       = "R"
showLangName CLang       = "C"
showLangName PerlLang    = "Perl"

-- | Read the name of a given language and translate it
readLangName :: Text -> Maybe Lang
readLangName "morloc"  = Just MorlocLang
readLangName "Morloc"  = Just MorlocLang
readLangName "python"  = Just Python3Lang
readLangName "python3" = Just Python3Lang
readLangName "py"      = Just Python3Lang
readLangName "R"       = Just RLang
readLangName "r"       = Just RLang
readLangName "C"       = Just CLang
readLangName "c"       = Just CLang
readLangName "Perl"    = Just PerlLang
readLangName "perl"    = Just PerlLang
readLangName _ = Nothing

-- | Generate a name for a pool top-level source file given a language.
makeSourceName
  :: Lang
  -> Text -- ^ basename
  -> Text -- ^ source file basename
makeSourceName lang base = base <> "." <> makeExtension lang

-- | Generate a name for a pool executable file given a language. For
-- interpreted languages this will be the same as the output of the
-- @makeSourceName@ function.
makeExecutableName
  :: Lang
  -> Text -- ^ basename
  -> Text -- ^ executable file basename
makeExecutableName CLang base = base <> "-c.out"
makeExecutableName lang  base = makeSourceName lang base -- For interpreted languages

-- TODO: Use this function at the parsing stage to standardize names
-- | Convert a given language name to the standard form of the name (e.g., "py"
-- to "python3")
standardizeLangName :: Text -> Maybe Text
standardizeLangName = fmap showLangName . readLangName
