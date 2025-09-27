{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language
Description : Handling for specific languages
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The purpose of this module currently is to 1) unify language naming conventions
and 2) provide defaults for prioritizing languages.  This module should serve
as the starting place for adding a new language.

-}
module Morloc.Language
  ( Lang(..)
  , mapLang
  , parseExtension
  , makeExtension
  , showLangName
  , readLangName
  , makeExecutablePoolName
  , makeSourcePoolName
  , standardizeLangName
  , pairwiseCost
  , languageCost
  , serialType
  ) where

import Data.Text (Text, toLower)
import Morloc.Data.Doc

-- | Programming languages in the Morloc ecosystem. This is the type that
-- should be used to refer to a language (don't use raw strings). Some of these
-- are languages that can be sourced (Python, R and C).
data Lang
  = Python3Lang
  | RLang
  | CLang
  | CppLang
  deriving (Ord, Eq, Show)

instance Pretty Lang where
  pretty = viaShow

serialType :: Lang -> Text
serialType CppLang = "uint8_t*"
serialType RLang = "character"
serialType Python3Lang = "str"
serialType CLang = error "C is not yet supported"

-- | Map a function over each supported language
mapLang :: (Lang -> a) -> [a]
mapLang f =
  [ f Python3Lang
  , f RLang
  , f CLang
  , f CppLang
  ]

-- | very rough function overhead costs that can be used when no benchmark info is available
pairwiseCost :: Lang -> Lang -> Int
-- functional overhead in each language
pairwiseCost CLang       CLang       = 1
pairwiseCost CppLang     CppLang     = 1
pairwiseCost Python3Lang Python3Lang = 10
pairwiseCost RLang       RLang       = 100
-- pairs of languages for which foreign calls are optimized
pairwiseCost CppLang CLang = 1
-- cost of naive foreign function calls
pairwiseCost _ CLang       = 5000 -- the cost of a system call
pairwiseCost _ CppLang     = 5000
pairwiseCost _ Python3Lang = 500000 -- the cost of opening the python interpreter and loading modules
-- this could be optimized by running R server
pairwiseCost _ RLang       = 50000000 -- an arm and a leg


-- | hello flame wars - these costs are mostly intended to break ties
languageCost :: Lang -> Int
languageCost CppLang = 0
languageCost CLang = 1
languageCost Python3Lang = 3
languageCost RLang = 4

-- | Try to determine the source language for a file from its extension
parseExtension :: Text -> Maybe Lang
parseExtension "loc" = Nothing
parseExtension "py" = Just Python3Lang
parseExtension "R" = Just RLang
parseExtension "c" = Just CLang
parseExtension "h" = Just CLang
parseExtension "cpp" = Just CppLang
parseExtension "hpp" = Just CppLang
parseExtension _ = Nothing

-- | Create an extension for a given language
makeExtension :: Lang -> String
makeExtension Python3Lang = "py"
makeExtension RLang = "R"
makeExtension CLang = "c"
makeExtension CppLang = "cpp"

-- | Create the name of a given language. This is the internal standard name
-- for the language and the string language name used in the RDF.
showLangName :: Lang -> Text
showLangName Python3Lang = "python3"
showLangName RLang = "r"
showLangName CLang = "c"
showLangName CppLang = "cpp"

-- | Read the name of a given language and try to translate it
readLangName :: Text -> Maybe Lang
readLangName name = case toLower name of
  "python" -> Just Python3Lang
  "python3" -> Just Python3Lang
  "py" -> Just Python3Lang
  "r" -> Just RLang
  "c" -> Just CLang
  "cpp" -> Just CppLang
  "c++" -> Just CppLang
  _ -> Nothing

-- | Generate a name for a pool top-level source file given a language.
makeSourceName ::
     Lang
  -> String -- ^ basename
  -> String -- ^ source file basename
makeSourceName lang base = base ++ "." ++ makeExtension lang

-- | Generate a name for a pool executable file given a language. For
-- interpreted languages this will be the same as the output of the
-- @makeSourceName@ function.
makeExecutableName ::
     Lang
  -> String -- ^ basename
  -> String -- ^ executable file basename
makeExecutableName CLang base = base <> "-c.out"
makeExecutableName CppLang base = base <> "-cpp.out"
makeExecutableName lang base = makeSourceName lang base -- For interpreted languages

makeExecutablePoolName :: Lang -> String
makeExecutablePoolName lang = makeExecutableName lang "pool"

makeSourcePoolName :: Lang -> String
makeSourcePoolName lang = makeSourceName lang "pool"

-- TODO: Use this function at the parsing stage to standardize names
-- | Convert a given language name to the standard form of the name (e.g., "py"
-- to "python3")
standardizeLangName :: Text -> Maybe Text
standardizeLangName = fmap showLangName . readLangName
