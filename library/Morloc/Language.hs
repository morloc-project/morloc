{-|
Module      : Language
Description : Handling for specific languages
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The purpose of this module currently is to unify language naming conventions.
We need to streamline the process of adding new languages to Morloc. This
module should serve as the starting place for adding a new language.
-}
module Morloc.Language
  ( Lang(..)
  , mapLang
  , parseExtension
  , makeExtension
  , showLangName
  , readLangName
  , makeExecutableName
  , makeSourceName
  , standardizeLangName
  , pairwiseCost
  ) where

import Morloc.Internal
import Data.Text (Text)

-- | Programming languages in the Morloc ecosystem. This is the type that
-- should be used to refer to a language (don't use raw strings). Some of these
-- are languages that can be sourced (Python, R and C). Perl is currently used
-- only in generating the nexus file.
data Lang
  = Python3Lang
  | RLang
  | CLang
  | CppLang
  | PerlLang
  deriving (Ord, Eq, Show)

-- | Map a function over each supported language
mapLang :: (Lang -> a) -> [a]
mapLang f =
  [ f Python3Lang
  , f RLang
  , f CLang
  , f CppLang
  , f PerlLang
  ]

-- | very rough function overhead costs that can be used when no benchmark info is available
-- `Nothing` indicates that the language pair are not interoperable
pairwiseCost :: Lang -> Lang -> Maybe Int
-- functional overhead in each language
pairwiseCost CLang       CLang       = Just 1
pairwiseCost CppLang     CppLang     = Just 1
pairwiseCost PerlLang    PerlLang    = Just 10
pairwiseCost Python3Lang Python3Lang = Just 10
pairwiseCost RLang       RLang       = Just 100
-- pairs of languages for which foreign calls are optimized
pairwiseCost CppLang CLang = Just 1
-- cost of naive foreign function calls
pairwiseCost _ CLang       = Just 100
pairwiseCost _ CppLang     = Just 100
pairwiseCost _ Python3Lang = Just 10000
pairwiseCost _ PerlLang    = Just 10000
pairwiseCost _ RLang       = Just 1000000

-- | Try to determine the source language for a file from its extension
parseExtension :: Text -> Maybe Lang
parseExtension "loc" = Nothing
parseExtension "py" = Just Python3Lang
parseExtension "R" = Just RLang
parseExtension "c" = Just CLang
parseExtension "h" = Just CLang
parseExtension "cpp" = Just CppLang
parseExtension "hpp" = Just CppLang
parseExtension "pl" = Just PerlLang
parseExtension _ = Nothing

-- | Create an extension for a given language
makeExtension :: Lang -> Text
makeExtension Python3Lang = "py"
makeExtension RLang = "R"
makeExtension CLang = "c"
makeExtension CppLang = "cpp"
makeExtension PerlLang = "pl"

-- | Create the name of a given language. This is the internal standard name
-- for the language and the string language name used in the RDF.
showLangName :: Lang -> Text
showLangName Python3Lang = "python3"
showLangName RLang = "R"
showLangName CLang = "C"
showLangName CppLang = "Cpp"
showLangName PerlLang = "Perl"

-- | Read the name of a given language and try to translate it
readLangName :: Text -> Maybe Lang
readLangName "python" = Just Python3Lang
readLangName "python3" = Just Python3Lang
readLangName "py" = Just Python3Lang
readLangName "R" = Just RLang
readLangName "r" = Just RLang
readLangName "C" = Just CLang
readLangName "c" = Just CLang
readLangName "cpp" = Just CppLang
readLangName "Cpp" = Just CppLang
readLangName "C++" = Just CppLang
readLangName "c++" = Just CppLang
readLangName "Perl" = Just PerlLang
readLangName "perl" = Just PerlLang
readLangName _ = Nothing

-- | Generate a name for a pool top-level source file given a language.
makeSourceName ::
     Lang
  -> Text -- ^ basename
  -> Text -- ^ source file basename
makeSourceName lang base = base <> "." <> makeExtension lang

-- | Generate a name for a pool executable file given a language. For
-- interpreted languages this will be the same as the output of the
-- @makeSourceName@ function.
makeExecutableName ::
     Lang
  -> Text -- ^ basename
  -> Text -- ^ executable file basename
makeExecutableName CLang base = base <> "-c.out"
makeExecutableName CppLang base = base <> "-cpp.out"
makeExecutableName lang base = makeSourceName lang base -- For interpreted languages

-- TODO: Use this function at the parsing stage to standardize names
-- | Convert a given language name to the standard form of the name (e.g., "py"
-- to "python3")
standardizeLangName :: Text -> Maybe Text
standardizeLangName = fmap showLangName . readLangName
