{-|
Module      : Language
Description : Handling for specific languages
Copyright   : (c) Zebulun Arendsee, 2021
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
  , makeExecutableName
  , makeSourceName
  , standardizeLangName
  , pairwiseCost
  , languageCost
  ) where

import Data.Text (Text, toLower)

-- | Programming languages in the Morloc ecosystem. This is the type that
-- should be used to refer to a language (don't use raw strings). Some of these
-- are languages that can be sourced (Python, R and C). Perl is currently used
-- only in generating the nexus file.
data Lang
  = Python3Lang
  | RLang
  | CLang
  | CppLang
  | RustLang
  | PerlLang
  deriving (Ord, Eq, Show)

-- | Map a function over each supported language
mapLang :: (Lang -> a) -> [a]
mapLang f =
  [ f Python3Lang
  , f RLang
  , f CLang
  , f CppLang
  , f RustLang
  , f PerlLang
  ]

-- | very rough function overhead costs that can be used when no benchmark info is available
pairwiseCost :: Lang -> Lang -> Int
-- functional overhead in each language
pairwiseCost CLang       CLang       = 1
pairwiseCost CppLang     CppLang     = 1
pairwiseCost RustLang    RustLang    = 1
pairwiseCost PerlLang    PerlLang    = 10
pairwiseCost Python3Lang Python3Lang = 10
pairwiseCost RLang       RLang       = 100
-- pairs of languages for which foreign calls are optimized
pairwiseCost CppLang CLang = 1
-- cost of naive foreign function calls
pairwiseCost _ CLang       = 500 -- the cost of a system call
pairwiseCost _ CppLang     = 500
pairwiseCost _ RustLang    = 500
pairwiseCost _ Python3Lang = 50000 -- the cost of opening the python interpreter and loading modules
pairwiseCost _ PerlLang    = 50000
-- this could be optimized by running R server
pairwiseCost _ RLang       = 5000000 -- an arm and a leg


-- | hello flame wars - these costs are mostly intended to break ties
languageCost :: Lang -> Int
languageCost CppLang = 0
languageCost CLang = 1
languageCost RustLang = 2
languageCost Python3Lang = 3
languageCost RLang = 4
languageCost PerlLang = 5

-- | Try to determine the source language for a file from its extension
parseExtension :: Text -> Maybe Lang
parseExtension "loc" = Nothing
parseExtension "py" = Just Python3Lang
parseExtension "R" = Just RLang
parseExtension "c" = Just CLang
parseExtension "h" = Just CLang
parseExtension "cpp" = Just CppLang
parseExtension "hpp" = Just CppLang
parseExtension "rs" = Just RustLang
parseExtension "pl" = Just PerlLang
parseExtension _ = Nothing

-- | Create an extension for a given language
makeExtension :: Lang -> String
makeExtension Python3Lang = "py"
makeExtension RLang = "R"
makeExtension CLang = "c"
makeExtension CppLang = "cpp"
makeExtension RustLang = "rs"
makeExtension PerlLang = "pl"

-- | Create the name of a given language. This is the internal standard name
-- for the language and the string language name used in the RDF.
showLangName :: Lang -> Text
showLangName Python3Lang = "python3"
showLangName RLang = "R"
showLangName CLang = "C"
showLangName CppLang = "Cpp"
showLangName RustLang = "Rust"
showLangName PerlLang = "Perl"

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
  "rust" -> Just RustLang
  "perl" -> Just PerlLang
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
makeExecutableName RustLang base = base <> "-rust.out" 
makeExecutableName lang base = makeSourceName lang base -- For interpreted languages

-- TODO: Use this function at the parsing stage to standardize names
-- | Convert a given language name to the standard form of the name (e.g., "py"
-- to "python3")
standardizeLangName :: Text -> Maybe Text
standardizeLangName = fmap showLangName . readLangName
