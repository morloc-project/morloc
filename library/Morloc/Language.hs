module Morloc.Language (
    Lang(..)
) where

-- supported languages
-- TODO Eventually the supported languages should not be hardcoded in Morloc
-- core, but should rather be specified in input textual files.
data Lang
  = LangR
  | LangPython3
  | LangBash
  | LangOther String
  deriving(Show, Ord, Eq)
