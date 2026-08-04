{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Build.Params
Description : Per-language build parameters passed via @-X lang:key=value@
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Build parameters let a user pass options to the language-specific builders
without morloc having to recapitulate each compiler's option surface. The morloc
UI only parses the @LANG:KEY=VALUE@ grammar and stores the result; it does not
interpret keys or values. Each language's builder reads the entries it
understands and validates values locally.

Parameters are stored as @lang -> key -> value@. Precedence, low to high, is
compiled-in default < build-config @lang-params@ < command-line @-X@. Most keys
are single-valued and the higher layer wins. The reserved 'flagsKey' has list
semantics instead: repeated entries accumulate in order and layers concatenate
(never dedup, never reorder), so raw compiler flags survive intact.
-}
module Morloc.Build.Params
  ( LangParams
  , parseLangParam
  , parseLangKey
  , foldLangParams
  , mergeLangParams
  , unsetParam
  , lookupParam
  , lookupFlags
  , renderSalt
  , paramEntries
  ) where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Resolved build parameters: @lang -> key -> value@.
type LangParams = Map Text (Map Text Text)

-- | The one key with list semantics: a raw, ordered, never-deduplicated flag
-- passthrough to the language's compile command.
flagsKey :: Text
flagsKey = "flags"

-- Internal separator joining accumulated 'flagsKey' tokens within a single
-- value string. Flag tokens never contain a newline, so this round-trips.
flagSep :: Text
flagSep = "\n"

-- | Combine two values for the same @lang:key@. 'flagsKey' concatenates left
-- (lower precedence / earlier) then right; every other key takes the right
-- (higher precedence / later) value.
combineParam :: Text -> Text -> Text -> Text
combineParam key old new
  | key == flagsKey = old <> flagSep <> new
  | otherwise = new

-- | Parse one @LANG:KEY=VALUE@ argument. Splits on the first @:@ then the first
-- @=@; VALUE is taken verbatim and may itself contain @:@ or @=@. LANG and KEY
-- must be non-empty; VALUE may be empty.
parseLangParam :: String -> Either Text (Text, Text, Text)
parseLangParam s =
  case break (== ':') s of
    (langS, ':' : rest)
      | not (null langS) ->
          case break (== '=') rest of
            (keyS, '=' : valS)
              | not (null keyS) -> Right (T.pack langS, T.pack keyS, T.pack valS)
            _ -> Left invalid
    _ -> Left invalid
  where
    invalid =
      "invalid -X argument '" <> T.pack s <> "'; expected LANG:KEY=VALUE"

-- | Parse a @LANG:KEY@ argument (no value), for removing a parameter. Splits on
-- the first @:@; both parts must be non-empty.
parseLangKey :: String -> Either Text (Text, Text)
parseLangKey s =
  case break (== ':') s of
    (langS, ':' : keyS)
      | not (null langS) && not (null keyS) -> Right (T.pack langS, T.pack keyS)
    _ -> Left ("invalid argument '" <> T.pack s <> "'; expected LANG:KEY")

-- | Fold parsed @(lang, key, value)@ triples into a nested map, applying
-- 'combineParam' when the same @lang:key@ appears more than once (so repeated
-- @-X lang:flags=...@ accumulate in order).
foldLangParams :: [(Text, Text, Text)] -> LangParams
foldLangParams = foldl' ins Map.empty
  where
    ins acc (lang, key, val) =
      Map.insertWith
        (\new old -> Map.unionWithKey combineParam old new)
        lang
        (Map.singleton key val)
        acc

-- | Overlay CLI parameters on build-config defaults. The higher-precedence
-- @override@ wins per key, except 'flagsKey', which concatenates
-- @base@ then @override@.
mergeLangParams :: LangParams -> LangParams -> LangParams
mergeLangParams = Map.unionWith (Map.unionWithKey combineParam)

-- | Remove a @lang:key@ entry, pruning a language whose map becomes empty.
unsetParam :: Text -> Text -> LangParams -> LangParams
unsetParam lang key = Map.update prune lang
  where
    prune inner =
      let inner' = Map.delete key inner
       in if Map.null inner' then Nothing else Just inner'

-- | Look up a single @lang:key@ value.
lookupParam :: Text -> Text -> LangParams -> Maybe Text
lookupParam lang key lp = Map.lookup lang lp >>= Map.lookup key

-- | The ordered, non-empty flag tokens for a language ('flagsKey'), in the
-- order they were given.
lookupFlags :: Text -> LangParams -> [Text]
lookupFlags lang lp =
  case lookupParam lang flagsKey lp of
    Nothing -> []
    Just v -> filter (not . T.null) (T.splitOn flagSep v)

-- | Flatten to @(lang, key, value)@ triples in deterministic (sorted) order.
paramEntries :: LangParams -> [(Text, Text, Text)]
paramEntries lp =
  [ (lang, key, val)
  | (lang, inner) <- Map.toAscList lp
  , (key, val) <- Map.toAscList inner
  ]

-- | A deterministic one-line fingerprint of all parameters, for mixing into a
-- build cache key.
renderSalt :: LangParams -> Text
renderSalt = T.intercalate ";" . map renderEntry . paramEntries
  where
    renderEntry (lang, key, val) = lang <> ":" <> key <> "=" <> val
