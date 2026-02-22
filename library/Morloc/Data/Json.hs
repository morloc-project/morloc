{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Data.Json
Description : Lightweight JSON builder utilities
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Minimal JSON text builders for generating manifest files and other JSON
output without depending on aeson for serialization. Each function produces
a 'Text' fragment that is valid JSON.
-}
module Morloc.Data.Json
  ( jsonEscape
  , jsonStr
  , jsonInt
  , jsonBool
  , jsonNull
  , jsonArr
  , jsonObj
  , jsonStrArr
  , jsonMaybeStr
  ) where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as MT
import Numeric (showHex)

-- | Escape a text value for inclusion in a JSON string (without surrounding quotes)
jsonEscape :: Text -> Text
jsonEscape = MT.concatMap esc
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc '\b' = "\\b"
    esc '\f' = "\\f"
    esc c | c < ' ' = "\\u" <> MT.pack (pad4 (showHex (ord c) ""))
    esc c = MT.singleton c

    pad4 s = replicate (4 - length s) '0' ++ s

-- | Wrap a text value as a JSON string (escaped and double-quoted)
jsonStr :: Text -> Text
jsonStr t = "\"" <> jsonEscape t <> "\""

-- | Render an 'Int' as a JSON number
jsonInt :: Int -> Text
jsonInt = MT.pack . show

-- | Render a 'Bool' as a JSON boolean
jsonBool :: Bool -> Text
jsonBool True = "true"
jsonBool False = "false"

-- | The JSON null literal
jsonNull :: Text
jsonNull = "null"

-- | Render a list of pre-formatted JSON values as a JSON array
jsonArr :: [Text] -> Text
jsonArr xs = "[" <> MT.intercalate "," xs <> "]"

-- | Render key-value pairs as a JSON object (values are pre-formatted JSON)
jsonObj :: [(Text, Text)] -> Text
jsonObj pairs = "{" <> MT.intercalate "," [jsonStr k <> ":" <> v | (k, v) <- pairs] <> "}"

-- | Render a list of text values as a JSON array of strings
jsonStrArr :: [Text] -> Text
jsonStrArr = jsonArr . map jsonStr

-- | Render 'Nothing' as @null@, 'Just' as a JSON string
jsonMaybeStr :: Maybe Text -> Text
jsonMaybeStr Nothing = jsonNull
jsonMaybeStr (Just t) = jsonStr t
