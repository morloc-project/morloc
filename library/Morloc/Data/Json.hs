{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Data.Json
Description : Lightweight JSON builder utilities
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
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

jsonStr :: Text -> Text
jsonStr t = "\"" <> jsonEscape t <> "\""

jsonInt :: Int -> Text
jsonInt = MT.pack . show

jsonBool :: Bool -> Text
jsonBool True = "true"
jsonBool False = "false"

jsonNull :: Text
jsonNull = "null"

jsonArr :: [Text] -> Text
jsonArr xs = "[" <> MT.intercalate "," xs <> "]"

jsonObj :: [(Text, Text)] -> Text
jsonObj pairs = "{" <> MT.intercalate "," [jsonStr k <> ":" <> v | (k, v) <- pairs] <> "}"

jsonStrArr :: [Text] -> Text
jsonStrArr = jsonArr . map jsonStr

jsonMaybeStr :: Maybe Text -> Text
jsonMaybeStr Nothing = jsonNull
jsonMaybeStr (Just t) = jsonStr t
