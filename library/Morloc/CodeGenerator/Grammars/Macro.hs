{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Macro
Description : Expand parameters in concrete types
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Macro
  ( expandMacro
  ) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- | Substitute each `$N` of a native form's template with the rendering of
-- its N-th type argument (NATIVE-1, 1-based, kind arguments not counted).
-- A `$` not followed by digits is text.
expandMacro :: Text -> [Text] -> Text
expandMacro t ps = case T.breakOn "$" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let (digits, after) = T.span isDigit (T.drop 1 rest)
         in if T.null digits
              then pre <> "$" <> expandMacro (T.drop 1 rest) ps
              else pre <> param (read (T.unpack digits)) digits <> expandMacro after ps
  where
    param :: Int -> Text -> Text
    param n digits = case drop (n - 1) ps of
      (x : _) | n >= 1 -> x
      _ -> "$" <> digits
