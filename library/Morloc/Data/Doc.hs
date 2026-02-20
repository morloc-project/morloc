{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Data.Doc
Description : Pretty-printing utilities wrapping prettyprinter
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Re-exports "Prettyprinter" and "Prettyprinter.Render.Text", plus convenience
functions for rendering documents to 'Text', building code blocks, and
performing template-style text substitution.
-}
module Morloc.Data.Doc
  ( module Prettyprinter
  , module Prettyprinter.Render.Text
  , render
  , render'
  , textEsc'
  , tupledNoFold
  , int
  , integer
  , block
  , format
  ) where

import qualified Data.Text as DT
import Prettyprinter hiding (annotate, (<>))
import Prettyprinter.Render.Text

-- | Render a 'Doc' to strict 'DT.Text' using default layout options
render :: Doc ann -> DT.Text
render = renderStrict . layoutPretty defaultLayoutOptions

-- | Render a 'Doc' to 'String' (ignores layout)
render' :: Doc ann -> String
render' = show

-- | Convenience wrapper: @pretty@ specialized to 'Int'
int :: Int -> Doc ann
int = pretty

-- | Convenience wrapper: @pretty@ specialized to 'Integer'
integer :: Integer -> Doc ann
integer = pretty

-- | Format a code block with braces and indentation
block :: Int -> Doc ann -> Doc ann -> Doc ann
block level header body = align . vsep $ [header, "{", indent level body, "}"]

-- | Like 'tupled' but never folds long lines (folding breaks commenting)
tupledNoFold :: [Doc ann] -> Doc ann
tupledNoFold [] = ""
tupledNoFold (x : xs) = parens (foldl (\l r -> l <> "," <+> r) x xs)

-- | Render a 'DT.Text' literal as a double-quoted, escaped 'Doc'
textEsc' :: DT.Text -> Doc ann
textEsc' lit = (dquotes . pretty) $ DT.concatMap escapeChar lit
  where
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = DT.singleton c

-- | Template substitution: split @fmtstr@ on @breaker@ and interleave @replacements@
format ::
  DT.Text -> -- main text with substitution patterns
  DT.Text -> -- break string
  [Doc ann] -> -- replacement strings
  Doc ann
format fmtstr breaker replacements =
  let xs = DT.splitOn breaker fmtstr
   in foldl (<>) (pretty . head $ xs) $ zipWith (\r x -> r <> pretty x) replacements (tail xs)
