{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Data.Doc
Description : A wrapper around prettyprint
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module re-exports Leijen's text builder along with a few other utilities.
-}

module Morloc.Data.Doc
  ( module Prettyprinter
  , module Prettyprinter.Render.Text
  , render
  , render'
  , textEsc'
  , tupledNoFold
    -- ** These are not strictly necessary, since @pretty@ could be used, but
    -- they avoid the requirements of an explicity type signature.
  , int
  , integer
  , block
  , format
  ) where

import qualified Data.Text as DT
import Prettyprinter hiding ((<>), annotate)
import Prettyprinter.Render.Text

render :: Doc ann -> DT.Text
render = renderStrict . layoutPretty defaultLayoutOptions

render' :: Doc ann -> String
render' = show -- NOTE: This ignores layouts

int :: Int -> Doc ann
int = pretty

integer :: Integer -> Doc ann
integer = pretty

block :: Int -> Doc ann -> Doc ann -> Doc ann
block level header body = align . vsep $ [header, "{", indent level body, "}"]

-- | a tupled function that does not fold long lines (folding breaks commenting)
tupledNoFold :: [Doc ann] -> Doc ann
tupledNoFold [] = ""
tupledNoFold (x:xs) = parens (foldl (\l r -> l <> "," <+> r) x xs)

textEsc' :: DT.Text -> Doc ann
textEsc' lit = (dquotes . pretty) $ DT.concatMap escapeChar lit
  where
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = DT.singleton c

format
  :: DT.Text -- main text with substitution patterns
  -> DT.Text -- break string
  -> [Doc ann] -- replacement strings
  -> Doc ann
format fmtstr breaker replacements =
  let xs = DT.splitOn breaker fmtstr
  in foldl (<>) (pretty . head $ xs) $ zipWith (\r x -> r <> pretty x) replacements (tail xs)
