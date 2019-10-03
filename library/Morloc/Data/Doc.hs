{-|
Module      : Morloc.Data.Doc
Description : A wrapper around Leijen's text builder
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module re-exports Leijen's text builder along with a few other utilities.
-}
module Morloc.Data.Doc
  ( module Data.Text.Prettyprint.Doc
  , module Data.Text.Prettyprint.Doc.Render.Text
  , putDoc
  , render
  , render'
  , textEsc'
  , tupledNoFold
    -- ** These are not strictly necessary, since @pretty@ could be used, but
    -- they avoid the requirements of an explicity type signature.
  , int
  , integer
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DL
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Text

render :: Doc ann -> DT.Text
render = renderStrict . layoutPretty defaultLayoutOptions

render' :: Doc ann -> String
render' = show -- NOTE: This ignores layouts

int :: Int -> Doc ann
int = pretty

integer :: Integer -> Doc ann
integer = pretty

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
