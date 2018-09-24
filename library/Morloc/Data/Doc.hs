{-# LANGUAGE OverloadedStrings #-}

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
  ( 
      module Text.PrettyPrint.Leijen.Text
    , render
    , render'
    , text'
    , textEsc'
  ) where

import Text.PrettyPrint.Leijen.Text
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DL

render :: Doc -> DT.Text
render = displayTStrict . renderPretty 0.5 70

render' :: Doc -> String
render' = DT.unpack . render

text' :: DT.Text -> Doc
text' = text . DL.fromStrict

textEsc' :: DT.Text -> Doc
textEsc' lit = (dquotes . string . DL.fromStrict) $ DT.concatMap escapeChar lit where
  escapeChar '\n' = "\\n"
  escapeChar '\t' = "\\t"
  escapeChar '\r' = "\\r"
  escapeChar '"'  = "\\\""
  escapeChar '\\' = "\\\\"
  escapeChar c    = DT.singleton c
