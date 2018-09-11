{-|
Module      : Morloc.Builder
Description : A wrapper around Leijen's text builder
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module re-exports Leijen's text builder along with a few other utilities.
-}

module Morloc.Builder
  ( 
      module Text.PrettyPrint.Leijen.Text
    , render
    , render'
    , text'
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
