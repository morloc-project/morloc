{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Data.Text
Description : All things text
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This is a general wrapper around all textual representations in Morloc.
-}

module Morloc.Data.Text
  ( 
      module Data.Text 
    , module Data.Text.IO
    , module Data.Text.Encoding
    , show'
    , pretty
    , read'
    , readMay'
    , parseTSV
    , unenclose
    , unangle
    , unquote
    , undquote
  ) where

import Prelude hiding (lines, length)
import Data.Text hiding (map)
import Data.Text.IO
import Data.Text.Encoding
import qualified Data.Text.Lazy as DL
import qualified Safe
import qualified Text.Pretty.Simple as Pretty 

show' :: Show a => a -> Text
show' = pack . show

read' :: Read a => Text -> a
read' =  read . unpack

readMay' :: Read a => Text -> Maybe a
readMay' = Safe.readMay . unpack

pretty :: Show a => a -> Text
pretty = DL.toStrict . Pretty.pShowNoColor

-- | Parse a TSV, ignore first line (header). Cells are also unquoted and
-- wrapping angles are removed.
parseTSV :: Text -> [[Maybe Text]]
parseTSV
  = map (map (nonZero . undquote . unangle))
  . map (split ((==) '\t'))
  . Prelude.tail
  . lines

nonZero :: Text -> Maybe Text
nonZero s =
  if
    length s == 0
  then
    Nothing
  else 
    Just s

unenclose :: Text -> Text -> Text -> Text
unenclose a b x = maybe x id (stripPrefix a x >>= stripSuffix b)

unangle :: Text -> Text
unangle = unenclose "<" ">"

unquote :: Text -> Text
unquote = unenclose "'" "'"

undquote :: Text -> Text
undquote = unenclose "\"" "\""
