{-# LANGUAGE OverloadedStrings #-} 

{-|
Module      : Morloc.Data.Text
Description : All things text
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This is a general wrapper around all textual representations in Morloc.
-}
module Morloc.Data.Text
  ( module Data.Text
  , module Data.Text.IO
  , module Data.Text.Encoding
  , show'
  , pretty
  , read'
  , readMay'
  , parseTSV
  , unparseTSV
  , unenclose
  , unangle
  , unquote
  , undquote
  , stripPrefixIfPresent
  , liftToText
  ) where

import Data.Text hiding (map)
import Data.Text.Encoding
import Data.Text.IO
import qualified Data.Text.Lazy as DL
import Prelude hiding (concat, length, lines, unlines)
import qualified Safe
import qualified Text.Pretty.Simple as Pretty

show' :: Show a => a -> Text
show' = pack . show

read' :: Read a => Text -> a
read' = read . unpack

readMay' :: Read a => Text -> Maybe a
readMay' = Safe.readMay . unpack

stripPrefixIfPresent :: Text -> Text -> Text
stripPrefixIfPresent prefix text =
  case stripPrefix prefix text of
    (Just x) -> x
    Nothing -> text

pretty :: Show a => a -> Text
pretty = DL.toStrict . Pretty.pShowNoColor

-- | Parse a TSV, ignore first line (header). Cells are also unquoted and
-- wrapping angles are removed.
parseTSV :: Text -> [[Maybe Text]]
parseTSV =
  map (map (nonZero . undquote . unangle)) .
  map (split ((==) '\t')) . Prelude.tail . lines

liftToText :: (String -> String) -> Text -> Text
liftToText f = pack . f . unpack

-- | Make a TSV text
unparseTSV :: [[Maybe Text]] -> Text
unparseTSV = unlines . map renderRow
  where
    renderRow :: [Maybe Text] -> Text
    renderRow = intercalate "\t" . map renderCell
    renderCell :: Maybe Text -> Text
    renderCell (Nothing) = "-"
    renderCell (Just x) = x

nonZero :: Text -> Maybe Text
nonZero s =
  if length s == 0
    then Nothing
    else Just s

unenclose :: Text -> Text -> Text -> Text
unenclose a b x = maybe x id (stripPrefix a x >>= stripSuffix b)

unangle :: Text -> Text
unangle = unenclose "<" ">"

unquote :: Text -> Text
unquote = unenclose "'" "'"

undquote :: Text -> Text
undquote = unenclose "\"" "\""
