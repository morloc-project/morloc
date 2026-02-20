{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Data.Text
Description : Text utilities and re-exports
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Re-exports "Data.Text", "Data.Text.IO", and "Data.Text.Encoding", plus
conversion helpers ('show'', 'read'') and string-stripping utilities
('unenclose', 'unangle', 'unquote', 'undquote').
-}
module Morloc.Data.Text
  ( module Data.Text
  , module Data.Text.IO
  , module Data.Text.Encoding
  , show'
  , pretty
  , read'
  , readMay'
  , unenclose
  , unangle
  , unquote
  , undquote
  , stripPrefixIfPresent
  , liftToText
  ) where

import Data.Maybe (fromMaybe)
import Data.Text hiding (map)
import Data.Text.Encoding
import Data.Text.IO
import qualified Data.Text.Lazy as DL
import qualified Safe
import qualified Text.Pretty.Simple as Pretty
import Prelude hiding (concat, length, lines, unlines)

-- | 'show' producing 'Text' instead of 'String'
show' :: (Show a) => a -> Text
show' = pack . Prelude.show

-- | 'read' accepting 'Text' instead of 'String'
read' :: (Read a) => Text -> a
read' = read . unpack

-- | Safe 'read' from 'Text', returning 'Nothing' on parse failure
readMay' :: (Read a) => Text -> Maybe a
readMay' = Safe.readMay . unpack

-- | Strip a prefix if present, otherwise return the text unchanged
stripPrefixIfPresent :: Text -> Text -> Text
stripPrefixIfPresent prefix text =
  case stripPrefix prefix text of
    (Just x) -> x
    Nothing -> text

-- | Pretty-print any 'Show' instance to 'Text' (no color)
pretty :: (Show a) => a -> Text
pretty = DL.toStrict . Pretty.pShowNoColor

-- | Lift a @String -> String@ function to operate on 'Text'
liftToText :: (String -> String) -> Text -> Text
liftToText f = pack . f . unpack

-- | Strip matching open\/close delimiters from a text value
unenclose :: Text -> Text -> Text -> Text
unenclose a b x = fromMaybe x (stripPrefix a x >>= stripSuffix b)

-- | Strip surrounding angle brackets: @\<foo\>@ -> @foo@
unangle :: Text -> Text
unangle = unenclose "<" ">"

-- | Strip surrounding single quotes: @\'foo\'@ -> @foo@
unquote :: Text -> Text
unquote = unenclose "'" "'"

-- | Strip surrounding double quotes: @\"foo\"@ -> @foo@
undquote :: Text -> Text
undquote = unenclose "\"" "\""
