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
    , show'
    , pretty
    , read'
    , readMay'
  ) where

import Data.Text
import Data.Text.IO
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
