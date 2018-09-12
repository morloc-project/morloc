{-|
Module      : Morloc.Text
Description : All things text
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This is a general wrapper around all textual representations in Morloc.
-}

module Morloc.Text
  ( 
      module Data.Text 
    , DTI.readFile
    , DTI.writeFile
    , show'
    , pretty
    , read'
    , readMay'
  ) where

import Data.Text
import qualified Data.Text.Lazy as DL
import qualified Data.Text.IO as DTI
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
