{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Pool
Description : Short description
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pool (generatePoolCode) where

import qualified Database.HSparql.Connection as DHC
import qualified Data.Text as DT

type Lang = DT.Text

generatePoolCode :: DHC.EndPoint -> Lang -> IO DT.Text
generatePoolCode = undefined
