{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Config
Description : Handle local configuration
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Config (
    getMorlocHome
) where

import qualified Morloc.Data.Text as MT

-- FIXME: HARDCODED NAME FOR LOCAL TESTING
--  [ ] make relative to users home
--  [ ] make path construction portable
getMorlocHome :: MT.Text
getMorlocHome = "/home/z/.morloc/lib"
