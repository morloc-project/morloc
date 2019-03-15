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
import qualified System.Directory as Sys 
import System.FilePath.Posix (combine)

-- TODO:
--  [ ] use "$HOME/.morloc/lib" as the default, but also read user config
--  [ ] make path construction portable (should work on Windows too)
getMorlocHome :: IO MT.Text
getMorlocHome = fmap (MT.pack . (flip combine) ".morloc/lib") Sys.getHomeDirectory
