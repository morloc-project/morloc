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
  , getMorlocLibrary
) where

import qualified Morloc.Data.Text as MT
import qualified System.Directory as Sys 
import System.FilePath.Posix (combine)

-- append the path
append :: String -> String -> MT.Text
append base path = MT.pack $ combine path base

-- TODO:
--  [ ] use "$HOME/.morloc" as the default, but also read user config
--  [ ] make path construction portable (should work on Windows too)
getMorlocHome :: IO MT.Text
getMorlocHome = fmap (append ".morloc") Sys.getHomeDirectory

getMorlocLibrary :: IO MT.Text
getMorlocLibrary = fmap (append ".morloc/lib") Sys.getHomeDirectory
