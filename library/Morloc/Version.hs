{-|
Module      : Morloc.Version
Description : Store the morloc version
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Version ( version, versionStr ) where

version :: (Int, Int, Int)
version = (0, 54, 0)

versionStr :: String
versionStr = case version of
  (major,minor,patch) -> show major ++ "." ++ show minor ++ "." ++ show patch
