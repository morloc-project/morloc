{- |
Module      : Morloc.Version
Description : Store the morloc version
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Version (version, versionStr) where

version :: (Int, Int, Int)
version = (0, 60, 0)

versionStr :: String
versionStr = case version of
  (major, minor, patch) -> show major ++ "." ++ show minor ++ "." ++ show patch
