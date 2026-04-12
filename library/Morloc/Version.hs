{- |
Module      : Morloc.Version
Description : Store the morloc version
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Version (versionStr) where

import Data.Version (showVersion)
import qualified Paths_morloc (version)

versionStr :: String
versionStr = showVersion Paths_morloc.version
