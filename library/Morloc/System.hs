{-|
Module      : Morloc.System
Description : General file system functions
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.System
  ( module System.Directory.Tree
  , module System.Directory
  , module System.FilePath.Posix
  , loadYamlConfig
  ) where

import Morloc.Namespace 

import Data.Aeson (FromJSON(..))
import qualified Data.Yaml.Config as YC
import System.FilePath.Posix
import System.Directory
import System.Directory.Tree

loadYamlConfig ::
     FromJSON a
  => Maybe [String] -- ^ possible locations of the config file 
  -> YC.EnvUsage -- ^ default values taken from the environment (or a hashmap)
  -> IO a -- ^ default configuration
  -> IO a
loadYamlConfig (Just fs) e _ = YC.loadYamlSettings fs [] e
loadYamlConfig Nothing _ d = d
