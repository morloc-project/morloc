{- |
Module      : Morloc.System
Description : Filesystem re-exports and YAML config loading
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Re-exports "System.Directory", "System.Directory.Tree", and
"System.FilePath.Posix" so that other modules can import a single module
for all filesystem operations. Also provides 'loadYamlConfig' for loading
YAML configuration with defaults.
-}
module Morloc.System
  ( module System.Directory.Tree
  , module System.Directory
  , module System.FilePath.Posix
  , loadYamlConfig
  ) where

import Morloc.Namespace.Prim

import Data.Aeson (FromJSON (..))
import qualified Data.Yaml.Config as YC
import System.Directory
import System.Directory.Tree
import System.FilePath.Posix

loadYamlConfig ::
  (FromJSON a) =>
  -- | possible locations of the config file
  Maybe [String] ->
  -- | default values taken from the environment (or a hashmap)
  YC.EnvUsage ->
  -- | default configuration
  IO a ->
  IO a
loadYamlConfig (Just fs) e _ = YC.loadYamlSettings fs [] e
loadYamlConfig Nothing _ d = d
