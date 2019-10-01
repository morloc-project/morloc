{-|
Module      : Morloc.System
Description : Handle dependencies and environment setup
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.System
  ( 
      loadYamlConfig
    , getHomeDirectory
    , appendPath
    , takeDirectory
    , takeFileName
    , combine
  ) where

import qualified Morloc.Data.Text as MT

import qualified System.Directory as Sys 
import qualified System.FilePath.Posix as Path
import qualified Data.Yaml.Config as YC
import Data.Aeson (FromJSON(..))

combine :: MT.Text -> MT.Text -> MT.Text
combine x y = MT.pack $ Path.combine (MT.unpack x) (MT.unpack y)

takeDirectory :: MT.Text -> MT.Text
takeDirectory x = MT.pack $ Path.takeDirectory (MT.unpack x)

takeFileName :: MT.Text -> MT.Text
takeFileName x = MT.pack $ Path.takeFileName (MT.unpack x)

-- | Append POSIX paths encoded as Text
appendPath :: MT.Text -> MT.Text -> MT.Text
appendPath base path = combine path base

getHomeDirectory :: IO MT.Text
getHomeDirectory = fmap MT.pack Sys.getHomeDirectory

loadYamlConfig :: FromJSON a
  => Maybe [MT.Text] -- ^ possible locations of the config file 
  -> YC.EnvUsage -- ^ default values taken from the environment (or a hashmap)
  -> IO a -- ^ default configuration
  -> IO a 
loadYamlConfig (Just fs) e _ = YC.loadYamlSettings (map MT.unpack fs) [] e
loadYamlConfig Nothing _ d = d
