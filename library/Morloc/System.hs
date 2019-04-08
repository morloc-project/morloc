{-# LANGUAGE OverloadedStrings #-}

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
      makeManifoldName
    , makePoolName
    , loadYamlConfig
    , getHomeDirectory
    , appendPath
  ) where

import Morloc.Types
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

import qualified System.Directory as Sys 
import System.FilePath.Posix (combine)
import qualified Data.Yaml.Config as YC
import Data.Aeson (FromJSON(..))

-- | Append POSIX paths encoded as Text
appendPath :: MT.Text -> MT.Text -> MT.Text
appendPath base path = MT.pack $ combine (MT.unpack path) (MT.unpack base)

makePoolName :: MT.Text -> MT.Text
makePoolName lang = "pool." <> lang

makeManifoldName :: MT.Text -> MorlocMonad MT.Text
makeManifoldName x = case reverse (MT.splitOn "/" x) of
  (y:_) -> return $ "m" <> y
  _ -> MM.throwError . InvalidRDF $ "Manifold uri does not match the pattern `.*/\\d+$`"

getHomeDirectory :: IO MT.Text
getHomeDirectory = fmap MT.pack Sys.getHomeDirectory

loadYamlConfig :: FromJSON a
  => Maybe [MT.Text] -- ^ possible locations of the config file 
  -> YC.EnvUsage -- ^ default values taken from the environment (or a hashmap)
  -> IO a -- ^ default configuration
  -> IO a 
loadYamlConfig (Just fs) e _ = YC.loadYamlSettings (map MT.unpack fs) [] e
loadYamlConfig Nothing   _ d = d
