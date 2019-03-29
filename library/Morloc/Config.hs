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
    Config(..)
  , loadMorlocConfig
  , loadDefaultMorlocConfig 
) where

import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified System.Directory as Sys 
import qualified Morloc.System as MS
import Control.Applicative ((<|>))

import qualified Data.HashMap.Strict as H
import qualified Data.Yaml.Config as YC
import Data.Aeson (withObject, FromJSON(..), (.:?), (.!=))

data Config = Config {
    configHome :: MT.Text
  , configLibrary :: MT.Text
  }
  deriving(Show, Ord, Eq)

instance FromJSON Config where
  parseJSON = withObject "object" $ \o ->
    Config <$> o .:? "home"    .!= ""
           <*> o .:? "library" .!= ""

-- | Get the Morloc home directory (absolute path)
getDefaultMorlocHome :: IO MT.Text
getDefaultMorlocHome = MS.getHomeDirectory |>> MS.appendPath ".morloc"

-- | Get the Morloc library directory (absolute path). Usually this will be a
-- folder inside the home directory.
getDefaultMorlocLibrary :: IO MT.Text
getDefaultMorlocLibrary = MS.getHomeDirectory |>> MS.appendPath ".morloc/lib"

-- | Get the default Morloc YAML config filename
getDefaultMorlocConfig :: IO MT.Text
getDefaultMorlocConfig = MS.getHomeDirectory |>> MS.appendPath ".morloc/config"

defaultFields :: IO (H.HashMap MT.Text MT.Text)
defaultFields = do
  home <- getDefaultMorlocHome
  lib <- getDefaultMorlocLibrary
  return $ H.fromList [ ("home", home), ("library", lib)]

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $ Config (defaults H.! "home") (defaults H.! "library")

-- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
-- default configuration will be used.
loadMorlocConfig :: Maybe MT.Text -> IO Config
loadMorlocConfig f = do
  defaults <- defaultFields
  MS.loadYamlConfig (fmap (\x -> [x]) f)
                    (YC.useCustomEnv defaults)
                    loadDefaultMorlocConfig
