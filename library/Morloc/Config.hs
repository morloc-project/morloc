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
  , loadConfig
  , defaultConfig 
) where


import qualified Morloc.Data.Text as MT
import qualified System.Directory as Sys 
import Control.Applicative ((<|>))
import System.FilePath.Posix (combine)

import qualified Data.Yaml.Config as YC
import qualified Data.HashMap.Strict as H
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

-- append the path
append :: String -> String -> MT.Text
append base path = MT.pack $ combine path base

getDefaultMorlocHome :: IO MT.Text
getDefaultMorlocHome = fmap (append ".morloc") Sys.getHomeDirectory

getDefaultMorlocLibrary :: IO MT.Text
getDefaultMorlocLibrary = fmap (append ".morloc/lib") Sys.getHomeDirectory

getDefaultMorlocConfig :: IO MT.Text
getDefaultMorlocConfig = fmap (append ".morloc/config") Sys.getHomeDirectory

defaultFields :: IO (H.HashMap MT.Text MT.Text)
defaultFields = do
  home <- getDefaultMorlocHome
  lib <- getDefaultMorlocLibrary
  return $ H.fromList [ ("home", home), ("library", lib)]

defaultConfig :: IO Config
defaultConfig = do
  defaults <- defaultFields
  return $ Config (defaults H.! "home") (defaults H.! "library")

loadConfig :: Maybe MT.Text -> IO Config
loadConfig (Just f) = do
  defaults <- defaultFields
  YC.loadYamlSettings [MT.unpack f] [] (YC.useCustomEnv defaults)
loadConfig Nothing = do
  defaults <- defaultFields
  defaultPath <- getDefaultMorlocConfig 
  fileExists <- Sys.doesFileExist (MT.unpack defaultPath)
  if fileExists
  then YC.loadYamlSettings [MT.unpack defaultPath] [] (YC.useCustomEnv defaults)
  else defaultConfig
