{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
  , getDefaultMorlocHome
  , getDefaultMorlocLibrary
  , getDefaultMorlocConfig
) where

import Morloc.Types
import qualified Morloc.Data.Text as MT
import qualified System.Directory as Sys 
import System.FilePath.Posix (combine)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson

data Config = Config {
    configHome :: MT.Text
  , configLibrary :: MT.Text
  }
  deriving(Show, Generic)

instance FromJSON Config

-- append the path
append :: String -> String -> MT.Text
append base path = MT.pack $ combine path base

getDefaultMorlocHome :: IO MT.Text
getDefaultMorlocHome = fmap (append ".morloc") Sys.getHomeDirectory

getDefaultMorlocLibrary :: IO MT.Text
getDefaultMorlocLibrary = fmap (append ".morloc/lib") Sys.getHomeDirectory

getDefaultMorlocConfig :: IO MT.Text
getDefaultMorlocConfig = fmap (append ".morloc/config") Sys.getHomeDirectory

-- | Load the config file (if it exists) or build a default one for this system
loadConfig :: Maybe MT.Text -> IO Config
loadConfig (Just f) = readConfigFile f
loadConfig Nothing = do
  defaultConfigFile <- getDefaultMorlocConfig 
  defaultExists <- Sys.doesFileExist (MT.unpack defaultConfigFile)
  if defaultExists
  then readConfigFile defaultConfigFile
  else makeDefaultConfig

-- | Build a default config for this system when no file is found
makeDefaultConfig :: IO Config
makeDefaultConfig = do
  home <- getDefaultMorlocHome
  lib  <- getDefaultMorlocLibrary
  return $ Config {
        configHome    = home
      , configLibrary = lib
    }

-- FIXME: make this type (IO (ThrowErrors Config)), and add YAML parse error type
-- | Read a config file
readConfigFile :: MT.Text -> IO Config
readConfigFile f = do
    content <- BS.readFile (MT.unpack f)
    let configContent = Y.decodeEither' content
    case configContent of
        (Left err) -> error (show err)
        (Right c) -> return c
