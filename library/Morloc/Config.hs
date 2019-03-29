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
  , getExecutor
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
  , configLangPython3 :: MT.Text
  , configLangR :: MT.Text
  , configLangPerl :: MT.Text
  }
  deriving(Show, Ord, Eq)

-- FIXME: remove this chronic multiplication
instance FromJSON Config where
  parseJSON = withObject "object" $ \o ->
    Config <$> o .:? "home"    .!= ""
           <*> o .:? "library" .!= ""
           <*> o .:? "lang_python3" .!= "python"
           <*> o .:? "lang_R" .!= "Rscript"
           <*> o .:? "lang_perl" .!= "perl"

defaultFields :: IO (H.HashMap MT.Text MT.Text)
defaultFields = do
  home <- getDefaultMorlocHome
  lib <- getDefaultMorlocLibrary
  return $ H.fromList
    [ ("home", home)
    , ("library", lib)
    , ("lang_python3", "python")
    , ("lang_R", "Rscript")
    , ("lang_perl", "perl")
    ]

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $ Config
    (defaults H.! "home")
    (defaults H.! "library")
    (defaults H.! "lang_python3")
    (defaults H.! "lang_R")
    (defaults H.! "lang_perl")

getExecutor :: Config -> MT.Text -> Maybe MT.Text
getExecutor c "R"    = Just $ configLangR c
getExecutor c "py"   = Just $ configLangPython3 c
getExecutor c "perl" = Just $ configLangPerl c
getExecutor _ _      = Nothing

-- | Get the Morloc home directory (absolute path)
getDefaultMorlocHome :: IO MT.Text
getDefaultMorlocHome = MS.getHomeDirectory |>> MS.appendPath ".morloc"

-- | Get the Morloc library directory (absolute path). Usually this will be a
-- folder inside the home directory.
getDefaultMorlocLibrary :: IO MT.Text
getDefaultMorlocLibrary = MS.getHomeDirectory |>> MS.appendPath ".morloc/lib"

-- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
-- default configuration will be used.
loadMorlocConfig :: Maybe MT.Text -> IO Config
loadMorlocConfig f = do
  defaults <- defaultFields
  MS.loadYamlConfig (fmap (\x -> [x]) f)
                    (YC.useCustomEnv defaults)
                    loadDefaultMorlocConfig
