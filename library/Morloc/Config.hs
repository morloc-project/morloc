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
  , getDefaultConfigFilepath
  , getDefaultMorlocTmpDir
  , makeLibSourceString
) where

import Morloc.Global
import Morloc.Operators
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified System.Directory as Sys 
import qualified Morloc.System as MS

import qualified Data.HashMap.Strict as H
import qualified Data.Yaml.Config as YC
import Data.Aeson (withObject, FromJSON(..), (.:?), (.!=))

getDefaultConfigFilepath :: IO MT.Text
getDefaultConfigFilepath = MS.getHomeDirectory |>> MS.appendPath ".morloc/config"

-- FIXME: remove this chronic multiplication
instance FromJSON Config where
  parseJSON = withObject "object" $ \o ->
    Config <$> o .:? "home"    .!= ""
           <*> o .:? "library" .!= ""
           <*> o .:? "tmpdir" .!= ""
           <*> o .:? "lang_python3" .!= "python"
           <*> o .:? "lang_R" .!= "Rscript"
           <*> o .:? "lang_perl" .!= "perl"

defaultFields :: IO (H.HashMap MT.Text MT.Text)
defaultFields = do
  home <- getDefaultMorlocHome
  lib <- getDefaultMorlocLibrary
  tmp <- getDefaultMorlocTmpDir
  return $ H.fromList
    [ ("home", home)
    , ("library", lib)
    , ("tmpdir", tmp)
    ]

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $ Config
    (defaults H.! "home")
    (defaults H.! "library")
    (defaults H.! "tmpdir")
    "python"  -- lang_python3
    "Rscript" -- lang_R
    "perl"    -- lang_perl

getExecutor :: Config -> MT.Text -> Maybe MT.Text
getExecutor _ "c"    = Just $ "." -- for compiled programs, we just call the executable
getExecutor _ "C"    = Just $ "." -- for compiled programs, we just call the executable
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

-- | Get the Morloc default temporary directory. This will store generated
-- SPARQL queries and rdf dumps that can be used in debugging.
getDefaultMorlocTmpDir :: IO MT.Text
getDefaultMorlocTmpDir = MS.getHomeDirectory |>> MS.appendPath ".morloc/tmp"

-- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
-- default configuration will be used.
loadMorlocConfig :: Maybe MT.Text -> IO Config
loadMorlocConfig f = do
  defaults <- defaultFields
  MS.loadYamlConfig (fmap (\x -> [x]) f)
                    (YC.useCustomEnv defaults)
                    loadDefaultMorlocConfig

-- | Get a source string for a library module. This will 1) remove the
-- user-specific home directory and 2) replace '/' separators with '.'. An
-- input of Nothing indicates the input is a local file or STDIN.
makeLibSourceString :: Maybe MT.Text -> MorlocMonad (Maybe MT.Text)
makeLibSourceString (Just x) = do
  homedir <- MM.liftIO getDefaultMorlocLibrary
  let x' = case (MT.stripPrefix homedir x) of {Nothing -> x; (Just y) -> y}
  let x'' = case (MT.stripPrefix "/" x') of {Nothing -> x'; (Just y) -> y}
  return . Just $ MT.replace "/" "__" x''
makeLibSourceString Nothing = return Nothing
