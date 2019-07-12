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
  , getPoolCallBuilder
  , getDefaultConfigFilepath
  , getDefaultMorlocTmpDir
  , makeLibSourceString
) where

import Morloc.Global
import Morloc.Operators hiding ((<>))
import Morloc.Data.Doc
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
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

-- | Attempt to create a function for building a call to a pool. The call is
-- represented as a list of arguments for a command line.
getPoolCallBuilder
  :: Config
  -> Lang
  -> (MDoc -> MDoc) -- ^ a function for quoting a string
  -> Maybe (MDoc -> MDoc -> [MDoc])
getPoolCallBuilder _ CLang       q = Just $ (\n i -> [ q ("./" <> n), q i])
getPoolCallBuilder _ CppLang     q = Just $ (\n i -> [ q ("./" <> n), q i])
getPoolCallBuilder c RLang       q = Just $ makeCmdPoolCall q (configLangR c)
getPoolCallBuilder c Python3Lang q = Just $ makeCmdPoolCall q (configLangPython3 c)
getPoolCallBuilder c PerlLang    q = Just $ makeCmdPoolCall q (configLangPerl c)
getPoolCallBuilder _ MorlocLang  _ = Nothing -- FIXME: add error handling

-- Build a simple pool call for an interpreted language
makeCmdPoolCall :: (MDoc -> MDoc) -> MT.Text -> (MDoc -> MDoc -> [MDoc])
makeCmdPoolCall q prog name i = [q (pretty prog), q name, q i]

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
  return . Just . MT.replace "/" "__" . MT.replace "." "_" $ x''
makeLibSourceString Nothing = return Nothing
