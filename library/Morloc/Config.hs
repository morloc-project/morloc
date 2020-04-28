{-|
Module      : Morloc.Config
Description : Handle local configuration
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Config
  ( Config(..)
  , loadMorlocConfig
  , loadDefaultMorlocConfig
  , buildPoolCallBase
  , getDefaultConfigFilepath
  , getDefaultMorlocTmpDir
  , makeLibSourceString
  ) where

import Data.Aeson (FromJSON(..), (.!=), (.:?), withObject)
import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Language as ML
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml.Config as YC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import Morloc.Pretty ()

getDefaultConfigFilepath :: IO Path
getDefaultConfigFilepath =
  MS.getHomeDirectory |>> MS.appendPath (Path ".morloc/config")

instance FromJSON Path where
  parseJSON = fmap Path . parseJSON

-- FIXME: remove this chronic multiplication
instance FromJSON Config where
  parseJSON =
    withObject "object" $ \o ->
      Config
        <$> fmap Path (o .:? "home" .!= "$HOME/.morloc")
        <*> fmap Path (o .:? "library" .!= "$HOME/.morloc/lib")
        <*> fmap Path (o .:? "tmpdir" .!= "$HOME/.morloc/tmp" )
        <*> fmap Path (o .:? "lang_python3" .!= "python3")
        <*> fmap Path (o .:? "lang_R" .!= "Rscript")
        <*> fmap Path (o .:? "lang_perl" .!= "perl")

-- | Load the default Morloc configuration, ignoring any local configurations.
loadDefaultMorlocConfig :: IO Config
loadDefaultMorlocConfig = do
  defaults <- defaultFields
  return $
    Config
      (Path $ defaults H.! "home")
      (Path $ defaults H.! "library")
      (Path $ defaults H.! "tmpdir")
      (Path "python") -- lang_python3
      (Path "Rscript") -- lang_R
      (Path "perl") -- lang_perl

-- | Load a Morloc config file. If no file is given (i.e., Nothing), then the
-- default configuration will be used.
loadMorlocConfig :: Maybe Path -> IO Config
loadMorlocConfig Nothing = do
  defaults <- defaultFields
  MS.loadYamlConfig
    Nothing 
    (YC.useCustomEnv defaults)
    loadDefaultMorlocConfig
loadMorlocConfig (Just configFile) = do
  configExists <- MS.fileExists configFile
  defaults <- defaultFields
  if configExists
    then
      MS.loadYamlConfig
        (Just [configFile])
        (YC.useCustomEnv defaults)
        loadDefaultMorlocConfig
    else
      loadMorlocConfig Nothing 

-- | Create the base call to a pool (without arguments)
-- For example:
--   ./pool.R 1 --
--   ./pool.py 1 --
--   ./pool-cpp.out 1 --
--   ./pool.R 1 [1,2,3] true
buildPoolCallBase
  :: Config
  -> Lang
  -> Int
  -> Maybe [MDoc]
buildPoolCallBase _ CLang i =
  Just ["./" <> pretty (ML.makeExecutableName CLang "pool"), pretty i]
buildPoolCallBase _ CppLang i =
  Just ["./" <> pretty (ML.makeExecutableName CppLang "pool"), pretty i]
buildPoolCallBase c RLang i =
  Just [pretty (configLangR c), pretty (ML.makeExecutableName RLang "pool"), pretty i]
buildPoolCallBase c Python3Lang i =
  Just [pretty (configLangPython3 c), pretty (ML.makeExecutableName Python3Lang "pool"), pretty i]
buildPoolCallBase _ _ _ = Nothing -- FIXME: add error handling

-- A key value map
defaultFields :: IO (H.HashMap MT.Text MT.Text)
defaultFields = do
  home <- fmap unPath getDefaultMorlocHome
  lib <- fmap unPath getDefaultMorlocLibrary
  tmp <- fmap unPath getDefaultMorlocTmpDir
  return $ H.fromList [("home", home), ("library", lib), ("tmpdir", tmp)]

-- | Get the Morloc home directory (absolute path)
getDefaultMorlocHome :: IO Path
getDefaultMorlocHome = MS.getHomeDirectory |>> MS.appendPath (Path ".morloc")

-- | Get the Morloc library directory (absolute path). Usually this will be a
-- folder inside the home directory.
getDefaultMorlocLibrary :: IO Path
getDefaultMorlocLibrary = MS.getHomeDirectory |>> MS.appendPath (Path ".morloc/lib")

-- | Get the Morloc default temporary directory. This will store generated
-- SPARQL queries and rdf dumps that can be used in debugging.
getDefaultMorlocTmpDir :: IO Path
getDefaultMorlocTmpDir = MS.getHomeDirectory |>> MS.appendPath (Path ".morloc/tmp")

-- | Get a source string for a library module. This will 1) remove the
-- user-specific home directory and 2) replace '/' separators with '.'. An
-- input of Nothing indicates the input is a local file or STDIN.
makeLibSourceString :: Maybe Path -> MorlocMonad (Maybe Path)
makeLibSourceString (Just (Path x)) = do
  homedir <- liftIO getDefaultMorlocLibrary
  let x' = maybe x id (MT.stripPrefix (unPath homedir) x)
  let x'' = maybe x' id (MT.stripPrefix "/" x')
  return . Just . Path . MT.replace "/" "__" . MT.replace "." "_" $ x''
makeLibSourceString Nothing = return Nothing
