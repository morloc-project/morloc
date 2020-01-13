{-|
Module      : Module
Description : Morloc module imports and paths 
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Module
  ( ModuleSource(..)
  , installModule
  , findModule
  , loadModuleMetadata
  ) where

import Morloc.Namespace
import Morloc.Data.Doc
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified System.Directory as SD

import Data.Aeson (FromJSON(..), (.!=), (.:?), withObject)
import qualified Data.Yaml.Config as YC

instance FromJSON PackageMeta where
  parseJSON = withObject "object" $ \o ->
    PackageMeta <$> o .:? "name"        .!= ""
                <*> o .:? "version"     .!= ""
                <*> o .:? "homepage"    .!= ""
                <*> o .:? "synopsis"    .!= ""
                <*> o .:? "description" .!= ""
                <*> o .:? "category"    .!= ""
                <*> o .:? "license"     .!= ""
                <*> o .:? "author"      .!= ""
                <*> o .:? "maintainer"  .!= ""
                <*> o .:? "github"      .!= ""
                <*> o .:? "bug-reports" .!= ""
                <*> o .:? "gcc-flags"   .!= ""

-- | Specify where a module is located 
data ModuleSource
  = LocalModule (Maybe MT.Text)
  -- ^ A module in the working directory
  | GithubRepo MT.Text
  -- ^ A module stored in an arbitrary Github repo: "<username>/<reponame>"
  | CoreGithubRepo MT.Text
  -- ^ The repo name of a core package, e.g., "math"

-- | Look for a local morloc module.
findModule :: MVar -> MorlocMonad Path
findModule moduleName = do
  config <- MM.ask
  let lib = Config.configLibrary config
  let allPaths = getModulePaths lib moduleName
  existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
  case existingPaths of
    (x:_) -> return x
    [] ->
      MM.throwError . CannotLoadModule . render $
        "module not found among the paths:" <+> list (map pretty allPaths)

-- | Give a module path (e.g. "/your/path/foo.loc") find the package metadata.
-- It currently only looks for a file named "package.yaml" in the same folder
-- as the main "*.loc" file. 
findModuleMetadata :: Path -> IO (Maybe Path)
findModuleMetadata mainFile =
  getFile $ MS.combine (MS.takeDirectory mainFile) (Path "package.yaml")

loadModuleMetadata :: Path -> MorlocMonad ()
loadModuleMetadata main = do
  maybef <- liftIO $ findModuleMetadata main
  meta <-
    case maybef of
      (Just f) -> liftIO $ YC.loadYamlSettings [MT.unpack . unPath $ f] [] YC.ignoreEnv
      Nothing -> return defaultPackageMeta
  state <- MM.get
  MM.put (appendMeta meta state)
  where
    appendMeta :: PackageMeta -> MorlocState -> MorlocState
    appendMeta m s = s {statePackageMeta = m : (statePackageMeta s)}

-- | Find an ordered list of possible locations to search for a module
getModulePaths :: Path -> MVar -> [Path]
getModulePaths (Path lib) (MVar base) = map Path
  [ base <> ".loc"                              -- "./${base}.loc"
  , base <> "/" <> "main.loc"                   -- "${base}/main.loc"
  , lib <> "/" <> base <> ".loc"                -- "${LIB}/${base}.loc"
  , lib <> "/" <> base <> "/" <> "main.loc"     -- "${LIB}/${base}/main.loc"
  , lib <> "/" <> base <> "/" <> base <> ".loc" -- "${LIB}/${base}/${base}.loc"
  ]

getFile :: Path -> IO (Maybe Path)
getFile x = do
  exists <- MS.fileExists x
  return $
    if exists
      then Just x
      else Nothing

-- | Attempt to clone a package from github
installGithubRepo ::
     MT.Text -- ^ the repo path ("<username>/<reponame>")
  -> MT.Text -- ^ the url for github (e.g., "https://github.com/")
  -> MorlocMonad ()
installGithubRepo repo url = do
  config <- MM.ask
  let (Path lib) = Config.configLibrary config
  let cmd = MT.unwords ["git clone", url, lib <> "/" <> repo]
  MM.runCommand "installGithubRepo" cmd

-- | Install a morloc module
installModule :: ModuleSource -> MorlocMonad ()
installModule (GithubRepo repo) =
  installGithubRepo repo ("https://github.com/" <> repo)
installModule (CoreGithubRepo name) =
  installGithubRepo name ("https://github.com/morloclib/" <> name)
installModule (LocalModule Nothing) =
  MM.throwError (NotImplemented "module installation from working directory")
installModule (LocalModule (Just _)) =
  MM.throwError (NotImplemented "module installation from local directory")
