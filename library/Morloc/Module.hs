{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Module
Description : Morloc module imports and paths 
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Module
( 
    ModuleSource(..)
  , installModule
  , findModule
) where

import Morloc.Global
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Config as Config
import qualified System.Directory as SD
import qualified Control.Monad as CM
import qualified Data.Maybe as DM
import Morloc.Operators
import qualified Data.List as DL


-- | Specify where a module is located 
data ModuleSource
  = LocalModule (Maybe MT.Text) -- ^ A module in the working directory
  | GithubRepo MT.Text -- ^ A module stored in an arbitrary Github repo: "<username>/<reponame>"
  | CoreGithubRepo MT.Text -- ^ The repo name of a core package, e.g., "math"

-- | Look for a local morloc module.
findModule :: MT.Text -> MorlocMonad MT.Text
findModule moduleName = do
  config <- MM.ask
  let lib = Config.configLibrary config
  let allPaths = getModulePaths lib moduleName
  existingPaths <- MM.liftIO . fmap DM.catMaybes . CM.mapM getFile $ allPaths
  case existingPaths of
    (x:_) -> return x
    [] -> MM.throwError (CannotLoadModule (
          "module not found among the paths: ["
          <> (MT.concat $ DL.intersperse ", " allPaths)
          <> "]"
      ))

-- | Find an ordered list of possible locations to search for a module
getModulePaths :: MT.Text -> MT.Text -> [MT.Text]
getModulePaths lib base = [
    base <> ".loc"                               -- "./${base}.loc"
  , base <> "/main.loc"                          -- "${base}/main.loc"
  , lib <> "/" <> base <> ".loc"                 -- "${LIB}/${base}.loc"
  , lib <> "/" <> base <> "/main.loc"            -- "${LIB}/${base}/main.loc"
  , lib <> "/" <> base <> "/" <> base <> ".loc"  -- "${LIB}/${base}/${base}.loc"
  ]

getFile :: MT.Text -> IO (Maybe MT.Text)
getFile x = do
  fileExists <- SD.doesFileExist (MT.unpack x) 
  return $ if fileExists
           then Just x
           else Nothing

-- | Attempt to clone a package from github
installGithubRepo
  :: MT.Text -- ^ the repo path ("<username>/<reponame>")
  -> MT.Text -- ^ the url for github (e.g., "https://github.com/")
  -> MorlocMonad ()
installGithubRepo repo url = do
  config <- MM.ask
  let lib = Config.configLibrary config
  let cmd = MT.unwords ["git clone", url, lib <> "/" <> repo] 
  MM.runCommand "installGithubRepo" cmd

-- | Install a morloc module
installModule :: ModuleSource -> MorlocMonad ()
installModule (GithubRepo repo) = installGithubRepo repo ("https://github.com/" <> repo)
installModule (CoreGithubRepo name) = installGithubRepo name ("https://github.com/morloclib/" <> name)
installModule (LocalModule Nothing) = MM.throwError (NotImplemented "module installation from working directory")
installModule (LocalModule (Just _)) = MM.throwError (NotImplemented "module installation from local directory")
