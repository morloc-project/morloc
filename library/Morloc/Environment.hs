{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Environment
Description : Environment code
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Environment
(
    ModuleSource(..)
  , installModule
) where

import Morloc.Operators
import Morloc.Types
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Config as MC

data ModuleSource
  = LocalModule (Maybe MT.Text) 
  | GithubRepo MT.Text
  | CoreGithubRepo MT.Text

installGithubRepo :: MT.Text -> MT.Text -> MM.MorlocMonad ()
installGithubRepo repo url = do
  config <- MM.ask
  let lib = MC.configLibrary config
  let cmd = MT.unwords ["git clone", url, lib <> "/" <> repo] 
  MM.runCommand cmd
  
installModule :: ModuleSource -> MM.MorlocMonad ()
installModule (GithubRepo repo)
  = installGithubRepo repo ("https://github.com/" <> repo)
installModule (CoreGithubRepo name)
  = installGithubRepo name ("https://github.com/morloc-project/" <> name)
installModule (LocalModule Nothing) = MM.throwError (NotImplemented "module installation from working directory")
installModule (LocalModule (Just dir)) = MM.throwError (NotImplemented "module installation from local directory")
