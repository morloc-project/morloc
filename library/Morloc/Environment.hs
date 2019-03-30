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
import qualified Morloc.Data.Text as MT
import qualified Morloc.Config as MC
import qualified System.Process  as SP
import qualified System.Exit  as SE

data ModuleSource
  = LocalModule (Maybe MT.Text) 
  | GithubRepo MT.Text
  | CoreGithubRepo MT.Text

installGithubRepo :: MC.Config -> MT.Text -> MT.Text -> IO ()
installGithubRepo config repo url = do
  let lib = MC.configLibrary config
  let cmd = MT.unwords ["git clone", url, lib <> "/" <> repo] 
  (_, _, herr, handle) <- SP.runInteractiveCommand (MT.unpack cmd)
  exitCode <- SP.waitForProcess handle
  err <- MT.hGetContents herr
  case exitCode of
    SE.ExitSuccess     -> putStr (MT.unpack err)
    (SE.ExitFailure _) -> SE.die (MT.unpack err)

installModule :: MC.Config -> ModuleSource -> IO ()
installModule config (GithubRepo repo)
  = installGithubRepo config repo ("https://github.com/" <> repo)
installModule config (CoreGithubRepo name)
  = installGithubRepo config name ("https://github.com/morloc-project/" <> name)
installModule _ (LocalModule Nothing) = undefined    -- install from working directory
installModule _ (LocalModule (Just dir)) = undefined -- install from dir
