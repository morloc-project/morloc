{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Subcommands
Description : Morloc executable subcommands
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Subcommands
( 
    getConfig
  , cmdInstall
  , cmdMake
  , cmdRdf
) where

import System.Console.Docopt
import Morloc.Operators
import Morloc.Types
import qualified Morloc as M
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Data.Text as MT
import qualified Morloc.Config as Config
import System.Console.Docopt
import Control.Monad (when)
import qualified System.Environment as SE
import qualified Morloc.Environment as ME

type Subcommand = Arguments -> Config.Config -> IO ()

getArgOrDie :: Arguments -> Option -> MT.Text
getArgOrDie args opt = case getArg args opt of
  Nothing -> error ("Invalid command: Expected option '" <> show opt)
  (Just x) -> MT.pack x

getConfig :: Arguments -> IO Config.Config
getConfig args = do
  let givenPath = getArg args (longOption "config")
  let isVanilla = isPresent args (longOption "vanilla")
  defaultPath <- Config.getDefaultConfigFilepath
  let configPath = if isVanilla
                   then Nothing
                   else case givenPath of
                     (Just f) -> Just (MT.pack f)
                     Nothing -> Just defaultPath
  -- load the config file
  Config.loadMorlocConfig configPath 

-- | handle the code, either from a file or a raw string
readScript :: Arguments -> IO MT.Text
readScript args = do
  if isPresent args (longOption "expression")
  then (return $ getArgOrDie args (argument "script"))
  else MT.readFile (MT.unpack $ getArgOrDie args (argument "script"))

cmdInstall :: Subcommand
cmdInstall args config = do
  let name = getArgOrDie args (argument "name")
  if isPresent args (longOption "github")
  then ME.installModule config (ME.GithubRepo name)
  else ME.installModule config (ME.CoreGithubRepo name)

cmdRemove :: Subcommand
cmdRemove args config = do
  putStrLn "not removing anything"

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: Subcommand
cmdMake args config = do
  script <- readScript args
  when (isPresent args (longOption "endpoint")) $ do
    let ep = SparqlEndPoint . MT.unpack $ getArgOrDie args (longOption "endpoint")
    M.writeProgram config ep script
  when (notPresent args (longOption "endpoint")) $ do
    M.writeProgram config (MR.makeRDF []) script

-- | compile a Morloc script to RDF (turtle format by default)
cmdRdf :: Subcommand
cmdRdf args config = do
  script <- readScript args
  let writer = if isPresent args (longOption "triple") 
               then M.writeTripleTo
               else M.writeTurtleTo
  writer config script "/dev/stdout" -- not the prettiest solution ...
