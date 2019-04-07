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

import Morloc.Operators
import Morloc.Types
import qualified Morloc as M
import qualified Morloc.Monad as MM
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Data.Text as MT
import qualified Morloc.Config as Config
import System.Console.Docopt
import Control.Monad (when)
import qualified Morloc.Environment as ME

type Subcommand = Arguments -> Config.Config -> IO ()

getArgOrDie :: Arguments -> Option -> MT.Text
getArgOrDie args opt = case getArg args opt of
  Nothing -> error ("Invalid command: Expected option '" <> show opt)
  (Just x) -> MT.pack x

-- | read the global morloc config file or return a default one
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
readScript :: Arguments -> IO (Maybe Path, MT.Text)
readScript args
  | isPresent args (longOption "expression") = return (Nothing, script)
  | otherwise = do
      code <- MT.readFile (MT.unpack script)
      return (Just script, code)
  where
    script = getArgOrDie args (argument "script")

-- | install a module
cmdInstall :: Subcommand
cmdInstall args conf
  =   (MM.runMorlocMonad conf Nothing cmdInstall')
  >>= MM.writeMorlocReturn where
  cmdInstall' = do
    let name = getArgOrDie args (argument "name")
    if isPresent args (longOption "github")
    then ME.installModule (ME.GithubRepo name)
    else ME.installModule (ME.CoreGithubRepo name)

-- | remove a previously installed module (NOT YET IMPLEMENTED)
cmdRemove :: Subcommand
cmdRemove args config = do
  putStrLn "not removing anything"

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: Subcommand
cmdMake args config = do
  (path, code) <- readScript args
  when (isPresent args (longOption "endpoint")) $ do
    let ep = SparqlEndPoint . MT.unpack $ getArgOrDie args (longOption "endpoint")
    MM.runMorlocMonad config Nothing (M.writeProgram path code ep) >>= MM.writeMorlocReturn
  when (notPresent args (longOption "endpoint")) $ do
    let ep = MR.makeRDF []
    MM.runMorlocMonad config Nothing (M.writeProgram path code ep) >>= MM.writeMorlocReturn

-- | compile a Morloc script to RDF (turtle format by default)
cmdRdf :: Subcommand
cmdRdf args config = do
  (path, code) <- readScript args
  let writer = if isPresent args (longOption "triple") 
               then M.writeTripleTo
               else M.writeTurtleTo
  MM.runMorlocMonad config Nothing (writer path code "/dev/stdout") >>= MM.writeMorlocReturn
