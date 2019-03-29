{-# LANGUAGE QuasiQuotes #-}

module Main where

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

patterns :: Docopt
patterns = [docoptFile|USAGE|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< SE.getArgs

  defaultPath <- Config.getDefaultConfigFilepath
  let givenPath = getArg args (longOption "config")

  let configPath = if (isPresent args (longOption "vanilla"))
                   then Nothing
                   else case givenPath of
                     (Just f) -> Just (MT.pack f)
                     Nothing -> Just defaultPath

  -- load the config file
  config <- Config.loadMorlocConfig configPath 

  when (isPresent args (command "install")) $ do
    name <- getArgOrExit args (argument "name")
    if isPresent args (longOption "github")
    then ME.installModule config (ME.GithubRepo name)
    else ME.installModule config (ME.CoreGithubRepo name)

  when (isPresent args (command "init")) $ do
    pkg <- getArgOrExit args (argument "package-name")
    ME.initModule pkg

  when (isPresent args (command "check")) $ do
    putStrLn "not checking anything"

  when (isPresent args (command "update")) $ do
    putStrLn "not updating anything"

  -- do the following if we are processing Morloc code
  when (isPresent args (argument "script")) $ do
    -- handle the code, either from a file or a raw string
    script <- if isPresent args (longOption "expression")
              then (fmap MT.pack $ getArgOrExit args (argument "script"))
              else getArgOrExit args (argument "script") >>= MT.readFile

    -- build a Morloc program, generating the nexus and pool files
    when (isPresent args (command "make")) $ do
      when (isPresent args (longOption "endpoint")) $ do
        ep <- (fmap SparqlEndPoint $ getArgOrExit args (longOption "endpoint"))
        M.writeProgram config ep script
      when (notPresent args (longOption "endpoint")) $ do
        M.writeProgram config (MR.makeRDF []) script

    -- compile a Morloc script to RDF (turtle format by default)
    when (isPresent args (command "rdf")) $ do
      let writer = if isPresent args (longOption "triple") 
                   then M.writeTripleTo
                   else M.writeTurtleTo
      writer config script "/dev/stdout" -- not the prettiest solution ...
