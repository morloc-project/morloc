{-# LANGUAGE QuasiQuotes #-}

module Main where

import Morloc.Types
import qualified Morloc as M
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Data.Text as MT
import System.Console.Docopt
import Control.Monad (when)
import qualified System.Environment as SE

patterns :: Docopt
patterns = [docoptFile|USAGE|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< SE.getArgs

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
        M.writeProgram ep script
      when (notPresent args (longOption "endpoint")) $ do
        M.writeProgram (MR.makeRDF []) script

    -- compile a Morloc script to RDF (turtle format by default)
    when (isPresent args (command "rdf")) $ do
      let writer = if isPresent args (longOption "triple") 
                   then M.writeTripleTo
                   else M.writeTurtleTo
      writer script "/dev/stdout" -- not the prettiest solution ...
