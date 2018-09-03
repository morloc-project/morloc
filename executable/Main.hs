{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Morloc as M
import System.Console.Docopt
import Control.Monad (when)
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified System.Environment as SE

patterns :: Docopt
patterns = [docopt|
morloc version 0.14.0

Usage:
  morloc make [--expression] <script> <sparql-endpoint>
  morloc rdf [--triple] [--expression] <script>

Options:
  -t, --triple       print RDF graph in triple format, rather than the turtle
  -e, --expression   read script as string rather than file
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< SE.getArgs

  -- do the following if we are processing Morloc code
  when (isPresent args (argument "script")) $ do
    -- handle the code, either from a file or a raw string
    script <- if isPresent args (longOption "expression")
              then (fmap DT.pack $ getArgOrExit args (argument "script"))
              else getArgOrExit args (argument "script") >>= DTI.readFile

    -- build a Morloc program, generating the nexus and pool files
    when (isPresent args (command "make")) $ do
      ep <- getArgOrExit args (argument "sparql-endpoint")
      M.writeProgram ep script

    -- compile a Morloc script to RDF (turtle format by default)
    when (isPresent args (command "rdf")) $ do
      let writer = if isPresent args (longOption "triple") 
                   then M.writeTriple
                   else M.writeTurtle
      writer script
