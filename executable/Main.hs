{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Morloc as M
import qualified System.Environment as SE
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

usage :: IO ()
usage = putStr $ unlines [
      "The morloc CLI is under development, try one of these patterns:"
    , "  morloc --rdf -e <code>"
    , "  morloc --rdf --triple -e <code>"
    , "  morloc --rdf -e <filename>"
    , "  morloc --rdf --triple -e <filename>"
    , "  morloc <filename> <sparql-endpoint>"
  ]

main :: IO ()
main = do
  args <- SE.getArgs
  case args of
    -- no input
    []  -> usage

    ["--rdf", "-e", text] -> M.writeTurtle (DT.pack text)

    ["--rdf", "--triple", "-e", text] -> M.writeTriple (DT.pack text)

    ["--rdf", x] -> DTI.readFile x >>= M.writeTurtle

    ["--rdf", "--triple", x] -> DTI.readFile x >>= M.writeTriple

    [x, endpoint] -> DTI.readFile x >>= M.writeProgram endpoint

    -- wrong input
    _   -> usage
