{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Morloc as M
import qualified System.Environment as SE
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

main :: IO ()
main = do
  args <- SE.getArgs
  case args of
    -- no input
    []  -> putStrLn "You must provide at least one argument"

    ["--rdf", "-e", text] -> M.writeTurtle (DT.pack text)

    ["--rdf", "--triple", "-e", text] -> M.writeTriple (DT.pack text)

    ["--rdf", x] -> DTI.readFile x >>= M.writeTurtle

    ["--rdf", "--triple", x] -> DTI.readFile x >>= M.writeTriple

    [endpoint] -> M.writeProgram endpoint

    -- wrong input
    _   -> putStrLn "Please provide a single filename"
