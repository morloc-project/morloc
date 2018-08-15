module Main (main) where

import qualified Morloc as M
import qualified System.Environment as SE

main :: IO ()
main = do
  args <- SE.getArgs
  case args of
    -- no input
    []  -> putStrLn "You must provide at least one argument"

    ["--rdf", "-e", text] -> M.writeTurtle text

    ["--rdf", "--triple", "-e", text] -> M.writeTriple text

    ["--rdf", x] -> readFile x >>= M.writeTurtle

    ["--rdf", "--triple", x] -> readFile x >>= M.writeTriple

    [x] -> readFile x >>= M.writeProgram

    -- wrong input
    _   -> putStrLn "Please provide a single filename"
