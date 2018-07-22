module Main (main) where

import Morloc 
import Morloc.Error
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- no input
    []  -> putStrLn "You must provide at least one argument"

    -- also NOT the right default
    ["--rdf", "-e", text] -> putStr (rdf text)

    ["--rdf", x] -> do
      input <- readFile x
      putStr (rdf input)

    [x] -> do
      input <- readFile x
      make input

    ["-e", text] -> make text

    -- wrong input
    _   -> putStrLn "Please provide a single filename"
