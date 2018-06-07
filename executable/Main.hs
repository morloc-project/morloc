module Main (main) where

import System.Environment (getArgs)
import Morloc (interpret)

scriptParser :: (String, String) -> String
scriptParser (_, s) = (unlines . map show . map interpret) (lines s)

interactWith function inputFiles = do
  inputs <- mapM readFile inputFiles
  putStrLn ((concat . map function) (zip inputFiles inputs))

main = mainWith scriptParser
  where mainWith f = do
          args <- getArgs
          case args of
            [] -> putStrLn "You must provide at least one argument"
            xs -> interactWith f xs
