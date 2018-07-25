module Main (main) where

import qualified Morloc as M
import qualified System.Environment as SE

main :: IO ()
main = do
  args <- SE.getArgs
  case args of
    -- no input
    []  -> putStrLn "You must provide at least one argument"

    ["-e", text] -> M.turtle text

    [x] -> readFile x >>= M.turtle

    -- wrong input
    _   -> putStrLn "Please provide a single filename"
