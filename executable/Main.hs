module Main (main) where

import Morloc 
import Morloc.Error
import Morloc.Data
import System.Environment (getArgs)

writeProgram :: ThrowsError (Script, [Script]) -> IO ()
writeProgram (Right (n, ps)) = do
  writeScript n
  mapM_ writeScript ps
writeProgram (Left err) = putStr (show err)
  
writeScript :: Script -> IO ()
writeScript (Script base lang code) =
  writeFile (base ++ "." ++ lang) code

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> putStrLn "You must provide at least one argument"
    [x] -> do
      input <- readFile x
      (writeProgram . build) input
    _   -> putStrLn "Please provide a single filename"
