module Main (main) where

import Morloc 
import Morloc.Error
import System.Environment (getArgs)

-- writeProgram :: ThrowsError (Script, [Script]) -> IO ()
-- writeProgram (Right (n, ps)) = do
--   writeScript n
--   mapM_ writeScript ps
-- writeProgram (Left err) = putStr (show err)
--
-- writeScript :: Script -> IO ()
-- writeScript (Script base lang code) =
--   writeFile (base ++ "." ++ lang) code

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- no input
    []  -> putStrLn "You must provide at least one argument"

    -- TODO: also NOT the right default
    ["-e", text] -> putStr (rdf text)

    -- TODO: this should NOT be the default
    [x] -> do
      input <- readFile x
      putStr (rdf input)

    -- wrong input
    _   -> putStrLn "Please provide a single filename"
