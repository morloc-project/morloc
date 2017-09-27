module Main (main) where
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

-- list of libraries to search for source files
arguments :: [String]
arguments = ["executable", "library", "test-suite"]
main :: IO ()
main = do
  hints <- hlint arguments
  if null hints then exitSuccess else exitFailure
