module Main where

import System.Environment (getArgs)
import Data.List.Split (splitOn)

data Language
  = LangHaskell
  | LangR
  deriving Show

data Package = Package {
      packageName      :: String
    , packageCode      :: [String]
    , packageLanguage  :: Maybe Language 
    , packageFunctions :: [(String, String)]
  } deriving Show

interactWith function inputFiles = do
  inputs <- mapM readFile inputFiles
  print (map function (zip inputFiles inputs))

main = mainWith (show . parseCode) 
  where mainWith function = do
          args <- getArgs
          case args of
            [] -> putStrLn "You must provide at least one argument"
            xs -> interactWith function xs

parseCode :: (String, String) -> Package 
parseCode (name, src) = Package {
      packageName      = name
    , packageCode      = lines src 
    , packageLanguage  = guessLanguage (name, src)
    , packageFunctions = [("a", "b")]
  }

-- guess what language we are working with based on extension
-- TODO: instead of relying on the extension, parse the file itself
guessLanguage :: (String, String) -> Maybe Language
guessLanguage (name, _) =
  case (splitOn "." name) of
    [] -> Nothing
    xs -> lookup (last xs) [
          ("R", LangR)
        , ("r", LangR)
        , ("hs", LangHaskell)
      ]
