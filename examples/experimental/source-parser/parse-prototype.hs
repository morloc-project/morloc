module Main where

import System.Environment (getArgs)
import Data.List (intersperse)
import Data.List.Split (splitOn)

newtype FileS = FileS {unFileS :: String}   deriving Show
newtype CodeS = CodeS {unCodeS :: [String]} deriving Show
newtype NameS = NameS {unNameS :: String}   deriving Show
newtype TypeS = TypeS {unTypeS :: String}   deriving Show

data Language
  = LangHaskell
  | LangR

instance Show Language where
  show LangHaskell = "haskell"
  show LangR       = "R"

data Package = Package {
      packageName      :: NameS 
    , packageCode      :: CodeS
    , packageLanguage  :: Maybe Language 
    , packageFunctions :: [(NameS, TypeS)]
  }

instance Show Package where
  show (Package (NameS filename) (CodeS code) lang fs) =
    "source filename: " ++ filename ++ "\n" ++
    "language: " ++ show lang ++ "\n" ++
    "exported functions: " ++ listExports ++ "\n" ++
    "source code:\n" ++ sourceCode
    where
      listExports = (concat . intersperse ", ") (map (unNameS . fst) fs)
      sourceCode = (unlines . map (\s -> "  " ++ s)) code

interactWith function inputFiles = do
  inputs <- mapM readFile inputFiles
  putStrLn ((concat . map function) (zip inputFiles inputs))

main = mainWith (show . parseCode) 
  where mainWith function = do
          args <- getArgs
          case args of
            [] -> putStrLn "You must provide at least one argument"
            xs -> interactWith function xs

parseCode :: (String, String) -> Package 
parseCode (filename, source) = Package {
      packageName      = filename' 
    , packageCode      = source'
    , packageLanguage  = guessLanguage (filename', source')
    , packageFunctions = [(NameS "a", TypeS "b")]
  } where
    filename' = NameS filename 
    source' = CodeS (lines source)

-- guess what language we are working with based on extension
-- TODO: instead of relying on the extension, parse the file itself
guessLanguage :: (NameS, CodeS) -> Maybe Language
guessLanguage (NameS name, _) =
  case (splitOn "." name) of
    [] -> Nothing
    xs -> lookup (last xs) [
          ("R", LangR)
        , ("r", LangR)
        , ("hs", LangHaskell)
      ]

findExportedFunctions :: Language -> CodeS -> [(NameS, TypeS)] 
findExportedFunctions = undefined
