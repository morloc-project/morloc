module Main (main) where

import System.Console.Repline
import Control.Monad.State.Strict
import Data.List (isPrefixOf)

import Morloc (interpret)
import Morloc.Mode (asLIL)

-- eventually there will be other modes
mode = asLIL

type Repl a = HaskelineT IO a

-- main command, interpret Morloc
cmd :: String -> Repl()
cmd line = liftIO $ process line where
  process s = case interpret mode s of
    Left  err -> putStr err
    Right res -> putStr res

opts :: [(String, [String] -> Repl ())]
opts = [
    ("validate"     , catFiles)
  , ("output-gen"   , catFiles)
  , ("input-gen"    , catFiles)
  , ("convert-json" , catFiles)
  ]

catFiles :: [String] -> Repl ()
catFiles args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents


main :: IO ()
main = evalRepl "morloc> " cmd opts (Prefix (wordCompleter byWord) defaultMatcher) initRepl where

  defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
  defaultMatcher = [
      (":validate"     , fileCompleter)
    , (":output-gen"   , fileCompleter)
    , (":input-gen"    , fileCompleter)
    , (":convert-json" , fileCompleter)
    ]

  byWord :: Monad m => WordCompleter m
  byWord n = do
    let names = [":validate", ":output-gen", ":input-gen", ":convert-json"]
    return $ filter (isPrefixOf n) names

  initRepl :: Repl ()
  initRepl = return ()
