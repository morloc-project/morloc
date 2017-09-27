module MorlocExecutable.Repl (repl) where

import System.Console.Repline
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

import Morloc (interpret)
import MorlocExecutable.Mode (asResult)

type Repl a = HaskelineT IO a

say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

says :: (MonadIO m, Show a) => a -> m ()
says = liftIO . print

-- main command, interpret Morloc
cmd :: String -> Repl()
cmd line = case interpret line of
  (Left err)  -> say err
  (Right res) -> liftIO . asResult $ res

opts :: [(String, [String] -> Repl ())]
opts = [( "cat", catFiles)]

catFiles :: [String] -> Repl ()
catFiles args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

repl = evalRepl prompt cmd opts autocomplete start where

  prompt = "morloc> "

  matcher :: MonadIO m => [(String, CompletionFunc m)]
  matcher = [(":cat", fileCompleter)]

  byWord :: Monad m => WordCompleter m
  byWord n = do
    let names = [":cat"]
    return $ filter (isPrefixOf n) names

  autocomplete = Prefix (wordCompleter byWord) matcher

  start :: Repl ()
  start = return ()
