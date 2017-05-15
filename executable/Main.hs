module Main (main) where

import Morloc (interpret)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    -- getInputLine - haskeline function that returns Maybe, with
    -- Nothing implying a Ctrl-D, end of file
    minput <- getInputLine "morloc> "
    case minput of
      Nothing -> outputStrLn "goodbye"
      -- process input - parse the input line
      -- liftIO - do it in the IO monad
      -- `>>` - jump to the next loop, discarding current value
      Just input -> liftIO (writeResult input) >> loop where
        writeResult s = case interpret s of
          Left  err -> putStr err
          Right res -> putStr res
