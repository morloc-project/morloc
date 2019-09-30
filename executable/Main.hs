{-# LANGUAGE QuasiQuotes #-}

module Main where

import Subcommands
import System.Console.Docopt
import Control.Monad (when)
import qualified System.Environment as SE

patterns :: Docopt
patterns = [docoptFile|USAGE|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< SE.getArgs
  config <- getConfig args

  when (isPresent args (command "install")) (cmdInstall args config)

  -- do the following if we are processing Morloc code
  when (isPresent args (argument "script")) $ do
    when (isPresent args (command "make")) (cmdMake args config)

  when (isPresent args (command "typecheck")) $ cmdTypecheck args config
