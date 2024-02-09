module Main where

import Subcommands (runMorloc)
import UI
import Options.Applicative

main :: IO ()
main = runMorloc =<< execParser opts
