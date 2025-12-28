{-|
Module      : Main
Description : Executable main module
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Main where

import Subcommands (runMorloc)
import UI
import Options.Applicative

main :: IO ()
main = runMorloc =<< execParser opts
