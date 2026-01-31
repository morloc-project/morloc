{- |
Module      : Main
Description : Executable main module
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Main where

import Options.Applicative
import Subcommands (runMorloc)
import UI

main :: IO ()
main = runMorloc =<< execParser opts
