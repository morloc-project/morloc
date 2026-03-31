{- |
Module      : Main
Description : Executable main module
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative
import Subcommands (runMorloc)
import UI

main :: IO ()
main = do
  setLocaleEncoding utf8
  runMorloc =<< execParser opts
