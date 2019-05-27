{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Build
Description : Manage system requirements and project building for pools
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Build
( 
  build
) where

import Morloc.Global
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS

import qualified System.Directory as SD

build :: Script -> MorlocMonad ()
build s = case scriptLang s of
  "py" -> MM.liftIO $ writeInterpreted s
  "R" -> MM.liftIO $ writeInterpreted s
  "perl" -> MM.liftIO $ writeInterpreted s
  "C" -> buildC s
  "c" -> buildC s
  _ -> MM.throwError (PoolBuildError s "Language not supported")

-- | Compile a C program
buildC :: Script -> MorlocMonad ()
buildC (Script base lang code') = do
  let src = MS.makePoolSourceName (MT.pack base) (MT.pack lang)  
  let exe = MS.makePoolExecutableName (MT.pack base) (MT.pack lang)  
  MM.liftIO $ MT.writeFile (MT.unpack src) code'
  MM.runCommand "buildC" ("gcc -o " <> exe <> " " <> src <> " -lm")

-- | Build an interpreted script.
writeInterpreted :: Script -> IO ()
writeInterpreted (Script base lang code') = do
  let f = MT.unpack $ MS.makePoolExecutableName (MT.pack base) (MT.pack lang)
  MT.writeFile f code'
  p <- SD.getPermissions f
  SD.setPermissions f (p {SD.executable = True})
