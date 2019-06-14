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
import qualified Morloc.Language as ML

import qualified System.Directory as SD

build :: Script -> MorlocMonad ()
build s = case scriptLang s of
  Python3Lang -> MM.liftIO $ writeInterpreted s
  RLang       -> MM.liftIO $ writeInterpreted s
  PerlLang    -> MM.liftIO $ writeInterpreted s
  CLang       -> buildC s
  MorlocLang  -> MM.throwError . GeneratorError $ "You don't want to do that"

-- | Compile a C program
buildC :: Script -> MorlocMonad ()
buildC (Script base lang code') = do
  let src = ML.makeSourceName lang (MT.pack base)
  let exe = ML.makeExecutableName lang (MT.pack base)  
  MM.liftIO $ MT.writeFile (MT.unpack src) code'
  MM.runCommand "buildC" ("gcc -o " <> exe <> " " <> src <> " -lm")

-- | Build an interpreted script.
writeInterpreted :: Script -> IO ()
writeInterpreted (Script base lang code') = do
  let f = MT.unpack $ ML.makeExecutableName lang (MT.pack base)
  MT.writeFile f code'
  p <- SD.getPermissions f
  SD.setPermissions f (p {SD.executable = True})
