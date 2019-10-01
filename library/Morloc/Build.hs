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
import qualified Morloc.Language as ML

import qualified System.Directory as SD

build :: Script -> MorlocMonad ()
build s = case scriptLang s of
  Python3Lang -> MM.liftIO $ writeInterpreted s
  RLang       -> MM.liftIO $ writeInterpreted s
  PerlLang    -> MM.liftIO $ writeInterpreted s
  CLang       -> gccBuild s "gcc"
  CppLang     -> gccBuild s "g++" -- TODO: I need more rigorous build handling
  MorlocLang  -> MM.throwError . GeneratorError $ "You don't want to do that"

-- | Compile a C program
gccBuild :: Script -> MT.Text -> MorlocMonad ()
gccBuild s cmd = do
  let src = ML.makeSourceName (scriptLang s) (MT.pack (scriptBase s))
  let exe = ML.makeExecutableName (scriptLang s) (MT.pack (scriptBase s))
  let inc = ["-I" <> i | i <- scriptInclude s]
  MM.liftIO $ MT.writeFile (MT.unpack src) (scriptCode s) 
  MM.runCommand "GccBuild" $ MT.unwords ([cmd, "-o", exe, src] ++ scriptCompilerFlags s ++ inc)

-- | Build an interpreted script.
writeInterpreted :: Script -> IO ()
writeInterpreted s = do
  let f = MT.unpack $ ML.makeExecutableName (scriptLang s) (MT.pack (scriptBase s))
  MT.writeFile f (scriptCode s)
  p <- SD.getPermissions f
  SD.setPermissions f (p {SD.executable = True})
