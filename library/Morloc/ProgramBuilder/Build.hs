{-|
Module      : Morloc.ProgramBuilder.Build
Description : Manage system requirements and project building for pools
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.ProgramBuilder.Build
  ( buildProgram
  ) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.Doc as MD
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM

import qualified System.Directory as SD

buildProgram :: (Script, [Script]) -> MorlocMonad ()
buildProgram (nexus, pools) = mapM_ build (nexus : pools)

build :: Script -> MorlocMonad ()
build s =
  case scriptLang s of
    Python3Lang -> liftIO $ writeInterpreted s
    RLang -> liftIO $ writeInterpreted s
    PerlLang -> liftIO $ writeInterpreted s
    CLang -> gccBuild s "gcc"
    CppLang -> gccBuild s "g++ --std=c++11" -- TODO: I need more rigorous build handling

-- | Compile a C program
gccBuild :: Script -> MT.Text -> MorlocMonad ()
gccBuild s cmd = do
  let src = ML.makeSourceName (scriptLang s) (MT.pack (scriptBase s))
  let exe = ML.makeExecutableName (scriptLang s) (MT.pack (scriptBase s))
  let inc = ["-I" <> unPath i | i <- scriptInclude s]
  liftIO $ MT.writeFile (MT.unpack src) (unCode (scriptCode s))
  MM.runCommand "GccBuild" $
    MT.unwords ([cmd, "-o", exe, src] ++ scriptCompilerFlags s ++ inc)

-- | Build an interpreted script.
writeInterpreted :: Script -> IO ()
writeInterpreted s = do
  let f =
        MT.unpack $
        ML.makeExecutableName (scriptLang s) (MT.pack (scriptBase s))
  MT.writeFile f (unCode (scriptCode s))
  p <- SD.getPermissions f
  SD.setPermissions f (p {SD.executable = True})
