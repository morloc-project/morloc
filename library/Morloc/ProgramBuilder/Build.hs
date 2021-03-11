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
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Control.Monad.State as CMS

import qualified System.Directory as SD

buildProgram :: (Script, [Script]) -> MorlocMonad ()
buildProgram (nexus, pools) = do
  mapM_ (build Nothing) pools
  outfile <- CMS.gets stateOutfile
  build outfile nexus

build :: Maybe Path -> Script -> MorlocMonad ()
build filename s =
  case (scriptLang s, exeName) of
    (Python3Lang, name) -> liftIO $ writeInterpreted name s
    (RLang, name) -> liftIO $ writeInterpreted name s
    (PerlLang, name) -> liftIO $ writeInterpreted name s
    (CLang, name) -> gccBuild name s "gcc"
    (CppLang, name) -> gccBuild name s "g++ --std=c++11" -- TODO: I need more rigorous build handling
    (RustLang, name) -> rustBuild name s "rustc"
  where
    exeName = Path $ makeExecutableName filename (scriptLang s) (MT.pack (scriptBase s))

makeExecutableName :: Maybe Path -> Lang -> MT.Text -> MT.Text
makeExecutableName Nothing lang base = ML.makeExecutableName lang base
makeExecutableName (Just (Path filename)) _ _ = filename

-- | Compile a C program
gccBuild :: Path -> Script -> MT.Text -> MorlocMonad ()
gccBuild (Path exe) s cmd = do
  let src = ML.makeSourceName (scriptLang s) (MT.pack (scriptBase s))
  let inc = ["-I" <> unPath i | i <- scriptInclude s]
  liftIO $ MT.writeFile (MT.unpack src) (unCode (scriptCode s))
  MM.runCommand "GccBuild" $
    MT.unwords ([cmd, "-o", exe, src] ++ scriptCompilerFlags s ++ inc)

-- | Compile a C program
rustBuild :: Path -> Script -> MT.Text -> MorlocMonad ()
rustBuild (Path exe) s cmd = do
  let src = ML.makeSourceName (scriptLang s) (MT.pack (scriptBase s))
  liftIO $ MT.writeFile (MT.unpack src) (unCode (scriptCode s))
  MM.runCommand "RustBuild" $
    MT.unwords ([cmd, "-o", exe, src] ++ scriptCompilerFlags s)

-- | Build an interpreted script.
writeInterpreted :: Path -> Script -> IO ()
writeInterpreted path s = do
  let exe = MT.unpack . unPath $ path
  MT.writeFile exe (unCode (scriptCode s))
  p <- SD.getPermissions exe
  SD.setPermissions exe (p {SD.executable = True})
