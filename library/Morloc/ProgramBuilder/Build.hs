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
import qualified Morloc.System as MS
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
    (RustLang, name) -> rustBuild name s
  where
    exeName = makeExecutableName filename (scriptLang s) (scriptBase s)

makeExecutableName :: Maybe Path -> Lang -> String -> Path
makeExecutableName Nothing lang base = ML.makeExecutableName lang base
makeExecutableName (Just filename) _ _ = filename

-- | Compile a C program
gccBuild :: Path -> Script -> MT.Text -> MorlocMonad ()
gccBuild exe s cmd = do
  let src = ML.makeSourceName (scriptLang s) (scriptBase s)
  let inc = ["-I" <> i | i <- scriptInclude s]
  liftIO $ MT.writeFile src (unCode (scriptCode s))
  MM.runCommand "GccBuild" $
    MT.unwords ([cmd, "-o", MT.pack exe, MT.pack src] ++ scriptCompilerFlags s ++ map MT.pack inc)

-- | Compile a C program
rustBuild :: Path -> Script -> MorlocMonad ()
rustBuild exeName s = do
  let poolCargoDir = MS.dropExtensions exeName <> "-mod"
  liftIO $ SD.createDirectoryIfMissing True poolCargoDir

-- | Build an interpreted script.
writeInterpreted :: Path -> Script -> IO ()
writeInterpreted path s = do
  MT.writeFile path (unCode (scriptCode s))
  p <- SD.getPermissions path
  SD.setPermissions path (p {SD.executable = True})
