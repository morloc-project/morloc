{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Subcommands
Description : Morloc executable subcommands
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Subcommands (runMorloc) where

import UI
import Morloc.Namespace
import qualified Morloc.Config as Config
import qualified Morloc as M
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.API as F


runMorloc :: CliCommand -> IO () 
runMorloc args = do
  config <- getConfig args
  let verbose = getVerbosity args
  case args of
    (CmdMake g) -> cmdMake g verbose config
    (CmdInstall g) -> cmdInstall g verbose config
    (CmdTypecheck g) -> cmdTypecheck g verbose config
    

-- | read the global morloc config file or return a default one
getConfig :: CliCommand -> IO Config.Config
getConfig (CmdMake g) = getConfig' (makeConfig g) (makeVanilla g)
getConfig (CmdInstall g) = getConfig' (installConfig g) (installVanilla g)
getConfig (CmdTypecheck g) = getConfig' (typecheckConfig g) (typecheckVanilla g)

getConfig' :: String -> Bool -> IO Config.Config
getConfig' _ True = Config.loadMorlocConfig Nothing
getConfig' "" _ = Config.loadMorlocConfig Nothing
getConfig' filename _ = Config.loadMorlocConfig (Just (Path (MT.pack filename)))

getVerbosity :: CliCommand -> Int
getVerbosity (CmdMake      g) = if makeVerbose      g then 1 else 0
getVerbosity (CmdInstall   g) = if installVerbose   g then 1 else 0
getVerbosity (CmdTypecheck g) = if typecheckVerbose g then 1 else 0

readScript :: Bool -> String -> IO (Maybe Path, Code)
readScript True code = return (Nothing, Code (MT.pack code))
readScript _ filename = do
  code <- MT.readFile filename
  return (Just (Path (MT.pack filename)), Code code)


-- | install a module
cmdInstall :: InstallCommand -> Int -> Config.Config -> IO ()
cmdInstall args verbosity conf =
  MM.runMorlocMonad verbosity conf cmdInstall' >>= MM.writeMorlocReturn
  where
    cmdInstall' = do
      let name = MT.pack $ installModuleName args
      if installGithub args
        then Mod.installModule (Mod.GithubRepo name)
        else Mod.installModule (Mod.CoreGithubRepo name)

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: MakeCommand -> Int -> Config.Config -> IO ()
cmdMake args verbosity config = do
  (path, code) <- readScript (makeExpression args) (makeScript args)
  MM.runMorlocMonad verbosity config (M.writeProgram path code) >>=
    MM.writeMorlocReturn

-- | run the typechecker on a module but do not build it
cmdTypecheck :: TypecheckCommand -> Int -> Config.Config -> IO ()
cmdTypecheck args verbosity config = do
  (path, code) <- readScript (typecheckExpression args) (typecheckScript args)
  let writer = if typecheckRaw args then F.ugly else F.cute
  if typecheckType args
    then print $ F.readType (unCode code)
    else MM.runMorlocMonad
           verbosity
           config
           (M.typecheck path code >>= MM.liftIO . writer) >>=
         MM.writeMorlocReturn
