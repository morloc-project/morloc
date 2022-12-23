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
import qualified Morloc.Data.GMap as GMap
import Morloc.CodeGenerator.Namespace (TypeP)
import Morloc.Pretty
import Morloc.Data.Doc
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Map as Map


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
getConfig' filename _ = Config.loadMorlocConfig (Just filename)

getVerbosity :: CliCommand -> Int
getVerbosity (CmdMake      g) = if makeVerbose      g then 1 else 0
getVerbosity (CmdInstall   g) = if installVerbose   g then 1 else 0
getVerbosity (CmdTypecheck g) = if typecheckVerbose g then 1 else 0

readScript :: Bool -> String -> IO (Maybe Path, Code)
readScript True code = return (Nothing, Code (MT.pack code))
readScript _ filename = do
  code <- MT.readFile filename
  return (Just filename, Code code)


-- | install a module
cmdInstall :: InstallCommand -> Int -> Config.Config -> IO ()
cmdInstall args verbosity conf =
  MM.runMorlocMonad Nothing verbosity conf cmdInstall' >>= MM.writeMorlocReturn
  where
    cmdInstall' = do
      let name' = installModuleName args
      if installGithub args
        then Mod.installModule (Mod.GithubRepo name')
        else Mod.installModule (Mod.CoreGithubRepo name')

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: MakeCommand -> Int -> Config.Config -> IO ()
cmdMake args verbosity config = do
  (path, code) <- readScript (makeExpression args) (makeScript args)
  outfile <- case makeOutfile args of
    "" -> return Nothing
    x -> return . Just $ x
  MM.runMorlocMonad outfile verbosity config (M.writeProgram path code) >>=
    MM.writeMorlocReturn

cmdTypecheck :: TypecheckCommand -> Int -> Config.Config -> IO ()
cmdTypecheck args _ config = do
  (path, code) <- readScript (typecheckExpression args) (typecheckScript args)
  let verbosity = if typecheckVerbose args then 1 else 0
  if typecheckType args
    then case F.readType (unCode code) of
      (Left err') -> print (errorBundlePretty err')
      (Right x) -> print x 
    else MM.runMorlocMonad
           Nothing
           verbosity
           config
           (M.typecheckFrontend path code) |>> writeTypecheckOutput verbosity >>= (\s -> putDoc (s <> "\n"))

writeTypecheckOutput :: Int -> ((Either MorlocError a, [MT.Text]), MorlocState) -> MDoc
writeTypecheckOutput _ ((Left e, _), _) = pretty e
writeTypecheckOutput 0 (_, s) = writeExports s
writeTypecheckOutput 1 (_, s) = "\nExports:\n\n" <> writeExports s
writeTypecheckOutput _ _ = "I don't know how to be that verbose"

 
writeExports :: MorlocState -> MDoc
writeExports s = msg where
   ids = stateExports s 
   names = map (`Map.lookup` (stateName s)) ids
   sigs = map (`GMap.lookup` (stateSignatures s)) ids
   msg = vsep $ zipWith writeTerm names sigs

   writeTerm :: Maybe EVar -> GMapRet TermTypes -> MDoc
   writeTerm (Just v) (GMapJust (TermTypes {termGeneral = Just t})) = pretty v <+> "::" <+> pretty t
   writeTerm _ _ = "MISSING"
