{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Subcommands
Description : Morloc executable subcommands
Copyright   : (c) Zebulun Arendsee, 2016-2024
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
import qualified Morloc.CodeGenerator.SystemConfig as MSC
import Morloc.CodeGenerator.Namespace (SerialManifold(..))
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)
import Morloc.Data.Doc
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Map as Map
import Morloc.CodeGenerator.Generate (generatePools)


runMorloc :: CliCommand -> IO ()
runMorloc args = do
  config <- getConfig args
  let verbose = getVerbosity args
  case args of
    (CmdMake g) -> cmdMake g verbose config
    (CmdInstall g) -> cmdInstall g verbose config
    (CmdTypecheck g) -> cmdTypecheck g verbose config
    (CmdDump g) -> cmdDump g verbose config
    (CmdInit g) -> cmdInit g config


-- | read the global morloc config file or return a default one
getConfig :: CliCommand -> IO Config.Config
getConfig (CmdMake g) = getConfig' (makeConfig g) (makeVanilla g)
getConfig (CmdInstall g) = getConfig' (installConfig g) (installVanilla g)
getConfig (CmdTypecheck g) = getConfig' (typecheckConfig g) (typecheckVanilla g)
getConfig (CmdDump g) = getConfig' (dumpConfig g) (dumpVanilla g)
getConfig (CmdInit g) = getConfig' (initConfig g) (initVanilla g)

getConfig' :: String -> Bool -> IO Config.Config
getConfig' _ True = Config.loadMorlocConfig Nothing
getConfig' "" _ = Config.loadMorlocConfig Nothing
getConfig' filename _ = Config.loadMorlocConfig (Just filename)

getVerbosity :: CliCommand -> Int
getVerbosity (CmdMake      g) = makeVerbose      g
getVerbosity (CmdInstall   g) = installVerbose   g
getVerbosity (CmdTypecheck g) = typecheckVerbose g
getVerbosity (CmdDump      g) = dumpVerbose      g
getVerbosity (CmdInit      g) = if initQuiet g then 0 else 1 

readScript :: Bool -> String -> IO (Maybe Path, Code)
readScript True code = return (Nothing, Code (MT.pack code))
readScript _ filename = do
  code <- MT.readFile filename
  return (Just filename, Code code)


-- | Install a module
cmdInstall :: InstallCommand -> Int -> Config.Config -> IO ()
cmdInstall args verbosity conf = MM.runMorlocMonad Nothing verbosity conf cmdInstall' >>= MM.writeMorlocReturn
  where
    modName = installModuleName args
    selector = installSelector args

    cmdInstall'
      | modName == "." = Mod.installModule (LocalModule Nothing) Nothing
      | (head modName) `elem` ['.', '/'] = Mod.installModule (LocalModule (Just modName)) Nothing
      | installGithub args = installGithubModule modName selector
      | otherwise = Mod.installModule (CoreGithubRepo modName selector) (Just $ configPlane conf)

    installGithubModule :: String -> GithubSnapshotSelector -> MorlocMonad ()
    installGithubModule fullName selector' = case break (== '/') fullName of
      (username, '/':repo) -> Mod.installModule (GithubRepo username repo selector') Nothing
      _ -> do
        MM.throwError . ModuleInstallError $ "Error: Expected \"<username>/<repo>\" format for GitHub module name"

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
  let verbosity = typecheckVerbose args
  if typecheckType args
    then case F.readType (unCode code) of
      (Left err') -> print (errorBundlePretty err')
      (Right x) -> print x
    else if typecheckRealize args
        then
            MM.runMorlocMonad
               Nothing
               verbosity
               config
               (M.typecheck path code >>= (generatePools . snd) )
               |>> writeTypecheckOutput verbosity
               >>= (\s -> putDoc (s <> "\n"))
        else
            MM.runMorlocMonad
               Nothing
               verbosity
               config
               (M.typecheckFrontend path code) |>> writeFrontendTypecheckOutput verbosity >>= (\s -> putDoc (s <> "\n"))

writeFrontendTypecheckOutput :: Int -> ((Either MorlocError [AnnoS (Indexed TypeU) Many Int], [MT.Text]), MorlocState) -> MDoc
writeFrontendTypecheckOutput _ ((Left e, _), _) = pretty e
writeFrontendTypecheckOutput 0 ((Right xs, _), _) = vsep (map writeFrontendTypes xs)
writeFrontendTypecheckOutput 1 x = writeFrontendTypecheckOutput 0 x -- no difference in verbosity
writeFrontendTypecheckOutput _ _ = "I don't know how to be that verbose"

writeFrontendTypes :: AnnoS (Indexed TypeU) Many Int -> MDoc
writeFrontendTypes  (AnnoS (Idx _ t) _ e) = pretty e <+> "::" <+> pretty t 

writeTypecheckOutput :: Int -> ((Either MorlocError [(Lang, [SerialManifold])], [MT.Text]), MorlocState) -> MDoc
writeTypecheckOutput _ ((Left e, _), _) = pretty e
writeTypecheckOutput _ ((Right pools, _), _) = vsep $ map (uncurry writePool) pools

writePool :: Lang -> [SerialManifold] -> MDoc
writePool lang manifolds = pretty lang <+> "pool:" <> "\n" <> vsep (map pseudocodeSerialManifold manifolds) <> "\n"


cmdDump :: DumpCommand -> Int -> Config.Config -> IO ()
cmdDump args _ config = do
  (path, code) <- readScript (dumpExpression args) (dumpScript args)
  let verbosity = dumpVerbose args
  ((x, _), _) <- MM.runMorlocMonad Nothing verbosity config (F.parse path code)
  case x of
    (Left e) -> putDoc $ pretty e
    (Right e) -> putDoc $ prettyDAG e


cmdInit :: InitCommand -> Config.Config -> IO ()
cmdInit ic config = MSC.configureAll (not (initQuiet ic)) (initForce ic) config

prettyDAG :: DAG MVar e ExprI -> MDoc
prettyDAG m0 = vsep (map prettyEntry (Map.toList m0)) where
  prettyEntry :: (MVar, (ExprI, [(MVar, e)])) -> MDoc
  prettyEntry (k, (n, _)) = block 4 (pretty k) (vsep [pretty n])
