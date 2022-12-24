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
import Morloc.CodeGenerator.Namespace (TypeP, prettyGenTypeP)
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
    else if typecheckRealize args
        then
            MM.runMorlocMonad
               Nothing
               verbosity
               config
               (M.typecheck path code) |>> writeTypecheckOutput verbosity >>= (\s -> putDoc (s <> "\n"))
        else
            MM.runMorlocMonad
               Nothing
               verbosity
               config
               (M.typecheckFrontend path code) |>> writeFrontendTypecheckOutput verbosity >>= (\s -> putDoc (s <> "\n"))

writeFrontendTypecheckOutput :: Int -> ((Either MorlocError [SAnno (Indexed TypeU) Many Int], [MT.Text]), MorlocState) -> MDoc
writeFrontendTypecheckOutput _ ((Left e, _), _) = pretty e
writeFrontendTypecheckOutput 0 ((Right xs, _), s) = vsep (map (writeFrontendTypes s) xs)
writeFrontendTypecheckOutput 1 ((Right xs, _), s) = "\nExports:\n\n" <> vsep (map (writeFrontendTypes s) xs)
writeFrontendTypecheckOutput _ _ = "I don't know how to be that verbose"

writeFrontendTypes :: MorlocState -> SAnno (Indexed TypeU) Many Int -> MDoc
writeFrontendTypes  s (SAnno _ (Idx gidx t)) = writeTerm s gidx (pretty t)

writeTerm :: MorlocState -> Int -> MDoc -> MDoc
writeTerm s i typeDoc =
    case ( Map.lookup i (stateName s)
         ,  GMap.lookup i (stateSignatures s))
    of
        (Just v, GMapJust (TermTypes {termGeneral = Just t'})) -> pretty v <+> "::" <+> pretty t'
        (Just v, _) -> pretty v <+> "|-" <+> typeDoc
        _ -> "MISSING"


writeTypecheckOutput :: Int -> ((Either MorlocError ([SAnno (Indexed Type) One ()], [SAnno Int One (Indexed TypeP)]), [MT.Text]), MorlocState) -> MDoc
writeTypecheckOutput _ ((Left e, _), _) = pretty e
writeTypecheckOutput 0 ((Right (_, rasts), _), s) = vsep (map (writeRast 0 s) rasts)
writeTypecheckOutput 1 ((Right (_, rasts), _), s) = "\nExports:\n\n" <> vsep (map (writeRast 1 s) rasts)
writeTypecheckOutput _ _ = "I don't know how to be that verbose"

 
-- data SAnno g f c = SAnno (f (SExpr g f c, c)) g
writeRast :: Int -> MorlocState -> SAnno Int One (Indexed TypeP) -> MDoc
writeRast _ s (SAnno (One (e, Idx _ t)) gidx) = msg where

  msg = generalSignatures <> "\n  " <> writeRealizedTermC e

  generalSignatures = writeTerm s gidx (prettyGenTypeP t)

  writeRealizedTermC :: SExpr Int One (Indexed TypeP) -> MDoc
  writeRealizedTermC UniS = "Unit"
  writeRealizedTermC (VarS v) = pretty v
  writeRealizedTermC (AccS x k) = parens (writeRealizedTermG x) <> "@" <> pretty k
  writeRealizedTermC (AppS x xs) = parens $ hsep (map writeRealizedTermG (x:xs)) 
  writeRealizedTermC (LamS vs x) = parens $ "\\" <+> hsep (map pretty vs) <+> "->" <+> writeRealizedTermG x
  writeRealizedTermC (LstS xs) = list $ map writeRealizedTermG xs
  writeRealizedTermC (TupS xs) = tupled $ map writeRealizedTermG xs
  writeRealizedTermC (NamS ks) = encloseSep "{" "}" "," [pretty k <+> "=" <+> writeRealizedTermG x | (k,x) <- ks]
  writeRealizedTermC (RealS v) = viaShow v
  writeRealizedTermC (IntS v) = pretty v
  writeRealizedTermC (LogS v) = pretty v
  writeRealizedTermC (StrS v) = dquotes (pretty v)
  writeRealizedTermC (CallS src) = pretty src

  writeRealizedTermG :: SAnno Int One (Indexed TypeP) -> MDoc
  writeRealizedTermG (SAnno (One (e', t')) _) = parens (writeRealizedTermC e') <+> "::" <+> parens (pretty t')
