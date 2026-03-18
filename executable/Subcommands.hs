{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Subcommands
Description : Dispatch CLI subcommands and inject the translator callback
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Implements each CLI subcommand (make, typecheck, install, init, dump) and
defines the 'TranslateFn' callback that routes C++ to 'CppTranslator' and
other languages to the generic translator. This is the dependency injection
point that keeps translator code out of the library.
-}
module Subcommands (runMorloc) where

import Control.Exception (SomeException, bracket, finally, try)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified CppTranslator
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Morloc (generatePools)
import qualified Morloc as M
import Morloc.CodeGenerator.Emit (TranslateFn)
import qualified Morloc.CodeGenerator.Grammars.Translator.Generic as Generic
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)
import Morloc.CodeGenerator.Namespace (SerialManifold (..))
import qualified Morloc.CodeGenerator.SystemConfig as MSC
import qualified Morloc.Completion as Completion
import qualified Morloc.Config as Config
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.API as F
import Morloc.Module (OverwriteProtocol (..), findMainLocFile)
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.Namespace.Type
import qualified Morloc.ProgramBuilder.Install as Install
import Morloc.Typecheck.Internal (prettyTypeU)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  , setCurrentDirectory
  )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeFileName)
import System.IO.Temp (createTempDirectory)
import qualified System.Process as SP
import UI

decodePackageMeta :: BL.ByteString -> Maybe PackageMeta
decodePackageMeta = JSON.decode

-- | Route each language to its translator.
translator :: TranslateFn
translator lang srcs es
  | lang == CppTranslator.cppLang = CppTranslator.translate srcs es
  | otherwise = Generic.translate lang srcs es

runMorloc :: CliCommand -> IO ()
runMorloc args = do
  config <- getConfig args
  buildConfig <- Config.loadBuildConfig config
  let verbose = getVerbosity args
  runPassed <- case args of
    (CmdMake g) -> cmdMake g verbose config buildConfig
    (CmdInstall g) -> cmdInstall g verbose config buildConfig
    (CmdTypecheck g) -> cmdTypecheck g verbose config buildConfig
    (CmdDump g) -> cmdDump g verbose config buildConfig
    (CmdInit g) -> cmdInit g config
    (CmdList g) -> cmdList g config
    (CmdUninstall g) -> cmdUninstall g config
    (CmdNew g) -> cmdNew g
    (CmdEval g) -> cmdEval g verbose config buildConfig
  case runPassed of
    True -> exitSuccess
    False -> exitFailure

-- | read the global morloc config file or return a default one
getConfig :: CliCommand -> IO Config.Config
getConfig (CmdMake g) = getConfig' (makeConfig g) (makeVanilla g)
getConfig (CmdInstall g) = getConfig' (installConfig g) (installVanilla g)
getConfig (CmdTypecheck g) = getConfig' (typecheckConfig g) (typecheckVanilla g)
getConfig (CmdDump g) = getConfig' (dumpConfig g) (dumpVanilla g)
getConfig (CmdInit g) = getConfig' (initConfig g) (initVanilla g)
getConfig (CmdList g) = getConfig' (listConfig g) (listVanilla g)
getConfig (CmdUninstall g) = getConfig' (uninstallConfig g) (uninstallVanilla g)
getConfig (CmdEval g) = getConfig' (evalConfig g) (evalVanilla g)
getConfig (CmdNew _) = getConfig' "" False

getConfig' :: String -> Bool -> IO Config.Config
getConfig' _ True = Config.loadMorlocConfig Nothing
getConfig' "" _ = Config.loadMorlocConfig Nothing
getConfig' filename _ = Config.loadMorlocConfig (Just filename)

getVerbosity :: CliCommand -> Int
getVerbosity (CmdMake g) = makeVerbose g
getVerbosity (CmdInstall g) = installVerbose g
getVerbosity (CmdTypecheck g) = typecheckVerbose g
getVerbosity (CmdDump g) = dumpVerbose g
getVerbosity (CmdInit g) = if initQuiet g then 0 else 1
getVerbosity (CmdList g) = listVerbose g
getVerbosity (CmdEval g) = evalVerbose g
getVerbosity (CmdUninstall _) = 0
getVerbosity (CmdNew _) = 0

readScript :: Bool -> String -> IO (Maybe Path, Code)
readScript True code = return (Nothing, Code (MT.pack code))
readScript _ filename = do
  code <- MT.readFile filename
  return (Just filename, Code code)

-- | Typecheck callback for module installation
typecheckModuleFn :: FilePath -> MorlocMonad [(T.Text, T.Text)]
typecheckModuleFn mainFile = do
  code <- liftIO $ MT.readFile mainFile
  -- Save current state, run typecheck in a clean sub-state
  savedState <- MM.get
  result <-
    MM.catchError
      ( do
          xs <- M.typecheckFrontend (Just mainFile) (Code code)
          st <- MM.get
          return
            [ (render (pretty v), render (pretty t))
            | AnnoS (Idx i t) _ _ <- xs
            , Just v <- [Map.lookup i (stateName st)]
            ]
      )
      (\_ -> return [])
  -- Restore state so module typechecking doesn't pollute the parent state
  MM.put savedState
  return result

-- | Install a module
cmdInstall :: InstallCommand -> Int -> Config.Config -> BuildConfig -> IO Bool
cmdInstall args verbosity conf buildConfig = do
  userSources <- Map.fromList <$> mapM (\modstr -> do
    name <- Mod.extractModuleName modstr
    return (name, modstr)) moduleTexts
  let cmdInstall' =
        mapM
          ( \modstr ->
              Mod.installModule
                (installForce args)
                (installUseSSH args)
                libpath
                (Config.configPlaneCore conf)
                mayTypecheck
                userSources
                Set.empty
                Mod.ExplicitInstall
                modstr
          )
          moduleTexts
  passed <- MM.runMorlocMonad Nothing verbosity conf buildConfig cmdInstall' >>= MM.writeMorlocReturn
  if passed && installBuild args
    then buildInstalledModules args verbosity conf buildConfig moduleTexts libpath
    else return passed
  where
    libpath = Config.configLibrary conf </> Config.configPlane conf
    moduleTexts = map MT.pack (installModuleStrings args)

    mayTypecheck =
      if installNoTypecheck args
        then Nothing
        else Just typecheckModuleFn

-- | Build and install executables for installed modules
buildInstalledModules ::
  InstallCommand -> Int -> Config.Config -> BuildConfig -> [T.Text] -> FilePath -> IO Bool
buildInstalledModules args verbosity conf buildConfig moduleTexts libpath = do
  results <- mapM buildOne moduleTexts
  return (and results)
  where
    force = installForce args == ForceOverwrite

    buildOne modstr = do
      name <- T.unpack <$> Mod.extractModuleName modstr
      let moduleDir = libpath </> name
      mainFile <- findMainLocFile moduleDir name
      case mainFile of
        Nothing -> do
          putStrLn $ "Warning: no main.loc found for '" <> name <> "', skipping build"
          return True
        Just locFile -> do
          origDir <- getCurrentDirectory
          setCurrentDirectory moduleDir
          buildResult <-
            buildModuleExecutable locFile name verbosity conf buildConfig force
              `finally` setCurrentDirectory origDir
          return buildResult

    buildModuleExecutable locFile name verbosity' config buildConfig' forceOverwrite = do
      code <- MT.readFile locFile
      let action = do
            MM.modify (\s -> s {stateInstall = True})
            M.writeProgram translator (Just locFile) (Code code)
      result <- MM.runMorlocMonad Nothing verbosity' config buildConfig' action
      passed <- MM.writeMorlocReturn result
      if passed
        then do
          let (_, finalState) = result
              pkgIncludes = concatMap packageInclude (statePackageMeta finalState)
          case stateInstallDir finalState of
            Nothing -> do
              putStrLn $ "Error: install directory was not set during compilation of '" <> name <> "'"
              return False
            Just installDir -> do
              let installName = takeFileName installDir
              Install.installProgram (Config.configHome config) installDir installName pkgIncludes forceOverwrite
              return True
        else return False

-- | build a Morloc program, generating the nexus and pool files
cmdMake :: MakeCommand -> Int -> Config.Config -> BuildConfig -> IO Bool
cmdMake args verbosity config buildConfig = do
  (path, code) <- readScript (makeExpression args) (makeScript args)
  outfile <- case makeOutfile args of
    "" -> return Nothing
    x -> return . Just $ x
  let install = makeInstall args
      action = do
        MM.modify (\s -> s {stateInstall = install})
        M.writeProgram translator path code
  result <- MM.runMorlocMonad outfile verbosity config buildConfig action
  passed <- MM.writeMorlocReturn result
  if passed && install
    then do
      let (_, finalState) = result
          cliIncludes = map T.pack (makeInclude args)
          pkgIncludes = concatMap packageInclude (statePackageMeta finalState)
          allIncludes = pkgIncludes ++ cliIncludes
      case stateInstallDir finalState of
        Nothing -> do
          putStrLn "Error: install directory was not set during compilation"
          return False
        Just installDir -> do
          let installName = takeFileName installDir
          Install.installProgram
            (Config.configHome config)
            installDir
            installName
            allIncludes
            (makeForce args)
          return True
    else return passed

-- | Evaluate a morloc expression
cmdEval :: EvalCommand -> Int -> Config.Config -> BuildConfig -> IO Bool
cmdEval args verbosity config buildConfig = do
  let rawExpr = evalExpression args
      code = MT.pack (preprocessEvalInput rawExpr)
      tmpBase = Config.configTmpDir config
      saveName = evalSave args
      extraArgs = evalArgs args
      isSave = not (null saveName)
      exeName = if isSave then saveName else "eval"
  createDirectoryIfMissing True tmpBase
  bracket
    (do
      origDir <- getCurrentDirectory
      tmpDir <- createTempDirectory tmpBase "morloc-eval-"
      setCurrentDirectory tmpDir
      return (origDir, tmpDir))
    (\(origDir, tmpDir) -> do
      setCurrentDirectory origDir
      cleanupTmpDir tmpDir)
    (\(_origDir, tmpDir) -> do
      let action = do
            MM.modify (\s -> s {stateEvalMode = True})
            if isSave then MM.modify (\s -> s {stateInstall = True}) else return ()
            M.writeProgram translator Nothing (Code code)
      result <- MM.runMorlocMonad (Just exeName) verbosity config buildConfig action
      passed <- MM.writeMorlocReturn result
      if not passed
        then return False
        else
          if isSave
            then do
              let (_, finalState) = result
                  pkgIncludes = concatMap packageInclude (statePackageMeta finalState)
              case stateInstallDir finalState of
                Nothing -> do
                  putStrLn "Error: install directory was not set during compilation"
                  return False
                Just installDir -> do
                  Install.installProgram (Config.configHome config) installDir saveName pkgIncludes True
                  writeEvalMeta (Config.configHome config) saveName rawExpr
                  return True
            else do
              let exe = tmpDir </> exeName
              subcommand <- getFirstSubcommand exe
              let cmdArgs = subcommand : extraArgs
              runResult <- try (SP.callProcess exe cmdArgs) :: IO (Either SomeException ())
              case runResult of
                Right () -> return True
                Left e -> do
                  putStrLn $ "Error running expression: " ++ show e
                  return False)
  where
    cleanupTmpDir dir = do
      exists <- doesDirectoryExist dir
      if exists then removeDirectoryRecursive dir else return ()

-- | Extract the first subcommand name from the manifest embedded in a wrapper script.
-- Falls back to "__expr__" if the manifest cannot be parsed.
getFirstSubcommand :: FilePath -> IO String
getFirstSubcommand wrapperPath = do
  result <- try (readFile wrapperPath) :: IO (Either SomeException String)
  case result of
    Left _ -> return "__expr__"
    Right contents -> do
      let marker = "### MANIFEST ###"
          afterMarker = drop 1 $ dropWhile (/= marker) (lines contents)
          manifestStr = unlines afterMarker
      case JSON.eitherDecode (BL.fromStrict (MT.encodeUtf8 (MT.pack manifestStr))) of
        Right pm -> case pmCommands pm of
          (cmd : _) -> return (T.unpack (pcName cmd))
          [] -> return "__expr__"
        Left _ -> return "__expr__"

-- | Write metadata about the saved eval expression
writeEvalMeta :: FilePath -> String -> String -> IO ()
writeEvalMeta cfgHome name expr = do
  now <- getCurrentTime
  let fdbDir = cfgHome </> "fdb"
      metaPath = fdbDir </> name ++ ".eval-meta"
      timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      json = "{\"expression\":" ++ jsonEscape expr ++ ",\"timestamp\":\"" ++ timestamp ++ "\"}"
  createDirectoryIfMissing True fdbDir
  writeFile metaPath json
  where
    jsonEscape s = "\"" ++ concatMap escChar s ++ "\""
    escChar '"' = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\t' = "\\t"
    escChar c = [c]

-- | Preprocess eval input: replace top-level semicolons with newlines.
-- Semicolons inside explicit brace blocks (depth > 0) are preserved.
-- Leading whitespace after each replacement is stripped so the layout
-- rule treats each statement as a new top-level declaration.
preprocessEvalInput :: String -> String
preprocessEvalInput = go (0 :: Int)
  where
    go _ [] = []
    go depth ('{' : rest) = '{' : go (depth + 1) rest
    go depth ('}' : rest) = '}' : go (max 0 (depth - 1)) rest
    go 0 (';' : rest) = '\n' : go 0 (dropWhile (== ' ') rest)
    go depth ('"' : rest) = '"' : goString depth rest
    go depth (c : rest) = c : go depth rest

    goString depth [] = go depth []
    goString depth ('"' : rest) = '"' : go depth rest
    goString depth ('\\' : c : rest) = '\\' : c : goString depth rest
    goString depth (c : rest) = c : goString depth rest

cmdTypecheck :: TypecheckCommand -> Int -> Config.Config -> BuildConfig -> IO Bool
cmdTypecheck args _ config buildConfig = do
  (path, code) <- readScript (typecheckExpression args) (typecheckScript args)
  let verbosity = typecheckVerbose args
  if typecheckType args
    then case F.readType (unCode code) of
      (Left err') -> do
        putStrLn err'
        return False
      (Right x) -> do
        print x
        return True
    else
      if typecheckRealize args
        then do
          (passed, result) <-
            MM.runMorlocMonad
              Nothing
              verbosity
              config
              buildConfig
              ( M.typecheck path code
                  >>= (generatePools . snd)
              )
              |>> writeTypecheckOutput verbosity
          putDoc (result <> "\n")
          return passed
        else do
          (passed, result) <-
            MM.runMorlocMonad
              Nothing
              verbosity
              config
              buildConfig
              (M.typecheckFrontend path code)
              |>> writeFrontendTypecheckOutput verbosity
          putDoc (result <> "\n")
          return passed

writeFrontendTypecheckOutput ::
  Int ->
  ((Either MorlocError [AnnoS (Indexed TypeU) Many Int], [MT.Text]), MorlocState) ->
  (Bool, MDoc)
writeFrontendTypecheckOutput _ ((Left e, _), st) = (False, MM.makeMorlocError st e)
writeFrontendTypecheckOutput 0 ((Right xs, _), st) = (True, vsep (map (writeFrontendTypes st) xs))
writeFrontendTypecheckOutput 1 x = writeFrontendTypecheckOutput 0 x -- no difference in verbosity
writeFrontendTypecheckOutput _ _ = (False, "I don't know how to be that verbose")

writeFrontendTypes :: MorlocState -> AnnoS (Indexed TypeU) Many Int -> MDoc
writeFrontendTypes st (AnnoS (Idx i t) _ _) =
  case Map.lookup i (stateName st) of
    (Just v) -> pretty v <+> "::" <+> prettyTypeU t
    Nothing -> "? ::" <+> prettyTypeU t

writeTypecheckOutput ::
  Int -> ((Either MorlocError [(Lang, [SerialManifold])], [MT.Text]), MorlocState) -> (Bool, MDoc)
writeTypecheckOutput _ ((Left e, _), st) = (False, MM.makeMorlocError st e)
writeTypecheckOutput _ ((Right pools, _), _) = (True, vsep $ map (uncurry writePool) pools)

writePool :: Lang -> [SerialManifold] -> MDoc
writePool lang manifolds = pretty lang <+> "pool:" <> "\n" <> vsep (map pseudocodeSerialManifold manifolds) <> "\n"

cmdDump :: DumpCommand -> Int -> Config.Config -> BuildConfig -> IO Bool
cmdDump args _ config buildConfig = do
  (path, code) <- readScript (dumpExpression args) (dumpScript args)
  let verbosity = dumpVerbose args
  ((x, _), st) <- MM.runMorlocMonad Nothing verbosity config buildConfig (F.parse path code)
  case x of
    (Left e) -> do
      putDoc $ MM.makeMorlocError st e
      return False
    (Right e) -> do
      putDoc $ prettyDAG e
      return True

cmdInit :: InitCommand -> Config.Config -> IO Bool
cmdInit ic config = MSC.configureAll (not (initQuiet ic)) (initForce ic) (initSlurmSupport ic) (initSanitize ic) config

cmdNew :: NewCommand -> IO Bool
cmdNew args = do
  let pkgFile = "package.yaml"
  exists <- doesFileExist pkgFile
  if exists
    then do
      putStrLn "package.yaml already exists"
      return True
    else do
      name <-
        if null (newName args)
          then takeFileName <$> getCurrentDirectory
          else return (newName args)
      writeFile pkgFile $
        unlines
          [ "name: " ++ name
          , "version: 0.1.0"
          , "homepage: null"
          , "synopsis: null"
          , "description: null"
          , "category: null"
          , "license: MIT"
          , "author: null"
          , "maintainer: null"
          , "github: null"
          , "bug-reports: null"
          , "dependencies: []"
          , "# Files to include when installing with `morloc make --install`"
          , "include: []"
          ]
      putStrLn $ "Created package.yaml for '" ++ name ++ "'"
      return True

prettyDAG :: DAG MVar e ExprI -> MDoc
prettyDAG m0 = vsep (map prettyEntry (Map.toList m0))
  where
    prettyEntry :: (MVar, (ExprI, [(MVar, e)])) -> MDoc
    prettyEntry (k, (n, _)) = block 4 (pretty k) (vsep [pretty n])

-- ======================================================================
-- List command
-- ======================================================================

-- Lightweight JSON types for reading manifests

data ModuleManifest = ModuleManifest
  { mmName :: T.Text
  , mmVersion :: T.Text
  , mmSynopsis :: T.Text
  , mmExports :: [(T.Text, T.Text)]
  , mmMorlocDeps :: [T.Text]
  , mmReason :: T.Text
  }

data ProgramManifest = ProgramManifest
  { pmName :: T.Text
  , pmCommands :: [ProgramCommand]
  }

data ProgramCommand = ProgramCommand
  { pcName :: T.Text
  , pcReturnType :: T.Text
  , _pcArgSchemas :: [T.Text]
  }

instance JSON.FromJSON ModuleManifest where
  parseJSON = JSON.withObject "ModuleManifest" $ \o ->
    ModuleManifest
      <$> o JSON..:? "name" JSON..!= ""
      <*> o JSON..:? "version" JSON..!= ""
      <*> o JSON..:? "synopsis" JSON..!= ""
      <*> (o JSON..:? "exports" JSON..!= [] >>= mapM parseExport)
      <*> o JSON..:? "morloc_dependencies" JSON..!= []
      <*> o JSON..:? "install_reason" JSON..!= ""
    where
      parseExport = JSON.withObject "Export" $ \o ->
        (,) <$> o JSON..: "name" <*> o JSON..: "type"

instance JSON.FromJSON ProgramManifest where
  parseJSON = JSON.withObject "ProgramManifest" $ \o ->
    ProgramManifest
      <$> o JSON..:? "name" JSON..!= ""
      <*> o JSON..:? "commands" JSON..!= []

instance JSON.FromJSON ProgramCommand where
  parseJSON = JSON.withObject "ProgramCommand" $ \o ->
    ProgramCommand
      <$> o JSON..: "name"
      <*> o JSON..:? "return_type" JSON..!= ""
      <*> o JSON..:? "arg_schemas" JSON..!= []

-- | Check if pattern is a subsequence of the target string (case-insensitive)
subsequenceMatch :: String -> String -> Bool
subsequenceMatch [] _ = True
subsequenceMatch _ [] = False
subsequenceMatch (p : ps) (t : ts)
  | toLower p == toLower t = subsequenceMatch ps ts
  | otherwise = subsequenceMatch (p : ps) ts

cmdList :: ListCommand -> Config.Config -> IO Bool
cmdList args config = do
  let fdbDir = Config.configHome config </> "fdb"
      libDir = Config.configLibrary config </> Config.configPlane config
      verbose = listVerbose args
      kind = listKind args
      pat = listPattern args

  -- Load module manifests
  allModules <-
    if kind /= Just ListPrograms
      then do
        mods <- loadModuleManifests fdbDir
        discovered <- discoverModules libDir fdbDir
        return (mods ++ discovered)
      else return []

  -- Load program manifests
  allPrograms <-
    if kind /= Just ListModules
      then loadProgramManifests fdbDir
      else return []

  -- Filter by pattern
  let modules = case pat of
        Nothing -> allModules
        Just p -> filter (\m -> subsequenceMatch p (T.unpack (mmName m))) allModules
      programs = case pat of
        Nothing -> allPrograms
        Just p -> filter (\m -> subsequenceMatch p (T.unpack (pmName m))) allPrograms

  -- For verbose mode, fill in exports from .loc files when manifest has none
  modules' <-
    if verbose > 0
      then mapM (fillModuleExports libDir) modules
      else return modules

  -- Print results
  if null modules' && null programs
    then putStrLn "No installed modules or programs found."
    else do
      if not (null modules')
        then do
          putStrLn "Modules:"
          mapM_ (printModule verbose) modules'
        else return ()
      if not (null programs)
        then do
          if not (null modules') then putStrLn "" else return ()
          putStrLn "Programs:"
          mapM_ (printProgram verbose) programs
        else return ()

  return True

-- | If a module has no exports in its manifest, scan its .loc file for type signatures
fillModuleExports :: FilePath -> ModuleManifest -> IO ModuleManifest
fillModuleExports libDir m
  | not (null (mmExports m)) = return m
  | otherwise = do
      let modDir = libDir </> T.unpack (mmName m)
          modName = T.unpack (mmName m)
      mainFile <- findMainLocFile modDir modName
      case mainFile of
        Nothing -> return m
        Just f -> do
          sigs <- extractTypeSignatures f
          return m {mmExports = sigs}

-- | Extract top-level type signatures from a .loc file
extractTypeSignatures :: FilePath -> IO [(T.Text, T.Text)]
extractTypeSignatures path = do
  result <- try (TIO.readFile path) :: IO (Either SomeException T.Text)
  case result of
    Left _ -> return []
    Right content ->
      return
        . map parseSig
        . filter isTypeSig
        . T.lines
        $ content
  where
    isTypeSig ln =
      let stripped = T.stripStart ln
       in not (T.null stripped)
            && T.head stripped /= '-' -- not a comment
            && T.head stripped /= '{' -- not a block comment
            && T.isInfixOf " :: " stripped
            && not (T.isPrefixOf "type " stripped)
            && not (T.isPrefixOf "source " stripped)
            && not (T.isPrefixOf "import " stripped)
            && not (T.isPrefixOf "module " stripped)
            && not (T.isPrefixOf "class " stripped)
            && not (T.isPrefixOf "instance " stripped)

    parseSig ln =
      let (sigName, rest) = T.breakOn " :: " (T.stripStart ln)
          typ = T.strip (T.drop 4 rest) -- drop " :: "
       in (T.strip sigName, typ)

loadModuleManifests :: FilePath -> IO [ModuleManifest]
loadModuleManifests fdbDir = do
  result <- try (listDirectory fdbDir) :: IO (Either SomeException [FilePath])
  case result of
    Left _ -> return []
    Right entries -> do
      let moduleFiles = filter (".module" `isSuffixOf`) entries
      catMaybes
        <$> mapM
          ( \f -> do
              r <- try (BL.readFile (fdbDir </> f)) :: IO (Either SomeException BL.ByteString)
              case r of
                Left _ -> return Nothing
                Right bs -> case JSON.eitherDecode bs of
                  Right m -> return (Just m)
                  Left _ -> return Nothing
          )
          moduleFiles

loadProgramManifests :: FilePath -> IO [ProgramManifest]
loadProgramManifests fdbDir = do
  result <- try (listDirectory fdbDir) :: IO (Either SomeException [FilePath])
  case result of
    Left _ -> return []
    Right entries -> do
      let manifestFiles = filter (".manifest" `isSuffixOf`) entries
      catMaybes
        <$> mapM
          ( \f -> do
              r <- try (BL.readFile (fdbDir </> f)) :: IO (Either SomeException BL.ByteString)
              case r of
                Left _ -> return Nothing
                Right bs -> case JSON.eitherDecode bs of
                  Right m ->
                    let m' =
                          if T.null (pmName m)
                            then m {pmName = T.pack (dropExtension (takeFileName f))}
                            else m
                     in return (Just m')
                  Left _ -> return Nothing
          )
          manifestFiles

-- | Discover modules in the library that lack manifests
discoverModules :: FilePath -> FilePath -> IO [ModuleManifest]
discoverModules libDir fdbDir = do
  libExists <- doesDirectoryExist libDir
  if not libExists
    then return []
    else do
      entries <- listDirectory libDir
      catMaybes
        <$> mapM
          ( \name -> do
              let manifestPath = fdbDir </> name ++ ".module"
                  moduleDir = libDir </> name
              hasManifest <- doesFileExist manifestPath
              isDir <- doesDirectoryExist moduleDir
              if hasManifest || not isDir
                then return Nothing
                else do
                  -- Try to read package.yaml for basic info
                  let pkgYaml = moduleDir </> "package.yaml"
                  pkgExists <- doesFileExist pkgYaml
                  if pkgExists
                    then do
                      r <- try (BL.readFile pkgYaml) :: IO (Either SomeException BL.ByteString)
                      case r of
                        Left _ -> return (Just (minimalManifest name))
                        Right bs -> case decodePackageMeta bs of
                          Just meta ->
                            return . Just $
                              ModuleManifest
                                { mmName = if T.null (packageName meta) then T.pack name else packageName meta
                                , mmVersion = packageVersion meta
                                , mmSynopsis = packageSynopsis meta
                                , mmExports = []
                                , mmMorlocDeps = []
                                , mmReason = ""
                                }
                          Nothing -> return (Just (minimalManifest name))
                    else return (Just (minimalManifest name))
          )
          entries
  where
    minimalManifest name =
      ModuleManifest
        { mmName = T.pack name
        , mmVersion = ""
        , mmSynopsis = ""
        , mmExports = []
        , mmMorlocDeps = []
        , mmReason = ""
        }

printModule :: Int -> ModuleManifest -> IO ()
printModule verbose m = do
  let name = mmName m
      ver = if T.null (mmVersion m) then "" else " " <> T.unpack (mmVersion m)
      syn = if T.null (mmSynopsis m) then "" else "  " <> T.unpack (mmSynopsis m)
  putStrLn $ "  " <> T.unpack name <> ver <> syn
  if verbose > 0
    then mapM_ (\(n, t) -> putStrLn $ "    " <> T.unpack n <> " :: " <> T.unpack t) (mmExports m)
    else return ()

printProgram :: Int -> ProgramManifest -> IO ()
printProgram verbose p = do
  let name = pmName p
      cmds = pmCommands p
      cmdCount = length cmds
      summary = show cmdCount <> " command" <> (if cmdCount /= 1 then "s" else "")
  putStrLn $ "  " <> T.unpack name <> "  " <> summary
  if verbose > 0
    then
      mapM_ (\c -> putStrLn $ "    " <> T.unpack (pcName c) <> " :: " <> T.unpack (pcReturnType c)) cmds
    else return ()

-- ======================================================================
-- Uninstall command
-- ======================================================================

cmdUninstall :: UninstallCommand -> Config.Config -> IO Bool
cmdUninstall args config = do
  let fdbDir = Config.configHome config </> "fdb"
      libDir = Config.configLibrary config </> Config.configPlane config
      binDir = Config.configHome config </> "bin"
      exeDir = Config.configHome config </> "exe"
      dryRun = uninstallDryRun args
      kind = uninstallKind args

  names <- if uninstallAll args
    then do
      fdbExists <- doesDirectoryExist fdbDir
      if not fdbExists
        then return []
        else do
          entries <- listDirectory fdbDir
          let moduleNames = [dropExtension f | f <- entries, ".module" `isSuffixOf` f]
          return moduleNames
    else return (uninstallNames args)

  if null names
    then do
      if uninstallAll args
        then putStrLn "No modules installed"
        else putStrLn "No module names specified. Use --all to uninstall all modules."
      return True
    else do
      let skipDepCheck = uninstallAll args
      allPassed <- mapM (\name -> uninstallOne fdbDir libDir binDir exeDir dryRun skipDepCheck kind name) names
      let anyRemoved = or allPassed

      -- Regenerate completions if anything was actually removed
      if anyRemoved && not dryRun
        then Completion.regenerateCompletions False (Config.configHome config)
        else return ()

      return True

uninstallOne ::
  FilePath -> FilePath -> FilePath -> FilePath -> Bool -> Bool -> Maybe ListKind -> String -> IO Bool
uninstallOne fdbDir libDir binDir exeDir dryRun skipDepCheck kind name = do
  let moduleManifest = fdbDir </> name ++ ".module"
      programManifest = fdbDir </> name ++ ".manifest"
      moduleDir = libDir </> name

  hasModule <- doesFileExist moduleManifest
  hasModuleDir <- doesDirectoryExist moduleDir
  hasProgram <- doesFileExist programManifest

  let removeModule = (hasModule || hasModuleDir) && kind /= Just ListPrograms
      removeProgram = hasProgram && kind /= Just ListModules

  if not removeModule && not removeProgram
    then do
      putStrLn $ "Nothing found for '" <> name <> "'"
      return False
    else do
      -- Reverse dependency check for modules (skip when uninstalling all)
      if removeModule
        then do
          if not skipDepCheck then checkReverseDeps fdbDir name else return ()
          if dryRun
            then do
              putStrLn $ "Would uninstall module '" <> name <> "'"
              if hasModuleDir
                then putStrLn $ "  Remove: " <> moduleDir
                else return ()
              if hasModule
                then putStrLn $ "  Remove: " <> moduleManifest
                else return ()
            else do
              if hasModuleDir then removeDirectoryRecursive moduleDir else return ()
              if hasModule then removeFile moduleManifest else return ()
              putStrLn $ "Uninstalled module '" <> name <> "'"
        else return ()

      if removeProgram
        then do
          let binPath = binDir </> name
          binExists <- doesFileExist binPath
          if dryRun
            then do
              putStrLn $ "Would uninstall program '" <> name <> "'"
              if binExists then putStrLn $ "  Remove: " <> binPath else return ()
              -- Check for exe dir
              exeDirPath <- findExeDir exeDir name
              case exeDirPath of
                Just d -> putStrLn $ "  Remove: " <> d
                Nothing -> return ()
              putStrLn $ "  Remove: " <> programManifest
            else do
              if binExists then removeFile binPath else return ()
              exeDirPath <- findExeDir exeDir name
              case exeDirPath of
                Just d -> removeDirectoryRecursive d
                Nothing -> return ()
              removeFile programManifest
              putStrLn $ "Uninstalled program '" <> name <> "'"
        else return ()

      return (removeModule || removeProgram)

-- | Find the exe directory for a program
findExeDir :: FilePath -> String -> IO (Maybe FilePath)
findExeDir exeDir name = do
  exists <- doesDirectoryExist exeDir
  if not exists
    then return Nothing
    else do
      entries <- listDirectory exeDir
      -- Look for name or name-<hash>
      let matches = filter (\e -> e == name || (name ++ "-") `isPrefixOf'` e) entries
      case matches of
        (m : _) -> return (Just (exeDir </> m))
        [] -> return Nothing
  where
    isPrefixOf' prefix str = take (length prefix) str == prefix

-- | Check if any other modules depend on the one being uninstalled
checkReverseDeps :: FilePath -> String -> IO ()
checkReverseDeps fdbDir name = do
  result <- try (listDirectory fdbDir) :: IO (Either SomeException [FilePath])
  case result of
    Left _ -> return ()
    Right entries -> do
      let moduleFiles = filter (".module" `isSuffixOf`) entries
          nameT = T.pack name
      forM_ moduleFiles $ \f -> do
        r <- try (BL.readFile (fdbDir </> f)) :: IO (Either SomeException BL.ByteString)
        case r of
          Left _ -> return ()
          Right bs -> case JSON.eitherDecode bs :: Either String ModuleManifest of
            Right m
              | nameT `elem` mmMorlocDeps m && mmName m /= nameT ->
                  putStrLn $ "Warning: module '" <> T.unpack (mmName m) <> "' depends on '" <> name <> "'"
            _ -> return ()
