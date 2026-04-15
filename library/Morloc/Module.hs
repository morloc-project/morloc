{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Module
Description : Module discovery, metadata loading, and installation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Handles all aspects of the morloc module system:

 * finding modules on the local filesystem (by name or path)
 * loading package YAML metadata
 * finding headers and shared libraries required by modules
 * installing modules from GitHub via @morloc install@
-}
module Morloc.Module
  ( findModule
  , loadModuleMetadata
  , findMainLocFile

    -- * Module installation
  , OverwriteProtocol (..)
  , GitProtocol (..)
  , InstallReason (..)
  , TypecheckFn
  , installModule
  , extractMorlocDeps
  , extractModuleName
  ) where

import Control.Applicative (optional)
import Control.Exception (onException)
import Text.Parsec (Parsec, try, parse, many, many1)
import Text.Parsec.Char (char, string, alphaNum, digit, satisfy)
import Text.Parsec.Text ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V  -- from aeson's transitive dependency
import qualified Data.Char as DC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Yaml.Config as YC
import qualified Morloc.Config as Config
import Morloc.Data.Doc
import Morloc.Data.Json
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import qualified Morloc.ProgramBuilder.Install as Install
import qualified Morloc.System as MS
import qualified Network.HTTP.Simple as HTTP
import System.Directory
import System.Process (callProcess, readProcess)

data InstallReason = ExplicitInstall | AutoDependency
  deriving (Show, Eq)

moduleInstallError :: MDoc -> MorlocMonad a
moduleInstallError msg = MM.throwSystemError $ "Failed to install module:" <+> msg

-- | Is this a local (.dot-prefixed) import?
isLocalImport :: MVar -> Bool
isLocalImport (MV x) = "." `MT.isPrefixOf` x

-- | Look for a morloc module: local (.dot-prefixed) or bare (system/plane).
findModule :: (Maybe Path, MVar) -> MVar -> MorlocMonad Path
findModule (_, currentModule) importModule
  | isLocalImport importModule = findLocalModule currentModule importModule
  | otherwise = findBareModule currentModule importModule

-- | Resolve a local import from the project root.
-- e.g., .foo.bar -> <root>/foo/bar.loc or <root>/foo/bar/main.loc
findLocalModule :: MVar -> MVar -> MorlocMonad Path
findLocalModule currentModule importModule = do
  projectRoot <- MM.gets stateProjectRoot
  case projectRoot of
    Nothing -> MM.throwSystemError $
      "Cannot resolve local import" <+> squotes (pretty importModule)
        <+> "without a project root (are you reading from stdin?)"
    Just root -> do
      let MV importText = importModule
          nameParts = map MT.unpack $ MT.splitOn "." (MT.drop 1 importText)
          candidates =
            [ MS.joinPath (root : init nameParts ++ [last nameParts ++ ".loc"])
            , MS.joinPath (root : nameParts ++ ["main.loc"])
            ]
      existingPaths <- liftIO . fmap catMaybes . mapM getFile $ candidates
      case existingPaths of
        (x : _) -> return x
        [] -> MM.throwSystemError $
          "Within module" <+> squotes (pretty currentModule)
            <> "," <+> "failed to import local module" <+> squotes (pretty importModule)
            <> "\nThe following paths were searched:\n"
            <+> indent 4 (vsep (map pretty candidates))

-- | Resolve a bare (non-dot-prefixed) import: search system/plane paths,
-- with deprecated fallback to local paths (project root).
-- Supports namespaced imports: "owner/name" searches lib/plane/owner/name/
-- Bare imports: "foo" searches lib/plane/morloclib/foo/ first, then lib/plane/foo/
findBareModule :: MVar -> MVar -> MorlocMonad Path
findBareModule currentModule importModule = do
  config <- MM.ask
  projectRoot <- MM.gets stateProjectRoot
  let lib = Config.configLibrary config
      plane = Config.configPlane config
      planeCore = Config.configPlaneCore config
      namePath = splitModuleName importModule
      -- Check if this is a namespaced import (contains "/")
      MV importText = importModule
      isNamespaced = "/" `MT.isInfixOf` importText
      -- For namespaced: "owner/name" -> [owner, name] as filesystem path
      -- For bare: "foo" -> search morloclib/foo first, then foo
      namespacedPath = case MT.splitOn "/" importText of
        [owner, name] -> [MT.unpack owner, MT.unpack name]
        _ -> namePath  -- fallback
      systemPaths
        | isNamespaced =
            -- Namespaced import: only search the explicit namespace path
            [ MS.joinPath ([lib, plane] <> namespacedPath <> ["main.loc"])
            , MS.joinPath ([lib, plane] <> init namespacedPath <> [last namespacedPath <> ".loc"])
            ]
        | otherwise =
            -- Bare import: search core org namespace first, then flat paths
            [ MS.joinPath ([lib, plane, planeCore] <> namePath <> ["main.loc"])
            , MS.joinPath ([lib, plane, planeCore] <> init namePath <> [last namePath <> ".loc"])
            , MS.joinPath ([lib, plane] <> init namePath <> [last namePath <> ".loc"])
            , MS.joinPath ([lib, plane] <> namePath <> ["main.loc"])
            , MS.joinPath (lib : init namePath <> [last namePath <> ".loc"])
            , MS.joinPath (lib : namePath <> ["main.loc"])
            ]
      localPaths
        | isNamespaced = []  -- namespaced imports are never local
        | otherwise = case projectRoot of
            Just root ->
              [ MS.joinPath (root : init namePath <> [last namePath <> ".loc"])
              , MS.joinPath (root : namePath <> ["main.loc"])
              ]
            Nothing ->
              [ MS.joinPath (init namePath <> [last namePath <> ".loc"])
              , MS.joinPath (namePath <> ["main.loc"])
              ]
  existingSystem <- liftIO . fmap catMaybes . mapM getFile $ systemPaths
  existingLocal <- liftIO . fmap catMaybes . mapM getFile $ localPaths
  case (existingSystem, existingLocal) of
    (s:_, l:_) ->
      MM.throwSystemError $
        "Ambiguous import" <+> squotes (pretty importModule)
          <+> "from module" <+> squotes (pretty currentModule)
          <> "\nFound in system:" <+> pretty s
          <> "\nFound locally:" <+> pretty l
          <> "\nUse" <+> squotes (pretty ("import ." <> unMVar importModule))
          <+> "for local or ensure the system module is installed"
    (x:_, []) -> return x
    ([], x:_) -> do
      MM.say $
        "WARNING: bare import" <+> squotes (pretty importModule)
          <+> "resolved locally."
          <+> "Use" <+> squotes (pretty ("import ." <> unMVar importModule))
          <+> "for explicit local imports."
      return x
    ([], []) -> do
      let allPaths = systemPaths <> localPaths
          nameNameLoc = namePath <> [last namePath <> ".loc"]
          hintPaths =
            map
              MS.joinPath
              [ nameNameLoc
              , lib : nameNameLoc
              , [lib, plane] <> nameNameLoc
              ]
      existingHints <- liftIO . fmap catMaybes . mapM getFile $ hintPaths
      let hintMsg = case existingHints of
            (found : _) ->
              let expected = MS.combine (MS.takeDirectory found) "main.loc"
               in "\n\nFound"
                    <+> squotes (pretty found)
                    <+> "but expected"
                    <+> squotes (pretty expected)
                    <> "\n  Rename the entry point: mv"
                      <+> pretty found
                      <+> pretty expected
            [] -> mempty
      MM.throwSystemError $
        "Within module" <+> squotes (pretty currentModule)
          <> ","
            <+> "failed to import module"
            <+> squotes (pretty importModule)
          <> "\n"
          <> "The following paths were searched:\n"
            <+> indent 4 (vsep (map pretty allPaths))
          <> "\nMaybe try running: morloc install" <+> pretty importModule
          <> hintMsg

{- | Give a module path (e.g. "/your/path/foo.loc") find the package metadata.
It currently only looks for a file named "package.yaml" in the same folder
as the main "*.loc" file.
-}
findModuleMetadata :: Path -> IO (Maybe Path)
findModuleMetadata mainFile =
  getFile $ MS.combine (MS.takeDirectory mainFile) "package.yaml"

loadModuleMetadata :: Path -> MorlocMonad ()
loadModuleMetadata main = do
  maybef <- liftIO $ findModuleMetadata main
  meta <-
    case maybef of
      (Just f) -> liftIO $ YC.loadYamlSettings [f] [] YC.ignoreEnv
      Nothing -> return defaultValue
  -- Reject include entries that escape the package directory. Absolute
  -- paths and `..` traversals are not allowed because they would break
  -- reproducibility and tie installs to ambient filesystem layout.
  liftIO $ Install.validateIncludeScope (packageInclude meta)
  state <- MM.get
  MM.put (appendMeta meta state)
  where
    appendMeta :: PackageMeta -> MorlocState -> MorlocState
    appendMeta m s = s {statePackageMeta = m : statePackageMeta s}

splitModuleName :: MVar -> [String]
splitModuleName (MV x) = map MT.unpack $ MT.splitOn "." x


getFile :: Path -> IO (Maybe Path)
getFile x = do
  exists <- MS.doesFileExist x
  return $
    if exists
      then Just x
      else Nothing

-- {{{ definitions

data GitProtocol = SshProtocol | HttpsProtocol
  deriving (Show, Eq, Ord)

data OverwriteProtocol
  = ForceOverwrite
  | DoNotOverwrite
  deriving (Show, Eq, Ord)

data RemoteSource
  = RemoteGithub
  | RemoteGitlab
  | RemoteBitbucket
  | RemoteCodeberg
  | RemoteAzure
  deriving (Show, Eq, Ord)

data GitSnapshotSelector
  = LatestDefaultBranch
  | LatestOnBranch Text
  | CommitHash Text
  | ReleaseTag Text
  deriving (Show, Eq, Ord)

data GitRemote = GitRemote
  { gitRemoteSource :: RemoteSource
  , gitReference :: GitSnapshotSelector
  , gitUsername :: Text
  , gitReponame :: Text
  }
  deriving (Show, Eq, Ord)

-- | Specify where a module is located
data ModuleSource
  = -- | Module in a local directory (may or may not be a git repo)
    ModuleSourceLocal Text (Maybe GitSnapshotSelector)
  | -- | A module stored in an arbitrary users github repo, e.g., (GithubRepo "weena" "math")
    ModuleSourceRemoteGit GitRemote
  | -- | A module from the morloc registry (owner, name)
    ModuleSourceRegistry Text Text
  deriving (Show, Eq, Ord)

-- }}}

-- | Check that a resolved module name is a valid identifier
validateModuleName :: Text -> Text -> Either Text Text
validateModuleName modstr name
  | MT.null name =
      Left $ "Could not determine module name from '" <> modstr <> "'"
  | not (DC.isAlphaNum (MT.head name)) =
      Left $ "Module name '" <> name <> "' (from '" <> modstr <> "') must start with an alphanumeric character"
  | MT.any (\c -> not (DC.isAlphaNum c) && c /= '-') name =
      Left $ "Module name '" <> name <> "' (from '" <> modstr <> "') contains invalid characters (only alphanumeric and hyphens allowed)"
  | otherwise = Right name

{- | Extract the module name from an install string.
For "github:user/repo" -> "repo", for "math" -> "math",
for "./path/to/foo" -> "foo", for "." -> current directory name
-}
extractModuleName :: Text -> IO Text
extractModuleName modstr = do
  name <- case parse (moduleInstallParser "morloclib") "" modstr of
    Right (Right (ModuleSourceLocal path _)) ->
      MT.pack . MS.takeFileName <$> (MS.makeAbsolute . MS.dropTrailingPathSeparator . MT.unpack $ path)
    Right (Right (ModuleSourceRemoteGit remote)) ->
      return $ gitReponame remote
    Right (Right (ModuleSourceRegistry _ n)) ->
      return n
    _ -> return modstr
  case validateModuleName modstr name of
    Left err -> ioError . userError $ MT.unpack err
    Right n -> return n

{- | Typecheck callback: takes a filepath, returns list of (name, type) exports.
Passed in from the executable layer to avoid circular imports.
-}
type TypecheckFn = FilePath -> MorlocMonad [(Text, Text)]

installModule ::
  -- | How should overwrites be handled
  OverwriteProtocol ->
  -- | Remote Git download protocol (HTTPS by default)
  GitProtocol ->
  -- | Absolute path to folder where modules are installed for the given plane
  Path ->
  -- | Default github org for the given plane for pulling core modules
  Path ->
  -- | Optional typecheck callback (Nothing = skip typecheck)
  Maybe TypecheckFn ->
  -- | User-specified module sources from the CLI batch (name -> install-string)
  Map.Map Text Text ->
  -- | Modules currently being installed (cycle detection)
  Set.Set Text ->
  -- | Why this module is being installed
  InstallReason ->
  -- | Installation string, such as "github:weena/math@version:0.1.0"
  Text ->
  MorlocMonad ()
installModule overwrite gitprot libpath coreorg mayTypecheck userSources inProgress reason modstr = do
  config <- MM.ask
  let registry = Config.configRegistry config
  -- Try registry first for bare names when a registry is configured
  case (registry, tryParseRegistryModule (MT.pack coreorg) modstr) of
    (Just _, Just (owner, name)) -> do
      let targetDir = libpath </> MT.unpack owner </> MT.unpack name
      if Set.member name inProgress
        then return ()
        else do
          targetExists <- liftIO $ doesDirectoryExist targetDir
          case (targetExists, overwrite) of
            (True, DoNotOverwrite) -> do
              case reason of
                ExplicitInstall ->
                  MM.say $ "Module" <+> pretty name <+> "is already installed, use --force to reinstall"
                AutoDependency ->
                  MM.sayVVV $ "Module" <+> pretty name <+> "already installed, skipping"
              return ()
            (True, ForceOverwrite) -> do
              liftIO $ removeDirectoryRecursive targetDir
              doInstall (ModuleSourceRegistry owner name) name targetDir
            (False, _) ->
              doInstall (ModuleSourceRegistry owner name) name targetDir
    _ -> installModuleClassic
  where
    installModuleClassic = case parse (moduleInstallParser (MT.pack coreorg)) "" modstr of
      (Left errstr) -> moduleInstallError (pretty . show $ errstr)
      (Right (Left errstr)) -> moduleInstallError $ pretty errstr
      (Right (Right source)) -> do
        rawName <- case source of
              ModuleSourceLocal path _ ->
                liftIO $ MT.pack . MS.takeFileName <$> (MS.makeAbsolute . MS.dropTrailingPathSeparator . MT.unpack $ path)
              ModuleSourceRemoteGit remote -> return $ gitReponame remote
              ModuleSourceRegistry _ n -> return n
        name <- case validateModuleName modstr rawName of
          Left err -> moduleInstallError $ pretty err
          Right n -> return n
        let targetDir = libpath </> MT.unpack name

        if Set.member name inProgress
          then return ()
          else do
            -- Check if already installed
            targetExists <- liftIO $ doesDirectoryExist targetDir
            case (targetExists, overwrite) of
              (True, DoNotOverwrite) -> do
                MM.sayVVV $ "Module" <+> pretty name <+> "already installed, skipping"
                return ()
              (True, ForceOverwrite) -> do
                liftIO $ removeDirectoryRecursive targetDir
                doInstall source name targetDir
              (False, _) ->
                doInstall source name targetDir


    doInstall :: ModuleSource -> Text -> FilePath -> MorlocMonad ()
    doInstall source name targetDir = do
      let inProgress' = Set.insert name inProgress

      -- create the library path if it is missing
      liftIO $ createDirectoryIfMissing True libpath

      -- Copy/clone files, with cleanup on exception
      liftIO $ createDirectoryIfMissing True (MS.takeDirectory targetDir)
      config' <- MM.ask
      let ioAction = case source of
            ModuleSourceLocal path selector ->
              installLocalIO targetDir selector path
            ModuleSourceRemoteGit remote ->
              installRemoteIO gitprot targetDir remote
            ModuleSourceRegistry owner' modName ->
              case Config.configRegistry config' of
                Just regUrl -> installFromRegistry regUrl owner' modName targetDir
                Nothing -> ioError $ userError "Registry URL not configured"
          cleanup = do
            exists <- doesDirectoryExist targetDir
            when exists $ removeDirectoryRecursive targetDir
      liftIO $ ioAction `onException` cleanup

      -- Read package.yaml for metadata and dependencies
      meta <- liftIO $ do
        let pkgYaml = targetDir </> "package.yaml"
        exists <- doesFileExist pkgYaml
        if exists
          then YC.loadYamlSettings [pkgYaml] [] YC.ignoreEnv
          else return defaultValue

      -- Determine morloc dependencies by scanning .loc imports
      morlocDeps <- do
        mainFile <- liftIO $ findMainLocFile targetDir (MT.unpack name)
        case mainFile of
          Nothing -> return []
          Just f -> liftIO $ extractMorlocDeps f

      -- Recursively install dependencies
      forM_ morlocDeps $ \dep -> do
        let depDir = libpath </> MT.unpack dep
            depDirNs = libpath </> coreorg </> MT.unpack dep
        depExists <- liftIO $ (||) <$> doesDirectoryExist depDir <*> doesDirectoryExist depDirNs
        unless (depExists || Set.member dep inProgress') $ do
          let depModstr = case Map.lookup dep userSources of
                Just s -> s
                Nothing -> dep
          MM.say $ "Auto-installing dependency:" <+> pretty dep
          installModule
            DoNotOverwrite
            gitprot
            libpath
            coreorg
            mayTypecheck
            userSources
            inProgress'
            AutoDependency
            depModstr

      -- Typecheck the module (if callback provided)
      exports <- case mayTypecheck of
        Nothing -> return []
        Just typecheckFn -> do
          mainFile <- liftIO $ findMainLocFile targetDir (MT.unpack name)
          case mainFile of
            Nothing -> return []
            Just f -> typecheckFn f

      -- Write module manifest to fdb/
      config <- MM.ask
      let fdbDir = Config.configHome config </> "fdb"
      liftIO $ createDirectoryIfMissing True fdbDir
      installTime <- liftIO $ floor <$> Time.getPOSIXTime
      let manifestPath = fdbDir </> MT.unpack name ++ ".module"
          manifestJson =
            buildModuleManifest
              meta
              name
              morlocDeps
              exports
              targetDir
              modstr
              reason
              installTime
      liftIO $ TIO.writeFile manifestPath manifestJson
      MM.say $ "Installed module" <+> squotes (pretty name)

-- | Find the main .loc file in a module directory
findMainLocFile :: FilePath -> String -> IO (Maybe FilePath)
findMainLocFile dir name = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then return Nothing
    else do
      let mainLoc = dir </> "main.loc"
          nameLoc = dir </> name ++ ".loc"
      mainExists <- doesFileExist mainLoc
      if mainExists
        then return (Just mainLoc)
        else do
          nameExists <- doesFileExist nameLoc
          return $ if nameExists then Just nameLoc else Nothing

-- | Build a module manifest JSON string
buildModuleManifest ::
  PackageMeta ->
  Text ->
  [Text] ->
  [(Text, Text)] ->
  FilePath ->
  Text ->
  InstallReason ->
  Int ->
  Text
buildModuleManifest meta name morlocDeps exports installPath installSource reason installTime =
  jsonObj
    [ ("kind", jsonStr "module")
    , ("name", jsonStr name)
    , ("version", jsonStr (packageVersion meta))
    , ("synopsis", jsonStr (packageSynopsis meta))
    , ("author", jsonStr (packageAuthor meta))
    , ("license", jsonStr (packageLicense meta))
    , ("homepage", jsonStr (packageHomepage meta))
    , ("c_dependencies", jsonStrArr (packageDependencies meta))
    , ("morloc_dependencies", jsonStrArr morlocDeps)
    ,
      ( "exports"
      , jsonArr
          [ jsonObj [("name", jsonStr n), ("type", jsonStr t)]
          | (n, t) <- exports
          ]
      )
    , ("install_path", jsonStr (MT.pack installPath))
    , ("install_source", jsonStr installSource)
    , ("install_reason", jsonStr (reasonText reason))
    , ("install_time", jsonInt installTime)
    ]
  where
    reasonText ExplicitInstall = "explicit"
    reasonText AutoDependency = "auto"

{- | Extract morloc module dependencies by scanning a .loc file for import statements.
This is a lightweight text scan, not using the full parser.
-}
extractMorlocDeps :: FilePath -> IO [Text]
extractMorlocDeps path = do
  content <- TIO.readFile path
  let ls = MT.lines content
      imports = concatMap extractImport (removeComments ls)
  return (unique imports)
  where
    extractImport :: Text -> [Text]
    extractImport ln =
      let stripped = MT.stripStart ln
       in case MT.stripPrefix "import " stripped of
            Nothing -> []
            Just rest ->
              let modName = MT.strip (MT.takeWhile (\c -> c /= '(' && c /= ' ') rest)
               in if "." `MT.isPrefixOf` modName
                    then [] -- skip local (.dot-prefixed) imports
                    else if "/" `MT.isInfixOf` modName
                      then [modName]  -- namespaced import: keep "owner/name" as-is
                      else case MT.splitOn "." modName of
                             (topLevel : _) | not (MT.null topLevel) -> [topLevel]
                             _ -> []

    removeComments :: [Text] -> [Text]
    removeComments = go False
      where
        go _ [] = []
        go True (l : ls)
          | MT.isInfixOf "-}" l = go False ls
          | otherwise = go True ls
        go False (l : ls)
          | MT.isPrefixOf "--" (MT.stripStart l) = go False ls
          | MT.isPrefixOf "{-" (MT.stripStart l) = go True ls
          | otherwise = l : go False ls

-- {{{ parse module source

type Parser = Parsec Text ()

decimal :: Parser Int
decimal = read <$> many1 digit

data ModulePath
  = ModulePathCore Text
  | ModulePathGit Text Text
  | ModulePathLocal Text
  deriving (Show, Eq, Ord)

data RefForm = RefHash | RefBranch | RefVersion

moduleInstallParser :: Text -> Parser (Either Text ModuleSource)
moduleInstallParser coreorg = do
  maySrcform <- optional (try parseSrcForm)
  modPath <- parseModname maySrcform
  ref <- optional (try parseRef)
  return $ makeModuleSource maySrcform modPath ref
  where
    makeModuleSource ::
      Maybe RemoteSource -> ModulePath -> Maybe GitSnapshotSelector -> Either Text ModuleSource
    makeModuleSource mayRemote (ModulePathCore modname) selector
      | mayRemote == Just RemoteGithub || mayRemote == Nothing =
          return . ModuleSourceRemoteGit $
            GitRemote
              { gitRemoteSource = RemoteGithub
              , gitReference = fromMaybe LatestDefaultBranch selector
              , gitUsername = coreorg
              , gitReponame = modname
              }
      | otherwise = Left "Core modules are only imported from github"
    makeModuleSource (Just _) (ModulePathLocal _) _ =
      Left "Invalid mix of local and remote import names"
    makeModuleSource (maybe RemoteGithub id -> remote) (ModulePathGit user repo) selector =
      return . ModuleSourceRemoteGit $
        GitRemote
          { gitRemoteSource = remote
          , gitReference = fromMaybe LatestDefaultBranch selector
          , gitUsername = user
          , gitReponame = repo
          }
    makeModuleSource Nothing (ModulePathLocal path) selector = return $ ModuleSourceLocal path selector

-- codeberg:weena/calendar@version:1.0.0
-- --------
parseSrcForm :: Parser RemoteSource
parseSrcForm = do
  remote <-
    try (string "github" >> return RemoteGithub)
      <|> try (string "gitlab" >> return RemoteGitlab)
      <|> try (string "bitbucket" >> return RemoteBitbucket)
      <|> try (string "codeberg" >> return RemoteCodeberg)
      <|> (string "azure" >> return RemoteAzure)
  _ <- char ':'
  return remote

-- codeberg:???????@version:1.0.0
--          -------
parseModname :: Maybe RemoteSource -> Parser ModulePath
parseModname (Just _) = try parseRemoteModule <|> parseCoreModule
parseModname Nothing =
  parseCoreModule -- must start with letter
    <|> parseLocalModule -- must start with [.~/]

-- codeberg:weena/calendar@version:1.0.0
--          --------------
parseRemoteModule :: Parser ModulePath
parseRemoteModule = do
  user <- parseModuleSegment
  _ <- char '/'
  repo <- parseModuleSegment
  return $ ModulePathGit user repo

-- root@version:1.0.0
-- ----
parseCoreModule :: Parser ModulePath
parseCoreModule = ModulePathCore <$> parseModuleSegment

parseModuleSegment :: Parser Text
parseModuleSegment = do
  firstChar <- alphaNum
  rest <- many (alphaNum <|> char '-')
  case rest of
    [] -> return (MT.pack [firstChar])
    _ | last rest == '-' -> fail "Module name cannot end with a dash"
      | otherwise -> return (MT.pack (firstChar : rest))

-- parse a local file
--   .
--   ./my/morloc/dir
--   ~/my/mod
parseLocalModule :: Parser ModulePath
parseLocalModule = do
  fstChar <- char '.' <|> char '/' <|> char '~'
  remaining <- MT.pack <$> many (satisfy (/= '@'))
  return $ ModulePathLocal (MT.cons fstChar remaining)

-- codeberg:weena/calendar@version:1.0.0
--                         -------------
parseRef :: Parser GitSnapshotSelector
parseRef = do
  char '@'
  mayForm <- optional (try parseRefForm)
  parseRefStr mayForm

-- codeberg:weena/calendar@version:1.0.0
--                         -------
parseRefForm :: Parser RefForm
parseRefForm = do
  form <-
    try (string "hash" >> return RefHash)
      <|> try (string "branch" >> return RefBranch)
      <|> try (string "version" >> return RefVersion)
      <|> try (string "tag" >> return RefVersion) -- same diff
  char ':'
  return form

-- codeberg:weena/calendar@version:1.0.0
--                                 -----
parseRefStr :: Maybe RefForm -> Parser GitSnapshotSelector
parseRefStr Nothing =
  try parseHash
    <|> try parseVersion
    <|> try parseBranch
parseRefStr (Just RefHash) = parseHash
parseRefStr (Just RefVersion) = parseVersion
parseRefStr (Just RefBranch) = parseBranch

-- match hexadecimal characters of 7 characters or more
parseHash :: Parser GitSnapshotSelector
parseHash = do
  hash <- MT.pack <$> many1 (satisfy isHexDigit)
  if MT.length hash >= 7
    then return $ CommitHash hash
    else fail "Hash must be at least 7 characters"
  where
    isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- match semantic version (with option of omitting patch, so v1.0 is legal)
parseVersion :: Parser GitSnapshotSelector
parseVersion = do
  version <- versionParser
  return $ ReleaseTag version
  where
    versionParser = do
      -- Optional 'v' prefix
      v <- optional (MT.pack <$> string "v")

      -- Parse major.minor
      major <- MT.show' <$> decimal
      _ <- char '.'
      minor <- MT.show' <$> decimal

      -- Optional .patch
      patchMay <- optional . try $ do
        _ <- char '.'
        MT.show' <$> decimal

      -- Optional pre-release (after '-')
      preRelease <- optional . try $ do
        _ <- char '-'
        MT.pack <$> many1 (satisfy (\c -> DC.isAlphaNum c || c == '.'))

      -- Optional build metadata (after '+')
      buildMeta <- optional . try $ do
        _ <- char '+'
        MT.pack <$> many1 (satisfy (\c -> DC.isAlphaNum c || c == '.'))

      -- Reconstruct the full version string
      return $
        mconcat
          [ fromMaybe "" v
          , major
          , "."
          , minor
          , maybe "" ("." <>) patchMay
          , maybe "" ("-" <>) preRelease
          , maybe "" ("+" <>) buildMeta
          ]

{- | Parse a legal git branch name according to git-check-ref-format

Git reference naming rules (from git-check-ref-format man page):
1. Cannot begin or end with slash '/'
2. Cannot contain two consecutive dots '..'
3. Cannot contain ASCII control characters (< 0x20), space, ~, ^, :, ?, *, [
4. Cannot end with '.lock'
5. Cannot end with a dot '.'
6. Cannot contain a backslash '\'
7. Cannot contain '@{' sequence (reflog syntax)
8. Cannot be a single '@' character
9. Components between slashes cannot begin with a dot '.'
10. Cannot contain multiple consecutive slashes '//'

Current implementation deviations from spec:
- Missing: check for '@{' sequence (reflog syntax - rule 7)
- Missing: check for '@' as sole character (rule 8)
- Missing: check that components don't start with '.' (rule 9)
- Missing: check for consecutive slashes '//' (rule 10)
- Missing: check for ASCII control characters (< 0x20)
- Incomplete: allows '@' freely, but spec restricts it in certain contexts
-}
parseBranch :: Parser GitSnapshotSelector
parseBranch = do
  branch <- MT.pack <$> many1 (satisfy isBranchChar)

  if isValidBranchName branch
    then return $ LatestOnBranch branch
    else fail $ "Invalid git branch name: " ++ MT.unpack branch
  where
    -- Characters allowed in branch names
    -- Note: We're permissive in parsing, strict in validation
    isBranchChar c =
      c > '\x1F' -- No ASCII control characters (0x00-0x1F)
        && c /= ' '
        && c /= '~'
        && c /= '^'
        && c /= ':'
        && c /= '?'
        && c /= '*'
        && c /= '['
        && c /= '\\'
        && c /= '\DEL' -- Also exclude DEL (0x7F)

    -- Comprehensive validation according to git-check-ref-format
    isValidBranchName branchName =
      not (MT.null branchName) -- Must have content
        && branchName /= "@" -- Cannot be just '@'
        && not (MT.isPrefixOf "/" branchName) -- Cannot start with /
        && not (MT.isSuffixOf "/" branchName) -- Cannot end with /
        && not (MT.isSuffixOf "." branchName) -- Cannot end with .
        && not (MT.isSuffixOf ".lock" branchName) -- Cannot end with .lock
        && not (".." `MT.isInfixOf` branchName) -- No consecutive dots
        && not ("@{" `MT.isInfixOf` branchName) -- No reflog syntax
        && not ("//" `MT.isInfixOf` branchName) -- No consecutive slashes
        && not (hasComponentStartingWithDot branchName) -- Components can't start with .
        && not (MT.any isInvalidChar branchName) -- Final safety check

    -- Check if any path component starts with a dot
    hasComponentStartingWithDot txt =
      let components = MT.splitOn "/" txt
       in any (MT.isPrefixOf ".") components

    -- Characters that should never appear (belt-and-suspenders check)
    isInvalidChar c =
      c <= '\x1F' -- Control characters
        || c == '\DEL'
        || c `elem` (" ~^:?*[\\" :: String)

-- }}}

-- | Try to parse a module string as a registry-resolvable module.
-- Bare names like "foo" -> (coreorg, "foo").
-- Namespaced names like "user/foo" -> ("user", "foo").
-- Returns Nothing for local paths, explicit remote sources, or git refs.
tryParseRegistryModule :: Text -> Text -> Maybe (Text, Text)
tryParseRegistryModule coreorg modstr
  -- Reject anything with remote source prefixes, git ref selectors, or local paths
  | MT.any (\c -> c == ':' || c == '@' || c == '.' || c == '~') modstr = Nothing
  -- Namespaced: "owner/name"
  | "/" `MT.isInfixOf` modstr =
      case MT.splitOn "/" modstr of
        [owner, name] | isValidSegment owner && isValidSegment name -> Just (owner, name)
        _ -> Nothing
  -- Bare name: resolve to core org
  | isValidSegment modstr = Just (coreorg, modstr)
  | otherwise = Nothing
  where
    isValidSegment t =
      not (MT.null t)
        && DC.isAlphaNum (MT.head t)
        && MT.all (\c -> DC.isAlphaNum c || c == '-') t

-- | Install a module from the morloc registry by downloading its tarball.
installFromRegistry :: Text -> Text -> Text -> FilePath -> IO ()
installFromRegistry registryUrl owner name targetDir = do
  -- Get latest version metadata
  let metaUrl = MT.unpack registryUrl <> "/api/registry/" <> MT.unpack owner <> "/" <> MT.unpack name
  metaReq <- HTTP.parseRequest metaUrl
  metaResp <- HTTP.httpLBS metaReq
  let metaStatus = HTTP.getResponseStatusCode metaResp
  when (metaStatus /= 200) $
    ioError . userError $
      "Registry lookup failed for " <> MT.unpack owner <> "/" <> MT.unpack name
        <> " (HTTP " <> show metaStatus <> ")"

  -- Extract latest version from the response
  let metaBody = HTTP.getResponseBody metaResp
  latestVersion <- case Aeson.decode metaBody of
    Just (Aeson.Object obj) -> case KM.lookup "versions" obj of
      Just (Aeson.Array arr) | not (V.null arr) -> case V.head arr of
        Aeson.Object vobj -> case KM.lookup "version" vobj of
          Just (Aeson.String v) -> return v
          _ -> ioError $ userError "Could not parse version from registry response"
        _ -> ioError $ userError "Could not parse version from registry response"
      _ -> ioError $ userError "No versions found for module"
    _ -> ioError $ userError "Could not parse registry response"

  -- Download tarball
  let tarUrl = metaUrl <> "/" <> MT.unpack latestVersion <> "/tarball"
  tarReq <- HTTP.parseRequest tarUrl
  tarResp <- HTTP.httpLBS tarReq
  let tarStatus = HTTP.getResponseStatusCode tarResp
  when (tarStatus /= 200) $
    ioError . userError $
      "Tarball download failed for " <> MT.unpack owner <> "/" <> MT.unpack name
        <> "@" <> MT.unpack latestVersion <> " (HTTP " <> show tarStatus <> ")"

  let tarball = HTTP.getResponseBody tarResp

  -- Extract tarball to target directory
  createDirectoryIfMissing True targetDir
  let tarballPath = targetDir <> ".tar.gz"
  BL.writeFile tarballPath tarball

  -- Verify SHA-256 if provided by the server
  case HTTP.getResponseHeader "X-Checksum-Sha256" tarResp of
    (expectedHash:_) -> do
      output <- readProcess "sha256sum" [tarballPath] ""
      let actualHash = takeWhile (/= ' ') output
          expected = BS8.unpack expectedHash
      when (actualHash /= expected) $ do
        removeFile tarballPath
        ioError . userError $
          "SHA-256 mismatch for " <> MT.unpack owner <> "/" <> MT.unpack name
    [] -> return ()

  readProcess "tar" ["xzf", tarballPath, "-C", targetDir] ""
  removeFile tarballPath

-- {{{ install from module source (IO-level helpers)

-- | Install from a local source (pure IO, no MorlocMonad)
installLocalIO ::
  FilePath -> Maybe GitSnapshotSelector -> Text -> IO ()
installLocalIO targetDir maySelector modulePath = do
  sourceDir <- MS.makeAbsolute . MS.dropTrailingPathSeparator . MT.unpack $ modulePath

  sourceExists <- doesDirectoryExist sourceDir
  unless sourceExists $
    ioError $ userError $
      "Source directory does not exist: " ++ sourceDir

  let gitDir = sourceDir </> ".git"
  isGitRepo <- doesDirectoryExist gitDir

  if isGitRepo
    then installLocalGitRepoIO sourceDir targetDir (fromMaybe LatestDefaultBranch maySelector)
    else callProcess "cp" ["-r", sourceDir, targetDir]

installLocalGitRepoIO :: FilePath -> FilePath -> GitSnapshotSelector -> IO ()
installLocalGitRepoIO sourceDir targetDir selector = do
  case selector of
    LatestDefaultBranch ->
      -- Just copy the working tree, then strip .git
      callProcess "cp" ["-r", sourceDir, targetDir]
    LatestOnBranch branch -> do
      callProcess "git" ["clone", "-q", sourceDir, targetDir]
      callProcess "git" ["-C", targetDir, "checkout", "refs/heads/" ++ MT.unpack branch]
    CommitHash hash -> do
      callProcess "git" ["clone", "-q", sourceDir, targetDir]
      callProcess "git" ["-C", targetDir, "checkout", "--detach", MT.unpack hash]
    ReleaseTag tag -> do
      callProcess "git" ["clone", "-q", sourceDir, targetDir]
      callProcess "git" ["-C", targetDir, "checkout", "refs/tags/" ++ MT.unpack tag]

  -- Remove .git directory from installed copy
  let gitDir = targetDir </> ".git"
  gitExists <- doesDirectoryExist gitDir
  when gitExists $ removeDirectoryRecursive gitDir

-- | Install from a remote git source (pure IO)
installRemoteIO :: GitProtocol -> FilePath -> GitRemote -> IO ()
installRemoteIO gitprot targetDir remote = do
  let gitUrl = buildGitUrl gitprot remote
  callProcess "git" ["clone", "-q", gitUrl, targetDir]
  checkoutRefIO targetDir (gitReference remote)

-- | Build a git URL from protocol and remote info
buildGitUrl :: GitProtocol -> GitRemote -> String
buildGitUrl protocol remote =
  let username = MT.unpack $ gitUsername remote
      reponame = MT.unpack $ gitReponame remote
      source = gitRemoteSource remote
      baseUrl = getBaseUrl source
   in case protocol of
        HttpsProtocol -> "https://" ++ baseUrl ++ "/" ++ username ++ "/" ++ reponame
        SshProtocol -> "git@" ++ baseUrl ++ ":" ++ username ++ "/" ++ reponame ++ ".git"

-- | Get base URL for each remote source
getBaseUrl :: RemoteSource -> String
getBaseUrl RemoteGithub = "github.com"
getBaseUrl RemoteGitlab = "gitlab.com"
getBaseUrl RemoteBitbucket = "bitbucket.org"
getBaseUrl RemoteCodeberg = "codeberg.org"
getBaseUrl RemoteAzure = "dev.azure.com"

-- | Checkout a specific git reference (pure IO)
checkoutRefIO :: FilePath -> GitSnapshotSelector -> IO ()
checkoutRefIO targetDir selector = case selector of
  LatestDefaultBranch -> return ()
  LatestOnBranch branch -> do
    callProcess "git" ["-C", targetDir, "checkout", MT.unpack branch]
    callProcess "git" ["-C", targetDir, "pull"]
  CommitHash hash ->
    callProcess "git" ["-C", targetDir, "checkout", MT.unpack hash]
  ReleaseTag tag ->
    callProcess "git" ["-C", targetDir, "checkout", "tags/" ++ MT.unpack tag]

-- }}}
