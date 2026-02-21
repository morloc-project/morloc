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

    -- * Module installation
  , OverwriteProtocol (..)
  , GitProtocol (..)
  , InstallReason (..)
  , TypecheckFn
  , installModule
  , extractMorlocDeps
  , extractModuleName
  ) where

import Control.Exception (onException)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
import qualified Morloc.System as MS
import System.Directory
import System.Process (callProcess)

data InstallReason = ExplicitInstall | AutoDependency
  deriving (Show, Eq)

moduleInstallError :: MDoc -> MorlocMonad a
moduleInstallError msg = MM.throwSystemError $ "Failed to install module:" <+> msg

-- | Look for a local morloc module.
findModule :: (Maybe Path, MVar) -> MVar -> MorlocMonad Path
findModule currentPathAndModule@(_, currentModule) importModule = do
  config <- MM.ask
  let lib = Config.configLibrary config
      plane = Config.configPlane config
      allPaths = getModulePaths lib plane currentPathAndModule importModule
  existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
  case existingPaths of
    [x] -> return x
    xs@(x : _) -> do
      MM.say $
        "WARNING: module path shadowing, for"
          <+> pretty importModule
          <+> "from module"
          <+> pretty currentModule
          <+> "found paths:"
          <+> "\n  "
          <> pretty xs
          <> "\n  selecting path:" <+> pretty x
      return x
    [] -> do
      -- Check if <name>/<name>.loc exists (should be <name>/main.loc)
      let namePath = splitModuleName importModule
          nameNameLoc = namePath <> [last namePath <> ".loc"]
          hintPaths = map MS.joinPath
            [ nameNameLoc
            , lib : nameNameLoc
            , [lib, plane] <> nameNameLoc
            ]
      existingHints <- liftIO . fmap catMaybes . mapM getFile $ hintPaths
      let hintMsg = case existingHints of
            (found:_) ->
              let expected = MS.combine (MS.takeDirectory found) "main.loc"
              in "\n\nFound" <+> squotes (pretty found)
                  <+> "but expected" <+> squotes (pretty expected)
                  <> "\n  Rename the entry point: mv"
                    <+> pretty found <+> pretty expected
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
  state <- MM.get
  MM.put (appendMeta meta state)
  where
    appendMeta :: PackageMeta -> MorlocState -> MorlocState
    appendMeta m s = s {statePackageMeta = m : statePackageMeta s}

splitModuleName :: MVar -> [String]
splitModuleName (MV x) = map MT.unpack $ MT.splitOn "." x

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []
commonPrefix _ _ = []

removePathSuffix :: [String] -> [String] -> [String]
removePathSuffix [] ys = ys
removePathSuffix _ [] = []
removePathSuffix xs ys
  | stringPath (last xs) == stringPath (last ys) = removePathSuffix (init xs) (init ys)
  | otherwise = ys
  where
    stringPath :: String -> String
    stringPath s
      | last s == '/' = init s
      | otherwise = s

-- | Find an ordered list of possible locations to search for a module
getModulePaths ::
  -- | morloc default library path (e.g., "~/.morloc/src/morloc")
  Path ->
  -- | morloc plane (e.g., "morloclib")
  Path ->
  -- | the path and name of the current module (e.g., `Just ("foo.loc", MVar "bar")`)
  (Maybe Path, MVar) ->
  -- | the name of the module that is being imported
  MVar ->
  [Path]
-- CASE #1
--   If we are not in a module, then the import may be from the system or
--   the local "working" directory.
--
-- Example:
--   Give the following code in  /../src/foo/bar/baz/main.loc
-- ```
-- import bif.buf
-- ```
-- `bif.buf` may be imported locally or from the system
-- --  1. /../src/foo/bar/baz/bif/buf/main.loc
-- --  2. $MORLOC_LIB/src/bif/buf/main.loc
getModulePaths lib plane (Nothing, _) (splitModuleName -> namePath) = map MS.joinPath paths
  where
    -- either search the working directory for a fife like "math.loc" or look
    -- for a folder named after the module with with a "main.loc" script
    localPaths =
      [ init namePath <> [last namePath <> ".loc"]
      , namePath <> ["main.loc"]
      ]

    -- if nothing is local, look for system libraries. The system libraries MUST
    -- be in a folder named after the module. The entry point for directory
    -- modules must be "main.loc".
    systemPaths =
      [ lib : init namePath <> [last namePath <> ".loc"]
      , lib : namePath <> ["main.loc"]
      ]

    planePaths =
      [ [lib, plane] <> init namePath <> [last namePath <> ".loc"]
      , [lib, plane] <> namePath <> ["main.loc"]
      ]

    paths = localPaths <> planePaths <> systemPaths
getModulePaths lib plane (Just (MS.splitPath -> modulePath), splitModuleName -> moduleName) (splitModuleName -> importName) =
  case commonPrefix moduleName importName of
    -- CASE #2
    --   If we are in a module, and if the module name path and the import name
    --   path do no share a common root, then look for the import in the system
    --   library or local working directory.
    --
    -- Example:
    --   Give the following code in file /../src/foo/bar/baz/main.loc
    -- ```
    -- module foo.bar.baz
    -- import bif.buf
    -- ```
    -- The only where `bif.buf` may be foud is the system library:
    --   $MORLOC_LIB/src/bif/buf/main.loc
    [] ->
      map
        MS.joinPath
        [ -- system paths
          lib : init importName <> [last importName <> ".loc"]
        , lib : importName <> ["main.loc"]
        , -- plane paths
          [lib, plane] <> init importName <> [last importName <> ".loc"]
        , [lib, plane] <> importName <> ["main.loc"]
        , -- local path
          init importName <> [last importName <> ".loc"]
        , importName <> ["main.loc"]
        ]
    -- CASE #3
    --   If we are in a module, and if the module name path shares a common
    --   root with the import name path, then the module file paths must
    --   share a common root.
    -- Example:
    -- Give the following code in file /../src/foo/bar/baz/main.loc
    --
    -- ```
    -- module foo.bar.baz
    -- import foo.bif
    -- ```
    --
    -- The only place where `foo.bif` may be found is:
    --    /../src/foo/bif/main.loc
    --
    -- For now I do not allow local directory imports that mask the module. So
    -- the import `foo.bif` in the module `foo.bar.baz` will not search for the
    -- local directory "foo". Maybe I should allow this?
    -- _ -> error $ show (modulePath, importName, moduleName)
    _ ->
      let rootPath =
            if last modulePath == "main.loc"
              -- e.g., `/../src/foo/bar/baz/main.loc` -> '/../src/'
              then removePathSuffix moduleName (init modulePath)
              -- e.g., `/../src/foo/bar/baz.loc`  -> '/../src/'
              else removePathSuffix (init moduleName) (init modulePath)
       in map
            MS.joinPath
            [ rootPath <> init importName <> [last importName <> ".loc"]
            , rootPath <> importName <> ["main.loc"]
            ]

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
  deriving (Show, Eq, Ord)

-- }}}

-- | Extract the module name from an install string.
-- For "github:user/repo" -> "repo", for "math" -> "math",
-- for "./path/to/foo" -> "foo"
extractModuleName :: Text -> Text
extractModuleName modstr =
  case parse (moduleInstallParser "morloclib") "" modstr of
    Right (Right (ModuleSourceLocal path _)) ->
      MT.pack . MS.takeFileName . MS.dropTrailingPathSeparator . MT.unpack $ path
    Right (Right (ModuleSourceRemoteGit remote)) ->
      gitReponame remote
    _ -> modstr

-- | Typecheck callback: takes a filepath, returns list of (name, type) exports.
-- Passed in from the executable layer to avoid circular imports.
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
installModule overwrite gitprot libpath coreorg mayTypecheck userSources inProgress reason modstr =
  case parse (moduleInstallParser (MT.pack coreorg)) "" modstr of
    (Left errstr) -> moduleInstallError (pretty . errorBundlePretty $ errstr)
    (Right (Left errstr)) -> moduleInstallError $ pretty errstr
    (Right (Right source)) -> do
      let name = case source of
            ModuleSourceLocal path _ ->
              MT.pack . MS.takeFileName . MS.dropTrailingPathSeparator . MT.unpack $ path
            ModuleSourceRemoteGit remote -> gitReponame remote
          targetDir = libpath </> MT.unpack name

      -- Cycle detection
      when (Set.member name inProgress) $ return ()

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
  where
    doInstall :: ModuleSource -> Text -> FilePath -> MorlocMonad ()
    doInstall source name targetDir = do
      let inProgress' = Set.insert name inProgress

      -- create the library path if it is missing
      liftIO $ createDirectoryIfMissing True libpath

      -- Copy/clone files, with cleanup on exception
      liftIO $ createDirectoryIfMissing True (MS.takeDirectory targetDir)
      let ioAction = case source of
            ModuleSourceLocal path selector ->
              installLocalIO libpath selector path
            ModuleSourceRemoteGit remote ->
              installRemoteIO gitprot targetDir remote
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
        depExists <- liftIO $ doesDirectoryExist depDir
        unless (depExists || Set.member dep inProgress') $ do
          let depModstr = case Map.lookup dep userSources of
                Just s -> s
                Nothing -> dep
          MM.say $ "Auto-installing dependency:" <+> pretty dep
          installModule
            DoNotOverwrite gitprot libpath coreorg
            mayTypecheck userSources inProgress'
            AutoDependency depModstr

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
          manifestJson = buildModuleManifest meta name morlocDeps exports
            targetDir modstr reason installTime
      liftIO $ TIO.writeFile manifestPath manifestJson
      MM.say $ "Installed module" <+> squotes (pretty name)

-- | Find the main .loc file in a module directory
findMainLocFile :: FilePath -> String -> IO (Maybe FilePath)
findMainLocFile dir name = do
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
  PackageMeta -> Text -> [Text] -> [(Text, Text)] ->
  FilePath -> Text -> InstallReason -> Int -> Text
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
    , ("exports", jsonArr
        [ jsonObj [("name", jsonStr n), ("type", jsonStr t)]
        | (n, t) <- exports
        ])
    , ("install_path", jsonStr (MT.pack installPath))
    , ("install_source", jsonStr installSource)
    , ("install_reason", jsonStr (reasonText reason))
    , ("install_time", jsonInt installTime)
    ]
  where
    reasonText ExplicitInstall = "explicit"
    reasonText AutoDependency = "auto"

-- | Extract morloc module dependencies by scanning a .loc file for import statements.
-- This is a lightweight text scan, not using the full parser.
extractMorlocDeps :: FilePath -> IO [Text]
extractMorlocDeps path = do
  content <- TIO.readFile path
  let ls = MT.lines content
      imports = concatMap extractImport (removeComments ls)
  return (unique imports)
  where
    extractImport :: Text -> [Text]
    extractImport line =
      let stripped = MT.stripStart line
      in case MT.stripPrefix "import " stripped of
        Nothing -> []
        Just rest ->
          let modName = MT.strip (MT.takeWhile (\c -> c /= '(' && c /= ' ') rest)
              topLevel = head (MT.splitOn "." modName)
          in if MT.null topLevel then [] else [topLevel]

    removeComments :: [Text] -> [Text]
    removeComments = go False
      where
        go _ [] = []
        go True (l:ls)
          | MT.isInfixOf "-}" l = go False ls
          | otherwise = go True ls
        go False (l:ls)
          | MT.isPrefixOf "--" (MT.stripStart l) = go False ls
          | MT.isPrefixOf "{-" (MT.stripStart l) = go True ls
          | otherwise = l : go False ls

-- {{{ parse module source

type Parser = Parsec Void Text

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
  ref <- optional parseRef
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
      <|> (string "gitlab" >> return RemoteGitlab)
      <|> (string "bitbucket" >> return RemoteBitbucket)
      <|> (string "codeberg" >> return RemoteCodeberg)
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
  firstChar <- alphaNumChar
  rest <- many (alphaNumChar <|> char '-')
  -- Ensure we don't end with dash if there are rest chars
  if last rest == '-'
    then fail "Module name cannot end with a dash"
    else return (MT.pack (firstChar : rest))

-- parse a local file
--   .
--   ./my/morloc/dir
--   ~/my/mod
parseLocalModule :: Parser ModulePath
parseLocalModule = do
  fstChar <- char '.' <|> char '/' <|> char '~'
  remaining <- takeWhileP Nothing (/= '@')
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
  hash <- takeWhile1P (Just "hex digit") isHexDigit
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
      v <- optional (string "v")

      -- Parse major version
      major <- MT.show' <$> (L.decimal :: Parser Int)
      _ <- char '.'

      -- Parse minor version
      minor <- MT.show' <$> (L.decimal :: Parser Int)
      _ <- char '.'

      -- Optional patch version
      patchMay <- optional $ do
        p <- MT.show' <$> (L.decimal :: Parser Int)
        _ <- char '.'
        return p

      -- Optional pre-release (after '-')
      preRelease <- optional $ do
        _ <- char '-'
        takeWhile1P Nothing (\c -> DC.isAlphaNum c || c == '.')

      -- Optional build metadata (after '+')
      buildMeta <- optional $ do
        _ <- char '+'
        takeWhile1P Nothing (\c -> DC.isAlphaNum c || c == '.')

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
  branch <- takeWhile1P (Just "branch character") isBranchChar

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

-- {{{ install from module source (IO-level helpers)

-- | Install from a local source (pure IO, no MorlocMonad)
installLocalIO ::
  Path -> Maybe GitSnapshotSelector -> Text -> IO ()
installLocalIO libpath maySelector modulePath = do
  sourceDir <- MS.makeAbsolute . MS.dropTrailingPathSeparator . MT.unpack $ modulePath
  let moduleName = MS.takeFileName sourceDir
      targetDir = libpath </> moduleName

  sourceExists <- doesDirectoryExist sourceDir
  unless sourceExists $
    error $ "Source directory does not exist: " ++ sourceDir

  createDirectoryIfMissing True libpath

  let gitDir = sourceDir </> ".git"
  isGitRepo <- doesDirectoryExist gitDir

  if isGitRepo
    then installLocalGitRepoIO sourceDir targetDir (fromMaybe LatestDefaultBranch maySelector)
    else callProcess "cp" ["-r", sourceDir, targetDir]

installLocalGitRepoIO :: FilePath -> FilePath -> GitSnapshotSelector -> IO ()
installLocalGitRepoIO sourceDir targetDir selector = do
  case selector of
    LatestDefaultBranch -> callProcess "cp" ["-r", sourceDir, targetDir]
    _ -> callProcess "git" ["clone", "-q", sourceDir, targetDir]

  case selector of
    LatestDefaultBranch -> return ()
    LatestOnBranch branch ->
      callProcess "git" ["-C", targetDir, "checkout", "refs/heads/" ++ MT.unpack branch]
    CommitHash hash ->
      callProcess "git" ["-C", targetDir, "checkout", "--detach", MT.unpack hash]
    ReleaseTag tag ->
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
