{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}


{-|
Module      : Module
Description : Morloc module imports and paths
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

All information about morloc module structure should be defined here.
 * define package YAML metadata
 * finding modules on the local filesystem
 * finding headers and shared libraries required by modules
 * installation of modules from github
-}
module Morloc.Module
  ( findModule
  , loadModuleMetadata
  , handleFlagsAndPaths
  -- * Module installation
  , OverwriteProtocol(..)
  , GitProtocol(..)
  , installModule
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

import Morloc.Namespace
import Morloc.Data.Doc
import qualified Data.Char as DC
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import Morloc.Data.Text (Text)
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified Data.Yaml.Config as YC

-- needed for github retrieval
import System.Directory
import Data.Aeson
import System.Process (callCommand)

data RepoInfo = RepoInfo { defaultBranch :: Text } deriving Show

instance FromJSON RepoInfo where
    parseJSON = withObject "RepoInfo" $ \v -> RepoInfo
        <$> v .: "default_branch"


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
    xs@(x:_) -> do
      MM.say $ "WARNING: module path shadowing, for" <+> pretty importModule
             <+> "from module" <+> pretty currentModule <+> "found paths:"
             <+> "\n  " <> pretty xs
             <> "\n  selecting path:" <+> pretty x
      return x
    [] ->
      MM.throwError . CannotLoadModule . render
        $  "Within module" <+> squotes (pretty currentModule) <> ","
        <+> "failed to import module" <+> squotes (pretty importModule) <> "\n"
        <> "The following paths were searched:\n"
        <+> indent 4 (vsep (map pretty allPaths))
        <> "\nPerhaps you need to install this module?"

-- | Give a module path (e.g. "/your/path/foo.loc") find the package metadata.
-- It currently only looks for a file named "package.yaml" in the same folder
-- as the main "*.loc" file.
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

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x:xs) (y:ys)
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
getModulePaths
    :: Path -- ^ morloc default library path (e.g., "~/.morloc/src/morloc")
    -> Path -- ^ morloc plane (e.g., "morloclib")
    -> (Maybe Path, MVar) -- ^ the path and name of the current module (e.g., `Just ("foo.loc", MVar "bar")`)
    -> MVar -- ^ the name of the module that is being imported
    -> [Path]
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
getModulePaths lib plane (Nothing, _) (splitModuleName -> namePath) = map MS.joinPath paths where

    -- either search the working directory for a life like "math.loc" or look
    -- for a folder named after the module with with a "main.loc" script
    localPaths = [
            init namePath <> [last namePath <> ".loc"]
          , namePath <> ["main.loc"]
        ]

    -- if nothing is local, look for system libraries. The system libraries MUST
    -- be in a folder named after the module. The main script may either have
    -- the name of the final import feild (e.g., "math" in the imports "math" or
    -- "alice.math") or by "main.loc"
    systemPaths = [
            lib : init namePath <> [last namePath <> ".loc"]
          , lib : namePath <> ["main.loc"]
        ]

    planePaths = [
            [lib, plane] <> init namePath <> [last namePath <> ".loc"]
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
        [] ->  map MS.joinPath [
                -- system paths
                  lib : init importName <> [last importName <> ".loc"]
                , lib : importName <> ["main.loc"]
                -- plane paths
                , [lib, plane] <> init importName <> [last importName <> ".loc"]
                , [lib, plane] <> importName <> ["main.loc"]
                -- local path
                , init importName <> [last importName <> ".loc"]
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
        _ -> let rootPath = if last modulePath == "main.loc"
                            -- e.g., `/../src/foo/bar/baz/main.loc` -> '/../src/'
                            then removePathSuffix moduleName (init modulePath)
                            -- e.g., `/../src/foo/bar/baz.loc`  -> '/../src/'
                            else removePathSuffix (init moduleName) (init modulePath)
             in map MS.joinPath [
                        rootPath <> importName <> ["main.loc"]
                    ,   rootPath <> init importName <> [last importName <> ".loc"]
                 ]


-- | An ordered list of where to search for C/C++ header files
getHeaderPaths
  :: Path      -- ^ the path the morloc home ("~/.morloc" be default)
  -> String   -- ^ the base header name without an extension
  -> [String] -- ^ a list of header options (e.g., ".h", ".hpp")
  -> [Path]    -- ^ an ordered list of paths to search (foo.h, foo.hpp, include/foo.h ...)
getHeaderPaths lib base exts = [path <> ext | path <- paths, ext <- exts]
  where
    paths = map MS.joinPath
            [ [base]
            , ["include", base]
            , [base, base]
            , [lib, "include", base]
            , [lib, "src", base, base]
            , ["/usr/include", base]
            , ["/usr/local/include", base]
            ]

-- | An ordered list of where to search for shared libraries
getLibraryPaths
  :: Path    -- ^ the path the morloc home ("~/.morloc" be default)
  -> String -- ^ the base source name, e.g., "SimplexNoise"
  -> String -- ^ the shared library name, e.g., "libsimplexnoise.so"
  -> [Path]  -- ^ an ordered list of paths to search
getLibraryPaths lib base sofile = map MS.joinPath
  [ [sofile]
  , ["lib", sofile]
  , [base, sofile]
  , [lib, "lib", sofile]
  , [lib, "src", base, sofile]
  , [lib, "src", base, "lib", sofile]
  ]

handleFlagsAndPaths :: Lang -> [Source] -> MorlocMonad ([Source], [Text], [Path])
handleFlagsAndPaths CppLang srcs = do
  state <- MM.get
  let gccversion = gccVersionFlag . foldl max 0 . map packageCppVersion $ statePackageMeta state
  let explicitLibs = map ("-l" <>) . unique . concatMap packageDependencies $ statePackageMeta state
  (srcs', libflags, paths) <-
      fmap unzip3
    . mapM flagAndPath
    . unique
    $ [s | s <- srcs, srcLang s == CppLang]

  -- include the morloc home directory include folder
  -- this should store the mlccpptypes library that defines Unit
  home <- MM.asks configHome
  let mlcInclude = ["-I" <> home <> "/include"]

  return (
         -- all sources that have a defined path (import something)
           filter (isJust . srcPath) srcs'
         -- compiler flags and shared libraries
         , [gccversion] <> explicitLibs ++ (map MT.pack . concat) (mlcInclude : libflags)

         -- paths to files to include
         , unique (catMaybes paths)
         )
handleFlagsAndPaths _ srcs = return (srcs, [], [])

gccVersionFlag :: Int -> Text
gccVersionFlag i
 | i <= 17 = "-std=c++17"
 | otherwise = "-std=c++" <> MT.show' i

flagAndPath :: Source -> MorlocMonad (Source, [String], Maybe Path)
flagAndPath src@(Source _ CppLang (Just p) _ _)
  = case (MS.takeDirectory p, MS.dropExtensions (MS.takeFileName p), MS.takeExtensions p) of
    -- lookup up "<base>.h" and "lib<base>.so"
    (".", base, "") -> do
      header <- lookupHeader base
      libFlags <- lookupLib base
      return (src {srcPath = Just header}, libFlags, Just (MS.takeDirectory header))
    -- use "<base>.h" and lookup "lib<base>.so"
    (dir, base, _) -> do
      libFlags <- lookupLib base
      return (src, libFlags, Just dir)
  where
    lookupHeader :: String -> MorlocMonad Path
    lookupHeader base = do
      home <- MM.asks configHome
      let allPaths = getHeaderPaths home base [".h", ".hpp", ".hxx"]
      existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
      case existingPaths of
        (x:_) -> return x
        [] -> MM.throwError . OtherError $ "Header file " <> MT.pack base <> ".* not found"


    lookupLib :: String -> MorlocMonad [String]
    lookupLib base = do
      home <- MM.asks configHome
      let libnamebase = filter DC.isAlphaNum (map DC.toLower base)
      let libname = "lib" <> libnamebase <> ".so"
      let allPaths = getLibraryPaths home base libname
      existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
      case existingPaths of
        (libpath:_) -> do
          libdir <- liftIO . MS.canonicalizePath . MS.takeDirectory $ libpath
          return
            [ "-Wl,-rpath=" <> libdir
            , "-L" <> libdir
            , "-l" <> libnamebase
            ]
        [] -> return []
flagAndPath src@(Source _ CppLang Nothing _ _) = return (src, [], Nothing)
flagAndPath _ = MM.throwError . OtherError $ "flagAndPath should only be called for C++ functions"


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
  = ModuleSourceLocal Text (Maybe GitSnapshotSelector)
  -- ^ Module in a local directory (may or may not be a git repo)
  | ModuleSourceRemoteGit GitRemote
  -- ^ A module stored in an arbitrary users github repo, e.g., (GithubRepo "weena" "math")
  deriving (Show, Eq, Ord)

-- }}}

installModule
  :: OverwriteProtocol
  -- ^ How should overwrites be handled
  -> GitProtocol
  -- ^ Remote Git download protocol (HTTPS by default)
  -> Path
  -- ^ Absolute path to folder where modules are installed for the given plane
  -> Path
  -- ^ Default github org for the given plane for pulling core modules
  -> Text
  -- ^ Installation string, such as "github:weena/math@version:0.1.0"
  -> MorlocMonad ()
installModule overwrite gitprot libpath coreorg modstr =
  case parse (moduleInstallParser (MT.pack coreorg)) "" modstr of
    (Left errstr) -> MM.throwError . ModuleInstallError . MT.pack . errorBundlePretty $ errstr
    (Right (Left errstr)) -> MM.throwError . ModuleInstallError $ errstr
    (Right (Right (ModuleSourceLocal path selector))) -> installLocal overwrite libpath selector path
    (Right (Right (ModuleSourceRemoteGit remote))) ->
      let targetDir = libpath </> MT.unpack (gitReponame remote)
      in installRemote overwrite gitprot targetDir remote

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
    makeModuleSource :: Maybe RemoteSource -> ModulePath -> Maybe GitSnapshotSelector -> Either Text ModuleSource
    makeModuleSource mayRemote (ModulePathCore modname) selector
      | mayRemote == Just RemoteGithub || mayRemote == Nothing
          = return . ModuleSourceRemoteGit $ GitRemote
              { gitRemoteSource = RemoteGithub
              , gitReference = fromMaybe LatestDefaultBranch selector
              , gitUsername = coreorg
              , gitReponame = modname
              }
      | otherwise = Left "Core modules are only imported from github"
    makeModuleSource (Just _) (ModulePathLocal _) _ 
      = Left "Invalid mix of local and remote import names"
    makeModuleSource (maybe RemoteGithub id -> remote) (ModulePathGit user repo) selector
      = return . ModuleSourceRemoteGit $ GitRemote
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
  remote <- try (string "github" >> return RemoteGithub)
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
parseModname Nothing
  =   parseCoreModule  -- must start with letter
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
    else return (MT.pack (firstChar:rest))

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
  form <-  try (string "hash" >> return RefHash)
       <|> try (string "branch" >> return RefBranch)
       <|> try (string "version" >> return RefVersion)
       <|> try (string "tag" >> return RefVersion) -- same diff
  char ':'
  return form

-- codeberg:weena/calendar@version:1.0.0
--                                 -----
parseRefStr :: Maybe RefForm -> Parser GitSnapshotSelector
parseRefStr Nothing
  =   try parseHash
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
      return $ mconcat
        [ fromMaybe "" v
        , major
        , "."
        , minor
        , maybe "" ("." <>) patchMay
        , maybe "" ("-" <>) preRelease
        , maybe "" ("+" <>) buildMeta
        ]

-- | Parse a legal git branch name according to git-check-ref-format
--
-- Git reference naming rules (from git-check-ref-format man page):
-- 1. Cannot begin or end with slash '/'
-- 2. Cannot contain two consecutive dots '..'
-- 3. Cannot contain ASCII control characters (< 0x20), space, ~, ^, :, ?, *, [
-- 4. Cannot end with '.lock'
-- 5. Cannot end with a dot '.'
-- 6. Cannot contain a backslash '\'
-- 7. Cannot contain '@{' sequence (reflog syntax)
-- 8. Cannot be a single '@' character
-- 9. Components between slashes cannot begin with a dot '.'
-- 10. Cannot contain multiple consecutive slashes '//'
--
-- Current implementation deviations from spec:
-- - Missing: check for '@{' sequence (reflog syntax - rule 7)
-- - Missing: check for '@' as sole character (rule 8)
-- - Missing: check that components don't start with '.' (rule 9)
-- - Missing: check for consecutive slashes '//' (rule 10)
-- - Missing: check for ASCII control characters (< 0x20)
-- - Incomplete: allows '@' freely, but spec restricts it in certain contexts
--
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
      c > '\x1F'  -- No ASCII control characters (0x00-0x1F)
      && c /= ' '
      && c /= '~'
      && c /= '^'
      && c /= ':'
      && c /= '?'
      && c /= '*'
      && c /= '['
      && c /= '\\'
      && c /= '\DEL'  -- Also exclude DEL (0x7F)

    -- Comprehensive validation according to git-check-ref-format
    isValidBranchName branchName =
      not (MT.null branchName)                           -- Must have content
      && branchName /= "@"                              -- Cannot be just '@'
      && not (MT.isPrefixOf "/" branchName)              -- Cannot start with /
      && not (MT.isSuffixOf "/" branchName)              -- Cannot end with /
      && not (MT.isSuffixOf "." branchName)              -- Cannot end with .
      && not (MT.isSuffixOf ".lock" branchName)          -- Cannot end with .lock
      && not (".." `MT.isInfixOf` branchName)            -- No consecutive dots
      && not ("@{" `MT.isInfixOf` branchName)            -- No reflog syntax
      && not ("//" `MT.isInfixOf` branchName)            -- No consecutive slashes
      && not (hasComponentStartingWithDot branchName)   -- Components can't start with .
      && not (MT.any isInvalidChar branchName)           -- Final safety check

    -- Check if any path component starts with a dot
    hasComponentStartingWithDot txt =
      let components = MT.splitOn "/" txt
      in any (MT.isPrefixOf ".") components

    -- Characters that should never appear (belt-and-suspenders check)
    isInvalidChar c =
      c <= '\x1F'    -- Control characters
      || c == '\DEL'
      || c `elem` (" ~^:?*[\\" :: String)

-- }}}

-- {{{ install from module source

installLocal
  :: OverwriteProtocol
  -- ^ whether to overwrite the existing module in libpath if it existss
  -> Path
  -- ^ path to the morloc module installation folder
  -> Maybe GitSnapshotSelector
  -- ^ if this is a local git repo, what version should be installed?
  -> Text
  -- ^ path to the module to be installed
  -> MorlocMonad ()
installLocal overwrite libpath maySelector modulePath = do
  sourceDir <- liftIO $ MS.makeAbsolute (MT.unpack modulePath)

  -- Extract module name from path (last component)
  let moduleName = MS.takeFileName sourceDir
  let targetDir = libpath </> moduleName

  -- Check if source exists
  sourceExists <- liftIO $ doesDirectoryExist sourceDir
  unless sourceExists $
    MM.throwError . ModuleInstallError . render $ "Source directory does not exist: " <> pretty sourceDir

  -- Check if target already exists
  targetExists <- liftIO $ doesDirectoryExist targetDir

  case (targetExists, overwrite) of
    (True, DoNotOverwrite) ->
      MM.throwError . ModuleInstallError . render $ "Module already exists at" <+> pretty targetDir <+> "and overwrite is disabled"
    (True, ForceOverwrite) -> liftIO $ removeDirectoryRecursive targetDir
    (False, _) ->
      -- do nothing, all is well
      return ()

  -- Check if source is a git repo
  let gitDir = sourceDir </> ".git"
  isGitRepo <- liftIO $ doesDirectoryExist gitDir

  if isGitRepo
    then installLocalGitRepo sourceDir targetDir (fromMaybe LatestDefaultBranch maySelector)
    else installLocalDirectory sourceDir targetDir

-- | Install from a local git repository
installLocalGitRepo :: FilePath -> FilePath -> GitSnapshotSelector -> MorlocMonad ()
installLocalGitRepo sourceDir targetDir selector = do
  case selector of
    -- If not ref is given, treat the repo like a folder
    LatestDefaultBranch -> cpDir sourceDir targetDir
    -- Otherwise, clone the repository, ignoring un-committed changes
    _ -> cloneRepo sourceDir targetDir

  -- Checkout the appropriate ref
  case selector of
    LatestDefaultBranch -> do
      MM.sayVVV "Using default branch, exactly mirror parent code with local changes"

    LatestOnBranch branch -> do
      MM.sayVVV $ "Checking out branch:" <+> pretty branch
      liftIO $ callCommand $ "cd " ++ targetDir ++ " && git checkout refs/heads/" ++ MT.unpack branch

    CommitHash hash -> do
      MM.sayVVV $ "Checking out commit:" <+> pretty hash
      liftIO $ callCommand $ "cd " ++ targetDir ++ " && git checkout --detach " ++ MT.unpack hash

    ReleaseTag tag -> do
      MM.sayVVV $ "Checking out tag:" <+> pretty tag
      liftIO $ callCommand $ "cd " ++ targetDir ++ " && git checkout refs/tags/" ++ MT.unpack tag

  MM.sayVVV $ "Removing history in the installed folder"
  liftIO $ callCommand $ "cd " ++ targetDir ++ " && rm -rf .git/"

cpDir :: FilePath -> FilePath -> MorlocMonad ()
cpDir fromDir toDir = do
  let cmd = "cp -r " <> fromDir <> toDir
  MM.sayVVV $ pretty cmd
  liftIO $ callCommand cmd

cloneRepo :: String -> FilePath -> MorlocMonad ()
cloneRepo repoURL targetDir = do
  let cmd = "git clone -q " <> repoURL <> " " <> targetDir
  MM.sayVVV $ pretty cmd
  liftIO $ callCommand cmd

-- | Install from a local directory (non-git)
installLocalDirectory :: FilePath -> FilePath -> MorlocMonad ()
installLocalDirectory sourceDir targetDir = do
  MM.sayVVV $ "Copying directory from" <+> pretty sourceDir <+> "to" <+> pretty targetDir
  liftIO . callCommand $ "cp -r " ++ sourceDir ++ " " ++ targetDir



-- TODO: Download directly from the archive with hashes, this will be MUCH faster
installRemote
  :: OverwriteProtocol
  -- ^ whether to overwrite existing modules
  -> GitProtocol
  -- ^ whether to use SSH or HTTPS protocols for cloning remote git repos
  -> Path
  -- ^ path to the morloc module that will be installed
  -> GitRemote
  -- ^ git repo info
  -> MorlocMonad ()
installRemote overwrite gitprot targetDir remote = do

  -- Check if target already exists
  targetExists <- liftIO $ doesDirectoryExist targetDir

  case (targetExists, overwrite) of
    (True, DoNotOverwrite) ->
      error $ "Module already exists at " ++ targetDir ++ " and overwrite is disabled"
    (True, ForceOverwrite) -> do
      MM.sayVVV $ "Removing existing module at" <+> pretty targetDir
      liftIO $ removeDirectoryRecursive targetDir
    (False, _) ->
      return ()

  -- Build the git URL
  let gitUrl = buildGitUrl gitprot remote

  -- Clone the repository
  cloneRepo gitUrl targetDir

  -- Checkout the appropriate ref
  checkoutRef targetDir (gitReference remote)

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

-- | Checkout a specific git reference
checkoutRef :: FilePath -> GitSnapshotSelector -> MorlocMonad ()
checkoutRef targetDir selector = case selector of
  LatestDefaultBranch ->
    MM.sayVVV "Using default branch"

  LatestOnBranch branch -> do
    MM.sayVVV $ "Checking out branch:" <+> pretty branch
    liftIO . callCommand $ "cd " ++ targetDir ++ " && git checkout " ++ MT.unpack branch
    liftIO . callCommand $ "cd " ++ targetDir ++ " && git pull"

  CommitHash hash -> do
    MM.sayVVV $ "Checking out commit:" <+> pretty hash
    liftIO . callCommand $ "cd " ++ targetDir ++ " && git checkout " ++ MT.unpack hash

  ReleaseTag tag -> do
    MM.sayVVV $ "Checking out tag:" <+> pretty tag
    liftIO . callCommand $ "cd " ++ targetDir ++ " && git checkout tags/" ++ MT.unpack tag

-- }}}
