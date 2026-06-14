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

    -- * Exposed-resource cleanup (for symmetric uninstall)
  , wipeExposed

    -- * Dependency-pin resolver (exported for testing)
  , PinEntry (..)
  , PinMap
  , addPin
  , hashEq
  , reconcileOverwrite
  , readInstalledHash
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
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (stderr)
import System.Process
  ( callProcess
  , createProcess
  , proc
  , readProcess
  , waitForProcess
  , CreateProcess(..)
  , StdStream(..)
  )

data InstallReason = ExplicitInstall | AutoDependency
  deriving (Show, Eq)

-- | A pinned dependency: git commit hash, the depth at which it was declared
-- (smaller = closer to the install root), and the package that declared it
-- (used in conflict diagnostics).
data PinEntry = PinEntry
  { pinHash :: !Text
  , pinDepth :: !Int
  , pinDeclaredBy :: !Text
  } deriving (Show, Eq, Ord)

-- | Resolver state: module name -> pinned entry. Built up during the
-- recursive install walk per the closer-to-install-root-wins rule.
type PinMap = Map.Map Text PinEntry

-- | Case-insensitive hash equality. Git is mid-transition between SHA-1
-- (40 hex chars) and SHA-256 (64 hex chars); compare lowercased so
-- copy-paste from different tools doesn't spuriously conflict.
hashEq :: Text -> Text -> Bool
hashEq a b = MT.toLower a == MT.toLower b

-- | Add a pin to the map per closer-wins. Equal-depth disagreement throws.
addPin :: PinMap -> Int -> Text -> Text -> Text -> MorlocMonad PinMap
addPin pinMap depth declaredBy depName depHash =
  case Map.lookup depName pinMap of
    Nothing ->
      return $ Map.insert depName (PinEntry depHash depth declaredBy) pinMap
    Just existing
      | pinDepth existing < depth ->
          -- existing entry is closer to root, keep it
          return pinMap
      | pinDepth existing == depth ->
          if hashEq (pinHash existing) depHash
            then return pinMap
            else MM.throwSystemError $
                   "Conflicting pins for module" <+> squotes (pretty depName)
                   <+> "at depth" <+> pretty depth <> ":"
                   <+> "hash" <+> squotes (pretty (pinHash existing))
                   <+> "from" <+> squotes (pretty (pinDeclaredBy existing))
                   <+> "vs hash" <+> squotes (pretty depHash)
                   <+> "from" <+> squotes (pretty declaredBy)
      | otherwise ->
          -- existing is deeper, we're closer, replace
          return $ Map.insert depName (PinEntry depHash depth declaredBy) pinMap

-- | Read the @installed_hash@ field from a module's fdb manifest.
-- Returns @Nothing@ if the manifest is absent, malformed, or has no hash
-- (e.g. legacy installs predating this schema bump).
readInstalledHash :: FilePath -> Text -> IO (Maybe Text)
readInstalledHash fdbDir modName = do
  let manifestPath = fdbDir </> MT.unpack modName ++ ".module"
  exists <- doesFileExist manifestPath
  if not exists
    then return Nothing
    else do
      bs <- BL.readFile manifestPath
      case Aeson.decode bs :: Maybe Aeson.Value of
        Just (Aeson.Object obj) ->
          case KM.lookup "installed_hash" obj of
            Just (Aeson.String s) -> return (Just s)
            _ -> return Nothing
        _ -> return Nothing

-- | Decide whether reconciliation forces an overwrite. If the cache
-- contains a different hash than the resolver expects, upgrade
-- @DoNotOverwrite@ to @ForceOverwrite@. Explicit @ForceOverwrite@ stands.
reconcileOverwrite ::
  OverwriteProtocol -> Maybe Text -> Maybe Text -> OverwriteProtocol
reconcileOverwrite ForceOverwrite _ _ = ForceOverwrite
reconcileOverwrite DoNotOverwrite (Just expected) (Just actual)
  | not (hashEq expected actual) = ForceOverwrite
reconcileOverwrite DoNotOverwrite (Just _) Nothing = ForceOverwrite
reconcileOverwrite ow _ _ = ow

moduleInstallError :: MDoc -> MorlocMonad a
moduleInstallError msg = MM.throwSystemError $ "Failed to install module:" <+> msg

-- | Recursively remove the install target if it exists. Used both as the
-- onException handler on the initial fetch IO and to roll back when a
-- later validation step fails.
runCleanup :: FilePath -> IO ()
runCleanup targetDir = do
  exists <- doesDirectoryExist targetDir
  when exists $ removeDirectoryRecursive targetDir

-- | Spawn the module's setup script. Working directory is the module's
-- installed location, stdout and stderr stream to the morloc process's
-- stderr so the user sees pip/apt-get/etc. output live. The morloc
-- environment is exposed via MORLOC_* variables. Non-zero exit raises
-- an IOException; the caller is responsible for cleanup-on-exception.
runSetupScript ::
     Config.Config
  -> Text       -- ^ module name
  -> PackageMeta
  -> FilePath   -- ^ libpath (the plane directory containing targetDir)
  -> FilePath   -- ^ targetDir (the module's installed dir)
  -> FilePath   -- ^ resolved absolute path to the setup script
  -> IO ()
runSetupScript config name meta libpath targetDir setupPath = do
  baseEnv <- getEnvironment
  let dirs = exposeDirsFor config name
      extraEnv =
        [ ("MORLOC_HOME",            Config.configHome config)
        , ("MORLOC_MODULE_NAME",     MT.unpack name)
        , ("MORLOC_MODULE_VERSION",  MT.unpack (packageVersion meta))
        , ("MORLOC_MODULE_DIR",      targetDir)
        , ("MORLOC_PLANE",           Config.configPlane config)
        , ("MORLOC_PLANE_DIR",       libpath)
        , ("MORLOC_EXPOSE_CPP_DIR",  exposeCppDir dirs)
        , ("MORLOC_EXPOSE_PY_DIR",   exposePyDir dirs)
        , ("MORLOC_EXPOSE_R_DIR",    exposeRDir dirs)
        ]
      -- Later entries shadow earlier ones, so the MORLOC_* additions win
      -- over any stray values in the inherited environment.
      mergedEnv = baseEnv ++ extraEnv
      cp = (proc "bash" [setupPath])
        { std_out = UseHandle stderr
        , cwd     = Just targetDir
        , env     = Just mergedEnv
        }
  (_, _, _, ph) <- createProcess cp
  code <- waitForProcess ph
  case code of
    ExitSuccess -> return ()
    ExitFailure n ->
      ioError . userError
        $ "setup script for module '"
        <> MT.unpack name
        <> "' failed with exit code "
        <> show n

-- | Per-module exposed-resource destinations under $MORLOC_HOME. Python's
-- destination uses a hyphen-to-underscore transform so the module name
-- becomes a legal Python identifier (e.g. `tensor-cpp` -> `tensor_cpp`).
data ExposeDirs = ExposeDirs
  { exposeCppDir :: !FilePath
  , exposePyDir  :: !FilePath
  , exposeRDir   :: !FilePath
  }

exposeDirsFor :: Config.Config -> Text -> ExposeDirs
exposeDirsFor config name =
  let home = Config.configHome config
      modName = MT.unpack name
  in ExposeDirs
       { exposeCppDir = home </> "include" </> modName
       , exposePyDir  = home </> "lib" </> "python" </> pythonizeName modName
       , exposeRDir   = home </> "lib" </> "R" </> modName
       }

-- | Convert a module name to a Python-legal identifier: hyphens become
-- underscores. Matches the documented downstream consumer pattern
-- (e.g. `import tensor_cpp.helpers`).
pythonizeName :: String -> String
pythonizeName = map (\c -> if c == '-' then '_' else c)

-- | Wipe all per-module exposed-resource subdirectories for a module.
-- Symmetric counterpart to the copy step in 'runExpose': used both at
-- install time (pre-copy, so stale files don't linger across reinstalls)
-- and at uninstall time (so removed modules don't leave header / Python /
-- R droppings on the system).
wipeExposed :: Config.Config -> Text -> IO ()
wipeExposed config name = do
  let dirs = exposeDirsFor config name
  mapM_ wipeIfExists [exposeCppDir dirs, exposePyDir dirs, exposeRDir dirs]
  where
    wipeIfExists d = do
      e <- doesDirectoryExist d
      when e $ removeDirectoryRecursive d

-- | Copy files declared in `expose` into per-language well-known
-- directories under $MORLOC_HOME, namespaced by module. Pre-wipes any
-- prior per-module exposure so stale files from earlier installs do not
-- linger. Patterns are routed through 'Install.resolveIncludePatterns',
-- so glob syntax matches `packageInclude`.
runExpose ::
     Config.Config
  -> Text          -- ^ module name
  -> ExposeSet
  -> FilePath      -- ^ module's installed dir (source of the copy)
  -> IO ()
runExpose config name es targetDir = do
  let dirs = exposeDirsFor config name
      modName = MT.unpack name
  wipeExposed config name
  unless (null (exposeCpp es)) $
    copyExposed targetDir (exposeCppDir dirs) modName "cpp" (exposeCpp es)
  unless (null (exposePy es)) $
    copyExposed targetDir (exposePyDir dirs) modName "py" (exposePy es)
  unless (null (exposeR es)) $
    copyExposed targetDir (exposeRDir dirs) modName "r" (exposeR es)
  where
    -- Resolve each pattern individually so a literal-no-match can be
    -- distinguished from a glob-no-match. Literal misses are typos and
    -- should fail loudly; glob misses are silent (matches packageInclude).
    copyExposed src dst modN lang pats = do
      createDirectoryIfMissing True dst
      forM_ pats $ \pat -> do
        matched <- Install.resolveIncludePatterns src [pat]
        let pStr = MT.unpack pat
            isGlob = '*' `elem` pStr || "/" `MT.isSuffixOf` pat
        case matched of
          [] | not isGlob ->
            ioError . userError $
              "expose." <> lang <> " in module '" <> modN
              <> "': listed path '" <> pStr <> "' matched no files"
          _ -> forM_ matched $ \srcAbs -> do
            let rel = MS.makeRelative src srcAbs
                dstAbs = dst </> rel
            createDirectoryIfMissing True (MS.takeDirectory dstAbs)
            copyFile srcAbs dstAbs

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
  allowLocal <- MM.gets stateAllowLocalModules
  if not allowLocal
    then MM.throwSystemError $
      "Within module" <+> squotes (pretty currentModule)
        <> "," <+> "local import" <+> squotes (pretty importModule)
        <+> "is not permitted in eval mode."
        <+> "Use 'morloc make' to build with local modules."
    else return ()
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
  allowLocal <- MM.gets stateAllowLocalModules
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
        | not allowLocal = []  -- eval mode: installed modules only
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
  case packageInclude meta of
    Just pats -> liftIO $ Install.validateIncludeScope pats
    Nothing -> return ()
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
  -- | Resolver state: pinned module versions discovered so far
  PinMap ->
  -- | Depth of this install in the dependency walk (0 = install root)
  Int ->
  -- | Why this module is being installed
  InstallReason ->
  -- | Installation string, such as "github:weena/math@version:0.1.0"
  Text ->
  MorlocMonad ()
installModule overwrite gitprot libpath coreorg mayTypecheck userSources inProgress pinMap depth reason modstr = do
  config <- MM.ask
  let registry = Config.configRegistry config
      fdbDir = Config.configHome config </> "fdb"
  -- Try registry first for bare names when a registry is configured
  case (registry, tryParseRegistryModule (MT.pack coreorg) modstr) of
    (Just _, Just (owner, name)) -> do
      let targetDir = libpath </> MT.unpack owner </> MT.unpack name
      reconcileAndDispatch fdbDir (ModuleSourceRegistry owner name) name targetDir
    _ -> installModuleClassic fdbDir
  where
    installModuleClassic fdbDir = case parse (moduleInstallParser (MT.pack coreorg)) "" modstr of
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
        reconcileAndDispatch fdbDir source name targetDir

    -- Shared reconciliation/dispatch logic. Reads any recorded
    -- installed_hash from fdb, compares to the expected hash from the
    -- resolver state, and upgrades DoNotOverwrite to ForceOverwrite when
    -- the cache disagrees with the pin.
    reconcileAndDispatch fdbDir source name targetDir =
      if Set.member name inProgress
        then return ()
        else do
          installedHash <- liftIO $ readInstalledHash fdbDir name
          let expectedHash = pinHash <$> Map.lookup name pinMap
              effectiveOverwrite = reconcileOverwrite overwrite expectedHash installedHash
          targetExists <- liftIO $ doesDirectoryExist targetDir
          case (targetExists, effectiveOverwrite) of
            (True, DoNotOverwrite) -> do
              case reason of
                ExplicitInstall ->
                  MM.say $ "Module" <+> pretty name <+> "is already installed, use --force to reinstall"
                AutoDependency ->
                  MM.sayVVV $ "Module" <+> pretty name <+> "already installed, skipping"
              return ()
            (True, ForceOverwrite) -> do
              case (expectedHash, installedHash) of
                (Just e, Just a) | not (hashEq e a) ->
                  MM.say $ "Reinstalling" <+> pretty name
                        <+> "to reconcile cache (have" <+> pretty a
                        <> "; pin requires" <+> pretty e <> ")"
                (Just e, Nothing) ->
                  MM.say $ "Reinstalling" <+> pretty name
                        <+> "to record pinned hash" <+> pretty e
                _ -> return ()
              liftIO $ removeDirectoryRecursive targetDir
              doInstall fdbDir source name targetDir
            (False, _) ->
              doInstall fdbDir source name targetDir

    doInstall :: FilePath -> ModuleSource -> Text -> FilePath -> MorlocMonad ()
    doInstall fdbDir source name targetDir = do
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
      liftIO $ ioAction `onException` runCleanup targetDir

      -- Enforce: the install-target directory name must equal the
      -- named `module <name>` declaration in the entry-point .loc file
      -- (main.loc, or the unique .loc file). Otherwise downstream
      -- `import <name>` will look in the wrong place. Runs before
      -- package.yaml is parsed so a mismatched install is rejected as
      -- early as possible, leaving runCleanup as the sole rollback.
      liftIO $ validateModuleNameMatchesDir targetDir
        `onException` runCleanup targetDir

      -- Read package.yaml for metadata and dependencies
      meta <- liftIO $ do
        let pkgYaml = targetDir </> "package.yaml"
        exists <- doesFileExist pkgYaml
        if exists
          then YC.loadYamlSettings [pkgYaml] [] YC.ignoreEnv
          else return defaultValue

      -- Fold this package's morloc-dependencies into the resolver state.
      -- Closer-wins: pins declared by this package (at depth `depth`) are
      -- shadowed by any already-present pin at a smaller depth, supersede
      -- any pin at a larger depth, and conflict with another pin at the
      -- same depth that names a different hash.
      pinMap' <- foldM
        (\pm (depName, depHash) -> addPin pm depth name depName depHash)
        pinMap
        (packageMorlocDependencies meta)

      -- Determine morloc dependencies by scanning .loc imports
      morlocDeps <- do
        mainFile <- liftIO $ findMainLocFile targetDir (MT.unpack name)
        case mainFile of
          Nothing -> return []
          Just f -> liftIO $ extractMorlocDeps f

      -- Stale-entry warnings: pins declared by THIS package whose name
      -- does not appear among the auto-discovered imports.
      let importedSet = Set.fromList morlocDeps
          declaredHere = packageMorlocDependencies meta
      forM_ declaredHere $ \(dn, _) ->
        unless (Set.member dn importedSet) $
          MM.say $ "warning: declared dependency" <+> squotes (pretty dn)
                <+> "is not imported anywhere in" <+> squotes (pretty name)
                <+> "(possibly stale)"

      -- Recursively install dependencies, threading the updated resolver
      -- state at depth+1.
      forM_ morlocDeps $ \dep -> do
        unless (Set.member dep inProgress') $ do
          let resolved = Map.lookup dep pinMap'
              resolvedHash = pinHash <$> resolved
              userOverride = Map.lookup dep userSources
              -- If the user supplied an explicit install string for this
              -- dep (via the CLI), honor it as-is. Otherwise build the
              -- modstr from the bare name plus an @hash:HASH suffix when
              -- pinned. The existing parser interprets the suffix as a
              -- CommitHash selector for both local and remote sources.
              depModstr = case userOverride of
                Just s -> s
                Nothing -> case resolvedHash of
                  Nothing -> dep
                  Just h  -> dep <> "@hash:" <> h
          when (resolved == Nothing) $
            -- Per-dep noise: only at verbose -v. Stale-entry warnings (a
            -- declared pin with no matching import) stay at default level
            -- because those are likely user errors, not the common case.
            MM.sayV $ "warning: dependency" <+> squotes (pretty dep)
                   <+> "is not pinned in any morloc-dependencies entry along the install path"
          MM.sayVVV $ "Auto-installing dependency:" <+> pretty depModstr
          installModule
            DoNotOverwrite
            gitprot
            libpath
            coreorg
            mayTypecheck
            userSources
            inProgress'
            pinMap'
            (depth + 1)
            AutoDependency
            depModstr

      -- Copy `expose`d files to per-language well-known dirs under
      -- $MORLOC_HOME, namespaced by module. Runs before the setup script
      -- so the script can rely on the exposed paths existing.
      do
        let es = packageExpose meta
            allPats = exposeCpp es ++ exposePy es ++ exposeR es
        liftIO $ Install.validateIncludeScope allPats
          `onException` runCleanup targetDir
        liftIO $ runExpose config' name es targetDir
          `onException` runCleanup targetDir

      -- Run the optional setup script, if one is declared in package.yaml.
      -- Runs after morloc deps are on disk (a setup script may rely on
      -- transitive resources) but before typecheck (so a script that
      -- builds C++/Python/R artifacts has a chance to lay them down).
      case packageSetup meta of
        Nothing -> return ()
        Just relPath -> do
          when (MS.isAbsolute relPath) $ do
            liftIO $ runCleanup targetDir
            moduleInstallError
              $ "setup path must be relative to the module directory, got"
              <+> squotes (pretty relPath)
          when (".." `elem` MS.splitDirectories relPath) $ do
            liftIO $ runCleanup targetDir
            moduleInstallError
              $ "setup path may not contain '..' segments, got"
              <+> squotes (pretty relPath)
          let setupPath = targetDir </> relPath
          setupExists <- liftIO $ doesFileExist setupPath
          unless setupExists $ do
            liftIO $ runCleanup targetDir
            moduleInstallError
              $ "setup script not found at" <+> squotes (pretty setupPath)
          MM.say $ "Running setup script for" <+> squotes (pretty name)
                <> ":" <+> pretty relPath
          liftIO $ runSetupScript config' name meta libpath targetDir setupPath
            `onException` runCleanup targetDir

      -- Typecheck the module (if callback provided)
      exports <- case mayTypecheck of
        Nothing -> return []
        Just typecheckFn -> do
          mainFile <- liftIO $ findMainLocFile targetDir (MT.unpack name)
          case mainFile of
            Nothing -> return []
            Just f -> typecheckFn f

      -- Write module manifest to fdb/
      liftIO $ createDirectoryIfMissing True fdbDir
      installTime <- liftIO $ floor <$> Time.getPOSIXTime
      let manifestPath = fdbDir </> MT.unpack name ++ ".module"
          -- For each auto-discovered dep, record the resolved hash (if any)
          morlocDepsWithHash =
            [ (dep, pinHash <$> Map.lookup dep pinMap') | dep <- morlocDeps ]
          installedSelfHash = pinHash <$> Map.lookup name pinMap
          manifestJson =
            buildModuleManifest
              meta
              name
              morlocDepsWithHash
              installedSelfHash
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

-- | Build a module manifest JSON string. @morlocDeps@ now carries each
-- dep's resolved hash alongside its name (Nothing = installed at latest);
-- @installedSelfHash@ is the hash this module itself was installed at, as
-- determined by the resolver (Nothing = installed at latest).
buildModuleManifest ::
  PackageMeta ->
  Text ->
  [(Text, Maybe Text)] ->
  Maybe Text ->
  [(Text, Text)] ->
  FilePath ->
  Text ->
  InstallReason ->
  Int ->
  Text
buildModuleManifest meta name morlocDeps installedSelfHash exports installPath installSource reason installTime =
  jsonObj
    [ ("kind", jsonStr "module")
    , ("name", jsonStr name)
    , ("version", jsonStr (packageVersion meta))
    , ("synopsis", jsonStr (packageSynopsis meta))
    , ("author", jsonStr (packageAuthor meta))
    , ("license", jsonStr (packageLicense meta))
    , ("homepage", jsonStr (packageHomepage meta))
    , ("c_dependencies", jsonStrArr (packageDependencies meta))
    ,
      ( "morloc_dependencies"
      , jsonArr
          [ jsonObj $
              [("name", jsonStr d)] ++
              maybe [] (\h -> [("git_hash", jsonStr h)]) mh
          | (d, mh) <- morlocDeps
          ]
      )
    , ("installed_hash", maybe jsonNull jsonStr installedSelfHash)
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
      imports = concatMap extractImport (stripLocComments ls)
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

-- | Line-based comment stripper for .loc files. Drops `-- to EOL` and
-- `{- ... -}` block-comment lines. Lifted out of extractMorlocDeps so
-- the module-header scanner can reuse it.
stripLocComments :: [Text] -> [Text]
stripLocComments = go False
  where
    go _ [] = []
    go True (l : ls)
      | MT.isInfixOf "-}" l = go False ls
      | otherwise = go True ls
    go False (l : ls)
      | MT.isPrefixOf "--" (MT.stripStart l) = go False ls
      | MT.isPrefixOf "{-" (MT.stripStart l) = go True ls
      | otherwise = l : go False ls

-- | Scan a .loc file for every named `module <name> (...)` declaration.
-- Anonymous declarations (`module ( ... )`) are intentionally skipped --
-- they mark auxiliary files used via relative `import .foo` and are not
-- top-level installable modules. The scan is a lightweight line-based
-- pass (matching extractMorlocDeps's precedent); it is not a full
-- parser, so a multi-line `module` declaration would be missed --
-- which is fine, because such files are not idiomatic.
extractNamedModuleDecls :: FilePath -> IO [Text]
extractNamedModuleDecls path = do
  content <- TIO.readFile path
  let ls = MT.lines content
  return $ mapMaybe extractName (stripLocComments ls)
  where
    extractName :: Text -> Maybe Text
    extractName ln =
      let stripped = MT.stripStart ln
       in case MT.stripPrefix "module" stripped of
            Nothing -> Nothing
            Just rest ->
              -- The `module` token must be followed by whitespace,
              -- else it is the prefix of some longer identifier.
              case MT.uncons rest of
                Just (c, after) | DC.isSpace c ->
                  let nameStart = MT.stripStart after
                   in if MT.isPrefixOf "(" nameStart
                        then Nothing  -- anonymous: no name to harvest
                        else
                          let name = MT.takeWhile isModNameChar nameStart
                           in if MT.null name then Nothing else Just name
                _ -> Nothing
    isModNameChar :: Char -> Bool
    isModNameChar c = DC.isAlphaNum c || c == '-'

-- | Find the canonical entry-point .loc file in a module directory.
-- Either `main.loc` (preferred), or the unique .loc file in the dir.
-- Multiple .loc files without a main.loc is ambiguous and rejected.
findEntryPointLocFile :: FilePath -> IO (Either Text FilePath)
findEntryPointLocFile dir = do
  let mainLoc = dir </> "main.loc"
  mainExists <- doesFileExist mainLoc
  if mainExists
    then return (Right mainLoc)
    else do
      dirExists <- doesDirectoryExist dir
      if not dirExists
        then return (Left $ "directory not found: " <> MT.pack dir)
        else do
          entries <- listDirectory dir
          let locCandidates = [ dir </> e | e <- entries, MS.takeExtension e == ".loc" ]
          locFiles <- filterM doesFileExist locCandidates
          case locFiles of
            [single] -> return (Right single)
            []       -> return (Left $ "no .loc file found in " <> MT.pack dir)
            many     -> return . Left $
              "multiple .loc files in " <> MT.pack dir <>
              " and no main.loc to disambiguate: " <>
              MT.intercalate ", " (map (MT.pack . MS.takeFileName) many)

-- | Verify the installed module's directory basename matches the
-- declared module name in its entry-point .loc file. Throws IOError on
-- mismatch / missing entry / no-named-module / multiple-named-modules.
-- Caller is responsible for cleanup-on-throw (use within onException).
validateModuleNameMatchesDir :: FilePath -> IO ()
validateModuleNameMatchesDir targetDir = do
  entryResult <- findEntryPointLocFile targetDir
  entryPath <- case entryResult of
    Right p   -> return p
    Left err  -> ioError . userError $ MT.unpack err
  declared <- extractNamedModuleDecls entryPath
  let dirName   = MT.pack (MS.takeFileName (MS.dropTrailingPathSeparator targetDir))
      entryFile = MT.pack (MS.takeFileName entryPath)
  case declared of
    [name]
      | name == dirName -> return ()
      | otherwise ->
          ioError . userError $
            "declared module name '" <> MT.unpack name <>
            "' in " <> MT.unpack entryFile <>
            " does not match the install directory name '" <>
            MT.unpack dirName <>
            "'. Either rename the directory to match the module, or " <>
            "edit the `module` header so the names agree."
    [] ->
      ioError . userError $
        "no named morloc module found in " <> MT.unpack entryFile <>
        ": all declarations are anonymous. Add a name to the top-level " <>
        "`module` header (e.g. `module " <> MT.unpack dirName <> " (...)`)."
    multiple ->
      ioError . userError $
        "multiple named morloc modules declared in " <> MT.unpack entryFile <>
        " (" <> MT.unpack (MT.intercalate ", " multiple) <>
        "). Exactly one named top-level module is required per file."

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
    else Install.copyAllFiltered sourceDir targetDir

installLocalGitRepoIO :: FilePath -> FilePath -> GitSnapshotSelector -> IO ()
installLocalGitRepoIO sourceDir targetDir selector = do
  case selector of
    LatestDefaultBranch ->
      -- Copy the working tree, filtering out .git and ignored files
      Install.copyAllFiltered sourceDir targetDir
    LatestOnBranch branch -> do
      callProcess "git" ["clone", "-q", sourceDir, targetDir]
      callProcess "git" ["-C", targetDir, "checkout", "refs/heads/" ++ MT.unpack branch]
    CommitHash hash -> do
      callProcess "git" ["clone", "-q", sourceDir, targetDir]
      callProcess "git" ["-C", targetDir, "checkout", "--detach", MT.unpack hash]
    ReleaseTag tag -> do
      callProcess "git" ["clone", "-q", sourceDir, targetDir]
      callProcess "git" ["-C", targetDir, "checkout", "refs/tags/" ++ MT.unpack tag]

  -- Remove .git and ignored files from installed copy
  Install.cleanIgnoredFiles targetDir

-- | Install from a remote git source (pure IO)
installRemoteIO :: GitProtocol -> FilePath -> GitRemote -> IO ()
installRemoteIO gitprot targetDir remote = do
  let gitUrl = buildGitUrl gitprot remote
  callProcess "git" ["clone", "-q", gitUrl, targetDir]
  checkoutRefIO targetDir (gitReference remote)
  Install.cleanIgnoredFiles targetDir

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
checkoutRefIO targetDir selector =
  let gitQuiet args = callProcess "git" (["-c", "advice.detachedHead=false", "-C", targetDir] ++ args)
   in case selector of
        LatestDefaultBranch -> return ()
        LatestOnBranch branch -> do
          gitQuiet ["checkout", MT.unpack branch]
          gitQuiet ["pull"]
        CommitHash hash ->
          gitQuiet ["checkout", MT.unpack hash]
        ReleaseTag tag ->
          gitQuiet ["checkout", "tags/" ++ MT.unpack tag]

-- }}}
