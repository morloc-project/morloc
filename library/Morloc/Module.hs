{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , findLocalModuleAt
  , findBareModuleMaybe
  , bareLocalMatch
  , isLocalImport
  , autoInstallDep
  , loadSnapshot
  , parseSnapshotLine
  , loadModuleMetadata
  , findMainLocFile

    -- * Module installation
  , OverwriteProtocol (..)
  , withModuleLock
  , GitProtocol (..)
  , InstallReason (..)
  , TypecheckFn
  , installModule
  , installStringModule
  , parseInstallString
  , InstallRequest (..)
  , InstallFrom (..)
  , extractMorlocDeps

    -- * Module identity, origins and the plane layout
  , ModuleId (..)
  , stdlibLibrary
  , moduleKey
  , parseModuleKey
  , validModuleName
  , moduleInstallDir
  , moduleStem
  , Origin (..)
  , RemoteSource (..)
  , GitRemote (..)
  , shippedOrigins
  , renderOrigin
  , parseOrigin
  , loadOrigins
  , addOrigin
  , planeLayoutVersion
  , layoutFile
  , checkPlaneLayout
  , requirePlaneLayout
  , initPlaneLayout

    -- * Exposed-resource cleanup (for symmetric uninstall)
  , wipeExposed

    -- * Dependency-pin resolver (exported for testing)
  , PinEntry (..)
  , PinMap
  , addPin
  , hashEq
  , reconcileOverwrite
  , readInstalledHash

    -- * Case-insensitive filesystems (exported for testing)
  , caseClash
  ) where

import Control.Applicative (optional)
import Control.Exception (IOException, SomeException, catch, finally, onException, throwIO)
import qualified Control.Exception as Exc
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Writer (tell)
import GHC.IO.Handle.Lock (LockMode(ExclusiveLock), hLock, hTryLock)
import Text.Parsec (Parsec, try, parse, many, many1, eof)
import Text.Parsec.Char (char, string, alphaNum, digit, satisfy)
import Text.Parsec.Text ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as DC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Yaml as Yaml
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
import qualified Morloc.Version as Version
import qualified Morloc.Version.Constraint as VC
import System.Directory
import System.Environment (getEnvironment)
import System.IO.Error (ioeGetErrorString, isAlreadyExistsError)
import System.Posix.Files (createLink)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Exit (ExitCode(..))
import System.IO (hClose, stderr)
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
  , pinFrom :: !(Maybe Text)
  } deriving (Show, Eq, Ord)

-- | Resolver state: module name -> pinned entry. Built up during the
-- recursive install walk per the closer-to-install-root-wins rule.
type PinMap = Map.Map Text PinEntry

-- | An installed module (PKG-1): module 'midName' of library 'midLibrary'.
data ModuleId = ModuleId {midLibrary :: !Text, midName :: !Text}
  deriving (Show, Eq, Ord)

stdlibLibrary :: Text
stdlibLibrary = "std"

-- | The module key: @name@ for a stdlib module, else @library/name@ (MOD-5).
moduleKey :: ModuleId -> Text
moduleKey (ModuleId l n)
  | l == stdlibLibrary = n
  | otherwise = l <> "/" <> n

-- | A library or installed module name (PKG-2).
validModuleName :: Text -> Bool
validModuleName t = case MT.uncons t of
  Just (c, rest) ->
    DC.isAsciiLower c
      && MT.all (\x -> DC.isAsciiLower x || DC.isDigit x || x == '-') rest
      && not ("-" `MT.isSuffixOf` t)
  Nothing -> False

-- | Read a module key with an optional submodule path: @x@, @x.s@, @l/x@ or
-- @l/x.s@ (MOD-5, MOD-7).
parseModuleKey :: Text -> Either MDoc (ModuleId, [Text])
parseModuleKey key = do
  (lib, rest) <- case MT.splitOn "/" key of
    [r] -> Right (stdlibLibrary, r)
    [l, r]
      | l == stdlibLibrary ->
          Left $
            "a standard library module is named without its library: write"
              <+> squotes (pretty r) <+> "for" <+> squotes (pretty key)
      | otherwise -> Right (l, r)
    _ -> Left $ squotes (pretty key) <+> "is not a module name (expected name or library/name)"
  (name, sub) <- case MT.splitOn "." rest of
    (n : s) | all (not . MT.null) s -> Right (n, s)
    _ -> Left $ squotes (pretty key) <+> "is not a module name"
  unless (validModuleName lib) . Left $
    "library name" <+> squotes (pretty lib) <+> "is not of the form [a-z][a-z0-9-]*"
  unless (validModuleName name) . Left $
    "module name" <+> squotes (pretty name) <+> "is not of the form [a-z][a-z0-9-]*"
  return (ModuleId lib name, sub)

-- | Where a module is installed (PKG-3).
moduleInstallDir :: Config.Config -> ModuleId -> Path
moduleInstallDir config (ModuleId l n) = Config.configLibrary config </> MT.unpack l </> MT.unpack n

-- | The stem of a module's record and lock files under fdb.
moduleStem :: ModuleId -> String
moduleStem (ModuleId l n) = MT.unpack l <> "." <> MT.unpack n

-- | The fdb stem of a key; a key that names no module keeps its text.
keyStem :: Text -> String
keyStem key = case parseModuleKey key of
  Right (mid, _) -> moduleStem mid
  Left _ -> map (\c -> if c == '/' then '.' else c) (MT.unpack key)

-- | A library's origin (PKG-5): its modules live at @host:owner/name@.
data Origin = Origin {originHost :: !RemoteSource, originOwner :: !Text}
  deriving (Show, Eq, Ord)

-- | The libraries whose origins ship with every environment (PKG-5).
shippedOrigins :: Map.Map Text Origin
shippedOrigins =
  Map.fromList
    [ (stdlibLibrary, Origin RemoteGithub "morloclib")
    , ("bio", Origin RemoteGithub "morloc-bio")
    ]

hostName :: RemoteSource -> Text
hostName RemoteGithub = "github"
hostName RemoteGitlab = "gitlab"
hostName RemoteBitbucket = "bitbucket"
hostName RemoteCodeberg = "codeberg"
hostName RemoteAzure = "azure"

renderOrigin :: Origin -> Text
renderOrigin (Origin h o) = hostName h <> ":" <> o

parseOrigin :: Text -> Maybe Origin
parseOrigin t = case parse ((Origin <$> parseSrcForm <*> parseModuleSegment) <* eof) "" t of
  Right o -> Just o
  Left _ -> Nothing

-- | Two origins are one when the host matches and the owner matches without
-- case, as hosts compare owners (PKG-5).
sameOrigin :: Origin -> Origin -> Bool
sameOrigin a b = originHost a == originHost b && MT.toLower (originOwner a) == MT.toLower (originOwner b)

originsFile :: Config.Config -> Path
originsFile config = Config.configState config </> "origins.yaml"

-- | The environment's origins: the shipped ones and those recorded in its
-- origins file, which may not name a shipped library.
loadOrigins :: MorlocMonad (Map.Map Text Origin)
loadOrigins = do
  config <- MM.ask
  let f = originsFile config
  exists <- liftIO $ doesFileExist f
  recorded <-
    if not exists
      then return []
      else do
        decoded <- liftIO (Yaml.decodeFileEither f :: IO (Either Yaml.ParseException (Maybe (Map.Map Text Text))))
        entries <- case decoded of
          Left e -> MM.throwRejected $ "Cannot read" <+> pretty f <> ":" <+> pretty (Yaml.prettyPrintParseException e)
          Right m -> return (fromMaybe Map.empty m)
        flip mapM (Map.toList (entries :: Map.Map Text Text)) $ \(lib, o) -> do
          unless (validModuleName lib) . MM.throwRejected $
            pretty f <+> "names the library" <+> squotes (pretty lib) <> ", which is not of the form [a-z][a-z0-9-]*"
          when (Map.member lib shippedOrigins) . MM.throwRejected $
            pretty f <+> "names the library" <+> squotes (pretty lib)
              <> ", whose origin ships with every environment and cannot be changed"
          case parseOrigin o of
            Just origin -> return (lib, origin)
            Nothing -> MM.throwRejected $
              pretty f <+> "gives" <+> squotes (pretty o) <+> "for" <+> squotes (pretty lib)
                <> "; an origin is host:owner, e.g. github:weena"
  return (Map.union shippedOrigins (Map.fromList recorded))

-- | Record a library's origin (PKG-5), as `morloc library add` does: under
-- the install lock, so no install records one at the same time.
addOrigin :: Text -> Origin -> MorlocMonad ()
addOrigin lib origin = do
  config <- MM.ask
  withInstallLock config $ do
    liftIO $ initPlaneLayout config
    writeOrigin lib origin

-- | Write a library's origin to the origins file (inside 'withInstallLock').
-- A shipped library, or one recorded with another origin, is refused.
writeOrigin :: Text -> Origin -> MorlocMonad ()
writeOrigin lib origin = do
  config <- MM.ask
  unless (validModuleName lib) . MM.throwRejected $
    "library name" <+> squotes (pretty lib) <+> "is not of the form [a-z][a-z0-9-]*"
  origins <- loadOrigins
  case Map.lookup lib origins of
    Just o
      | sameOrigin o origin -> return ()
      | otherwise -> MM.throwRejected $
          "Library" <+> squotes (pretty lib) <+> "already comes from" <+> pretty (renderOrigin o)
    Nothing -> do
      let recorded = Map.insert lib origin (Map.difference origins shippedOrigins)
          body = MT.unlines [l <> ": \"" <> renderOrigin o <> "\"" | (l, o) <- Map.toList recorded]
          f = originsFile config
      liftIO $ do
        createDirectoryIfMissing True (MS.takeDirectory f)
        TIO.writeFile (f <> ".tmp") body
        renameFile (f <> ".tmp") f
      MM.say $ "Recorded library" <+> squotes (pretty lib) <+> "from" <+> pretty (renderOrigin origin)

-- | Run an action holding the environment's install lock: one install, or
-- one origin change, at a time.
withInstallLock :: Config.Config -> MorlocMonad a -> MorlocMonad a
withInstallLock config = withModuleLock (Config.fdbDir config) "_install"

-- | The plane layout this compiler reads and writes (PLANE-1).
planeLayoutVersion :: Int
planeLayoutVersion = 2

-- | The state's layout file (PLANE-1). It lives at the state root, beside
-- the stores it describes, and is created once, never rewritten.
layoutFile :: Config.Config -> Path
layoutFile config = Config.configState config </> "LAYOUT"

-- | Check that the state was written with this compiler's layout (PLANE-1).
-- A state with no layout file passes only while it holds no modules.
checkPlaneLayout :: Config.Config -> IO ()
checkPlaneLayout config = do
  let f = layoutFile config
  present <- doesFileExist f
  if present
    then do
      txt <- TIO.readFile f
      case MT.words txt of
        ["layout", n] | [(v, "")] <- reads (MT.unpack n) ->
          if v == planeLayoutVersion
            then return ()
            else
              if v > planeLayoutVersion
                then
                  ioError . userError $
                    "The morloc state at " <> Config.configState config <> " was written by a newer morloc (layout "
                      <> show v <> "; this morloc reads layout " <> show planeLayoutVersion <> "). Use that version of morloc with it."
                else older (Just v)
        _ -> older Nothing
    else do
      planeEntries <- listIfDir (Config.configLibrary config)
      records <- filter (".module" `MS.isExtensionOf`) <$> listIfDir (Config.fdbDir config)
      unless (null planeEntries && null records) (older Nothing)
  where
    listIfDir d = do
      e <- doesDirectoryExist d
      if e then listDirectory d else return []
    older :: Maybe Int -> IO ()
    older v =
      ioError . userError $
        "The morloc state at " <> Config.configState config <> " was written by an older morloc ("
          <> maybe "no layout file" (\x -> "layout " <> show x) v <> "; this morloc reads layout "
          <> show planeLayoutVersion <> "). Rebuild the environment ('mim new'), or remove its"
          <> " src/morloc/plane, fdb, modules, origins.yaml, snapshots and LAYOUT, run 'morloc init' and"
          <> " reinstall your modules."

-- | 'checkPlaneLayout' as a rejection.
requirePlaneLayout :: MorlocMonad ()
requirePlaneLayout = do
  config <- MM.ask
  r <- liftIO $ Exc.try (checkPlaneLayout config)
  case r of
    Left (e :: IOException) -> MM.throwRejected (pretty (ioeGetErrorString e))
    Right () -> return ()

-- | Check the layout, and create the layout file of a new state (PLANE-1).
-- The file is linked into place, so it appears whole or not at all, and an
-- existing one is never changed.
initPlaneLayout :: Config.Config -> IO ()
initPlaneLayout config = do
  checkPlaneLayout config
  let f = layoutFile config
  present <- doesFileExist f
  unless present $ do
    createDirectoryIfMissing True (Config.configState config)
    t <- Time.getPOSIXTime
    pid <- show <$> (getProcessID :: IO ProcessID)
    let tmp = f <> "." <> pid <> "." <> show (floor (t * 1000000) :: Integer) <> ".tmp"
    TIO.writeFile tmp layoutText
    createLink tmp f `catch` \(e :: IOException) -> unless (isAlreadyExistsError e) (ioError e)
    removeFile tmp
  checkPlaneLayout config

layoutText :: Text
layoutText = "layout " <> MT.pack (show planeLayoutVersion) <> "\n"

-- | Case-insensitive hash equality. Git is mid-transition between SHA-1
-- (40 hex chars) and SHA-256 (64 hex chars); compare lowercased so
-- copy-paste from different tools doesn't spuriously conflict.
hashEq :: Text -> Text -> Bool
hashEq a b = MT.toLower a == MT.toLower b

-- | Add a pin to the map per closer-wins. Equal-depth disagreement throws.
addPin :: PinMap -> Int -> Text -> Text -> Text -> Maybe Text -> MorlocMonad PinMap
addPin pinMap depth declaredBy depName depHash depFrom =
  case Map.lookup depName pinMap of
    Nothing ->
      return $ Map.insert depName (PinEntry depHash depth declaredBy depFrom) pinMap
    Just existing
      | pinDepth existing < depth ->
          -- existing entry is closer to root, keep it
          return pinMap
      | pinDepth existing == depth ->
          if hashEq (pinHash existing) depHash
            then return pinMap
            else MM.throwRejected $
                   "Conflicting pins for module" <+> squotes (pretty depName)
                   <+> "at depth" <+> pretty depth <> ":"
                   <+> "hash" <+> squotes (pretty (pinHash existing))
                   <+> "from" <+> squotes (pretty (pinDeclaredBy existing))
                   <+> "vs hash" <+> squotes (pretty depHash)
                   <+> "from" <+> squotes (pretty declaredBy)
      | otherwise ->
          -- existing is deeper, we're closer, replace
          return $ Map.insert depName (PinEntry depHash depth declaredBy depFrom) pinMap

-- | Read the @installed_hash@ field from a module's fdb manifest.
-- Returns @Nothing@ if the manifest is absent, malformed, or has no hash
-- (e.g. legacy installs predating this schema bump).
readInstalledHash :: FilePath -> Text -> IO (Maybe Text)
readInstalledHash fdbDir modName = do
  let manifestPath = fdbDir </> keyStem modName ++ ".module"
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

-- | Apply an authoritative pin to a module source: when a hash is resolved (env
-- snapshot, else a closer-wins package pin) and the install string gave no
-- explicit git ref, fetch that exact commit -- so the fetched hash matches what
-- gets recorded and coherence-checked. An explicit ref in the install string
-- (@\@branch:@/@\@version:@/@\@hash:@) is left untouched; a genuine disagreement
-- is then surfaced by the coherence check rather than silently overridden.
pinSource :: Maybe Text -> ModuleSource -> ModuleSource
pinSource (Just h) (ModuleSourceRemoteGit r)
  | gitReference r == LatestDefaultBranch =
      ModuleSourceRemoteGit r {gitReference = CommitHash h}
pinSource _ src = src

moduleInstallError :: MDoc -> MorlocMonad a
moduleInstallError msg = MM.throwFailed $ "Failed to install module:" <+> msg

-- | Recursively remove the install target if it exists. Used both as the
-- onException handler on the initial fetch IO and to roll back when a
-- later validation step fails.
runCleanup :: FilePath -> IO ()
runCleanup targetDir = do
  exists <- doesDirectoryExist targetDir
  when exists $ removeDirectoryRecursive targetDir

-- | Run an action holding an advisory @flock@ on the lockfile @<name>.lock@
-- under fdb, released when the action ends however it ends. A message is
-- printed only when the lock is contended.
withModuleLock :: FilePath -> Text -> MorlocMonad a -> MorlocMonad a
withModuleLock fdbDir name action = do
  let lockPath = fdbDir </> (keyStem name ++ ".lock")
  liftIO $ createDirectoryIfMissing True fdbDir
  h <- liftIO $ MS.openLockFile lockPath
  gotImmediately <- liftIO $ hTryLock h ExclusiveLock `onException` hClose h
  unless gotImmediately $ do
    MM.say "Waiting for another morloc process to finish installing..."
    liftIO $ hLock h ExclusiveLock `onException` hClose h
  -- The action runs as plain IO so the lock is released however it ends: a
  -- MorlocError, or an IO exception that a caller may catch and go on from.
  cfg <- ask
  st <- get
  ((r, logs), st') <-
    liftIO $ MM.runMorlocStack cfg st action `finally` hClose h
  put st'
  tell logs
  either throwError return r

-- | Spawn the module's setup script. Working directory is the module's
-- installed location, stdout and stderr stream to the morloc process's
-- stderr so the user sees pip/apt-get/etc. output live. The morloc
-- environment is exposed via MORLOC_* variables. Non-zero exit raises
-- an IOException; the caller is responsible for cleanup-on-exception.
runSetupScript ::
     Config.Config
  -> ModuleId
  -> PackageMeta
  -> FilePath   -- ^ targetDir (the module's installed dir)
  -> FilePath   -- ^ resolved absolute path to the setup script
  -> IO ()
runSetupScript config mid meta targetDir setupPath = do
  baseEnv <- getEnvironment
  let dirs = exposeDirsFor config mid
      name = moduleKey mid
      extraEnv =
        [ ("MORLOC_HOME",            Config.configHome config)
        , ("MORLOC_MODULE_NAME",     MT.unpack (midName mid))
        , ("MORLOC_MODULE_LIBRARY",  MT.unpack (midLibrary mid))
        , ("MORLOC_MODULE_VERSION",  MT.unpack (packageVersion meta))
        , ("MORLOC_MODULE_DIR",      targetDir)
        , ("MORLOC_PLANE_DIR",       Config.configLibrary config)
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

exposeDirsFor :: Config.Config -> ModuleId -> ExposeDirs
exposeDirsFor config (ModuleId lib name) =
  let modName = MT.unpack name
      libName = MT.unpack lib
  in ExposeDirs
       -- Installed module resources are STATE: they live under the state root
       -- (MORLOC_STATE/modules), NOT interleaved into the immutable runtime
       -- lib/include trees. This is the un-nesting that lets a container mount
       -- state without shadowing the baked runtime.
       { exposeCppDir = Config.moduleDir config "include" </> libName </> modName
       , exposePyDir  = Config.moduleDir config "python" </> "mlc_lib" </> pythonizeName libName </> pythonizeName modName
       , exposeRDir   = Config.moduleDir config "R" </> libName </> modName
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
wipeExposed :: Config.Config -> ModuleId -> IO ()
wipeExposed config mid = do
  let dirs = exposeDirsFor config mid
  mapM_ wipeIfExists [exposeCppDir dirs, exposePyDir dirs, exposeRDir dirs]
  where
    wipeIfExists d = do
      e <- doesDirectoryExist d
      when e $ removeDirectoryRecursive d
      -- a library directory left empty is removed with its last module
      let parent = MS.takeDirectory d
      parentExists <- doesDirectoryExist parent
      when parentExists $ do
        rest <- listDirectory parent
        when (null rest) $ removeDirectory parent

-- | Copy files declared in `expose` into per-language well-known
-- directories under $MORLOC_HOME, namespaced by module. Pre-wipes any
-- prior per-module exposure so stale files from earlier installs do not
-- linger. Patterns are routed through 'Install.resolveIncludePatterns',
-- so glob syntax matches `packageInclude`.
runExpose ::
     Config.Config
  -> ModuleId
  -> ExposeSet
  -> FilePath      -- ^ module's installed dir (source of the copy)
  -> IO ()
runExpose config mid es targetDir = do
  let dirs = exposeDirsFor config mid
      modName = MT.unpack (moduleKey mid)
  wipeExposed config mid
  unless (null (exposeCpp es)) $
    copyExposed targetDir (exposeCppDir dirs) modName "cpp" (exposeCpp es)
  unless (null (exposePy es)) $
    copyExposed targetDir (exposePyDir dirs) modName "py" (exposePy es)
  unless (null (exposeR es)) $ do
    -- R package names are global to a process (PKG-3)
    let rRoot = Config.moduleDir config "R"
    libs <- doesDirectoryExist rRoot >>= \e -> if e then listDirectory rRoot else return []
    others <- filterM (\l -> doesDirectoryExist (rRoot </> l </> MT.unpack (midName mid)))
      [l | l <- libs, l /= MT.unpack (midLibrary mid)]
    case others of
      l : _ -> ioError . userError $
        "module '" <> modName <> "' exposes the R package '" <> MT.unpack (midName mid)
          <> "', which library '" <> l <> "' already exposes; R loads one package of a name"
      [] -> return ()
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
-- @localRoot@ is the root of the importing module's package (MOD-6).
findModule :: Maybe Path -> MVar -> MVar -> MorlocMonad Path
findModule localRoot currentModule importModule
  | isLocalImport importModule = findLocalModule currentModule importModule
  | otherwise = findBareModule localRoot currentModule importModule

-- | Resolve a local import from the project root.
-- e.g., .foo.bar -> <root>/foo/bar.loc or <root>/foo/bar/main.loc
findLocalModule :: MVar -> MVar -> MorlocMonad Path
findLocalModule currentModule importModule = do
  allowLocal <- MM.gets stateAllowLocalModules
  if not allowLocal
    then MM.throwRejected $
      "Within module" <+> squotes (pretty currentModule)
        <> "," <+> "local import" <+> squotes (pretty importModule)
        <+> "is not permitted in eval mode."
        <+> "Use 'morloc make' to build with local modules."
    else return ()
  projectRoot <- MM.gets stateProjectRoot
  case projectRoot of
    Nothing -> MM.throwRejected $
      "Cannot resolve local import" <+> squotes (pretty importModule)
        <+> "without a project root (are you reading from stdin?)"
    Just root -> findLocalModuleAt currentModule root importModule

-- | Resolve a local import from the root of the package that contains it.
-- e.g., .foo.bar -> <root>/foo/bar.loc or <root>/foo/bar/main.loc
findLocalModuleAt :: MVar -> Path -> MVar -> MorlocMonad Path
findLocalModuleAt currentModule root importModule = do
      let MV importText = importModule
          nameParts = map MT.unpack $ MT.splitOn "." (MT.drop 1 importText)
          candidates =
            [ MS.joinPath (root : init nameParts ++ [last nameParts ++ ".loc"])
            , MS.joinPath (root : nameParts ++ ["main.loc"])
            ]
      existingPaths <- liftIO . fmap catMaybes . mapM getFile $ candidates
      case existingPaths of
        (x : _) -> return x
        [] -> MM.throwRejected $
          "Within module" <+> squotes (pretty currentModule)
            <> "," <+> "failed to import local module" <+> squotes (pretty importModule)
            <> "\nThe following paths were searched:\n"
            <+> indent 4 (vsep (map pretty candidates))

-- | The installed paths an import key names (MOD-7, PKG-3): exactly one
-- module directory, and within it the submodule's file.
systemModulePaths :: Config.Config -> MVar -> [Path]
systemModulePaths config (MV key) = case parseModuleKey key of
  Left _ -> []
  Right (mid, sub) ->
    let dir = moduleInstallDir config mid
        subPath = map MT.unpack sub
     in case subPath of
          [] -> [dir </> "main.loc", dir </> (MT.unpack (midName mid) <> ".loc")]
          _ ->
            [ MS.joinPath (dir : init subPath <> [last subPath <> ".loc"])
            , MS.joinPath (dir : subPath <> ["main.loc"])
            ]

-- | The installed module an import key names, if present.
findBareModuleMaybe :: MVar -> MorlocMonad (Maybe Path)
findBareModuleMaybe importModule = do
  config <- MM.ask
  recorded <- liftIO $ hasRecord config importModule
  if recorded
    then listToMaybe <$> liftIO (catMaybes <$> mapM getFile (systemModulePaths config importModule))
    else return Nothing

-- | Whether the module an import key names has an fdb record: one without
-- is not installed, whatever its directory holds.
hasRecord :: Config.Config -> MVar -> IO Bool
hasRecord config (MV key) = case parseModuleKey key of
  Right (mid, _) -> doesFileExist (moduleRecord config mid)
  Left _ -> return False

-- | The local module a bare import's name would reach under @localRoot@,
-- where local imports are allowed.
bareLocalMatch :: Maybe Path -> MVar -> MorlocMonad (Maybe Path)
bareLocalMatch localRoot importModule@(MV importText) = do
  allowLocal <- MM.gets stateAllowLocalModules
  case localRoot of
    Just root | allowLocal, not ("/" `MT.isInfixOf` importText) -> do
      let namePath = splitModuleName importModule
      listToMaybe . catMaybes <$> liftIO (mapM getFile
        [ MS.joinPath (root : init namePath <> [last namePath <> ".loc"])
        , MS.joinPath (root : namePath <> ["main.loc"])
        ])
    _ -> return Nothing

-- | Resolve a dotless import to an installed module (MOD-7). A local module
-- of the same name under @localRoot@ is never used; when no installed module
-- exists, it is named in the error.
findBareModule :: Maybe Path -> MVar -> MVar -> MorlocMonad Path
findBareModule localRoot currentModule importModule = do
  config <- MM.ask
  mid <- case parseModuleKey (unMVar importModule) of
    Left e -> MM.throwRejected $ "Within module" <+> squotes (pretty currentModule) <> ":" <+> e
    Right (m, _) -> return m
  let systemPaths = systemModulePaths config importModule
  recorded <- liftIO $ hasRecord config importModule
  existingSystem <-
    if recorded
      then liftIO . fmap catMaybes . mapM getFile $ systemPaths
      else return []
  local <- bareLocalMatch localRoot importModule
  case (existingSystem, local) of
    (x:_, _) -> return x
    ([], Just l) ->
      MM.throwRejected $
        "Within module" <+> squotes (pretty currentModule)
          <> "," <+> "no installed module" <+> squotes (pretty importModule)
          <+> "exists, and none is downloaded while the local module"
          <+> squotes (pretty l) <+> "has its name."
          <+> "A dotless import names an installed module: write"
          <+> squotes (pretty ("import ." <> unMVar importModule))
          <+> "for the local module, or run" <+> squotes ("morloc install" <+> pretty importModule)
          <+> "for the installed one"
    ([], Nothing) ->
      MM.throwRejected $
        "Within module" <+> squotes (pretty currentModule)
          <> ","
            <+> "failed to import module"
            <+> squotes (pretty importModule)
          <> "\n"
          <> "The following paths were searched:\n"
            <+> indent 4 (vsep (map pretty systemPaths))
          <> "\nMaybe try running: morloc install" <+> pretty (moduleKey mid)

{- | Install a missing module during `morloc make`. The environment's
snapshot pin wins (PKG-7), then a pin of the root project (PKG-6), else the
latest commit from the library's origin. Transitive dependencies are resolved
by the install itself. Installed modules are typechecked as part of the
build, so no typecheck callback is passed.
-}
autoInstallDep :: MVar -> MorlocMonad ()
autoInstallDep importModule = do
  mid <- case parseModuleKey (unMVar importModule) of
    Left e -> MM.throwRejected e
    Right (m, _) -> return m
  metas <- MM.gets statePackageMeta
  snapshot <- MM.gets stateSnapshot
  -- the root project's package is loaded first, so it is last, and its pins
  -- win over those of the packages it imports
  let pins = Map.fromList [(mdepKey d, (mdepHash d, mdepSource d)) | meta <- metas, d <- packageMorlocDependencies meta]
  req <- either MM.throwRejected return (dependencyRequest snapshot Map.empty pins mid)
  MM.say $ "Auto-installing missing dependency:" <+> pretty (reqText req)
  installTop DoNotOverwrite HttpsProtocol Nothing Map.empty AutoDependency req

-- | A request for a module the snapshot pins to a source, giving no source
-- of its own, fetches from the snapshot's source (PKG-7).
snapshotRoute :: Map.Map Text (Text, Maybe Text) -> InstallRequest -> Either MDoc InstallRequest
snapshotRoute snapshot req = case (reqFrom req, reqId req) of
  (FromOrigin, Just mid)
    | Just (_, Just src) <- Map.lookup (moduleKey mid) snapshot ->
        case parse (parseRemoteSource <* eof) "" src of
          Right (host, owner, repo) -> Right req {reqFrom = FromRemote host owner repo}
          Left _ -> Left $ "the snapshot source" <+> squotes (pretty src) <+> "of"
            <+> squotes (pretty (moduleKey mid)) <+> "is not host:owner/name"
  _ -> Right req

-- | The pins of a resolver state, as (hash, source) by module key.
pinRequests :: PinMap -> Map.Map Text (Text, Maybe Text)
pinRequests = Map.map (\p -> (pinHash p, pinFrom p))

-- | How a dependency is installed: the snapshot's pin and source win, then a
-- request given on the command line, then a package pin and its source, else
-- the library's origin at its latest commit.
dependencyRequest ::
  Map.Map Text (Text, Maybe Text) ->
  Map.Map Text InstallRequest ->
  Map.Map Text (Text, Maybe Text) ->
  ModuleId ->
  Either MDoc InstallRequest
dependencyRequest snapshot userRequests pins mid =
  case Map.lookup key snapshot of
    Just (h, src) -> request src (Just h)
    Nothing -> case Map.lookup key userRequests of
      Just r -> Right r {reqId = Just mid}
      Nothing -> case Map.lookup key pins of
        Just (h, src) -> request src (Just h)
        Nothing -> request Nothing Nothing
  where
    key = moduleKey mid
    request src h = do
      from <- case src of
        Nothing -> Right FromOrigin
        Just s -> case parse (parseRemoteSource <* eof) "" s of
          Right (host, owner, repo) -> Right (FromRemote host owner repo)
          Left _ -> Left $ "the source" <+> squotes (pretty s) <+> "of" <+> squotes (pretty key)
            <+> "is not host:owner/name"
      let text = maybe key id src <> maybe "" ("@hash:" <>) h
      return (InstallRequest (Just mid) from (CommitHash <$> h) text)

{- | Load and merge the env's module-pin snapshot from 'Config.snapshotDir'
(PKG-7): one @name hash@ or @library/name hash [host:owner/name]@ per line,
blank lines and @#@ comments ignored. Two entries pinning one module to
different hashes, a malformed line, or a line of a library that does not
ship with the environment and gives no source, is a hard error. Returns an
empty map when the directory is absent.
-}
loadSnapshot :: MorlocMonad (Map.Map Text (Text, Maybe Text))
loadSnapshot = do
  config <- MM.ask
  let dir = Config.snapshotDir config
  exists <- liftIO $ doesDirectoryExist dir
  if not exists
    then return Map.empty
    else do
      entries <- liftIO $ listDirectory dir
      -- Skip dot-files: a snapshot writer's atomic temp (e.g. mim's
      -- `.stdlib.txt.tmp`) or an editor/OS artifact (`.swp`, `.DS_Store`) in
      -- this directory must not be parsed as pins -- an interrupted deposit
      -- would otherwise leave a temp with a conflicting hash and hard-fail
      -- every build.
      let realEntries = filter (\e -> take 1 e /= ".") entries
      foldM mergeSnapshotFile Map.empty (map (dir </>) realEntries)
  where
    mergeSnapshotFile :: Map.Map Text (Text, Maybe Text) -> Path -> MorlocMonad (Map.Map Text (Text, Maybe Text))
    mergeSnapshotFile acc path = do
      isFile <- liftIO $ doesFileExist path
      if not isFile
        then return acc
        else do
          content <- liftIO $ TIO.readFile path
          lineEntries <- forM (mapMaybe parseSnapshotLine (MT.lines content)) $
            either (\e -> MM.throwFailed $ "In snapshot" <+> pretty path <> ":" <+> pretty e) return
          foldM (addSnapshotEntry path) acc lineEntries
    addSnapshotEntry :: Path -> Map.Map Text (Text, Maybe Text) -> (Text, Text, Maybe Text) -> MorlocMonad (Map.Map Text (Text, Maybe Text))
    addSnapshotEntry path acc (key, hash, src) =
      case Map.lookup key acc of
        Just (h, _)
          | not (hashEq h hash) ->
              MM.throwFailed $
                "Conflicting snapshot pins for module" <+> squotes (pretty key)
                <> ":" <+> pretty h <+> "vs" <+> pretty hash
                <+> "(in" <+> pretty path <> ")"
        _ -> return (Map.insert key (hash, src) acc)

-- | Parse one snapshot line (PKG-7). Blank lines and @#@ comments give
-- 'Nothing'.
parseSnapshotLine :: Text -> Maybe (Either Text (Text, Text, Maybe Text))
parseSnapshotLine ln =
  case MT.words (MT.takeWhile (/= '#') ln) of
    [] -> Nothing
    [key, hash] -> Just (entry key hash Nothing)
    [key, hash, src] -> Just (entry key hash (Just src))
    _ -> Just (Left ("expected 'name hash' or 'library/name hash [host:owner/name]': " <> ln))
  where
    entry key hash src = case parseModuleKey key of
      Left e -> Left (render e)
      Right (_, _ : _) -> Left ("a snapshot pins modules, not submodules: " <> key)
      Right (mid, [])
        | midLibrary mid == stdlibLibrary, Just _ <- src ->
            Left ("a standard library line gives no source: " <> ln)
        | Map.notMember (midLibrary mid) shippedOrigins, Nothing <- src ->
            Left ("library " <> midLibrary mid <> " does not ship with the environment, so its line needs a source: " <> ln)
        | otherwise -> Right (moduleKey mid, hash, src)

{- | Give a module path (e.g. "/your/path/foo.loc") find the package metadata.
It currently only looks for a file named "package.yaml" in the same folder
as the main "*.loc" file.
-}
findModuleMetadata :: Path -> IO (Maybe Path)
findModuleMetadata mainFile =
  getFile $ MS.combine (MS.takeDirectory mainFile) "package.yaml"

loadModuleMetadata :: Bool -> Path -> MorlocMonad ()
loadModuleMetadata isRoot main = do
  maybef <- liftIO $ findModuleMetadata main
  meta <-
    case maybef of
      (Just f) -> liftIO $ YC.loadYamlSettings [f] [] YC.ignoreEnv
      Nothing -> return defaultValue
  -- Nudge the top-level project off the deprecated `morloc-dependencies` key,
  -- which has been renamed to `morloc-deps`. Both are still accepted (see the
  -- FromJSON PackageMeta instance); the warning is scoped to the root so that
  -- imported modules still using the old alias do not spam the build.
  when isRoot $ case maybef of
    Just f -> do
      raw <- liftIO (YC.loadYamlSettings [f] [] YC.ignoreEnv :: IO Aeson.Value)
      case raw of
        Aeson.Object o
          | KM.member "morloc-dependencies" o && not (KM.member "morloc-deps" o) ->
              MM.say $
                "warning: package.yaml key 'morloc-dependencies' is deprecated;"
                  <+> "rename it to 'morloc-deps'"
        _ -> return ()
    Nothing -> return ()
  -- Reject include entries that escape the package directory. Absolute
  -- paths and `..` traversals are not allowed because they would break
  -- reproducibility and tie installs to ambient filesystem layout.
  case packageInclude meta of
    Just pats -> liftIO $ Install.validateIncludeScope pats
    Nothing -> return ()
  -- Reject dependencies whose declared source is illegal for their language
  -- (e.g. a Python dep with no source, or an R cran/bioconductor source).
  either (liftIO . ioError . userError . MT.unpack) return (checkPackageDeps meta)
  -- Local (filesystem-path) dependencies are honored ONLY from the top-level
  -- project: an imported/installed module's relative path has no meaning from the
  -- build root (and would break portability). Reject them with a precise error.
  when (not isRoot && not (Map.null (packageLocalDeps meta))) $
    liftIO . ioError . userError . MT.unpack $
      "module '" <> packageName meta <> "' declares local-deps, but local "
        <> "(filesystem-path) dependencies may only be declared by the top-level "
        <> "project being built, not by an imported module"
  -- Reject a module whose declared morloc-version excludes the running
  -- compiler: a fast, actionable failure instead of a cryptic downstream
  -- codegen error. A non-numeric compiler version (dev build) warns and passes.
  case VC.gateModuleVersion (packageName meta) (MT.pack Version.versionStr) (packageMorlocVersion meta) of
    Left err -> liftIO . ioError . userError $ MT.unpack err
    Right Nothing -> return ()
    Right (Just warning) -> MM.say (pretty warning)
  state <- MM.get
  MM.put (appendMeta meta state)
  where
    appendMeta :: PackageMeta -> MorlocState -> MorlocState
    appendMeta m s = s {statePackageMeta = m : statePackageMeta s}

splitModuleName :: MVar -> [String]
splitModuleName (MV x) = map MT.unpack $ MT.splitOn "." x


-- | An entry beside @path@ whose name differs from its name only in case.
caseClash :: Path -> IO (Maybe String)
caseClash path = do
  let name = MS.takeFileName path
      folded = map DC.toLower name
  entries <- MS.listDirectory (MS.takeDirectory path) `catch` \(_ :: IOException) -> return []
  return . listToMaybe $ [e | e <- entries, e /= name, map DC.toLower e == folded]

-- | The path, if a file exists there spelled exactly so. A case-insensitive
-- filesystem (the macOS default) also finds @Foo.loc@ for @foo.loc@; taking
-- that would build a program there whose imports fail on Linux, and could
-- load one file as two modules. The last three components, those a module
-- name contributes, must match their directory entries.
getFile :: Path -> IO (Maybe Path)
getFile x = do
  exists <- MS.doesFileExist x
  exact <- if exists then spelledExactly (3 :: Int) x else return False
  return $ if exact then Just x else Nothing
  where
    spelledExactly 0 _ = return True
    spelledExactly n p = do
      let parent = MS.takeDirectory p
          name = MS.takeFileName p
      if null name || parent == p
        then return True
        else do
          entries <- MS.listDirectory parent `catch` \(_ :: IOException) -> return [name]
          if name `elem` entries then spelledExactly (n - 1) parent else return False

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

-- | Where an install request fetches from.
data InstallFrom
  = -- | the origin of the request's library (PKG-5)
    FromOrigin
  | -- | an explicit remote: host, owner and repository
    FromRemote RemoteSource Text Text
  | -- | a local directory
    FromLocal Text
  deriving (Show, Eq, Ord)

-- | What to install: the module, when known before the fetch, where it comes
-- from, the commit, and the text it was asked for by.
data InstallRequest = InstallRequest
  { reqId :: Maybe ModuleId
  , reqFrom :: InstallFrom
  , reqRef :: Maybe GitSnapshotSelector
  , reqText :: Text
  }
  deriving (Show, Eq, Ord)

-- | Parse an install string: @name@, @library/name@, @host:owner/name@ or a
-- local path (starting with @.@, @/@ or @~@), each with an optional @\@ref@.
parseInstallString :: Text -> Either Text InstallRequest
parseInstallString modstr = case parse (installString <* eof) "" modstr of
  Left e -> Left (MT.pack (show e))
  Right r -> r
  where
    installString = do
      target <-
        try (Right <$> parseRemoteSource)
          <|> try (Left . Left <$> parseLocalPath)
          <|> (Left . Right <$> parseKeyText)
      ref <- optional (try parseRef)
      return $ case target of
        Right (host, owner, repo) -> Right (InstallRequest Nothing (FromRemote host owner repo) ref modstr)
        Left (Left path) -> Right (InstallRequest Nothing (FromLocal path) ref modstr)
        Left (Right key) -> case parseModuleKey key of
          Left e -> Left (render e)
          Right (_, _ : _) -> Left ("a submodule cannot be installed on its own: " <> key)
          Right (mid, []) -> Right (InstallRequest (Just mid) FromOrigin ref modstr)
    parseKeyText = MT.pack <$> many1 (satisfy (\c -> c /= '@'))
    parseLocalPath = do
      c <- char '.' <|> char '/' <|> char '~'
      rest <- many (satisfy (/= '@'))
      return (MT.pack (c : rest))

-- | @host:owner/name@
parseRemoteSource :: Parser (RemoteSource, Text, Text)
parseRemoteSource = do
  host <- parseSrcForm
  owner <- parseModuleSegment
  _ <- char '/'
  repo <- parseModuleSegment
  return (host, owner, repo)

-- | The module an install string names, without fetching it (for matching
-- command-line requests to dependencies).
installStringModule :: Text -> MorlocMonad ModuleId
installStringModule modstr = do
  req <- either (\e -> MM.throwRejected ("Failed to install module:" <+> pretty e)) return (parseInstallString modstr)
  origins <- loadOrigins
  snapshot <- MM.gets stateSnapshot
  req' <- either MM.throwRejected return (snapshotRoute snapshot req)
  fst <$> resolveRequest origins req'

-- | The module a request installs and the remote it is fetched from
-- ('Nothing' for a local directory). An explicit remote must be the
-- library's origin, when the library has one (PKG-5, PKG-6).
resolveRequest :: Map.Map Text Origin -> InstallRequest -> MorlocMonad (ModuleId, Either Text GitRemote)
resolveRequest origins req = case reqFrom req of
  FromOrigin -> case reqId req of
    Nothing -> MM.throwFailed "an install from an origin names no module"
    Just mid -> case Map.lookup (midLibrary mid) origins of
      Nothing -> MM.throwRejected $
        "Library" <+> squotes (pretty (midLibrary mid)) <+> "has no recorded origin, so"
          <+> squotes (pretty (moduleKey mid)) <+> "cannot be fetched. Install it from its repository"
          <+> "(morloc install github:<owner>/" <> pretty (midName mid) <> ")"
          <+> "or record the library with 'morloc library add" <+> pretty (midLibrary mid) <+> "github:<owner>'"
      Just o -> return (mid, Right (remote (originHost o) (originOwner o) (midName mid)))
  FromRemote host owner repo -> do
    let origin = Origin host owner
    lib <- case reqId req of
      Just mid -> return (midLibrary mid)
      Nothing -> case [l | (l, o) <- Map.toList origins, sameOrigin o origin] of
        [l] -> return l
        [] -> return (MT.toLower owner)
        ls -> MM.throwRejected $
          pretty (renderOrigin origin) <+> "is the origin of several libraries ("
            <> hsep (punctuate "," (map pretty ls)) <> "); install the module as library/name"
    let mid = ModuleId lib repo
    unless (validModuleName lib) . MM.throwRejected $
      "The library" <+> squotes (pretty lib) <+> "of" <+> pretty (renderOrigin origin <> "/" <> repo)
        <+> "is not of the form [a-z][a-z0-9-]*; record its library with 'morloc library add <library>"
        <+> pretty (renderOrigin origin) <> "' and install it as <library>/" <> pretty repo
    unless (validModuleName repo) . MM.throwRejected $
      "The repository" <+> squotes (pretty repo) <+> "cannot hold an installable module:"
        <+> "a module is named after its repository, and names are of the form [a-z][a-z0-9-]*"
    case Map.lookup lib origins of
      Just o
        | not (sameOrigin o origin) -> MM.throwRejected $
            "Library" <+> squotes (pretty lib) <+> "comes from" <+> pretty (renderOrigin o) <> ","
              <+> "so" <+> squotes (pretty (moduleKey mid)) <+> "cannot be installed from"
              <+> pretty (renderOrigin origin <> "/" <> repo)
      _ -> return ()
    return (mid, Right (remote host owner repo))
  FromLocal path -> do
    dir <- liftIO $ MS.makeAbsolute (MS.dropTrailingPathSeparator (MT.unpack path))
    meta <- readPackageMeta dir
    lib <- localLibrary dir meta
    when (MT.null (packageName meta)) . MM.throwRejected $
      pretty (dir </> "package.yaml") <+> "gives no name; an installed package needs one (PKG-1)"
    let mid = ModuleId lib (packageName meta)
    unless (validModuleName (midName mid)) . MM.throwRejected $
      "module name" <+> squotes (pretty (midName mid)) <+> "is not of the form [a-z][a-z0-9-]*"
    return (mid, Left path)
  where
    remote host owner repo =
      GitRemote
        { gitRemoteSource = host
        , gitReference = fromMaybe LatestDefaultBranch (reqRef req)
        , gitUsername = owner
        , gitReponame = repo
        }

readPackageMeta :: Path -> MorlocMonad PackageMeta
readPackageMeta dir = do
  let f = dir </> "package.yaml"
  exists <- liftIO $ doesFileExist f
  unless exists . MM.throwRejected $
    pretty dir <+> "has no package.yaml; an installed package names its library and name there (PKG-1)"
  liftIO $ YC.loadYamlSettings [f] [] YC.ignoreEnv

-- | The library of a local package. A package from before the field existed
-- that is a clone of a repository of a shipped library belongs to that
-- library (PKG-1).
localLibrary :: Path -> PackageMeta -> MorlocMonad Text
localLibrary dir meta = case packageLibrary meta of
  Just l -> do
    unless (validModuleName l) . MM.throwRejected $
      "library name" <+> squotes (pretty l) <+> "is not of the form [a-z][a-z0-9-]*"
    return l
  Nothing -> do
    url <- liftIO $
      (MT.strip . MT.pack <$> readProcess "git" ["-C", dir, "remote", "get-url", "origin"] "")
        `catch` \(_ :: IOException) -> return ""
    case urlOrigin url >>= shippedLibraryOf of
      Just l -> return l
      Nothing -> MM.throwRejected $
        pretty (dir </> "package.yaml") <+> "gives no library; add a line such as"
          <+> squotes "library: <your-github-name>" <+> "(PKG-1)"
  where
    urlOrigin u =
      let lowered = MT.toLower u
          owner prefix = MT.takeWhile (/= '/') <$> MT.stripPrefix prefix lowered
       in Origin RemoteGithub <$> (owner "https://github.com/" <|> owner "git@github.com:" <|> owner "ssh://git@github.com/")

-- | The shipped library whose origin this is, if any. A package with no
-- `library` from a shipped origin belongs to that library (PKG-1).
shippedLibraryOf :: Origin -> Maybe Text
shippedLibraryOf o = listToMaybe [l | (l, s) <- Map.toList shippedOrigins, sameOrigin s o]

{- | Typecheck callback: takes a filepath, returns list of (name, type) exports.
Passed in from the executable layer to avoid circular imports.
-}
type TypecheckFn = FilePath -> MorlocMonad [(Text, Text)]

installModule ::
  -- | How should overwrites be handled
  OverwriteProtocol ->
  -- | Remote Git download protocol (HTTPS by default)
  GitProtocol ->
  -- | Optional typecheck callback (Nothing = skip typecheck)
  Maybe TypecheckFn ->
  -- | Requests given together on the command line, by module key
  Map.Map Text InstallRequest ->
  -- | Installation string, such as "github:weena/math@version:0.1.0"
  Text ->
  MorlocMonad ()
installModule overwrite gitprot mayTypecheck userRequests modstr =
  case parseInstallString modstr of
    Left e -> MM.throwRejected $ "Failed to install module:" <+> pretty e
    Right req -> installTop overwrite gitprot mayTypecheck userRequests ExplicitInstall req

-- | One install and the dependencies it pulls in, under the environment's
-- install lock. The layout is checked and the origins read once; the
-- origins the install records (PKG-5) are written only once it succeeds.
installTop ::
  OverwriteProtocol ->
  GitProtocol ->
  Maybe TypecheckFn ->
  Map.Map Text InstallRequest ->
  InstallReason ->
  InstallRequest ->
  MorlocMonad ()
installTop overwrite gitprot mayTypecheck userRequests reason req = do
  config <- MM.ask
  withInstallLock config $ do
    liftIO $ initPlaneLayout config
    origins <- loadOrigins
    recorded <- installRequest overwrite gitprot mayTypecheck userRequests Set.empty Map.empty 0 reason origins req
    mapM_ (uncurry writeOrigin) (Map.toList recorded)

-- | Whether a module is installed: its fdb record is the commit point, and
-- a module directory without one is the debris of an install that did not
-- finish.
isInstalled :: Config.Config -> ModuleId -> IO Bool
isInstalled config mid = do
  record <- doesFileExist (moduleRecord config mid)
  dir <- doesDirectoryExist (moduleInstallDir config mid)
  return (record && dir)

moduleRecord :: Config.Config -> ModuleId -> Path
moduleRecord config mid = Config.fdbDir config </> moduleStem mid ++ ".module"

-- | Run an action; if it fails, by an error or an exception, run the
-- rollback and fail the same way.
withRollback :: MorlocMonad () -> MorlocMonad a -> MorlocMonad a
withRollback rollback action = do
  cfg <- ask
  st <- get
  r <- liftIO $ Exc.try (MM.runMorlocStack cfg st action)
  case r of
    Left (e :: SomeException) -> rollback >> liftIO (throwIO e)
    Right ((result, logs), st') -> do
      put st'
      tell logs
      case result of
        Left err -> rollback >> throwError err
        Right x -> return x

-- | Install a module and its dependencies (inside 'installTop'). Returns
-- the origins this install records, given the environment's origins with
-- those already recorded by the enclosing install.
installRequest ::
  OverwriteProtocol ->
  GitProtocol ->
  Maybe TypecheckFn ->
  -- | Requests given together on the command line, by module key
  Map.Map Text InstallRequest ->
  -- | Modules currently being installed (cycle detection)
  Set.Set Text ->
  -- | Resolver state: pinned module versions discovered so far
  PinMap ->
  -- | Depth of this install in the dependency walk (0 = install root)
  Int ->
  InstallReason ->
  Map.Map Text Origin ->
  InstallRequest ->
  MorlocMonad (Map.Map Text Origin)
installRequest overwrite gitprot mayTypecheck userRequests inProgress pinMap depth reason origins req0 = do
  config <- MM.ask
  snapshot0 <- MM.gets stateSnapshot
  req <- either MM.throwRejected return (snapshotRoute snapshot0 req0)
  (mid, from) <- resolveRequest origins req
  let key = moduleKey mid
      targetDir = moduleInstallDir config mid
  case reqId req of
    Just m | m /= mid -> MM.throwRejected $
      squotes (pretty (reqText req)) <+> "holds" <+> squotes (pretty key)
        <> ", not" <+> squotes (pretty (moduleKey m))
    _ -> return ()
  if Set.member key inProgress
    then return Map.empty
    else do
      snapshot <- MM.gets stateSnapshot
      -- Make the fetch honor the authoritative pin (snapshot, else package
      -- pin) when the request gives no commit.
      let effHash = case Map.lookup key snapshot of
            Just (h, _) -> Just h
            Nothing -> pinHash <$> Map.lookup key pinMap
          source = either (\p -> ModuleSourceLocal p (reqRef req)) (pinSource effHash . ModuleSourceRemoteGit) from
          -- PKG-5: an explicit install from an unrecorded owner records the
          -- library; its dependencies, which may be in the same library,
          -- see it at once
          register = case reqFrom req of
            FromRemote host owner _
              | reason == ExplicitInstall
              , isNothing (reqId req)
              , Map.notMember (midLibrary mid) origins ->
                  Map.singleton (midLibrary mid) (Origin host owner)
            _ -> Map.empty
      reconcileAndDispatch config source register mid targetDir
  where
    -- Reads any recorded installed_hash from fdb, compares to the expected
    -- hash from the resolver state, and upgrades DoNotOverwrite to
    -- ForceOverwrite when the cache disagrees with the pin.
    reconcileAndDispatch config source register mid targetDir = do
      let key = moduleKey mid
          fdbDir = Config.fdbDir config
      installedHash <- liftIO $ readInstalledHash fdbDir key
      snapshot <- MM.gets stateSnapshot
      let expectedHash = case Map.lookup key snapshot of
            Just (h, _) -> Just h
            Nothing -> pinHash <$> Map.lookup key pinMap
          coherenceMismatch = case (expectedHash, installedHash) of
            (Just e, Just a) -> not (hashEq e a)
            _ -> False
      -- Env-wide coherence: never silently change an installed module's
      -- hash. A snapshot/pin that disagrees with what is installed is a
      -- hard error unless an explicit --force (ForceOverwrite) sanctions
      -- the migration.
      when (coherenceMismatch && overwrite /= ForceOverwrite) . MM.throwFailed $
        "Environment coherence violation for module" <+> squotes (pretty key) <> "."
        <> "\n  installed:" <+> pretty (fromMaybe "?" installedHash)
        <> "\n  required :" <+> pretty (fromMaybe "?" expectedHash)
        <> "\nAll programs in an environment must share one hash per module."
        <> "\nReconcile deliberately with 'morloc install --force"
          <+> pretty key <> "' or 'mim update'."
      -- A library or module whose name differs from an installed one's only
      -- in case shares its directory on a case-insensitive filesystem (the
      -- macOS default), so installing one replaces the other. Refused
      -- everywhere, so an environment means the same thing on every machine.
      clashes <- liftIO $ mapM caseClash [targetDir, MS.takeDirectory targetDir]
      case catMaybes clashes of
        other : _ -> MM.throwFailed $
          "Module" <+> squotes (pretty key) <+> "cannot be installed beside"
            <+> squotes (pretty other) <> ": their names differ only in case,"
            <+> "which a case-insensitive filesystem does not distinguish"
        [] -> return ()
      let effectiveOverwrite = reconcileOverwrite overwrite expectedHash installedHash
      installed <- liftIO $ isInstalled config mid
      case (installed, effectiveOverwrite) of
        (True, DoNotOverwrite) -> do
          case reason of
            ExplicitInstall ->
              MM.say $ "Module" <+> pretty key <+> "is already installed, use --force to reinstall"
            AutoDependency ->
              MM.sayVVV $ "Module" <+> pretty key <+> "already installed, skipping"
          return Map.empty
        (True, ForceOverwrite) -> do
          case (expectedHash, installedHash) of
            (Just e, Just a) | not (hashEq e a) ->
              MM.say $ "Reinstalling" <+> pretty key
                    <+> "to reconcile cache (have" <+> pretty a
                    <> "; pin requires" <+> pretty e <> ")"
            (Just e, Nothing) ->
              MM.say $ "Reinstalling" <+> pretty key
                    <+> "to record pinned hash" <+> pretty e
            _ -> return ()
          doInstall config source register mid targetDir True
        (False, _) -> do
          -- the debris of an install that did not finish
          liftIO $ do
            removePathForcibly targetDir
            removePathForcibly (moduleRecord config mid)
            wipeExposed config mid
          doInstall config source register mid targetDir False

    doInstall :: Config.Config -> ModuleSource -> Map.Map Text Origin -> ModuleId -> FilePath -> Bool -> MorlocMonad (Map.Map Text Origin)
    doInstall config source register mid targetDir replacing = do
      let key = moduleKey mid
          name = midName mid
          fdbDir = Config.fdbDir config
          inProgress' = Set.insert key inProgress
          origins' = Map.union register origins
          recordPath = moduleRecord config mid
          oldDir = targetDir ++ ".old"

      MM.say $ "Fetching module" <+> squotes (pretty key) <> "..."

      -- Fetch into a temporary sibling dir, validate it there, and only then
      -- move it into place, so a rejected fetch never touches the installed
      -- module.
      liftIO $ createDirectoryIfMissing True (MS.takeDirectory targetDir)
      let tmpDir = targetDir ++ ".partial"
          ioAction = case source of
            ModuleSourceLocal path selector ->
              installLocalIO tmpDir selector path
            ModuleSourceRemoteGit remote ->
              installRemoteIO gitprot tmpDir remote
      liftIO $ (removePathForcibly tmpDir >> ioAction) `onException` runCleanup tmpDir

      meta <- liftIO $ do
        let pkgYaml = tmpDir </> "package.yaml"
        exists <- doesFileExist pkgYaml
        if exists
          then YC.loadYamlSettings [pkgYaml] [] YC.ignoreEnv
          else return defaultValue
      let shippedFrom = case source of
            ModuleSourceRemoteGit r -> shippedLibraryOf (Origin (gitRemoteSource r) (gitUsername r))
            ModuleSourceLocal _ _ -> Just (midLibrary mid)
          fetchedLibrary = case packageLibrary meta of
            Just l -> Just l
            Nothing -> shippedFrom
          fetched = ModuleId <$> fetchedLibrary <*> (if MT.null (packageName meta) then Nothing else Just (packageName meta))
      case fetched of
        Just f | f == mid -> return ()
        Just f -> do
          liftIO $ runCleanup tmpDir
          moduleInstallError $
            "the package fetched for" <+> squotes (pretty key) <+> "is"
              <+> squotes (pretty (moduleKey f)) <+> "(package.yaml library and name, PKG-3)"
              <> if midLibrary f /= midLibrary mid
                then "; a library other than its owner's name is recorded with 'morloc library add"
                  <+> pretty (midLibrary f) <+> "<host>:<owner>' before installing"
                else mempty
        Nothing -> do
          liftIO $ runCleanup tmpDir
          moduleInstallError $
            "the package.yaml of" <+> squotes (pretty key) <+> "does not give both its library and name (PKG-1)"
      liftIO $ validateModuleNameMatches name tmpDir `onException` runCleanup tmpDir

      -- Reject a module whose declared morloc-version excludes the running
      -- compiler, cleaning the temp fetch and leaving the old install intact.
      -- An explicit --force downgrades the rejection to a warning.
      case VC.gateModuleVersion key (MT.pack Version.versionStr) (packageMorlocVersion meta) of
        Right Nothing -> return ()
        Right (Just warning) -> MM.say (pretty warning)
        Left err
          | overwrite == ForceOverwrite -> MM.say ("warning:" <+> pretty err)
          | otherwise -> do
              liftIO $ runCleanup tmpDir
              moduleInstallError (pretty err)

      -- The fdb record is the commit point. A module being replaced loses
      -- its record first, and its old tree is kept aside, so a failure from
      -- here on can put it back, and a crash leaves no record naming the new
      -- tree.
      oldRecord <-
        if replacing
          then liftIO $ do
            r <- doesFileExist recordPath
            b <- if r then Just <$> BL.readFile recordPath else return Nothing
            maybe (return ()) (\x -> BL.length x `seq` return ()) b
            removePathForcibly recordPath
            removePathForcibly oldDir
            renameDirectory targetDir oldDir
            return b
          else return Nothing
      liftIO $ renameDirectory tmpDir targetDir `onException` runCleanup tmpDir

      let rollback = liftIO $ do
            removePathForcibly targetDir
            wipeExposed config mid
            restored <- doesDirectoryExist oldDir
            when restored $ do
              renameDirectory oldDir targetDir
              let oldYaml = targetDir </> "package.yaml"
              hasYaml <- doesFileExist oldYaml
              oldMeta <- if hasYaml then YC.loadYamlSettings [oldYaml] [] YC.ignoreEnv else return defaultValue
              runExpose config mid (packageExpose oldMeta) targetDir
                `catch` \(_ :: SomeException) -> return ()
              forM_ oldRecord $ \b -> do
                BL.writeFile (recordPath ++ ".tmp") b
                renameFile (recordPath ++ ".tmp") recordPath

      depRecorded <- withRollback rollback $ do
        -- Fold this package's morloc-deps into the resolver state.
        -- Closer-wins: pins declared by this package (at depth `depth`) are
        -- shadowed by any already-present pin at a smaller depth, supersede
        -- any pin at a larger depth, and conflict with another pin at the
        -- same depth that names a different hash.
        pinMap' <- foldM
          (\pm d -> addPin pm depth key (mdepKey d) (mdepHash d) (mdepSource d))
          pinMap
          (packageMorlocDependencies meta)
        let depEntries = Map.fromList [(mdepKey d, d) | d <- packageMorlocDependencies meta]

        -- The installed modules this package's files import (PKG-8)
        morlocDeps <- do
          mainFile <- liftIO $ findMainLocFile targetDir (MT.unpack name)
          case mainFile of
            Nothing -> return []
            Just f -> liftIO $ extractMorlocDeps f

        -- Stale-entry warnings: pins declared by THIS package whose module
        -- does not appear among the discovered imports.
        let importedSet = Set.fromList morlocDeps
        forM_ (packageMorlocDependencies meta) $ \d ->
          unless (Set.member (mdepKey d) importedSet) $
            MM.say $ "warning: declared dependency" <+> squotes (pretty (mdepKey d))
                  <+> "is not imported anywhere in" <+> squotes (pretty key)
                  <+> "(possibly stale)"

        -- A local install warns of each import others could not fetch (PKG-8).
        case source of
          ModuleSourceLocal _ _ ->
            forM_ morlocDeps $ \dep -> case parseModuleKey dep of
              Right (dmid, _)
                | Map.notMember (midLibrary dmid) shippedOrigins
                , maybe True (isNothing . mdepSource) (Map.lookup dep depEntries) ->
                    MM.say $
                      "warning:" <+> squotes (pretty key) <+> "imports" <+> squotes (pretty dep)
                        <> ", whose library does not ship with morloc and which has no morloc-deps source;"
                        <+> "others can install it only after recording its origin"
                        <> (if Map.member (midLibrary dmid) origins' then mempty else " (it has none here)")
              _ -> return ()
          _ -> return ()

        -- Recursively install dependencies, threading the updated resolver
        -- state and the origins recorded so far.
        snapshot <- MM.gets stateSnapshot
        recorded <- foldM
          ( \acc dep ->
              if Set.member dep inProgress'
                then return acc
                else do
                  dmid <- either MM.throwRejected (return . fst) (parseModuleKey dep)
                  depReq' <- either MM.throwRejected return $
                    dependencyRequest snapshot userRequests (pinRequests pinMap') dmid
                  when (Map.notMember dep pinMap' && Map.notMember dep snapshot) $
                    MM.sayV $ "warning: dependency" <+> squotes (pretty dep)
                           <+> "is not pinned in any morloc-deps entry along the install path"
                  MM.sayVVV $ "Auto-installing dependency:" <+> pretty (reqText depReq')
                  new <- installRequest
                    DoNotOverwrite
                    gitprot
                    mayTypecheck
                    userRequests
                    inProgress'
                    pinMap'
                    (depth + 1)
                    AutoDependency
                    (Map.union acc origins')
                    depReq'
                  return (Map.union new acc)
          )
          Map.empty
          morlocDeps

        -- Copy `expose`d files to per-language well-known dirs, namespaced
        -- by library and module. Runs before the setup script so the script
        -- can rely on the exposed paths existing.
        let es = packageExpose meta
        liftIO $ Install.validateIncludeScope (exposeCpp es ++ exposePy es ++ exposeR es)
        liftIO $ runExpose config mid es targetDir

        -- Run the optional setup script, if one is declared in package.yaml.
        -- Runs after morloc deps are on disk (a setup script may rely on
        -- transitive resources) but before typecheck (so a script that
        -- builds C++/Python/R artifacts has a chance to lay them down).
        case packageSetup meta of
          Nothing -> return ()
          Just relPath -> do
            when (MS.isAbsolute relPath) . moduleInstallError $
              "setup path must be relative to the module directory, got" <+> squotes (pretty relPath)
            when (".." `elem` MS.splitDirectories relPath) . moduleInstallError $
              "setup path may not contain '..' segments, got" <+> squotes (pretty relPath)
            let setupPath = targetDir </> relPath
            setupExists <- liftIO $ doesFileExist setupPath
            unless setupExists . moduleInstallError $
              "setup script not found at" <+> squotes (pretty setupPath)
            MM.say $ "Running setup script for" <+> squotes (pretty key)
                  <> ":" <+> pretty relPath
            liftIO $ runSetupScript config mid meta targetDir setupPath

        -- Typecheck the module (if callback provided)
        exports <- case mayTypecheck of
          Nothing -> return []
          Just typecheckFn -> do
            mainFile <- liftIO $ findMainLocFile targetDir (MT.unpack name)
            case mainFile of
              Nothing -> return []
              Just f -> typecheckFn f

        -- Write the record last: it commits the install.
        liftIO $ createDirectoryIfMissing True fdbDir
        installTime <- liftIO $ floor <$> Time.getPOSIXTime
        let resolveDepHash dep = case Map.lookup dep snapshot of
              Just (h, _) -> Just h
              Nothing -> pinHash <$> Map.lookup dep pinMap'
            morlocDepsWithHash = [(dep, resolveDepHash dep) | dep <- morlocDeps]
            installedSelfHash = case Map.lookup key snapshot of
              Just (h, _) -> Just h
              Nothing -> pinHash <$> Map.lookup key pinMap
            manifestJson =
              buildModuleManifest
                meta
                mid
                morlocDepsWithHash
                installedSelfHash
                exports
                targetDir
                (reqText req0)
                reason
                installTime
        liftIO $ do
          TIO.writeFile (recordPath ++ ".tmp") manifestJson
          renameFile (recordPath ++ ".tmp") recordPath
        return recorded
      liftIO $ removePathForcibly oldDir
      MM.say $ "Installed module" <+> squotes (pretty key)
      return (Map.union register depRecorded)

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
  ModuleId ->
  [(Text, Maybe Text)] ->
  Maybe Text ->
  [(Text, Text)] ->
  FilePath ->
  Text ->
  InstallReason ->
  Int ->
  Text
buildModuleManifest meta mid morlocDeps installedSelfHash exports installPath installSource reason installTime =
  jsonObj
    [ ("kind", jsonStr "module")
    , ("name", jsonStr (midName mid))
    , ("library", jsonStr (midLibrary mid))
    , ("key", jsonStr (moduleKey mid))
    , ("version", jsonStr (packageVersion meta))
    -- The module's declared compiler-version constraint (null = unconstrained)
    -- and the compiler that performed this install. Let the manager gate a
    -- morloc-version bump without invoking the compiler.
    , ("morloc_version", maybe jsonNull jsonStr (packageMorlocVersion meta))
    , ("built_with_morloc", jsonStr (MT.pack Version.versionStr))
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

{- | The installed modules a package imports (PKG-8), by module key: those
imported by its entry file and by every local module the entry file reaches,
found by a lightweight text scan, not the full parser. A local import @.a.b@
is the file @a/b.loc@ or @a/b/main.loc@ under the entry file's directory.
-}
extractMorlocDeps :: FilePath -> IO [Text]
extractMorlocDeps entry = go Set.empty [entry] []
  where
    root = MS.takeDirectory entry
    go _ [] acc = return (unique (reverse acc))
    go seen (f : fs) acc
      | Set.member f seen = go seen fs acc
      | otherwise = do
          exists <- doesFileExist f
          if not exists
            then go (Set.insert f seen) fs acc
            else do
              content <- TIO.readFile f
              let names = concatMap importName (stripLocComments (MT.lines content))
                  locals = [n | n <- names, "." `MT.isPrefixOf` n]
                  installed = [k | n <- names, not ("." `MT.isPrefixOf` n), Right (mid, _) <- [parseModuleKey n], let k = moduleKey mid]
              localFiles <- concat <$> mapM localFile locals
              go (Set.insert f seen) (fs <> localFiles) (reverse installed <> acc)
    localFile n = do
      let parts = map MT.unpack (MT.splitOn "." (MT.drop 1 n))
          candidates =
            [ MS.joinPath (root : init parts <> [last parts <> ".loc"])
            , MS.joinPath (root : parts <> ["main.loc"])
            ]
      if null parts then return [] else filterM doesFileExist candidates
    importName ln = case MT.stripPrefix "import " (MT.stripStart ln) of
      Nothing -> []
      Just rest ->
        let n = MT.strip (MT.takeWhile (\c -> c /= '(' && c /= ' ') rest)
         in [n | not (MT.null n)]

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

-- | Verify the installed module's declared module name (in its entry-point
-- .loc file) matches an explicitly-supplied expected name rather than the
-- directory's basename -- so the fetched content can be validated in a temp
-- dir (whose basename is @<name>.partial@) before it is swapped into place.
-- Throws IOError on mismatch / missing entry / no-named-module /
-- multiple-named-modules. Caller is responsible for cleanup-on-throw (use
-- within onException).
validateModuleNameMatches :: Text -> FilePath -> IO ()
validateModuleNameMatches dirName dir = do
  entryResult <- findMainLocFile dir (MT.unpack dirName)
  entryPath <- case entryResult of
    Just p -> return p
    Nothing -> ioError . userError $
      "no entry file: a module's entry is main.loc or " <> MT.unpack dirName <> ".loc"
  declared <- extractNamedModuleDecls entryPath
  let entryFile = MT.pack (MS.takeFileName entryPath)
  case declared of
    names | dirName `elem` names -> return ()
    [name] ->
          ioError . userError $
            "declared module name '" <> MT.unpack name <>
            "' in " <> MT.unpack entryFile <>
            " does not match the package name '" <>
            MT.unpack dirName <>
            "' in package.yaml; make them agree."
    [] ->
      ioError . userError $
        "no named morloc module found in " <> MT.unpack entryFile <>
        ": all declarations are anonymous. Add a name to the top-level " <>
        "`module` header (e.g. `module " <> MT.unpack dirName <> " (...)`)."
    multiple ->
      ioError . userError $
        "no module named '" <> MT.unpack dirName <> "' is declared in " <> MT.unpack entryFile <>
        " (it declares " <> MT.unpack (MT.intercalate ", " multiple) <> ")"

-- {{{ parse module source

type Parser = Parsec Text ()

decimal :: Parser Int
decimal = read <$> many1 digit

data RefForm = RefHash | RefBranch | RefVersion

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

parseModuleSegment :: Parser Text
parseModuleSegment = do
  firstChar <- alphaNum
  rest <- many (alphaNum <|> char '-')
  case rest of
    [] -> return (MT.pack [firstChar])
    _ | last rest == '-' -> fail "Module name cannot end with a dash"
      | otherwise -> return (MT.pack (firstChar : rest))

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
