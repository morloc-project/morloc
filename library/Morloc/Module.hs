{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Module
Description : Morloc module imports and paths
Copyright   : (c) Zebulun Arendsee, 2016-2024
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
  ( ModuleSource(..)
  , installModule
  , findModule
  , loadModuleMetadata
  , handleFlagsAndPaths
  ) where

import Morloc.Namespace
import Morloc.Data.Doc
import qualified Data.Char as DC
import qualified Morloc.Config as Config
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified Data.Yaml.Config as YC

-- | Specify where a module is located
data ModuleSource
  = LocalModule (Maybe String)
  -- ^ A module in the working directory
  | GithubRepo String
  -- ^ A module stored in an arbitrary Github repo: "<username>/<reponame>"
  | CoreGithubRepo String
  -- ^ The repo name of a core package, e.g., "math"

-- | Look for a local morloc module.
findModule :: Maybe (Path, MVar) -> MVar -> MorlocMonad Path
findModule currentModuleM importModule = do
  config <- MM.ask
  let lib = Config.configLibrary config
      plain = Config.configPlain config
      allPaths = getModulePaths lib plain currentModuleM importModule
  existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
  case existingPaths of
    [x] -> return x
    xs@(x:_) -> do
      MM.say $ "WARNING: module path shadowing, for" <+> pretty importModule <+> "found paths:"
             <+> "\n  " <> pretty xs
             <> "\n  selecting path:" <+> pretty x
      return x
    [] ->
      MM.throwError . CannotLoadModule . render $
        "module" <+> squotes (pretty importModule) <+> "not found among the paths:" <+> list (map pretty allPaths) <> "\n" <>
        "current module:" <+> viaShow currentModuleM <> "\n" <>
        "import module:" <+> viaShow importModule

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
    -> Path -- ^ morloc plain (e.g., "morloclib")
    -> Maybe (Path, MVar) -- ^ the path and name of the current module (e.g., `Just ("foo.loc", MVar "bar")`)
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
getModulePaths lib plain Nothing (splitModuleName -> namePath) = map MS.joinPath paths where

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

    plainPaths = [
            [lib, "plain", plain] <> init namePath <> [last namePath <> ".loc"]
          , [lib, "plain", plain] <> namePath <> ["main.loc"]
        ]

    paths = localPaths <> plainPaths <> systemPaths

getModulePaths lib plain (Just (MS.splitPath -> modulePath, splitModuleName -> moduleName)) (splitModuleName -> importName) =
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
                -- plain paths
                , [lib, "plain", plain] <> init importName <> [last importName <> ".loc"]
                , [lib, "plain", plain] <> importName <> ["main.loc"]
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
  , ["/usr/bin", sofile]
  , ["/usr/local/bin", sofile]
  ]

handleFlagsAndPaths :: Lang -> [Source] -> MorlocMonad ([Source], [MT.Text], [Path])
handleFlagsAndPaths CppLang srcs = do
  state <- MM.get
  let gccflags = filter (/= "") . map packageGccFlags $ statePackageMeta state

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
         , gccflags ++ (map MT.pack . concat) (mlcInclude : libflags)
         -- paths to files to include
         , unique (catMaybes paths)
         )
handleFlagsAndPaths _ srcs = return (srcs, [], [])

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

-- | Attempt to clone a package from github
installGithubRepo ::
     String -- ^ the repo path ("<username>/<reponame>")
  -> [String] -- ^ directory path (e.g., ["github"], or ["plain", "rbase"] for core)
  -> String -- ^ the url for github (e.g., "https://github.com/")
  -> MorlocMonad ()
installGithubRepo repo dirPath url = do
  config <- MM.ask
  let path = foldl MS.combine (Config.configLibrary config) (dirPath <> [repo])
      cmd = unwords ["git clone", url, path]
  MM.runCommand "installGithubRepo" (MT.pack cmd)

-- | Install a morloc module
installModule
    :: ModuleSource
    -> Maybe Path -- plain path
    -> MorlocMonad ()
installModule (GithubRepo repo) _ =
  installGithubRepo repo ["github"] ("https://github.com/" <> repo) -- repo has form "user/reponame"
installModule (CoreGithubRepo name') (Just plain) = do
  plainDir <- MM.asks configPlain
  installGithubRepo name' ["plain", plain] ("https://github.com/" <> plainDir <> "/" <> name')
installModule (LocalModule Nothing) _ =
  MM.throwError (NotImplemented "module installation from working directory")
installModule (LocalModule (Just _)) _ =
  MM.throwError (NotImplemented "module installation from local directory")
installModule _ _ = undefined
