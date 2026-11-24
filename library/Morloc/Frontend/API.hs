{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.API
Description : Entry point for the frontend pipeline (parse, typecheck, valuecheck)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Orchestrates the full frontend pipeline: parsing source files into a module
DAG, recursively resolving imports, and re-exporting the typechecker and
valuechecker entry points. This is the primary interface consumed by the
top-level compiler driver ('Morloc').
-}
module Morloc.Frontend.API
  ( parse
  , Parser.readType
  , Typecheck.typecheck
  , Typecheck.validate
  , Typecheck.resolveTypes
  , Valuecheck.valuecheck
  , Valuecheck.checkRecursion
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Morloc.Config as Config
import qualified Morloc.Data.DAG as MDD
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import Morloc.Frontend.Namespace
import Morloc.Frontend.Parser (PState (..), emptyPState)
import qualified Morloc.Frontend.Parser as Parser
import Morloc.Frontend.Token (posSrcLoc)
import qualified Morloc.Frontend.Typecheck as Typecheck
import qualified Morloc.Frontend.Valuecheck as Valuecheck
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.ProgramBuilder.Install as Install
import qualified Morloc.System as MS
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)

{- | Parse a morloc source file and all its imports into a module DAG.
Recursively discovers and parses imported modules.
-}
parse ::
  -- | path to the current module (if we are reading from a file)
  Maybe Path ->
  -- | code of the current module
  Code ->
  MorlocMonad (DAG MVar Import ExprI)
parse f (Code code) = do
  moduleConfig <- Config.loadModuleConfig f
  langMap <- buildLangMap'

  -- Load the env's module-pin snapshot only when auto-install can actually run
  -- (`morloc make`, not --offline). Read-only commands (typecheck, dump) and
  -- eval never install, so they must neither pay the directory scan nor be
  -- aborted by a conflicting snapshot they will not consult. `morloc install`
  -- loads it separately (Subcommands.cmdInstall).
  Mod.requirePlaneLayout
  autoInstall <- MM.gets stateAutoInstall
  when autoInstall $ do
    snapshot <- Mod.loadSnapshot
    MM.modify' (\st -> st {stateSnapshot = snapshot})

  -- The main module's @log-template@ becomes the program-wide default
  -- log message template. Per-label overrides win over this; the
  -- built-in defaults in 'Morloc.CodeGenerator.LogTemplate' fill any
  -- subfield neither this nor the per-label config supplies.
  MM.modify' (\st -> st {stateLogTemplate = moduleConfigLogTemplate moduleConfig})

  -- Likewise the program-wide @benchmark-template@: one row shape for the
  -- end-of-run summary the nexus renders from every label's timings.
  MM.modify' (\st -> st {stateBenchTemplate = moduleConfigBenchTemplate moduleConfig})

  -- The main module's run-scope @prologue@ / @epilogue@ templates are
  -- rendered by the nexus at run boundaries. Sub-modules cannot
  -- contribute to these (a workflow has exactly one entry-point, so
  -- run-scope events have exactly one source of truth).
  MM.modify'
    ( \st -> st
        { stateRunLog =
            case (moduleConfigPrologue moduleConfig, moduleConfigEpilogue moduleConfig) of
              (Nothing, Nothing) -> Nothing
              (mp, me) ->
                Just
                  RunLogTemplate
                    { runLogPrologue = mp
                    , runLogEpilogue = me
                    }
        }
    )

  -- Compute project root from entry-point file path
  let projectRoot = fmap MS.takeDirectory f

  -- Resolve the main module's @hash-include@ patterns into a concrete
  -- list of files. Their contents will be folded into every pool's
  -- cache hash so that editing a foreign source file (or a runtime
  -- data file the user explicitly lists) invalidates the cache. Scope
  -- validation rejects absolute paths and @..@ traversals; the same
  -- contract as @packageInclude@.
  case moduleConfigHashInclude moduleConfig of
    Just patterns | not (null patterns) -> do
      MM.liftIO $ Install.validateIncludeScope patterns
      case projectRoot of
        Just root -> do
          paths <- MM.liftIO $ Install.resolveIncludePatterns root patterns
          MM.modify' (\st -> st {stateHashIncludePaths = paths})
        Nothing -> return ()
    _ -> return ()

  reg <- MM.gets stateLangRegistry
  let parserState =
        emptyPState
          { psModuleConfig = moduleConfig
          , psLangMap = langMap
          , psTypeKinds = Map.map LR.lreTypeKinds (LR.lrEntries reg)
          , psProjectRoot = projectRoot
          }

  -- store source text, project root, and load package metadata for the main file
  case f of
    Just path -> do
      MM.modify' (\st -> st
        { stateSourceText = Map.insert path code (stateSourceText st)
        , stateProjectRoot = projectRoot
        })
      Mod.loadModuleMetadata True path
    Nothing -> return ()

  case Parser.readProgram Nothing f code parserState mempty of
    (Left (pos, e)) -> MM.throwRejectedSource (posSrcLoc pos) (pretty e)
    (Right (mainDag0, mainState)) -> do
      rejectBareSiblingImports mainDag0
      mainDag <- either MM.throwRejected return (keyEntrySiblings mainDag0)
      forM_ [k | k <- Map.keys mainDag, Map.notMember k mainDag0] $ \k -> do
        clash <- Mod.bareLocalMatch projectRoot (MV (lastComponent (unMVar k)))
        forM_ clash $ \file -> MM.throwRejected $
          "Module" <+> squotes (pretty k) <+> "declared in the entry file has the name of the local module"
            <+> pretty file
      -- capture module-level docs from the main module before imports overwrite them
      MM.modify' (\st -> st
        { stateModuleDoc = psModuleDoc mainState
        , stateModuleEpilogues = psModuleEpilogues mainState
        })
      entryPath <- case f of
        Just path -> Just <$> MM.liftIO (canonicalizePath path)
        Nothing -> return Nothing
      let entryKeys = [k | k <- Map.keys mainDag, not (Mod.isLocalImport k)]
      parseImports entryKeys entryPath mainDag mainState Map.empty Map.empty Map.empty
  where
    -- Resolve an import to a file path. Local (.dot) imports are project
    -- files and are never downloaded. A missing bare/namespaced import is
    -- auto-downloaded only when stateAutoInstall is set (make, unless
    -- --offline), we are not in eval mode, and the importing module's
    -- package holds no local module of that name (MOD-7: findModule then
    -- names it rather than install a registry package in its place).
    resolveImport :: Maybe Path -> MVar -> MVar -> MorlocMonad Path
    resolveImport localRoot mainModule importedModule
      | Mod.isLocalImport importedModule =
          Mod.findModule localRoot mainModule importedModule
      | otherwise = do
          present <- Mod.findBareModuleMaybe importedModule
          case present of
            Just path -> return path
            Nothing -> do
              autoInstall <- MM.gets stateAutoInstall
              evalMode <- MM.gets stateEvalMode
              local <- Mod.bareLocalMatch localRoot importedModule
              when (autoInstall && not evalMode && isNothing local) $
                Mod.autoInstallDep importedModule
              Mod.findModule localRoot mainModule importedModule

    -- descend recursively into imports
    --
    -- A module's local imports resolve from the root of its own package
    -- (MOD-6): the entry file's directory for the program, the directory of
    -- a package reached by a bare import for that package. In such a
    -- package, a local import `.x` becomes the module `p.x` (p the package),
    -- so two packages' local modules of one name stay distinct. @pkgs@ holds
    -- each module's package, absent for the program's own modules; @rw@
    -- holds each renamed import's package root and written name.
    parseImports ::
      [MVar] ->
      Maybe Path ->
      DAG MVar Import ExprI ->
      PState ->
      Map.Map Path MVar ->
      Map.Map MVar (MVar, Path) ->
      Map.Map MVar (Path, MVar) ->
      MorlocMonad (DAG MVar Import ExprI)
    parseImports entryKeys entryPath d s m pkgs rw = case unimported of
      [] -> do
        -- transfer source positions from parser state into MorlocState
        MM.modify' (\st -> st
          { stateSourceMap = psSourceMap s <> stateSourceMap st
          , stateTermDocs = psTermDocs s <> stateTermDocs st
          , stateStreamElems = psStreamElems s <> stateStreamElems st
          , stateMatchSites = psMatchSites s <> stateMatchSites st
          })
        -- emit any docstring warnings accumulated during desugar
        case psWarnings s of
          [] -> return ()
          ws -> MM.tell ws
        -- MOD-5: the program's module keeps its header as its key, so no
        -- import may name a module of that name
        case [(a, v) | (a, v) <- MDD.edgelist d, v `elem` entryKeys] of
          (a, v) : _ -> MM.throwRejected $
            "Module" <+> squotes (pretty a) <+> "imports" <+> squotes (pretty v)
              <> ", which names an installed module, but the program's own module has that name;"
              <+> "rename the program's module"
          [] -> return ()
        case MDD.findCycle d of
          Nothing -> return d
          Just cyc -> MM.throwRejected $
            "Import cycle:" <+> hsep (punctuate " ->" (map pretty cyc))
      ((mainModule, importedModule) : _) -> do
        importPath <- case Map.lookup importedModule rw of
          Just (root, written) -> Mod.findLocalModuleAt mainModule root written
          Nothing -> do
            localRoot <- case Map.lookup mainModule pkgs of
              Just (_, root) -> return (Just root)
              Nothing -> MM.gets stateProjectRoot
            resolveImport localRoot mainModule importedModule

        -- Every loaded module is reached from the entry file, so importing
        -- the entry file closes a cycle.
        canonPath <- MM.liftIO $ canonicalizePath importPath
        when (Just canonPath == entryPath) . MM.throwRejected $
          "Import cycle: module" <+> pretty mainModule <+> "imports the entry file"
            <+> pretty importPath <+> "as" <+> pretty importedModule

        -- Load the <main>.yaml file associated with the main morloc package file
        moduleConfig <- Config.loadModuleConfig (Just importPath)
        -- the package this module belongs to, absent for the program's own
        let pkg = case Map.lookup importedModule rw of
              Just _ -> Map.lookup mainModule pkgs
              Nothing
                | Mod.isLocalImport importedModule -> Map.lookup mainModule pkgs
                | otherwise -> Just (importedModule, MS.takeDirectory importPath)
            newState = s {psModuleConfig = moduleConfig, psOwnModule = isNothing pkg}

        case Map.lookup canonPath m of
          Just other | other /= importedModule -> MM.throwRejected $
            "The file" <+> pretty importPath <+> "is reached as both" <+> squotes (pretty other)
              <+> "and" <+> squotes (pretty importedModule)
          _ -> return ()

        Mod.loadModuleMetadata False importPath
        (childPath, code') <- openLocalModule importPath
        case Parser.readProgram (Just importedModule) childPath code' newState mempty of
          (Left (pos, e)) -> MM.throwRejectedSource (posSrcLoc pos) (pretty e)
          (Right (fileDag, s')) -> do
            siblingRoot <- case pkg of
              Just (_, root) -> return (Just root)
              Nothing -> MM.gets stateProjectRoot
            keyed <- keyFileModules importPath code' importedModule (fst <$> pkg) siblingRoot fileDag
            d3 <- foldM (insertKeyed importPath) d keyed
            let keys = map fst keyed
                (d4, renamed) = case pkg of
                  Just (p, root) ->
                    foldl
                      (\(dg, acc) k -> let (dg', r) = renameLocalImports p root k dg in (dg', acc <> r))
                      (d3, [])
                      keys
                  Nothing -> (d3, [])
                pkgs' = maybe pkgs (\v -> foldl (\acc k -> Map.insert k v acc) pkgs keys) pkg
                rw' = Map.union rw (Map.fromList renamed)
            parseImports entryKeys entryPath d4 s' (Map.insert canonPath importedModule m) pkgs' rw'
      where
        -- all modules that have already been parsed
        parsed = Map.keysSet d
        -- find all (module to module) edges in the graph where the imported
        -- module has not yet been parsed
        unimported = filter (\(_, importMod) -> not (Set.member importMod parsed)) (MDD.edgelist d)

    -- Give a package module's local imports their package-qualified names,
    -- returning each new name's package root and written name.
    renameLocalImports ::
      MVar -> Path -> MVar -> DAG MVar Import ExprI -> (DAG MVar Import ExprI, [(MVar, (Path, MVar))])
    renameLocalImports (MV p) root k dag = case Map.lookup k dag of
      Nothing -> (dag, [])
      Just (ExprI i e, edges) ->
        let locals = [v | (v, _) <- edges, Mod.isLocalImport v]
            qualified v@(MV t) = if Mod.isLocalImport v then MV (p <> t) else v
            renameImport imp = imp {importModuleName = qualified (importModuleName imp)}
            renameExpr (ExprI j (ModE n es)) = ExprI j (ModE n (map renameExpr es))
            renameExpr (ExprI j (ImpE imp)) = ExprI j (ImpE (renameImport imp))
            renameExpr x = x
            edges' = [(qualified v, renameImport imp) | (v, imp) <- edges]
         in ( Map.insert k (renameExpr (ExprI i e), edges') dag
            , [(qualified v, (root, v)) | v <- locals]
            )

    -- MOD-1, MOD-5: the module the import reaches takes the import's key;
    -- every other module of the file is a local module of the file's
    -- package. The reached one is the module named by the import's last
    -- component, or the file's only module when it has no header of its own.
    keyFileModules ::
      Path -> Text -> MVar -> Maybe MVar -> Maybe Path -> DAG MVar Import ExprI -> MorlocMonad [(MVar, (ExprI, [(MVar, Import)]))]
    keyFileModules path source key pkgKey root fileDag = do
      rejectBareSiblingImports fileDag
      let entries = Map.toList fileDag
          lastPart (MV t) = lastComponent t
          -- the key's module path, e.g. `lib.math` for `.lib.math` or `l/p.lib.math`
          keyPath = T.dropWhile (== '.') (last (T.splitOn "/" (unMVar key)))
          names n = n == keyPath || ("." <> n) `T.isSuffixOf` keyPath
          named = [e | e@(MV n, _) <- entries, names n]
          -- a header-less file is read as `module main`
          headerless = not (any (T.isPrefixOf "module" . T.stripStart) (T.lines source))
          anonymous = case entries of
            [e@(MV n, _)] | T.isPrefixOf "." n || (n == "main" && headerless) -> [e]
            _ -> []
      reached <- case named <> anonymous of
        (e : _) -> return e
        [] -> MM.throwRejected $
          "Loading" <+> pretty path <+> "for the import of" <+> pretty key
            <+> "found no module of that name; it declares"
            <+> hsep (punctuate "," (map (pretty . fst) entries))
      let others = filter ((/= fst reached) . fst) entries
          localName (MV n) = MV ("." <> lastComponent n)
          step seen = Set.union seen $ Set.fromList
            [ v | (n, (_, edges)) <- (fst reached, snd reached) : others
            , n == fst reached || Set.member (localName n) seen
            , (v, _) <- edges ]
          closure seen = let seen' = step seen in if seen' == seen then seen else closure seen'
          used = closure Set.empty
      siblings <- forM (filter (\(n, _) -> Set.member (localName n) used) others) $ \(n, e) -> do
        clash <- Mod.bareLocalMatch root (MV (lastPart n))
        case clash of
          Just file -> MM.throwRejected $
            "Module" <+> squotes (pretty n) <+> "declared in" <+> pretty path
              <+> "has the name of the local module" <+> pretty file
          Nothing -> return (siblingKey pkgKey n, e)
      return ((key, snd reached) : siblings)

    insertKeyed ::
      Path -> DAG MVar Import ExprI -> (MVar, (ExprI, [(MVar, Import)])) -> MorlocMonad (DAG MVar Import ExprI)
    insertKeyed path dag (k, (e, edges))
      | Map.member k dag = MM.throwRejected $
          "Loading" <+> pretty path <+> "declares the module" <+> squotes (pretty k)
            <> ", which is already loaded"
      | otherwise = return (Map.insert k (renameModE k e, edges) dag)

-- | Imports, by one module of a file, of another module of the file by its
-- bare name (MOD-1 has them imported as @.name@).
bareSiblingImports :: DAG MVar Import ExprI -> [(MVar, MVar)]
bareSiblingImports dag =
  [ (n, v)
  | (n, (_, edges)) <- Map.toList dag
  , (v, _) <- edges
  , v /= n
  , Map.member v dag
  ]

-- | A bare import of a sibling's name names an installed module (MOD-7);
-- when none is installed, the sibling was meant, and the error says how to
-- import it.
rejectBareSiblingImports :: DAG MVar Import ExprI -> MorlocMonad ()
rejectBareSiblingImports dag =
  forM_ (bareSiblingImports dag) $ \(n, v) -> do
    installed <- Mod.findBareModuleMaybe v
    when (isNothing installed) . MM.throwRejected $
      "Module" <+> squotes (pretty n) <+> "imports" <+> squotes (pretty v)
        <+> "by a bare name, which names an installed module, and none is installed;"
        <+> "a module of the same file is imported as" <+> squotes ("import ." <> pretty v)

-- | A sibling module's key: local to the program, or to package @p@.
siblingKey :: Maybe MVar -> MVar -> MVar
siblingKey pkgKey (MV n) = case pkgKey of
  Just (MV p) -> MV (p <> "." <> lastComponent n)
  Nothing -> MV ("." <> lastComponent n)

lastComponent :: Text -> Text
lastComponent t = case T.split (\c -> c == '.' || c == '/') t of
  [] -> t
  parts -> last parts

renameModE :: MVar -> ExprI -> ExprI
renameModE k (ExprI i (ModE _ es)) = ExprI i (ModE k es)
renameModE _ e = e

-- | In the entry file, the module no sibling imports is the program; when
-- several are imported by none, the one named @main@ is. Every other module
-- is a local module, imported as @.name@ (MOD-1); one the program does not
-- reach is dropped, as in an imported file.
keyEntrySiblings :: DAG MVar Import ExprI -> Either MDoc (DAG MVar Import ExprI)
keyEntrySiblings dag
  | Map.size dag < 2 = Right dag
  | otherwise =
      let entries = Map.toList dag
          local (MV n) = MV ("." <> lastComponent n)
          imported = Set.fromList [v | (_, (_, edges)) <- entries, (v, _) <- edges]
          roots = [n | (n, _) <- entries, not (Set.member (local n) imported)]
          rootOf = case roots of
            [r] -> Just r
            _ | MV "main" `elem` roots -> Just (MV "main")
            _ -> Nothing
       in case rootOf of
            Just rootName ->
              let rekeyed =
                    Map.fromList
                      [ if n == rootName then (n, v) else (local n, (renameModE (local n) e, edges))
                      | (n, v@(e, edges)) <- entries
                      ]
                  reach seen [] = seen
                  reach seen (k : ks)
                    | Set.member k seen = reach seen ks
                    | otherwise = reach (Set.insert k seen) (ks <> maybe [] (map fst . snd) (Map.lookup k rekeyed))
                  reached = reach Set.empty [rootName]
               in Right (Map.filterWithKey (\k _ -> Set.member k reached) rekeyed)
            Nothing ->
              Left $
                "The entry file declares several modules that no other module of it imports;"
                  <+> "name the program's module" <+> squotes "main" <> ", and import the others as"
                  <+> squotes ".name" <> ":" <+> hsep (punctuate "," (map pretty roots))

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  MM.modify' (\st -> st {stateSourceText = Map.insert filename code (stateSourceText st)})
  return (Just filename, code)

{- | Build a map from language aliases to Lang values, combining the
registry (built-in languages) with filesystem-discovered plugins.
-}
buildLangMap' :: MorlocMonad (Map.Map T.Text Lang)
buildLangMap' = do
  -- Get the registry-based lang map (all built-in languages)
  reg <- MM.gets stateLangRegistry
  let registryMap = LR.buildLangMap reg

  -- Discover additional plugin languages on the filesystem
  home <- MM.asks configHome
  let langDir = home </> "lang"
  exists <- liftIO $ doesDirectoryExist langDir
  pluginMap <-
    if not exists
      then return Map.empty
      else do
        dirs <- liftIO $ listDirectory langDir
        results <- liftIO $ mapM (scanLangDir langDir) dirs
        return $ Map.fromList [(n, lang) | Just (n, lang) <- results]

  -- Registry entries take precedence over filesystem discoveries
  return $ Map.union registryMap pluginMap
  where
    scanLangDir :: FilePath -> String -> IO (Maybe (T.Text, Lang))
    scanLangDir langDir dirName = do
      let descPath = langDir </> dirName </> "lang.yaml"
      hasDesc <- doesFileExist descPath
      if not hasDesc
        then return Nothing
        else do
          result <- LR.parseLangYamlFile descPath
          case result of
            Left _ -> return Nothing
            Right (name, ext) -> return $ Just (name, Lang name ext)
