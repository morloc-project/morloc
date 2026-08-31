{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Namespace.State
Description : Compiler state, monad stack, config, errors
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The compiler monad ('MorlocMonad') and its components:

* 'Config' -- read-only configuration loaded from @~\/.local\/share\/morloc\/config@
* 'MorlocError' -- all compiler error types
* 'MorlocState' -- mutable state threading type info, sources, and metadata
  through the pipeline
* 'Gamma' \/ 'GammaIndex' -- typechecking context (ordered list of assumptions)
* 'Script' -- a generated pool file with its build commands
-}
module Morloc.Namespace.State
  ( -- * Morloc monad
    MorlocMonadGen
  , MorlocMonad
  , MorlocReturn
  , MorlocState (..)
  , WrapperSpec (..)
  , WrapperMode (..)
  , WrapperFile (..)
  , SignatureSet (..)
  , Instance (..)
  , TermTypes (..)

    -- * Error handling
  , MorlocError (..)

    -- * Configuration
  , Config (..)

    -- * Package metadata
  , PackageMeta (..)
  , DepSpec (..)
  , RegDep (..)
  , LocalDep (..)
  , DepSource (..)
  , LangDepPolicy (..)
  , depPolicy
  , knownDepLangs
  , renderDepCapabilities
  , depSourceText
  , dsChannel
  , dsSource
  , regOfSource
  , defaultDepSource
  , effectiveDepSource
  , condaForgeChannel
  , checkPackageDeps
  , ExposeSet (..)

    -- * Typechecking
  , Gamma (..)
  , GammaIndex (..)
  , ConstVal (..)
  , NumLitKind (..)

    -- * Data files
  , NexusSource (..)

    -- * Sockets
  , Socket (..)

    -- * System
  , SysCommand (..)
  , Script (..)

    -- * Language registry
  , LangRegistry (..)
  , LangRegistryEntry (..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Aeson (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.Data.Doc
import Morloc.LangRegistry (LangRegistry (..), LangRegistryEntry (..))
import qualified Morloc.LangRegistry as LR
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.Type

---- Monad types

{- | The general monad transformer stack: Reader for config, Except for errors,
Writer for log messages, State for mutable compiler state, over IO.
-}
type MorlocMonadGen c e l s a =
  ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

-- | The full result of running a MorlocMonad computation
type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)

-- | The concrete compiler monad used throughout the pipeline
type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

---- State

-- | The morloc-nexus run mode a generated launcher wrapper selects.
-- (MCP is served from the compiled program directly via `morloc-nexus mcp
-- <manifest>` and needs no dedicated launcher; see `morloc mcp`.)
data WrapperMode = WCli | WDaemon
  deriving (Show, Eq, Ord)

-- | A launcher wrapper to emit: which nexus mode and the executable name.
data WrapperSpec = WrapperSpec
  { wsMode :: WrapperMode
  , wsName :: String
  }
  deriving (Show, Eq, Ord)

-- | A launcher wrapper file ready to write to disk: the absolute target
-- path and the shell-script body.
data WrapperFile = WrapperFile
  { wfPath :: FilePath
  , wfBody :: Text
  }
  deriving (Show, Eq)

{- | Mutable compiler state threaded through the entire pipeline.
Accumulates type signatures, source bindings, typedefs, and metadata
as modules are parsed, linked, and typechecked.
-}
data MorlocState = MorlocState
  { statePackageMeta :: [PackageMeta]
  , stateVerbosity :: Int
  , stateCounter :: Int
  , stateDepth :: Int
  , stateSignatures :: GMap Int Int SignatureSet
  , stateTypeclasses :: Map.Map EVar Instance
  , stateConcreteTypedefs :: GMap Int MVar (Map Lang Scope)
  , stateGeneralTypedefs :: GMap Int MVar Scope
  , stateUniversalGeneralTypedefs :: Scope
  , stateUniversalConcreteTypedefs :: Map Lang Scope
  , stateSources :: GMap Int MVar [Source]
  , stateAnnotations :: Map Int TypeU
  , stateOutfile :: Maybe Path
  , stateExports :: [Int]
  , stateName :: Map Int EVar
  , stateTermDocs :: Map.Map EVar [Text]
  -- ^ Declaration-level docstrings keyed by term name. Takes precedence over
  -- signature docstrings for the command-level description.
  , stateManifoldConfig :: Map Int ManifoldConfig
  , stateLogTemplate :: Maybe LogTemplate
  -- ^ Program-wide log message template from the main module's YAML
  -- @log-template@ field. Per-label overrides live in 'ManifoldConfig';
  -- the resolution order is per-label > this field > built-in default.
  , stateRunLog :: Maybe RunLogTemplate
  -- ^ Run-scope log templates from the main module's YAML @prologue@
  -- and @epilogue@ fields. The nexus renders @prologue@ at run start
  -- and the matching @epilogue.ok@ / @epilogue.fail@ at run end.
  , stateHashIncludePaths :: [Path]
  -- ^ Resolved (glob-expanded, scope-validated) list of files whose
  -- contents are folded into every pool's cache hash. Sourced from the
  -- main YAML's top-level @hash-include@ list. Paths are
  -- lexicographically sorted for deterministic hashing across runs.
  -- Empty (default) when no @hash-include@ is set.
  , stateSourceMap :: Map Int SrcLoc
  , stateSourceText :: Map Path Text
  , stateBuildConfig :: BuildConfig
  , stateLangParams :: Map Text (Map Text Text)
  -- ^ Resolved per-language build parameters, keyed lang -> key -> value,
  -- from @-X lang:key=value@ overlaid on the build config's @lang-params@.
  -- The morloc UI does not interpret keys/values; each language's builder
  -- reads the entries it understands. See "Morloc.Build.Params".
  , stateModuleName :: Maybe MVar
  , stateInstall :: Bool
  , stateInstallForce :: Bool
  , stateInstallDir :: Maybe Path
  -- ^ The build dir @<root>/<key>-build@ (where manifest.json + pools land),
  -- for both make and install. See 'stateBuildRoot' for the root.
  , stateBuildRoot :: Maybe Path
  -- ^ The source/install ROOT: the working directory for make, or
  -- @exe/<key>@ (a mirror of the working directory) for install. Sources
  -- live here and a pool resolves them at @../../..@. For install this is
  -- the atomically-swapped, marker-owned unit; @stateInstallDir@ nests
  -- inside it. @buildDir = root </> (key <> "-build")@.
  , stateProgramKey :: Maybe String
  -- ^ Program identity for the build directory (@<key>-build@ / @exe/<key>@).
  -- Set to @--name@ if given, else the source-file basename. @Nothing@ falls
  -- back to the outfile/module name (this is how eval reuses its @--save@ name).
  , stateWrapperSpecs :: Maybe [WrapperSpec]
  -- ^ Exact set of launcher wrappers to emit. @Nothing@ = default (a single
  -- CLI wrapper named after the program); @Just []@ = emit none (@--no-cli@
  -- with no other output flags).
  , stateBuildParentDir :: Maybe Path
  -- ^ Parent directory for the @<key>-build@ folder (@--build-dir@).
  -- @Nothing@ = current working directory.
  , stateClassDefs :: Map ClassName [Constraint]
  , stateEffects :: Map.Map EffectLabel Bool
  -- ^ Declared effects: label -> isEscapable (True = escapable). The
  -- compiler hardcodes none; all entries come from `effect` /
  -- `escapable effect` declarations.
  , stateLangRegistry :: LangRegistry
  , stateExportGroups :: Map Text ([Text], [Int])
  -- ^ Map from group name to (description lines, member export indices)
  , stateManifoldLang :: Map Int Lang
  -- ^ Map from export manifold ID to its pool language
  , stateManifoldEffects :: Map Int (Set.Set EffectLabel)
  -- ^ Map from export manifold ID to its original return effect labels
  , stateProjectRoot :: Maybe Path
  -- ^ Project root directory (directory of the entry-point file)
  , stateEnvSpecLangs :: [Text]
  -- ^ The languages the built program's pools use (from the emitted
  -- envspec.json). Set during nexus generation; read by the build's
  -- environment-provisioning hook so a program that USES a language triggers
  -- on-demand provisioning even when it declares no package dependencies.
  , stateEvalMode :: Bool
  -- ^ True when running in eval mode (restricts source/class/instance)
  , stateAllowLocalModules :: Bool
  -- ^ When False, import resolution ignores local/project-relative
  -- modules and resolves only installed (system) modules. False in
  -- eval mode (the API sandbox boundary) unless --allow-local-modules.
  , stateAutoInstall :: Bool
  -- ^ When True, a missing bare/namespaced module dependency is auto-downloaded
  -- during import resolution. Enabled only by `morloc make` (and disabled there
  -- by `--offline`); left False for read-only commands (typecheck, dump) and for
  -- eval/served mode, so those never trigger network installs.
  , stateSnapshot :: Map.Map Text Text
  -- ^ Module-pin snapshot: module name -> exact git hash, merged from the env's
  -- snapshot directory. Consulted AUTHORITATIVELY during on-demand module
  -- resolution (a snapshot pin overrides package pins). Loaded lazily on the
  -- make/install path; empty otherwise (read-only commands, no env).
  , stateEvalSandbox :: Maybe (Set.Set MVar)
  -- ^ Nothing = trusted eval (dev CLI): no extra gates. Just mods =
  -- sandboxed eval (served): only these modules may be imported at the
  -- top level of the eval expression, and IO intrinsics may not be
  -- written directly. Set only at the single arbitrary-source entry
  -- (cmdEval) from an explicit value -- never a defaulted flag a new
  -- eval call site could silently skip.
  , stateUnsafeSkipNullCheck :: Bool
  -- ^ True when @morloc make --unsafe-skip-null-check@ was given. The
  -- emitted manifest's top-level @unsafe_skip_null_check@ flag is set
  -- from this; the runtime then skips the boundary NUL scan.
  , stateInlineSize :: Maybe Int64
  -- ^ Inline-vs-route threshold, in bytes, from @morloc make
  -- --inline-size@. @Nothing@ = use the libmorloc default; emitted as
  -- the manifest @inline_size@ field, which the nexus forwards to
  -- libmorloc via env var and FFI setter.
  , stateNoShm :: Bool
  -- ^ True when @morloc make --no-shm@ was given. Disables shared
  -- memory at the runtime; data over the inline threshold is written
  -- to a temp file and passed by path.
  , stateTmpdir :: Maybe Path
  -- ^ Directory for transient data files written by the @--no-shm@
  -- routing path, from @morloc make --tmpdir@. @Nothing@ = use the
  -- libmorloc default (@$TMPDIR@ or @/tmp@); emitted as the manifest
  -- @tmpdir@ field, forwarded by the nexus via @MORLOC_TMPDIR@.
  , stateDebugTrace :: Bool
  -- ^ True when @morloc make --debug@ was given. Causes Express to
  -- wrap every foreign-call manifold body with a 'PolyDebugWrap'
  -- node, which codegen lowers to per-language try/catch that dumps
  -- the manifold's args via @morloc_debug_record_frame@ on
  -- exception. Zero cost on the happy path.
  , stateModuleDoc :: [Text]
  -- ^ Module-level description lines (from docstrings before module declaration)
  , stateModuleEpilogues :: [[Text]]
  -- ^ Epilogue blocks for the top-level help output
  , stateSerialAncestors :: Set.Set TVar
  -- ^ General-type names of records currently being lowered by
  -- 'makeSerialAST''. Used to detect guarded self-recursive records:
  -- on the way down we insert the FVar's general name; if we hit it
  -- again, we emit 'SerialRec' instead of expanding the cycle. The
  -- field is reset to empty at every top-level 'makeSerialAST' call
  -- and saved/restored around each NamF descent, so it never leaks
  -- across unrelated invocations.
  }
  deriving (Show)

data SignatureSet
  = Monomorphic TermTypes
  | Polymorphic
      ClassName
      EVar
      EType
      [TermTypes]
  deriving (Show)

data Instance = Instance
  { className :: ClassName
  , classVars :: [TVar]
  , classType :: EType
  , instanceTerms :: [TermTypes]
  }
  deriving (Show, Ord, Eq)

data TermTypes = TermTypes
  { termGeneral :: Maybe EType
  , termConcrete :: [(MVar, Indexed Source)]
  , termDecl :: [ExprI]
  }
  deriving (Show, Ord, Eq)

---- Error types

-- | All compiler errors
data MorlocError
  = -- | Error tied to a specific AST node index
    SourcedError Int MDoc
  | -- | Internal compiler error (bug)
    SystemError MDoc
  | -- | Type unification failure
    UnificationError Int Int Int MDoc
  deriving (Show)

---- Configuration

-- | Read-only configuration loaded from the morloc config file
data Config
  = Config
  { configHome :: !Path
  -- | Root for MUTABLE per-environment STATE (built programs, module db,
  -- installed module code, plane source) -- distinct from the immutable
  -- runtime under 'configHome' (bin/lib/include). Set from $MORLOC_STATE;
  -- defaults to 'configHome' so a plain host install stays single-directory.
  -- The split lets a container mount only the state root without shadowing the
  -- image-baked runtime.
  , configState :: !Path
  , configLibrary :: !Path
  , configPlane :: !Path
  , configPlaneCore :: !Path
  , configTmpDir :: !Path
  , configBuildConfig :: !Path
  , configLangOverrides :: !(Map Text [Text])
  , configRegistry :: !(Maybe Text)
  }
  deriving (Show, Ord, Eq)

---- Package metadata

-- | The ecosystem (package database / index) a dependency is drawn from. This
-- names WHERE a package comes from, never the tool that fetches it: 'SrcConda'
-- (the conda package database), 'SrcPypi' (PyPI), 'SrcCran'/'SrcBioconductor'
-- (R registries), 'SrcCrates' (crates.io), 'SrcPkg' (Julia's General registry).
-- A module declaring its sources is self-describing: the meaning of a name does
-- not depend on which channels the builder happens to provide.
data DepSource
  = SrcConda
  | SrcPypi
  | SrcCran
  | SrcBioconductor
  | SrcCrates
  | SrcPkg
  deriving (Show, Eq, Ord)

-- | Which registry a dependency is drawn from, carrying only the fields that are
-- valid for that registry so illegal combinations are unrepresentable. A channel
-- (the package DATABASE within conda, e.g. bioconda) lives ONLY in 'RegConda', so
-- a channel can never accompany a non-conda source. 'RegDefault' is the
-- source-omitted form (a bare version string): it resolves to the per-language
-- default ('defaultDepSource') later, when the language is known.
data RegDep
  = RegConda !(Maybe Text)   -- optional channel; Nothing means conda-forge
  | RegPypi
  | RegCrates
  | RegPkg
  | RegCran
  | RegBioconductor
  | RegDefault
  deriving (Show, Eq, Ord)

-- | One declared registry dependency: which registry (with its conda channel, if
-- any) and a version constraint. Local (filesystem-path) dependencies are NOT
-- registry deps; they are declared separately (see 'LocalDep',
-- 'packageLocalDeps') so this type stays a tight registry-only sum.
data DepSpec = DepSpec
  { dsReg :: !RegDep
  , dsVersion :: !Text
  }
  deriving (Show, Eq, Ord)

-- | The conda channel of a dependency (conda-only; 'Nothing' for any other
-- registry). Derived from 'dsReg' so channel-without-conda is unrepresentable.
dsChannel :: DepSpec -> Maybe Text
dsChannel ds = case dsReg ds of
  RegConda mch -> mch
  _ -> Nothing

-- | The declared source of a dependency, or 'Nothing' when omitted ('RegDefault',
-- resolved to the language default by 'effectiveDepSource').
dsSource :: DepSpec -> Maybe DepSource
dsSource ds = case dsReg ds of
  RegConda _ -> Just SrcConda
  RegPypi -> Just SrcPypi
  RegCrates -> Just SrcCrates
  RegPkg -> Just SrcPkg
  RegCran -> Just SrcCran
  RegBioconductor -> Just SrcBioconductor
  RegDefault -> Nothing

-- | One declared local (filesystem-path) dependency. 'ldPath' is relative to the
-- module root (no @..@, no absolute; an in-project symlink is the escape hatch to
-- an external source). 'ldEditable' is an INTENT: editable in interactive
-- contexts (native, dev run), a plain snapshot when serving or frozen.
data LocalDep = LocalDep
  { ldPath :: !Text
  , ldEditable :: !Bool
  }
  deriving (Show, Eq, Ord)

-- | Parse a source name. The vocabulary is the set of package databases morloc
-- understands; anything else is a hard parse error.
parseDepSource :: Text -> Maybe DepSource
parseDepSource "conda" = Just SrcConda
parseDepSource "pypi" = Just SrcPypi
parseDepSource "cran" = Just SrcCran
parseDepSource "bioconductor" = Just SrcBioconductor
parseDepSource "crates" = Just SrcCrates
parseDepSource "pkg" = Just SrcPkg
parseDepSource _ = Nothing

-- | Canonical name for a source (the inverse of 'parseDepSource'): the string
-- written in @package.yaml@ and emitted on the envspec wire.
depSourceText :: DepSource -> Text
depSourceText SrcConda = "conda"
depSourceText SrcPypi = "pypi"
depSourceText SrcCran = "cran"
depSourceText SrcBioconductor = "bioconductor"
depSourceText SrcCrates = "crates"
depSourceText SrcPkg = "pkg"

-- | A language's dependency-source policy: the sources honored today, sources
-- recognized but not yet wired up, and the source assumed when the author omits
-- one ('Nothing' means a source is mandatory). One table drives validation and
-- defaulting so the two cannot drift; by construction 'ldpDefault' is either
-- 'Nothing' or one of 'ldpHonored'.
data LangDepPolicy = LangDepPolicy
  { ldpHonored :: ![DepSource]
  , ldpUnsupported :: ![DepSource]
  , ldpDefault :: !(Maybe DepSource)
  -- | Whether this language supports tracked local (filesystem-path)
  -- dependencies. The single source of truth: both 'checkPackageDeps' (the gate)
  -- and any advertised capability table read this, so they cannot drift.
  , ldpLocalInstall :: !Bool
  }

depPolicy :: Text -> LangDepPolicy
depPolicy "py" = LangDepPolicy [SrcConda, SrcPypi] [] Nothing True
depPolicy "r" = LangDepPolicy [SrcConda] [SrcCran, SrcBioconductor] (Just SrcConda) False
depPolicy "cpp" = LangDepPolicy [SrcConda] [] (Just SrcConda) False
depPolicy "rust" = LangDepPolicy [SrcCrates] [] (Just SrcCrates) True
depPolicy "julia" = LangDepPolicy [SrcPkg] [] (Just SrcPkg) False
depPolicy _ = LangDepPolicy [SrcConda] [] (Just SrcConda) False

-- | The source assumed for a language's dependency when none is declared.
defaultDepSource :: Text -> Maybe DepSource
defaultDepSource = ldpDefault . depPolicy

-- | The languages with an explicit dependency policy, in display order. The
-- capability table ('renderDepCapabilities') iterates these.
knownDepLangs :: [Text]
knownDepLangs = ["py", "r", "cpp", "rust", "julia"]

-- | Render the per-language dependency-capability table as plain lines, DERIVED
-- entirely from 'depPolicy' (the single source of truth) so an advertised table
-- can never disagree with the actual validation gate. Each row lists the honored
-- registry sources and whether local (filesystem-path) deps are supported.
renderDepCapabilities :: Text
renderDepCapabilities = T.unlines (header : map row knownDepLangs)
  where
    header = "language | registries | local-path"
    row lang =
      let pol = depPolicy lang
          regs = case map depSourceText (ldpHonored pol) of
                   [] -> "-"
                   ss -> T.intercalate ", " ss
          local = if ldpLocalInstall pol then "yes" else "no"
       in lang <> " | " <> regs <> " | " <> local

-- | The source a dependency resolves to: its declared source, else the language
-- default. (A bare @{channel: ...}@ already parses to 'RegConda', so channels no
-- longer need to imply conda here.) 'Nothing' means unresolvable (a Python dep
-- with no source), which 'checkPackageDeps' rejects. This is the single source of
-- truth shared by validation and envspec emission so the two cannot drift.
effectiveDepSource :: Text -> DepSpec -> Maybe DepSource
effectiveDepSource lang ds = dsSource ds <|> defaultDepSource lang

-- | The universal default conda channel and highest strict-priority base. An
-- omitted channel means this; it is stripped from the envspec wire so a
-- channel-less program stays byte-identical to the pre-channel schema.
condaForgeChannel :: Text
condaForgeChannel = "conda-forge"

instance FromJSON DepSource where
  parseJSON = Aeson.withText "source" $ \t ->
    case parseDepSource t of
      Just s -> return s
      Nothing ->
        fail $
          "unknown dependency source (expected one of: conda, pypi, cran, \
          \bioconductor, crates, pkg)"

-- A registry dependency value is either a bare version string (source defaulted)
-- or an object {version, source, channel}. The bare form keeps the common case
-- terse. Illegal combinations are rejected here so they are unrepresentable
-- downstream: a channel may accompany only conda (or an omitted source, which a
-- channel pins to conda).
instance FromJSON DepSpec where
  parseJSON (Aeson.String s) = return (DepSpec RegDefault s)
  parseJSON (Aeson.Object o) = do
    version <- o .:? "version" .!= "*"
    msrc <- o .:? "source"
    mchan <- o .:? "channel"
    reg <- case (msrc, mchan) of
      (Nothing, Nothing) -> return RegDefault
      (Nothing, Just ch) -> return (RegConda (Just ch)) -- a channel pins conda
      (Just SrcConda, mch) -> return (RegConda mch)
      (Just s, Nothing) -> return (regOfSource s)
      (Just s, Just _) ->
        fail $
          "dependency declares channel with source '" <> T.unpack (depSourceText s)
            <> "', but channels apply only to conda dependencies"
    return (DepSpec reg version)
  parseJSON _ =
    fail "dependency must be a version string or an object {version, source, channel}"

-- | The 'RegDep' for a non-conda source (conda is handled separately so its
-- channel is captured). Total over the non-conda 'DepSource' constructors.
regOfSource :: DepSource -> RegDep
regOfSource SrcConda = RegConda Nothing
regOfSource SrcPypi = RegPypi
regOfSource SrcCrates = RegCrates
regOfSource SrcPkg = RegPkg
regOfSource SrcCran = RegCran
regOfSource SrcBioconductor = RegBioconductor

-- A local dependency value is an object {path, editable?}. 'path' is required;
-- 'editable' defaults to False.
instance FromJSON LocalDep where
  parseJSON = Aeson.withObject "local dependency" $ \o ->
    LocalDep <$> o .: "path" <*> o .:? "editable" .!= False

-- | Validate a module's declared dependencies against the per-language source
-- policy. Returns a human-readable error on the first violation. This is the
-- authoritative gate; it is language-aware (each @*-deps@ block knows its
-- language) in a way the source-agnostic 'FromJSON DepSpec' cannot be.
--
--   * Python: @source@ is mandatory and must be @conda@ or @pypi@.
--   * R: defaults to conda (the @r-@ feedstock); @cran@/@bioconductor@ are not
--     yet supported (the R pool installs only through conda).
--   * C++: conda only.
--   * Rust: crates.io only.  * Julia: the General registry only.
--   * @channel@ is conda-only: it may not accompany a non-conda source, and a
--     conda-forge R name must be a bare CRAN name (no @r-@ prefix).
checkPackageDeps :: PackageMeta -> Either Text ()
checkPackageDeps pm =
  mapM_ checkGroup groups >> mapM_ checkLocalGroup (Map.toList (packageLocalDeps pm))
  where
    groups =
      [ ("py", packagePyDeps pm)
      , ("r", packageRDeps pm)
      , ("cpp", packageCppDeps pm)
      , ("rust", packageRustDeps pm)
      , ("julia", packageJuliaDeps pm)
      ]

    checkGroup (lang, m) = mapM_ (checkDep lang) (Map.toList m)

    checkDep lang (name, ds) = do
      _ <- resolveSource lang name ds
      -- A channel-on-non-conda contradiction is now unrepresentable (parsed into
      -- 'RegConda' or rejected in 'FromJSON DepSpec'); only the R feedstock naming
      -- rule remains a semantic check.
      checkRName lang name ds

    -- Validate the dependency's resolved source (see 'effectiveDepSource', the
    -- shared resolution rule). Returns the validated source or an error.
    resolveSource lang name ds =
      let pol = depPolicy lang
       in case effectiveDepSource lang ds of
            Nothing ->
              Left $
                "dependency '" <> name <> "' must declare a source for " <> lang
                  <> " (one of: " <> validList pol <> ")"
            Just s
              | s `elem` ldpHonored pol -> Right s
              | s `elem` ldpUnsupported pol ->
                  Left $
                    depSourceText s <> " dependencies are not yet supported for " <> lang
                      <> " (package '" <> name <> "'); use conda"
              | otherwise ->
                  Left $
                    "dependency '" <> name <> "' declares source " <> depSourceText s
                      <> ", which is not valid for " <> lang
                      <> " (expected: " <> validList pol <> ")"

    checkRName lang name ds
      | lang == "r"
      , channelIsCondaForge (dsChannel ds)
      , T.isPrefixOf "r-" (T.toLower name) =
          Left $
            "dependency '" <> name <> "' names a conda-forge R feedstock; r-deps "
              <> "names are bare CRAN names (drop the 'r-' prefix)"
      | otherwise = Right ()

    channelIsCondaForge = maybe True (== condaForgeChannel)

    validList pol = T.intercalate ", " (map depSourceText (ldpHonored pol))

    -- Local (filesystem-path) deps: the language must support tracked local
    -- installs, and each path must be module-relative (no absolute, no '..'; an
    -- in-project symlink is the escape hatch to an external source).
    checkLocalGroup (lang, m)
      | ldpLocalInstall (depPolicy lang) = mapM_ (checkLocalDep lang) (Map.toList m)
      | otherwise = Left (localUnsupportedMsg lang (Map.keys m))

    checkLocalDep lang (name, ld) = checkLocalPath lang name (ldPath ld)

    checkLocalPath lang name p
      | "/" `T.isPrefixOf` p =
          Left $ localPathErr lang name p "must be relative to the module root (not absolute)"
      | any (== "..") (T.splitOn "/" p) =
          Left $ localPathErr lang name p "must not contain '..' (use an in-project symlink for an external source)"
      | T.null p = Left $ localPathErr lang name p "must not be empty"
      | otherwise = Right ()

    localPathErr lang name p why =
      "local dependency '" <> name <> "' for " <> lang <> " has path '" <> p <> "' that " <> why

    localUnsupportedMsg lang names =
      let pkgs = T.intercalate ", " names
       in case lang of
            "cpp" ->
              "local dependencies are not supported for cpp (packages: " <> pkgs
                <> "); C++ has no package-manager-tracked local deps. Use cxx-flags "
                <> "for ad-hoc (untracked) linkage of a locally built library."
            _ ->
              "local dependencies are not supported for " <> lang
                <> " (packages: " <> pkgs <> "); supported for: py, rust."

data PackageMeta
  = PackageMeta
  { packageName :: !Text
  , packageVersion :: !Text
  , packageHomepage :: !Text
  , packageSynopsis :: !Text
  , packageDescription :: !Text
  , packageCategory :: !Text
  , packageLicense :: !Text
  , packageAuthor :: !Text
  , packageMaintainer :: !Text
  , packageGithub :: !Text
  , packageBugReports :: !Text
  , packageCppVersion :: !Int
  , packageDependencies :: [Text]
  -- | Extra flags appended to the C++ pool compile line (e.g. -O3,
  -- -march=native, -DXYZ). Propagates transitively through dependencies.
  , packageCxxFlags :: [Text]
  -- | External Rust crates (crate name -> 'DepSpec') a module needs available
  -- to the Rust pool. The DAG-wide union is written into the generated pool
  -- @Cargo.toml@ dependencies. Propagates transitively.
  , packageRustDeps :: Map Text DepSpec
  -- | External Python packages (name -> 'DepSpec') needed by the Python pool.
  -- The DAG-wide union flows into the generated EnvSpec. Propagates
  -- transitively.
  , packagePyDeps :: Map Text DepSpec
  -- | External R packages (name -> 'DepSpec') needed by the R pool.
  , packageRDeps :: Map Text DepSpec
  -- | External C++ libraries (name -> 'DepSpec') needed by the C++ pool; the
  -- structured, version-managed successor to 'packageDependencies'.
  , packageCppDeps :: Map Text DepSpec
  -- | External Julia packages (name -> 'DepSpec') needed by the Julia pool
  -- (resolved by Pkg.jl).
  , packageJuliaDeps :: Map Text DepSpec
  -- | Local (filesystem-path) dependencies, grouped by language
  -- (lang -> name -> 'LocalDep'). Non-portable until frozen; honored only from
  -- the root/entry module. Supported languages: py, rust (see 'ldpLocalInstall').
  , packageLocalDeps :: Map Text (Map Text LocalDep)
  -- | Optional per-language toolchain version constraints
  -- (lang name -> constraint, e.g. "python" -> ">=3.10"), merged into the
  -- EnvSpec language list.
  , packageLangVersions :: Map Text Text
  , packageInclude :: Maybe [Text]
  -- | Optional constraint on the morloc COMPILER version this module supports,
  -- in the same conda match-spec grammar as language/package versions: e.g.
  -- ">=0.98, <0.99" (an interval), "0.98" (the 0.98.x series), or "*". 'Nothing'
  -- means unconstrained. Checked at install and make.
  , packageMorlocVersion :: !(Maybe Text)
  -- | Pinned morloc module dependencies (name, git commit hash). Optional;
  -- empty = unpinned, install latest. See plan: closer-to-install-root wins.
  , packageMorlocDependencies :: [(Text, Text)]
  -- | Optional path (relative to the module root) to a shell script that
  -- runs once during `morloc install`, after the source is on disk and
  -- after morloc deps are installed. Non-zero exit fails the install.
  , packageSetup :: !(Maybe FilePath)
  -- | Per-language lists of paths (relative to module root, glob patterns
  -- allowed) to copy on install into per-language well-known dirs under
  -- $MORLOC_HOME so downstream foreign code can reference them by a
  -- stable, module-namespaced path.
  , packageExpose :: !ExposeSet
  }
  deriving (Show, Ord, Eq)

-- | Per-language exposure lists. Each list holds paths or glob patterns
-- relative to the module root. The install pipeline copies the matched
-- files (preserving subtree under the module root) into
-- $MORLOC_HOME/include/<module>/        (cpp)
-- $MORLOC_HOME/lib/python/<py_module>/  (py, hyphens in module name -> _)
-- $MORLOC_HOME/lib/R/<module>/          (r)
data ExposeSet
  = ExposeSet
  { exposeCpp :: ![Text]
  , exposePy  :: ![Text]
  , exposeR   :: ![Text]
  }
  deriving (Show, Ord, Eq)

---- Typechecking context

{- | Entries in the typechecking context (an ordered list of assumptions).
The context is manipulated as a stack during bidirectional typechecking.
-}
data GammaIndex
  = VarG TVar
  | AnnG EVar TypeU
  | ExistG
      TVar
      ([TypeU], OpenOrClosed)
      ([(Key, TypeU)], OpenOrClosed)
  | SolvedG TVar TypeU
  | MarkG TVar
  | SrcG Source
  deriving (Ord, Eq, Show)

{- | Typechecking context using IntMap for O(log N) operations.
Entries are keyed by monotonically increasing slot numbers (higher = newer).
Side-indexes provide O(log N) lookup of ExistG entries by TVar.
-}
data Gamma = Gamma
  { gammaCounter :: !Int
  -- | Next available slot number (always increasing)
  , gammaSlot :: !Int
  -- | Ordered context: higher slot = newer entry
  , gammaContext :: IntMap.IntMap GammaIndex
  -- | Index: ExistG TVar -> slot number (for O(log N) access1)
  , gammaExist :: Map TVar Int
  -- | Cache of solved existential types
  , gammaSolved :: Map TVar TypeU
  -- | Nat constraints that could not be solved (deferred)
  , gammaDeferred :: [(TypeU, TypeU)]
  -- | Solutions for kind-tagged variables (KVarU / NatVarU / StrVarU /
  -- RecVarU / ListVarU / SetVarU) from the per-kind constraint solvers.
  -- Keyed on (TVar, Kind) so all five previously-parallel gammaXSubs
  -- maps share a single storage. Str-solver semantics are literals-only
  -- (see plans/tables/04-str-solver-scope.md); Rec-solver decidability
  -- notes in plans/tables/10-rec-solver-decidability.md.
  , gammaKindSubs :: Map (TVar, Kind) TypeU
  -- | Solutions for effect-row tail variables ('EffectVar') from
  -- effect-row unification. A separate namespace for the implicitly
  -- universally-quantified effect variables (values are 'EffectSet'
  -- rather than 'TypeU', so cannot share 'gammaKindSubs').
  , gammaEffSubs :: Map TVar EffectSet
  -- | Generic primitive constraints (Member / Subset / Disjoint) waiting for
  -- enough information to discharge. Stage 9 of the tables refactor. The
  -- 'Constraint' values carried here are the same as the ones the
  -- typeclass mechanism uses; primitive forms have CMember / CSubset /
  -- CDisjoint constructors.
  , gammaConstraints :: [Constraint]
  -- | Constraints declared on the *current* function's signature, taken as
  -- assumptions during this function's body typecheck. A deferred
  -- obligation in @gammaConstraints@ that cannot be decided at end-of-
  -- typecheck is allowed to discharge if it is alpha-equivalent (after
  -- applying gamma) to one of these assumptions; otherwise it is an
  -- unsolved-constraint error.
  --
  -- @Nothing@ means "no signature has been claimed yet"; the next
  -- @VarS (MonomorphicExpr (Just _) _)@ handled by 'synthE' will lock
  -- in its declared @econs@ as the assumption set. @Just@ (even @Just
  -- []@) means the slot is taken - subsequent inner VarS calls add
  -- only to @gammaConstraints@, never to assumptions. The Maybe
  -- distinguishes "outermost-not-seen-yet" from "outermost-seen-with-
  -- empty-econs", which a plain list cannot.
  , gammaAssumedConstraints :: Maybe [Constraint]
  -- | Known constant values for let-bound variables (for nat label resolution).
  -- Tracks integers, tuples, and records so accessors like .0 can be evaluated.
  , gammaIntVals :: Map EVar ConstVal
  -- | Numeric literals whose type-checking was deferred because they were
  -- checked against an unsolved existential. Each entry is (caret index,
  -- existential TVar, default kind). At end-of-typecheck the queue is
  -- drained: if the existential is now solved to a compatible base type,
  -- accept; otherwise apply the default (@Int@ for @IntDefault@, @Real@
  -- for @RealDefault@). This is the @gammaDeferred@ pattern lifted to
  -- numeric-literal polymorphism in argument positions like
  -- @fold (+) 0 (xs :: Vector n Int8)@.
  , gammaPendingNumLits :: [(Int, TVar, NumLitKind)]
  -- | Reverse index of receiver existentials that carry positional
  -- (tuple) field-slot constraints through a solved structural alias
  -- (a @_pattern_@ getter or @set_slot_@ setter). Populated at the
  -- single 'cacheSolved' choke point. Grow-only and over-approximating:
  -- membership gates the O(|gammaSolved|) scan in
  -- 'accumulatedPositionalSets' so the common ground-solve, which has no
  -- positional constraint, pays only an O(log n) lookup. Mirrors the
  -- cheap own-entry pre-check that 'accumulatedRecords' uses for records.
  , gammaPositionalReceivers :: Set.Set TVar
  }

-- | Compile-time constant values tracked during typechecking for nat / str
-- label resolution. Only pure literal expressions are tracked.
data ConstVal
  = ConstInt Integer
  | ConstStr Text
  | ConstTup [ConstVal]
  | ConstList [ConstVal]
  deriving (Show, Eq, Ord)

-- | Distinguishes integer-like literals (no decimal point, default @Int@)
-- from float-like literals (with decimal point, default @Real@). Used by
-- @gammaPendingNumLits@ so that an unresolved existential gets the
-- right default at end-of-typecheck.
data NumLitKind
  = IntDefault
  | RealDefault
  deriving (Show, Eq, Ord)

---- Data files and system

data NexusSource = NexusSource
  { nexusSourceUtility :: MDoc
  , nexusSourceMain :: MDoc
  }

data Socket = Socket
  { socketLang :: Lang
  , socketPath :: MDoc
  }
  deriving (Show)

data SysCommand
  = SysExe Path
  | SysMove Path Path
  | SysRun Code
  | SysInstall Path
  | SysUnlink Path
  deriving (Show, Ord, Eq)

data Script
  = Script
  { scriptBase :: !String
  , scriptLang :: !Lang
  , scriptCode :: !(AnchoredDirTree Code)
  , scriptMake :: ![SysCommand]
  }
  deriving (Show, Ord, Eq)

---- Instances

instance Defaultable MorlocState where
  defaultValue =
    MorlocState
      { statePackageMeta = []
      , stateVerbosity = 0
      , stateCounter = -1
      , stateDepth = 0
      , stateSignatures = GMap Map.empty Map.empty
      , stateTypeclasses = Map.empty
      , stateConcreteTypedefs = GMap Map.empty Map.empty
      , stateGeneralTypedefs = GMap Map.empty Map.empty
      , stateUniversalConcreteTypedefs = Map.empty
      , stateUniversalGeneralTypedefs = Map.empty
      , stateSources = GMap Map.empty Map.empty
      , stateAnnotations = Map.empty
      , stateOutfile = Nothing
      , stateExports = []
      , stateName = Map.empty
      , stateTermDocs = Map.empty
      , stateManifoldConfig = Map.empty
      , stateLogTemplate = Nothing
      , stateRunLog = Nothing
      , stateHashIncludePaths = []
      , stateSourceMap = Map.empty
      , stateSourceText = Map.empty
      , stateBuildConfig = defaultValue
      , stateLangParams = Map.empty
      , stateModuleName = Nothing
      , stateInstall = False
      , stateInstallForce = False
      , stateInstallDir = Nothing
      , stateBuildRoot = Nothing
      , stateProgramKey = Nothing
      , stateWrapperSpecs = Nothing
      , stateBuildParentDir = Nothing
      , stateClassDefs = Map.empty
      , stateEffects = Map.empty
      , stateLangRegistry = LR.emptyRegistry
      , stateExportGroups = Map.empty
      , stateManifoldLang = Map.empty
      , stateManifoldEffects = Map.empty
      , stateProjectRoot = Nothing
      , stateEnvSpecLangs = []
      , stateEvalMode = False
      , stateAllowLocalModules = True
      , stateAutoInstall = False
      , stateSnapshot = Map.empty
      , stateEvalSandbox = Nothing
      , stateUnsafeSkipNullCheck = False
      , stateInlineSize = Nothing
      , stateNoShm = False
      , stateTmpdir = Nothing
      , stateDebugTrace = False
      , stateModuleDoc = []
      , stateModuleEpilogues = []
      , stateSerialAncestors = Set.empty
      }

instance Defaultable PackageMeta where
  defaultValue =
    PackageMeta
      { packageName = ""
      , packageVersion = ""
      , packageHomepage = ""
      , packageSynopsis = ""
      , packageDescription = ""
      , packageCategory = ""
      , packageLicense = ""
      , packageAuthor = ""
      , packageMaintainer = ""
      , packageGithub = ""
      , packageBugReports = ""
      , packageCppVersion = 20
      , packageDependencies = []
      , packageCxxFlags = []
      , packageRustDeps = Map.empty
      , packagePyDeps = Map.empty
      , packageRDeps = Map.empty
      , packageCppDeps = Map.empty
      , packageJuliaDeps = Map.empty
      , packageLocalDeps = Map.empty
      , packageLangVersions = Map.empty
      , packageInclude = Nothing
      , packageMorlocVersion = Nothing
      , packageMorlocDependencies = []
      , packageSetup = Nothing
      , packageExpose = defaultValue
      }

instance Defaultable ExposeSet where
  defaultValue = ExposeSet [] [] []

instance FromJSON Config where
  parseJSON =
    Aeson.withObject "object" $ \o -> do
      home' <- o .:? "home" .!= "~/.local/share/morloc"
      source' <- o .:? "source" .!= "~/.local/share/morloc/src/morloc"
      plane' <- o .:? "plane" .!= "default"
      planeCore' <- o .:? "plane-core" .!= "morloclib"
      tmpdir' <- o .:? "tmpdir" .!= "~/.local/share/morloc/tmp"
      buildConfig' <- o .:? "build-config" .!= "~/.local/share/morloc/build-config.yaml"
      -- Parse legacy lang_python3/lang_R fields into langOverrides
      pyCmd <- o .:? "lang_python3" .!= ("" :: Text)
      rCmd <- o .:? "lang_R" .!= ("" :: Text)
      overrides <- o .:? "lang_overrides" .!= Map.empty
      registry' <- o .:? "registry"
      let legacyOverrides =
            Map.fromList $
              filter
                (not . null . snd)
                [ ("py", if pyCmd == "" then [] else [pyCmd])
                , ("r", if rCmd == "" then [] else [rCmd])
                ]
          allOverrides = Map.union overrides legacyOverrides
      -- configState defaults to home' here; the post-load resolver in
      -- Morloc.Config applies $MORLOC_STATE and re-derives configLibrary from it.
      return $ Config home' home' source' plane' planeCore' tmpdir' buildConfig' allOverrides registry'

instance FromJSON PackageMeta where
  parseJSON = Aeson.withObject "object" $ \o ->
    PackageMeta
      <$> o .:? "name" .!= ""
      <*> o .:? "version" .!= ""
      <*> o .:? "homepage" .!= ""
      <*> o .:? "synopsis" .!= ""
      <*> o .:? "description" .!= ""
      <*> o .:? "category" .!= ""
      <*> o .:? "license" .!= ""
      <*> o .:? "author" .!= ""
      <*> o .:? "maintainer" .!= ""
      <*> o .:? "github" .!= ""
      <*> o .:? "bug-reports" .!= ""
      <*> o .:? "cpp-version" .!= 0
      <*> o .:? "dependencies" .!= []
      <*> o .:? "cxx-flags" .!= []
      <*> o .:? "rust-deps" .!= Map.empty
      <*> o .:? "py-deps" .!= Map.empty
      <*> o .:? "r-deps" .!= Map.empty
      <*> o .:? "cpp-deps" .!= Map.empty
      <*> o .:? "julia-deps" .!= Map.empty
      <*> o .:? "local-deps" .!= Map.empty
      <*> o .:? "lang-versions" .!= Map.empty
      <*> o .:? "include"
      <*> o .:? "morloc-version"
      <*> parseMorlocDeps o
      <*> o .:? "setup"
      <*> o .:? "expose" .!= defaultValue
    where
      parseMorlocDep = Aeson.withObject "morloc-dependency" $ \od ->
        (,) <$> od Aeson..: "name" <*> od Aeson..: "git-hash"
      -- Accept the canonical `morloc-deps` key, falling back to the deprecated
      -- `morloc-dependencies` alias. `morloc-deps` wins when both are present.
      -- The deprecation warning is emitted at load time (see loadModuleMetadata).
      parseMorlocDeps obj = do
        primary <- obj .:? "morloc-deps"
        legacy  <- obj .:? "morloc-dependencies"
        mapM parseMorlocDep (maybe [] id (primary <|> legacy))

instance FromJSON ExposeSet where
  parseJSON = Aeson.withObject "expose" $ \o ->
    ExposeSet
      <$> o .:? "cpp" .!= []
      <*> o .:? "py"  .!= []
      <*> o .:? "r"   .!= []

----- Pretty instances -------------------------------------------------------

instance Pretty Instance where
  pretty (Instance cls vs et ts) =
    "Instance"
      <+> pretty cls
      <+> pretty vs
      <+> parens (pretty (etype et))
      <+> list (map pretty ts)

instance Pretty TermTypes where
  pretty (TermTypes (Just t) cs es) = "TermTypes" <+> (align . vsep $ (parens (pretty t) : map pretty cs <> map pretty es))
  pretty (TermTypes Nothing cs es) = "TermTypes" <+> "?" <> (align . vsep $ (map pretty cs <> map pretty es))

instance Pretty SignatureSet where
  pretty (Monomorphic t) = pretty t
  pretty (Polymorphic cls v t ts) =
    "class"
      <+> pretty cls
      <+> (align . vsep $ (pretty v <+> "::" <+> parens (pretty t)) : map pretty ts)

instance Pretty GammaIndex where
  pretty (VarG tv) = "VarG:" <+> pretty tv
  pretty (ExistG tv ([], _) ([], _)) = angles (pretty tv)
  pretty (ExistG tv (ts, _) (rs, _)) =
    "ExistG:"
      <+> pretty tv
      <+> list (map (parens . pretty) ts)
      <+> list (map ((\(x, y) -> tupled [x, y]) . bimap pretty pretty) rs)
  pretty (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> pretty t
  pretty (MarkG tv) = "MarkG:" <+> pretty tv
  pretty (SrcG (Source ev1 lang _ _ _ _ _ _ _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
  pretty (AnnG v t) = pretty v <+> "::" <+> pretty t
