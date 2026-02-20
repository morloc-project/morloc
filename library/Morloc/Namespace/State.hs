{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Namespace.State
Description : Compiler state, monad stack, config, errors
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Namespace.State
  ( -- * Morloc monad
    MorlocMonadGen
  , MorlocMonad
  , MorlocReturn
  , MorlocState (..)
  , SignatureSet (..)
  , Instance (..)
  , TermTypes (..)

    -- * Error handling
  , MorlocError (..)

    -- * Configuration
  , Config (..)

    -- * Package metadata
  , PackageMeta (..)

    -- * Typechecking
  , Gamma (..)
  , GammaIndex (..)

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

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Aeson (FromJSON (..), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Morloc.Data.Doc
import Morloc.LangRegistry (LangRegistry(..), LangRegistryEntry(..))
import qualified Morloc.LangRegistry as LR
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr

---- Monad types

type MorlocMonadGen c e l s a =
  ReaderT c (ExceptT e (WriterT l (StateT s IO))) a

type MorlocReturn a = ((Either MorlocError a, [Text]), MorlocState)

type MorlocMonad a = MorlocMonadGen Config MorlocError [Text] MorlocState a

---- State

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
  , stateManifoldConfig :: Map Int ManifoldConfig
  , stateTypeQualifier :: Map Int [(TVar, TypeU, Int)]
  , stateSourceMap :: Map Int SrcLoc
  , stateSourceText :: Map Path Text
  , stateBuildConfig :: BuildConfig
  , stateModuleName :: Maybe MVar
  , stateInstall :: Bool
  , stateInstallDir :: Maybe Path
  , stateClassDefs :: Map ClassName [Constraint]
  , stateLangRegistry :: LangRegistry
  , stateExportGroups :: Map Text ([Text], [Int])
    -- ^ Map from group name to (description lines, member export indices)
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

data MorlocError
  = SourcedError Int MDoc
  | SystemError MDoc
  | UnificationError Int Int Int MDoc
  deriving (Show)

---- Configuration

data Config
  = Config
  { configHome :: !Path
  , configLibrary :: !Path
  , configPlane :: !Path
  , configPlaneCore :: !Path
  , configTmpDir :: !Path
  , configBuildConfig :: !Path
  , configLangOverrides :: !(Map Text [Text])
  }
  deriving (Show, Ord, Eq)

---- Package metadata

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
  , packageMorlocDependencies :: [Text]
  , packageDependencies :: [Text]
  , packageInclude :: [Text]
  }
  deriving (Show, Ord, Eq)

---- Typechecking context

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

data Gamma = Gamma
  { gammaCounter :: Int
  , gammaContext :: [GammaIndex]
  , gammaSolved :: Map TVar TypeU
  }

---- Data files and system

data NexusSource = NexusSource
  { nexusSourceUtility :: MDoc
  , nexusSourceMain :: MDoc
  }

data Socket = Socket
  { socketLang :: Lang
  , socketServerInit :: [MDoc]
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
      , stateManifoldConfig = Map.empty
      , stateTypeQualifier = Map.empty
      , stateSourceMap = Map.empty
      , stateSourceText = Map.empty
      , stateBuildConfig = defaultValue
      , stateModuleName = Nothing
      , stateInstall = False
      , stateInstallDir = Nothing
      , stateClassDefs = Map.empty
      , stateLangRegistry = LR.emptyRegistry
      , stateExportGroups = Map.empty
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
      , packageCppVersion = 17
      , packageMorlocDependencies = []
      , packageDependencies = []
      , packageInclude = []
      }

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
      let legacyOverrides = Map.fromList $ filter (not . null . snd)
            [ ("py", if pyCmd == "" then [] else [pyCmd])
            , ("r", if rCmd == "" then [] else [rCmd])
            ]
          allOverrides = Map.union overrides legacyOverrides
      return $ Config home' source' plane' planeCore' tmpdir' buildConfig' allOverrides

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
      <*> o .:? "morloc-dependencies" .!= []
      <*> o .:? "dependencies" .!= []
      <*> o .:? "include" .!= []

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
  pretty (SrcG (Source ev1 lang _ _ _ _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
  pretty (AnnG v t) = pretty v <+> "::" <+> pretty t
