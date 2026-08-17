{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.LangSupport
Description : Per-release language-support table parsed from requirements.yaml
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The language-support table records, for THIS morloc release, the conda packages
morloc's own language binders (pymorloc, rmorloc, cppmorloc, rustmorloc) require
and the language runtime versions morloc supports. It is morloc's contribution to
a native environment's constraints, independent of any user program's deps.

The data is VOLATILE and lives beside the binder sources it describes, kept OUT
of the (stable) grammar files: the core build toolchain in
@data\/lang\/requirements.yaml@ (beside @languages.yaml@), and per-language
requirements in @data\/lang\/<lang>\/requirements.yaml@ (beside @lang.yaml@). This
module only PARSES and aggregates them (mirroring 'Morloc.LangRegistry'); the
values live in the YAML.

The compat key is a TUPLE, not a per-language version: pymorloc.c uses the numpy
C-API, so python compatibility is @(python, numpy-ABI, compiler)@. The table
therefore constrains the whole ABI-relevant closure as per-package requirements.

It drives four consumers: the pixi solve-clamp + package injection, the
multi-version CI matrix, pin-vs-support conflict diagnostics, and bare
@new --lang@ defaults. Values are PROVISIONAL until the CI matrix verifies them.
-}
module Morloc.LangSupport
  ( LangSupport (..)
  , LangEntry (..)
  , RuntimeSpec (..)
  , PkgReq (..)
  , Phase (..)
  , parseRequirements
  , renderLangSupport
  ) where

import Control.Monad (forM)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import Morloc.Data.Json
import qualified Morloc.Data.Text as MT
import qualified Morloc.Version

-- | When a required package is needed: at build time only (droppable from serve
-- images), at runtime only, or both.
data Phase = Build | Runtime | Both
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON Phase where
  parseJSON =
    Aeson.withText "phase" $ \t -> case t of
      "build" -> pure Build
      "runtime" -> pure Runtime
      "both" -> pure Both
      _ -> fail ("unknown phase '" <> T.unpack t <> "' (expected build|runtime|both)")

-- | One conda package morloc requires, with a version match-spec. An OPTIONAL
-- requirement is one a binder can use if present but does not need for basic
-- operation -- needed only when a program uses a particular feature/type (e.g.
-- pyarrow for Arrow\/Table data, imported lazily). A backend includes optional
-- packages for a full-featured env and may omit them for a minimal one; a
-- non-optional requirement (the default) is always installed.
data PkgReq = PkgReq
  { pkgName :: !Text
  , pkgConstraint :: !Text
  , pkgPhase :: !Phase
  , pkgOptional :: !Bool
  }
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON PkgReq where
  parseJSON =
    Aeson.withObject "package requirement" $ \o ->
      PkgReq
        <$> o .: "package"
        <*> o .:? "version" .!= "*"
        <*> o .:? "phase" .!= Both
        <*> o .:? "optional" .!= False

-- | A language's versioned runtime (absent for e.g. C++, whose "version" is the
-- standard, not a package).
data RuntimeSpec = RuntimeSpec
  { rsPackage :: !Text
  , rsVersion :: !Text          -- ^ supported floor..ceiling (conda match-spec)
  , rsDefault :: !(Maybe Text)  -- ^ blessed default within the range
  }
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON RuntimeSpec where
  parseJSON =
    Aeson.withObject "runtime" $ \o ->
      RuntimeSpec
        <$> o .: "package"
        <*> o .:? "version" .!= "*"
        <*> o .:? "default"

-- | Support for one language: its runtime (if versioned) plus morloc's own extra
-- conda packages for that language's binder.
data LangEntry = LangEntry
  { leRuntime :: !(Maybe RuntimeSpec)
  , leRequires :: ![PkgReq]
  }
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON LangEntry where
  parseJSON =
    Aeson.withObject "requirements" $ \o ->
      LangEntry
        <$> o .:? "runtime"
        <*> o .:? "requires" .!= []

-- | The whole table for this release.
data LangSupport = LangSupport
  { lsMorlocVersion :: !Text
  , lsToolchain :: ![PkgReq]              -- ^ core toolchain, always required
  , lsLanguages :: !(Map Text LangEntry)  -- ^ keyed by canonical short name (py/r/cpp/rust)
  }
  deriving (Show, Eq)

-- | Internal shape of the core @data\/lang\/requirements.yaml@.
newtype CoreReq = CoreReq {crToolchain :: [PkgReq]}

instance Aeson.FromJSON CoreReq where
  parseJSON =
    Aeson.withObject "core requirements" $ \o ->
      CoreReq <$> o .:? "toolchain" .!= []

-- | Parse the embedded requirements files into the support table. Arguments: the
-- per-language @(name, yaml-text)@ pairs and the core toolchain yaml-text.
parseRequirements :: [(Text, Text)] -> Text -> Either String LangSupport
parseRequirements langFiles coreText = do
  core <- decodeYaml "requirements.yaml (core toolchain)" coreText
  langs <- forM langFiles $ \(name, content) -> do
    entry <- decodeYaml ("requirements.yaml for " <> T.unpack name) content
    return (name, entry)
  return
    LangSupport
      { lsMorlocVersion = MT.pack Morloc.Version.versionStr
      , lsToolchain = crToolchain core
      , lsLanguages = Map.fromList langs
      }

decodeYaml :: Aeson.FromJSON a => String -> Text -> Either String a
decodeYaml label content =
  case Y.decodeEither' (TE.encodeUtf8 content) of
    Left err -> Left ("Failed to parse " <> label <> ": " <> Y.prettyPrintParseException err)
    Right x -> Right x

-- | Deterministic JSON. Map keys are emitted in ascending order.
renderLangSupport :: LangSupport -> Text
renderLangSupport ls =
  jsonObj
    [ ("morloc_version", jsonStr (lsMorlocVersion ls))
    , ("toolchain", jsonArr (map pkgJson (lsToolchain ls)))
    , ("languages", jsonObj [(name, langJson e) | (name, e) <- Map.toAscList (lsLanguages ls)])
    ]
  where
    langJson e =
      jsonObj
        [ ("runtime", maybe jsonNull runtimeJson (leRuntime e))
        , ("requires", jsonArr (map pkgJson (leRequires e)))
        ]
    runtimeJson (RuntimeSpec p v d) =
      jsonObj
        [ ("package", jsonStr p)
        , ("version", jsonStr v)
        , ("default", jsonMaybeStr d)
        ]
    pkgJson (PkgReq n c ph opt) =
      jsonObj
        [ ("package", jsonStr n)
        , ("constraint", jsonStr c)
        , ("phase", jsonStr (phaseStr ph))
        , ("optional", jsonBool opt)
        ]
    phaseStr Build = "build"
    phaseStr Runtime = "runtime"
    phaseStr Both = "both"
