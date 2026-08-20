{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.EnvSpec
Description : Backend-agnostic environment-requirement record
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The 'EnvSpec' is a pure, offline function of a program's typed dependency
closure. It records the languages a program's pools use, the external native
packages each pool needs (each carrying the package DATABASE it is drawn from),
the system libraries, and the pinned morloc module dependencies. It is written
to @envspec.json@ beside @manifest.json@ and consumed by environment backends
(pixi, container, nix) that lower it to a concrete environment.

Every package carries an explicit source ('DepSource') -- conda, PyPI, crates,
etc. The source is declared in the module's @package.yaml@ (or defaulted per
language) and validated at module load, so the spec is self-describing: a
backend routes each package by its stated database, not by guessing from the
name.
-}
module Morloc.CodeGenerator.EnvSpec
  ( EnvSpec(..)
  , LangReq(..)
  , PackageReq(..)
  , SystemReq(..)
  , ModuleReq(..)
  , buildEnvSpec
  , renderEnvSpec
  ) where

import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import Morloc.Data.Json
import Morloc.Internal (unique)
import Morloc.Language (Lang, showLangName)
import Morloc.Namespace.State
  (PackageMeta(..), DepSpec(..), DepSource, defaultDepSource, depSourceText)

-- | One external native package required by a pool, with the package database
-- ('DepSource') it is drawn from.
data PackageReq = PackageReq
  { prName       :: !Text
  , prConstraint :: !Text
  , prSource     :: !DepSource
  }
  deriving (Show, Eq, Ord)

-- | One language toolchain used by the program's pools.
data LangReq = LangReq
  { lrLang       :: !Text        -- canonical morloc lang name (py, r, cpp, rust, ...)
  , lrConstraint :: !(Maybe Text)
  , lrStd        :: !(Maybe Text) -- e.g. cpp "c++20"
  }
  deriving (Show, Eq, Ord)

-- | A system library named by the C++ @dependencies:@ field. 'srProvider' is a
-- forward-compatible enum ("conda-forge" | "host" | "vcpkg" | "unspecified");
-- the compiler cannot know the true provider, so it emits "unspecified".
data SystemReq = SystemReq
  { srName     :: !Text
  , srProvider :: !Text
  }
  deriving (Show, Eq, Ord)

-- | A pinned morloc module dependency (existing exact-git-hash model).
data ModuleReq = ModuleReq
  { mrName    :: !Text
  , mrGitHash :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord)

data EnvSpec = EnvSpec
  { esVersion       :: !Int
  , esMorlocVersion :: !Text
  , esLanguages     :: ![LangReq]
  , esPackages      :: ![(Text, [PackageReq])]  -- canonical lang name -> package reqs
  , esSystem        :: ![SystemReq]
  , esModules       :: ![ModuleReq]
  }
  deriving (Show, Eq, Ord)

-- | Current on-disk schema version.
envSpecVersion :: Int
envSpecVersion = 2

-- | Assemble the EnvSpec from the program's pool languages and the DAG-wide
-- package metadata. Pure and offline.
buildEnvSpec
  :: String        -- ^ morloc version (Morloc.Version.versionStr)
  -> [Lang]        -- ^ the deduped set of pool languages (host pools; guests fold in)
  -> [PackageMeta] -- ^ statePackageMeta: one entry per module in the DAG
  -> EnvSpec
buildEnvSpec morlocVersion langs metas =
  EnvSpec
    { esVersion       = envSpecVersion
    , esMorlocVersion = MT.pack morlocVersion
    , esLanguages     = map langReq langNames
    , esPackages      = packageGroups
    , esSystem        = systemReqs
    , esModules       = moduleReqs
    }
  where
    -- Preserve pool order, drop duplicates.
    langNames = unique (map showLangName langs)

    -- Union of per-language toolchain version constraints across the DAG.
    langVersions = Map.unionsWith mergeConstraint (map packageLangVersions metas)
    -- Highest requested C++ standard across the DAG.
    cppVer = maximum (0 : map packageCppVersion metas)

    langReq name =
      LangReq
        { lrLang       = name
        , lrConstraint = Map.lookup name langVersions
        , lrStd        = if name == "cpp" && cppVer > 0
                           then Just ("c++" <> MT.pack (show cppVer))
                           else Nothing
        }

    -- Per-language dependency maps, unioned across the DAG. Each package's
    -- source is its declared source, or the language default when omitted.
    -- Only emit a group when it has at least one package.
    packageGroups =
      [ (lang, reqs)
      | (lang, pick) <- langDepFields
      , let reqs =
              [ PackageReq name (dsVersion ds) src
              | (name, ds) <- Map.toList (unionDepSpecs (map pick metas))
              , Just src <- [effectiveSource lang (dsSource ds)]
              ]
      , not (null reqs)
      ]

    langDepFields =
      [ ("py",    packagePyDeps)
      , ("r",     packageRDeps)
      , ("cpp",   packageCppDeps)
      , ("julia", packageJuliaDeps)
      , ("rust",  packageRustDeps)
      ]

    -- The bare C++ `-l` link libraries; provider is unknowable at compile time.
    systemReqs =
      [ SystemReq name "unspecified"
      | name <- unique (concatMap packageDependencies metas)
      ]

    -- Existing exact-git-hash module pins, deduplicated by module name
    -- (closer-to-root resolution already happened during load/install).
    moduleReqs =
      [ ModuleReq name (Just h)
      | (name, h) <- dedupFst (concatMap packageMorlocDependencies metas)
      ]

-- | Resolve a dependency's effective source: its declared source, or the
-- language default when omitted. 'Nothing' means unresolvable (a Python dep
-- with no source) -- module-load validation ('checkPackageDeps') rejects that
-- case, so post-validation this is always 'Just'.
effectiveSource :: Text -> Maybe DepSource -> Maybe DepSource
effectiveSource lang mSource = mSource <|> defaultDepSource lang

-- | Union dependency maps across the DAG. Constraints on the same package are
-- merged (the resolver intersects them); the first-seen source wins.
unionDepSpecs :: [Map Text DepSpec] -> Map Text DepSpec
unionDepSpecs = Map.unionsWith combine
  where
    combine a b =
      DepSpec (mergeConstraint (dsVersion a) (dsVersion b))
              (dsSource a <|> dsSource b)

-- | Merge two version constraints. When they differ, comma-join and dedup --
-- the resolver intersects them. Union, do NOT error on difference.
mergeConstraint :: Text -> Text -> Text
mergeConstraint a b
  | a == b = a
  | otherwise = MT.intercalate "," (unique (MT.splitOn "," a <> MT.splitOn "," b))

-- | Deduplicate association-list entries by their first component (keep first).
dedupFst :: Ord a => [(a, b)] -> [(a, b)]
dedupFst = go Set.empty
  where
    go _ [] = []
    go seen ((k, v):xs)
      | k `Set.member` seen = go seen xs
      | otherwise = (k, v) : go (Set.insert k seen) xs

-- | Render an EnvSpec as deterministic JSON text (map keys are sorted; language
-- order follows pool order). Uses the shared text-based JSON builders so the
-- artifact is stable for golden comparison.
renderEnvSpec :: EnvSpec -> Text
renderEnvSpec es =
  jsonObj
    [ ("envspec_version", jsonInt (esVersion es))
    , ("morloc_version", jsonStr (esMorlocVersion es))
    , ("languages", jsonArr (map langJson (esLanguages es)))
    , ("packages", jsonObj [ (lang, jsonArr (map pkgJson reqs))
                           | (lang, reqs) <- esPackages es ])
    , ("system", jsonArr (map sysJson (esSystem es)))
    , ("modules", jsonArr (map modJson (esModules es)))
    ]
  where
    langJson (LangReq name mc mstd) =
      jsonObj $
        [("lang", jsonStr name)]
        ++ maybe [] (\c -> [("constraint", jsonStr c)]) mc
        ++ maybe [] (\s -> [("std", jsonStr s)]) mstd

    pkgJson (PackageReq n c src) =
      jsonObj
        [ ("name", jsonStr n)
        , ("constraint", jsonStr c)
        , ("source", jsonStr (depSourceText src))
        ]

    sysJson (SystemReq n p) =
      jsonObj [("name", jsonStr n), ("provider", jsonStr p)]

    modJson (ModuleReq n mh) =
      jsonObj $ [("name", jsonStr n)]
        ++ maybe [] (\h -> [("git_hash", jsonStr h)]) mh
