{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.EnvSpec
Description : Backend-agnostic environment-requirement record
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The 'EnvSpec' is a pure, offline function of a program's typed dependency
closure. It records the languages a program's pools use, the external native
packages each pool needs (with a CLASSIFICATION HINT), the system libraries,
and the pinned morloc module dependencies. It is written to @envspec.json@
beside @manifest.json@ and consumed by environment backends (pixi, container,
nix) that lower it to a concrete environment.

Deliberately absent: any "purity" verdict. The compiler cannot see transitive
dependency closures or query package channels offline, so it emits a
classification HYPOTHESIS ('abi'/'source'/'unknown'), never a decision. The
backend solve -- which does see the full closure and channel availability --
is the authority on whether an environment can be materialized natively.
-}
module Morloc.CodeGenerator.EnvSpec
  ( EnvSpec(..)
  , LangReq(..)
  , PackageReq(..)
  , DepClass(..)
  , SystemReq(..)
  , ModuleReq(..)
  , buildEnvSpec
  , renderEnvSpec
  , classifyDep
  ) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import Morloc.Data.Json
import Morloc.Internal (unique)
import Morloc.Language (Lang, showLangName)
import Morloc.Namespace.State (PackageMeta(..))

-- | Whether a declared dependency is expected to drop externally-built machine
-- code into a pool process. A HINT computed from the top-level package name;
-- the backend solve refines it against the transitive closure.
data DepClass = Abi | Source | Unknown
  deriving (Show, Eq, Ord)

-- | One external native package required by a pool.
data PackageReq = PackageReq
  { prName       :: !Text
  , prConstraint :: !Text
  , prClass      :: !DepClass
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
envSpecVersion = 1

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
    langVersions = unionConstraints (map packageLangVersions metas)
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

    -- Per-language dependency maps, unioned across the DAG, then classified.
    -- Only emit a group when it has at least one package.
    packageGroups =
      [ (lang, reqs)
      | (lang, pick) <- langDepFields
      , let reqs = classifyGroup lang (unionConstraints (map pick metas))
      , not (null reqs)
      ]

    langDepFields =
      [ ("py",    packagePyDeps)
      , ("r",     packageRDeps)
      , ("cpp",   packageCppDeps)
      , ("julia", packageJuliaDeps)
      , ("rust",  packageRustDeps)
      ]

    classifyGroup lang m =
      [ PackageReq name constraint (classifyDep lang name)
      | (name, constraint) <- Map.toList m
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

-- | Classification HINT for a top-level package name. This is a hypothesis
-- refined by the backend solve, never a gate.
--
--   * C/C++ libs are compiled native code -> abi.
--   * Rust crates are source, EXCEPT @-sys@ crates that link a native library
--     -> unknown (the solve confirms whether that library is in-world).
--   * Python/R packages are abi when a known compiled package, source when a
--     known pure package, else unknown.
--   * Julia packages are source (Pkg.jl artifacts are handled separately).
classifyDep :: Text -> Text -> DepClass
classifyDep lang name
  | lang == "cpp"   = Abi
  | lang == "rust"  = if "-sys" `MT.isSuffixOf` name then Unknown else Source
  | lang == "py"    = if name `Set.member` pyAbi then Abi
                      else if name `Set.member` pyPure then Source
                      else Unknown
  | lang == "r"     = if name `Set.member` rPure then Source else Unknown
  | lang == "julia" = Source
  | otherwise       = Unknown

-- Small, extensible hint tables. Refined by the backend solve; not exhaustive.
pyAbi :: Set.Set Text
pyAbi = Set.fromList
  [ "numpy", "scipy", "pandas", "pyarrow", "torch", "tensorflow"
  , "scikit-learn", "matplotlib", "pillow", "lxml", "cryptography"
  , "numba", "h5py", "opencv-python", "polars", "grpcio"
  ]

pyPure :: Set.Set Text
pyPure = Set.fromList
  [ "requests", "click", "pyyaml", "six", "urllib3", "typing-extensions"
  , "attrs", "jinja2", "toml", "packaging"
  ]

rPure :: Set.Set Text
rPure = Set.fromList
  [ "ggplot2", "dplyr", "tidyr", "purrr", "stringr", "jsonlite" ]

-- | Union a list of constraint maps. When two modules constrain the same
-- package differently, keep both constraints (comma-joined, deduplicated) --
-- the resolver intersects them. Union, do NOT error on difference.
unionConstraints :: [Map Text Text] -> Map Text Text
unionConstraints = foldr (Map.unionWith combine) Map.empty
  where
    combine a b
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

    pkgJson (PackageReq n c cls) =
      jsonObj
        [ ("name", jsonStr n)
        , ("constraint", jsonStr c)
        , ("class", jsonStr (classStr cls))
        ]

    sysJson (SystemReq n p) =
      jsonObj [("name", jsonStr n), ("provider", jsonStr p)]

    modJson (ModuleReq n mh) =
      jsonObj $ [("name", jsonStr n)]
        ++ maybe [] (\h -> [("git_hash", jsonStr h)]) mh

    classStr Abi = "abi"
    classStr Source = "source"
    classStr Unknown = "unknown"
