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
import Control.Monad (foldM)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import Morloc.Data.Json
import Morloc.Internal (unique)
import Morloc.Language (Lang, showLangName)
import Morloc.Namespace.State
  ( PackageMeta(..), DepSpec(..), DepSource(..)
  , effectiveDepSource, depSourceText, condaForgeChannel
  )

-- | One external native package required by a pool, with the package database
-- ('DepSource') it is drawn from. 'prChannel' is the conda channel (sub-database)
-- the package is drawn from, present only when it is NOT conda-forge: conda-forge
-- is the universal default and is omitted so existing envspecs stay byte-identical.
data PackageReq = PackageReq
  { prName       :: !Text
  , prConstraint :: !Text
  , prSource     :: !DepSource
  , prChannel    :: !(Maybe Text)
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

-- | A pinned morloc module dependency (exact-git-hash model).
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
envSpecVersion = 3

-- | Assemble the EnvSpec from the program's pool languages and the DAG-wide
-- package metadata. Pure and offline. Returns 'Left' with a human-readable
-- message when two modules declare conflicting conda channels for one package
-- (channels are a provenance choice and cannot be intersected).
buildEnvSpec
  :: String        -- ^ morloc version (Morloc.Version.versionStr)
  -> [Lang]        -- ^ the deduped set of pool languages (host pools; guests fold in)
  -> [PackageMeta] -- ^ statePackageMeta: one entry per module in the DAG
  -> Either Text EnvSpec
buildEnvSpec morlocVersion langs metas = do
  packageGroups <- fmap (filter (not . null . snd)) (mapM mkGroup langDepFields)
  return
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
    -- source is its declared source, or the language default when omitted; its
    -- channel rides the wire only when it is a non-conda-forge conda channel.
    -- May fail on a cross-module channel conflict. Empty groups are dropped by
    -- the caller.
    mkGroup (lang, pick) = do
      merged <- unionDepSpecs [(packageName m, pick m) | m <- metas]
      let reqs =
            [ PackageReq name (dsVersion ds) src (resolveChannel src (dsChannel ds))
            | (name, ds) <- Map.toList merged
            , Just src <- [effectiveDepSource lang ds]
            ]
      return (lang, reqs)

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

    -- Exact-git-hash module pins, deduplicated by module name (closer-to-root
    -- resolution already happened during load/install).
    moduleReqs =
      [ ModuleReq name (Just h)
      | (name, h) <- dedupFst (concatMap packageMorlocDependencies metas)
      ]

-- | The channel carried on the wire for a package. Only a conda dependency on a
-- NON-conda-forge channel keeps its channel; conda-forge (the universal default)
-- and every non-conda source omit it, so existing envspecs stay byte-identical.
resolveChannel :: DepSource -> Maybe Text -> Maybe Text
resolveChannel SrcConda (Just ch) | ch /= condaForgeChannel = Just ch
resolveChannel _ _ = Nothing

-- | Union dependency maps across the DAG, each tagged with its owning module so a
-- conflict can name both sides. Constraints on the same package are merged (the
-- resolver intersects them) and the first-seen source wins. A channel is a
-- provenance choice and is NOT intersectable: an explicit channel beats an
-- omitted one, but two DIFFERENT explicit channels on one package is a hard error.
unionDepSpecs :: [(Text, Map Text DepSpec)] -> Either Text (Map Text DepSpec)
unionDepSpecs modMaps = fmap (Map.map fst) (foldM addModule Map.empty modMaps)
  where
    -- accumulator pairs each dep with the module that set its explicit channel
    addModule acc (modName, m) = foldM (addDep modName) acc (Map.toList m)

    addDep modName acc (name, ds) =
      case Map.lookup name acc of
        Nothing -> Right (Map.insert name (ds, chanOwner modName ds) acc)
        Just (prev, prevOwner) -> do
          (ch, owner) <-
            mergeChannel name (dsChannel prev, prevOwner) (dsChannel ds, chanOwner modName ds)
          let merged =
                DepSpec (mergeConstraint (dsVersion prev) (dsVersion ds))
                        (dsSource prev <|> dsSource ds)
                        ch
          Right (Map.insert name (merged, owner) acc)

    -- the module that established this dep's explicit channel, if any
    chanOwner modName ds = modName <$ dsChannel ds

    -- 'prev' already carries the winning channel and its owner.
    mergeChannel _ (Nothing, _) (b, bOwner) = Right (b, bOwner)
    mergeChannel _ (a, aOwner) (Nothing, _) = Right (a, aOwner)
    mergeChannel name (Just a, aOwner) (Just b, bOwner)
      | a == b = Right (Just a, aOwner)
      | otherwise =
          Left $
            "conflicting conda channels for dependency '" <> name <> "': "
              <> ownerLabel aOwner <> " requires channel '" <> a <> "' but "
              <> ownerLabel bOwner <> " requires channel '" <> b <> "'"

    ownerLabel (Just m) = "module '" <> m <> "'"
    ownerLabel Nothing = "a module"

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

    pkgJson (PackageReq n c src mch) =
      jsonObj $
        [ ("name", jsonStr n)
        , ("constraint", jsonStr c)
        , ("source", jsonStr (depSourceText src))
        ]
        ++ maybe [] (\ch -> [("channel", jsonStr ch)]) mch

    sysJson (SystemReq n p) =
      jsonObj [("name", jsonStr n), ("provider", jsonStr p)]

    modJson (ModuleReq n mh) =
      jsonObj $ [("name", jsonStr n)]
        ++ maybe [] (\h -> [("git_hash", jsonStr h)]) mh
