{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : EnvSpecTests
Description : Unit tests for the backend-agnostic EnvSpec (envspec.json)

Covers the pure pieces of the dependency-management foundation: the per-language
source policy and its validation ('checkPackageDeps'), the DAG-wide aggregation
('buildEnvSpec') carrying each package's explicit source and conda channel, and
the deterministic JSON renderer ('renderEnvSpec').
-}
module EnvSpecTests (envSpecTests) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.EnvSpec
import Morloc.Language (makeLang)
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State
  (PackageMeta (..), DepSpec (..), DepSource (..), checkPackageDeps)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

pm :: PackageMeta
pm = defaultValue

-- A version + source dependency with no channel (the common case).
dep :: Text -> DepSource -> DepSpec
dep v s = DepSpec v (Just s) Nothing

-- A conda dependency drawn from an explicit channel (source implied by channel
-- in package.yaml, but modeled here as an explicit conda source).
chanDep :: Text -> Text -> DepSpec
chanDep v ch = DepSpec v (Just SrcConda) (Just ch)

-- buildEnvSpec returns Left only on a cross-module channel conflict; every other
-- test input is conflict-free, so force the EnvSpec out.
forceSpec :: Either Text EnvSpec -> EnvSpec
forceSpec = either (error . T.unpack) id

-- A module declaring Python + Rust deps, a bare C++ link lib, and a module pin.
-- Python sources are explicit; the Rust crate lets its source default to crates.
pmA :: PackageMeta
pmA =
  pm
    { packagePyDeps =
        Map.fromList [("numpy", dep ">=2,<3" SrcConda), ("requests", dep "*" SrcPypi)]
    , packageRustDeps = Map.fromList [("ndarray", DepSpec "0.16" Nothing Nothing)]
    , packageDependencies = ["blas"]
    , packageMorlocDependencies = [("tensor-cpp", "abc123")]
    }

-- A second module declaring a C++ dep (source defaults to conda) and a Python
-- toolchain constraint.
pmB :: PackageMeta
pmB =
  pm
    { packageCppDeps = Map.fromList [("opencv", DepSpec ">=4.8" Nothing Nothing)]
    , packageCppVersion = 20
    , packageLangVersions = Map.fromList [("py", ">=3.10")]
    }

spec :: EnvSpec
spec =
  forceSpec $
    buildEnvSpec
      "0.98.2"
      [makeLang "py" "py", makeLang "cpp" "cpp", makeLang "rust" "rs"]
      [pmA, pmB]

envSpecTests :: TestTree
envSpecTests =
  testGroup
    "EnvSpec (envspec.json) foundation"
    [ testGroup
        "checkPackageDeps source policy"
        [ testCase "python dep without a source is rejected" $
            assertLeft (checkPackageDeps (pm {packagePyDeps = Map.fromList [("numpy", DepSpec "*" Nothing Nothing)]}))
        , testCase "python dep with pypi source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("requests", dep "*" SrcPypi)]}))
        , testCase "python dep with conda source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("numpy", dep "*" SrcConda)]}))
        , testCase "r cran source is not yet supported" $
            assertLeft (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", dep "*" SrcCran)]}))
        , testCase "r default (no source) is accepted" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", DepSpec "*" Nothing Nothing)]}))
        , testCase "rust with a pypi source is invalid" $
            assertLeft (checkPackageDeps (pm {packageRustDeps = Map.fromList [("ndarray", dep "*" SrcPypi)]}))
        , testCase "cpp default (no source) is accepted" $
            assertRight (checkPackageDeps (pm {packageCppDeps = Map.fromList [("boost", DepSpec "*" Nothing Nothing)]}))
        ]
    , testGroup
        "checkPackageDeps channel policy"
        [ testCase "conda channel with no source is accepted (channel implies conda)" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("samtools", DepSpec "*" Nothing (Just "bioconda"))]}))
        , testCase "channel on an explicit conda source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("samtools", chanDep "*" "bioconda")]}))
        , testCase "channel on a pypi source is a contradiction" $
            assertLeft (checkPackageDeps (pm {packagePyDeps = Map.fromList [("requests", DepSpec "*" (Just SrcPypi) (Just "bioconda"))]}))
        , testCase "channel on a rust dep is invalid (crates has no channel)" $
            assertLeft (checkPackageDeps (pm {packageRustDeps = Map.fromList [("ndarray", DepSpec "0.16" Nothing (Just "bioconda"))]}))
        , testCase "conda-forge R feedstock name with r- prefix is rejected" $
            assertLeft (checkPackageDeps (pm {packageRDeps = Map.fromList [("r-ggplot2", DepSpec "*" Nothing Nothing)]}))
        , testCase "bare CRAN name under conda-forge is accepted" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", DepSpec "*" Nothing Nothing)]}))
        , testCase "non-conda-forge channel R name passes literally (no prefix check)" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("bioconductor-deseq2", chanDep "*" "bioconda")]}))
        ]
    , testGroup
        "buildEnvSpec aggregation"
        [ testCase "schema + morloc version" $ do
            esVersion spec @?= 1
            esMorlocVersion spec @?= "0.98.2"
        , testCase "languages carry version + cpp std" $
            esLanguages spec
              @?= [ LangReq "py" (Just ">=3.10") Nothing
                  , LangReq "cpp" Nothing (Just "c++20")
                  , LangReq "rust" Nothing Nothing
                  ]
        , testCase "package groups: non-empty only, sorted by name, source-carrying" $
            esPackages spec
              @?= [ ("py", [PackageReq "numpy" ">=2,<3" SrcConda Nothing, PackageReq "requests" "*" SrcPypi Nothing])
                  , ("cpp", [PackageReq "opencv" ">=4.8" SrcConda Nothing])
                  , ("rust", [PackageReq "ndarray" "0.16" SrcCrates Nothing])
                  ]
        , testCase "system libs from bare -l dependencies (provider unspecified)" $
            esSystem spec @?= [SystemReq "blas" "unspecified"]
        , testCase "module pins carried through as git hashes" $
            esModules spec @?= [ModuleReq "tensor-cpp" (Just "abc123")]
        ]
    , testGroup
        "channel aggregation across the DAG"
        [ testCase "a non-conda-forge channel rides the wire" $
            let m = pm {packagePyDeps = Map.fromList [("samtools", chanDep "*" "bioconda")]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m])
             in esPackages s @?= [("py", [PackageReq "samtools" "*" SrcConda (Just "bioconda")])]
        , testCase "conda-forge is omitted from the wire (byte-identical default)" $
            let m = pm {packagePyDeps = Map.fromList [("numpy", chanDep ">=2" "conda-forge")]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m])
             in esPackages s @?= [("py", [PackageReq "numpy" ">=2" SrcConda Nothing])]
        , testCase "matching explicit channels across modules merge" $
            let m1 = pm {packagePyDeps = Map.fromList [("samtools", chanDep ">=1" "bioconda")]}
                m2 = pm {packagePyDeps = Map.fromList [("samtools", chanDep "<2" "bioconda")]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m1, m2])
             in esPackages s @?= [("py", [PackageReq "samtools" ">=1,<2" SrcConda (Just "bioconda")])]
        , testCase "conflicting explicit channels across modules is a hard error" $
            let m1 = pm {packagePyDeps = Map.fromList [("samtools", chanDep "*" "bioconda")]}
                m2 = pm {packagePyDeps = Map.fromList [("samtools", chanDep "*" "custom")]}
             in assertLeft (fmap (const ()) (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m1, m2]))
        ]
    , testGroup
        "constraint union across the DAG (union, do not error)"
        [ testCase "same package, differing constraints, are merged not rejected" $
            let m1 = pm {packagePyDeps = Map.fromList [("numpy", dep ">=2" SrcConda)]}
                m2 = pm {packagePyDeps = Map.fromList [("numpy", dep "<3" SrcConda)]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m1, m2])
             in esPackages s @?= [("py", [PackageReq "numpy" ">=2,<3" SrcConda Nothing])]
        ]
    , testGroup
        "renderEnvSpec (deterministic JSON with explicit source)"
        [ testCase "minimal spec renders exactly" $
            renderEnvSpec minimal @?= minimalJson
        , testCase "each package carries a source field" $
            assertBool "envspec.json must emit a source per package" $
              T.isInfixOf "\"source\":\"conda\"" (renderEnvSpec spec)
        , testCase "a non-conda-forge channel is emitted" $
            assertBool "envspec.json must emit the channel field" $
              T.isInfixOf "\"channel\":\"bioconda\"" (renderEnvSpec channelSpec)
        , testCase "conda-forge channel is omitted from JSON" $
            assertBool "conda-forge channel must not appear on the wire" $
              not (T.isInfixOf "\"channel\"" (renderEnvSpec spec))
        ]
    ]
  where
    minimal =
      EnvSpec
        { esVersion = 1
        , esMorlocVersion = "0.98.2"
        , esLanguages = [LangReq "py" Nothing Nothing]
        , esPackages = [("py", [PackageReq "numpy" ">=2" SrcConda Nothing])]
        , esSystem = []
        , esModules = []
        }
    minimalJson :: Text
    minimalJson =
      "{\"envspec_version\":1,\"morloc_version\":\"0.98.2\","
        <> "\"languages\":[{\"lang\":\"py\"}],"
        <> "\"packages\":{\"py\":[{\"name\":\"numpy\",\"constraint\":\">=2\",\"source\":\"conda\"}]},"
        <> "\"system\":[],\"modules\":[]}"

    channelSpec =
      forceSpec $
        buildEnvSpec
          "0.0.0"
          [makeLang "py" "py"]
          [pm {packagePyDeps = Map.fromList [("samtools", chanDep "*" "bioconda")]}]

    assertLeft :: Either Text () -> Assertion
    assertLeft (Left _) = return ()
    assertLeft (Right _) = assertFailure "expected a validation error (Left), got Right"

    assertRight :: Either Text () -> Assertion
    assertRight (Right _) = return ()
    assertRight (Left e) = assertFailure ("expected Right, got Left: " <> T.unpack e)
