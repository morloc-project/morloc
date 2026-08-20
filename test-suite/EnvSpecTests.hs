{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : EnvSpecTests
Description : Unit tests for the backend-agnostic EnvSpec (envspec.json)

Covers the pure pieces of the dependency-management foundation: the per-language
source policy and its validation ('checkPackageDeps'), the DAG-wide aggregation
('buildEnvSpec') carrying each package's explicit source, and the deterministic
JSON renderer ('renderEnvSpec').
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

dep :: Text -> DepSource -> DepSpec
dep v s = DepSpec v (Just s)

-- A module declaring Python + Rust deps, a bare C++ link lib, and a module pin.
-- Python sources are explicit; the Rust crate lets its source default to crates.
pmA :: PackageMeta
pmA =
  pm
    { packagePyDeps =
        Map.fromList [("numpy", dep ">=2,<3" SrcConda), ("requests", dep "*" SrcPypi)]
    , packageRustDeps = Map.fromList [("ndarray", DepSpec "0.16" Nothing)]
    , packageDependencies = ["blas"]
    , packageMorlocDependencies = [("tensor-cpp", "abc123")]
    }

-- A second module declaring a C++ dep (source defaults to conda) and a Python
-- toolchain constraint.
pmB :: PackageMeta
pmB =
  pm
    { packageCppDeps = Map.fromList [("opencv", DepSpec ">=4.8" Nothing)]
    , packageCppVersion = 20
    , packageLangVersions = Map.fromList [("py", ">=3.10")]
    }

spec :: EnvSpec
spec =
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
            assertLeft (checkPackageDeps (pm {packagePyDeps = Map.fromList [("numpy", DepSpec "*" Nothing)]}))
        , testCase "python dep with pypi source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("requests", dep "*" SrcPypi)]}))
        , testCase "python dep with conda source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("numpy", dep "*" SrcConda)]}))
        , testCase "r cran source is not yet supported" $
            assertLeft (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", dep "*" SrcCran)]}))
        , testCase "r default (no source) is accepted" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", DepSpec "*" Nothing)]}))
        , testCase "rust with a pypi source is invalid" $
            assertLeft (checkPackageDeps (pm {packageRustDeps = Map.fromList [("ndarray", dep "*" SrcPypi)]}))
        , testCase "cpp default (no source) is accepted" $
            assertRight (checkPackageDeps (pm {packageCppDeps = Map.fromList [("boost", DepSpec "*" Nothing)]}))
        ]
    , testGroup
        "buildEnvSpec aggregation"
        [ testCase "schema + morloc version" $ do
            esVersion spec @?= 2
            esMorlocVersion spec @?= "0.98.2"
        , testCase "languages carry version + cpp std" $
            esLanguages spec
              @?= [ LangReq "py" (Just ">=3.10") Nothing
                  , LangReq "cpp" Nothing (Just "c++20")
                  , LangReq "rust" Nothing Nothing
                  ]
        , testCase "package groups: non-empty only, sorted by name, source-carrying" $
            esPackages spec
              @?= [ ("py", [PackageReq "numpy" ">=2,<3" SrcConda, PackageReq "requests" "*" SrcPypi])
                  , ("cpp", [PackageReq "opencv" ">=4.8" SrcConda])
                  , ("rust", [PackageReq "ndarray" "0.16" SrcCrates])
                  ]
        , testCase "system libs from bare -l dependencies (provider unspecified)" $
            esSystem spec @?= [SystemReq "blas" "unspecified"]
        , testCase "module pins carried through as git hashes" $
            esModules spec @?= [ModuleReq "tensor-cpp" (Just "abc123")]
        ]
    , testGroup
        "constraint union across the DAG (union, do not error)"
        [ testCase "same package, differing constraints, are merged not rejected" $
            let m1 = pm {packagePyDeps = Map.fromList [("numpy", dep ">=2" SrcConda)]}
                m2 = pm {packagePyDeps = Map.fromList [("numpy", dep "<3" SrcConda)]}
                s = buildEnvSpec "0.0.0" [makeLang "py" "py"] [m1, m2]
             in esPackages s @?= [("py", [PackageReq "numpy" ">=2,<3" SrcConda])]
        ]
    , testGroup
        "renderEnvSpec (deterministic JSON with explicit source)"
        [ testCase "minimal spec renders exactly" $
            renderEnvSpec minimal @?= minimalJson
        , testCase "each package carries a source field" $
            assertBool "envspec.json must emit a source per package" $
              T.isInfixOf "\"source\":\"conda\"" (renderEnvSpec spec)
        ]
    ]
  where
    minimal =
      EnvSpec
        { esVersion = 2
        , esMorlocVersion = "0.98.2"
        , esLanguages = [LangReq "py" Nothing Nothing]
        , esPackages = [("py", [PackageReq "numpy" ">=2" SrcConda])]
        , esSystem = []
        , esModules = []
        }
    minimalJson :: Text
    minimalJson =
      "{\"envspec_version\":2,\"morloc_version\":\"0.98.2\","
        <> "\"languages\":[{\"lang\":\"py\"}],"
        <> "\"packages\":{\"py\":[{\"name\":\"numpy\",\"constraint\":\">=2\",\"source\":\"conda\"}]},"
        <> "\"system\":[],\"modules\":[]}"

    assertLeft :: Either Text () -> Assertion
    assertLeft (Left _) = return ()
    assertLeft (Right _) = assertFailure "expected a validation error (Left), got Right"

    assertRight :: Either Text () -> Assertion
    assertRight (Right _) = return ()
    assertRight (Left e) = assertFailure ("expected Right, got Left: " <> T.unpack e)
