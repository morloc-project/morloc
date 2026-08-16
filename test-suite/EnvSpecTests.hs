{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : EnvSpecTests
Description : Unit tests for the backend-agnostic EnvSpec (envspec.json)

Covers the three pure pieces of the P1 dependency-management foundation:
the classification HINT ('classifyDep'), the DAG-wide aggregation
('buildEnvSpec'), and the deterministic JSON renderer ('renderEnvSpec').
The classification is a hypothesis, not a verdict -- these tests pin the
hints the compiler emits; the real purity decision is a backend concern
and is deliberately absent from the schema.
-}
module EnvSpecTests (envSpecTests) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.EnvSpec
import Morloc.Language (makeLang)
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State (PackageMeta (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

pm :: PackageMeta
pm = defaultValue

-- A module declaring Python + Rust deps, a bare C++ link lib, and a module pin.
pmA :: PackageMeta
pmA =
  pm
    { packagePyDeps = Map.fromList [("numpy", ">=2,<3"), ("requests", "*")]
    , packageRustDeps = Map.fromList [("ndarray", "0.16")]
    , packageDependencies = ["blas"]
    , packageMorlocDependencies = [("tensor-cpp", "abc123")]
    }

-- A second module declaring a C++ dep and a Python toolchain constraint.
pmB :: PackageMeta
pmB =
  pm
    { packageCppDeps = Map.fromList [("opencv", ">=4.8")]
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
        "classifyDep hint table"
        [ testCase "cpp libs are abi" $ classifyDep "cpp" "opencv" @?= Abi
        , testCase "rust crate is source" $ classifyDep "rust" "ndarray" @?= Source
        , testCase "rust -sys crate is unknown" $ classifyDep "rust" "openssl-sys" @?= Unknown
        , testCase "known compiled py pkg is abi" $ classifyDep "py" "numpy" @?= Abi
        , testCase "known pure py pkg is source" $ classifyDep "py" "requests" @?= Source
        , testCase "unrecognized py pkg is unknown" $ classifyDep "py" "mysteriolib" @?= Unknown
        , testCase "known pure r pkg is source" $ classifyDep "r" "ggplot2" @?= Source
        , testCase "unrecognized r pkg is unknown" $ classifyDep "r" "data.table" @?= Unknown
        , testCase "julia pkg is source" $ classifyDep "julia" "DataFrames" @?= Source
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
        , testCase "package groups: non-empty only, sorted by name, classified" $
            esPackages spec
              @?= [ ("py", [PackageReq "numpy" ">=2,<3" Abi, PackageReq "requests" "*" Source])
                  , ("cpp", [PackageReq "opencv" ">=4.8" Abi])
                  , ("rust", [PackageReq "ndarray" "0.16" Source])
                  ]
        , testCase "system libs from bare -l dependencies (provider unspecified)" $
            esSystem spec @?= [SystemReq "blas" "unspecified"]
        , testCase "module pins carried through as git hashes" $
            esModules spec @?= [ModuleReq "tensor-cpp" (Just "abc123")]
        ]
    , testGroup
        "constraint union across the DAG (union, do not error)"
        [ testCase "same package, differing constraints, are merged not rejected" $
            let m1 = pm {packagePyDeps = Map.fromList [("numpy", ">=2")]}
                m2 = pm {packagePyDeps = Map.fromList [("numpy", "<3")]}
                s = buildEnvSpec "0.0.0" [makeLang "py" "py"] [m1, m2]
             in esPackages s @?= [("py", [PackageReq "numpy" ">=2,<3" Abi])]
        ]
    , testGroup
        "renderEnvSpec (deterministic JSON, no purity verdict)"
        [ testCase "minimal spec renders exactly" $
            renderEnvSpec minimal @?= minimalJson
        , testCase "no purity field is emitted" $
            assertBool "envspec.json must not contain a purity verdict" $
              not (T.isInfixOf "purity" (renderEnvSpec spec))
        ]
    ]
  where
    minimal =
      EnvSpec
        { esVersion = 1
        , esMorlocVersion = "0.98.2"
        , esLanguages = [LangReq "py" Nothing Nothing]
        , esPackages = [("py", [PackageReq "numpy" ">=2" Abi])]
        , esSystem = []
        , esModules = []
        }
    minimalJson :: Text
    minimalJson =
      "{\"envspec_version\":1,\"morloc_version\":\"0.98.2\","
        <> "\"languages\":[{\"lang\":\"py\"}],"
        <> "\"packages\":{\"py\":[{\"name\":\"numpy\",\"constraint\":\">=2\",\"class\":\"abi\"}]},"
        <> "\"system\":[],\"modules\":[]}"
