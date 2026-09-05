{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : EnvSpecTests
Description : Unit tests for the backend-agnostic EnvSpec (envspec.json)

Covers the pure pieces of the dependency-management foundation: the registry
dependency schema and its parser (illegal states, e.g. a channel on a non-conda
source, are unrepresentable / rejected at parse), the per-language source policy
and its validation ('checkPackageDeps') including local (filesystem-path) deps,
the DAG-wide aggregation ('buildEnvSpec'), and the deterministic JSON renderer.
-}
module EnvSpecTests (envSpecTests) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.EnvSpec
import Morloc.Language (makeLang)
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State
  ( PackageMeta (..)
  , DepSpec (..)
  , RegDep (..)
  , LocalDep (..)
  , DepSource (..)
  , checkPackageDeps
  , regOfSource
  , renderDepCapabilities
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

pm :: PackageMeta
pm = defaultValue

-- A version + source dependency with no channel (the common case).
dep :: Text -> DepSource -> DepSpec
dep v s = DepSpec (regOfSource s) v

-- A conda dependency drawn from an explicit channel.
chanDep :: Text -> Text -> DepSpec
chanDep v ch = DepSpec (RegConda (Just ch)) v

-- A dependency with the source omitted (resolves to the language default).
noSrc :: Text -> DepSpec
noSrc = DepSpec RegDefault

-- Wrap a lang -> (name -> LocalDep) table into the packageLocalDeps shape.
localGroup :: Text -> [(Text, LocalDep)] -> Map.Map Text (Map.Map Text LocalDep)
localGroup lang deps = Map.singleton lang (Map.fromList deps)

-- buildEnvSpec returns Left only on a cross-module channel conflict; every other
-- test input is conflict-free, so force the EnvSpec out.
forceSpec :: Either Text EnvSpec -> EnvSpec
forceSpec = either (error . T.unpack) id

-- Parse a package.yaml dependency VALUE (bare string or object) via its FromJSON.
decodeDep :: BL.ByteString -> Either String DepSpec
decodeDep = eitherDecode

-- Parse a whole package.yaml body (as JSON, which YAML decodes to) via the
-- PackageMeta FromJSON instance.
decodeMeta :: BL.ByteString -> Either String PackageMeta
decodeMeta = eitherDecode

-- A module declaring Python + Rust deps, a bare C++ link lib, and a module pin.
-- Python sources are explicit; the Rust crate lets its source default to crates.
pmA :: PackageMeta
pmA =
  pm
    { packagePyDeps =
        Map.fromList [("numpy", dep ">=2,<3" SrcConda), ("requests", dep "*" SrcPypi)]
    , packageRustDeps = Map.fromList [("ndarray", noSrc "0.16")]
    , packageDependencies = ["blas"]
    , packageMorlocDependencies = [("tensor-cpp", "abc123")]
    }

-- A second module declaring a C++ dep (source defaults to conda) and a Python
-- toolchain constraint.
pmB :: PackageMeta
pmB =
  pm
    { packageCppDeps = Map.fromList [("opencv", noSrc ">=4.8")]
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
        "FromJSON DepSpec (illegal states rejected at parse)"
        [ testCase "a bare version string parses with source defaulted" $
            case decodeDep "\">=2\"" of
              Right (DepSpec RegDefault ">=2") -> return ()
              other -> assertFailure ("expected DepSpec RegDefault \">=2\", got " <> show other)
        , testCase "an object with source+version parses" $
            assertBool "pypi dep should parse" (isRight (decodeDep "{\"source\":\"pypi\",\"version\":\"*\"}"))
        , testCase "a channel with no source pins conda" $
            case decodeDep "{\"channel\":\"bioconda\"}" of
              Right (DepSpec (RegConda (Just "bioconda")) _) -> return ()
              other -> assertFailure ("expected RegConda (Just bioconda), got " <> show other)
        , testCase "a channel on a conda source parses" $
            assertBool "conda+channel should parse" (isRight (decodeDep "{\"source\":\"conda\",\"channel\":\"bioconda\"}"))
        , testCase "a channel on a pypi source is a parse error (unrepresentable)" $
            assertBool "pypi+channel must be rejected" (isLeft (decodeDep "{\"source\":\"pypi\",\"channel\":\"bioconda\"}"))
        , testCase "a channel on a crates source is a parse error" $
            assertBool "crates+channel must be rejected" (isLeft (decodeDep "{\"source\":\"crates\",\"channel\":\"x\"}"))
        , testCase "an unknown source is a parse error" $
            assertBool "unknown source must be rejected" (isLeft (decodeDep "{\"source\":\"npm\"}"))
        ]
    , testGroup
        "FromJSON PackageMeta agrees with the no-package.yaml defaults"
        [ testCase "an omitted cpp-version defaults to the same standard as no package.yaml" $
            case decodeMeta "{\"name\":\"dnd\"}" of
              Right m -> packageCppVersion m @?= packageCppVersion (defaultValue :: PackageMeta)
              Left e -> assertFailure ("package.yaml should parse: " <> e)
        , testCase "an explicit cpp-version wins" $
            case decodeMeta "{\"name\":\"dnd\",\"cpp-version\":23}" of
              Right m -> packageCppVersion m @?= 23
              Left e -> assertFailure ("package.yaml should parse: " <> e)
        , testCase "a decoded meta still emits the cpp std on the wire" $
            case decodeMeta "{\"name\":\"dnd\"}" of
              Right m ->
                esLanguages (forceSpec (buildEnvSpec "0.98.2" [makeLang "cpp" "cpp"] [m]))
                  @?= [LangReq "cpp" Nothing (Just "c++20")]
              Left e -> assertFailure ("package.yaml should parse: " <> e)
        ]
    , testGroup
        "FromJSON LocalDep"
        [ testCase "path with editable parses" $
            case (eitherDecode "{\"path\":\"./vendor/mylib\",\"editable\":true}" :: Either String LocalDep) of
              Right (LocalDep "./vendor/mylib" True) -> return ()
              other -> assertFailure ("unexpected: " <> show other)
        , testCase "editable defaults to False" $
            case (eitherDecode "{\"path\":\"./x\"}" :: Either String LocalDep) of
              Right (LocalDep "./x" False) -> return ()
              other -> assertFailure ("unexpected: " <> show other)
        , testCase "a missing path is a parse error" $
            assertBool "path is required" (isLeft (eitherDecode "{\"editable\":true}" :: Either String LocalDep))
        ]
    , testGroup
        "checkPackageDeps source policy"
        [ testCase "python dep without a source is rejected" $
            assertLeft (checkPackageDeps (pm {packagePyDeps = Map.fromList [("numpy", noSrc "*")]}))
        , testCase "python dep with pypi source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("requests", dep "*" SrcPypi)]}))
        , testCase "python dep with conda source is accepted" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("numpy", dep "*" SrcConda)]}))
        , testCase "r cran source is not yet supported" $
            assertLeft (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", dep "*" SrcCran)]}))
        , testCase "r default (no source) is accepted" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", noSrc "*")]}))
        , testCase "rust with a pypi source is invalid" $
            assertLeft (checkPackageDeps (pm {packageRustDeps = Map.fromList [("ndarray", dep "*" SrcPypi)]}))
        , testCase "cpp default (no source) is accepted" $
            assertRight (checkPackageDeps (pm {packageCppDeps = Map.fromList [("boost", noSrc "*")]}))
        ]
    , testGroup
        "checkPackageDeps channel policy"
        [ testCase "conda channel with no source is accepted (channel implies conda)" $
            assertRight (checkPackageDeps (pm {packagePyDeps = Map.fromList [("samtools", chanDep "*" "bioconda")]}))
        , testCase "a conda channel on a rust dep is invalid (conda not valid for rust)" $
            assertLeft (checkPackageDeps (pm {packageRustDeps = Map.fromList [("ndarray", chanDep "0.16" "bioconda")]}))
        , testCase "conda-forge R feedstock name with r- prefix is rejected" $
            assertLeft (checkPackageDeps (pm {packageRDeps = Map.fromList [("r-ggplot2", noSrc "*")]}))
        , testCase "bare CRAN name under conda-forge is accepted" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("ggplot2", noSrc "*")]}))
        , testCase "non-conda-forge channel R name passes literally (no prefix check)" $
            assertRight (checkPackageDeps (pm {packageRDeps = Map.fromList [("bioconductor-deseq2", chanDep "*" "bioconda")]}))
        ]
    , testGroup
        "checkPackageDeps local-deps policy"
        [ testCase "python local dep is accepted" $
            assertRight (checkPackageDeps (pm {packageLocalDeps = localGroup "py" [("mylib", LocalDep "./vendor/mylib" True)]}))
        , testCase "rust local dep is accepted" $
            assertRight (checkPackageDeps (pm {packageLocalDeps = localGroup "rust" [("mycrate", LocalDep "./vendor/mycrate" False)]}))
        , testCase "cpp local dep is rejected (points to cxx-flags)" $
            assertLeftWith "cxx-flags" (checkPackageDeps (pm {packageLocalDeps = localGroup "cpp" [("mylib", LocalDep "./vendor/mylib" False)]}))
        , testCase "r local dep is rejected" $
            assertLeft (checkPackageDeps (pm {packageLocalDeps = localGroup "r" [("mylib", LocalDep "./vendor/mylib" False)]}))
        , testCase "julia local dep is rejected (not yet supported)" $
            assertLeft (checkPackageDeps (pm {packageLocalDeps = localGroup "julia" [("mylib", LocalDep "./vendor/mylib" False)]}))
        , testCase "an absolute local path is rejected" $
            assertLeft (checkPackageDeps (pm {packageLocalDeps = localGroup "py" [("mylib", LocalDep "/opt/mylib" False)]}))
        , testCase "a local path with .. is rejected" $
            assertLeft (checkPackageDeps (pm {packageLocalDeps = localGroup "py" [("mylib", LocalDep "../mylib" False)]}))
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
              @?= [ ("py", [ReqRegistry "numpy" ">=2,<3" SrcConda Nothing, ReqRegistry "requests" "*" SrcPypi Nothing])
                  , ("cpp", [ReqRegistry "opencv" ">=4.8" SrcConda Nothing])
                  , ("rust", [ReqRegistry "ndarray" "0.16" SrcCrates Nothing])
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
             in esPackages s @?= [("py", [ReqRegistry "samtools" "*" SrcConda (Just "bioconda")])]
        , testCase "conda-forge is omitted from the wire (byte-identical default)" $
            let m = pm {packagePyDeps = Map.fromList [("numpy", chanDep ">=2" "conda-forge")]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m])
             in esPackages s @?= [("py", [ReqRegistry "numpy" ">=2" SrcConda Nothing])]
        , testCase "matching explicit channels across modules merge" $
            let m1 = pm {packagePyDeps = Map.fromList [("samtools", chanDep ">=1" "bioconda")]}
                m2 = pm {packagePyDeps = Map.fromList [("samtools", chanDep "<2" "bioconda")]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m1, m2])
             in esPackages s @?= [("py", [ReqRegistry "samtools" ">=1,<2" SrcConda (Just "bioconda")])]
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
             in esPackages s @?= [("py", [ReqRegistry "numpy" ">=2,<3" SrcConda Nothing])]
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
    , testGroup
        "local deps in the envspec (root module)"
        [ testCase "py and rust local deps become ReqLocal entries per language" $
            let m =
                  pm
                    { packageLocalDeps =
                        Map.fromList
                          [ ("py", Map.fromList [("mylib", LocalDep "./vendor/mylib" True)])
                          , ("rust", Map.fromList [("mycrate", LocalDep "./vendor/mycrate" False)])
                          ]
                    }
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py", makeLang "rust" "rs"] [m])
             in esPackages s
                  @?= [ ("py", [ReqLocal "mylib" "./vendor/mylib" True])
                      , ("rust", [ReqLocal "mycrate" "./vendor/mycrate" False])
                      ]
        , testCase "a local dep renders source:local with path + editable" $
            let m = pm {packageLocalDeps = Map.fromList [("py", Map.fromList [("mylib", LocalDep "./vendor/mylib" True)])]}
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m])
                j = renderEnvSpec s
             in do
                  assertBool "source:local" (T.isInfixOf "\"source\":\"local\"" j)
                  assertBool "path" (T.isInfixOf "\"path\":\"./vendor/mylib\"" j)
                  assertBool "editable" (T.isInfixOf "\"editable\":true" j)
        , testCase "registry and local deps for one language coexist in the group" $
            let m =
                  pm
                    { packagePyDeps = Map.fromList [("numpy", dep ">=2" SrcConda)]
                    , packageLocalDeps = Map.fromList [("py", Map.fromList [("mylib", LocalDep "./mylib" False)])]
                    }
                s = forceSpec (buildEnvSpec "0.0.0" [makeLang "py" "py"] [m])
             in esPackages s
                  @?= [("py", [ReqRegistry "numpy" ">=2" SrcConda Nothing, ReqLocal "mylib" "./mylib" False])]
        ]
    , testGroup
        "renderDepCapabilities (derived from depPolicy, single source of truth)"
        [ testCase "python and rust support local deps; cpp/r/julia do not" $ do
            assertBool "py local yes" (T.isInfixOf "py | conda, pypi | yes" caps)
            assertBool "rust local yes" (T.isInfixOf "rust | crates | yes" caps)
            assertBool "cpp local no" (T.isInfixOf "cpp | conda | no" caps)
            assertBool "julia local no" (T.isInfixOf "julia | pkg | no" caps)
        ]
    ]
  where
    caps = renderDepCapabilities

    minimal =
      EnvSpec
        { esVersion = 2
        , esMorlocVersion = "0.98.2"
        , esLanguages = [LangReq "py" Nothing Nothing]
        , esPackages = [("py", [ReqRegistry "numpy" ">=2" SrcConda Nothing])]
        , esSystem = []
        , esModules = []
        }
    minimalJson :: Text
    minimalJson =
      "{\"envspec_version\":2,\"morloc_version\":\"0.98.2\","
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

    assertLeftWith :: Text -> Either Text () -> Assertion
    assertLeftWith needle (Left e) =
      assertBool ("error should mention '" <> T.unpack needle <> "': " <> T.unpack e) (T.isInfixOf needle e)
    assertLeftWith _ (Right _) = assertFailure "expected a validation error (Left), got Right"

    assertRight :: Either Text () -> Assertion
    assertRight (Right _) = return ()
    assertRight (Left e) = assertFailure ("expected Right, got Left: " <> T.unpack e)
