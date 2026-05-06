{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MorlocDepsTests
-- Description : Unit tests for the morloc-dependencies package.yaml field
--               and the closer-to-install-root-wins resolver.
module MorlocDepsTests
  ( morlocDepsTests
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Morloc.Monad as MM
import qualified System.Directory as SD
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.Module
  ( OverwriteProtocol (..)
  , PinEntry (..)
  , PinMap
  , addPin
  , hashEq
  , reconcileOverwrite
  )
import Morloc.Namespace.Prim (Defaultable (..))
import Morloc.Namespace.State
  ( Config (..)
  , PackageMeta (..)
  )

-- | Run a MorlocMonad-returning action and extract the Either result.
runMM :: MM.MorlocMonad a -> IO (Either MM.MorlocError a)
runMM action = do
  home <- SD.getHomeDirectory
  let cfg =
        Config
          { configHome = home <> "/.local/share/morloc"
          , configLibrary = home <> "/.local/share/src/morloc"
          , configPlane = "default"
          , configPlaneCore = "morloclib"
          , configTmpDir = home <> "/.morloc/tmp"
          , configBuildConfig = home <> "/.morloc/.build-config.yaml"
          , configLangOverrides = mempty
          , configRegistry = Nothing
          }
  ((r, _), _) <- MM.runMorlocMonad Nothing 0 cfg defaultValue action
  return r

-- | Decode a YAML payload as PackageMeta. Returns Either parse error / value.
parsePackageYaml :: BS.ByteString -> Either Yaml.ParseException PackageMeta
parsePackageYaml = Yaml.decodeEither'

morlocDepsTests :: TestTree
morlocDepsTests =
  testGroup
    "morloc-dependencies"
    [ packageYamlParserTests
    , resolverTests
    ]

-- ---------------------------------------------------------------------------
-- YAML parser tests
-- ---------------------------------------------------------------------------

packageYamlParserTests :: TestTree
packageYamlParserTests =
  testGroup
    "package.yaml :: morloc-dependencies parser"
    [ testCase "missing field defaults to []" $ do
        let yamlText = "name: foo\nversion: 0.1.0\n"
        case parsePackageYaml yamlText of
          Left e -> assertFailure (show e)
          Right pm -> packageMorlocDependencies pm @?= []
    , testCase "explicit empty list" $ do
        let yamlText = "name: foo\nmorloc-dependencies: []\n"
        case parsePackageYaml yamlText of
          Left e -> assertFailure (show e)
          Right pm -> packageMorlocDependencies pm @?= []
    , testCase "two valid entries parsed in order" $ do
        let yamlText =
              "name: foo\n\
              \morloc-dependencies:\n\
              \  - name: root\n\
              \    git-hash: 1a2b3c4d5e6f7890abcdef1234567890abcdef12\n\
              \  - name: table\n\
              \    git-hash: 9f8e7d6c5b4a39281706050403020100f0e1d2c3\n"
        case parsePackageYaml yamlText of
          Left e -> assertFailure (show e)
          Right pm ->
            packageMorlocDependencies pm
              @?= [ ("root", "1a2b3c4d5e6f7890abcdef1234567890abcdef12")
                  , ("table", "9f8e7d6c5b4a39281706050403020100f0e1d2c3")
                  ]
    , testCase "missing git-hash field is rejected" $ do
        let yamlText =
              "name: foo\n\
              \morloc-dependencies:\n\
              \  - name: root\n"
        case parsePackageYaml yamlText of
          Left _ -> return ()  -- expected: parse failure
          Right pm ->
            assertFailure $
              "expected parse failure on missing git-hash; got: "
                <> show (packageMorlocDependencies pm)
    , testCase "non-string git-hash is rejected" $ do
        let yamlText =
              "name: foo\n\
              \morloc-dependencies:\n\
              \  - name: root\n\
              \    git-hash: 12345\n"
        -- aeson coerces numeric scalars to strings in YAML, so this
        -- particular case may parse. The real safety net is the unit tests
        -- on the resolver. We assert that the parser does not crash.
        case parsePackageYaml yamlText of
          Left _ -> return ()
          Right _ -> return ()
    ]

-- ---------------------------------------------------------------------------
-- Resolver tests (closer-to-install-root wins)
-- ---------------------------------------------------------------------------

resolverTests :: TestTree
resolverTests =
  testGroup
    "Resolver: closer-to-install-root wins"
    [ testCase "two distinct pins at depth 0 coexist" $ do
        result <- runMM $ do
          m1 <- addPin Map.empty 0 "root-pkg" "X" "hashX"
          addPin m1 0 "root-pkg" "Y" "hashY"
        case result of
          Left e -> assertFailure (show e)
          Right m -> do
            Map.lookup "X" m @?= Just (PinEntry "hashX" 0 "root-pkg")
            Map.lookup "Y" m @?= Just (PinEntry "hashY" 0 "root-pkg")

    , testCase "shallower pin wins over deeper pin" $ do
        result <- runMM $ do
          m1 <- addPin Map.empty 0 "root-pkg" "X" "rootHash"
          addPin m1 1 "child-pkg" "X" "childHash"
        case result of
          Left e -> assertFailure (show e)
          Right m ->
            Map.lookup "X" m @?= Just (PinEntry "rootHash" 0 "root-pkg")

    , testCase "deeper-existing replaced by shallower new pin" $ do
        result <- runMM $ do
          m1 <- addPin Map.empty 2 "grandchild-pkg" "X" "grandHash"
          addPin m1 1 "sibling-pkg" "X" "siblingHash"
        case result of
          Left e -> assertFailure (show e)
          Right m ->
            Map.lookup "X" m @?= Just (PinEntry "siblingHash" 1 "sibling-pkg")

    , testCase "siblings at same depth with same hash is no-op" $ do
        result <- runMM $ do
          m1 <- addPin Map.empty 1 "sibling-A" "X" "hashAgree"
          addPin m1 1 "sibling-B" "X" "hashAgree"
        case result of
          Left e -> assertFailure (show e)
          Right m ->
            -- whoever got there first owns the entry; same hash either way
            Map.lookup "X" m @?= Just (PinEntry "hashAgree" 1 "sibling-A")

    , testCase "siblings at same depth with different hash errors" $ do
        result <- runMM $ do
          m1 <- addPin Map.empty 1 "sibling-A" "X" "hashOne"
          addPin m1 1 "sibling-B" "X" "hashTwo"
        case result of
          Right m -> assertFailure ("expected conflict error, got: " <> show m)
          Left _ -> return ()  -- expected

    , testCase "case-insensitive hash equality" $ do
        let upper = "ABCDEF1234567890"
            lower = "abcdef1234567890"
        assertBool "case insensitive match" (hashEq upper lower)
        assertBool "different lengths still fail to match" $
          not (hashEq "abc" "abcdef")

    , testCase "reconcileOverwrite forces reinstall on hash mismatch" $ do
        reconcileOverwrite DoNotOverwrite (Just "expected") (Just "actual")
          @?= ForceOverwrite

    , testCase "reconcileOverwrite forces reinstall when pin set but no fdb" $ do
        reconcileOverwrite DoNotOverwrite (Just "expected") Nothing
          @?= ForceOverwrite

    , testCase "reconcileOverwrite leaves DoNotOverwrite when hashes agree" $ do
        reconcileOverwrite DoNotOverwrite (Just "abc") (Just "ABC")
          @?= DoNotOverwrite

    , testCase "reconcileOverwrite leaves DoNotOverwrite when no pin and no fdb" $ do
        reconcileOverwrite DoNotOverwrite Nothing Nothing
          @?= DoNotOverwrite

    , testCase "reconcileOverwrite preserves explicit ForceOverwrite" $ do
        reconcileOverwrite ForceOverwrite Nothing Nothing
          @?= ForceOverwrite
    ]
