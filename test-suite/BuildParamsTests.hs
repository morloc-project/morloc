{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : BuildParamsTests
-- Description : Unit tests for per-language build parameters (-X lang:key=value)
--
-- Covers the grammar parser, the fold/merge/flag-list semantics, the Futhark
-- backend/device resolver (where value validation lives), the backend link
-- flags, the cache-key salt (P1: differing params must differ), and the build
-- config round-trip (P2: writing lang-params must not drop other fields).
module BuildParamsTests
  ( buildParamsTests
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.Build.Params
  ( LangParams
  , foldLangParams
  , lookupFlags
  , lookupParam
  , mergeLangParams
  , parseLangParam
  , renderSalt
  )
import Morloc.CodeGenerator.Guest.Futhark (backendLinkFlags, resolveFutharkBuild)
import Morloc.Namespace.Expr (BuildConfig (..))

-- helper to build a nested param map
lp :: [(Text, [(Text, Text)])] -> LangParams
lp xs = Map.fromList [(lang, Map.fromList kvs) | (lang, kvs) <- xs]

-- | Assert that an Either is a Left (a rejection).
assertLeft :: Show a => Either e a -> Assertion
assertLeft e = case e of
  Left _ -> return ()
  Right v -> assertFailure ("expected rejection, got " ++ show v)

buildParamsTests :: TestTree
buildParamsTests =
  testGroup
    "Build parameters (-X lang:key=value)"
    [ parseTests
    , foldMergeTests
    , futharkResolveTests
    , linkFlagTests
    , saltTests
    , buildConfigRoundTripTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "parseLangParam"
    [ testCase "simple" $
        parseLangParam "futhark:backend=cuda" @?= Right ("futhark", "backend", "cuda")
    , testCase "value keeps a second '='" $
        parseLangParam "cpp:flags=-DFOO=bar" @?= Right ("cpp", "flags", "-DFOO=bar")
    , testCase "value keeps a ':'" $
        parseLangParam "r:libdir=C:/R" @?= Right ("r", "libdir", "C:/R")
    , testCase "empty value is allowed" $
        parseLangParam "cpp:flags=" @?= Right ("cpp", "flags", "")
    , testCase "missing ':' is rejected" $
        assertLeft (parseLangParam "nocolon")
    , testCase "missing '=' is rejected" $
        assertLeft (parseLangParam "cpp:flagsnovalue")
    , testCase "empty lang is rejected" $
        assertLeft (parseLangParam ":backend=x")
    , testCase "empty key is rejected" $
        assertLeft (parseLangParam "cpp:=x")
    ]

foldMergeTests :: TestTree
foldMergeTests =
  testGroup
    "fold / merge / flags"
    [ testCase "structured key: last write wins" $
        lookupParam "futhark" "backend" (foldLangParams [("futhark", "backend", "c"), ("futhark", "backend", "cuda")])
          @?= Just "cuda"
    , testCase "flags: repeated entries accumulate in order" $
        lookupFlags "cpp" (foldLangParams [("cpp", "flags", "-a"), ("cpp", "flags", "-b")])
          @?= ["-a", "-b"]
    , testCase "merge: override wins for structured key" $
        lookupParam "futhark" "backend" (mergeLangParams (lp [("futhark", [("backend", "c")])]) (lp [("futhark", [("backend", "cuda")])]))
          @?= Just "cuda"
    , testCase "merge: flags concatenate base then override" $
        lookupFlags "cpp" (mergeLangParams (lp [("cpp", [("flags", "-a")])]) (lp [("cpp", [("flags", "-b")])]))
          @?= ["-a", "-b"]
    , testCase "merge: keys from both langs survive" $
        lookupParam "rust" "opt-level" (mergeLangParams (lp [("cpp", [("flags", "-a")])]) (lp [("rust", [("opt-level", "3")])]))
          @?= Just "3"
    ]

futharkResolveTests :: TestTree
futharkResolveTests =
  testGroup
    "resolveFutharkBuild (value semantics live in the language module)"
    [ testCase "no params -> default c backend, no device" $
        resolveFutharkBuild Map.empty @?= Right (Nothing, Nothing)
    , testCase "valid GPU backend" $
        resolveFutharkBuild (lp [("futhark", [("backend", "cuda")])]) @?= Right (Just "cuda", Nothing)
    , testCase "backend + device on a GPU backend" $
        resolveFutharkBuild (lp [("futhark", [("backend", "cuda"), ("device", "#1")])])
          @?= Right (Just "cuda", Just "#1")
    , testCase "unknown backend is rejected" $
        assertLeft (resolveFutharkBuild (lp [("futhark", [("backend", "bogus")])]))
    , testCase "device without a GPU backend is rejected" $
        assertLeft (resolveFutharkBuild (lp [("futhark", [("device", "#1")])]))
    ]

linkFlagTests :: TestTree
linkFlagTests =
  testGroup
    "backendLinkFlags"
    [ testCase "c" $ backendLinkFlags "c" @?= ["-lm"]
    , testCase "multicore" $ backendLinkFlags "multicore" @?= ["-lpthread", "-lm"]
    , testCase "opencl" $ backendLinkFlags "opencl" @?= ["-lOpenCL", "-lm"]
    , testCase "cuda" $ backendLinkFlags "cuda" @?= ["-lcuda", "-lnvrtc", "-lm"]
    , testCase "hip" $ backendLinkFlags "hip" @?= ["-lamdhip64", "-lhiprtc", "-lm"]
    ]

-- P1: a build-parameter change must change the cache-key salt, otherwise a pool
-- built one way could serve results cached from a build built another way.
saltTests :: TestTree
saltTests =
  testGroup
    "renderSalt (P1 cache-key regression)"
    [ testCase "different backend -> different salt" $
        assertBool "salts must differ" $
          renderSalt (lp [("futhark", [("backend", "c")])]) /= renderSalt (lp [("futhark", [("backend", "cuda")])])
    , testCase "different cpp flags -> different salt" $
        assertBool "salts must differ" $
          renderSalt (lp [("cpp", [("flags", "-O2")])]) /= renderSalt (lp [("cpp", [("flags", "-O3")])])
    , testCase "same params -> same salt (deterministic)" $
        renderSalt (lp [("futhark", [("backend", "cuda")])]) @?= renderSalt (lp [("futhark", [("backend", "cuda")])])
    ]

-- P2: writing lang-params must round-trip and must not drop sibling fields when
-- one of them is later changed (as `morloc init` does with sanitize).
buildConfigRoundTripTests :: TestTree
buildConfigRoundTripTests =
  testGroup
    "BuildConfig round-trip (P2)"
    [ testCase "lang-params survive an encode/decode" $ do
        let bc = BuildConfig (Just True) (Just False) (Just (lp [("futhark", [("backend", "cuda")])]))
        case Aeson.decode (Aeson.encode bc) of
          Just bc' -> buildConfigLangParams bc' @?= buildConfigLangParams bc
          Nothing -> assertFailure "failed to decode encoded BuildConfig"
    , testCase "changing sanitize preserves lang-params" $ do
        let bc = BuildConfig Nothing (Just False) (Just (lp [("cpp", [("flags", "-march=native")])]))
            -- mirror the read-modify-write morloc init performs
            bc2 = bc {buildConfigSanitize = Just True}
        case Aeson.decode (Aeson.encode bc2) of
          Just bc' -> do
            buildConfigLangParams bc' @?= buildConfigLangParams bc
            buildConfigSanitize bc' @?= Just True
          Nothing -> assertFailure "failed to decode encoded BuildConfig"
    ]
