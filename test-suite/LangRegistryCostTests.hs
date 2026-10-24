{-# LANGUAGE OverloadedStrings #-}

-- | The cost coefficients of languages.yaml (COST-10): which entry prices a
-- crossing, and which keys the file must state.
module LangRegistryCostTests (langRegistryCostTests) where

import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Morloc.LangRegistry as LR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

langRegistryCostTests :: TestTree
langRegistryCostTests =
  testGroup
    "language cost coefficients"
    [ testCase "the exact pair wins" $ latency "cpp" "py" @?= Right 1
    , testCase "(*, to) wins over (from, *)" $ latency "cpp" "r" @?= Right 2
    , testCase "(from, *) wins over (*, *)" $ latency "rust" "cpp" @?= Right 3
    , testCase "(*, *) wins over the default" $ latency "py" "cpp" @?= Right 4
    , testCase "the order of entries does not matter" $
        (\reg -> LR.registryCrossingLatency reg "cpp" "py") <$> LR.parseLanguagesYaml (T.unlines (take 3 table <> reverse (drop 3 table))) @?= Right 1
    , testCase "a pair nothing matches costs the default" $
        (\reg -> LR.registryCrossingLatency reg "py" "cpp")
          <$> LR.parseLanguagesYaml (T.unlines ["default_call_overhead_ns: 7", "default_crossing_latency_ns: 9"])
          @?= Right 9
    , testCase "a language with no overhead entry costs the default" $
        (\reg -> LR.registryCallOverhead reg "zig") <$> registry @?= Right 7
    , testCase "the default crossing latency is required" $
        assertBool "parsed" (isLeft (LR.parseLanguagesYaml "default_call_overhead_ns: 7"))
    , testCase "the default call overhead is required" $
        assertBool "parsed" (isLeft (LR.parseLanguagesYaml "default_crossing_latency_ns: 9"))
    ]
  where
    table =
      [ "default_call_overhead_ns: 7"
      , "default_crossing_latency_ns: 9"
      , "crossing_latency_ns:"
      , "  - {from: \"*\", to: \"*\", ns: 4}"
      , "  - {from: rust, to: \"*\", ns: 3}"
      , "  - {from: cpp, to: \"*\", ns: 5}"
      , "  - {from: \"*\", to: r, ns: 2}"
      , "  - {from: cpp, to: py, ns: 1}"
      ]
    registry = LR.parseLanguagesYaml (T.unlines table)
    latency from to = (\reg -> LR.registryCrossingLatency reg from to) <$> registry
