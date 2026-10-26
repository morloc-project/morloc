{-# LANGUAGE OverloadedStrings #-}

-- | The language preference of languages.yaml (COST-16).
module LangRegistryCostTests (langRegistryCostTests) where

import Data.Either (isLeft)
import qualified Morloc.LangRegistry as LR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

langRegistryCostTests :: TestTree
langRegistryCostTests =
  testGroup
    "language preference"
    [ testCase "the first language listed is most preferred" $ rank "cpp" @?= Right 0
    , testCase "a later language ranks after an earlier one" $ rank "r" @?= Right 2
    , testCase "a language not listed comes after every listed one" $ rank "zig" @?= Right 3
    , testCase "the order is required" $
        assertBool "parsed" (isLeft (LR.parseLanguagesYaml "other: 1"))
    ]
  where
    registry = LR.parseLanguagesYaml "language_order: [cpp, py, r]"
    rank l = (`LR.registryPreference` l) <$> registry
