{-# LANGUAGE OverloadedStrings #-}

-- | The language preference of languages.yaml (COST-16).
module LangRegistryCostTests (langRegistryCostTests) where

import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Morloc.LangRegistry as LR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

langRegistryCostTests :: TestTree
langRegistryCostTests =
  testGroup
    "language registry"
    [ testCase "the first language listed is most preferred" $ rank "cpp" @?= Right 0
    , testCase "a later language ranks after an earlier one" $ rank "r" @?= Right 2
    , testCase "a language not listed comes after every listed one" $ rank "zig" @?= Right 3
    , testCase "the order is required" $
        assertBool "parsed" (isLeft (LR.parseLanguagesYaml "other: 1"))
    , testCase "the shipped py and r pools fork their workers, cpp and rust do not" $ do
        files <- mapM (\l -> (,) l <$> TIO.readFile ("binders/" <> l <> "/lang.yaml")) ["py", "r", "cpp", "rust"]
        case LR.buildDefaultRegistry files "language_order: [cpp, py, r, rust]" of
          Left e -> assertFailure e
          Right reg -> map (LR.registryForkWorkers reg . T.pack) ["py", "r", "cpp", "rust"] @?= [True, True, False, False]
    , testCase "a language that does not say forks no workers" $
        case LR.buildDefaultRegistry [("z", "name: z\nextension: z\n")] "language_order: [z]" of
          Left e -> assertFailure e
          Right reg -> LR.registryForkWorkers reg "z" @?= False
    ]
  where
    registry = LR.parseLanguagesYaml "language_order: [cpp, py, r]"
    rank l = (`LR.registryPreference` l) <$> registry
