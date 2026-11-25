{- |
Module      : VariantMergeTests
Description : Unit tests for merging the occurrences of a `data` type
-}
{-# LANGUAGE OverloadedStrings #-}
module VariantMergeTests (variantMergeTests) where

import Morloc.CodeGenerator.Grammars.Common (mergeEnumOccurrences, mergeVariantOccurrences)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- An occurrence: each arm with its fields as written (here just labels)
-- and as the backend renders them.
type Occ = [(T.Text, ([T.Text], [T.Text]))]

full :: Occ
full = [("Leaf", ([], [])), ("Node", (["Int", "Tree", "Tree"], ["int", "Tree", "Tree"]))]

-- Occurrences tagged by their position, so a test can see which one the
-- merge took its payload from.
tagged :: [Occ] -> [(Int, Occ)]
tagged = zip [0 ..]

arms :: Occ -> [(T.Text, [T.Text])]
arms = map (\(n, (w, _)) -> (n, w))

fails :: Either T.Text a -> Assertion
fails = either (const (return ())) (const (assertFailure "merged"))

variantMergeTests :: TestTree
variantMergeTests =
  testGroup
    "mergeVariantOccurrences (one declaration per data type)"
    [ testCase "one occurrence is itself" $
        mergeVariantOccurrences "Tree" (tagged [full]) @?= Right (0, arms full)
    , testCase "an arm with fields in one occurrence and none in another fails" $
        fails (mergeVariantOccurrences "Tree" (tagged [[("Leaf", ([], [])), ("Node", ([], []))], full]))
    , testCase "no occurrence fails" $
        fails (mergeVariantOccurrences "Tree" (tagged []))
    , testCase "occurrences listing different constructors fail naming both lists" $
        case mergeVariantOccurrences "T" (tagged [[("A", ([], [])), ("B", ([], []))], [("A", ([], [])), ("C", ([], []))]]) of
          Left msg -> do
            assertBool "names the first list" (T.isInfixOf "A, B" msg)
            assertBool "names the second list" (T.isInfixOf "A, C" msg)
          Right r -> assertFailure ("merged to " <> show r)
    , testCase "occurrences listing the constructors in another order fail" $
        fails (mergeVariantOccurrences "Tree" (tagged [full, reverse full]))
    , testCase "arms that render alike merge" $
        mergeVariantOccurrences "Tree"
          (tagged [full, [("Leaf", ([], [])), ("Node", (["Int", "Self", "Self"], ["int", "Tree", "Tree"]))]])
          @?= Right (0, [("Leaf", []), ("Node", ["Int", "Tree", "Tree"])])
    , testCase "arms that render differently fail naming the type, arm and spellings" $
        case mergeVariantOccurrences "Tree"
               (tagged [full, [("Leaf", ([], [])), ("Node", (["Int", "Tree", "Tree"], ["int", "std::vector<Tree>", "Tree"]))]]) of
          Left msg -> do
            assertBool "names the type" (T.isInfixOf "Tree" msg)
            assertBool "names the arm" (T.isInfixOf "Node" msg)
            assertBool "shows the first spelling" (T.isInfixOf "int, Tree, Tree" msg)
            assertBool "shows the second spelling" (T.isInfixOf "int, std::vector<Tree>, Tree" msg)
          Right _ -> assertFailure "two spellings of one arm merged silently"
    , testCase "enum occurrences listing the same constructors merge" $
        mergeEnumOccurrences "Color" [["Red", "Green"], ["Red", "Green"]] @?= Right ["Red", "Green"]
    , testCase "enum occurrences listing different constructors fail" $
        case mergeEnumOccurrences "Color" [["Red", "Green"], ["Red", "Blue"]] of
          Left msg -> assertBool "names both lists" (T.isInfixOf "Red, Green" msg && T.isInfixOf "Red, Blue" msg)
          Right ns -> assertFailure ("merged to " <> show ns)
    ]
