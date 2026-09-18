{- |
Module      : VariantMergeTests
Description : Unit tests for merging the occurrences of a `data` type
-}
{-# LANGUAGE OverloadedStrings #-}
module VariantMergeTests (variantMergeTests) where

import Morloc.CodeGenerator.Grammars.Common (mergeVariantOccurrences)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- An occurrence: each arm with its fields as written (here just labels)
-- and as the backend renders them.
type Occ = [(T.Text, ([T.Text], [T.Text]))]

full :: Occ
full = [("Leaf", ([], [])), ("Node", (["Int", "Tree", "Tree"], ["int", "Tree", "Tree"]))]

variantMergeTests :: TestTree
variantMergeTests =
  testGroup
    "mergeVariantOccurrences (one declaration per data type)"
    [ testCase "one occurrence is itself" $
        mergeVariantOccurrences "Tree" [full] @?= Right (map (\(n, (w, _)) -> (n, w)) full)
    , testCase "a literal's fieldless arm takes the fields of the full occurrence" $
        mergeVariantOccurrences "Tree" [[("Node", ([], []))], full]
          @?= Right [("Leaf", []), ("Node", ["Int", "Tree", "Tree"])]
    , testCase "declaration order comes from the longer occurrence" $
        mergeVariantOccurrences "Tree" [[("Node", (["Int", "Tree", "Tree"], ["int", "Tree", "Tree"]))], full]
          @?= Right [("Leaf", []), ("Node", ["Int", "Tree", "Tree"])]
    , testCase "arms that render alike merge" $
        mergeVariantOccurrences "Tree"
          [full, [("Leaf", ([], [])), ("Node", (["Int", "Self", "Self"], ["int", "Tree", "Tree"]))]]
          @?= Right [("Leaf", []), ("Node", ["Int", "Tree", "Tree"])]
    , testCase "arms that render differently fail naming the type, arm and spellings" $
        case mergeVariantOccurrences "Tree"
               [full, [("Leaf", ([], [])), ("Node", (["Int", "Tree", "Tree"], ["int", "std::vector<Tree>", "Tree"]))]] of
          Left msg -> do
            assertBool "names the type" (T.isInfixOf "Tree" msg)
            assertBool "names the arm" (T.isInfixOf "Node" msg)
            assertBool "shows the first spelling" (T.isInfixOf "int, Tree, Tree" msg)
            assertBool "shows the second spelling" (T.isInfixOf "int, std::vector<Tree>, Tree" msg)
          Right _ -> assertFailure "two spellings of one arm merged silently"
    , testCase "no occurrence is no declaration" $
        mergeVariantOccurrences "Tree" ([] :: [Occ]) @?= Right []
    ]
