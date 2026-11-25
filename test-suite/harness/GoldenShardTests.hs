{- |
Module      : GoldenShardTests
Description : Unit tests for splitting the golden tests across CI jobs
-}
module GoldenShardTests (goldenShardTests) where

import Data.List (sort)
import GoldenShard (Shard (..), parseShard, selectShard)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Positive (..), testProperty)

goldenShardTests :: TestTree
goldenShardTests =
  testGroup
    "golden test sharding (MORLOC_TEST_SHARD)"
    [ testCase "first of three" $ parseShard "1/3" @?= Right (Shard 1 3)
    , testCase "last of three" $ parseShard "3/3" @?= Right (Shard 3 3)
    , testCase "one of one" $ parseShard "1/1" @?= Right (Shard 1 1)
    , testCase "surrounding whitespace allowed" $ parseShard " 2/4 \n" @?= Right (Shard 2 4)
    , testCase "zero index rejected" $ assertLeft (parseShard "0/3")
    , testCase "index past count rejected" $ assertLeft (parseShard "4/3")
    , testCase "zero count rejected" $ assertLeft (parseShard "1/0")
    , testCase "negative rejected" $ assertLeft (parseShard "-1/3")
    , testCase "missing slash rejected" $ assertLeft (parseShard "13")
    , testCase "empty rejected" $ assertLeft (parseShard "")
    , testCase "trailing junk rejected" $ assertLeft (parseShard "1/3x")
    , testCase "round-robin over the given order" $
        map (\i -> selectShard (Shard i 3) "abcdefg") [1, 2, 3] @?= ["adg", "be", "cf"]
    , testProperty "shards partition the input" $ \(Positive k) xs ->
        let n = 1 + k `mod` 20
            items = zip [0 :: Int ..] (xs :: [Int])
            shards = [selectShard (Shard i n) items | i <- [1 .. n]]
         in sort (concat shards) == items
    ]

assertLeft :: Either String a -> Assertion
assertLeft (Left _) = return ()
assertLeft (Right _) = assertFailure "expected Left"
