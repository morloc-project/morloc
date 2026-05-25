{- |
Module      : SizeParseTests
Description : Unit tests for the human-readable byte-count parser
-}
module SizeParseTests (sizeParseTests) where

import Data.Int (Int64)
import Morloc.Data.Size (parseHumanSize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

sizeParseTests :: TestTree
sizeParseTests =
  testGroup
    "parseHumanSize (morloc make --inline-size argument parser)"
    [ testCase "bare zero" $ parseHumanSize "0" @?= Right 0
    , testCase "small bare integer" $ parseHumanSize "64" @?= Right 64
    , testCase "lowercase k" $ parseHumanSize "64k" @?= Right (64 * 1024)
    , testCase "uppercase K" $ parseHumanSize "64K" @?= Right (64 * 1024)
    , testCase "lowercase m" $ parseHumanSize "4m" @?= Right (4 * 1024 * 1024)
    , testCase "uppercase M" $ parseHumanSize "4M" @?= Right (4 * 1024 * 1024)
    , testCase "lowercase g" $ parseHumanSize "1g" @?= Right (1024 * 1024 * 1024)
    , testCase "uppercase G" $ parseHumanSize "1G" @?= Right (1024 * 1024 * 1024)
    , testCase "round-trip default (64k)" $
        parseHumanSize "64k" @?= Right (64 * 1024 :: Int64)
    , testCase "empty input rejected" $
        assertLeft (parseHumanSize "")
    , testCase "lone suffix rejected" $
        assertLeft (parseHumanSize "k")
    , testCase "negative rejected" $
        assertLeft (parseHumanSize "-1")
    , testCase "unknown suffix rejected" $
        assertLeft (parseHumanSize "64x")
    , testCase "trailing garbage rejected" $
        assertLeft (parseHumanSize "64kb")
    , testCase "non-digit prefix rejected" $
        assertLeft (parseHumanSize "+64")
    , testCase "overflow rejected" $
        -- 8 EiB worth of bytes -> overflows Int64.
        assertLeft (parseHumanSize "9999999999g")
    ]

assertLeft :: Either String Int64 -> Assertion
assertLeft (Right n) = assertFailure $ "expected Left, got Right " <> show n
assertLeft (Left _)  = return ()
