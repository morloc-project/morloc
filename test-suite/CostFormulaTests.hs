{-# LANGUAGE OverloadedStrings #-}

-- | The formulas of cost directives (COST-7): what they parse to, their
-- dimensions, and their values.
module CostFormulaTests (costFormulaTests) where

import Data.Either (isLeft)
import Data.Text (Text)
import Morloc.Cost.Formula
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

costFormulaTests :: TestTree
costFormulaTests =
  testGroup
    "cost formulas"
    [ testCase "a time literal is in seconds" $ value "10ns" @?= Just 1e-8
    , testCase "a byte literal is in bytes" $ value "2KiB" @?= Just 2048
    , testCase "a unit may follow a space" $ value "3 ms" @?= Just 3e-3
    , testCase "decimal units are powers of 1000" $ value "1.5MB" @?= Just 1.5e6
    , testCase "a name takes its value" $ value "10ns * n" @?= Just 1e-7
    , testCase "multiplication binds tighter than addition" $ value "1 + 2 * 3" @?= Just 7
    , testCase "a power binds tighter than multiplication and to the right" $
        value "2 * 2 ^ 3 ^ 2" @?= Just 1024
    , testCase "logarithms" $ value "log2 8 + log 1" @?= Just 3
    , testCase "parentheses" $ value "(1 + 2) * 3" @?= Just 9
    , testCase "a negative value counts as no value" $ value "1 - 2" @?= Nothing
    , testCase "an infinite value counts as no value" $ value "1 / 0" @?= Nothing
    , testCase "a time is a time" $ dim "10ns * n" @?= Right time
    , testCase "bytes over time" $ dim "4MB / 2s" @?= Right (Dim (-1) 1)
    , testCase "an integer power of bytes" $ dim "n * 1B ^ 2" @?= Right (Dim 0 2)
    , testCase "adding a time to a plain number has no dimension" $
        assertBool "dimension" (isLeft (dim "10ns * a + k * n"))
    , testCase "a logarithm of a time has no dimension" $
        assertBool "dimension" (isLeft (dim "log 1s"))
    , testCase "a time raised to a name has no dimension" $
        assertBool "dimension" (isLeft (dim "1s ^ n"))
    , testCase "a name beside a number without an operator is refused" $
        assertBool "parsed" (isLeft (parseFormula "10 n"))
    , testCase "an unknown unit is refused" $
        assertBool "parsed" (isLeft (parseFormula "10 parsecs"))
    , testCase "the names of a formula" $
        formulaNames <$> parseFormula "a * log b + a" @?= Right ["a", "b"]
    ]
  where
    env :: Text -> Double
    env "n" = 10
    env _ = 1
    value src = either (const Nothing) (evalFormula env) (parseFormula src)
    dim src = parseFormula src >>= dimOf
