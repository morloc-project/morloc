{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : VersionConstraintTests
-- Description : Unit tests for the morloc-version compiler-compatibility gate.
--
-- Mirrored contract: these vectors track
-- @data/rust/morloc-deps/src/constraint.rs@ on version ordering and range
-- admission; the two implement the SAME conda match-spec grammar, so a bare
-- version / @==v@ / @X.Y.*@ all denote the prefix range @[v, next(v))@.
module VersionConstraintTests
  ( versionConstraintTests
  ) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.Version.Constraint
  ( gateModuleVersion
  , inRange
  , parseConstraint
  , parseVersion
  )

-- | Does version @v@ satisfy constraint @spec@? Fails the test if either does
-- not parse (use the dedicated parse tests for rejection cases).
admits :: Text -> Text -> Bool
admits spec v =
  case (parseConstraint spec, parseVersion v) of
    (Right c, Just ver) -> inRange c ver
    _ -> error "admits: expected both to parse"

versionConstraintTests :: TestTree
versionConstraintTests =
  testGroup
    "morloc-version constraint gate"
    [ testGroup
        "version ordering"
        [ testCase "0.9 < 0.10 (numeric, not lexical)" $
            assertBool "0.9 should be below >=0.10" (not (admits ">=0.10" "0.9"))
        , testCase "0.10 >= 0.10" $
            assertBool "0.10 should satisfy >=0.10" (admits ">=0.10" "0.10")
        , testCase "0.98 == 0.98.0 (trailing-zero equal)" $
            assertBool "0.98 in [0.98.0,0.98.0]" (admits ">=0.98.0, <=0.98.0" "0.98")
        , testCase "0.98.2 inside [0.98, 0.99)" $
            assertBool "in range" (admits ">=0.98, <0.99" "0.98.2")
        , testCase "0.99.0 outside [0.98, 0.99)" $
            assertBool "excluded by <0.99" (not (admits ">=0.98, <0.99" "0.99.0"))
        , testCase "strict > excludes equal" $
            assertBool "0.98 not > 0.98" (not (admits ">0.98" "0.98"))
        ]
    , testGroup
        "accepted grammar (unified conda match-spec)"
        [ testCase "* is unbounded" $ assertBool "any" (admits "*" "123.4.5")
        , testCase "empty is unbounded" $ assertBool "any" (admits "" "0.1")
        , testCase "two-sided range parses" $
            assertBool "parses" (isRight (parseConstraint ">=0.98, <0.99"))
        , testCase "bare version = the minor series [0.98, 0.99)" $ do
            assertBool "0.98.5 in series" (admits "0.98" "0.98.5")
            assertBool "0.98 in series" (admits "0.98" "0.98")
            assertBool "0.99 excluded" (not (admits "0.98" "0.99"))
            assertBool "0.97.9 excluded" (not (admits "0.98" "0.97.9"))
        , testCase "==v behaves like the bare prefix range" $ do
            assertBool "0.98.5 in" (admits "==0.98" "0.98.5")
            assertBool "0.99 out" (not (admits "==0.98" "0.99"))
        , testCase "X.Y.* glob = the minor series" $ do
            assertBool "0.98.5 in" (admits "0.98.*" "0.98.5")
            assertBool "0.99 out" (not (admits "0.98.*" "0.99"))
        , testCase "three-segment bare = the patch series [0.98.2, 0.98.3)" $ do
            assertBool "0.98.2 in" (admits "0.98.2" "0.98.2")
            assertBool "0.98.3 out" (not (admits "0.98.2" "0.98.3"))
        ]
    , testGroup
        "rejected input"
        [ testCase "non-numeric constraint segment rejected" $
            assertBool "nonnum" (isLeft (parseConstraint "not-a-version"))
        , testCase "non-numeric version segment rejected" $
            assertBool "nonnum" (parseVersion "0.98.x" == Nothing)
        , testCase "dev suffix is not a plain version" $
            assertBool "devsuffix" (parseVersion "0.98.2-dev" == Nothing)
        ]
    , testGroup
        "gateModuleVersion"
        [ testCase "no constraint -> pass silently" $
            gateModuleVersion "m" "0.98.2" Nothing @?= Right Nothing
        , testCase "compatible -> pass silently" $
            gateModuleVersion "m" "0.98.2" (Just ">=0.98, <0.99") @?= Right Nothing
        , testCase "incompatible -> reject" $
            assertBool "reject" (isLeft (gateModuleVersion "m" "0.98.2" (Just ">=0.99")))
        , testCase "bare series constraint: compatible patch passes" $
            gateModuleVersion "m" "0.98.2" (Just "0.98") @?= Right Nothing
        , testCase "bare series constraint: next minor is rejected" $
            assertBool "reject" (isLeft (gateModuleVersion "m" "0.99.0" (Just "0.98")))
        , testCase "malformed constraint -> reject" $
            assertBool "reject" (isLeft (gateModuleVersion "m" "0.98.2" (Just "not-a-version")))
        , testCase "dev-build compiler version -> warn and pass" $
            case gateModuleVersion "m" "0.98.2-dev" (Just ">=0.99") of
              Right (Just _) -> return ()
              other -> assertFailure ("expected warn+pass, got " ++ show other)
        ]
    ]
