{- |
Module      : SystemConfigTests
Description : Unit tests for the conda toolchain-coherence path check

Guards 'Morloc.CodeGenerator.SystemConfig.pathIsWithin', the pure predicate
behind the native-backend coherence guard: a build tool is coherent only when
its directory is the conda prefix itself or nested under it. The subtle case is
a sibling whose name merely shares a textual prefix (@/opt/env@ vs @/opt/env2@),
which must NOT count as "within" -- a naive @isPrefixOf@ without the trailing
separator would wrongly accept it and let a foreign toolchain pass the guard.
-}
module SystemConfigTests (systemConfigTests) where

import Morloc.CodeGenerator.SystemConfig (pathIsWithin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

systemConfigTests :: TestTree
systemConfigTests =
  testGroup
    "SystemConfig.pathIsWithin (conda coherence guard)"
    [ testCase "the prefix itself is within" $
        assertBool "prefix == child" (pathIsWithin "/opt/env" "/opt/env")
    , testCase "a nested directory is within" $
        assertBool "child under prefix" (pathIsWithin "/opt/env" "/opt/env/bin")
    , testCase "a deeply nested directory is within" $
        assertBool "deep child" (pathIsWithin "/opt/env" "/opt/env/x86_64-conda/bin")
    , testCase "a host directory is NOT within" $
        assertBool "system path" (not (pathIsWithin "/opt/env" "/usr/bin"))
    , testCase "a name-sharing sibling is NOT within" $
        -- The load-bearing case: /opt/env2 must not be accepted just because it
        -- starts with the string "/opt/env".
        assertBool "sibling prefix" (not (pathIsWithin "/opt/env" "/opt/env2/bin"))
    , testCase "a parent directory is NOT within" $
        assertBool "parent" (not (pathIsWithin "/opt/env" "/opt"))
    ]
