{- |
Module      : SystemConfigTests
Description : Unit tests for the conda toolchain-coherence guard

Guards the two pure decisions behind the native-backend coherence check.

'Morloc.CodeGenerator.SystemConfig.pathIsWithin' decides whether a build tool is
coherent: only when its directory is the conda prefix itself or nested under it.
The subtle case is a sibling whose name merely shares a textual prefix
(@/opt/env@ vs @/opt/env2@), which must NOT count as "within" -- a naive
@isPrefixOf@ without the trailing separator would wrongly accept it and let a
foreign toolchain pass the guard.

'Morloc.CodeGenerator.SystemConfig.compilerIncoherence' decides WHY a C/C++
compiler resolved outside the prefix, which is what the reported error tells the
user to go fix: a tool the env owns but PATH order lost, or an environment that
activated no compiler at all.
-}
module SystemConfigTests (systemConfigTests) where

import Morloc.CodeGenerator.SystemConfig (Incoherence (..), compilerIncoherence, pathIsWithin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

systemConfigTests :: TestTree
systemConfigTests =
  testGroup
    "SystemConfig (conda coherence guard)"
    [ pathIsWithinTests
    , compilerIncoherenceTests
    ]

pathIsWithinTests :: TestTree
pathIsWithinTests =
  testGroup
    "pathIsWithin"
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

compilerIncoherenceTests :: TestTree
compilerIncoherenceTests =
  testGroup
    "compilerIncoherence"
    [ testCase "an env copy losing the PATH race is shadowed" $
        Shadowed @=? compilerIncoherence True False
    , testCase "a $CC naming a compiler outside the env is shadowed" $
        Shadowed @=? compilerIncoherence False True
    , testCase "both together are still shadowed" $
        Shadowed @=? compilerIncoherence True True
    , testCase "no env compiler and no $CC means the activation never ran" $
        -- A macOS conda env has no bin/gcc at all (its compiler is clang, reached
        -- only through $CC), so an unset $CC here is a failed activation, NOT a
        -- host copy shadowing anything -- and telling the user to reorder PATH
        -- would send them after a cause that does not exist.
        Unactivated @=? compilerIncoherence False False
    ]
