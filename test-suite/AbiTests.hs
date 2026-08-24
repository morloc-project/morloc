{- |
Module      : AbiTests
Description : Unit test for the ABI/wire-format version parsed from morloc.h

Guards the 'Morloc.Abi.abiVersion' parse of the embedded @morloc.h@. The
cross-language lockstep (Rust mirror == header) is enforced separately by the
Rust @abi_version_matches_header@ test; because 'abiVersion' reads the same
header, Haskell and Rust cannot drift from each other.
-}
module AbiTests (abiTests) where

import Morloc.Abi (abiVersion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

abiTests :: TestTree
abiTests =
  testGroup
    "Morloc.Abi (ABI/wire-format version)"
    [ testCase "abiVersion parses from morloc.h and is positive" $
        -- Forcing the value catches a parse failure (which would `error`),
        -- and >= 1 confirms a sane version was read rather than a default.
        assertBool
          ("abiVersion should be >= 1, got " <> show abiVersion)
          (abiVersion >= 1)
    ]
