{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AbiTests
Description : Unit test for the ABI/wire-format version parsed from morloc.h

Guards the 'Morloc.Abi.abiVersion' parse of the embedded @morloc.h@ and the
handle kinds the compiler emits against the header's @MLC_KIND_*@.
-}
module AbiTests (abiTests) where

import Morloc.Abi (abiVersion, headerDefine)
import qualified Morloc.BaseTypes as MBT
import qualified Morloc.DataFiles as DF
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
    , testCase "the handle kinds the compiler emits are the ones morloc.h defines" $ do
        let defined name = headerDefine name DF.libmorlocHeader
        defined "MLC_KIND_IFILE" @?= Just (fromIntegral MBT.mlcKindIFile)
        defined "MLC_KIND_ISTREAM" @?= Just (fromIntegral MBT.mlcKindIStream)
        defined "MLC_KIND_OSTREAM" @?= Just (fromIntegral MBT.mlcKindOStream)
    ]
