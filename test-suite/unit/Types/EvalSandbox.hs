{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.EvalSandbox
  ( evalSandboxTests
  ) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Types.Common

-- | Gate 1 of the eval sandbox: a directly-written IO intrinsic is refused,
-- while pure intrinsics and trusted (dev) eval are unaffected. The check runs
-- before typecheck, so these probes need no valid types or imports. Gate 2 (the
-- module allow-list) needs installed modules and is covered by the
-- `eval-sandbox` golden.
evalSandboxTests :: TestTree
evalSandboxTests =
  testGroup "eval sandbox IO-intrinsic gate"
    [ testCase "direct IO intrinsic (@tell) rejected under sandbox" $
        runEvalSandbox [] "module main (x)\nx = @tell" >>= assertLeft
    , testCase "IO intrinsic nested in a tuple rejected under sandbox" $
        runEvalSandbox [] "module main (x)\nx = (@tell, 0)" >>= assertLeft
    , testCase "non-IO intrinsic (@lang) allowed under sandbox" $
        runEvalSandbox [] "module main (x)\nx = @lang" >>= assertRight
    , testCase "IO intrinsic (@tell) allowed under trusted dev eval" $
        runEvalTrusted "module main (x)\nx = @tell" >>= assertRight
    ]
  where
    assertLeft res = case res of
      Left _ -> return ()
      Right _ -> assertFailure "expected the sandbox to reject this eval expression"
    assertRight res = case res of
      Right _ -> return ()
      Left e -> assertFailure ("expected acceptance, got error: " <> show e)
