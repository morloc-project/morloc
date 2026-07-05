{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PatternChainTests
Description : Unit tests for the PatternChain grammar and Selector bridge
-}
module PatternChainTests (patternChainTests) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.CodeGenerator.IFile
  ( WalkStep (..)
  , walkStepsToPath
  )
import Morloc.Namespace.Expr (Selector (..))
import Morloc.PatternChain.Grammar
  ( GrammarError (..)
  , LinearStep (..)
  , Path (..)
  , WalkError (..)
  , parsePath
  , placeholderCount
  , renderPath
  , selectorToPath
  , walkPathType
  )
import Morloc.Namespace.Prim (Key (..), TVar (..))
import Morloc.Namespace.Type (NamType (..), TypeU (..))

patternChainTests :: TestTree
patternChainTests = testGroup "PatternChain grammar"
  [ roundTripTests
  , placeholderCountTests
  , malformedInputTests
  , selectorAgreementTests
  , walkerBroadcastTests
  ]

--------------------------------------------------------------------------------
-- Round-trip: parsePath (renderPath p) == Right p
--------------------------------------------------------------------------------

roundTripCases :: [(String, Path)]
roundTripCases =
  [ ("single field",              LinearPath [StepField "foo"])
  , ("single tuple idx zero",     LinearPath [StepTupleIdx 0])
  , ("single tuple idx nonzero",  LinearPath [StepTupleIdx 3])
  , ("large tuple idx",           LinearPath [StepTupleIdx 42])
  , ("single bracket index",      LinearPath [StepBracketIndex])
  , ("single bracket slice",      LinearPath [StepBracketSlice])
  , ("field chain",               LinearPath [StepField "foo", StepField "bar"])
  , ( "mixed chain (field, tuple, bracket)"
    , LinearPath [StepField "x", StepTupleIdx 2, StepBracketIndex])
  , ("underscored field",         LinearPath [StepField "_privateField"])
  , ("digit-containing field",    LinearPath [StepField "field1"])
  , ( "group of two fields"
    , GroupPath []
        [ LinearPath [StepField "x"]
        , LinearPath [StepField "y"]
        ])
  , ( "group after linear prefix"
    , GroupPath [StepField "row"]
        [ LinearPath [StepField "x"]
        , LinearPath [StepField "y"]
        ])
  , ( "nested group"
    , GroupPath []
        [ GroupPath []
            [ LinearPath [StepField "x"]
            , LinearPath [StepField "y"]
            ]
        , LinearPath [StepField "z"]
        ])
  , ( "group with bracket children"
    , GroupPath []
        [ LinearPath [StepBracketIndex]
        , LinearPath [StepBracketIndex]
        ])
  , ( "group with mixed bracket + field children"
    , GroupPath []
        [ LinearPath [StepField "foo", StepBracketSlice]
        , LinearPath [StepField "bar", StepBracketIndex]
        ])
  , ( "field then bracket then field"
    , LinearPath [StepField "row", StepBracketIndex, StepField "name"])
  ]

roundTripTests :: TestTree
roundTripTests = testGroup "renderPath then parsePath is identity"
  [ testCase name $
      case parsePath (renderPath p) of
        Right p' -> p' @?= p
        Left err -> assertFailure $
          "parsePath (renderPath " <> show p <> ") failed with " <> show err
  | (name, p) <- roundTripCases
  ]

--------------------------------------------------------------------------------
-- Placeholder counting
--------------------------------------------------------------------------------

placeholderCountTests :: TestTree
placeholderCountTests = testGroup "placeholderCount"
  [ testCase "no brackets" $
      placeholderCount (LinearPath [StepField "foo"]) @?= 0
  , testCase "one bracket-index consumes 1" $
      placeholderCount (LinearPath [StepBracketIndex]) @?= 1
  , testCase "one bracket-slice consumes 3" $
      placeholderCount (LinearPath [StepBracketSlice]) @?= 3
  , testCase "index followed by slice consumes 4" $
      placeholderCount (LinearPath [StepBracketIndex, StepBracketSlice]) @?= 4
  , testCase "group children accumulate" $
      placeholderCount
        (GroupPath [StepBracketIndex]
          [ LinearPath [StepBracketIndex]
          , LinearPath [StepBracketSlice]
          ]) @?= 5
  , testCase "empty linear prefix in group" $
      placeholderCount
        (GroupPath []
          [ LinearPath [StepField "x"]
          , LinearPath [StepField "y"]
          ]) @?= 0
  ]

--------------------------------------------------------------------------------
-- Malformed input: parsePath rejects
--------------------------------------------------------------------------------

malformedInputTests :: TestTree
malformedInputTests = testGroup "parsePath rejects malformed input"
  [ testCase "empty string" $
      case parsePath "" of
        Left EmptyPath -> return ()
        other -> assertFailure $ "expected Left EmptyPath, got " <> show other
  , testCase "trailing dot" $           assertLeft (parsePath ".")
  , testCase "double dot" $             assertLeft (parsePath "..foo")
  , testCase "leading digit-only bad" $ assertLeft (parsePath ".01")
  , testCase "unclosed bracket" $       assertLeft (parsePath ".[")
  , testCase "unclosed slice bracket" $ assertLeft (parsePath ".[:")
  , testCase "step after group" $       assertLeft (parsePath ".(.x;.y).z")
  , testCase "unclosed group" $         assertLeft (parsePath ".(.x;.y")
  , testCase "bare left paren" $        assertLeft (parsePath ".(")
  ]

assertLeft :: Either GrammarError Path -> Assertion
assertLeft (Right p) = assertFailure $ "expected Left, got Right " <> show p
assertLeft (Left _)  = return ()

--------------------------------------------------------------------------------
-- Selector agreement: Grammar.renderPath . selectorToPath
--                     ==
--                     IFile.walkStepsToPath . (pure) selectorToWalkSteps
--
-- The comparison keeps both encodings from drifting. We do NOT invoke
-- IFile.selectorToWalkSteps directly (it lives in the MorlocMonad) --
-- instead each case declares the expected wire string, and both sides
-- are checked against it.
--------------------------------------------------------------------------------

selectorAgreementCases :: [(String, Selector, T.Text, [WalkStep])]
selectorAgreementCases =
  [ ( "single field"
    , SelectorKey ("foo", SelectorEnd) []
    , ".foo"
    , [WalkKey "foo"])
  , ( "chained fields"
    , SelectorKey ("x", SelectorKey ("y", SelectorEnd) []) []
    , ".x.y"
    , [WalkKey "x", WalkKey "y"])
  , ( "single tuple index"
    , SelectorIdx (0, SelectorEnd) []
    , ".0"
    , [WalkField 0])
  , ( "single bracket-index"
    , SelectorBracketIndex SelectorEnd
    , ".[]"
    , [WalkBracketIndex])
  , ( "field then bracket-index"
    , SelectorKey ("data", SelectorBracketIndex SelectorEnd) []
    , ".data.[]"
    , [WalkKey "data", WalkBracketIndex])
  , ( "field then bracket-slice"
    , SelectorKey ("data", SelectorBracketSlice) []
    , ".data.[:]"
    , [WalkKey "data", WalkBracketSlice])
  , ( "group of two fields"
    , SelectorKey ("x", SelectorEnd) [("y", SelectorEnd)]
    , ".(.x;.y)"
    , [WalkGroup [[WalkKey "x"], [WalkKey "y"]]])
  , ( "group of two tuple indices"
    , SelectorIdx (0, SelectorEnd) [(1, SelectorEnd)]
    , ".(.0;.1)"
    , [WalkGroup [[WalkField 0], [WalkField 1]]])
  ]

selectorAgreementTests :: TestTree
selectorAgreementTests = testGroup
  "Grammar.selectorToPath + renderPath == expected wire string"
  [ testGroup name
      [ testCase "renderPath matches expected" $
          case selectorToPath sel of
            Nothing -> assertFailure "selectorToPath returned Nothing"
            Just p  -> renderPath p @?= expected
      , testCase "walkStepsToPath matches expected" $
          walkStepsToPath steps @?= expected
      ]
  | (name, sel, expected, steps) <- selectorAgreementCases
  ]

--------------------------------------------------------------------------------
-- Walker broadcast rule
--
-- `.field` / `.N` steps applied to a list receiver broadcast per the
-- compiler's IntrMap desugar (see Morloc.Frontend.Desugar), so
-- `.[i:j].0` on `[Tuple]` types as `[Tuple.0]` instead of erroring.
-- The Rust `check_pattern_against_schema` mirror is exercised by
-- `runtime-types` unit tests; these cases keep both walkers aligned.
--------------------------------------------------------------------------------

-- | Trivial helpers for building TypeU shapes without the full parser.
intT :: TypeU
intT = VarU (TV "Int")

strT :: TypeU
strT = VarU (TV "Str")

listT :: TypeU -> TypeU
listT a = AppU (VarU (TV "List")) [a]

tupleT :: [TypeU] -> TypeU
tupleT tys =
    let n = length tys
        tName = "Tuple" <> T.pack (show n)
    in AppU (VarU (TV tName)) tys

recordT :: [(T.Text, TypeU)] -> TypeU
recordT fields =
    NamU NamRecord (TV "Anon") []
      [ (Key k, v) | (k, v) <- fields ]

-- | Assert the walker produces a specific result type; use `show` to
-- compare since WalkError has no Eq instance.
assertWalkType :: TypeU -> Path -> TypeU -> Assertion
assertWalkType recv p expected = case walkPathType recv p of
    Right got -> show got @?= show expected
    Left err  -> assertFailure $ "expected " <> show expected
                 <> ", got walker error " <> show err

-- | Parse a path or fail the test; used to keep case bodies flat.
parsePathOrFail :: T.Text -> (Path -> Assertion) -> Assertion
parsePathOrFail src k = case parsePath src of
    Right p  -> k p
    Left err -> assertFailure $ "parsePath " <> show src
                <> " failed: " <> show err

walkerBroadcastTests :: TestTree
walkerBroadcastTests = testGroup "walkPathType broadcast rule"
  [ testCase ".[i:j].0 on [Tuple] broadcasts to [field0_type]" $
      parsePathOrFail ".[:].0" $ \p ->
        assertWalkType (listT (tupleT [strT, intT])) p (listT strT)

  , testCase ".[i:j].outer.inner broadcasts the whole tail chain" $
      parsePathOrFail ".[:].outer.inner" $ \p ->
        let recv = listT (recordT [("outer", recordT [("inner", intT)])])
        in assertWalkType recv p (listT intT)

  , testCase "bare .field on [Record] still errors (runtime doesn't broadcast at root)" $
      parsePathOrFail ".name" $ \p ->
        let recv = listT (recordT [("name", strT)])
        in case walkPathType recv p of
             Left (WalkExpectedRecord _) -> return ()
             other -> assertFailure $ "expected WalkExpectedRecord, got " <> show other

  , testCase ".field on bare scalar still errors" $
      parsePathOrFail ".foo" $ \p ->
        case walkPathType intT p of
          Left (WalkExpectedRecord _) -> return ()
          other -> assertFailure $ "expected WalkExpectedRecord, got " <> show other

  , testCase ".[i] on [T] takes an element (no broadcast at bracket-index)" $
      parsePathOrFail ".[]" $ \p ->
        assertWalkType (listT (tupleT [strT, intT])) p (tupleT [strT, intT])

  , testCase ".[i:j] alone (no tail) preserves list shape" $
      parsePathOrFail ".[:]" $ \p ->
        let recv = listT (recordT [("name", strT)])
        in assertWalkType recv p (listT (recordT [("name", strT)]))
  ]
