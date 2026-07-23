{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : RefutablePatternTests
Description : Lexer and parse/desugar unit tests for `|`-clause refutable patterns
-}
module RefutablePatternTests (refutablePatternTests) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.Frontend.Lexer (lexMorloc)
import Morloc.Frontend.Parser (emptyPState, readProgram)
import Morloc.Frontend.Token (Located (..), Token (..))

-- | Significant token stream (layout tokens and EOF dropped).
lex' :: Text -> [Token]
lex' src = case lexMorloc "<test>" src of
  Left err -> error ("lex error: " ++ show err)
  Right (toks, _, _) -> [t | Located _ t _ <- toks, keep t]
  where
    keep TokVLBrace = False
    keep TokVRBrace = False
    keep TokVSemi   = False
    keep TokEOF     = False
    keep _          = True

-- | Parse + desugar a module source (no typecheck), collapsing success to
-- unit. A `|`-definition is well-formed here even though its synthesized
-- '==' tests are only resolved later during typechecking.
parseMod :: Text -> Either String ()
parseMod src = () <$ readProgram Nothing Nothing src emptyPState mempty

refutablePatternTests :: TestTree
refutablePatternTests = testGroup "Refutable patterns (`|`-clauses)"
  [ lexerTests
  , parseTests
  , guardInteractionTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer: '|' reservation"
  [ testCase "single '|' lexes as TokPipe" $
      assertEqual "" [TokLowerName "a", TokPipe, TokLowerName "b"] (lex' "a | b")
  , testCase "glued 'a|b' still separates the pipe" $
      assertEqual "" [TokLowerName "a", TokPipe, TokLowerName "b"] (lex' "a|b")
  , testCase "'||' remains a multi-char operator (reservation is single-char)" $
      assertEqual "" [TokLowerName "a", TokOperator "||", TokLowerName "b"] (lex' "a || b")
  ]

parseTests :: TestTree
parseTests = testGroup "parse/desugar"
  [ testCase "well-formed multi-clause definition is accepted" $
      assertBool "expected Right" (isRight (parseMod validDef))
  , testCase "Bool clauses covering {True,False} need no catch-all" $
      assertBool "expected Right" (isRight (parseMod boolExhaustive))
  , testCase "non-exhaustive literals without a catch-all are rejected" $
      assertBool "expected Left" (isLeft (parseMod nonExhaustive))
  , testCase "clauses of differing arity are rejected" $
      assertBool "expected Left" (isLeft (parseMod arityMismatch))
  , testCase "a clause binding the same name twice is rejected" $
      assertBool "expected Left" (isLeft (parseMod dupBinder))
  ]
  where
    validDef = T.unlines
      [ "module main (f)"
      , "f | 0 = 1"
      , "  | n = n"
      ]
    boolExhaustive = T.unlines
      [ "module main (f)"
      , "f | True  = False"
      , "  | False = True"
      ]
    nonExhaustive = T.unlines
      [ "module main (f)"
      , "f | 0 = 1"
      , "  | 1 = 2"
      ]
    arityMismatch = T.unlines
      [ "module main (f)"
      , "f | 0   = 1"
      , "  | 0 1 = 2"
      , "  | n   = 3"
      ]
    dupBinder = T.unlines
      [ "module main (f)"
      , "f | x x = 1"
      , "  | _ _ = 2"
      ]

-- A `|`-clause may carry a `?`/`:` guarded body, occupying the same
-- syntactic slot as a plain `= body`. These check the two features compose
-- and that the guard's internal completeness does not affect pattern
-- exhaustiveness.
guardInteractionTests :: TestTree
guardInteractionTests = testGroup "with `?`-guards"
  [ testCase "a `|`-clause accepts a `?`/`:` guarded body" $
      assertBool "expected Right" (isRight (parseMod guardedClause))
  , testCase "a destructuring pattern feeds its clause guard" $
      assertBool "expected Right" (isRight (parseMod destructureGuard))
  , testCase "several `?`-clauses may share one `|`-clause" $
      assertBool "expected Right" (isRight (parseMod multiGuard))
  , testCase "a guard does not make a refutable pattern exhaustive" $
      assertBool "expected Left" (isLeft (parseMod refutableGuardedLast))
  ]
  where
    guardedClause = T.unlines
      [ "module main (foo)"
      , "foo | 0 = 0"
      , "    | x ? x < 10 = 1"
      , "        : 2"
      ]
    destructureGuard = T.unlines
      [ "module main (cmp)"
      , "cmp | (a, b) ? a < b = 0"
      , "            : 1"
      ]
    multiGuard = T.unlines
      [ "module main (sign)"
      , "sign | x ? x < 0 = 0"
      , "         ? x == 0 = 1"
      , "         : 2"
      ]
    -- last clause's pattern is the literal 0, so it is refutable even though
    -- its guard has a `:` default -- the definition is still non-exhaustive.
    refutableGuardedLast = T.unlines
      [ "module main (f)"
      , "f | 0 ? True = 1"
      , "      : 2"
      ]
