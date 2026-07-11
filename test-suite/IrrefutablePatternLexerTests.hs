{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : IrrefutablePatternLexerTests
Description : Lexer disambiguation tests for '@' as as-pattern vs intrinsic
-}
module IrrefutablePatternLexerTests (irrefutablePatternLexerTests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.Frontend.Lexer (lexMorloc)
import Morloc.Frontend.Token
  ( Located (..)
  , Token (..)
  )

-- | Run the lexer on a fragment and drop layout tokens plus EOF so the tests
-- can pattern-match against the significant token stream directly.
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

expectTokens :: String -> Text -> [Token] -> TestTree
expectTokens name src expected =
  testCase name $ assertEqual "" expected (lex' src)

irrefutablePatternLexerTests :: TestTree
irrefutablePatternLexerTests =
  testGroup "Irrefutable-pattern lexer disambiguation"
    [ testGroup "'@' glued to a preceding identifier -> TokAt"
        [ expectTokens
            "foo@bar -> LOWER, At, LOWER"
            "foo@bar"
            [TokLowerName "foo", TokAt, TokLowerName "bar"]

        , expectTokens
            "foo@(x, y) -> LOWER, At, LParen, ..., RParen"
            "foo@(x, y)"
            [ TokLowerName "foo", TokAt, TokLParen
            , TokLowerName "x", TokComma, TokLowerName "y", TokRParen
            ]

        , expectTokens
            "foo@{a=x} -> LOWER, At, LBrace, ..., RBrace"
            "foo@{a=x}"
            [ TokLowerName "foo", TokAt, TokLBrace
            , TokLowerName "a", TokEquals, TokLowerName "x", TokRBrace
            ]

        , expectTokens
            "foo@_ -> LOWER, At, Underscore"
            "foo@_"
            [TokLowerName "foo", TokAt, TokUnderscore]

        , expectTokens
            "nested q@(a, r@(b, c))"
            "q@(a, r@(b, c))"
            [ TokLowerName "q", TokAt, TokLParen
            , TokLowerName "a", TokComma
            , TokLowerName "r", TokAt, TokLParen
            , TokLowerName "b", TokComma, TokLowerName "c"
            , TokRParen, TokRParen
            ]
        ]

    , testGroup "'@name' at token-start position -> TokIntrinsic"
        [ expectTokens
            "@stdout at line start -> Intrinsic"
            "@stdout"
            [TokIntrinsic "stdout"]

        , expectTokens
            "@stdin @stdout in sequence"
            "@stdin @stdout"
            [TokIntrinsic "stdin", TokIntrinsic "stdout"]

        , expectTokens
            "foo @bar with whitespace -> LOWER, Intrinsic"
            "foo @bar"
            [TokLowerName "foo", TokIntrinsic "bar"]

        , expectTokens
            "'(' @bar -> LParen, Intrinsic (parens are not operand-finishing)"
            "( @bar"
            [TokLParen, TokIntrinsic "bar"]

        , expectTokens
            "',' @bar -> Comma, Intrinsic"
            ", @bar"
            [TokComma, TokIntrinsic "bar"]
        ]

    , testGroup "'f@stdout' tightening (previously legal, now not)"
        [ -- Pre-change: 'f@stdout' lexed as [LOWER "f", Intrinsic "stdout"]
          -- (an application of f to the intrinsic). Post-change: '@'
          -- glued to a preceding identifier is TokAt, so this now lexes as
          -- an as-pattern head that will fail downstream in a value
          -- position. Users who wanted 'f @stdout' must now use the space.
          expectTokens
            "f@stdout no longer lexes as [f, @stdout]"
            "f@stdout"
            [TokLowerName "f", TokAt, TokLowerName "stdout"]

        , expectTokens
            "f @stdout (space) still lexes as [f, @stdout]"
            "f @stdout"
            [TokLowerName "f", TokIntrinsic "stdout"]
        ]

    , testGroup "'@' fallthrough positions"
        [ -- Bare '@' with no operand-finishing predecessor and no lowercase
          -- follower falls through to the general operator lexer, so it
          -- reaches the parser as TokOperator "@". Nothing binds it, so
          -- the parser reports it as an undefined operator downstream.
          expectTokens
            "bare '@' with trailing space -> TokOperator (no rule matched)"
            "@ "
            [TokOperator "@"]

        , expectTokens
            "')' @foo -> RParen, Intrinsic (space breaks the glue rule)"
            ") @foo"
            [TokRParen, TokIntrinsic "foo"]

        , expectTokens
            "'_' @foo -> Underscore, Intrinsic (space breaks the glue rule)"
            "_ @foo"
            [TokUnderscore, TokIntrinsic "foo"]
        ]
    ]
