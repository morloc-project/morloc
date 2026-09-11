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
  , constructorPatternTests
  , payloadPatternTests
  , dataDeclarationTests
  ]

dataDeclarationTests :: TestTree
dataDeclarationTests = testGroup "data declarations"
  [ -- The command line matches a constructor without regard to case, so
    -- two that differ only in case could not be told apart there.
    testCase "two constructors differing only in case are rejected" $
      assertBool "Red and RED collide" . isLeft $ parseMod
        "module main (f)\n\
        \data Color = Red | RED\n\
        \f :: Color -> Int\n\
        \f | _ = 0\n"

  , testCase "a described constructor is accepted" $
      assertBool "prose above a constructor" . isRight $ parseMod
        "module main (f)\n\
        \--' A color\n\
        \data Color\n\
        \  --' warm\n\
        \  = Red\n\
        \  --' cool\n\
        \  | Blue\n\
        \f :: Color -> Int\n\
        \f | _ = 0\n"

  , -- A constructor's docstring may sit above the name itself when the
    -- bar ends the line before it.
    testCase "a constructor described on its own line is accepted" $
      assertBool "docstring above the name" . isRight $ parseMod
        "module main (f)\n\
        \data Color =\n\
        \  --' warm\n\
        \  Red |\n\
        \  --' cool\n\
        \  Blue\n\
        \f :: Color -> Int\n\
        \f | _ = 0\n"

  , -- A constructor is a value, not an argument: the directives that shape
    -- an argument have no meaning on it.
    testCase "a directive on a constructor is rejected" $
      assertBool "@arg on Red" . isLeft $ parseMod
        "module main (f)\n\
        \data Color\n\
        \  --' @arg -r/--red\n\
        \  = Red\n\
        \  | Blue\n\
        \f :: Color -> Int\n\
        \f | _ = 0\n"
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer: '|' reservation"
  [ testCase "single '|' lexes as TokPipe" $
      assertEqual "" [TokLowerName "a", TokPipe, TokLowerName "b"] (lex' "a | b")
  , testCase "glued 'a|b' still separates the pipe" $
      assertEqual "" [TokLowerName "a", TokPipe, TokLowerName "b"] (lex' "a|b")
  , testCase "'||' remains a multi-char operator (reservation is single-char)" $
      assertEqual "" [TokLowerName "a", TokOperator "||", TokLowerName "b"] (lex' "a || b")
  , testCase "an alias-qualified constructor lexes as a namespace dot" $
      assertEqual "" [TokLowerName "p", TokNsDot, TokUpperName "Red"] (lex' "p.Red")
  , testCase "a composition written with spaces is not a qualified name" $
      assertEqual "" [TokLowerName "f", TokDot, TokUpperName "Just"] (lex' "f . Just")
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

-- Constructor patterns over a nullary `data`.
--
-- These pin the three ways the existing `|`-clause machinery goes wrong if
-- constructors become terms without matching pattern support. Each failure
-- is silent, so each gets a test:
--
--   1. 'exprToRefutPat' maps a bare 'CVarE' to 'CRPatVar' -- a binder that
--      matches everything. An UPPER constructor must not take that path.
--   2. 'checkRefutCoverage' asks 'refutPatHasLit', which knows only about
--      literals, so a constructor clause would read as an irrefutable
--      catch-all and a non-exhaustive definition would be accepted.
--   3. 'assembleCascade' then drops the final clause's condition outright,
--      so the last constructor arm would answer for every unmatched input.
--
-- `Bool` already works this way ('boolExhaustive'), which is the precedent
-- an n-constructor set generalizes.
--
-- NOTE while this is red: `data` does not parse yet, so the rejection cases
-- pass vacuously. They only mean something once the acceptance cases pass.
constructorPatternTests :: TestTree
constructorPatternTests = testGroup "constructor patterns"
  [ testCase "constructor clauses parse" $
      assertBool "complement over DNA" . isRight $ parseMod
        "module main (f)\n\
        \data DNA = A | C | G | T\n\
        \f :: DNA -> DNA\n\
        \f | A = T\n\
        \  | C = G\n\
        \  | G = C\n\
        \  | T = A\n"

  , testCase "a complete clause set needs no catch-all" $
      assertBool "all three constructors covered" . isRight $ parseMod
        "module main (f)\n\
        \data Color = Red | Green | Blue\n\
        \f :: Color -> Int\n\
        \f | Red = 0\n\
        \  | Green = 1\n\
        \  | Blue = 2\n"

  , -- A qualified lowercase name is a term from another module; in pattern
    -- position it would otherwise become a binder named `p.x` that matches
    -- everything and silently deadens every clause after it.
    testCase "a qualified lowercase name cannot be a pattern variable" $
      assertBool "p.x is not a binder" . isLeft $ parseMod
        "module main (f)\n\
        \import other as p\n\
        \f :: Int -> Int\n\
        \f | p.x = 0\n\
        \  | _ = 1\n"

  , testCase "an alias-qualified constructor pattern is accepted" $
      assertBool "p.Red names a constructor" . isRight $ parseMod
        "module main (f)\n\
        \import other as p\n\
        \f :: Int -> Int\n\
        \f | p.Red = 0\n\
        \  | _ = 1\n"

  , -- The irrefutable side of the same trap: `go Red = ...` would bind a
    -- variable named `Red` and match every input.
    testCase "a constructor cannot be a binding pattern" $
      assertBool "Red is not a binder" . isLeft $ parseMod
        "module main (f)\n\
        \data Color = Red | Green\n\
        \f :: Color -> Int\n\
        \f Red = 0\n"

  , testCase "an alias-qualified constructor cannot be a binding pattern" $
      assertBool "p.Red is not a binder" . isLeft $ parseMod
        "module main (f)\n\
        \import other as p\n\
        \f :: Int -> Int\n\
        \f p.Red = 0\n"

  , -- A dotted name whose qualifier is no import alias is most likely a
    -- composition written without spaces.
    testCase "a qualifier that is not an import alias is rejected in an expression" $
      assertBool "area.Circle is not a qualified name" . isLeft $ parseMod
        "module main (f)\n\
        \f :: Int -> Int\n\
        \f = area.Circle\n"

  , -- An operator's name may contain a dot without being qualified.
    testCase "the composition operator is still a value" $
      assertBool "(.) is not a qualified name" . isRight $ parseMod
        "module main (f)\n\
        \g :: Int -> Int\n\
        \g x = x\n\
        \f :: Int -> Int\n\
        \f = (.) g g\n"

  , testCase "a constructor qualified by an undeclared alias is rejected" $
      assertBool "no import is aliased q" . isLeft $ parseMod
        "module main (f)\n\
        \import other as p\n\
        \f :: Int -> Int\n\
        \f | q.Red = 0\n\
        \  | _ = 1\n"

  , -- Trap 2/3: this must be rejected. If it is accepted, `f Blue` silently
    -- returns the Green arm, because the last clause's test is discarded.
    testCase "an incomplete clause set is rejected" $
      assertBool "Blue is unmatched" . isLeft $ parseMod
        "module main (f)\n\
        \data Color = Red | Green | Blue\n\
        \f :: Color -> Int\n\
        \f | Red = 0\n\
        \  | Green = 1\n"

  , testCase "an incomplete clause set with a catch-all is accepted" $
      assertBool "wildcard covers the rest" . isRight $ parseMod
        "module main (f)\n\
        \data Color = Red | Green | Blue\n\
        \f :: Color -> Int\n\
        \f | Red = 0\n\
        \  | _ = 1\n"

  , testCase "a repeated constructor clause is rejected" $
      assertBool "Red matched twice" . isLeft $ parseMod
        "module main (f)\n\
        \data Color = Red | Green | Blue\n\
        \f :: Color -> Int\n\
        \f | Red = 0\n\
        \  | Red = 1\n\
        \  | Green = 2\n\
        \  | Blue = 3\n"

  , testCase "a clause after a catch-all is rejected" $
      assertBool "Blue is unreachable" . isLeft $ parseMod
        "module main (f)\n\
        \data Color = Red | Green | Blue\n\
        \f :: Color -> Int\n\
        \f | Red = 0\n\
        \  | c = 1\n\
        \  | Blue = 2\n"

  , -- Trap 1: a lowercase head binds, an UPPER head tests. If the UPPER
    -- case fell through to CRPatVar, this would be a complete definition
    -- and the second clause would be unreachable rather than rejected.
    testCase "a lowercase binder is not a constructor test" $
      assertBool "x binds and shadows the rest" . isLeft $ parseMod
        "module main (f)\n\
        \data Color = Red | Green | Blue\n\
        \f :: Color -> Int\n\
        \f | x = 0\n\
        \  | Green = 1\n"

  , testCase "constructor patterns nest inside a tuple" $
      assertBool "pair of constructors" . isRight $ parseMod
        "module main (f)\n\
        \data Color = Red | Green\n\
        \f :: (Color, Color) -> Int\n\
        \f | (Red, Red) = 0\n\
        \  | (a, b) = 1\n"

  , testCase "constructor patterns nest inside a record" $
      assertBool "constructor in a field position" . isRight $ parseMod
        "module main (f)\n\
        \data Color = Red | Green\n\
        \record Cell where\n\
        \  hue :: Color\n\
        \  n :: Int\n\
        \f :: Cell -> Int\n\
        \f | {hue = Red, n = k} = k\n\
        \  | c = 0\n"

  , testCase "an as-pattern may bind a constructor match" $
      assertBool "label@Red" . isRight $ parseMod
        "module main (f)\n\
        \data Color = Red | Green\n\
        \f :: Color -> Color\n\
        \f | c@Red = c\n\
        \  | Green = Green\n"
  ]

-- Constructor patterns that bind a payload.
--
-- `(Circle r)` is two things at once: a tag test on the scrutinee and a
-- binding of the constructor's field to `r`. The argument-free tier needed
-- only the first half.
--
-- NOTE while this is red: payload arms are rejected in desugar, so the
-- acceptance cases fail and the rejection cases pass vacuously.
payloadPatternTests :: TestTree
payloadPatternTests = testGroup "constructor patterns with payloads"
  [ testCase "a payload pattern binds its field" $
      assertBool "Circle r binds r" . isRight $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Dot\n\
        \f :: Shape -> Real\n\
        \f | (Circle r) = r\n\
        \  | Dot = 0.0\n"

  , testCase "a multi-field payload pattern binds every field" $
      assertBool "Rect w h binds both" . isRight $ parseMod
        "module main (f)\n\
        \data Shape = Rect Real Real | Dot\n\
        \f :: Shape -> Real\n\
        \f | (Rect w h) = w\n\
        \  | Dot = 0.0\n"

  , testCase "a payload field may be matched rather than bound" $
      assertBool "nested literal inside a constructor" . isRight $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Dot\n\
        \f :: Shape -> Str\n\
        \f | (Circle 0.0) = \"degenerate\"\n\
        \  | (Circle r) = \"round\"\n\
        \  | Dot = \"dot\"\n"

  , testCase "a wildcard may stand in for a payload field" $
      assertBool "Circle _ ignores the field" . isRight $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Dot\n\
        \f :: Shape -> Str\n\
        \f | (Circle _) = \"round\"\n\
        \  | Dot = \"dot\"\n"

  , -- Coverage is still by constructor: naming every one closes the set
    -- whether or not the arms carry payloads.
    testCase "payload arms still close the constructor set" $
      assertBool "no catch-all needed" . isRight $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Rect Real Real | Dot\n\
        \f :: Shape -> Real\n\
        \f | (Circle r) = r\n\
        \  | (Rect w h) = w\n\
        \  | Dot = 0.0\n"

  , testCase "an incomplete payload clause set is rejected" $
      assertBool "Dot unmatched" . isLeft $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Rect Real Real | Dot\n\
        \f :: Shape -> Real\n\
        \f | (Circle r) = r\n\
        \  | (Rect w h) = w\n"

  , testCase "a payload pattern of the wrong arity is rejected" $
      assertBool "Circle takes one field" . isLeft $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Dot\n\
        \f :: Shape -> Real\n\
        \f | (Circle r s) = r\n\
        \  | Dot = 0.0\n"

  , -- A constructor whose fields are matched rather than bound does not
    -- close that constructor: a Circle holding any other radius falls
    -- past it. Getting this wrong is silent, because 'assembleCascade'
    -- drops the final clause's test -- the Dot branch would simply
    -- swallow every non-degenerate Circle.
    testCase "a refutable payload field does not close its constructor" $
      assertBool "Circle 0.0 does not cover Circle" . isLeft $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Dot\n\
        \f :: Shape -> Str\n\
        \f | (Circle 0.0) = \"degenerate\"\n\
        \  | Dot = \"dot\"\n"

  , -- The mirror: repeating a constructor with DIFFERENT field patterns is
    -- the ordinary way to write a special case ahead of the general one,
    -- and must not be read as an unreachable duplicate.
    testCase "a special case may precede the general one for a constructor" $
      assertBool "Circle 0.0 then Circle r" . isRight $ parseMod
        "module main (f)\n\
        \data Shape = Circle Real | Dot\n\
        \f :: Shape -> Str\n\
        \f | (Circle 0.0) = \"degenerate\"\n\
        \  | (Circle r) = \"round\"\n\
        \  | Dot = \"dot\"\n"

  , testCase "recursive constructor patterns parse" $
      assertBool "Node l r" . isRight $ parseMod
        "module main (f)\n\
        \data Tree = Leaf | Node Tree Tree\n\
        \f :: Tree -> Tree\n\
        \f | (Node l r) = l\n\
        \  | Leaf = Leaf\n"
  ]
