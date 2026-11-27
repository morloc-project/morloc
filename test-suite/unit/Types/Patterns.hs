{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Patterns
  ( irrefutablePatternTests
  , tuplePatternLambdaTests
  , patternSelectorTests
  ) where

import Text.RawString.QQ

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Types.Common

-- ============================================================
-- Irrefutable-pattern binding syntax
-- ============================================================

irrefutablePatternTests :: TestTree
irrefutablePatternTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Irrefutable pattern bindings"
      [ -- === Tuple destructuring in let ===
        assertGeneralType
          "let with tuple pattern picks first component"
          "module main (x)\nx = let (a, b) = (1, True) in a"
          int

      , assertGeneralType
          "let with tuple pattern picks second component"
          "module main (x)\nx = let (a, b) = (1, True) in b"
          bool

      , assertGeneralType
          "let with wildcard in tuple ignores component"
          "module main (x)\nx = let (_, b) = (1, True) in b"
          bool

      , assertGeneralType
          "let with nested tuple pattern"
          "module main (x)\nx = let (a, (b, c)) = (1, (True, 3)) in b"
          bool

      -- === Tuple destructuring in lambda ===
      -- These use signatures because inferring through a lambda-projected
      -- irrefutable pattern still hits the existential-leak bug tracked as
      -- a separate TODO. The desugar produces the correct AST; the tests
      -- confirm end-to-end synth+check works when the outer type is fixed.

      , assertGeneralType
          "lambda with tuple pattern picks first"
          "module main (x)\nx :: Int\nx = (\\ (a, b) -> a) (7, True)"
          int

      , assertGeneralType
          "lambda with wildcard in tuple pattern"
          "module main (x)\nx :: Bool\nx = (\\ (_, b) -> b) (7, True)"
          bool

      -- === Tuple destructuring in function args ===

      , assertGeneralType
          "function-arg tuple pattern picks first"
          [r|
        fst2 :: (Int, Bool) -> Int
        fst2 (a, b) = a
        fst2 (42, True)
          |]
          int

      , assertGeneralType
          "function-arg tuple pattern picks second"
          [r|
        snd2 :: (Int, Bool) -> Bool
        snd2 (a, b) = b
        snd2 (42, True)
          |]
          bool

      , assertGeneralType
          "function-arg wildcard in tuple"
          [r|
        second :: (Int, Bool, Int) -> Bool
        second (_, b, _) = b
        second (1, True, 3)
          |]
          bool

      -- === Record destructuring (field-polymorphic on structural records) ===
      -- NOTE: `let {...}` alone is ambiguous with explicit let-blocks, so
      -- record patterns on a let LHS must be parenthesized.

      , assertGeneralType
          "let with record pattern extracts named field"
          "module main (x)\nx = let ({a=p, b=q}) = {a=1, b=True} in p"
          int

      , assertGeneralType
          "record pattern extracts second field"
          "module main (x)\nx = let ({a=p, b=q}) = {a=1, b=True} in q"
          bool

      , assertGeneralType
          "record pattern field order does not matter"
          "module main (x)\nx = let ({b=q, a=p}) = {a=1, b=True} in p"
          int

      -- === As-patterns ===

      , assertGeneralType
          "as-pattern binds label to whole receiver"
          [r|
        first p@(a, b) = p
        first (1, True)
          |]
          (tuple [int, bool])

      , assertGeneralType
          "as-pattern binds label and inner components together"
          [r|
        firstElem :: (Int, Bool) -> Int
        firstElem p@(a, b) = a
        firstElem (1, True)
          |]
          int

      -- === Do-block pattern binds ===

      , expectPass
          "do-block tuple bind unpacks and returns first component"
          [r|
        module main (x)
        effect IO
        f :: <IO> (Int, Bool)
        x = do
            (a, b) <- f
            a
          |]

      , expectPass
          "do-block wildcard bind runs effect and returns other"
          [r|
        module main (x)
        effect IO
        f :: <IO> (Int, Bool)
        x = do
            (_, b) <- f
            b
          |]

      -- === Nested / complex patterns ===

      , assertGeneralType
          "function-arg mixing tuple, record, wildcard"
          [r|
        record R = R {a :: Bool, b :: Real}
        pick :: (Int, R) -> Real
        pick (n, {a=_, b=q}) = q
        pick (1, {a=True, b=3.5})
          |]
          real

      , assertGeneralType
          "wildcard in let followed by identity use"
          "module main (x)\nx = let _ = 42 in True"
          bool

      -- === Error cases ===

      , expectError
          "duplicate binder in tuple pattern is rejected"
          [r|
        module main (x)
        x = let (a, a) = (1, 2) in a
          |]

      , expectError
          "duplicate binder across as-pattern is rejected"
          [r|
        module main (x)
        x = let a@(a, b) = (1, 2) in b
          |]
      ]

-- | When a function argument has a concrete type, each variable pulled
-- out by a destructuring pattern (a tuple pattern in a lambda parameter
-- or a let binding) must receive the corresponding component type --
-- even when that component is passed straight through to the result
-- without any other constraint. Axes covered: lambda-param vs let
-- destructuring; argument pinned by a binding vs an inline literal;
-- pass-through of the first / second / both / swapped / three-wide /
-- nested components; a component pinned by a same-application function
-- as a control; plain-variable lambdas and annotated/checked forms as
-- controls that must keep working; and negative cases where consuming a
-- destructured component at a contradictory type must be rejected.
tuplePatternLambdaTests :: TestTree
tuplePatternLambdaTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Destructuring-pattern argument-type propagation"
      [ -- Controls: plain-variable lambdas already flow the argument
        -- type in, including a pass-through into a constructed tuple.
        assertGeneralType
          "plain-var lambda, argument pinned by a binding"
          (tplProg "map (\\x -> dbl x) ys")
          (lst int)
      , assertGeneralType
          "plain-var lambda constructing a tuple (pass-through)"
          (tplProg "map (\\x -> (x, x)) ys")
          (lst (tuple [int, int]))
      , assertGeneralType
          "plain-var lambda, direct application"
          (tplProg "(\\x -> dbl x) n")
          int
      , -- Core: a tuple-pattern parameter must receive each component
        -- type from the argument. The pass-through first component is
        -- the one with no other constraint.
        assertGeneralType
          "tuple-pattern lambda, pass-through first component"
          (tplProg "map (\\(k, v) -> (k, dbl v)) xs")
          (lst (tuple [str, int]))
      , assertGeneralType
          "tuple-pattern lambda over an inline literal argument"
          (tplProg "map (\\(k, v) -> (k, dbl v)) [(\"a\", 1)]")
          (lst (tuple [str, int]))
      , assertGeneralType
          "tuple-pattern lambda, project the first component"
          (tplProg "map (\\(k, v) -> k) xs")
          (lst str)
      , assertGeneralType
          "tuple-pattern lambda, project the second component"
          (tplProg "map (\\(k, v) -> v) xs")
          (lst int)
      , assertGeneralType
          "tuple-pattern lambda, both components (identity)"
          (tplProg "map (\\(k, v) -> (k, v)) xs")
          (lst (tuple [str, int]))
      , assertGeneralType
          "tuple-pattern lambda, swapped components"
          (tplProg "map (\\(k, v) -> (v, k)) xs")
          (lst (tuple [int, str]))
      , assertGeneralType
          "tuple-pattern lambda, component pinned by a function (control)"
          (tplProg "map (\\(k, v) -> dbl v) xs")
          (lst int)
      , assertGeneralType
          "three-wide tuple-pattern lambda"
          (tplProg "map (\\(a, b, c) -> (a, c)) ts")
          (lst (tuple [str, bool]))
      , assertGeneralType
          "nested tuple-pattern lambda"
          (tplProg "map (\\((a, b), c) -> a) us")
          (lst str)
      , assertGeneralType
          "direct application of a tuple-pattern lambda"
          (tplProg "(\\(k, v) -> k) t")
          str
      , assertGeneralType
          "let-bound tuple destructuring inside a lambda"
          (tplProg "map (\\p -> let (k, v) = p in k) xs")
          (lst str)
      , -- Controls in checking mode: an annotated result and a named
        -- helper with an explicit signature must keep resolving.
        assertGeneralType
          "annotated result (checking mode control)"
          (tplProg "map (\\(k, v) -> (k, dbl v)) xs :: [(Str, Int)]")
          (lst (tuple [str, int]))
      , assertGeneralType
          "named helper with an explicit signature (control)"
          (tplProg "map proj xs")
          (lst str)
      , -- Negative: a destructured component consumed at a type that
        -- contradicts the argument must be rejected. A component whose
        -- type is dropped would silently accept these.
        exprTestBad
          "destructured first component used at a wrong type is rejected"
          (tplProg "map (\\(k, v) -> dbl k) xs")
      , exprTestBad
          "wrong-typed destructured component under direct application is rejected"
          (tplProg "(\\(k, v) -> dbl k) t")
      , exprTestBad
          "destructured second component used at a wrong type is rejected"
          (tplProg "map (\\(k, v) -> dbl v) sxs")
      , -- A destructuring pattern pins its parameter to a tuple shape, so
        -- applying such a lambda to a non-tuple must be rejected up front,
        -- not deferred to a codegen crash on the projection.
        exprTestBad
          "tuple-pattern lambda mapped over non-tuples is rejected"
          (tplProg "map (\\(k, v) -> k) ys")
      , exprTestBad
          "tuple-pattern lambda applied to a scalar is rejected"
          (tplProg "(\\(k, v) -> k) n")
      , -- A field setter reached through a lambda parameter must enforce the
        -- field's type just as it does on a concrete receiver: setting the
        -- Str field .0 to an Int literal must fail.
        exprTestBad
          "wrong-typed field setter through a lambda parameter is rejected"
          (tplProg "map (\\p -> (.0 = 7) p) xs")
      , exprTestBad
          "wrong-typed field setter under direct application is rejected"
          (tplProg "(\\p -> (.0 = 7) p) t")
      ]

-- | Selector shapes a pattern accessor accepts and rejects, checked at
-- the frontend because none of them reach codegen. A group entry is
-- either a record key or a tuple index and the Selector carries one
-- kind per level, so a group naming both has no representation: it
-- must be reported against the source, not raised as an internal
-- error. A setter's value is checked against the field it is written
-- at, so a value that only inhabits the field's type after a widening
-- coercion still has to be accepted.
patternSelectorTests :: TestTree
patternSelectorTests =
  localOption (mkTimeout 2000000) $ -- 2s
    testGroup
      "pattern selector shapes"
      [ expectError
          "group mixing a record key and a tuple index is rejected"
          [r|
        module main (foo)
        record R = R { a :: Int, b :: Str }
        foo :: R -> (Int, Int)
        foo r = .(.a, .0) r
          |]
      , expectError
          "setter group mixing a record key and a tuple index is rejected"
          [r|
        module main (foo)
        record R = R { a :: Int, b :: Str }
        foo :: R -> R
        foo r = .(.a = 1, .0 = 2) r
          |]
      , expectError
          "chain mixing a record key and a tuple index on a tuple is rejected"
          [r|
        module main (foo)
        foo :: (Int, Int) -> Int
        foo t = .(.name, .0) t
          |]
      , expectPass
          "setter writes a bare value into an optional field"
          [r|
        module main (foo)
        record R = R { a :: ?Str, b :: Int }
        base :: R
        base = { a = "one", b = 1 }
        foo :: R
        foo = .(.a = "new") base
          |]
      , expectPass
          "setter writes a bare value into an optional field of a literal"
          [r|
        module main (foo)
        record R = R { a :: ?Str, b :: Int }
        foo :: R
        foo = .(.a = "new") { a = Null, b = 1 }
          |]
      , expectPass
          "setter writes Null into an optional field"
          [r|
        module main (foo)
        record R = R { a :: ?Str, b :: Int }
        base :: R
        base = { a = "one", b = 1 }
        foo :: R
        foo = .(.a = Null) base
          |]
      , expectError
          "setter on a field that the receiver does not have is rejected"
          [r|
        module main (foo)
        record R = R { a :: Int, b :: Str }
        base :: R
        base = { a = 1, b = "x" }
        foo :: R
        foo = .(.c = 1) base
          |]
      , -- An IFile is a handle onto bytes already written, and the
        -- runtime walker that serves a pattern on one only reads. A
        -- getter on an IFile receiver is therefore fine and a setter
        -- is not; the pair is here so a change that made the setter
        -- compile could not pass unnoticed.
        expectPass
          "getter on an IFile receiver is accepted"
          [r|
        module main (foo)
        record R = R { a :: Int, b :: Str }
        foo :: IFile R -> Int
        foo f = .a f
          |]
      , expectError
          "setter on an IFile receiver is rejected"
          [r|
        module main (foo)
        record R = R { a :: Int, b :: Str }
        foo :: IFile R -> R
        foo f = .(.a = 1) f
          |]
      ]
