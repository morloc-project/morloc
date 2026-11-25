{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Effects
  ( effectSubtypeTests
  , effectSynthesisTests
  , effectErrorTests
  , evalSugarTests
  , effectEscapabilityTests
  , effectPartialApplicationTests
  , polymorphicEffectRowTests
  , effectCoverageMessageTests
  , streamIntrinsicTests
  , suspensionLawTests
  ) where

import Morloc.Frontend.Namespace
import Text.RawString.QQ

import qualified Data.Set as Set
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Types.Common

-- | Effect subtyping covers four cases the spec lays out
-- (EFF-8 and EFF-10):
--
--   1. Widening accepted: fewer effects can be used where more are
--      expected.  This is the one direction the rule permits.
--   2. Narrowing rejected: a value with more effects cannot satisfy a
--      slot with fewer.  Previously this was silently accepted; the
--      stricter rule turns it into a hard subtype error.
--   3. Effectful-to-pure rejected: an effectful type cannot satisfy a
--      pure slot.  Effects do not silently drop.
--   4. EffectVar deferral: when either side mentions an unsolved
--      EffectVar, judgement is deferred (the variable may later be
--      solved to anything).  This is a known TODO; the deferral is
--      tested here so the loophole is visible.
effectSubtypeTests :: TestTree
effectSubtypeTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect subtype tests"
      [ -- === Reflexivity ===
        -- A type is always a subtype of itself, regardless of its effect set.
        assertSubtypeGamma "reflex: <IO> A <: <IO> A"
          [] (ioEff a) (ioEff a) []
      , assertSubtypeGamma "reflex: <> A <: <> A"
          [] (emptyEff a) (emptyEff a) []
      , assertSubtypeGamma "reflex: <IO,Error> A <: <IO,Error> A"
          [] (ioErrEff a) (ioErrEff a) []

        -- === Widening accepted (covariant subset) ===
        -- A value producing fewer effects may be used where more are
        -- expected.  This is the only allowed direction.
      , assertSubtypeGamma "widening: <IO> A <: <IO,Error> A"
          [] (ioEff a) (ioErrEff a) []
      , assertSubtypeGamma "widening: <> A <: <IO> A"
          [] (emptyEff a) (ioEff a) []
      , assertSubtypeGamma "widening: <> A <: <IO,Error> A"
          [] (emptyEff a) (ioErrEff a) []
      , assertSubtypeGamma "widening: <Error> A <: <IO,Error> A"
          [] (errEff a) (ioErrEff a) []

        -- === Narrowing rejected ===
        -- The signature `a :: <IO> Int = (rint :: <IO,Error> Int)`
        -- previously compiled, silently losing the Error tag.  The new
        -- rule rejects it as a subtype error.
      , assertSubtypeBad "narrowing: <IO,Error> A </: <IO> A"
          [] (ioErrEff a) (ioEff a)
      , assertSubtypeBad "narrowing: <IO> A </: <Error> A"
          [] (ioEff a) (errEff a)
      , assertSubtypeBad "narrowing: <IO> A </: <> A"
          [] (ioEff a) (emptyEff a)
      , assertSubtypeBad "narrowing: <IO,Error> A </: <Rand> A (disjoint)"
          [] (ioErrEff a) (randEff a)

        -- === Effectful-to-pure rejected ===
        -- Effects do not silently drop.  An effectful value cannot
        -- inhabit a plain type slot; the caller must declare the
        -- effect or force it in a do-block.
      , assertSubtypeBad "drop effect: <IO> A </: A"
          [] (ioEff a) a
      , assertSubtypeBad "drop effect: <IO,Error> A </: A"
          [] (ioErrEff a) a

        -- === The empty row is a row: <> A is a suspension, not an A ===
        -- A suspension and its result are different types with no
        -- coercion either way; the only way from <> A to A is running it.
      , assertSubtypeBad "empty row is a suspension: <> A </: A"
          [] (emptyEff a) a
      , assertSubtypeBad "a value is not a suspension: A </: <> A"
          [] a (emptyEff a)
      , assertSubtypeGamma "empty row is included in every row: <> A <: <IO> A"
          [] (emptyEff a) (ioEff a) []

        -- === No lift: a value never inhabits a suspension slot ===
        -- `do v` is the only way to build a suspension from a value.
      , assertSubtypeBad "pure </: <IO>: no implicit lift"
          [] a (ioEff a)

        -- === Recursion through inner type ===
        -- The subtype rule recurses on the inner types after the
        -- effect-subset check passes, so structural mismatches inside
        -- are caught normally.
      , assertSubtypeGamma "inner fun: <IO> (A -> B) <: <IO> (A -> B)"
          [] (ioEff (fun [a, b])) (ioEff (fun [a, b])) []
      , assertSubtypeGamma "inner optional: <IO> ?A <: <IO> ?A"
          [] (ioEff (OptionalU a)) (ioEff (OptionalU a)) []
      , assertSubtypeGamma "inner list: <IO> [A] <: <IO> [A]"
          [] (ioEff (lst a)) (ioEff (lst a)) []
      , assertSubtypeBad "inner type mismatch: <IO> A </: <IO> B"
          [] (ioEff a) (ioEff b)

        -- === Existential solving inside effects ===
        -- Existentials on either side of the effect wrapper are solved
        -- by recursing into the inner-type subtype check.  This
        -- behaviour is unchanged by the strictness fix.
      , assertSubtypeGamma "existential inside: <a> -| <IO> <a> <: <IO> A |- <a>:A"
          [eag] (ioEff ea) (ioEff a) [solvedA a]
      , assertSubtypeGamma "existential inside reverse: <a> -| <IO> A <: <IO> <a> |- <a>:A"
          [eag] (ioEff a) (ioEff ea) [solvedA a]

        -- === EffectVar deferral (known loophole) ===
        -- EffectVars are introduced internally when a do-block forces
        -- a non-final statement.  They are not yet solved, so subtype
        -- relaxes when either side mentions one rather than rejecting.
        -- Locking this in keeps the loophole visible; tightening it
        -- requires implementing effect-variable solving (TODO).
      , assertSubtypeGamma "deferral: <e1> A <: <IO> A passes (var on left)"
          [] (effVar "e1" a) (ioEff a) []
      , assertSubtypeGamma "deferral: <IO> A <: <e1> A passes (var on right)"
          [] (ioEff a) (effVar "e1" a) []
      , assertSubtypeGamma "deferral: <IO,Error> A <: <e1> A passes (var on right)"
          [] (ioErrEff a) (effVar "e1" a) []

        -- ...but a genuinely effectful value still cannot satisfy a
        -- concrete non-effect type (that rejection is unchanged).
      , assertSubtypeBad "effect into concrete still rejected: <Error> A </: A"
          [] (errEff a) a
      ]
  where
    a = var "A"
    b = var "B"
    ea = exist "x1"
    eag = ExistG (TV "x1") ([], Open) ([], Open)
    solvedA t = SolvedG (TV "x1") t

-- | Effect synthesis tests verify that the inferred effect set on a
-- top-level export matches the spec's structural propagation rule
-- (EFF-6):
effectSynthesisTests :: TestTree
effectSynthesisTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect synthesis tests"
      [ -- a do-block that forces nothing is still a suspension: <> Int
        assertGeneralType
          "pure do-block"
          [r|
        module main (x)
        x = do 42
          |]
          (emptyEff int)
      , assertGeneralType
          "do-block with with one function call"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do { f 1 }
          |]
          (ioEff int)
      , assertGeneralType
          "do-block with tuple of effectful elements"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do
          x <- f 1
          y <- f 2
          (x, y)
          |]
          (ioEff (tuple [int, int]))
      , -- let with pure RHS in do-block infers empty effect
        assertGeneralType
          "do-block with pure let binding"
          [r|
        module main (x)
        x = do
            let y = 1
            y
          |]
          (emptyEff int)
      , assertGeneralType
          "do-block with bind and let"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        add :: Int -> Int -> Int
        x = do
            y <- f 1
            let z = add y 1
            z
          |]
          (ioEff int)
      , -- a pure tail is returned; the block is a suspension with the empty row
        assertGeneralType
          "pure expression in do-block"
          [r|
        module main (x)
        add :: Int -> Int -> Int
        x = do add 1 2
          |]
          (emptyEff int)
      , assertGeneralType
          "do-block with multiple effect labels"
          [r|
        module main (x)
        effect IO
        effect Error
        f :: Int -> <IO> Int
        g :: Int -> <Error> Int
        add :: Int -> Int -> Int
        x = do
          x <- f 1
          y <- g 2
          add x y
          |]
          (ioErrEff int)
      , -- chained binds feeding results forward
        assertGeneralType
          "do-block with chained dependent binds"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        add :: Int -> Int -> Int
        x = do
            a <- f 1
            b <- f a
            add a b
          |]
          (ioEff int)
      , -- do-block with effect annotation matching inferred effects
        assertGeneralType
          "annotated do-block matches inferred effects"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x :: <IO> Int
        x = do
            y <- f 1
            y
          |]
          (ioEff int)
      , -- polymorphic function applied inside do-block
        assertGeneralType
          "polymorphic function in do-block"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        id :: a -> a
        x = do
          f (id 42)
          |]
          (ioEff int)
      , -- do-block returning a list with forces
        assertGeneralType
          "do-block returning list"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do
          x <- f 1
          y <- f 2
          [x, y]
          |]
          (ioEff (lst int))

        -- Polymorphic effectful typeclass method used TWICE inside a
        -- nested do-block that is bound with '<-'. The inner do-block's
        -- final synths to a concrete type (Int, driven by 'add'), so the
        -- inner DoBlockS returns 'EffectU collected Int'. Without the
        -- ForallU peel in 'effectOfAnno', 'collected' comes back empty
        -- because 'random :: forall a. <IO> a' is not a bare EffectU;
        -- then 'mkEffectU' collapses '<> Int' to 'Int', and the outer
        -- '<-' EvalS throws "Cannot force a non-effectful value (got
        -- type Int)."
      , assertGeneralType
          "polymorphic method twice inside nested '<-' do-block"
          [r|
        module main (foo)
        effect IO
        class C a where
          random :: <IO> a
        instance C Int where
          source Py from "helpers.py" ("r" as random)
        add :: Int -> Int -> Int
        foo :: <IO> Int
        foo = do
          a <- (do
            v1 <- random
            v2 <- random
            add v1 v2)
          a
          |]
          (ioEff int)

        -- The same shape via '!' sugar. Under Option A, 'a <- add !v1 !v2'
        -- desugars to 'a <- (do; x <- v1; y <- v2; add x y)', so this
        -- exercises the identical typechecker path with a different
        -- surface syntax.
      , assertGeneralType
          "polymorphic method twice through '!' in a '<-' bind RHS"
          [r|
        module main (foo)
        effect IO
        class C a where
          random :: <IO> a
        instance C Int where
          source Py from "helpers.py" ("r" as random)
        add :: Int -> Int -> Int
        foo :: <IO> Int
        foo = do
          a <- add !random !random
          a
          |]
          (ioEff int)
      ]

-- | Program-level effect rejection tests.  Each test is a complete
-- morloc module that exercises a specific spec rule and asserts the
-- compiler refuses it.  The structure mirrors the spec subsections:
--
--   * Force misuse: '!' applied to a non-suspended value.
--   * Type mismatch inside a forced expression.
--   * Effectful value flowing into a pure parameter slot.
--   * Effect narrowing at a binding boundary (assigning a more-effectful
--     value to a less-effectful annotation).
--   * Effect narrowing at an application boundary (passing a more-
--     effectful argument to a less-effectful parameter).
--   * Effectful value flowing out of a function with a pure return type.
--   * Disjoint-effect mismatch (Rand passed where IO expected).
--
-- When the inference pass for declared-vs-inferred mismatches lands
-- (a separate item in the effects plan), additional widening-in-body
-- tests belong here.
effectErrorTests :: TestTree
effectErrorTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect error tests"
      [ -- A function `g :: Int -> Int` requires a pure Int.  Passing
        -- the effectful application `f 1 :: <IO> Int` directly (no
        -- force) must be rejected: effects do not silently drop at
        -- the argument boundary.
        exprTestBad
          "effectful arg passed to pure parameter"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        g :: Int -> Int
        x = g (f 1)
          |]

        -- Narrowing at a binding: rint produces <IO,Error> Int, but
        -- the annotation requests <IO> Int.  The Error tag would be
        -- silently dropped under the old permissive rule.  The strict
        -- rule rejects.
      , exprTestBad
          "narrowing at binding: <IO,Error> bound to <IO>"
          [r|
        module main (x)
        effect IO
        effect Error
        rint :: <IO, Error> Int
        x :: <IO> Int
        x = rint
          |]

        -- Narrowing at a binding to a pure type: dropping all effects
        -- in one step is rejected for the same reason.
      , exprTestBad
          "narrowing at binding: <IO> bound to pure"
          [r|
        module main (x)
        effect IO
        rint :: <IO> Int
        x :: Int
        x = rint
          |]

        -- Narrowing at an application boundary: `consume` accepts an
        -- <IO> Int parameter; passing an <IO,Error> Int would discard
        -- the Error effect at the call site.  Rejected.
      , exprTestBad
          "narrowing at application: <IO,Error> arg to <IO> param"
          [r|
        module main (x)
        effect IO
        effect Error
        rint :: <IO, Error> Int
        consume :: <IO> Int -> Int
        x = consume rint
          |]

        -- Disjoint effects between argument and parameter.  Rand is
        -- not a subset of IO; the call cannot be accepted under any
        -- interpretation of the rule.
      , exprTestBad
          "disjoint effects: <Rand> arg to <IO> param"
          [r|
        module main (x)
        effect Rand
        effect IO
        rrand :: <Rand> Int
        consume :: <IO> Int -> Int
        x = consume rrand
          |]
      ]

-- | Eval-sugar ('!' prefix) desugars to do-block binds pre-typecheck.
-- The typechecker never sees a '!'. These tests confirm:
--   * Types match the equivalent explicit do-block form.
--   * '!' at various expression positions (tuple, application, lambda,
--     let, guard branches) sequences into the enclosing scope, with
--     effect propagating outward (nothing is discharged).
--   * '!' at redundant positions ('<-' RHS, bare do stmt) is rejected
--     during desugaring.
--   * '!' on a pure value is rejected during typechecking as it would
--     be for an explicit do-bind.
evalSugarTests :: TestTree
evalSugarTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Eval-sugar '!' desugar tests"
      [ -- '!' inside a tuple: hoist per-element to a wrapping do-block.
        -- Type must match the explicit do-block form.
        assertGeneralType
          "'!' in tuple: (!x, !y)"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo = (!x, !y)
          |]
          (ioEff (tuple [int, int]))

        -- Reference: the equivalent explicit do-block has the same type.
      , assertGeneralType
          "do-block form of (!x, !y)"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo = do
          a <- x
          b <- y
          (a, b)
          |]
          (ioEff (tuple [int, int]))

        -- '!' as function arguments: bar (!x) (!y).
      , assertGeneralType
          "'!' as function arguments"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo = add !x !y
          |]
          (ioEff int)

        -- '!' inside a lambda body: hoists INSIDE the body (per-call).
      , assertGeneralType
          "'!' in lambda body: \\x -> !(f x)"
          [r|
        module main (foo)
        effect IO
        f :: Int -> <IO> Int
        foo = \x -> !(f x)
          |]
          (fun [int, ioEff int])


        -- '!' in guard branches (branch-local): each branch is a boundary.
      , assertGeneralType
          "'!' in guard branches"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo b
          ? b = !x
          : !y
          |]
          (fun [bool, ioEff int])

        -- '!' inside an existing do-block: hoist per-statement to a fresh
        -- bind inserted BEFORE the containing statement, preserving order.
      , assertGeneralType
          "'!' inside a do-block bind RHS"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo = do
          a <- add !x !y
          a
          |]
          (ioEff int)

        -- '!' inside a nested do-block: the inner do is a boundary; its
        -- effect does not leak out (the outer only sees the inner block's
        -- return type wrapped in its own inferred effect).
      , assertGeneralType
          "'!' in a nested do-block"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo = do
          a <- do
            x
            x
          c <- do
            y
            y
          (a, c)
          |]
          (ioEff (tuple [int, int]))

        -- '!' with an AnnE: the ascription applies to the do-block wrapper.
      , assertGeneralType
          "'!' with type annotation"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo = (!x) :: <IO> Int
          |]
          (ioEff int)

        -- '!' on a pure value is rejected: EvalS on a non-effectful type.
      , exprTestBad
          "'!' on a pure value"
          [r|
        module main (foo)
        x :: Int
        foo :: Int
        foo = !x
          |]

        -- Redundant '!' on the RHS of '<-' is a desugar error.
      , exprTestBad
          "redundant '!' on RHS of '<-'"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          y <- !x
          y
          |]

        -- Redundant '!' as a bare do statement is a desugar error.
      , exprTestBad
          "redundant '!' as bare do statement"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          !x
          x
          |]

        -- '!' at the RHS of a let in a do-block is rejected. 'let'
        -- should bind a pure value; a bang at a hoistable position
        -- would silently rewrite the let into an effectful bind chain
        -- followed by a pure let, making the surface line read as pure
        -- while the effect fires above it. Bare form.
      , exprTestBad
          "bare '!' as RHS of let in do-block"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          let y = !x
          y
          |]

        -- Same rejection for a composite RHS: any '!' at a hoistable
        -- position inside the let RHS is rejected, not just a bare
        -- '!expr'.
      , exprTestBad
          "composite let RHS with '!' at hoistable position"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = do
          let s = add !x !y
          s
          |]

        -- '!' inside a lambda body under a let is fine: the lambda body
        -- is a boundary, so the bang stays local (the effect fires when
        -- the function is applied, not at the let itself). The let
        -- correctly binds a function value.
      , assertGeneralType
          "'!' inside a lambda body under a let stays legal"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          let f = \z -> !x
          y <- f 42
          y
          |]
          (ioEff int)

        -- '!' inside the RHS of a '<-' bind is legal: the RHS is a
        -- boundary, so the bangs seal into a nested do-block wrapping
        -- the residual expression. This keeps the effect firing at the
        -- '<-' line rather than silently hoisting above it.
      , assertGeneralType
          "'!' inside a '<-' bind RHS with pure residual"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = do
          a <- add !x !y
          a
          |]
          (ioEff int)

        -- Same for a bare non-final do statement: the RHS is a boundary.
      , assertGeneralType
          "'!' inside a bare non-final do statement"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = do
          add !x !y
          x
          |]
          (ioEff int)

        -- The same rejection applies OUTSIDE a do-block: 'let x = !expr
        -- in body' is rejected. Keeps the mental model uniform (let
        -- always binds a pure value; effect firing goes through '<-')
        -- and avoids the compiler silently synthesizing a LetE with an
        -- effect-firing binding from a user 'let'.
      , exprTestBad
          "'!' at RHS of an expression-level 'let ... in ...'"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        foo :: <IO> Int
        foo = let z = !x in add z 1
          |]

        -- Same in composite form.
      , exprTestBad
          "composite '!' in expression-level let RHS"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = let s = add !x !y in s
          |]

        -- '!' inside a lambda body under an expression-level let is
        -- still legal (bang sealed by lambda boundary).
      , assertGeneralType
          "'!' inside a lambda body under expression-level let"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: Int -> <IO> Int
        foo = let f = \z -> !x in f
          |]
          (fun [int, ioEff int])

        -- '!' inside the BODY of an expression-level let is legal:
        -- the bang hoists into the LetE's continuation (bindings still
        -- in scope), and the whole let becomes an effectful expression.
      , assertGeneralType
          "'!' inside the body of an expression-level let"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        foo :: <IO> Int
        foo = let n = 5 in add n !x
          |]
          (ioEff int)
      ]

-- | Escapability tests.  An effect is inescapable by default
-- (`effect E`); `escapable effect E` opts it into being
-- dischargeable.  Two rules interact:
--   * inescapable-propagation (signature-level): every inescapable
--     concrete label in a parameter's effect row must also appear in
--     the result row, so an inescapable effect is never silently
--     consumed -- not even by a handler-shaped signature.
--   * sourced-discharge (value-level): an escapable effect is exempt
--     from propagation, but only a SOURCED handler may actually drop
--     it; a defined function that carries the effect cannot.
-- The handlers are APPLIED and the positive cases assert the
-- discharged/propagated result type, so the assertion itself proves
-- the rule.  The escapable-keyword contrast is two otherwise-identical
-- applied programs differing only by the `escapable` keyword.
effectEscapabilityTests :: TestTree
effectEscapabilityTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect escapability tests"
      [ -- === Escapable effect discharged by a SOURCED handler, applied ===
        -- foo performs the escapable Error; the sourced handler recover
        -- consumes <Error> Int and returns a pure Int.  Applying it
        -- must yield Int -- the assertion itself proves the Error was
        -- discharged at the call site, not merely that some unrelated
        -- binding is well typed.
        assertGeneralType
          "escapable Error discharged by sourced handler (applied)"
          [r|
        module main (x)
        escapable effect Error
        source Py ("foo")
        source Py ("recover")
        foo :: Int -> <Error> Int
        recover :: <Error> Int -> Int
        x :: Int
        x = recover (foo 1)
          |]
          int
      , -- Tail-variable handler <Error, e> a -> <e> a applied: Error is
        -- discharged and the row variable solves to the empty row. The
        -- result is still a suspension, with the empty row.
        assertGeneralType
          "escapable Error discharged, row variable solved (applied)"
          [r|
        module main (x)
        escapable effect Error
        source Py ("foo")
        source Py ("handle")
        foo :: Int -> <Error> Int
        handle :: <Error, e> a -> <e> a
        x :: <> Int
        x = handle (foo 1)
          |]
          (emptyEff int)
      , -- An inescapable effect propagates THROUGH application: passt
        -- keeps Cap in its result, so applying it to an effectful
        -- argument yields <Cap> Int (the effect is not dropped).
        assertGeneralType
          "inescapable Cap propagates through application"
          [r|
        module main (x)
        effect Cap
        source Py ("foo")
        source Py ("passt")
        foo :: Int -> <Cap> Int
        passt :: <Cap> Int -> <Cap> Int
        x :: <Cap> Int
        x = passt (foo 1)
          |]
          (EffectU (EffectSet (Set.singleton "Cap")) int)

        -- === Escapable-keyword contrast (same applied program) ===
        -- Identical to the first test EXCEPT `escapable` is dropped, so
        -- Error is inescapable.  The handler is now rejected by the
        -- inescapable-propagation rule -- isolating exactly what the
        -- `escapable` keyword controls, with the handler applied.
      , exprTestBad
          "inescapable Error: the same applied program is rejected"
          [r|
        module main (x)
        effect Error
        source Py ("foo")
        source Py ("recover")
        foo :: Int -> <Error> Int
        recover :: <Error> Int -> Int
        x :: Int
        x = recover (foo 1)
          |]

        -- === Only a SOURCED handler may discharge an escapable effect ===
        -- recover is now DEFINED (recover t = t), not sourced.  Even
        -- though Error is escapable, a defined function that carries the
        -- effect cannot claim a pure result -- the discharge privilege
        -- belongs to sourced handlers alone.
      , exprTestBad
          "escapable Error: a defined (non-sourced) handler cannot discharge it"
          [r|
        module main (x)
        escapable effect Error
        source Py ("foo")
        foo :: Int -> <Error> Int
        recover :: <Error> Int -> Int
        recover t = t
        x :: Int
        x = recover (foo 1)
          |]

        -- === Inescapable effect cannot escape through application ===
        -- consume takes a pure Int; passing the inescapable effectful
        -- `foo 1 :: <Cap> Int` would silently drop Cap at the argument
        -- boundary.  Rejected.
      , exprTestBad
          "inescapable Cap cannot escape into a pure parameter (applied)"
          [r|
        module main (x)
        effect Cap
        source Py ("foo")
        source Py ("consume")
        foo :: Int -> <Cap> Int
        consume :: Int -> Int
        x :: Int
        x = consume (foo 1)
          |]

        -- === Signature-level rejections (illegal before any use) ===
        -- These declarations are rejected at the point of declaration
        -- by the inescapable-propagation rule, so no application is
        -- even reachable.
      , exprTestBad
          "signature: inescapable Cap in parameter absent from result"
          [r|
        module main (ok)
        effect Cap
        consume :: <Cap> Int -> Int
        ok :: Int
        ok = 42
          |]
      , exprTestBad
          "signature: handler-shaped <Cap,e> a -> <e> a rejected (Cap inescapable)"
          [r|
        module main (ok)
        effect Cap
        bad :: <Cap, e> a -> <e> a
        ok :: Int
        ok = 42
          |]
      , exprTestBad
          "signature: escapable Error droppable but inescapable Cap in same row leaks"
          [r|
        module main (ok)
        escapable effect Error
        effect Cap
        mixed :: <Error, Cap> Int -> <Error> Int
        ok :: Int
        ok = 42
          |]
      , exprTestBad
          "signature: inescapable Cap spelled through an alias still propagates"
          [r|
        module main (ok)
        effect Cap
        type Capped a = <Cap> a
        consume :: Capped Int -> Int
        ok :: Int
        ok = 42
          |]
      ]

-- | Effect propagation under partial application.
--
-- The hard invariant: any effect at the top level of an applied
-- argument's type must reach the terminal output of the enclosing
-- application. The bare-existential subtyping rule (instantiate
-- ExistU<->EffectU) solves a bare ?b to <IO> ?b' and stores the effect
-- inside the gamma binding for the consumed slot. Without the
-- application-site lift, the effect drops out of the user-visible spine
-- whenever it lands in a slot whose type variable does not appear in
-- the result. These tests pin down the post-fix behavior in five
-- positive shapes plus a negative case that locks in non-interference
-- with row-polymorphic handler-style signatures.
effectPartialApplicationTests :: TestTree
effectPartialApplicationTests =
  localOption (mkTimeout 200000) $ -- 0.2 second timeout
    testGroup
      "Effect propagation under partial application"
      [ -- Control: the consumed slot IS the result slot.  The instantiate
        -- path stores <IO> in ?a's binding, and ?a is the terminal, so
        -- the effect already reaches the output without the lift.
        assertGeneralType
          "f1 control: foo (bar str) -- consumed slot equals result slot"
          [r|
        module main (f1)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f1 y z = do
          x <- bar "hi"
          foo x y z
          |]
          (fun [exist "a", exist "b", ioEff str])

        -- Partial application: effectful arg goes into ?b (NOT the
        -- result slot).  Pre-fix the <IO> tag silently dropped because
        -- ?b is consumed.  Post-fix the lift places <IO> on the
        -- terminal Int.
      , assertGeneralType
          "f2 partial app: foo (int) (bar str) -- effect into non-result slot"
          [r|
        module main (f2)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f2 y = do
          x <- bar "hi"
          foo 42 x y
          |]
          (fun [exist "a", ioEff int])

        -- Full application: every slot consumed.  Pre-fix the function
        -- returns plain Int and the <IO> tag vanishes entirely.
        -- Post-fix the terminal is wrapped as <IO> Int.
      , assertGeneralType
          "f3 full app: foo (int) (bar str) (bool) -- effect must reach pure-Int result"
          [r|
        module main (f3)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f3 = do
          x <- bar "hi"
          foo 42 x True
          |]
          (ioEff int)

        -- Two effectful args, each in a bare-existential slot.  Both
        -- top-level effect sets must union onto the terminal output.
        -- Here only IO is involved, but two independent absorbing slots
        -- exercise the foldr-union path in liftAbsorbedEffects.
      , assertGeneralType
          "f4 two effectful args -- both contribute to terminal"
          [r|
        module main (f4)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        qux :: Str -> <IO> Str
        f4 z = do
          x <- bar "hi"
          y <- qux "by"
          foo x y z
          |]
          (fun [exist "a", ioEff str])

        -- Different effects from different bare-existential slots must
        -- UNION into the terminal output, not be lost or pick one side.
        -- Exercises the foldr unionEffectSet path with non-equal sets.
      , assertGeneralType
          "f6 multi-effect union: <IO> arg and <Error> arg combine on terminal"
          [r|
        module main (f6)
        effect IO
        effect Error
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        qux :: Str -> <Error> Str
        f6 = do
          x <- bar "hi"
          y <- qux "by"
          foo 1 x y 
          |]
          (ioErrEff int)

        -- Let-bound effectful value flowing into a bare slot.  The
        -- let-RHS still synthesizes to <IO> Str and is checked against
        -- the bare existential through the same instantiate path, so
        -- the lift must fire identically.
      , assertGeneralType
          "f5 let-bound effectful value into non-result slot"
          [r|
        module main (f5)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f5 = do
          x <- bar "hi"
          foo 42 x True
          |]
          (ioEff int)

        -- Row-polymorphic handler signature: the Error effect is
        -- escapable and discharged by the handler; the unannotated result
        -- is a suspension with an open row (the empty row at ground), and
        -- no <IO> or <Error> is manufactured.
      , expectPass
          "handleEsc: row-poly handler discharges escapable Error"
          [r|
        module main (h)
        escapable effect Error
        source Py ("foo")
        source Py ("handle")
        foo :: Int -> <Error> Int
        handle :: <Error, e> a -> <e> a
        h = handle (foo 1)
          |]
      ]

-- | Pure values filling `<e> T` slots must recurse on the inner type
-- only. Solving the RHS row (via subtypeEffRows with a synthetic empty
-- LHS) would over-constrain any open row-variable to empty; adding an
-- existential-LHS guard would spuriously trip the occurs check when
-- the RHS's inner referenced the same existential.
polymorphicEffectRowTests :: TestTree
polymorphicEffectRowTests =
  localOption (mkTimeout 200000) $
    testGroup
      "Polymorphic effect row / pure-into-effect subtype"
      [ -- A helper that @catches on a polymorphic fallible must
        -- typecheck (guards against InstantiateL firing to solve
        -- `b := <e> b` and tripping the occurs check).
        expectPass
          "1. match on a polymorphic-row Try with a bare alternative"
          [r|
        module main (withTry)
        data Try e a = Err e | Ok a
        withTry :: (a -> <e> (Try Str b)) -> a -> b -> <e> b
        withTry f x fb = do
          r <- f x
          match r | (Ok v) = v | (Err _) = fb
          |]

        -- A constant suspension `do 42 :: <> Int` in a `<e> Int` slot;
        -- the result <> Int satisfies the export's <IO> Int by row
        -- inclusion.
      , expectPass
          "2. constant suspension in <e> Int slot"
          [r|
        module main (pureRoot)
        effect IO
        passThrough :: <e> Int -> <e> Int
        passThrough x = x
        pureRoot :: <IO> Int
        pureRoot = passThrough (do 42)
          |]

        -- A value never inhabits a suspension slot; `do 42` does.
      , expectError
          "3a. bare Int in <IO, Err> Int slot is rejected"
          [r|
        module main (x)
        effect IO
        escapable effect Err
        x :: <IO, Err> Int
        x = 42
          |]
      , expectPass
          "3b. do 42 in <IO, Err> Int slot"
          [r|
        module main (x)
        effect IO
        escapable effect Err
        x :: <IO, Err> Int
        x = do 42
          |]

        -- === Nested polymorphic @catch ===
        -- Outer @catch's fallback is itself an @catch, both polymorphic
        -- in e. Each layer independently exercises the pure-into-
        -- existential-EffectU rule when its bare fallback is checked.
      , expectPass
          "4. nested match, both layers polymorphic in e"
          [r|
        module main (withTwoTries)
        data Try e a = Err e | Ok a
        withTwoTries :: (a -> <e> (Try Str b)) -> (a -> <e> (Try Str b)) -> a -> b -> <e> b
        withTwoTries f g x fb = do
          r1 <- f x
          r2 <- g x
          match r1
            | (Ok v) = v
            | (Err _) = match r2 | (Ok w) = w | (Err _) = fb
          |]

        -- Two constant suspensions: the row variable is the empty row,
        -- which the export's <IO> includes.
      , expectPass
          "5. row-var survives two constant suspensions"
          [r|
        module main (run)
        effect IO
        pick :: <e> Int -> <e> Int -> <e> Int
        pick x _ = x
        run :: <IO> Int
        run = pick (do 42) (do 99)
          |]

        -- === Pure-then-effectful ordering ===
        -- First arg pure, second arg concretely effectful. The row
        -- var must be solvable to <IO> by the second arg despite the
        -- first arg not constraining it.
      , expectPass
          "6. row-var solved by effectful second arg after a constant suspension"
          [r|
        module main (run)
        effect IO
        source Py ("saveInt")
        saveInt :: Int -> <IO> Int
        seq2 :: <e> Int -> <e> Int -> <e> Int
        seq2 _ y = y
        run :: <IO> Int
        run = seq2 (do 42) (saveInt 99)
          |]

        -- === Negative: effect cannot drop when caller expects pure ===
        -- The pure-into-EffectU rule must NOT be bidirectional; the
        -- reverse (EffectU-into-pure) is unsound and must still reject.
      , expectError
          "7. <IO> Int in Int slot rejected (wrong subtype direction)"
          [r|
        module main (f)
        effect IO
        source Py ("io_int")
        io_int :: <IO> Int
        f :: Int
        f = io_int
          |]

      , expectError
          "8. <IO> Int in Int slot rejected via direct assignment"
          [r|
        module main (bad)
        effect IO
        source Py ("readInt")
        readInt :: <IO> Int
        bad :: Int
        bad = readInt
          |]

        -- === Negative: @catch on non-Err fallible still rejected ===
        -- Loosening the pure-into-EffectU rule must NOT loosen
        -- @catch's requirement that its fallible have Err in its row.
      , expectError
          "9. @catch on <IO>-only (no Err) fallible rejected"
          [r|
        module main (bad)
        effect IO
        source Py ("readInt")
        readInt :: <IO> Int
        bad :: <IO> Int
        bad = @catch readInt 0
          |]
      ]

-- | Row-inheritance of @catch. The fallback declares its own effect row
-- and the whole @catch inherits it, so the same operator handles both
-- "recover to pure" (fallback is <>) and "fall through to another
-- fallible attempt" (fallback keeps Err). The primary's non-Err effects
-- propagate too.
effectCoverageMessageTests :: TestTree
effectCoverageMessageTests =
  localOption (mkTimeout 200000) $
    testGroup
      "Effect-coverage error messages (rejection-only, message quality checked manually)"
      [ -- Err missing → message should suggest declare + mention @catch.
        expectError
          "effect in body, sig declares only <IO> → rejected"
          [r|
        module main (bad)
        effect IO
        effect Audit
        source Py ("paudit")
        paudit :: Str -> <Audit> ()
        bad :: <IO> Int
        bad = do
          paudit "oops"
          0
          |]

        -- Non-Err escapable missing → message should suggest declare
        -- + mention handler function (no @catch mention).
      , expectError
          "escapable non-Err in body, sig pure → rejected"
          [r|
        module main (bad)
        escapable effect Log
        source Py ("plog")
        plog :: Str -> <Log> ()
        bad :: Int
        bad = do
          plog "hi"
          0
          |]

        -- Non-escapable missing → message should suggest declaration
        -- ONLY (no handler-function mention).
      , expectError
          "inescapable effect in body, sig pure → rejected"
          [r|
        module main (bad)
        effect IO
        source Py ("psideEffect")
        psideEffect :: Int -> <IO> Int
        bad :: Int
        bad = do
          x <- psideEffect 1
          x
          |]

        -- Mixed escapable + inescapable missing → message should
        -- suggest declaration for all, mention handler for escapable
        -- ones, and specifically @catch for Err.
      , expectError
          "Err + inescapable both missing → rejected"
          [r|
        module main (bad)
        effect IO
        escapable effect Err
        source Py ("psideEffect")
        psideEffect :: Int -> <IO> Int
        bad :: Int
        bad = do
          x <- psideEffect 1
          @throw "boom"
          x
          |]
      ]

-- | `IFile a` names the whole value stored in the file, while `IStream a`
-- names the element of a list file. `@stream` turns the one into the other,
-- so it takes an `IFile [a]` and gives an `IStream a`, and `@next` on that
-- stream gives `[a]`. A file whose value is not a list has no elements to
-- stream.
streamIntrinsicTests :: TestTree
streamIntrinsicTests =
  localOption (mkTimeout 1000000) $ -- 1s
    testGroup
      "@stream element type"
      [ expectPass
          "@stream on IFile [Int] then @next gives [Int]"
          [r|
        module main (foo)
        effect IO
        foo :: IFile [Int] -> <IO> (Try Str [Int])
        foo f = do
          s <- @stream f
          @next s
          |]
      , expectPass
          "@stream strips one list layer from IFile [[Int]]"
          [r|
        module main (foo)
        effect IO
        foo :: IFile [[Int]] -> <IO> (Try Str [[Int]])
        foo f = do
          s <- @stream f
          @next s
          |]
      , expectError
          "@stream on IFile [Int] does not give [[Int]]"
          [r|
        module main (foo)
        effect IO
        foo :: IFile [Int] -> <IO> (Try Str [[Int]])
        foo f = do
          s <- @stream f
          @next s
          |]
      , expectPass
          "@stream through an alias of the element list"
          [r|
        module main (foo)
        effect IO
        type Rows = [Int]
        foo :: IFile Rows -> <IO> (Try Str [Int])
        foo f = do
          s <- @stream f
          @next s
          |]
      , expectPass
          "@stream through an alias of the whole handle"
          [r|
        module main (foo)
        effect IO
        type Rows = IFile [Int]
        foo :: Rows -> <IO> (Try Str [Int])
        foo f = do
          s <- @stream f
          @next s
          |]
      , expectPass
          "@stream in a polymorphic helper"
          [r|
        module main (foo)
        effect IO
        streamOf :: IFile [a] -> <IO> IStream a
        streamOf f = @stream f
        foo :: IFile [Int] -> <IO> (Try Str [Int])
        foo f = do
          s <- streamOf f
          @next s
          |]
      , expectPass
          "@stream and @next on receivers typed by inference"
          [r|
        module main (foo)
        effect IO
        foo :: IFile [Int] -> <IO> (Try Str [Int])
        foo f = do
          let open = \h -> @stream h
          let pull = \s -> @next s
          s <- open f
          pull s
          |]
      , expectError
          "@stream on a receiver inferred as a non-list file is rejected"
          [r|
        module main (foo)
        effect IO
        foo :: IFile Int -> <IO> (Try Str [Int])
        foo f = do
          let open = \h -> @stream h
          s <- open f
          @next s
          |]
      , expectError
          "@stream on a file whose value is not a list is rejected"
          [r|
        module main (foo)
        effect IO
        foo :: IFile Int -> <IO> (Try Str [Int])
        foo f = do
          s <- @stream f
          @next s
          |]
      ]

-- | A suspension @<E> T@ is a value distinct from @T@ with no coercion
-- either way; @do v@ is the only way to build one from a value; the empty
-- row is a row; a row over a suspension is a second layer, distinct from
-- a row union and written only with parentheses.
suspensionLawTests :: TestTree
suspensionLawTests =
  localOption (mkTimeout 5000000) $
    testGroup
      "suspension law"
      [ expectError
          "a value does not fill a suspension slot"
          [r|
        module main (passPlain)
        effect IO
        source Py ("run_t")
        run_t :: <IO> Int -> <IO> Int
        passPlain :: Int -> <IO> Int
        passPlain k = run_t k
          |]
      , expectPass
          "a bind runs a suspension whose type is an alias"
          [r|
        module main (viaAlias)
        effect IO
        source Py ("get")
        type IOInt = <IO> Int
        get :: Int -> IOInt
        viaAlias :: Int -> <IO> Int
        viaAlias n = do
          x <- get n
          do x
          |]
      , expectPass
          "a do-block tail of alias type is run"
          [r|
        module main (viaAlias)
        effect IO
        source Py ("get", "put")
        type IOInt = <IO> Int
        get :: Int -> IOInt
        put :: Int -> <IO> Int
        viaAlias :: Int -> <IO> Int
        viaAlias n = do
          put n
          get n
          |]
      , expectPass
          "do builds the constant suspension that fills it"
          [r|
        module main (passPlain)
        effect IO
        source Py ("run_t")
        run_t :: <IO> Int -> <IO> Int
        passPlain :: Int -> <IO> Int
        passPlain k = run_t (do k)
          |]
      , expectError
          "a declared row is not a lift of a pure body"
          [r|
        module main (stub)
        effect IO
        stub :: Int -> <IO> Int
        stub k = k + 1
          |]
      , expectError
          "a guard base case is not lifted"
          [r|
        module main (loop)
        effect IO
        source Py ("next_int")
        next_int :: Int -> <IO> Int
        source Py ("le")
        le :: Int -> Int -> Bool
        loop :: Int -> Int -> <IO> Int
        loop k n = ? le n 0 = 0 : next_int n
          |]
      , expectPass
          "a constant suspension joins an effectful branch by row inclusion"
          [r|
        module main (loop)
        effect IO
        source Py ("next_int")
        next_int :: Int -> <IO> Int
        source Py ("le")
        le :: Int -> Int -> Bool
        loop :: Int -> Int -> <IO> Int
        loop k n = ? le n 0 = (do 0) : next_int n
          |]
      , expectError
          "a do-block is a suspension, not a value, at a pure return"
          [r|
        module main (f)
        f :: Int -> Int
        f x = do
          let y = x
          y
          |]
      , assertGeneralType
          "the empty row is a row"
          [r|
        module main (f)
        f :: Int -> <> Int
        f x = do x
          |]
          (fun [int, emptyEff int])
      , assertGeneralType
          "a row over a suspension is a second layer"
          [r|
        module main (prep)
        effect IO
        source Py ("next_int")
        next_int :: Int -> <IO> Int
        prep :: Int -> <IO> (<IO> Int)
        prep k = do
          x <- next_int k
          next_int x
          |]
          (fun [int, ioEff (ioEff int)])
      , expectError
          "two layers are not one layer"
          [r|
        module main (once)
        effect IO
        source Py ("next_int")
        next_int :: Int -> <IO> Int
        prep :: Int -> <IO> (<IO> Int)
        prep k = do
          x <- next_int k
          next_int x
        once :: Int -> <IO> Int
        once k = prep k
          |]
      , expectError
          "a second layer is written with parentheses"
          [r|
        module main (prep)
        effect IO
        prep :: Int -> <IO> <IO> Int
        prep k = do (do k)
          |]
      , expectPass
          "a suspension instantiates a type variable"
          [r|
        module main (viaId)
        effect IO
        source Py ("next_int")
        next_int :: Int -> <IO> Int
        ident :: a -> a
        ident x = x
        viaId :: Int -> <IO> Int
        viaId k = ident (next_int k)
          |]
      , expectError
          "a suspension is not its result"
          [r|
        module main (f)
        effect IO
        source Py ("next_int")
        next_int :: Int -> <IO> Int
        f :: Int -> Int
        f k = next_int k
          |]
      ]
