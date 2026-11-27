{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Literals
  ( pendingNumLitTests
  , literalDispatchTests
  ) where

import Morloc.Frontend.Namespace
import Text.RawString.QQ

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Types.Common

-- Numeric literals appearing in argument positions where the
-- function's parameter type is an unsolved existential. The fix in
-- @checkE (IntS / RealS)@ defers the literal binding until a later
-- arg pins the existential. If nothing pins it, the kind-appropriate
-- default (Int / Real) is applied at end-of-typecheck.
--
-- @IntS@ also accepts real base types (Int-to-Real promotion), so
-- expressions like @4 + 2.3@ where the @+@ wants @Real@ on both sides
-- let the @4@ adopt @Real@ rather than locking the existential to
-- @Int@ and failing.
pendingNumLitTests :: TestTree
pendingNumLitTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Numeric literals in polymorphic argument positions"
      [ -- ----- Bare top-level: synth-mode default -----
        assertGeneralType
          "bare int literal defaults to Int"
          [r|
        module main (x)
        x = 42
        |]
          int
      , assertGeneralType
          "bare real literal defaults to Real"
          [r|
        module main (x)
        x = 4.2
        |]
          real

      -- ----- Direct integer-to-real promotion at the literal site -----
      , assertGeneralType
          "int literal :: Real (promotes)"
          [r|
        module main (x)
        x :: Real
        x = 42
        |]
          real
      , assertGeneralType
          "int literal :: F32 (promotes)"
          [r|
        module main (x)
        x :: F32
        x = 42
        |]
          (var "F32")
      , assertGeneralType
          "int literal :: F64 (promotes)"
          [r|
        module main (x)
        x :: F64
        x = 42
        |]
          (var "F64")

      -- ----- Mixed-numeric list literal -----
      , assertGeneralType
          "list of int + real literals at [Real]"
          [r|
        module main (x)
        x :: List Real
        x = [1, 2, 3.0]
        |]
          (lst real)

      -- ----- Polymorphic identity: literal flows through, default applies -----
      , assertGeneralType
          "id of int literal defaults to Int"
          [r|
        module main (x)
        id :: a -> a
        x = id 42
        |]
          int
      , assertGeneralType
          "id of real literal defaults to Real"
          [r|
        module main (x)
        id :: a -> a
        x = id 4.2
        |]
          real

      -- ----- Polymorphic binary function: both args share the existential -----
      , assertGeneralType
          "add of two int literals defaults to Int"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 1 1
        |]
          int
      , assertGeneralType
          "add of two real literals defaults to Real"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 1.5 2.5
        |]
          real
      , assertGeneralType
          "add of int + real literals -- int promotes (1 + 2.3)"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 1 2.3
        |]
          real
      , assertGeneralType
          "add of real + int literals -- int promotes (2.3 + 1)"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 2.3 1
        |]
          real

      -- ----- Annotated arg pins the existential, literal adopts the width -----
      , assertGeneralType
          "add of int literal + I8-annotated -- literal adopts I8"
          [r|
        module main (x)
        add :: a -> a -> a
        y :: I8
        y = 5
        x :: I8
        x = add 1 y
        |]
          (var "I8")

      -- ----- A later argument that is itself an application pins the literal -----
      , assertGeneralType
          "int literal adopts Real from a later applied argument"
          [r|
        module main (x)
        pair :: a -> a -> (a, a)
        toR :: Int -> Real
        x = pair 7 (toR 1)
        |]
          (tuple [real, real])
      , assertGeneralType
          "int literal adopts ?Int from a later applied argument"
          [r|
        module main (x)
        pair :: a -> a -> (a, a)
        wrap :: Int -> ?Int
        x = pair 7 (wrap 1)
        |]
          (tuple [OptionalU int, OptionalU int])
      , assertGeneralType
          "int literal in a list adopts Real from a later list argument"
          [r|
        module main (x)
        pair :: a -> a -> (a, a)
        toR :: Int -> Real
        x = pair [7] [toR 1]
        |]
          (tuple [lst real, lst real])
      , assertGeneralType
          "int literal in a nested list adopts Real from a later argument"
          [r|
        module main (x)
        pair :: a -> a -> (a, a)
        toR :: Int -> Real
        x = pair [[7]] [[toR 1]]
        |]
          (tuple [lst (lst real), lst (lst real)])
      , assertGeneralType
          "int literal in a tuple adopts Real from a later tuple argument"
          [r|
        module main (x)
        pair :: a -> a -> (a, a)
        toR :: Int -> Real
        x = pair (7, 1) (toR 1, 2)
        |]
          (tuple [tuple [real, int], tuple [real, int]])
      , assertGeneralType
          "int literal at the head of a list adopts Real from a later element"
          [r|
        module main (x)
        toR :: Int -> Real
        x = [7, toR 1]
        |]
          (lst real)
      , assertGeneralType
          "list of int literals defaults to Int"
          [r|
        module main (x)
        x = [7, 8, 9]
        |]
          (lst int)
      , assertGeneralType
          "tuple of an int and a real literal keeps both"
          [r|
        module main (x)
        x = (7, 2.5)
        |]
          (tuple [int, real])
      , assertGeneralType
          "int literal before an applied Int argument stays Int"
          [r|
        module main (x)
        pair :: a -> a -> (a, a)
        inc :: Int -> Int
        x = pair 7 (inc 1)
        |]
          (tuple [int, int])

      -- ----- Negative: numeric literal in a non-numeric slot fails -----
      , exprTestBad
          "int literal in Bool slot rejected"
          [r|
        module main (x)
        x :: Bool
        x = 42
        |]
      , exprTestBad
          "real literal in Int slot rejected (no demotion)"
          [r|
        module main (x)
        x :: Int
        x = 4.2
        |]
      ]

-- | Tests for value-level literals being accepted at newtype-typed
-- positions. The typechecker walks the newtype's wire-parent chain via
-- @wireParentRoot@: if the chain reaches the literal's natural primitive
-- (e.g. @newtype Foo = Int@ for an integer literal), the literal is
-- accepted at the newtype's identity. Codegen handles per-language
-- conversion via @*Like@ instances when the newtype's effective
-- per-language form diverges from the wire-parent's; the typechecker
-- itself doesn't consult @*Like@ instances.
literalDispatchTests :: TestTree
literalDispatchTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Literal acceptance at newtype types"
      [ -- An integer literal inhabits a newtype whose wire-parent
        -- chain reaches @Int@.
        assertGeneralType
          "integer literal at newtype Foo (Foo's wire-parent is Int)"
          [r|
        module main (x)
        newtype Foo = Int
        x :: Foo
        x = 5
          |]
          (var "Foo")

      , -- A string literal at a newtype whose wire-parent is Str.
        assertGeneralType
          "string literal at newtype Name (Name's wire-parent is Str)"
          [r|
        module main (x)
        newtype Name = Str
        x :: Name
        x = "Alice"
          |]
          (var "Name")

      , -- A boolean literal at a newtype whose wire-parent is Bool.
        assertGeneralType
          "bool literal at newtype Flag (Flag's wire-parent is Bool)"
          [r|
        module main (x)
        newtype Flag = Bool
        x :: Flag
        x = True
          |]
          (var "Flag")

      , -- A real literal at a newtype whose wire-parent is Real.
        assertGeneralType
          "real literal at newtype Speed (Speed's wire-parent is Real)"
          [r|
        module main (x)
        newtype Speed = Real
        x :: Speed
        x = 3.14
          |]
          (var "Speed")

      , -- A list literal at a Nat-parameterised newtype container whose
        -- wire-parent is List. Length-checks against the Nat dimension.
        assertGeneralType
          "list literal at newtype Vec n a (wire-parent is List a)"
          [r|
        module main (x)
        newtype Vec (n :: Nat) a = List a
        x :: Vec 3 Int
        x = [1, 2, 3]
          |]
          (arr "Vec" [NatLitU 3, var "Int"])

      , -- A literal whose natural primitive doesn't match the newtype's
        -- wire-parent chain is rejected. Foo here has wire-parent Str,
        -- so an integer literal cannot inhabit it.
        exprTestBad
          "integer literal rejected when newtype's wire-parent is not Int"
          [r|
        module main (x)
        newtype Foo = Str
        x :: Foo
        x = 5
          |]

      , -- Length mismatch against a Nat-parameterised newtype: the
        -- structural shape check accepts the list, then the existing
        -- nat-dim check rejects the wrong length.
        exprTestBad
          "list literal length mismatch at Vec 3 Int"
          [r|
        module main (x)
        newtype Vec (n :: Nat) a = List a
        x :: Vec 3 Int
        x = [1, 2]
          |]

      , -- Bare-VarT newtype-list: an unparameterised newtype that aliases
        -- a fully-applied list (e.g. @newtype Bytes = List U8@).
        -- A list literal at @Bytes@ is accepted because the wire-parent
        -- chain reaches a list shape; the children are typed at the
        -- wire-parent's element type.
        assertGeneralType
          "list literal at bare-VarT newtype Bytes (Bytes = List U8)"
          [r|
        module main (x)
        newtype Bytes = List U8
        x :: Bytes
        x = [1, 2, 3]
          |]
          (var "Bytes")

      , -- The same shape applied to a newtype whose body is a list of a
        -- different primitive. Confirms the element type comes from the
        -- wire-parent body, not from the user-facing type's args (which
        -- are empty here).
        assertGeneralType
          "list literal at bare-VarT newtype Reals (Reals = List Real)"
          [r|
        module main (x)
        newtype Reals = List Real
        x :: Reals
        x = [1.0, 2.0, 3.0]
          |]
          (var "Reals")

      , -- Chained newtype: a list literal at an outer newtype whose
        -- wire-parent walks through another newtype to reach List. The
        -- iterated 'wireParentRoot' must follow the full chain.
        assertGeneralType
          "list literal at chained newtype-of-newtype-of-List"
          [r|
        module main (x)
        newtype Inner = List Int
        newtype Outer = Inner
        x :: Outer
        x = [1, 2, 3]
          |]
          (var "Outer")

      , -- Bracket-index pattern on a Nat-parameterized container returns
        -- the element type (Vec n a -> a). The test inlines a stub
        -- __to_index__ since the desugarer wraps user-supplied bounds.
        assertGeneralType
          "bracket index on Vec n a returns a"
          [r|
        module main (f)
        class IndexLike i where
          __to_index__ :: i -> I64
        instance IndexLike Int where
          source Py ("identity" as __to_index__)
        class Indexable f where
          __access_index__ :: ?I64 -> f a -> a
        newtype Vec (n :: Nat) a = List a
        instance Indexable (Vec n)
        f :: Vec 5 Int -> Int
        f x = .[0] x
          |]
          (FunU [arr "Vec" [NatLitU 5, var "Int"]] (var "Int"))

      , -- Bracket slice on a List preserves the shape.
        assertGeneralType
          "bracket slice on List a returns List a"
          [r|
        module main (f)
        class IndexLike i where
          __to_index__ :: i -> I64
        instance IndexLike Int where
          source Py ("identity" as __to_index__)
        f :: [Int] -> [Int]
        f x = .[1:5] x
          |]
          (FunU [arr "List" [var "Int"]] (arr "List" [var "Int"]))
      ]
