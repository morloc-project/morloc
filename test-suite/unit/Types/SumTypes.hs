{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.SumTypes
  ( newtypeTests
  , sumTypeTests
  , variantTests
  ) where

import Morloc.Frontend.Namespace
import Text.RawString.QQ

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Types.Common

-- | Tests for the @newtype@ declaration's nominal-distinct, wire-equivalent
-- semantics. @newtype Foo = Bar@ inherits only the wire format from Bar;
-- typeclass instances, per-language overrides, and type identity are Foo's
-- own. Covers basic acceptance, instance non-inheritance, frontend
-- invariants (Invariant 1: transparent @type@ alias cannot carry a
-- per-language form; Invariant 2: cycle in the newtype wire-parent graph
-- is rejected), and composition with @?@/@<E>@ wrappers.
newtypeTests :: TestTree
newtypeTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "newtype declaration"
      [ -- A bare newtype with no inherited instances should parse and
        -- typecheck. The wire-parent identity (List) is preserved in the
        -- general scope; the newtype identity (MyList) is what the
        -- typechecker sees at the boundary.
        assertGeneralType
          "bare newtype parses and is distinct from wire-parent"
          [r|
        module main (f)
        newtype MyList a = List a
        f :: MyList Int -> MyList Int
        f x = x
          |]
          (fun [arr "MyList" [int], arr "MyList" [int]])

      , -- A newtype with an explicit typeclass instance is usable through
        -- that instance. The wire-parent's instance is irrelevant here:
        -- only the newtype's own instance routes to the dispatch.
        assertGeneralType
          "newtype with explicit Functor instance routes through it"
          [r|
        module main (f)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        newtype MyList a = List a
        instance Functor MyList
        f :: MyList Int -> MyList Int
        f = fmap (\x -> x)
          |]
          (fun [arr "MyList" [int], arr "MyList" [int]])

      , -- Without an explicit instance on the newtype, a typeclass method
        -- declared on the wire-parent is not visible. Instance resolution
        -- stops at the newtype boundary.
        exprTestBad
          "newtype does NOT inherit Functor instance from wire-parent"
          [r|
        module main (f)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        newtype MyList a = List a
        instance Functor List
        f :: MyList Int -> MyList Int
        f = fmap (\x -> x)
          |]

      , -- Invariant 1: a transparent @type@ alias cannot carry a per-language
        -- form. The right alternative is to declare @Foo@ as a @newtype@.
        exprTestBad
          "type alias with per-language form is rejected"
          [r|
        module main (f)
        type Foo = Int
        type Py => Foo = "my_foo"
        f :: Foo -> Foo
        f x = x
          |]

      , -- Invariant 2: a cycle in the newtype wire-parent graph is rejected.
        exprTestBad
          "cyclic newtype chain is rejected"
          [r|
        module main (f)
        newtype A = B
        newtype B = A
        f :: A -> A
        f x = x
          |]

      , -- Optional composes structurally with newtype: ?(MyList Int) is
        -- a distinct nominal type whose wire format is Optional (List Int).
        assertGeneralType
          "newtype composes with Optional wrapper"
          [r|
        module main (f)
        newtype MyList a = List a
        f :: ?(MyList Int) -> ?(MyList Int)
        f x = x
          |]
          (fun [OptionalU (arr "MyList" [int]), OptionalU (arr "MyList" [int])])

      , -- Effect wrappers compose with newtype the same way.
        assertGeneralType
          "newtype composes with effect wrapper"
          [r|
        module main (f)
        effect IO
        newtype MyList a = List a
        f :: MyList Int -> <IO> MyList Int
        f x = do x
          |]
          (fun [arr "MyList" [int], ioEff (arr "MyList" [int])])

      , -- A newtype with its own per-language form. The Cpp form is
        -- registered alongside the general declaration; the typechecker
        -- treats MyInt as nominally distinct from Int even though both
        -- compile to @int@ in C++ here. (No instance inheritance.)
        assertGeneralType
          "newtype with explicit per-language form"
          [r|
        module main (f)
        newtype MyInt = Int
        type Cpp => MyInt = "int"
        f :: MyInt -> MyInt
        f x = x
          |]
          (fun [var "MyInt", var "MyInt"])

      , -- A newtype whose wire-parent is another newtype: instance and
        -- per-language lookup walk the wire-parent chain in code-gen but
        -- treat each level as a distinct nominal type at the type level.
        assertGeneralType
          "chained newtypes: distinct identities, shared wire format"
          [r|
        module main (f)
        newtype Inner = Int
        newtype Outer = Inner
        f :: Outer -> Outer
        f x = x
          |]
          (fun [var "Outer", var "Outer"])

      , -- A vacuous newtype body (Foo = Foo) is rejected by the same
        -- self-reference check that rejects vacuous type aliases.
        exprTestBad
          "vacuous newtype body is rejected"
          [r|
        module main (f)
        newtype Foo = Foo
        f :: Foo -> Foo
        f x = x
          |]
      ]

-- Sum types: declaration syntax, constructor scoping, and typechecking.
--
-- Stage 1 is `data` with nullary constructors only. Constructors are bare
-- terms in the module's term namespace and are globally unique; a name
-- already bound is a hard error at the second declaration. Because a
-- constructor name determines its type, `f = A` synthesizes `DNA` with no
-- annotation.
--
-- NOTE while this is red: `data` is not yet a keyword, so every source
-- below fails to parse. The positive cases therefore fail (which is the
-- point), but the negative cases pass *vacuously* -- they are rejected for
-- the wrong reason. A negative here only earns its keep once the positives
-- are green.
sumTypeTests :: TestTree
sumTypeTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "sum types (`data`)"
      [ -- The declaration introduces a nominal type usable in a signature.
        assertGeneralType
          "nullary data declares a type"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: DNA -> DNA
        f x = x
          |]
          (fun [var "DNA", var "DNA"])

      , -- A constructor is a term of its own type.
        assertGeneralType
          "constructor is a term of the declared type"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: DNA
        f = G
          |]
          (var "DNA")

      , -- Uniqueness is what makes this work: the name alone picks the type,
        -- so no annotation is needed to synthesize it.
        assertGeneralType
          "constructor name alone determines the type"
          [r|
        module main (f)
        data Color = Red | Green | Blue
        f = Red
          |]
          (var "Color")

      , assertGeneralType
          "a single-constructor data is legal"
          [r|
        module main (f)
        data Unitary = Only
        f :: Unitary
        f = Only
          |]
          (var "Unitary")

      , assertGeneralType
          "enum inside a list"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: [DNA] -> [DNA]
        f x = x
          |]
          (fun [lst (var "DNA"), lst (var "DNA")])

      , assertGeneralType
          "enum as a record field"
          [r|
        module main (f)
        data DNA = A | C | G | T
        record Site where
          base :: DNA
          pos  :: Int
        f :: Site -> DNA
        f = .base
          |]
          (fun [record' "Site" [(Key "base", var "DNA"), (Key "pos", int)], var "DNA"])

      , assertGeneralType
          "enum under an optional"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: ?DNA -> ?DNA
        f x = x
          |]
          (fun [OptionalU (var "DNA"), OptionalU (var "DNA")])

      , assertGeneralType
          "a per-language binding does not change the general type"
          [r|
        module main (f)
        data DNA = A | C | G | T
        data Cpp => DNA = "dna_t"
        f :: DNA -> DNA
        f x = x
          |]
          (fun [var "DNA", var "DNA"])

      , -- Two distinct enums coexist as long as no constructor name repeats.
        assertGeneralType
          "two enums with disjoint constructors coexist"
          [r|
        module main (f)
        data DNA = A | C | G | T
        data Strand = Fwd | Rev
        f :: DNA -> Strand
        f _ = Fwd
          |]
          (fun [var "DNA", var "Strand"])

      , exprTestBad
          "a constructor repeated within one type is rejected"
          [r|
        module main (f)
        data DNA = A | C | A | T
        f :: DNA -> DNA
        f x = x
          |]

      , -- The uniqueness rule, which is what buys the inference above.
        exprTestBad
          "a constructor colliding with another type's constructor is rejected"
          [r|
        module main (f)
        data DNA = A | C | G | T
        data Other = A | B
        f :: DNA -> DNA
        f x = x
          |]

      , exprTestBad
          "a constructor colliding with an existing term is rejected"
          [r|
        module main (f)
        data DNA = A | C | G | T
        A :: Int
        f :: DNA -> DNA
        f x = x
          |]

      , exprTestBad
          "an undeclared constructor is rejected"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: DNA
        f = N
          |]

      , exprTestBad
          "a constructor of the wrong enum is rejected"
          [r|
        module main (f)
        data DNA = A | C | G | T
        data Strand = Fwd | Rev
        f :: DNA
        f = Fwd
          |]

      , -- Derived instances are deliberately deferred, so `==` on an enum
        -- must be a clean error rather than an internal dump or a silent
        -- fallthrough to some structural comparison.
        exprTestBad
          "== on an enum has no instance in stage 1"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: DNA -> Bool
        f x = x == A
          |]

      , -- A clause naming a constructor of a different type is a type
        -- error, not a parse error: the constructor resolves fine, it just
        -- does not belong to the scrutinee's type.
        exprTestBad
          "a constructor from another enum is rejected in a clause"
          [r|
        module main (f)
        data Color = Red | Green
        data Strand = Fwd | Rev
        f :: Color -> Int
        f | Red = 0
          | Fwd = 1
          |]

      , exprTestBad
          "an enum used where an unrelated type is expected is rejected"
          [r|
        module main (f)
        data DNA = A | C | G | T
        f :: Int
        f = A
          |]
      ]

-- Payload-bearing `data` constructors.
--
-- A constructor with arguments is a FUNCTION into its type: `Circle Real`
-- gives `Circle :: Real -> Shape`. That is the whole difference from the
-- argument-free tier, where a constructor is a value. A type is
-- payload-bearing if ANY of its constructors takes an argument, so `Dot` in
-- `data Shape = Circle Real | Dot` is a variant with an empty payload, not
-- an enum member -- the wire form is a property of the type, not of the
-- individual constructor.
variantTests :: TestTree
variantTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "sum types with payloads"
      [ assertGeneralType
          "a one-argument constructor is a function into its type"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f = Circle
          |]
          (fun [real, var "Shape"])

      , assertGeneralType
          "a multi-argument constructor curries"
          [r|
        module main (f)
        data Shape = Rect Real Real | Dot
        f = Rect
          |]
          (fun [real, real, var "Shape"])

      , -- A constructor with no arguments in a payload-bearing type is still
        -- a plain value of that type.
        assertGeneralType
          "an argument-free constructor beside a payload one is a value"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f :: Shape
        f = Dot
          |]
          (var "Shape")

      , assertGeneralType
          "applying a constructor yields its type"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f :: Shape
        f = Circle 1.0
          |]
          (var "Shape")

      , assertGeneralType
          "a parameterized constructor carries the parameter"
          [r|
        module main (f)
        data Opt a = None | Some a
        f :: Int -> Opt Int
        f x = Some x
          |]
          (fun [int, arr "Opt" [int]])

      , -- The payload is behind a pointer on the wire, which is what gives
        -- the recursive width equation a finite fixed point.
        assertGeneralType
          "a self-recursive data type is legal"
          [r|
        module main (f)
        data Tree = Leaf | Node Tree Tree
        f :: Tree -> Tree
        f x = x
          |]
          (fun [var "Tree", var "Tree"])

      , exprTestBad
          "applying a constructor to too many arguments is rejected"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f :: Shape
        f = Circle 1.0 2.0
          |]

      , exprTestBad
          "applying a constructor to the wrong type is rejected"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f :: Shape
        f = Circle "big"
          |]

      , exprTestBad
          "an argument-free constructor is not a function"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f :: Shape
        f = Dot 1.0
          |]

      , -- A constructor is an ordinary function into its type, so supplying
        -- some of its arguments yields a function expecting the rest. This
        -- is what makes `map (Rect 4.5) heights` work.
        assertGeneralType
          "a constructor may be partially applied"
          [r|
        module main (f)
        data Shape = Rect Real Real | Dot
        f = Rect 4.5
          |]
          (fun [real, var "Shape"])

      , -- Payload patterns are asserted here, through inference, and not
        -- only in the pattern-lowering tests: those run the frontend
        -- without the typechecker, so they cannot see that a payload
        -- constructor is a FUNCTION into its type and therefore cannot be
        -- checked as a value of it, the way an argument-free one can.
        assertGeneralType
          "a payload pattern binds its field at the field's type"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        f :: Shape -> Real
        f | (Circle r) = r
          | Dot = 0.0
          |]
          (fun [var "Shape", real])

      , assertGeneralType
          "a multi-field payload pattern types every field"
          [r|
        module main (f)
        data Shape = Rect Real Real | Dot
        f :: Shape -> Real
        f | (Rect w h) = w
          | Dot = 0.0
          |]
          (fun [var "Shape", real])

      , -- The constructor-table lookup replaces the value check for a
        -- payload type, so it has to keep rejecting a constructor borrowed
        -- from another type just as firmly.
        exprTestBad
          "a constructor of another type is rejected in a payload clause"
          [r|
        module main (f)
        data Shape = Circle Real | Dot
        data Color = Red Real | Blue
        f :: Shape -> Real
        f | (Circle r) = r
          | (Red x) = x
          | Dot = 0.0
          |]

      , -- A getter names a position, but which positions exist depends on
        -- the constructor, and that is not known until the value is
        -- matched. Only a declaration whose arms happen to agree at that
        -- position would type, and a later arm would silently break it.
        exprTestBad
          "an index getter cannot be applied to a data type"
          [r|
        module main (f)
        data Shape = Circle Real | Rect Real Real
        f :: Shape -> Real
        f x = .[0] x
          |]

      , -- The same rejection has to survive an alias. A getter reaching a
        -- `data` type through one is not a corner case: aliases are how
        -- types are usually named in signatures, and the check is worth
        -- nothing if the spelling of the type decides whether it applies.
        exprTestBad
          "an index getter cannot reach a data type through an alias"
          [r|
        module main (f)
        data Shape = Circle Real | Rect Real Real
        type MyShape = Shape
        f :: MyShape -> Real
        f x = .[0] x
          |]

        -- Recursion guards. A constructor field naming its own type is the
        -- reason the payload sits behind a pointer at all, so these are the
        -- shapes the representation exists to support.
        --
        -- Lowering a `data` expands its constructors' field types, because
        -- the payload's own schema has to be reachable. A self-reference
        -- therefore re-enters the type being lowered. Termination comes from
        -- leaving a re-entrant reference opaque -- the same treatment a
        -- record's self-reference gets -- and cutting the cycle once, later,
        -- when the wire form is built. Without that, two self-referential
        -- fields make the expansion branch, so it exhausts memory rather
        -- than merely looping; hence a generation test with a timeout rather
        -- than a type assertion.
      , localOption (mkTimeout 20000000) $
          testCase "a directly self-recursive data lowers" $
            assertGenerates
              [r|
        module main (idt)
        data Tree = Leaf | Node Tree Tree
        source Py from "t.py" ("idt")
        idt :: Tree -> Tree
              |]

      , -- Recursion reached through a list rather than directly. The
        -- back-edge sits under an array element here, which is a different
        -- position in the wire form than a bare field.
        localOption (mkTimeout 20000000) $
          testCase "a data recursing through a list lowers" $
            assertGenerates
              [r|
        module main (idt)
        type Py => List a = "list" a
        data Rose = Rose [Rose]
        source Py from "t.py" ("idt")
        idt :: Rose -> Rose
              |]

      , -- Two `data` types that refer to each other. A `data` stops type
        -- reduction at its own boundary and holds its payload behind a
        -- pointer, so nothing that walks the cycle can loop and no value
        -- has infinite width -- the same two facts that make a `data`
        -- self-recursive, applied across two names.
        localOption (mkTimeout 20000000) $
          testCase "mutually recursive data types lower" $
            assertGenerates
              [r|
        module main (idt)
        data Expr = Lit | Neg Term
        data Term = Wrap Expr
        source Py from "t.py" ("idt")
        idt :: Expr -> Expr
              |]

      , -- INTR-7: @eq and @le compare the wire form, so what they reject
        -- is decided there: a function or handle anywhere in it, but not a
        -- type parameter the wire form does not hold.
        localOption (mkTimeout 20000000) $
          testCase "spec-intr-7-6" $ do
            assertGenerates
              [r|
        module main (f)
        type Py => Int = "int"
        type Py => Bool = "bool"
        type Py => (IFile a) = "int" a
        source Py from "t.py" ("mk", "inc")
        inc :: Int -> Int
        record Tag a where
          n :: Int
        record Py => Tag = "dict"
        mk :: Int -> Tag (Int -> Int)
        f :: Int -> Bool
        f x = @eq (mk x) (mk 1)
              |]
            assertGenerationFails
              [r|
        module main (f)
        type Py => Int = "int"
        type Py => Bool = "bool"
        type Py => (IFile a) = "int" a
        source Py from "t.py" ("mk", "inc")
        inc :: Int -> Int
        record R where
          g :: Int -> Int
          n :: Int
        record Py => R = "dict"
        mkR :: Int -> R
        mkR x = {g = inc, n = x}
        f :: Int -> Bool
        f x = @eq (mkR x) (mkR 1)
              |]
            assertGenerationFails
              [r|
        module main (f)
        type Py => Int = "int"
        type Py => Bool = "bool"
        type Py => (IFile a) = "int" a
        source Py from "t.py" ("mk", "inc")
        inc :: Int -> Int
        record R where
          h :: IFile Int
          n :: Int
        record Py => R = "dict"
        mk :: Int -> R
        f :: Int -> Bool
        f x = @eq (mk x) (mk 1)
              |]
            assertGenerationFails
              [r|
        module main (f)
        type Py => Int = "int"
        type Py => Bool = "bool"
        type Py => (IFile a) = "int" a
        source Py from "t.py" ("mk", "inc")
        inc :: Int -> Int
        newtype N = (Int -> Int)
        mk :: Int -> N
        f :: Int -> Bool
        f x = @eq (mk x) (mk 1)
              |]

      , -- A declared table column is checked against the block's Arrow
        -- type at every crossing, and only flat primitives, Str and
        -- optionals of them have an Arrow form the runtime can name. A
        -- list column would compile and fail on the first receive, so it
        -- is refused when the wire form is built.
        localOption (mkTimeout 20000000) $
          testCase "a declared table column must be primitive" $ do
            assertGenerates
              [r|
        module main (idt)
        newtype Table (n :: Nat) (r :: Rec)
        type Py => (Table (n :: Nat) (r :: Rec)) = "arrow" n r
        type Py => Int = "int"
        type Py => Str = "str"
        source Py from "t.py" ("idt")
        idt :: Table n {x = Int, s = ?Str} -> Table n {x = Int, s = ?Str}
              |]
            assertGenerationFails
              [r|
        module main (idt)
        newtype Table (n :: Nat) (r :: Rec)
        type Py => (Table (n :: Nat) (r :: Rec)) = "arrow" n r
        type Py => Int = "int"
        type Py => List a = "list" a
        source Py from "t.py" ("idt")
        idt :: Table n {xs = [Int]} -> Table n {xs = [Int]}
              |]

      , -- A record on the cycle is fine as long as a `data` is on it too:
        -- the `data` is where reduction stops.
        localOption (mkTimeout 20000000) $
          testCase "a record and a data recursing into each other lower" $
            assertGenerates
              [r|
        module main (idt)
        record Node where
          body :: Expr
        record Py => Node = "dict"
        data Expr = Leaf | Branch Node
        source Py from "t.py" ("idt")
        idt :: Expr -> Expr
              |]

      , -- Without a `data` on the cycle nothing stops reduction: a cycle of
        -- transparent aliases would loop the evaluator, and a cycle of
        -- records has no cycle-level guard. Both stay rejected.
        exprTestBad
          "mutually recursive aliases are rejected"
          [r|
        module main (idt)
        type A = [B]
        type B = [A]
        source Py from "t.py" ("idt")
        idt :: A -> A
          |]

      , -- A self-recursive type is still usable through a container, which
        -- is the shape a real tree walk takes.
        localOption (mkTimeout 20000000) $
          testCase "a list of a recursive data lowers" $
            assertGenerates
              [r|
        module main (idt)
        type Py => List a = "list" a
        data Tree = Leaf | Node Tree Tree
        source Py from "t.py" ("idt")
        idt :: [Tree] -> [Tree]
              |]
      ]
