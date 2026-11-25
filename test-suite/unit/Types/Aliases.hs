{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Aliases
  ( typeAliasTests
  , numericLiteralAliasTests
  , aliasExpansionTests
  , aliasConstructorTests
  , aliasTransparencyTests
  ) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc (render)
import qualified Morloc.Typecheck.Internal as MTI
import Text.RawString.QQ

import qualified Data.Map as Map
import qualified Data.Text as MT
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Types.Common

typeAliasTests :: TestTree
typeAliasTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test type alias substitutions"
      [ -- A per-language form's right-hand side is a name in the TARGET
        -- language, so it may legitimately coincide with the morloc name
        -- on the left. Binding a type to a same-named native type is the
        -- ordinary case, not a self-reference; both the vacuity check and
        -- the self-recursion check have to leave it alone.
        assertGeneralType
          "a per-language form may repeat the type's own name"
          [r|
        module main (f)
        newtype Foo = Int
        type Cpp => Foo = "Foo"
        f :: Foo -> Foo
        f x = x
          |]
          (fun [var "Foo", var "Foo"])

      , assertGeneralType
          "a parameterized per-language form may repeat the name"
          [r|
        module main (f)
        newtype Box a = List a
        type Cpp => Box a = "Box<$1>" a
        f :: Box Int -> Box Int
        f x = x
          |]
          (fun [arr "Box" [int], arr "Box" [int]])

      , -- The exemption is for the HEAD only. A morloc type in an argument
        -- slot is still a morloc type, so real recursion is still caught.
        exprTestBad
          "a per-language form recursing through a parameter is rejected"
          [r|
        module main (f)
        newtype Foo = Int
        type Cpp => Foo = "Foo" Foo
        f :: Foo -> Foo
        f x = x
          |]

      , -- And a general alias with no language is unaffected.
        exprTestBad
          "a vacuous general alias is still rejected"
          [r|
        module main (f)
        type Foo = Foo
        f :: Foo -> Foo
        f x = x
          |]

      , assertGeneralType
          "general type alias"
          [r|
        module main (f)
        type A
        type Foo = A
        f :: Foo
        |]
          (var "A")
      , assertGeneralType
          "parameterized generic"
          [r|
        module main (f)
        f :: m (a -> b)
        |]
          (forallu ["m@q0", "a@q1", "b@q2"] (arr "m@q0" [fun [var "a@q1", var "b@q2"]]))
      , assertGeneralType
          "non-parametric, general type alias"
          [r|
        module main (f)
        type A
        type B
        type Foo = A
        f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "deep type substitution: `[Foo] -> B`"
          [r|
        module main (f)
        type A
        type B
        type Foo = A
        f :: [Foo] -> B
        |]
          (fun [lst (var "A"), var "B"])
      , assertGeneralType
          "deep type substitution: `[Foo] -> Foo`"
          [r|
        module main (f)
        type A
        type Foo = A
        f :: [Foo] -> Foo
        |]
          (fun [lst (var "A"), var "A"])
      , assertGeneralType
          "parametric alias, general type alias"
          [r|
        module main (f)
        type X
        type Y
        type Z
        type (Foo a b) = (a,b)
        f :: Foo X Y -> Z
        |]
          (fun [tuple [var "X", var "Y"], var "Z"])
      , assertGeneralType
          "nested types"
          [r|
           module main (foo)
           type C
           type A = B
           type B = C
           foo :: A -> B -> C
        |]
          (fun [var "C", var "C", var "C"])
      , assertGeneralType
          "state is preserved across binding"
          [r|
           module main (f)
           type A
           type Foo = A
           g :: Foo -> Int
           f = g
        |]
          (fun [var "A", var "Int"])
      , assertGeneralType
          "state is inherited across binding"
          [r|
           module main (f)
           type A
           type Foo = A
           g :: a -> b
           f :: Foo -> Int
           f = g  {- yes, g isn't defined -}
        |]
          (fun [var "A", var "Int"])
      , expectError
          "fail on too many type aliases parameters"
          [r|
           type A = B
           foo :: A Int -> C
           foo
        |]
      , expectError
          "fail on too few type aliases parameters"
          [r|
           type (A a) = (a,a)
           foo :: A -> C
           foo
        |]
      , expectError
          "fail on conflicting types (Int vs Str)"
          [r|
           type A = Int
         
           module b (A)
           type A = Str
         
           module main (foo)
           import a (A)
           import .b (A)
         
           foo :: A -> A -> A
        |]
      , testCase "conflicting forms name the other declaration" $ do
          result <-
            runFront
              "module main (f)\nnewtype Foo = Int\ntype Py => Foo = \"int\"\ntype Py => Foo = \"float\"\nf :: Foo -> Foo\n"
          case result of
            Left (Rejected (AtNode _) d) ->
              assertBool (MT.unpack (render d)) (":3:1" `MT.isInfixOf` render d)
            Left e -> assertFailure $ "expected a sourced error, got " <> show e
            Right _ -> assertFailure "expected failure"
      , expectError
          "fail on conflicting types (Map vs List)"
          [r|
           module a (A)
           type A a b = Map a b
           
           module b (A)
           type A a b = List (Tuple2 a b)
           
           module main (foo)
           import .a (A)
           import .b (A)
           
           foo :: A a b -> A a b -> A a b
        |]
      , -- import tests ---------------------------------------
        assertGeneralType
          "non-parametric, general type alias, imported"
          [r|
           module m1 (Foo)
             type A
             type Foo = A
           module main (f)
             type B
             import .m1 (Foo)
             f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, reimported"
          [r|
           module m3 (Foo)
             type A
             type Foo = A
           module m2 (Foo)
             import .m3 (Foo)
           module m1 (Foo)
             import .m2 (Foo)
           module main (f)
             type B
             import .m1 (Foo)
             f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, imported aliased"
          [r|
           module m1 (Foo)
             type A
             type Foo = A
           module main (f)
             type B
             import .m1 (Foo as Bar)
             f :: Bar -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, reimported aliased"
          [r|
           module m3 (Foo1)
             type A
             type Foo1 = A

           module m2 (Foo2)
             import .m3 (Foo1 as Foo2)

           module m1 (Foo3)
             import .m2 (Foo2 as Foo3)

           module main (f)
             type B
             import .m1 (Foo3 as Foo4)
             f :: Foo4 -> B
        |]
          (fun [var "A", var "B"])
      , expectError
          "two imported aliases of one name are ambiguous"
          [r|
           module m2 (Foo)
             type A
             type Foo = A

           module m1 (Foo)
             type A
             type Foo = A

           module main (f)
             type B
             import .m1 (Foo)
             import .m2 (Foo)
             f :: Foo -> B
        |]
      , expectError
          "two imported parametric aliases of one name are ambiguous"
          [r|
           module m2 (Foo)
             type (Foo a b) = (a,b)

           module m1 (Foo)
             type (Foo c d) = (c,d)

           module main (f)
             type X
             type Y
             type Z
             import .m1 (Foo)
             import .m2 (Foo)
             f :: Foo X Y -> Z
        |]
      -- Type-level record literals use `=` to bind fields (mirroring
      -- morloc's term-level `{x = 3, y = "a"}` syntax). Mistakenly using
      -- `::` (the declaration separator) raises a specific parser error
      -- rather than a generic 'unexpected ::'.
      , expectError
          "type-level record literal: `{x :: Int}` raises a parse error"
          [r|
        module main (f)
        type R = {x :: Int, y :: Str}
        f :: R -> R
        |]
      , expectError
          "type-level record literal: `::` inside a tuple type annotation"
          [r|
        module main (c)
        c :: ({x :: Int, y :: Int}, Int)
        c = undefined
        |]
      -- Invariant 3 (R1): an @instance@ may only be declared on the root of
      -- an alias tree. A transparent @type@ alias inherits the root's
      -- instances; declaring an instance on the leaf would collide with the
      -- root's.
      , expectError
          "instance on transparent alias is rejected (simple)"
          [r|
        module main (f)
        type MyStr = Str
        class Eq a where
          (==) :: a -> a -> Bool
        instance Eq MyStr where
          (==) x y = True
        f :: MyStr -> MyStr -> Bool
        f x y = x == y
        |]
      , expectError
          "instance on transparent alias is rejected (parameterized)"
          [r|
        module main (f)
        type MyList a = List a
        class Functor f where
          map :: (a -> b) -> f a -> f b
        instance Functor MyList where
          map = undefined
        f :: MyList Int -> MyList Int
        f xs = map (\x -> x) xs
        |]
      , expectError
          "instance on a transparent alias of an optional type is rejected"
          [r|
        module main (f)
        type Opt a = ?a
        class Size a where
          size :: a -> Int
        instance Size (Opt a) where
          size x = 1
        f :: Int -> Int
        f x = x
        |]
      , expectError
          "instance on transparent alias is rejected (deep chain)"
          [r|
        module main (f)
        type A = Str
        type B = A
        class Foo a where
          tag :: a -> Str
        instance Foo B where
          tag x = x
        f :: B -> Str
        f x = tag x
        |]
      -- R1 positive: a @newtype@ legitimately owns its own instances.
      , expectPass
          "instance on newtype is accepted"
          [r|
        module main (f)
        newtype MyStr = Str
        class Tag a where
          tag :: a -> Str
        instance Tag MyStr where
          tag x = "tagged"
        f :: MyStr -> Str
        f x = tag x
        |]
      -- R2: cousin transparent aliases unify and share the root's
      -- instance. Both @B@ and @C@ reduce to @Str@, so the root's
      -- @Default@ method dispatches at every call site.
      , expectPass
          "cousin transparent aliases share the root's instance"
          [r|
        module main (f)
        type B = Str
        type C = Str
        class Default a where
          def :: a
        instance Default Str where
          def = ""
        f :: B -> C
        f x = def
        |]
      , assertGeneralType
          "cousin transparent aliases are interchangeable (subtype)"
          [r|
        module main (f)
        type Cousin1 = Str
        type Cousin2 = Str
        g :: Cousin1 -> Cousin1
        f :: Cousin2 -> Cousin1
        f x = g x
        |]
          (fun [var "Str", var "Str"])
      ]

-- | Tests for integer/real literal defaulting through type aliases.
-- A literal `65` checked against a type alias such as `type Char = U8`
-- must take on the aliased base integer type instead of synthesizing as
-- `Int` and failing the `Int <: U8` subtype step. Same for real
-- literals and F32/F64 aliases. Covers single-depth and multi-hop
-- alias chains, and every fixed-width primitive in the integer/real
-- families.
numericLiteralAliasTests :: TestTree
numericLiteralAliasTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Numeric literal defaulting through type aliases"
      [ -- single-depth integer aliases: every fixed-width family member
        assertGeneralType
          "int literal :: Char  (Char = U8)"
          [r|
        module main (x)
        type Char = U8
        x :: Char
        x = 65
        |]
          (var "U8")
      , assertGeneralType
          "int literal :: Word16  (Word16 = U16)"
          [r|
        module main (x)
        type Word16 = U16
        x :: Word16
        x = 65000
        |]
          (var "U16")
      , assertGeneralType
          "int literal :: Word32  (Word32 = U32)"
          [r|
        module main (x)
        type Word32 = U32
        x :: Word32
        x = 1
        |]
          (var "U32")
      , assertGeneralType
          "int literal :: Word64  (Word64 = U64)"
          [r|
        module main (x)
        type Word64 = U64
        x :: Word64
        x = 1
        |]
          (var "U64")
      , assertGeneralType
          "int literal :: Byte  (Byte = I8)"
          [r|
        module main (x)
        type Byte = I8
        x :: Byte
        x = 1
        |]
          (var "I8")
      , assertGeneralType
          "int literal :: Short  (Short = I16)"
          [r|
        module main (x)
        type Short = I16
        x :: Short
        x = 1
        |]
          (var "I16")
      , assertGeneralType
          "int literal :: I32  (I32 = I32)"
          [r|
        module main (x)
        type Int32 = I32
        x :: Int32
        x = 1
        |]
          (var "I32")
      , assertGeneralType
          "int literal :: I64  (I64 = I64)"
          [r|
        module main (x)
        type Int64 = I64
        x :: Int64
        x = 1
        |]
          (var "I64")
      , assertGeneralType
          "int literal :: Word  (Word = UInt)"
          [r|
        module main (x)
        type Word = UInt
        x :: Word
        x = 1
        |]
          (var "UInt")
        -- multi-hop alias chains: literal must default through the chain
      , assertGeneralType
          "int literal :: FooChar  (FooChar = Char = U8)"
          [r|
        module main (x)
        type Char = U8
        type FooChar = Char
        x :: FooChar
        x = 65
        |]
          (var "U8")
      , assertGeneralType
          "int literal :: A  (A = B = C = U32)"
          [r|
        module main (x)
        type A = B
        type B = C
        type C = U32
        x :: A
        x = 1
        |]
          (var "U32")
        -- list literal flowing each element through the alias:
        -- mirrors the encode/decode test in stdlib char-cpp that
        -- triggered the original bug report.
      , assertGeneralType
          "list of int literals :: [Char]  (Char = U8)"
          [r|
        module main (xs)
        type Char = U8
        xs :: [Char]
        xs = [65, 66, 67]
        |]
          (lst (var "U8"))
      , assertGeneralType
          "list of int literals :: [FooChar]  (chain to U8)"
          [r|
        module main (xs)
        type Char = U8
        type FooChar = Char
        xs :: [FooChar]
        xs = [65, 66, 67]
        |]
          (lst (var "U8"))
        -- single-depth real aliases
      , assertGeneralType
          "real literal :: Mass  (Mass = F32)"
          [r|
        module main (x)
        type Mass = F32
        x :: Mass
        x = 1.5
        |]
          (var "F32")
      , assertGeneralType
          "real literal :: Distance  (Distance = F64)"
          [r|
        module main (x)
        type Distance = F64
        x :: Distance
        x = 1.5
        |]
          (var "F64")
      , assertGeneralType
          "real literal :: Quantity  (Quantity = Real)"
          [r|
        module main (x)
        type Quantity = Real
        x :: Quantity
        x = 1.5
        |]
          (var "Real")
        -- multi-hop real alias chain
      , assertGeneralType
          "real literal :: A  (A = B = F32)"
          [r|
        module main (x)
        type A = B
        type B = F32
        x :: A
        x = 1.5
        |]
          (var "F32")
      , assertGeneralType
          "list of real literals :: [Mass]  (Mass = F32)"
          [r|
        module main (xs)
        type Mass = F32
        xs :: [Mass]
        xs = [1.0, 2.0, 3.0]
        |]
          (lst (var "F32"))
        -- negative: an integer literal still cannot inhabit a non-numeric
        -- alias. Guards against the fix accidentally letting any alias
        -- swallow integer literals.
      , expectError
          "int literal :: Flag (Flag = Bool) must fail"
          [r|
        module main (x)
        type Flag = Bool
        x :: Flag
        x = 65
        |]
        -- negative: a real literal still cannot inhabit an integer alias.
      , expectError
          "real literal :: Char (Char = U8) must fail"
          [r|
        module main (x)
        type Char = U8
        x :: Char
        x = 1.5
        |]
        -- Nat-parameterized list newtypes: `Vector 4 I32` has the wire
        -- form of `List I32` (`newtype Vector (n :: Nat) a = List a`). The list
        -- literal's elements must take on the reduced element type,
        -- not synthesize as `Int` and fail the subsequent subtype check.
        -- This is the user's original reproducer from the bug report.
      , assertGeneralType
          "list literal :: Vector 4 I32  (Vector (n::Nat) a = List a)"
          [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        x :: Vector 4 I32
        x = [1, 2, 3, 4]
        |]
          (arr "Vector" [NatLitU 4, var "I32"])
      , assertGeneralType
          "list literal :: Vector 3 I8  (other fixed-width int)"
          [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        x :: Vector 3 I8
        x = [1, 2, 3]
        |]
          (arr "Vector" [NatLitU 3, var "I8"])
      , assertGeneralType
          "list literal :: Vector 2 U16  (unsigned fixed-width int)"
          [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        x :: Vector 2 U16
        x = [1, 2]
        |]
          (arr "Vector" [NatLitU 2, var "U16"])
        -- Nested nat-parameterized newtype: Matrix m n a = [[a]] requires
        -- the element-type propagation to recurse through both layers.
      , assertGeneralType
          "nested list literal :: Matrix 2 2 I32"
          [r|
        module main (x)
        newtype Matrix (m :: Nat) (n :: Nat) a = List (List a)
        x :: Matrix 2 2 I32
        x = [[1, 2], [3, 4]]
        |]
          (arr "Matrix" [NatLitU 2, NatLitU 2, var "I32"])
        -- Real literals through nat-parameterized newtypes use the same
        -- dispatch — confirm the RealS path is unaffected.
      , assertGeneralType
          "real list literal :: Vector 2 F32"
          [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        x :: Vector 2 F32
        x = [1.5, 2.5]
        |]
          (arr "Vector" [NatLitU 2, var "F32"])
        -- Negative: Nat-dimension mismatch must still fail. The element
        -- type was successfully propagated (I32 accepted into the
        -- literals), but length 3 does not satisfy Nat dimension 4.
        -- This guards against the fix bypassing the nat-dim check.
      , expectError
          "list literal :: Vector 4 I32 with wrong length must fail"
          [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        x :: Vector 4 I32
        x = [1, 2, 3]
        |]
      ]

-- | Aliases of arrow, effect and optional types are expanded wherever they
-- occur. Other transparent aliases are expanded when a type leaves the module
-- that declared them.
aliasExpansionTests :: TestTree
aliasExpansionTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Alias expansion"
      [ testEqual "uncurryU flattens arrows grouped into the result"
          ([int, str], int)
          (uncurryU (FunU [int] (FunU [str] int)))
      , testEqual "uncurryU stops at a suspension"
          ([int], FunU [] int)
          (uncurryU (FunU [int] (FunU [] int)))
      , testEqual "uncurryU stops at an optional result"
          ([int], OptionalU (FunU [str] int))
          (uncurryU (FunU [int] (OptionalU (FunU [str] int))))
      , testEqual "uncurryU of a thunk has no arguments"
          ([], FunU [] int)
          (uncurryU (FunU [] int))
      , testEqual "uncurryU looks through quantifiers"
          ([var "a", var "a"], var "a")
          (uncurryU (forallu ["a"] (FunU [var "a"] (FunU [var "a"] (var "a")))))
      , testEqual "a function alias nested in its own argument is expanded"
          (FunU [FunU [str] int] int)
          (MTI.expandStructuralAliases scorerScope (arr "Scorer" [arr "Scorer" [str]]))
      , testEqual "a function alias used twice side by side is expanded twice"
          (tuple [FunU [str] int, FunU [int] int])
          (MTI.expandStructuralAliases scorerScope (tuple [arr "Scorer" [str], arr "Scorer" [int]]))
      , testEqual "a nested function alias leaves no alias behind"
          []
          (MTI.structuralAliasesIn scorerScope
            (MTI.expandStructuralAliases scorerScope (arr "Scorer" [arr "Scorer" [str]])))
      , testEqual "an alias of a record is not structural"
          (arr "Batch" [int])
          (MTI.expandStructuralAliases (aliasScope [("Batch", ["a"], lst (var "a"))]) (arr "Batch" [int]))
      , testEqual "a recursive function alias is left in place"
          [TV "F"]
          (let sc = aliasScope [("F", [], FunU [int] (var "F"))]
            in MTI.structuralAliasesIn sc (MTI.expandStructuralAliases sc (var "F")))
      , testEqual "a transparent alias nested in its own argument is expanded"
          (arr "Map" [str, arr "Map" [str, int]])
          (MTI.expandTransparentAliases
            (aliasScope [("Dict", ["a"], arr "Map" [str, var "a"])])
            (arr "Dict" [arr "Dict" [int]]))
      , assertGeneralType
          "a signature through a function alias nested in its own argument"
          [r|
          module main (apply2)
          type Scorer a = a -> Int
          apply2 :: Scorer (Scorer Str)
          apply2 f = f "x"
        |]
          (FunU [FunU [str] int] int)
      , expectPass
          "a typedef naming a function alias nested in its own argument"
          [r|
          module main (f)
          type Scorer a = a -> Int
          type Meta = Scorer (Scorer Str)
          f :: Meta
          f g = g "x"
        |]
      , expectError
          "a recursive function alias is rejected"
          [r|
          module main (f)
          type F = Int -> F
          f :: F
          f x = f
        |]
      ]
  where
    aliasScope defs = Map.fromList
      [ (TV n, [([Left (TV p, KindType) | p <- ps], body, ArgDocAlias defaultValue, False, TypedefAlias)])
      | (n, ps, body) <- defs ]
    scorerScope = aliasScope [("Scorer", ["a"], FunU [var "a"] int)]

-- | Tests for typeclass instance resolution across newtype boundaries.
-- @newtype Deque a = List a@ and @newtype Array a = List a@ are nominally
-- distinct from List and from each other, but share its wire format. Each
-- needs its own typeclass instances; an instance on List does not apply
-- to Deque, and a Deque value cannot be passed where an Array is expected.
aliasConstructorTests :: TestTree
aliasConstructorTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Typeclass resolution with newtype families"
      [
        -- === POSITIVE: newtypes used as type constructors with their own instances ===

        -- Two newtypes of the same wire-parent, each with its own Foldable
        -- instance. concat needs (Foldable f, Monoid a); the call site fixes
        -- f to a specific newtype which routes to the matching instance.
        assertGeneralType
          "fold over newtype: Deque and List each carry their own Foldable instance"
          [r|
        module main (f)
        class Semigroup a where
          append :: a -> a -> a
        class Semigroup a => Monoid a where
          mempty :: a
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        newtype Deque a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        instance Monoid (List a)
        instance Monoid (Deque a)
        instance Foldable List
        instance Foldable Deque
        concat :: (Foldable f, Monoid (f a)) => f (f a) -> f a
        concat = fold append mempty
        f :: [[Int]] -> [Int]
        f = concat
          |]
          (fun [lst (lst int), lst int])

      , -- Three newtypes sharing a wire-parent, each with its own Functor instance
        assertGeneralType
          "three newtypes (List, Deque, Array) each carry their own Functor instance"
          [r|
        module main (f)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        newtype Deque a = List a
        newtype Array a = List a
        instance Functor List
        instance Functor Deque
        instance Functor Array
        f :: [Int] -> [Int]
        f = fmap (\x -> x)
          |]
          (fun [lst int, lst int])

      , -- Chained newtype wire-parents: MyList -> Deque -> List
        assertGeneralType
          "chained newtypes: MyList wire-parent Deque, Deque wire-parent List"
          [r|
        module main (f)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        newtype Deque a = List a
        newtype MyList a = Deque a
        instance Foldable List
        instance Foldable Deque
        instance Foldable MyList
        f :: [Int] -> Int
        f = fold (\a x -> a) 0
          |]
          (fun [lst int, int])

      , -- Multi-parameter newtype: same arity as wire-parent
        assertGeneralType
          "two-parameter newtype with its own instance"
          [r|
        module main (f)
        type Map a b
        class MyClass f where
          myMethod :: f a b -> f a b
        newtype MyMap a b = Map a b
        instance MyClass Map
        instance MyClass MyMap
        f :: Map Int Str -> Map Int Str
        f = myMethod
          |]
          (fun [arr "Map" [int, str], arr "Map" [int, str]])

      , -- === ANNOTATION-DRIVEN NEWTYPE SELECTION ===
        -- A return-type annotation propagates the newtype through the
        -- expression: both append arguments must be Deque Int because
        -- the signature says so. Demonstrates that newtype identity
        -- flows through bidirectional checking without alias reduction.

        assertRawType
          "newtype annotation on right propagates to result"
          [r|
        module main (bar)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        bar :: Deque Int
        bar = append [1,2,3] ([4,5,6] :: Deque Int)
          |]
          (arr "Deque" [int])

      , assertRawType
          "newtype annotation on left propagates to result"
          [r|
        module main (baz)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        baz :: Deque Int
        baz = append ([4,5,6] :: Deque Int) [1,2,3]
          |]
          (arr "Deque" [int])

      , -- Annotation across a chain: MyList -> Deque -> List wire-parents
        assertRawType
          "newtype annotation propagates through chained wire-parents"
          [r|
        module main (bar)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        newtype MyList a = Deque a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        instance Semigroup (MyList a)
        bar :: MyList Int
        bar = append [1,2,3] ([4,5,6] :: MyList Int)
          |]
          (arr "MyList" [int])

      , -- concat used over a List value with full Semigroup/Monoid/Foldable
        -- instances on the List newtype. Demonstrates polymorphic typeclass
        -- dispatch when each newtype carries its own instance set.
        assertGeneralType
          "concat typechecks over List with full instance set"
          [r|
        module main (f)
        class Semigroup a where
          append :: a -> a -> a
        class Semigroup a => Monoid a where
          mempty :: a
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        newtype Deque a = List a
        newtype Array a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        instance Semigroup (Array a)
        instance Monoid (List a)
        instance Monoid (Deque a)
        instance Monoid (Array a)
        instance Foldable List
        instance Foldable Deque
        instance Foldable Array
        concat :: (Foldable f, Monoid (f a)) => f (f a) -> f a
        concat = fold append mempty
        f :: [[Int]] -> [Int]
        f = concat
          |]
          (fun [lst (lst int), lst int])

      , -- === NEGATIVE: NEWTYPE NOMINAL DISTINCTION ===

        -- Newtypes of the same wire-parent are nominally distinct.
        -- A binary operator with shared type variable cannot mix them.
        exprTestBad
          "newtypes Array and Deque are nominally distinct"
          [r|
        module main (bad)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        newtype Array a = List a
        instance Semigroup (Deque a)
        instance Semigroup (Array a)
        bad = append ([1,2,3] :: Array Int) ([4,5,6] :: Deque Int)
          |]

      , -- A function expecting one newtype must reject a value of a
        -- different newtype, even when both share a wire-parent.
        exprTestBad
          "function expecting Deque rejects Array argument"
          [r|
        module main (bad)
        newtype Deque a = List a
        newtype Array a = List a
        f :: Deque Int -> Int
        bad :: Int
        bad = f ([1,2,3] :: Array Int)
          |]

      , -- Non-alias types remain incompatible
        exprTestBad
          "non-alias types Int vs Str remain incompatible"
          [r|
        module main (f)
        f :: Int
        f = ("hello" :: Str)
          |]
      ]

aliasTransparencyTests :: TestTree
aliasTransparencyTests =
  localOption (mkTimeout 5000000) $
    testGroup
      "Transparent aliases are fully substitutable"
      [ testGroup "constructor patterns through an alias"
          [ expectPass "clauses on an alias of a data type"
              [r|
        module main (leaves)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        type IntTree = Tree Int
        leaves :: IntTree -> Int
        leaves | (Leaf _) = 1
               | (Node a b) = leaves a
              |]
          , expectPass "match on an alias of a data type"
              [r|
        module main (leaves)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        type IntTree = Tree Int
        leaves :: IntTree -> Int
        leaves t = match t
          | (Leaf _) = 1
          | (Node a b) = leaves a
              |]
          , expectPass "clauses on a parameterized alias"
              [r|
        module main (leaves)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        type T a = Tree a
        leaves :: T Int -> Int
        leaves | (Leaf _) = 1
               | (Node a b) = leaves a
              |]
          , expectPass "clauses on an alias chain"
              [r|
        module main (leaves)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        type IntTree = Tree Int
        type IT2 = IntTree
        leaves :: IT2 -> Int
        leaves | (Leaf _) = 1
               | (Node a b) = leaves a
              |]
          , expectPass "clauses on a recursive alias over a data type"
              [r|
        module main (top)
        data Rose a = R a [Rose a]
        type X = Rose ?X
        top :: X -> Int
        top | (R _ _) = 1
              |]
          , expectUserError "a constructor of another type is still rejected"
              [r|
        module main (f)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        data Opt a = None | Some a
        type IntTree = Tree Int
        f :: IntTree -> Int
        f | (Some _) = 1
          | None = 0
              |]
          ]
      , testGroup "constructor classes through an alias"
          [ sameAsExpanded "a partially used alias under Foldable"
              [r|
        module main (modelSize)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        count :: Int -> a -> Int
        length :: Foldable f => f a -> Int
        length = fold count 0
        type Model k v = [(k, v)]
        sizeOf :: Model k Int -> Int
        sizeOf xs = length xs
        modelSize :: [(Int, Int)] -> Int
        modelSize xs = sizeOf xs
              |]
              [r|
        module main (modelSize)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        count :: Int -> a -> Int
        length :: Foldable f => f a -> Int
        length = fold count 0
        sizeOf :: [(k, Int)] -> Int
        sizeOf xs = length xs
        modelSize :: [(Int, Int)] -> Int
        modelSize xs = sizeOf xs
              |]
          , expectPass "a partially used alias under Foldable, concrete"
              [r|
        module main (sizeOf)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        count :: Int -> a -> Int
        length :: Foldable f => f a -> Int
        length = fold count 0
        type Model k v = [(k, v)]
        sizeOf :: Model Int Int -> Int
        sizeOf xs = length xs
              |]
          , assertGeneralType "the element type of a fully applied alias is its body's"
              [r|
        module main (keys)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        instance Functor List
        fst :: (a, b) -> a
        type Pairs k = [(k, Int)]
        keysOf :: Pairs k -> [k]
        keysOf xs = fmap fst xs
        keys :: [(Str, Int)] -> [Str]
        keys xs = keysOf xs
              |]
              (fun [lst (tuple [str, int]), lst str])
          , expectUserError "a recursive alias with no Foldable instance is rejected"
              [r|
        module main (f)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        count :: Int -> a -> Int
        length :: Foldable f => f a -> Int
        length = fold count 0
        type LL a = (a, ?(LL a))
        f :: LL Int -> Int
        f xs = length xs
              |]
          ]
      , testGroup "inference keeps alias names"
          [ testCase "an unsigned term keeps the alias of the term it calls" $ do
              t <- rawGeneralType
                [r|
        module main (travel3)
        type WindSpeed = Int
        travel :: WindSpeed -> Int
        travel3 x = travel x
                |]
              case t of
                Right t' -> t' @?= fun [var "WindSpeed", int]
                Left e -> assertFailure (show e)
          ]
      , testGroup "recursive aliases terminate"
          [ expectPass "two recursive list aliases with equal unfoldings"
              [r|
        module main (f)
        type P = [P]
        type Q = [Q]
        g :: Q -> Int
        f :: P -> Int
        f x = g x
              |]
          , expectPass "two recursive tuple aliases with equal unfoldings"
              [r|
        module main (f)
        type L1 = (Int, ?L1)
        type L2 = (Int, ?(Int, ?L2))
        g :: L2 -> Int
        f :: L1 -> Int
        f x = g x
              |]
          , expectUserError "a recursive alias whose arguments change is rejected"
              [r|
        module main (f)
        type N a = (a, [N [a]])
        f :: N Int -> Int
              |]
          , expectPass "a recursive alias against its own two-step unfolding"
              [r|
        module main (f)
        type Pair a = (a, ?(Pair a))
        g :: (Int, ?(Int, ?(Pair Int))) -> Int
        f :: Pair Int -> Int
        f x = g x
              |]
          ]
      , testGroup "alias declarations and applications are checked"
          [ expectUserError "an alias applied to too few arguments in a signature"
              [r|
        module main (f)
        type Model k v = [(k, v)]
        f :: Model Int -> Int
              |]
          , expectUserError "an alias applied to too few arguments in a constraint"
              [r|
        module main (f)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        instance Foldable List
        type Model k v = [(k, v)]
        f :: Foldable (Model k) => Model k Int -> Int
              |]
          , expectUserError "an alias applied to too many arguments"
              [r|
        module main (f)
        data Tree a = Leaf a | Node (Tree a) (Tree a)
        type L = Tree
        f :: L Int -> Int
              |]
          , expectUserError "two declarations of one alias"
              [r|
        module main (f)
        type A = Int
        type A = Str
        f :: A -> A
              |]
          , expectUserError "an alias parameter its body does not use"
              [r|
        module main (f)
        type FixedPair (n :: Nat) a = (a, a)
        f :: FixedPair 2 Int -> Int
              |]
          ]
      , testGroup "alias parameter kinds come from the body"
          [ expectPass "a parameter in a Nat slot is a Nat"
              [r|
        module main (merges)
        newtype Grid (r :: Nat) (c :: Nat) a = List a
        type Image h w = Grid h w Int
        type Labels h w = Grid h w Real
        graph :: Grid h w Int -> Grid h w Real -> Int
        merges :: Image h w -> Labels h w -> Int
        merges img labels = graph img labels
              |]
          , sameAsExpanded "aliases with inferred Nat parameters equal their expansion"
              [r|
        module main (merges)
        newtype Grid (r :: Nat) (c :: Nat) a = List a
        type Image h w = Grid h w Int
        type Labels h w = Grid h w Real
        graph :: Image h w -> Labels h w -> Int
        merges :: Image h w -> Labels h w -> Int
        merges img labels = graph img labels
              |]
              [r|
        module main (merges)
        newtype Grid (r :: Nat) (c :: Nat) a = List a
        graph :: Grid h w Int -> Grid h w Real -> Int
        merges :: Grid h w Int -> Grid h w Real -> Int
        merges img labels = graph img labels
              |]
          , expectUserError "a parameter used at two kinds"
              [r|
        module main (f)
        newtype Grid (r :: Nat) (c :: Nat) a = List a
        type Bad h = (Grid h h Int, [h])
        f :: Bad 2 -> Int
              |]
          , expectPass "a parameter used only inside a row operation is used"
              [r|
        module main (f)
        newtype Tbl (n :: Nat) (r :: Rec) = Int
        type Bed3 r = {chrom = Str, start = Int} + r
        f :: Tbl n (Bed3 r) -> Int
              |]
          , expectPass "a declared Nat parameter in a Type slot keeps its kind"
              [r|
        module main (f)
        newtype T1 d a = [a]
        type V (n :: Nat) a = T1 n a
        f :: V 3 Int -> Int
              |]
          ]
      , testGroup "aliases with Nat parameters"
          [ assertGeneralType "a list literal through an alias of a phantom-Nat newtype"
              [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        type MyVec (n :: Nat) a = Vector n a
        x :: MyVec 3 Int
        x = [1, 2, 3]
              |]
              (arr "Vector" [NatLitU 3, int])
          , expectUserError "a dimension mismatch through the alias is caught"
              [r|
        module main (x)
        newtype Vector (n :: Nat) a = List a
        type MyVec (n :: Nat) a = Vector n a
        x :: MyVec 4 Int
        x = [1, 2, 3]
              |]
          , expectPass "an alias's arguments may differ where its expansions agree"
              [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        g :: Foo 2 2 -> Int
        f :: Foo 1 3 -> Int
        f x = g x
              |]
          , expectUserError "an alias's arguments that expand differently are rejected"
              [r|
        module main (f)
        newtype Vector (n :: Nat) a = List a
        type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        g :: Foo 2 3 -> Int
        f :: Foo 1 3 -> Int
        f x = g x
              |]
          ]
      ]
