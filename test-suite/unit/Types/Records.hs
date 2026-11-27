{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Records
  ( anonymousRecordOrderTests
  , recordLiteralOrderTests
  , recordIdentityTests
  , recursiveRecordTests
  ) where

import Morloc.Frontend.Namespace
import Text.RawString.QQ

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Types.Common

-- A record and an anonymous record type are one type only when their
-- fields come in the same order.
anonymousRecordOrderTests :: TestTree
anonymousRecordOrderTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "A record and an anonymous record type agree only in field order"
      [ expectPass
          "a record converts to an anonymous type with its own field order"
          [r|
module main (f, g, h)
record A where
  x :: Int
  y :: Int
f :: A -> {x = Int, y = Int}
f r = r
g :: {x = Int, y = Int} -> A
g r = r
h :: Int -> A
h n = {y = n, x = 0}
|]
      , expectError
          "a record does not convert to its fields in another order"
          [r|
module main (f)
record A where
  x :: Int
  y :: Int
f :: A -> {y = Int, x = Int}
f r = r
|]
      , expectError
          "an anonymous type does not convert to a record in another order"
          [r|
module main (g)
record A where
  x :: Int
  y :: Int
g :: {y = Int, x = Int} -> A
g r = r
|]
      , expectError
          "a reordered conversion nested in a list is rejected"
          [r|
module main (f)
record A where
  x :: Int
  y :: Int
f :: [A] -> [{y = Int, x = Int}]
f rs = rs
|]
      , expectError
          "a reordered conversion through an alias is rejected"
          [r|
module main (f)
record A where
  x :: Int
  y :: Int
type P = {y = Int, x = Int}
f :: A -> P
f r = r
|]
      , expectError
          "two records with swapped fields do not convert through an anonymous type"
          [r|
module main (f)
record A where
  x :: Int
  y :: Int
record B where
  y :: Int
  x :: Int
toAnon :: A -> {x = Int, y = Int}
toAnon r = r
f :: A -> B
f r = toAnon r
|]
      , expectPass
          "a conversion nested in a list keeps its field order"
          [r|
module main (f)
record A where
  x :: Int
  y :: Int
f :: [A] -> [{x = Int, y = Int}]
f rs = rs
|]
      , expectPass
          "a conversion through an alias keeps its field order"
          [r|
module main (f)
record A where
  x :: Int
  y :: Int
type P = {x = Int, y = Int}
f :: A -> P
f r = r
|]
      , expectError
          "an object does not convert to an anonymous record"
          [r|
module main (f)
object Person = Person {name :: Str, age :: Int}
f :: Person -> {name = Str, age = Int}
f p = p
|]
      ]

recordLiteralOrderTests :: TestTree
recordLiteralOrderTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Record literal field-order tests"
      [ assertGeneralType
          "permuted literal (mixed types) typechecks against declared record"
          [r|
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { age = 30, name = "Alice" }
          b
        |]
          (record' "Person" [(Key "name", str), (Key "age", int)])
      , assertGeneralType
          "permuted literal (same-typed fields) typechecks"
          [r|
          record Point = Point { x :: Int, y :: Int }
          p :: Point
          p = { y = 2, x = 1 }
          p
        |]
          (record' "Point" [(Key "x", int), (Key "y", int)])
      , assertGeneralType
          "fully-reversed literal typechecks"
          [r|
          record T = T { a :: Int, b :: Int, c :: Int }
          v :: T
          v = { c = 3, b = 2, a = 1 }
          v
        |]
          (record' "T" [(Key "a", int), (Key "b", int), (Key "c", int)])
      , exprTestBad
          "literal missing a declared field is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { name = "Alice" }
        |]
      , exprTestBad
          "literal with extra unknown field is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { name = "Alice", age = 30, weight = 65 }
        |]
      , exprTestBad
          "literal with completely disjoint keys is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { foo = "x", bar = 1 }
        |]
      , exprTestBad
          "literal with one matching and one wrong key is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { name = "Alice", years = 30 }
        |]
      ]

-- | Records are nominal: a record type is its name and its parameters, phantom
-- parameters included. An alias of a record is that record. The timeout
-- asserts termination of the negative cases.
recordIdentityTests :: TestTree
recordIdentityTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout; divergence appears as failure
    testGroup
      "Record identity and applied aliases"
      [ expectError
          "distinct records with the same fields are distinct types"
          [r|
          module main (f)
          record A where
            x :: Int
          record B where
            x :: Int
          f :: A -> B
          f a = a
        |]
      , expectError
          "a literal typed at one record does not inhabit a same-fielded record"
          [r|
          module main (f)
          record A where
            x :: Int
          record B where
            x :: Int
          mkA :: Int -> A
          mkA i = {x = i}
          f :: Int -> B
          f i = mkA i
        |]
      , expectError
          "an alias of one record does not name a same-fielded record"
          [r|
          module main (f)
          record A where
            x :: Int
          record B where
            x :: Int
          type C = A
          f :: C -> B
          f c = c
        |]
      , expectError
          "distinct parameterized records with the same fields are distinct types"
          [r|
          module main (f)
          record A a where
            x :: a
          record B a where
            x :: a
          f :: A Int -> B Int
          f a = a
        |]
      , expectError
          "a phantom parameter distinguishes two instantiations of a record"
          [r|
          module main (f)
          record Tag a where
            n :: Int
          f :: Tag Int -> Tag Str
          f t = t
        |]
      , expectError
          "a phantom parameter survives a polymorphic identity"
          [r|
          module main (h)
          record Tag a where
            n :: Int
          mk :: Int -> Tag Str
          mk i = {n = i}
          retag :: Tag a -> Tag a
          retag t = t
          h :: Int -> Tag Int
          h i = retag (mk i)
        |]
      , expectError
          "a phantom parameter is checked between guard branches"
          [r|
          module main (g)
          record Tag a where
            n :: Int
          c :: Tag Int
          c = {n = 1}
          g :: Bool -> Tag Str
          g b ? b = {n = 2}
            : c
        |]
      , expectPass
          "an alias of a record is that record"
          [r|
          module main (f)
          record A where
            x :: Int
          type C = A
          f :: C -> A
          f c = c
        |]
      , expectPass
          "a literal builds a record at a new phantom parameter"
          [r|
          module main (f)
          record Tag a where
            n :: Int
          f :: Tag Int -> Tag Str
          f t = {n = .n t}
        |]
      , expectPass
          "a polymorphic identity keeps a phantom parameter"
          [r|
          module main (g)
          record Tag a where
            n :: Int
          mk :: Int -> Tag Str
          mk i = {n = i}
          retag :: Tag a -> Tag a
          retag t = t
          g :: Int -> Tag Str
          g i = retag (mk i)
        |]
      , expectPass
          "an effect alias names the effect type it expands to"
          [r|
          module main (f)
          effect IO
          type Act a = <IO> a
          source Py from "m.py" ("rd")
          rd :: Int -> <IO> Int
          f :: Int -> Act Int
          f i = rd i
        |]
      , expectPass
          "an effect type is the effect alias that names it"
          [r|
          module main (f)
          effect IO
          type Act a = <IO> a
          source Py from "m.py" ("rd")
          rd :: Int -> Act Int
          f :: Int -> <IO> Int
          f i = rd i
        |]
      , expectError
          "a record named Rec is nominal like any other"
          [r|
          module main (f)
          record Rec where
            x :: Int
          record Other where
            x :: Int
          f :: Rec -> Other
          f a = a
        |]
      ]

-- Recursive record types. Self-recursive references through @[_]@ or @?_@
-- are legal (the data is heap-indirected and base-case-terminating).
-- Bare self-recursion (under tuple, function, named app, or directly) and
-- any mutual recursion remain illegal. Divergence is treated as failure
-- via the per-group timeout: if alias expansion or any later phase loops
-- on a recursive typedef, the offending test fires the timeout.
--
-- Syntax notes:
--   * Record types use @record T where { f :: t; ... }@; type-level
--     record literals @{f :: t}@ on the RHS of @type ... = ...@ are
--     rejected by the parser (CLAUDE.md: the @::@-form is reserved for
--     decls, not literals).
--   * Self-recursion on a non-record type alias (@type B3 = B3 -> Int@,
--     @type B2 = (B2, Int)@) parses and is exercised below for the
--     non-record bare-self cases.
recursiveRecordTests :: TestTree
recursiveRecordTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout; divergence appears as failure
    testGroup
      "Recursive record types"
      [
        -- POSITIVE: guarded self-recursion is accepted

        expectPass
          "self-recursion through list field"
          [r|
        module main (f)
        record T1 where
          a :: [T1]
        f :: T1 -> T1
        |]
      , expectPass
          "self-recursion through optional field"
          [r|
        module main (f)
        record T2 where
          a :: ?T2
        f :: T2 -> T2
        |]
      , expectPass
          "self-recursion through optional of list"
          [r|
        module main (f)
        record T3 where
          a :: ?[T3]
        f :: T3 -> T3
        |]
      , expectPass
          "self-recursion through list of optional"
          [r|
        module main (f)
        record T4 where
          a :: [?T4]
        f :: T4 -> T4
        |]
      , expectPass
          "self-recursion through nested lists"
          [r|
        module main (f)
        record T5 where
          a :: [[T5]]
        f :: T5 -> T5
        |]
      , expectPass
          "self-recursion through optional of nested lists"
          [r|
        module main (f)
        record T6 where
          a :: ?[[T6]]
        f :: T6 -> T6
        |]
      , expectPass
          "parameterized self-recursion"
          [r|
        module main (f)
        record T7 a where
          value :: a
          children :: [T7 a]
        f :: T7 Int -> T7 Int
        |]

        -- NEGATIVE: bare self-recursion is rejected
      , expectError
          "bare self under record field is rejected"
          [r|
        module main (f)
        record B1 where
          a :: B1
        f :: B1 -> B1
        |]
      , expectError
          "bare self under tuple alias is rejected"
          [r|
        module main (f)
        type B2 = (B2, Int)
        f :: B2 -> B2
        |]
      , expectError
          "bare self under function argument is rejected"
          [r|
        module main (f)
        type B3 = B3 -> Int
        f :: B3 -> Int
        |]
      , expectError
          "bare self under function return is rejected"
          [r|
        module main (f)
        type B4 = Int -> B4
        f :: B4
        |]
      , expectError
          "bare self under non-list non-optional app is rejected"
          [r|
        module main (f)
        type Pair a b = (a, b)
        record B5 where
          a :: Pair B5 Int
        f :: B5 -> B5
        |]
      , expectError
          "mixed good-and-bad fields: rejection wins"
          [r|
        module main (f)
        record B6 where
          ok :: [B6]
          bad :: B6
        f :: B6 -> B6
        |]

        -- NEGATIVE: a cycle of records is rejected whether or not its fields
        -- are guarded, since only a `data` on the cycle stops reduction
      , expectError
          "mutual recursion 2-cycle, unguarded, is rejected"
          [r|
        module main (f)
        record M1 where
          x :: M2
        record M2 where
          y :: M1
        f :: M1 -> M1
        |]
      , expectError
          "mutual recursion 2-cycle, both sides guarded, is rejected"
          [r|
        module main (f)
        record N1 where
          x :: [N2]
        record N2 where
          y :: [N1]
        f :: N1 -> N1
        |]
      , expectError
          "mutual recursion 3-cycle is rejected"
          [r|
        module main (f)
        record C1 where
          x :: C2
        record C2 where
          y :: C3
        record C3 where
          z :: C1
        f :: C1 -> C1
        |]
      ]
