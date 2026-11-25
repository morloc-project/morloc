{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Names
  ( namespaceErrorTests
  , typeclassTests
  , typeIdentityTests
  , sourceNameTests
  , tupleNameTests
  ) where

import Morloc.Frontend.Namespace
import Morloc.CodeGenerator.LanguageDescriptor (declaredNamePatterns, matchNamePattern)
import qualified Morloc.DataFiles as DF
import Text.RawString.QQ

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Types.Common

namespaceErrorTests :: TestTree
namespaceErrorTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Tests for namespace import error cases"
      [ -- chained namespace dots should be a parse error
        exprTestBad
          "chained namespace dots a.b.c"
          [r|
        module main (x)
        x :: Int
        x = a.b.c
          |]
      , -- keyword used as namespace should fail
        exprTestBad
          "keyword as namespace name (let)"
          [r|
        module foo (y)
        y :: Int
        y = 1
        module main (x)
        import .foo as let
        x :: Int
        x = let.y
          |]
      , -- keyword used as namespace should fail
        exprTestBad
          "keyword as namespace name (do)"
          [r|
        module foo (y)
        y :: Int
        y = 1
        module main (x)
        import .foo as do
        x :: Int
        x = do.y
          |]
      , -- undefined namespace prefix should fail
        exprTestBad
          "undefined namespace prefix"
          [r|
        module main (x)
        x :: Int
        x = noexist.foo 5
          |]
      , -- namespace-qualified name used with wrong arg type
        exprTestBad
          "namespace qualified name type mismatch"
          [r|
        module helpers (double)
        double :: Int -> Int
        double x = x
        module main (x)
        import .helpers as h
        x :: Int
        x = h.double "hello"
          |]
      , -- bare name should fail when imported with namespace
        exprTestBad
          "bare name fails with namespace import"
          [r|
        module helpers (double)
        double :: Int -> Int
        double x = x
        module main (x)
        import .helpers as h
        x :: Int
        x = double 5
          |]
      , -- typeclass methods are not exported as standalone symbols and
        -- cannot be selectively imported; this should raise an error
        -- (the test asserts only the presence of an error, not its text)
        exprTestBad
          "typeclass method cannot be selectively imported"
          [r|
        module eq (*)
        class Eq a where
          (==) :: a -> a -> Bool
        module main (test)
        import .eq ((==))
        test :: Bool
        test = True
          |]
      ]

typeclassTests :: TestTree
typeclassTests =
  localOption (mkTimeout 200000) $ -- 0.2 second timeout
    testGroup
      "Typeclass tests"
      [ -- === ANNOTATION PROPAGATION FIX (the core bug) ===
        -- Annotation leaked through copyState/reindexExprI to implementation
        -- indices, causing checkG to wrongly constrain non-matching instances.

        -- Instance declaration order must not matter
        assertGeneralType
          "annotation selects Str instance (Str declared first)"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Str
        x = mempty :: Str
          |]
          str

      , assertGeneralType
          "annotation selects Str instance (List declared first)"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid (List a) where
          mempty = []
        instance Monoid Str where
          mempty = ""
        x :: Str
        x = mempty :: Str
          |]
          str

      , -- Annotation selects the parametric instance
        assertGeneralType
          "annotation selects List instance"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: [Int]
        x = mempty :: [Int]
          |]
          (lst int)

      , -- Export signature alone (no inline annotation) resolves the instance
        assertGeneralType
          "export signature resolves instance without inline annotation"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Str
        x = mempty
          |]
          str

      , -- === MONOMORPHIC ANNOTATION FIX ===
        -- Annotation on standalone polymorphic functions (not typeclass methods)
        -- leaked via copyState to MonomorphicExpr implementation indices.

        assertGeneralType
          "annotation on standalone polymorphic function with args"
          [r|
        module main (foo)
        type Py => Int = "int"
        myId :: a -> a
        source Py ("myId")
        foo :: Int
        foo = (myId :: Int -> Int) 42
          |]
          int

      , assertGeneralType
          "annotation on standalone polymorphic nullary function"
          [r|
        module main (foo)
        type Py => Real = "float"
        myVal :: a
        source Py ("myVal")
        foo :: Real
        foo = myVal :: Real
          |]
          real

      , -- === SUPERCLASS CONSTRAINTS ===

        assertGeneralType
          "superclass method usable with subclass instance"
          [r|
        module main (x)
        type Py => Str = "str"
        class Semigroup a where
          append :: a -> a -> a
        class Semigroup a => Monoid a where
          mempty :: a
        instance Semigroup Str where
          source Py from "foo.py" ("appendStr" as append)
        instance Monoid Str where
          mempty = ""
        x :: Str
        x = append "" ""
          |]
          str

      , -- === NEGATIVE TESTS ===

        exprTestBad
          "no matching instance for annotated type"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        x :: Real
        x = mempty :: Real
          |]

      , exprTestBad
          "no instances defined"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        x :: Str
        x = mempty
          |]

      , exprTestBad
          "annotation contradicts export signature"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Int
        x = mempty :: Str
          |]

      , -- === COERCION-AWARE INSTANCE RESOLUTION ===

        -- CLS-17: no coercion makes a head match
        expectUserError
          "instance not resolved through optional coercion"
          [r|
        module main (x)
        class Default a where
          def :: a
        instance Default Int where
          def = 0
        x :: ?Int
        x = def
          |]

      , assertGeneralType
          "typeclass method in effectful do-block"
          [r|
        module main (x)
        effect IO
        class Default a where
          def :: a
        instance Default Int where
          def = 0
        f :: Int -> <IO> Int
        x = do
          f def
          |]
          (ioEff int)

      , -- === INTERACTION WITH OTHER FEATURES ===

        assertGeneralType
          "typeclass method in let binding"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Str
        x =
          let y = (mempty :: Str)
          in y
          |]
          str

      , assertGeneralType
          "typeclass method resolved by function argument context"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        f :: Str -> Str
        x :: Str
        x = f mempty
          |]
          str

      , assertGeneralType
          "class with multiple nullary methods"
          [r|
        module main (x)
        class Bounded a where
          minBound :: a
          maxBound :: a
        instance Bounded Int where
          minBound = 0
          maxBound = 100
        x :: Int
        x = minBound
          |]
          int

      , assertGeneralType
          "nested parametric instance"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid (List a) where
          mempty = []
        x :: [[Int]]
        x = mempty
          |]
          (lst (lst int))
      ]

-- A type name denotes the declaration in scope where it is written, wherever
-- the type is later used; a module may not write a name it has not declared
-- or imported.
typeIdentityTests :: TestTree
typeIdentityTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "Type identity across modules"
      [ expectPass
          "an imported signature's alias resolves without importing the alias"
          [r|
          module lib (mk, withSink)
            type Batch = [Int]
            mk :: Int -> Batch
            withSink :: Int -> (Batch -> Int) -> Int
          module main (total, viaSink)
            import .lib (mk, withSink)
            sum :: [Int] -> Int
            total :: Int -> Int
            total n = sum (mk n)
            viaSink :: Int -> Int
            viaSink n = withSink n sum
        |]
      , expectPass
          "an imported signature's record resolves without importing the record"
          [r|
          module lib (mk)
            record P where
              x :: Int
              y :: Int
            mk :: Int -> P
          module main (f)
            import .lib (mk)
            f :: Int -> Int
            f n = .y (mk n)
        |]
      , expectPass
          "a type reached through an imported alias resolves"
          [r|
          module lib (mk, Batch)
            record P where
              x :: Int
              y :: Int
            type Batch = [P]
            mk :: Int -> Batch
          module main (f)
            import .lib (mk, Batch)
            map :: (a -> b) -> [a] -> [b]
            f :: Int -> [Int]
            f n = map .y (mk n)
        |]
      , expectPass
          "a type imported under another name still resolves in signatures that use the original"
          [r|
          module lib (mk, Batch)
            type Batch = [Int]
            mk :: Int -> Batch
          module main (f)
            import .lib (mk, Batch as B)
            sum :: [Int] -> Int
            f :: Int -> Int
            f n = sum (mk n)
            g :: B -> Int
            g b = sum b
        |]
      , expectPass
          "a class method signature resolves its alias where the class is declared"
          [r|
          module lib (Sized, mk)
            type Batch = [Int]
            class Sized a where
              size :: a -> Batch
            mk :: Int -> Int
          module main (f)
            import .lib (Sized, mk)
            sum :: [Int] -> Int
            instance Sized Int where
              size x = [x]
            f :: Int -> Int
            f n = sum (size n)
        |]
      , expectPass
          "two modules may each declare a type of the same name"
          [r|
          module lib (mk, getx)
            record P where
              x :: Int
            mk :: Int -> P
            getx :: P -> Int
          module main (f, g)
            import .lib (mk, getx)
            record P where
              z :: Str
            mkq :: Str -> P
            f :: Int -> Int
            f n = getx (mk n)
            g :: Str -> Str
            g s = .z (mkq s)
        |]
      , expectError
          "a local alias of the same name does not stand for an imported one"
          [r|
          module lib (mk)
            type Batch = [Int]
            mk :: Int -> Batch
          module main (bad)
            import .lib (mk)
            type Batch = Str
            bad :: Int -> Str
            bad n = mk n
        |]
      , expectError
          "a local record of the same name does not stand for an imported one"
          [r|
          module lib (mk)
            record P where
              x :: Int
            mk :: Int -> P
          module main (bad)
            import .lib (mk)
            record P where
              z :: Str
            bad :: Int -> Str
            bad n = .z (mk n)
        |]
      , expectError
          "a local record of the same name is a distinct type"
          [r|
          module lib (mk)
            record P where
              x :: Int
            mk :: Int -> P
          module main (bad)
            import .lib (mk)
            record P where
              x :: Int
            bad :: Int -> P
            bad n = mk n
        |]
      , expectError
          "a type name that is neither declared nor imported is rejected"
          [r|
          module main (f)
            f :: Int -> Foo
        |]
      , expectError
          "a type name reachable only through an imported signature cannot be written"
          [r|
          module lib (mk)
            record P where
              x :: Int
            mk :: Int -> P
          module main (f)
            import .lib (mk)
            f :: P -> Int
            f p = .x p
        |]
      , expectError
          "importing a type and declaring one of the same name is rejected"
          [r|
          module lib (Batch)
            type Batch = [Int]
          module main (f)
            import .lib (Batch)
            type Batch = [Str]
            f :: Batch -> Int
        |]
      , expectError
          "importing a type the module does not export is rejected"
          [r|
          module lib (mk)
            type Batch = [Int]
            mk :: Int -> Batch
          module main (f)
            import .lib (mk, Batch)
            f :: Int -> Int
        |]
      , expectError
          "a type cycle among qualified names is rejected"
          [r|
          module lib (x)
            type A = Int
            type B = Int
            x :: A -> B
          module main (f)
            import .lib (x)
            type A = B
            type B = A
            f :: A -> Int
        |]
      , expectError
          "two different declarations of a built-in type name are rejected"
          [r|
          module lib (x)
            type Cell = Int
            x :: Cell -> Int
          module main (f)
            import .lib (x)
            type Cell = Str
            f :: Cell -> Int
        |]
      , expectError
          "conflicting language forms for one type are rejected"
          [r|
          module lib (Box)
            record Box where
              v :: Int
            record Py => Box = "dict"
          module main (f)
            import .lib (Box)
            record Py => Box = "MyBox"
            f :: Box -> Int
        |]
      ]

-- A sourced name is emitted verbatim into the pool, so a name that is not a
-- name in its language would be code.
sourceNameTests :: TestTree
sourceNameTests =
  testGroup "a sourced name is a name, not code"
    [ expectError "C++: an expression is not a source name"
          [r|
          module main (f)
          source Cpp from "m" ("plain(1)+ns::inc" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Rust: an expression is not a source name"
          [r|
          module main (f)
          source Rust from "m" ("plain(1)+inc" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "C: an expression is not a source name"
          [r|
          module main (f)
          source C from "m" ("plain(1)+inc" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Futhark: an expression is not a source name"
          [r|
          module main (f)
          source Futhark from "m" ("f (1)" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "C++: a single colon is not a scope operator"
          [r|
          module main (f)
          source Cpp from "m" ("ns:inc" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "C++: a trailing scope operator names nothing"
          [r|
          module main (f)
          source Cpp from "m" ("ns::" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Rust: a leading scope operator is not a path"
          [r|
          module main (f)
          source Rust from "m" ("::inc" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "C++: a qualified name is a source name"
          [r|
          module main (f)
          source Cpp from "m" ("ns::inc" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "C++: a leading scope operator is not a name"
          [r|
          module main (f)
          source Cpp from "m" ("::plain" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "Rust: a path is a source name"
          [r|
          module main (f)
          source Rust from "m" ("a::b" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Python: a manifold's name is not a source name"
          [r|
          module main (f)
          source Py from "m" ("__mm_3" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Python: a name the pool reserves is not a source name"
          [r|
          module main (f)
          source Py from "m" ("__morloc_encode" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "Python: a dotted builtin is a source name"
          [r|
          module main (f)
          source Py ("str.upper" as g)
          g :: Str -> Str
          f x = g x
        |]
    , expectError "Python: a keyword is not a source name"
          [r|
          module main (f)
          source Py ("yield" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Python: a keyword is not a segment of a source name"
          [r|
          module main (f)
          source Py from "m.py" ("ns.class" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "Python: a dotted name from a file is a source name"
          [r|
          module main (f)
          source Py from "m.py" ("helpers.basename" as g)
          g :: Str -> Str
          f x = g x
        |]
    , expectPass "R: a name with a leading dot is a source name"
          [r|
          module main (f)
          source R from "m.R" (".helper" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "R: a helper the pool binds for user files is not a source name"
          [r|
          module main (f)
          source R from "m.R" (".morloc.pooldir" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "R: an operator named by a string is an operator"
          [r|
          module main (f)
          source R from "m.R" ("+" as (<+>))
          (<+>) :: Int -> Int -> Int
          f x = x <+> x
        |]
    , expectError "Rust: a name the pool reserves is not a source name"
          [r|
          module main (f)
          source Rust from "m" ("__morloc_schema" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Rust: a path into the pool's names is not a source name"
          [r|
          module main (f)
          source Rust from "m" ("crate::__mm_3" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Rust: the pool's entry point is not a source name"
          [r|
          module main (f)
          source Rust from "m" ("main" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "Rust: a name containing a reserved one is a source name"
          [r|
          module main (f)
          source Rust from "m" ("main_loop" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectError "Rust: a manifold's name is not a source name"
          [r|
          module main (f)
          source Rust from "m" ("__mm_3" as g)
          g :: Int -> Int
          f x = g x
        |]
    , expectPass "Python: a name that only resembles a reserved one is a source name"
          [r|
          module main (f)
          source Py from "m" ("mm_3" as g)
          g :: Int -> Int
          f x = g x
        |]
    , testCase "every built-in language declares patterns that compile" $
        mapM_
          (\(n, f) -> case declaredNamePatterns (DF.embededFileText f) of
              Left e -> assertFailure (n <> ": " <> e)
              Right _ -> return ())
          DF.langRegistryFiles
    , testCase "a descriptor without a name pattern is refused" $
        assertBool "accepted" . isLeft $
          declaredNamePatterns "name: x\nldOperatorPattern: \"[+]+\"\n"
    , testCase "an empty pattern is refused" $
        assertBool "accepted" . isLeft $
          declaredNamePatterns "ldNamePattern: \"\"\nldOperatorPattern: \"[+]+\"\n"
    , testCase "a pattern that does not parse is refused" $
        assertBool "accepted" . isLeft $
          declaredNamePatterns "ldNamePattern: \"[a-\"\nldOperatorPattern: \"[+]+\"\n"
    , testCase "a group repeats" $
        matchNamePattern "a(::a)*" "a::a::a" @?= Right True
    , testCase "a group must match whole" $
        matchNamePattern "a(::a)*" "a::" @?= Right False
    ]

tupleNameTests :: TestTree
tupleNameTests =
  testGroup "a tuple is Tuple<k> applied to k types, and nothing else"
    [ expectPass "a class named like a tuple is a class"
          [r|
          module main (f)
          class TupleLike a where
            tl :: a -> a
          instance TupleLike Int where
            source Py from "x.py" ("tl")
          f :: TupleLike a => a -> a
          f x = tl x
        |]
    , expectPass "an index selector reads a tuple"
          [r|
          module main (g)
          source Py from "m.py" ("mkpair")
          mkpair :: Int -> (Int, Int)
          g x = .0 (mkpair x)
        |]
    , expectError "a type named like a tuple has no tuple fields"
          [r|
          module main (g)
          newtype TupleBox a = (a, a)
          source Py from "m.py" ("mkbox")
          mkbox :: Int -> TupleBox Int
          g x = .0 (mkbox x)
        |]
    , expectError "a tuple type cannot be declared"
          [r|
          module main (g)
          newtype Tuple3 a = [a]
          source Py from "m.py" ("mk")
          mk :: Int -> Tuple3 Int
          g x = mk x
        |]
    , expectError "a tuple type is applied to its arity"
          [r|
          module main (g)
          source Py from "m.py" ("mk")
          mk :: Int -> Tuple3 Int Int
          g x = .1 (mk x)
        |]
    ]
