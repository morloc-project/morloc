{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Typecheck
  ( unitTypeTests
  , unitValuecheckTests
  , solvedKindCheckTests
  , complexityRegressionTests
  , gradualDesugarTests
  , typedefKindVarTests
  , bidirectionalAppCheckTests
  , postArgPropagationTests
  ) where

import Morloc.Frontend.Namespace
import qualified Morloc.TypeEval as TE
import qualified Morloc.Typecheck.Internal as MTI
import Text.RawString.QQ

import qualified Data.Map as Map
import qualified Data.Text as MT
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Types.Common

unitTypeTests :: TestTree
unitTypeTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Typechecker unit tests"
      -- instantiating a data type's parameters
      [ -- programs that reach a positional pairing with lists of different
        -- lengths are refused as the user's error
        expectError "a definition with more parameters than its @with signature"
          "module main (streamNums)\nsumFile :: IFile [Int] -> <IO> Int\nproduce :: ([Int] -> <IO> ()) -> <IO> ()\n--' Stream numbers\n--' @with -m/--summary=sumFile\nstreamNums :: Int -> <IO> (Collect Int)\nstreamNums x y = @collect produce\n"
      , expectError "a specialized alias applied to too many arguments"
          "module main (f)\ntype Q Int b = (b, Str)\nf :: Q Int Int Int -> Real\n"
      , expectError "a specialized alias applied to none"
          "module main (f)\ntype Q Int b = (b, Str)\nf :: Q -> Real\n"
      , expectError "a function of a type variable applied to two arguments"
          "module main (g)\nf :: a -> Int\ng x = f x 2\n"
      , expectError "a parameter applied with two arities"
          "module main (g)\nadd :: Int -> Int -> Int\ng f = add (f 1) (f 1 2)\n"
      , expectPass "a record field applying a parameterized data type"
          "module main (f)\ndata Opt a = None | Some a\nrecord R where\n  x :: Opt Int\nf :: R -> Int\n"
      , expectError "a record field naming a parameterized data type bare"
          "module main (f)\ndata Opt a = None | Some a\nrecord R where\n  x :: Opt\nf :: R -> Int\n"
      , expectError "a record nested in itself at a larger argument"
          "module main (f)\nrecord Nest a where\n  val :: a\n  sub :: ?(Nest [a])\nf :: Nest Int -> Int\n"
      , expectError "a record nested in itself at a doubling argument"
          "module main (f)\nrecord Nest a where\n  val :: a\n  sub :: ?(Nest (a, a))\nf :: Nest Int -> Int\n"
      , expectError "a data type nested in itself at a larger argument"
          "module main (f)\ndata Perfect a = Leaf a | Node (Perfect (a, a))\nf :: Perfect Int -> Int\n"
      , expectError "a record and a data type nested in each other at a larger argument"
          "module main (f)\ndata D a = E | M (R [a])\nrecord R a where\n  v :: a\n  d :: D a\nf :: R Int -> Int\n"
      , expectPass "a record nested in itself at its parameters in another order"
          "module main (f)\nrecord T a b where\n  val :: a\n  sub :: ?(T b a)\nf :: T Int Str -> Int\n"
      , expectPass "a record nested in itself through an alias of its parameter"
          "module main (f)\ntype Id a = a\nrecord P a where\n  val :: a\n  sub :: ?(P (Id a))\nf :: P Int -> Int\n"
      , expectPass "a record nested in itself at an argument its nesting drops"
          "module main (f)\nrecord N a b where\n  val :: b\n  sub :: ?(N Int [a])\nf :: N Int [Int] -> Int\n"
      , expectPass "a record and a data type nested in each other at arguments that close"
          "module main (f)\ndata D b = E | M b (R Int)\nrecord R a where\n  v :: a\n  d :: D (a, Int)\nf :: R Int -> Int\n"
      , expectPass "a record holding itself in a list"
          "module main (f)\nrecord Rose a where\n  val :: a\n  kids :: [Rose a]\nf :: Rose Int -> Int\n"
      , testCase "parameters are set to their arguments at once" $
          map (instantiateParams [(TV "a", VarU (TV "b")), (TV "b", int)]) [VarU (TV "a"), VarU (TV "b")]
            @?= [VarU (TV "b"), int]
      , testCase "a binder shadowing a parameter is respected" $
          instantiateParams [(TV "a", int)] (ForallU (TV "a") (VarU (TV "a")))
            @?= ForallU (TV "a") (VarU (TV "a"))
      -- comments
      , assertGeneralType "block comments (1)" "{- -} 42" int
      , assertGeneralType "block comments (2)" " {--} 42{-   foo -} " int
      , assertGeneralType "line comments (3)" "-- foo\n 42" int
      , -- reals versus integers
        assertGeneralType "0 is an int" "0" int
      , assertGeneralType "42 is an int" "42" int
      , assertGeneralType "-42 is an int" "-42" int
      , assertGeneralType "big integers are OK" "123456789123456789123456789123456789123456789123456789" int
      , assertGeneralType
          "big negative integers are OK"
          "-123456789123456789123456789123456789123456789123456789"
          int
      , assertGeneralType "0.0 is a real" "0.0" real
      , assertGeneralType "4.2 is a real" "4.2" real
      , assertGeneralType "-4.2 is a real" "-4.2" real
      , assertGeneralType "4e1 is a real (scientific notation is real)" "4e1" real
      , assertGeneralType "-4e1 is a real" "-4e1" real
      , assertGeneralType "-4e-1 is a real" "-4e-1" real
      , assertGeneralType "4.2e3000 is a real" "4.2e3000" real
      , assertGeneralType "irregular scientific notation is OK" "123456789123456789123456789e-3000" real
      , assertGeneralType "reals may be big" "123456789123456789123456789.123456789123456789123456789" real
      , -- other primitives
        assertGeneralType "primitive boolean" "True" bool
      , assertGeneralType "primitive string" "\"this is a string literal\"" str
      , -- The `\0` escape is intentionally legal at the source-level so
        --   that NUL-tolerant pools (Python, C++) and the pure
        --   morloc nexus can carry interior NULs. The dispatch-layer
        --   guard rejects NUL-bearing values only at boundaries into
        --   NUL-intolerant languages (R, C). See features-strings.asc.
        assertGeneralType "string with embedded NUL escape" "\"abc\\0def\"" str
      , assertGeneralType "primitive integer annotation" "42 :: Int" int
      , assertGeneralType "primitive boolean annotation" "True :: Bool" bool
      , assertGeneralType "primitive double annotation" "4.2 :: Real" real
      , assertGeneralType
          "primitive string annotation"
          "\"this is a string literal\" :: Str"
          str
      , assertGeneralType "primitive declaration" "x = True\n4.2" real
      , -- containers
        -- - lists
        assertGeneralType "list of one primitive" "[1]" (lst int)
      , assertGeneralType "list of many primitives" "[1,2,3]" (lst int)
      , assertGeneralType "list of many containers" "[(True,1),(False,2)]" (lst (tuple [bool, int]))
      , -- - tuples
        assertGeneralType "tuple of primitives" "(1,2,True)" (tuple [int, int, bool])
      , assertGeneralType "tuple with containers" "(1,(2,True))" (tuple [int, tuple [int, bool]])
      , -- - records
        assertGeneralType
          "primitive record statement"
          [r|
        {x=42, y="yolo"}
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", str)])
      , assertGeneralType
          "primitive record signature"
          [r|
        record Foo = Foo {x :: Int, y :: Str}
        f :: Int -> Foo
        f 42
        |]
          (record' "Foo" [(Key "x", int), (Key "y", str)])
      , assertGeneralType
          "primitive record declaration"
          [r|
        foo = {x = 42, y = "yolo"}
        foo
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", str)])
      , assertGeneralType
          "nested records"
          [r|
        {x = 42, y = {bob = 24601, tod = "listen now closely and hear how I've planned it"}}
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", existP "e1" [] [(Key "bob", int), (Key "tod", str)])])
      , assertGeneralType
          "records with bound variables"
          [r|
        foo a = {x=a, y="yolo"}
        foo 42
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", str)])
      , testGroup
          "anonymous record row-form vs NamU-form reconciliation"
          [ expectPass
              "field accessor on an anonymous-record-typed argument"
              [r|
        f :: {x = Int} -> Int
        f r = .x r
        f
        |]
          , expectPass
              "record construction checked against an anonymous-record annotation"
              [r|
        f :: Int -> {x = Int}
        f n = {x = n}
        f
        |]
          , expectPass
              "constructed record passed to an anonymous-record-typed parameter"
              [r|
        g :: {x = Int} -> Int
        g r = 0
        g {x = 5}
        |]
          , expectPass
              "irrefutable record pattern binding an anonymous-record field"
              [r|
        f :: {x = Int} -> Int
        f r = let ({x = a}) = r in a
        f
        |]
          , expectPass
              "refutable `|`-pattern destructuring an anonymous record"
              [r|
        f :: {x = Int} -> Int
        f | {x = a} = a
        f
        |]
          ]
      , -- functions
        assertGeneralType
          "1-arg function declaration without signature"
          [r|
        f x = True
        f 42
        |]
          bool
      , assertGeneralType
          "2-arg function declaration without signature"
          [r|
        f x y = True
        f 42 True
        |]
          bool
      , assertGeneralType
          "1-arg function signature without declaration"
          [r|
        f :: Int -> Bool
        f 42
        |]
          bool
      , assertGeneralType
          "2-arg function signature without declaration"
          [r|
        f :: Int -> Bool -> Str
        f 42 True
        |]
          str
      , assertGeneralType
          "partial 1-2 function signature without declaration"
          [r|
        f :: Int -> Bool -> Str
        f 42
        |]
          (fun [bool, str])
      , assertGeneralType
          "identity function declaration and application"
          [r|
        f x = x
        f 42
        |]
          int
      , assertGeneralType
          "const declared function"
          [r|
        const x y = x
        const 42 True
        |]
          int
      , assertGeneralType
          "identity signature function"
          [r|
        id :: a -> a
        id 42
        |]
          int
      , assertGeneralType
          "const signature function"
          [r|
        const :: a -> b -> a
        const 42 True
        |]
          int
      , assertGeneralType
          "fst signature function"
          [r|
        fst :: (a,b) -> a
        fst (42,True)
        |]
          int
      , assertGeneralType
          "value to list function"
          [r|
        single :: a -> [a]
        single 42
        |]
          (lst int)
      , assertGeneralType
          "head function"
          [r|
        head :: [a] -> a
        head [1,2,3]
        |]
          int
      , assertGeneralType
          "make list function"
          [r|
        f :: a -> [a]
        f 1
        |]
          (lst int)
      , assertGeneralType
          "make list function"
          [r|
        single :: a -> [a]
        single 1
        |]
          (lst int)
      , assertGeneralType
          "existential function passing"
          [r|
        module main (g)
        g f = f True
        |]
          (fun [fun [bool, exist "e0"], exist "e0"])
      , assertGeneralType
          "app single function"
          [r|
        app :: (a -> b) -> a -> b
        f :: a -> [a]
        app f 42
        |]
          (lst int)
      , assertGeneralType
          "app head function"
          [r|
        app :: (a -> b) -> a -> b
        f :: [a] -> a
        app f [42]
        |]
          int
      , assertGeneralType
          "simple nested call"
          [r|
      f x = x
      g x = f x
      g 1
      |]
          int
      , assertGeneralType
          "nested calls"
          [r|
      f x y = (x, y)
      g x y = (x, f 1 y)
      g True "hi"
      |]
          (tuple [bool, tuple [int, str]])
      , assertGeneralType
          "zip pair"
          [r|
      pair x y = (x, y)
      zip :: (x -> y -> z) -> [x] -> [y] -> [z]
      zip pair [1,2] [True, False]
      |]
          (lst (tuple [int, bool]))
      , assertGeneralType
          "nested identity"
          [r|
      id :: a -> a
      id (id (id 1))
      |]
          int
      , assertGeneralType
          "head (head [[1]])"
          [r|
      head :: [a] -> a
      head (head [[42]])
      |]
          int
      , assertGeneralType
          "snd (snd (1,(1,True)))"
          [r|
      snd :: (a, b) -> b
      snd (snd (1, (1, True)))
      |]
          bool
      , assertGeneralType
          "f x y = [x, y]"
          [r|
        f x y = [x, y]
        f 1
        |]
          (fun [int, lst int])
      , assertGeneralType
          "map head function"
          [r|
        map :: (a -> b) -> [a] -> [b]
        head :: [a] -> a
        map head [[1],[1,2,3]]
        |]
          (lst int)
      , assertGeneralType
          "t a -> a"
          [r|
        type G a
        gify :: a -> G a
        out :: f a -> a
        out (gify 1)
        |]
          int
      , assertGeneralType
          "f a b -> b"
          [r|
        type G a b
        gify :: a -> b -> G a b
        snd :: f a b -> b
        snd (gify 1 True)
        |]
          bool
      , assertGeneralType
          "map id over number list"
          [r|
        map :: (a -> b) -> [a] -> [b]
        id :: a -> a
        map id [1,2,3]
        |]
          (lst int)
      , assertGeneralType
          "map fst over tuple list"
          [r|
        map :: (a -> b) -> [a] -> [b]
        fst :: (a,b) -> a
        map fst [(1,True),(2,False)]
        |]
          (lst int)
      , assertGeneralType
          "map fstG over (G a b) list"
          [r|
        type G a b
        gify :: a -> b -> G a b
        map :: (a -> b) -> [a] -> [b]
        fstF :: f a b -> a
        map fstF [gify 1 True, gify 2 False]
        |]
          (lst int)
      , assertGeneralType
          "fmap generic fst over functor"
          [r|
        type G a
        gify :: a -> G a
        fmap :: (a -> b) -> f a -> f b
        out :: f a -> a
        fmap out (gify [1])
        |]
          (arr "G" [int])
      , assertGeneralType
          "generic parameter reordering"
          [r|
        module m (biz)
        type R a b c
        type N a b
        type X
        type Y
        type Z
        type M a b c = R b a c
        foo :: M a b c -> N b c
        bar :: a -> b -> c -> R a b c
        da :: Int -> X
        db :: Int -> Y
        dc :: Int -> Z
        baz a b c = foo (bar a b c)
        -- biz :: N X Z
        biz = baz (da 1) (db 2) (dc 3)
        |]
          (arr "N" [var "X", var "Z"])
      , assertGeneralType
          "variable annotation"
          [r|
        module main (f)
        type Foo
        f :: Foo
        |]
          (var "Foo")
      , -- lambdas
        assertGeneralType
          "function with parameterized types"
          [r|
        module main (f)
        type A a
        type B
        type C
        f :: A B -> C
        |]
          (fun [arr "A" [var "B"], var "C"])
      , assertGeneralType "fully applied lambda (1)" "(\\x y -> x) 1 True" int
      , assertGeneralType "fully applied lambda (2)" "(\\x -> True) 42" bool
      , assertGeneralType "fully applied lambda (3)" "(\\x -> (\\y -> True) x) 42" bool
      , assertGeneralType "fully applied lambda (4)" "(\\x -> (\\y -> x) True) 42" int
      , assertGeneralType
          "unapplied lambda, polymorphic (1)"
          [r|\x -> True|]
          (fun [exist "e0", bool])
      , assertGeneralType
          "unapplied lambda, polymorphic (2)"
          "(\\x y -> x) :: a -> b -> a"
          (fun [exist "e0", exist "e1", exist "e0"])
      , assertGeneralType
          "annotated, fully applied lambda"
          "((\\x -> x) :: a -> a) True"
          bool
      , assertGeneralType
          "annotated, partially applied lambda"
          "((\\x y -> x) :: a -> b -> a) True"
          (fun [exist "e0", bool])
      , assertGeneralType
          "recursive functions are A-OK"
          "\\f -> f 5"
          (fun [fun [int, exist "e0"], exist "e0"])
      , -- applications
        assertGeneralType
          "primitive variable in application"
          [r|
        x = True
        (\y -> y) x
        |]
          bool
      , assertGeneralType
          "function variable in application"
          [r|
        f x y = x
        f 42 True
        |]
          int
      , assertGeneralType
          "partially applied function variable in application"
          [r|
        f x y = x
        x = f 42
        x
        |]
          (fun [exist "e0", int])
      , exprTestBad
          "applications with too many arguments fail"
          [r|
        f :: a -> a
        f True 12
        |]
      , exprTestBad
          "applications with mismatched types fail (1)"
          [r|
        abs :: Int -> Int
        abs True
        |]
      , exprTestBad
          "applications with mismatched types fail (2)"
          [r|
        f = 14
        g = \x h -> h x
        (g True) f
        |]
      , expectError
          "applications of non-functions should fail (1)"
          [r|
        f = 5
        g = \x -> f x
        g 12
        |]
      , expectError
          "applications of non-functions should fail (2)"
          [r|
        f = 5
        g = \h -> h 5
        g f
        |]
      , -- evaluation within containers
        expectError
          "arguments to a function are monotypes"
          [r|
        f :: a -> a
        g = \h -> (h 42, h True)
        g f
        |]
      , assertGeneralType
          "polymorphism under lambdas (203f8c) (1)"
          [r|
        f :: a -> a
        g = \h -> (h 42, h 1234)
        g f
        |]
          (tuple [int, int])
      , assertGeneralType
          "polymorphism under lambdas (203f8c) (2)"
          [r|
        f :: a -> a
        g = \h -> [h 42, h 1234]
        g f
        |]
          (lst int)
      , -- binding
        assertGeneralType
          "annotated variables without definition are legal"
          [r|
        module main (x)
        x :: Int
        |]
          int
      , assertGeneralType
          "unannotated variables with definition are legal"
          [r|
        x = 42
        x
        |]
          int
      , -- , exprTestBad
        --     "unannotated variables without definitions are illegal ('x')"
        --     "x"

        -- parameterized types
        assertGeneralType
          "parameterized type (n=1)"
          [r|
        module main (xs)
        type Foo a
        type A
        xs :: Foo A
        |]
          (arr "Foo" [var "A"])
      , assertGeneralType
          "parameterized type (n=2)"
          [r|
        module main (xs)
        type Foo a b
        type A
        type B
        xs :: Foo A B
        |]
          (arr "Foo" [var "A", var "B"])
      , assertGeneralType
          "nested parameterized type"
          [r|
        module main (xs)
        type Foo a b
        type Bar a
        type A
        type B
        xs :: Foo (Bar A) [B]
        |]
          (arr "Foo" [arr "Bar" [var "A"], arr "List" [var "B"]])
      , -- type signatures and higher-order functions
        assertGeneralType
          "type signature: identity function"
          [r|
        f :: a -> a
        f 42
        |]
          int
      , assertGeneralType
          "type signature: apply function with primitives"
          [r|
        apply :: (Int -> Bool) -> Int -> Bool
        f :: Int -> Bool
        apply f 42
        |]
          bool
      , assertGeneralType
          "type signature: generic apply function"
          [r|
        apply :: (a->b) -> a -> b
        f :: Int -> Bool
        apply f 42
        |]
          bool
      , assertGeneralType
          "type signature: map"
          [r|
        map :: (a->b) -> [a] -> [b]
        f :: Int -> Bool
        map f [5,2]
        |]
          (lst bool)
      , -- shadowing
        assertGeneralType
          "name shadowing in lambda expressions"
          [r|
        f x = (14, x)
        g x f = f x
        g True f
        |]
          (tuple [int, bool])
      , assertGeneralType
          "function passing without shadowing"
          [r|
        f x = (14, x)
        g foo = foo True
        g f
        |]
          (tuple [int, bool])
      , assertGeneralType
          "shadowed qualified type variables (7ffd52a)"
          [r|
        f :: a -> a
        g :: a -> Int
        g f
        |]
          int
      , assertGeneralType
          "non-shadowed qualified type variables (7ffd52a)"
          [r|
        f :: a -> a
        g :: b -> Int
        g f
        |]
          int
      , -- lists
        assertGeneralType "list of primitives" "[1,2,3]" (lst int)
      , assertGeneralType
          "list containing an applied variable"
          [r|
        f :: a -> a
        [53, f 34]
        |]
          (lst int)
      , -- NOTE: this test relies on internal renaming implementation
        assertGeneralType "empty list" "[]" (lst (exist "e0"))
      , assertGeneralType
          "list in function signature and application"
          [r|
        f :: [Int] -> Bool
        f [1]
        |]
          bool
      , -- , assertGeneralType
        --     "list in generic function signature and application"
        --     "f :: [a] -> Bool\nf [1]"
        --     [bool]
        -- , exprTestBad "failure on heterogenous list" "[1,2,True]"

        -- tuples
        assertGeneralType
          "tuple of primitives"
          [r|
        (4.2, True)
        |]
          (tuple [real, bool])
      , assertGeneralType
          "tuple containing an applied variable"
          [r|
        f :: a -> a
        (f 53, True)
        |]
          (tuple [int, bool])
      , assertGeneralType
          "check 2-tuples type signature"
          [r|
        module main (f)
        f :: (Int, Str)
        |]
          (tuple [int, str])
      , assertGeneralType "1-tuples are just for grouping" "module main (f)\nf :: (Int)" int
      , -- unit type
        assertGeneralType
          "unit as input"
          [r|
        module main (f)
        f :: () -> Bool
        |]
          (fun [VarU (TV "Unit"), bool])
      , assertGeneralType
          "unit as 2rd input"
          [r|
        module main (f)
        f :: Int -> () -> Bool
        |]
          (fun [int, VarU (TV "Unit"), bool])
      , assertGeneralType
          "unit as output"
          [r|
        module main (f)
        f :: Bool -> ()
        |]
          (fun [bool, VarU (TV "Unit")])
      , -- FIXME - I really don't like "Unit" being a normal var ...
        -- I am inclined to cast it as the unit type
        assertGeneralType "empty tuples are of unit type" "module main (f)\nf :: ()" (var "Unit")
      , -- extra space
        assertGeneralType "leading space" " 42" int
      , assertGeneralType "trailing space" "42 " int
      , -- adding signatures to declarations
        assertGeneralType
          "declaration with a signature (1)"
          [r|
        f :: a -> a
        f x = x
        f 42
        |]
          int
      , assertGeneralType
          "declaration with a signature (2)"
          [r|
        f :: Int -> Bool
        f x = True
        f 42
        |]
          bool
      , assertGeneralType
          "declaration with a signature (3)"
          [r|
        f :: Int -> Bool
        f x = True
        f
        |]
          (fun [int, bool])
      , expectError
          "primitive type mismatch should raise error"
          [r|
        module main (f)
        f :: Int -> Bool
        f x = 9999
        |]
      , expectError
          "catch infinite recursion of list"
          [r|
        module main (f)
        g :: [a] -> a
        f :: a -> a
        f x = g x
        |]
      , expectError
          "catch infinite recursion of tuple"
          [r|
        module main (f)
        g :: (a, b) -> a
        f :: a -> a
        f x = g x
        |]
      , expectError
          "check signatures under supposed identity"
          [r|
        module main (f)
        g :: (a -> b) -> a
        f :: a -> a
        f x = g x
        |]
      ,

        -- constraint syntax (implicit quantification wraps free vars in ForallU)
        assertGeneralType
          "constraint syntax (1)"
          "module main (f)\nf :: (Ord a) => a -> a -> a"
          (forallu ["a"] (fun [var "a", var "a", var "a"]))
      , assertGeneralType
          "constraint syntax (2)"
          "module main (f)\nf :: Ord a => a -> a -> a"
          (forallu ["a"] (fun [var "a", var "a", var "a"]))
      , assertGeneralType
          "constraint syntax (3)"
          "module main (f)\nf :: (Ord a, Eq b) => a -> b -> Bool"
          (forallu ["a", "b"] (fun [var "a", var "b", VarU (TV "Bool")]))
      , -- tests modules
        assertGeneralType
          "basic main module"
          [r|
          module main(x)
          x = [1,2,3]
        |]
          (lst int)
      , (flip $ assertGeneralType "import/export") (lst int) $
          [r|
          module foo (x)
            x = 42
          module bar (f)
            f :: a -> [a]
          module main (z)
            import .foo (x)
            import .bar (f)
            z = f x
        |]
      , (flip $ assertGeneralType "complex parse (1)") int $
          [r|
         module foo (x)
           add :: Int -> Int -> Int
           x = add a y where
             a = 1
             y = add b z where
               b = 42
           z = 19
      |]
      ]

unitValuecheckTests :: TestTree
unitValuecheckTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Valuechecker unit tests"
      [ valuecheckFail
          "unequal primitives fail"
          -- primitives
          [r|
         module foo (x)
           x = 1
           x = 2
      |]
      , valuecheckPass
          "equal primitives pass"
          [r|
         module foo (x)
           x = 1
           x = 1
      |]
      , -- containers
        valuecheckFail
          "lists with unequal values fail"
          [r|
         module foo (x)
           x = [1,3]
           x = [1,2]
      |]
      , valuecheckFail
          "lists of unequal length fail"
          [r|
         module foo (x)
           x = [1]
           x = [1,2]
      |]
      , valuecheckPass
          "identical lists pass"
          [r|
         module foo (x)
           x = [1,2]
           x = [1,2]
      |]
      , -- bound terms in simple expressions
        valuecheckFail
          "argument constraints"
          [r|
         module foo (f)
           f x y = x
           f a b = b
      |]
      , valuecheckFail
          "lambda var mismatches"
          [r|
         module foo (f)
           f x y = [x,y]
           f a b = [b,a]
      |]
      , valuecheckPass
          "identical lambda passes"
          [r|
         module foo (f)
           f x y = [x,y]
           f a b = [a,b]
      |]
      , -- the application may well equal the literal (REAL-5)
        valuecheckPass
          "a constant beside an application passes"
          [r|
         module foo (x)
           source Py ("sum")
           sum :: [Int] -> Int
           x = sum [1, 2]
           x = 3
      |]
      , valuecheckFail
          "unequal types"
          [r|
         module foo (f)
           source Py ("sum")
           sum :: [Int] -> Int
           f xs = [1, sum xs]
           f xs = [2, sum xs]
      |]
      , -- non-export terms must also be value-checked
        valuecheckFail
          "non-export term value contradiction"
          [r|
         module foo (a)
           a = y
           y = 1
           y = 2
      |]
      , valuecheckFail
          "where-clause value contradiction"
          [r|
         module foo (a)
           a = y where
             y = 1
             y = 2
      |]
      , valuecheckFail
          "where-clause contradiction inside lambda body"
          [r|
         module foo (f)
           f x = [x, y] where
             y = 1
             y = 2
      |]
      , valuecheckFail
          "where with equivalent values is illegal (duplicate binding)"
          [r|
         module foo (a)
           a = y where
             y = 1
             y = 1
      |]
      , -- self-referential / cyclic bindings without concrete sources
        valuecheckFail
          "direct self-referential binding"
          [r|
         module foo (omega)
           omega = omega
      |]
      , valuecheckFail
          "mutual self-reference cycle"
          [r|
         module foo (a)
           a = b
           b = a
      |]
      , valuecheckFail
          "three-cycle self-reference"
          [r|
         module foo (a)
           a = b
           b = c
           c = a
      |]
      , -- duplicate record fields (caught at parse time)
        valuecheckFail
          "duplicate record field literal"
          [r|
         module foo (r)
           r = {a = 1, b = 2, a = 3}
      |]
      , valuecheckPass
          "two parameters of a one-valued type agree"
          [r|
         module foo (f)
           f :: () -> () -> ()
           f x y = x
           f x y = y
      |]
      , valuecheckPass
          "equal values through differently named lets agree"
          [r|
         module foo (x)
           x :: Int
           x = let a = 1 in a
           x = let b = 1 in b
      |]
      , valuecheckPass
          "identical branches agree"
          [r|
         module foo (f)
           f :: Bool -> Int
           f x = ? x = 1 : 2
           f y = ? y = 1 : 2
      |]
      , valuecheckPass
          "lambdas of different arity are compared on their shared parameters"
          [r|
         module foo (f)
           f :: Int -> Int -> Int
           f = \x y -> x
           f a = \b -> a
      |]
      , valuecheckFail
          "lambdas of different arity that differ fail"
          [r|
         module foo (f)
           f :: Int -> Int -> Int
           f = \x y -> x
           f a = \b -> b
      |]
      , valuecheckFail
          "optional literals that differ fail"
          [r|
         module foo (x)
           x :: ?Int
           x = 1
           x = 2
      |]
      , valuecheckFail
          "null beside a value fails"
          [r|
         module foo (x)
           x :: ?Int
           x = null
           x = 1
      |]
      , valuecheckFail
          "a where-bound parameter is still a parameter"
          [r|
         module foo (f)
           f :: Int -> Int -> Int
           f a b = g
             where
               g = a
           f c d = h
             where
               h = d
      |]
      , valuecheckPass
          "arguments of a non-injective intrinsic may be reordered"
          [r|
         module foo (f)
           f :: Int -> Int -> Bool
           f x y = @eq x y
           f a b = @eq b a
      |]
      , valuecheckPass
          "the type of either parameter is the same"
          [r|
         module foo (f)
           f :: Int -> Int -> Str
           f x y = @typeof x
           f x y = @typeof y
      |]
      , valuecheckPass
          "an alias between two sources is not an ambiguity"
          [r|
         module foo (foo)
           source Py from "m.py" ("f" as foo, "g" as bar)
           foo :: Int -> Int
           bar :: Int -> Int
           foo = bar
      |]
      , valuecheckPass
          "parameters of a one-valued data type agree"
          [r|
         module foo (f)
           data U = U
           f :: U -> U
           f x = x
           f y = U
      |]
      , valuecheckPass
          "an integer literal at Real equals the real literal"
          [r|
         module foo (x)
           x :: Real
           x = 1
           x = 1.0
      |]
      , valuecheckFail
          "constructors with differing fields fail"
          [r|
         module foo (x)
           data Opt = No | Yes Int
           x :: Opt
           x = Yes 1
           x = Yes 2
      |]
      , valuecheckFail
          "a parameter inside a constructor differs from a constant"
          [r|
         module foo (f)
           data Box = Box Int
           f :: Int -> Box
           f n = Box n
           f m = Box 2
      |]
      , valuecheckFail
          "swapped parameters through a helper fail"
          [r|
         module foo (f)
           pick :: Int -> Int -> Int
           pick a b = h
             where
               h = a
           f :: Int -> Int -> Int
           f x y = pick x y
           f x y = pick y x
      |]
      , valuecheckPass
          "two spellings of one double agree"
          [r|
         module foo (x)
           x :: Real
           x = 0.1
           x = 0.10000000000000001
      |]
      , valuecheckFail
          "a parameter beside an optional constant fails"
          [r|
         module foo (f)
           f :: ?Int -> ?Int
           f x = x
           f y = 5
      |]
      , valuecheckPass
          "parameters of a one-valued constructor agree"
          [r|
         module foo (f)
           type Nothing' = ()
           data Box = Box Nothing'
           f :: Box -> Box
           f b = b
           f c = Box ()
      |]
      , valuecheckFail
          "a difference through an alias chain fails"
          [r|
         module foo (f)
           bar :: Int -> Int -> Int
           bar a b = a
           baz :: Int -> Int -> Int
           baz = bar
           f :: Int -> Int -> Int
           f x y = baz x y
           f x y = y
      |]
      , valuecheckPass
          "an alias and a lambda may implement one term"
          [r|
         module foo (foo)
           bar :: Int -> Int
           bar x = x
           foo :: Int -> Int
           foo = bar
           foo x = x
      |]
      , valuecheckPass
          "distinct record fields are legal"
          [r|
         module foo (r)
           r = {a = 1, b = 2, c = 3}
      |]
      , valuecheckFail
          "duplicate field in record type (where form)"
          [r|
         module foo (Foo)
           record Foo where
             a :: Int
             b :: Bool
             a :: Real
      |]
      , valuecheckFail
          "duplicate field in record type (legacy form)"
          [r|
         module foo (Foo)
           record Foo = MkFoo {a :: Int, b :: Bool, a :: Real}
      |]
      , valuecheckPass
          "distinct fields in record type (where form)"
          [r|
         module foo (Foo)
           record Foo where
             a :: Int
             b :: Bool
             c :: Real
      |]
      , -- let blocks intentionally allow shadowing (non-recursive sequential
        -- let): `let { x = 1 ; x = 2 } in x` is the layout-free spelling of
        -- `let x = 1 in let x = 2 in x` and returns 2. Only where-clauses
        -- (which are order-invariant) reject duplicates.
        valuecheckPass
          "nested let shadowing is legal"
          [r|
         module foo (a)
           a = let x = 1 in let x = 2 in x
      |]
      , valuecheckPass
          "multi-binding let with same name shadows (sequential)"
          [r|
         module foo (a)
           a = let { x = 1 ; x = 2 } in x
      |]
      , -- where-clause shadows function parameter
        expectError
          "where-clause binding shadows parameter"
          [r|
         module foo (g)
           g x = y where
             x = 100
             y = x + 1
      |]
      , -- the body's lambda binds a parameter too
        expectError
          "where-clause binding shadows a parameter of the body's lambda"
          [r|
         module foo (g)
           g :: Int -> Int -> Int
           g x = \y -> y where
             y = x
      |]
      , expectError
          "where-clause binding shadows a parameter of a definition that is a lambda"
          [r|
         module foo (g)
           g :: Int -> Int
           g = \x -> x where
             x = 1
      |]
      , -- a lambda nested in an argument binds locally, not a parameter
        valuecheckPass
          "where-clause binding named like a nested lambda's parameter"
          [r|
         module foo (g)
           g :: Int -> Int
           g x = h (\y -> y) where
             y = 1
             h f = f x
      |]
      , -- duplicate names in a single where-clause
        expectError
          "duplicate where-clause binding"
          [r|
         module foo (g)
           g n = y where
             y = n + 1
             y = n + 2
      |]
      , -- A polymorphic export that reaches a typeclass method backed by
        -- instances with differing literal bodies (e.g. Integral Int zero=0
        -- vs Integral Real zero=0.0). The instances are type-disjoint and
        -- never co-selected, so this must not be flagged as a value conflict.
        valuecheckPass
          "polymorphic export through multi-instance typeclass with literal bodies"
          [r|
         module foo (f)
           class Foo a where
             bar :: a
           instance Foo Int where
             bar = 5
           instance Foo Real where
             bar = 5.0
           f :: Foo a => a
           f = bar
      |]
      , -- Conflicting bodies for the SAME instance type must still fail:
        -- type-disjointness is the only thing that licenses skipping the
        -- comparison, and here both alternatives are Int.
        valuecheckFail
          "duplicate same-type instance bodies still flagged"
          [r|
         module foo (f)
           class Foo a where
             bar :: a
           instance Foo Int where
             bar = 5
           instance Foo Int where
             bar = 6
           f :: Int
           f = bar
      |]
      , valuecheckPass
          "identical intrinsic bodies across instances pass"
          [r|
         module foo (f)
           class Show a where
             show :: a -> Str
           instance Show Int where
             show x = @show x
           instance Show Int where
             show x = @show x
           f :: Int -> Str
           f = show
      |]
      , -- Two declarations that differ only in a parameter's name are the
        -- same value, including where the parameter is read by an intrinsic.
        valuecheckPass
          "alpha-equivalent intrinsic bodies pass"
          [r|
         module foo (f)
           g :: Int -> Str
           g x = @show x
           g y = @show y
           f :: Int -> Str
           f = g
      |]
      , valuecheckFail
          "distinct intrinsic bodies across instances fail"
          [r|
         module foo (f)
           class Show a where
             show :: a -> Str
           instance Show Int where
             show x = @show x
           instance Show Int where
             show x = @show 1
           f :: Int -> Str
           f = show
      |]
      ]

-- Problems that only surface once every kind variable is solved. Each was
-- previously invisible unless the user happened to annotate the result and
-- force the solver to look.
solvedKindCheckTests :: TestTree
solvedKindCheckTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "Checks that run after every kind variable is solved"
      [ exprTestBad
          "a computed dimension that comes out negative"
          [r|
module main (bad)
roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
src :: Vector 10 Real
bad :: Vector m Real
bad = roll 14 src
|]
      , expectPass
          "the same arithmetic when it stays positive"
          [r|
module main (ok)
roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real
src :: Vector 10 Real
ok = roll 3 src
|]
      , exprTestBad
          "a column union whose sides share a name, with no annotation"
          [r|
module main (oops)
type Holder (r :: Rec) a
a :: Holder {x=Int} Int
c :: Holder {x=Str} Int
join2 :: Holder r1 z -> Holder r2 z -> Holder (r1 + r2) z
oops = join2 a c
|]
      , expectPass
          "a column union of disjoint sides"
          [r|
module main (fine)
type Holder (r :: Rec) a
a :: Holder {x=Int} Int
d :: Holder {y=Str} Int
join2 :: Holder r1 z -> Holder r2 z -> Holder (r1 + r2) z
fine = join2 a d
|]
      ]

complexityRegressionTests :: TestTree
complexityRegressionTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Complexity regression tests"
      [ -- Deep function composition - tests batch subtype optimization
        assertGeneralType
          "deep identity composition"
          [r|
          id :: a -> a
          f = id (id (id (id (id (id (id (id (id (id 42)))))))))
          f
        |]
          int
      , assertGeneralType
          "deep function composition chain"
          [r|
          id :: a -> a
          (.) :: (b -> c) -> (a -> b) -> a -> c
          f = id . id . id . id . id . id . id . id . id . id
          f 42
        |]
          int
      , -- Eta expansion - tests avoiding re-inference
        assertGeneralType
          "nested lambdas returning functions"
          [r|
          add :: Int -> Int -> Int
          f = \x -> add x
          f
        |]
          (fun [int, int, int])
      , assertGeneralType
          "deeply nested partial application"
          [r|
          add3 :: Int -> Int -> Int -> Int
          f = \x -> \y -> add3 x y
          f
        |]
          (fun [int, int, int, int])
      , assertGeneralType
          "lambda with multi-arg function body"
          [r|
          add4 :: Int -> Int -> Int -> Int -> Int
          g = \a -> \b -> add4 a b
          g 1 2 3 4
        |]
          int
      , -- Multi-argument function subtyping
        assertGeneralType
          "many-argument function"
          [r|
          f :: Int -> Int -> Int -> Int -> Int -> Int -> Int
          f 1 2 3 4 5 6
        |]
          int
      , assertGeneralType
          "polymorphic many-argument function"
          [r|
          f :: a -> b -> c -> d -> e -> (a, b, c, d, e)
          f 1 True "x" 2.0 [1]
        |]
          (tuple [int, bool, str, real, lst int])
      , -- HOF shared type variable enforcement
        exprTestBad
          "fold with (==) should fail: shared var c gets Bool and Str"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          (==) :: c -> c -> Bool
          test = fold (==) True ["hello", "hello"]
          test
        |]
      , assertGeneralType
          "fold with (+) should succeed: shared var resolved consistently"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          (+) :: Int -> Int -> Int
          test = fold (+) 0 [1, 2, 3]
          test
        |]
          int
      , assertGeneralType
          "map with lambda using (==) should succeed: same type both args"
          [r|
          map :: (a -> b) -> [a] -> [b]
          (==) :: c -> c -> Bool
          test = map (\x -> x == x) ["hello"]
          test
        |]
          (lst bool)
      , -- zipSubtype path: type constructor with repeated variable
        exprTestBad
          "zipSubtype: Pair a a cannot unify with Pair Bool Str"
          [r|
          mkPair :: a -> Pair a a
          consume :: Pair Bool Str -> Int
          test = consume (mkPair True)
          test
        |]
      , assertGeneralType
          "zipSubtype: Pair a a consistent with Pair Int Int"
          [r|
          type Pair a b
          mkPair :: a -> Pair a a
          fst :: Pair a b -> a
          test = fst (mkPair 42)
          test
        |]
          int
      , -- Shared var in return: id passed where Bool -> Str expected
        exprTestBad
          "shared var via HOF: id cannot satisfy Bool -> Str"
          [r|
          apply :: (a -> b) -> a -> b
          id :: a -> a
          asStr :: Str -> Str
          test = asStr (apply id True)
          test
        |]
      , -- Triple-shared variable through HOF
        exprTestBad
          "triple-shared var forced to different types through fold"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          choose :: c -> c -> c
          test = fold choose "hello" [1, 2]
          test
        |]
      , -- Shared return var conflicts with arguments
        exprTestBad
          "shared var return type conflicts with argument through fold"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          weirdEq :: c -> c -> Str
          test = fold weirdEq "start" [1, 2]
          test
        |]
      , -- Two distinct shared vars both violated
        exprTestBad
          "two distinct shared vars both inconsistent through HOF"
          [r|
          hof :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
          f :: x -> y -> x -> y -> Bool
          test = hof f 1 "hi" True 42.0
          test
        |]
      , -- Nested HOF shared var conflict
        exprTestBad
          "nested HOF: shared var conflict through double application"
          [r|
          apply :: (a -> b) -> a -> b
          (==) :: c -> c -> Bool
          test = apply (apply (==) True) "hello"
          test
        |]
      , -- Regression: fold with (==) on consistent types should pass
        assertGeneralType
          "fold with (==) consistent types: all Bool"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          (==) :: c -> c -> Bool
          test = fold (==) True [True, False]
          test
        |]
          bool
      , -- Regression: multiple shared vars all consistent
        assertGeneralType
          "multiple shared vars consistent through HOF"
          [r|
          hof :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
          f :: x -> y -> x -> y -> Bool
          test = hof f 1 2 3 4
          test
        |]
          bool
      , -- A long composition chain with no tuples at all: the positional
        -- (tuple-slot) reconciliation runs on every bare-existential solve,
        -- so if it scans the whole solved-context map unconditionally the
        -- cost is quadratic in the chain length and this times out. With the
        -- receiver index gating the scan, a chain that has no destructuring
        -- receivers pays only a set membership test per solve and stays
        -- linear. This mirrors the standard-library `root` test, whose
        -- `test` value is one large composition of mostly non-tuple groups.
        localOption (mkTimeout 1000000) $
          assertGeneralType
            "long non-tuple composition chain stays linear"
            (deepIdComposition 200)
            int
      , -- The companion positive case: a long chain that *does* destructure
        -- tuples still typechecks promptly, confirming the gate admits the
        -- scan when destructuring receivers are present.
        localOption (mkTimeout 1000000) $
          assertGeneralType
            "long tuple-pattern composition chain typechecks"
            (tuplePatternComposition 40)
            (lst (tuple [str, int]))
      , -- 'applyLambdas' must SHARE, not clone, a multiply-used lambda
        -- parameter when it beta-reduces on the pool path. Beta-reducing
        -- @(\\a -> add a a) arg@ by substitution inserts a fresh copy of @arg@
        -- at each of @a@'s occurrences; through a chain of such reductions the
        -- argument doubles per step (2^depth copies) and exhausts memory. The
        -- fix binds the argument once as a @let@. This must run the full
        -- codegen path ('generatePools', where 'applyLambdas' lives), not just
        -- the frontend, so it uses 'runGen'. With the fix this finishes
        -- instantly; without it, 30 levels is ~10^9 nodes and overruns the
        -- timeout / memory. Mirrors the standard-library 'root' test, whose
        -- pool AST feeds such a chain into 'applyLambdas False'.
        localOption (mkTimeout 10000000) $
          testCase "deep multiply-used-parameter chain shares instead of cloning"
            (assertGenerates (dupParamChain 30))
      , -- 'applyLambdas' must MOVE, not clone, a SINGLY-used lambda parameter.
        -- Beta-reducing @(\\a -> inc a) arg@ needs no copy at all: @a@ occurs
        -- once, so the argument is placed at that site with its own indices
        -- (reindexing exists only to disambiguate several copies). Cloning it
        -- instead re-copies the whole rest-of-chain at every level of a deep
        -- reduction chain -- @arg@ at level k has size O(k), so the total is
        -- O(depth^2) copies and a deep chain exhausts memory. This is the shape
        -- of the standard-library 'root' test's pool AST (a long composition
        -- threading one value), which OOMed before the move fix. First-order
        -- @Int -> Int@ throughout so realize stays cheap and this isolates
        -- 'applyLambdas'. With the fix it is linear and finishes instantly.
        localOption (mkTimeout 20000000) $
          testCase "deep singly-used-parameter chain moves instead of cloning"
            (assertGenerates (singleUseChain 2000))
      , -- The lets each level floats out of its argument move up as one
        -- piece, so twice the depth costs about twice the time; a quadratic
        -- float spends far longer than this budget on it.
        localOption (mkTimeout 30000000) $
          testCase "a singly-used-parameter chain twice as deep stays linear"
            (assertGenerates (singleUseChain 4000))
      , -- The realizer's work is linear in a nest of higher-order calls: each
        -- argument is scored once, and a lambda parameter bound to a
        -- function value refers to that score.
        localOption (mkTimeout 5000000) $
          testCase "nested function arguments are scored once"
            (assertGenerates (nestedComposeChain 25))
      , -- A use whose type only its instance determines (CLS-20) is solved
        -- while the export is checked with its references left as
        -- signatures, so the export is specialized use by use rather than
        -- expanded in place.
        localOption (mkTimeout 10000000) $
          assertGeneralType
            "a use solved by its instance keeps the export shallow"
            (soleInstanceComparisons 150)
            (fun [lst (VarU (TV "Tag")), int])
      , -- Each method use checks every instance of its class and trims the
        -- context back after each; that trim must not cost the size of
        -- the context, which grows with the definition.
        localOption (mkTimeout 10000000) $
          assertGeneralType
            "method uses in one definition cost no more as the definition grows"
            (manyMethodUses 3000)
            (fun [int, int, lst bool])
      , -- Checking a definition against its signature takes a method use as
        -- an obligation, so it never copies the bodies of the use's
        -- candidate instances, which here use the class again.
        localOption (mkTimeout 10000000) $
          assertGeneralType
            "a signed definition's method uses do not copy instance bodies"
            (nestedMethodUses 1000)
            (fun [int, int, lst bool])
      ]

gradualDesugarTests :: TestTree
gradualDesugarTests =
  testGroup
    "gradual desugar of missing kind arguments"
    [ -- Baseline: pre-existing annotation forms still typecheck.
      expectPass "concrete-Nat: `Vector 3 Int`"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = [a]
      foo :: Vector 3 Int -> Int
      foo _ = 0
        |]
    , expectPass "polymorphic-Nat: `Vector n Int`"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = [a]
      foo :: Vector n Int -> Int
      foo _ = 0
        |]
    , expectPass "Nat arithmetic: `Vector (2+3) Int`"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = [a]
      foo :: Vector (2+3) Int -> Int
      foo _ = 0
        |]

    -- Single-Nat gradual form.
    , expectPass "gradual `Vector Int` typechecks"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = [a]
      foo :: Vector Int -> Int
      foo _ = 0
        |]
    , expectPass "gradual `Vector Int` accepts list literal of any length"
        [r|
      module main (xs)
      newtype Vector (n :: Nat) a = [a]
      xs :: Vector Int
      xs = [1, 2, 3, 4, 5]
        |]
    , expectPass "gradual `[Vector Int]` accepts het-length elements"
        [r|
      module main (xs)
      newtype Vector (n :: Nat) a = [a]
      xs :: [Vector Int]
      xs = [[1, 2], [3, 4, 5], [6]]
        |]

    -- Structural check on the desugar output. After alias reduction
    -- `Vector NatVoidU Int` may collapse to `[Int]`; both encode the
    -- same runtime shape.
    , testCase "gradual `Vector Int` desugars to `AppU Vector [NatVoidU, Int]`" $ do
        result <- runFrontRaw [r|
          module main (foo)
          newtype Vector (n :: Nat) a = [a]
          foo :: Vector Int -> Int
          foo _ = 0
            |]
        case result of
          Right [x] -> case stripForalls (gtypeof x) of
            FunU [AppU (VarU (TV "Vector")) [NatVoidU, VarU (TV "Int")]] _ -> return ()
            FunU [AppU (VarU (TV "List")) [VarU (TV "Int")]] _ -> return ()
            t -> assertFailure $
                   "Expected `Vector NatVoidU Int -> _` or `[Int] -> _`, got: " <> show t
          Right _ -> assertFailure "Expected exactly one export"
          Left e  -> assertFailure $ "Expected success, got: " <> show e

    -- Concrete <-> gradual subtype flows in both directions.
    , expectPass "concrete `Vector 3 Int` flows into `Vector Int` arg"
        [r|
      module main (result)
      newtype Vector (n :: Nat) a = [a]
      f :: Vector Int -> Int
      f _ = 0
      xs :: Vector 3 Int
      xs = [1, 2, 3]
      result :: Int
      result = f xs
        |]
    , expectPass "gradual `Vector Int` flows into `Vector 3 Int` arg"
        [r|
      module main (result)
      newtype Vector (n :: Nat) a = [a]
      f :: Vector 3 Int -> Int
      f _ = 0
      xs :: Vector Int
      xs = [1, 2, 3]
      result :: Int
      result = f xs
        |]

    -- Multi-Nat: Tensor3-style with 3 Nat params + 1 Type.
    , expectPass "gradual `Tensor3 Int` (3 Nat positions missing)"
        [r|
      module main (foo)
      newtype Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 Int -> Int
      foo _ = 0
        |]
    , expectPass "gradual `Tensor3 h Int` (2 Nat positions missing)"
        [r|
      module main (foo)
      newtype Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 h Int -> Int
      foo _ = 0
        |]
    , expectPass "gradual `Tensor3 h w Int` (1 Nat position missing)"
        [r|
      module main (foo)
      newtype Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 h w Int -> Int
      foo _ = 0
        |]
    , expectPass "fully-specified `Tensor3 2 3 5 Int` (no desugar)"
        [r|
      module main (foo)
      newtype Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 2 3 5 Int -> Int
      foo _ = 0
        |]

    -- Multi-kind: Table with Nat + Rec params.
    , expectPass "gradual `Table` (both Nat and Rec missing)"
        [r|
      module main (foo)
      newtype Table (n :: Nat) (r :: Rec) = Int
      foo :: Table -> Int
      foo _ = 0
        |]
    , expectPass "partial `Table 100` (only Rec missing)"
        [r|
      module main (foo)
      newtype Table (n :: Nat) (r :: Rec) = Int
      foo :: Table 100 -> Int
      foo _ = 0
        |]

    -- Type-position under-application is a normal arity error.
    , expectError "bare `Vector` (missing Type arg) is rejected"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = [a]
      foo :: Vector -> Int
      foo _ = 0
        |]

    -- Regression: polymorphic-Nat call-through via NatSolver's poly extractor.
    , expectPass "`g v = f v` with `Vector n Int` on both sides"
        [r|
      module main (g)
      newtype Vector (n :: Nat) a = [a]
      f :: Vector n Int -> Int
      f _ = 0
      g :: Vector n Int -> Int
      g v = f v
        |]

    -- Regression: gradual under-applied type constructor inside a
    -- type-alias body (e.g. `type Read a = (a, Vector U8)`) must also
    -- get its missing Nat position padded. Without walking TypE bodies
    -- through the fillMissingKindArgs pass, the alias would keep the
    -- unpadded `AppU Vector [U8]` and downstream newtype reduction
    -- couldn't match the 2-arg Vector definition.
    , expectPass "gradual `Vector U8` under a type alias reduces cleanly"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      type Read a = (a, Vector U8)
      foo :: Read Str -> Read Str
      foo x = x
        |]
    , expectPass "gradual `Vector U8` under a paramless type alias"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      type Bytes = Vector U8
      foo :: Bytes -> Bytes
      foo x = x
        |]

    , expectPass "gradual `Vector U8` in an instance head"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      class Fooable a where fooOf :: a -> Int
      instance Fooable (Vector U8) where
        fooOf _ = 0
      foo :: Vector U8 -> Int
      foo v = fooOf v
        |]

    , expectPass "gradual `Vector U8` in an expression annotation"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      foo :: Int
      foo = let v = ([1, 2, 3] :: Vector U8) in 0
        |]

    , exprTestBad
        "unknown kind name is rejected with a source-located error"
        [r|
      module main (foo)
      newtype Vector (n :: Nut) a = [a]
      foo :: Vector 3 Int -> Int
      foo _ = 0
        |]

    , expectPass "partial gradual with Rec literal preserves the schema"
        [r|
      module main (foo)
      newtype Table (n :: Nat) (r :: Rec)
      foo :: Table {x = Int} -> Int
      foo _ = 0
        |]
    ]
  where
    stripForalls (ForallU _ t) = stripForalls t
    stripForalls t = t

-- | Tests that typedef expansion correctly substitutes kind-specific
-- variables (NatVarU/StrVarU/RecVarU/ListVarU/SetVarU) into the body.
-- Regression coverage for the parsub fix: previously parsub treated
-- these as inert (parallel to its NatVarU base case), so typedefs of
-- the form @type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int@ would
-- leave the body's promoted Nat variables unbound after expansion. The
-- function-call substitution path (Internal.apply / gammaNatSubs) was
-- unaffected, which is why polymorphic function signatures worked but
-- typedef-style nat arithmetic did not.
typedefKindVarTests :: TestTree
typedefKindVarTests =
  testGroup
    "typedef expansion substitutes kind-specific variables"
    [ -- === Direct evaluateType tests (no morloc source, no frontend) ===
      testCase "evaluateType: Nat typedef param substitutes NatLitU into NatAddU" $
        -- Scope: type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 3, NatLitU 2]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatLitU 3) (NatLitU 2), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param leaves unrelated NatVarU intact" $
        -- Substitution replaces only the parameter: in the body
        -- Vector (k + n) Int, Foo 3 gives Vector (k + 3) Int. A declaration
        -- with the free k is rejected before expansion (ALIAS-11); the
        -- expander is tested on it directly.
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "k")) (NatVarU (TV "n")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 3]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatVarU (TV "k")) (NatLitU 3), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param substitutes through NatSubU" $
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatSubU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 5, NatLitU 3]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatSubU (NatLitU 5) (NatLitU 3), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param substitutes through NatMulU" $
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatMulU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 3, NatLitU 2]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatMulU (NatLitU 3) (NatLitU 2), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param substitutes through NatDivU" $
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatDivU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 6, NatLitU 2]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatDivU (NatLitU 6) (NatLitU 2), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Str typedef param substitutes through StrConcatU" $
        let bodyT = StrConcatU (StrVarU (TV "s")) (StrLitU "_suffix")
            params = [Left (TV "s", KindStr)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [StrLitU "prefix"]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (StrConcatU (StrLitU "prefix") (StrLitU "_suffix"))
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: substitution at one position does NOT leak to other vars" $
        -- type Foo (n :: Nat) = Vector (n + n) Int
        -- Foo 4 should give Vector (4 + 4) Int (both occurrences substituted)
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "n")) (NatVarU (TV "n")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 4]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatLitU 4) (NatLitU 4), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: mixed Type and Nat typedef params" $
        -- type Foo (n :: Nat) a = Vector (n + 1) a
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "n")) (NatLitU 1), VarU (TV "a")]
            params = [Left (TV "n", KindNat), Left (TV "a", KindType)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 4, VarU (TV "Int")]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatLitU 4) (NatLitU 1), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e

      -- === End-to-end morloc-source tests ===
      -- The user's original reproducer: typechecking succeeds when the
      -- typedef-parameter substitution correctly threads through Nat ops.
    , assertGeneralType
        "Foo n m = Vector (n + m) Int; Foo 3 2 = [1..5]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
      x :: Foo 3 2
      x = [1, 2, 3, 4, 5]
        |]
        (arr "Vector" [NatLitU 5, var "Int"])
    , assertGeneralType
        "Foo n m = Vector (n - m) Int; Foo 5 3 = [1, 2]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n - m) Int
      x :: Foo 5 3
      x = [1, 2]
        |]
        (arr "Vector" [NatLitU 2, var "Int"])
    , assertGeneralType
        "Foo n m = Vector (n * m) Int; Foo 3 2 = [1..6]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n * m) Int
      x :: Foo 3 2
      x = [1, 2, 3, 4, 5, 6]
        |]
        (arr "Vector" [NatLitU 6, var "Int"])
    , assertGeneralType
        "Foo n m = Vector (n / m) Int; Foo 6 2 = [1..3]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n / m) Int
      x :: Foo 6 2
      x = [1, 2, 3]
        |]
        (arr "Vector" [NatLitU 3, var "Int"])
    , expectError
        "Foo n m = Vector (n + m) Int; Foo 3 2 with wrong-length value fails"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
      x :: Foo 3 2
      x = [1, 2, 3, 4, 5, 6]
        |]
    , assertGeneralType
        "Mixed Type/Nat params: Foo (n :: Nat) a = Vector (n + 1) a; Foo 4 Int = [1..5]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) a = Vector (n + 1) a
      x :: Foo 4 Int
      x = [1, 2, 3, 4, 5]
        |]
        (arr "Vector" [NatLitU 5, var "Int"])
    , assertGeneralType
        "Repeated Nat param: Foo (n :: Nat) = Vector (n + n) Int; Foo 3 = [1..6]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) = Vector (n + n) Int
      x :: Foo 3
      x = [1, 2, 3, 4, 5, 6]
        |]
        (arr "Vector" [NatLitU 6, var "Int"])
    , assertGeneralType
        "Single Nat param: Foo (n :: Nat) = Vector (n + 1) Int; Foo 4 = [1..5]"
        [r|
      module main (x)
      newtype Vector (n :: Nat) a = List a
      type Foo (n :: Nat) = Vector (n + 1) Int
      x :: Foo 4
      x = [1, 2, 3, 4, 5]
        |]
        (arr "Vector" [NatLitU 5, var "Int"])

    -- Bare VarU classifies as a Nat expression (see typeUToNatExpr's
    -- VarU case in Typecheck/Internal.hs), so a synthetic pair that
    -- classifies as NONE of Nat/Str/Rec/List/Set must use a shape no
    -- classifier accepts: FunU here.
    , exprTestBad "deferred row equations are decided together, not one by one" $
        "module main (top)\n\
        \type Table (n :: Nat) (r :: Rec)\n\
        \two :: Table n (u + w) -> Table n (u + v) -> Table n w -> Table n v -> Int\n\
        \top :: Table m {a = Int} -> Table m {c = Int} -> Table m {} -> Int\n\
        \top x y z = two x y z z\n"
    , testCase "recheckDeferred rejects an unclassifiable pair" $
        let bad = FunU [] (VarU (TV "a"))
            g0 = (listToGamma []) { gammaDeferred = [(bad, bad)] }
        in case MTI.recheckDeferred g0 of
             Left msg ->
               assertBool ("expected 'unclassifiable' in error, got: " ++ show msg)
                 ("unclassifiable" `MT.isInfixOf` MT.pack (show msg))
             Right _ ->
               assertFailure "expected Left, got Right (classifier let unknown-kind pair through)"
    ]

-- | Bidirectional checking for function application against structurally
-- recursive types. The new `checkE (AppS f xs) t` rule propagates the
-- expected type into the function's return position BEFORE checking the
-- args. Without it, a literal at a recursive alias gets synthesised to
-- a finite tuple shape and subtype fails when it tries to roll the
-- finite type back into the recursive alias.
--
-- The cases here are typecheck-only (no codegen, no pool build) so the
-- whole group runs in milliseconds. The deep tests below pin the
-- linear-complexity claim from the plan.
bidirectionalAppCheckTests :: TestTree
bidirectionalAppCheckTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Bidirectional App-check against recursive types"
      [ -- Bug repro: monomorphic id-shaped call at a recursive alias.
        -- Without the new rule, the literal synthesises to
        -- `(Str, (Str, ?ex))` and subtype against `Pair Str` fails at
        -- the inner `(Str, ?ex) <: ?(Pair Str)`.
        expectPass
          "monomorphic id at recursive Pair alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        idPair :: Pair Str -> Pair Str
        foo :: Pair Str
        foo = idPair ("a", ("b", Null))
        |]
      , -- Polymorphic id: forces stripForallU + propagation through the
        -- shared `a` existential.
        expectPass
          "polymorphic id at recursive Pair alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        id :: a -> a
        foo :: Pair Str
        foo = id ("a", ("b", Null))
        |]
      , -- Multi-arg application: exercises the
        -- `length xs == length paramTypes` arm beyond unary.
        expectPass
          "multi-arg polymorphic function at recursive alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        pick :: Bool -> a -> a -> a
        foo :: Pair Str
        foo = pick True ("a", Null) ("b", Null)
        |]
      , -- Nested AppS: argument is itself a function application, so
        -- the new rule must recurse through checkG -> checkE -> AppS.
        expectPass
          "nested polymorphic id at recursive alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        id :: a -> a
        foo :: Pair Str
        foo = id (id ("a", ("b", Null)))
        |]
      , -- Partial application: falls back cleanly via checkEFallback.
        -- The new rule's `_ -> checkEFallback` arm is exercised.
        expectPass
          "partial application falls through cleanly"
          [r|
        module main (bar)
        add :: Int -> Int -> Int
        bar :: Int -> Int
        bar = add 1
        |]
      , -- Genuine type error must still be rejected: the propagation
        -- step solves `a := Int`, then the argument check `"x" :: Int`
        -- fails inside checkG (same diagnostic surface as today).
        exprTestBad
          "wrong-typed argument still rejected"
          [r|
        module main (foo)
        id :: a -> a
        foo :: Int
        foo = id "hello"
        |]
      , -- Function-as-argument: confirms the rule doesn't over-eagerly
        -- fire on FunU-returning calls where the existing logic was
        -- already correct.
        expectPass
          "function-as-argument keeps working"
          [r|
        module main (foo)
        apply :: (a -> b) -> a -> b
        inc :: Int -> Int
        foo :: Int
        foo = apply inc 3
        |]
      , -- Linear-complexity probe: depth-20 recursive literal under a
        -- monomorphic id at a recursive alias. With the linear
        -- algorithm this is microseconds; with an O(N^2) regression
        -- depth-20 is 400x slower, still well inside 2 s but visible
        -- if cubic or exponential.
        expectPass
          "deep-20 recursive literal, monomorphic id"
          (deepLiteralProgram False 20)
      , -- Depth-20 literal under polymorphic id: forces stripForallU +
        -- propagation at the recursive type.
        expectPass
          "deep-20 recursive literal, polymorphic id"
          (deepLiteralProgram True 20)
      , -- Depth-20 chain of id applications around a small literal:
        -- exercises the application-nesting axis orthogonally to
        -- literal depth.
        expectPass
          "deep-20 chained id applications"
          (deepIdChainProgram 20)
      , -- Combined axes: depth-20 literal nested inside a depth-20 id
        -- chain. If complexity is O(M*N) this is 400 work units, still
        -- linear-ish in 2 s but a regression to O((M*N)^k) for k>=2
        -- would blow up here.
        expectPass
          "deep-20 literal under deep-20 id chain"
          (deepCombinedProgram 20)
      , -- Negative deep case: depth-20 literal at a wrong type. The
        -- failure path must also be linear (no pathological cost when
        -- the rule's propagation succeeds but the argument check
        -- rejects).
        exprTestBad
          "deep-20 recursive literal at wrong type still rejected"
          (deepWrongTypeProgram 20)
      , -- Regression: a polymorphic indexer (`at :: Int -> [a] -> a`)
        -- whose return is the bare element type, applied in a
        -- position that expects `?a`. The App-Check shortcut was
        -- pre-pinning the function's return existential to the FULL
        -- `?a`, which propagated through the polymorphic arg and
        -- forced `xs :: [?a]` against the actual `xs :: [a]`,
        -- producing an occurs check `a <: ?a`. The fix bails to
        -- checkEFallback when the expected type is OptionalU so the
        -- coercion fires at the application boundary.
        expectPass
          "indexer at ?a return position coerces at boundary"
          [r|
        module main (foo)
        listAt :: Int -> [a] -> a
        foo :: [a] -> ?a
        foo xs = listAt 0 xs
        |]
      , -- The full listToMaybe pattern from the maybe stdlib:
        -- explicit null-guard on one branch, polymorphic indexer on
        -- the other. The else-branch must coerce `a` to `?a` cleanly.
        expectPass
          "guarded safeHead pattern with polymorphic indexer"
          [r|
        module main (foo)
        listAt :: Int -> [a] -> a
        isEmpty :: [a] -> Bool
        foo :: [a] -> ?a
        foo xs
          ? isEmpty xs = Null
          : listAt 0 xs
        |]
      , -- Negative: applying a function whose return is NOT the
        -- expected `?a`'s inner type must still be rejected. `f ::
        -- a -> a` applied to `xs : [a]` returns `[a]`, which cannot
        -- coerce to `?a`. Confirms the OptionalU bypass doesn't
        -- accidentally permit wrong-shape coercions.
        exprTestBad
          "wrong-shape return at ?a position still rejected"
          [r|
        module main (foo)
        f :: a -> a
        foo :: [a] -> ?a
        foo xs = f xs
        |]
      , -- Widening under an effect wrapper: a do-block always synthesizes
        -- <E> T, so a bare-value final checked against a declared <E> ?T
        -- return must coerce the inner T to ?T. tryCoerce now traverses the
        -- matching EffectU wrapper; without that, <Err> Int <: <Err> ?Int
        -- fell through.
        expectPass
          "do-block bare final coerces to optional under effect"
          [r|
        module main (foo)
        escapable effect Err
        source Py ("act")
        act :: Int -> <Err> Int
        foo :: Int -> <Err> ?Int
        foo x = do
          y <- act x
          y
        |]
      , -- A `?:` whose Null branch is a constant suspension of an optional
        -- and whose other branch is an effectful do-block yielding a bare
        -- value. checkE pushes the <Err> ?Int expectation into both
        -- branches; the do-block branch widens its inner type.
        expectPass
          "null-guarded effectful do-block branch coerces to optional"
          [r|
        module main (foo)
        escapable effect Err
        source Py ("act", "isZero")
        act :: Int -> <Err> Int
        isZero :: Int -> Bool
        foo :: Int -> <Err> ?Int
        foo x
          ? isZero x = do Null
          : do y <- act x
               y
        |]
      , -- Guard against over-firing: the effect traversal must still reject
        -- a wrong inner type. <Err> Int cannot widen to <Err> ?Str.
        exprTestBad
          "effectful do-block final at wrong optional inner type rejected"
          [r|
        module main (foo)
        escapable effect Err
        source Py ("act")
        act :: Int -> <Err> Int
        foo :: Int -> <Err> ?Str
        foo x = do
          y <- act x
          y
        |]
      ]

-- | Coverage for the class of nat/str/list-label propagation
-- patterns: information that flows from a literal argument OUT
-- through one or more carrier functions to satisfy an outer
-- expected type. The "labeled dims propagate through 3-function
-- chain" case in natKindPromotionTests was the canonical example;
-- these tests broaden the coverage along several axes:
--
--   * different chain depths (1, 2, 4) on the same nat-label axis
--   * monomorphic vs polymorphic carrier functions
--   * multi-label functions where multiple pinnings must propagate
--   * multi-arg functions where one arg pins a shared variable
--   * cross-existential pinning via a tuple's first element
--   * negative cases: a downstream constraint must reject a wrong
--     literal seen at any depth in the chain
--
-- Most cases here are handled by the OLD synth+subtype path -- the
-- new checkE AppS rule is gated to bare-existential return types,
-- which excludes the parameterized-carrier functions used in chains.
-- That gating is itself under test: a regression that broadened the
-- rule's trigger (e.g. dropping the isBareExistU guard) would let it
-- over-commit on the structured-return cases below and surface as
-- either a stranded NatVar or a Rec-key-set mismatch.
postArgPropagationTests :: TestTree
postArgPropagationTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "App-check post-arg propagation of inner solutions"
      [ -- One level of indirection: monomorphic carrier with a single
        -- nat var. The pre-subtype defers (NatVar n_f vs NatVar n_g),
        -- the labelled `make` arg pins n_f := 5, and the post-subtype
        -- has to push n_f's value out to n_g.
        assertRawType
          "depth-1 nat label chain through monomorphic id_"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        id_ :: T1 n Real -> T1 n Real
        x = id_ (make 5)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")])
      , -- Polymorphic carrier (forall a. a -> a) instead of the
        -- nat-typed id_. Exercises the stripForallU + label-pinning
        -- combination through the new rule.
        assertRawType
          "depth-1 nat label chain through polymorphic id"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        id :: a -> a
        x = id (make 6)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 6, VarU (TV "Real")])
      , -- Depth-2 chain: same direction as the 3-function chain, but
        -- one level shorter. The post-subtype has to chain through
        -- two carriers (the inner f and the outer g_ both leave the
        -- nat var unsolved by themselves).
        assertRawType
          "depth-2 nat label chain"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        x = g (f (make 7))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")])
      , -- Depth-4 chain: confirms the post-subtype propagation works
        -- at arbitrary depth. If the propagation lost information at
        -- some level the result would be a stranded NatVar like the
        -- bug report.
        assertRawType
          "depth-4 nat label chain"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f1 :: T1 n Real -> T1 n Real
        f2 :: T1 n Real -> T1 n Real
        f3 :: T1 n Real -> T1 n Real
        f4 :: T1 n Real -> T1 n Real
        x = f4 (f3 (f2 (f1 (make 11))))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 11, VarU (TV "Real")])
      , -- Two labels in the same function, both must propagate
        -- through one level of indirection. Without the post-subtype
        -- one of the nat vars (typically the second) would stay
        -- stranded because the deferred constraint never re-fires.
        assertRawType
          "two-label function chained through id_"
          [r|
        module main (x)
        newtype T2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
        make :: m@Int -> n@Int -> T2 m n Real
        id_ :: T2 m n Real -> T2 m n Real
        x = id_ (make 3 4)
          |]
          (AppU (VarU (TV "T2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
      , -- Multi-arg outer function where the recursive position is one
        -- of several args. The other arg (a plain Real) must not
        -- interfere with the post-subtype's nat-var propagation.
        assertRawType
          "label propagates through second arg of multi-arg function"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        scale :: Real -> T1 n Real -> T1 n Real
        x = scale 2.0 (make 8)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 8, VarU (TV "Real")])
      , -- Both args of a multi-arg function contribute the same label
        -- value. The pre-subtype against the expected type defers,
        -- but the first arg's labelled-nat pins one side; the second
        -- arg then has to be checked against the now-pinned type,
        -- and the post-subtype propagates the result back to the
        -- expected nat var.
        assertRawType
          "shared label resolved by both args of binary op"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        add :: T1 n Real -> T1 n Real -> T1 n Real
        x = add (make 4) (make 4)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 4, VarU (TV "Real")])
      , -- Negative: depth-3 chain ending in a consumer that demands a
        -- specific dimension. The mismatch must be caught, not
        -- silently accepted by a stranded NatVar.
        exprTestBad
          "wrong-dim labelled arg at end of chain still rejected"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        consume :: T1 5 Real -> Int
        x = consume (g (f (make 7)))
          |]
      , -- Negative: shared label disagreement across two arg positions.
        -- Without correct propagation a stranded NatVar would let the
        -- mismatch slip through.
        exprTestBad
          "label disagreement across shared-var binary op rejected"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        add :: T1 n Real -> T1 n Real -> T1 n Real
        x = add (make 3) (make 5)
          |]
      , -- Nat arithmetic in the return type, propagated through a
        -- chain. `make 6 2` returns `T1 (6*2) Real = T1 12 Real`; the
        -- outer carriers must not lose the arithmetic result during
        -- propagation.
        assertRawType
          "labelled nat arithmetic propagates through chain"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: m@Int -> n@Int -> T1 (m * n) Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        x = g (f (make 6 2))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 12, VarU (TV "Real")])
      , -- Polymorphic id wraps a labelled call inside a multi-arg
        -- outer. Combines the Forall-instantiation path with the
        -- label-resolution path and exercises propagation at both
        -- layers.
        assertRawType
          "polymorphic id wrapping labelled call inside outer scale"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        id :: a -> a
        scale :: Real -> T1 n Real -> T1 n Real
        x = scale 1.0 (id (make 9))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 9, VarU (TV "Real")])
      , -- Pin a `forall a` from inside a TupS literal: the first
        -- element of the tuple arg carries a labelled-nat value, and
        -- the function's polymorphism `forall a.` must be solved by
        -- that arg. Exercises the post-subtype when the carrier is a
        -- non-nat existential.
        assertRawType
          "tuple-arg's labelled element pins outer existential"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        fst_ :: (a, Bool) -> a
        x = fst_ (make 5, True)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")])
      , -- The 3-function chain (the originally-failing test) as a
        -- regression marker in the same group so the whole class is
        -- co-located. Duplicated from natKindPromotionTests on
        -- purpose: if someone later moves or renames the original
        -- test, this group keeps locking in the contract.
        assertRawType
          "regression: labelled dims propagate through 3-function chain"
          [r|
        module main (x)
        newtype T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        x = g (f (make 9))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 9, VarU (TV "Real")])
      ]
