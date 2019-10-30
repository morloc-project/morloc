module UnitTypeTests
  ( unitTypeTests
  ) where

import Morloc.Namespace
import Morloc.Parser.Parser
import Morloc.TypeChecker.Infer
import qualified Morloc.TypeChecker.API as API

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

main :: [Module] -> [Expr]
main [] = error "Missing main"
main [m] = moduleBody m
main (m:ms)
  | moduleName m == (MV "Main") = moduleBody m
  | otherwise = main ms

-- get the toplevel type of a fully annotated expression
typeof :: [Expr] -> [Type]
typeof es = f' . head . reverse $ es
  where
    f' (Signature _ e) = [etype e]
    f' e@(AnnE _ ts) = ts
    f' t = error ("No annotation found for: " <> show t)

unres :: ((a, b), c) -> a 
unres ((x, _), _) = x

assertTerminalType :: String -> T.Text -> [Type] -> TestTree
assertTerminalType msg code t = testCase msg $ do
  result <- API.runStack 0 (typecheck (readProgram Nothing code))
  case unres result of
    -- the order of the list is not important, so sort before comparing
    (Right es') -> assertEqual "" (sort t) (sort (typeof (main es')))
    (Left err) -> error $
      "The following error was raised: " <> show err <> "\nin:\n" <> show code

-- remove all type annotations and type signatures
unannotate :: [Expr] -> [Expr]
unannotate = mapMaybe unannotate' where
  unannotate' :: Expr -> Maybe Expr
  unannotate' (AnnE e t) = unannotate' e
  unannotate' (ListE xs) = Just $ ListE (unannotate xs)
  unannotate' (TupleE xs) = Just $ TupleE (unannotate xs)
  unannotate' (LamE v e) = LamE <$> pure v <*> unannotate' e
  unannotate' (AppE e1 e2) = AppE <$> unannotate' e1 <*> unannotate' e2
  unannotate' (Declaration v e) = Declaration <$> pure v <*> unannotate' e
  unannotate' (Signature v t) = Nothing
  unannotate' e = Just e

assertTerminalExpr :: String -> T.Text -> Expr -> TestTree
assertTerminalExpr msg code expr = testCase msg $ do
  result <- API.runStack 0 (typecheck (readProgram Nothing code))
  case unres result of
    -- the order of the list is not important, so sort before comparing
    (Right es') -> assertEqual "" expr (head . reverse . sort . unannotate . main $ es')
    (Left err) -> error $
      "The following error was raised: " <> show err <> "\nin:\n" <> show code


exprEqual :: String -> T.Text -> T.Text -> TestTree
exprEqual msg code1 code2 =
  testCase msg $ do
  result1 <- API.runStack 0 (typecheck (readProgram Nothing code1))
  result2 <- API.runStack 0 (typecheck (readProgram Nothing code2))
  case (unres result1, unres result2) of
    (Right e1, Right e2) -> assertEqual "" e1 e2
    _ -> error $ "Expected equal"

exprTestFull :: String -> T.Text -> T.Text -> TestTree
exprTestFull msg code expCode =
  testCase msg $ do
  result <- API.runStack 0 (typecheck (readProgram Nothing code))
  case unres result of
    (Right e) -> assertEqual "" (main e) (main $ readProgram Nothing expCode)
    (Left err) -> error (show err)

exprTestBad :: String -> T.Text -> TestTree
exprTestBad msg e =
  testCase msg $ do
  result <- API.runStack 0 (typecheck (readProgram Nothing e))
  case unres result of
    (Right _) -> assertFailure . T.unpack $ "Expected '" <> e <> "' to fail"
    (Left _) -> return ()

expectError :: String -> MorlocError -> T.Text -> TestTree
expectError msg err expr =
  testCase msg $ do
  result <- API.runStack 0 (typecheck (readProgram Nothing expr))
  case unres result of
    (Right _) -> assertFailure . T.unpack $ "Expected failure"
    (Left err) -> return ()

testPasses :: String -> T.Text -> TestTree
testPasses msg e =
  testCase msg $ do
  result <- API.runStack 0 (typecheck (readProgram Nothing e))
  case unres result of
    (Right _) -> return ()
    (Left e) ->
      assertFailure $
      "Expected this test to pass, but it failed with the message: " <> show e

bool = VarT (TV Nothing "Bool")

num = VarT (TV Nothing "Num")

str = VarT (TV Nothing "Str")

fun [] = error "Cannot infer type of empty list"
fun [t] = t
fun (t:ts) = FunT t (fun ts)

forall [] t = t
forall (s:ss) t = Forall (TV Nothing s) (forall ss t)

forallc _ [] t = t
forallc lang (s:ss) t = Forall (TV (Just lang) s) (forall ss t)

var s = VarT (TV Nothing s)
varc l s = VarT (TV (Just l) s)

arrc l s ts = ArrT (TV (Just l) s) ts

arr s ts = ArrT (TV Nothing s) ts

lst t = arr "List" [t]

tuple ts = ArrT v ts
  where
    v = (TV Nothing . T.pack) ("Tuple" ++ show (length ts))

record rs = RecT (map (\(x, t) -> (TV Nothing x, t)) rs)

unitTypeTests =
  testGroup
    "Typechecker unit tests"
    -- comments
    [ assertTerminalType "block comments (1)" "{- -} 42" [num]
    , assertTerminalType "block comments (2)" " {--} 42{-   foo -} " [num]
    , assertTerminalType "line comments (3)" "-- foo\n 42" [num]
    -- semicolons
    , assertTerminalType "semicolons are allowed at the end" "42;" [num]
    -- primitives
    , assertTerminalType "primitive integer" "42" [num]
    , assertTerminalType "primitive big integer" "123456789123456789123456789" [num]
    , assertTerminalType "primitive decimal" "4.2" [num]
    , assertTerminalType "primitive negative number" "-4.2" [num]
    , assertTerminalType "primitive positive number (with sign)" "+4.2" [num]
    , assertTerminalType "primitive scientific large exponent" "4.2e3000" [num]
    , assertTerminalType
        "primitive scientific irregular"
        "123456789123456789123456789e-3000"
       [num]
    , assertTerminalType
        "primitive big real"
        "123456789123456789123456789.123456789123456789123456789"
       [num]
    , assertTerminalType "primitive boolean" "True" [bool]
    , assertTerminalType "primitive string" "\"this is a string literal\"" [str]
    , assertTerminalType "primitive integer annotation" "42 :: Num" [num]
    , assertTerminalType "primitive boolean annotation" "True :: Bool" [bool]
    , assertTerminalType "primitive double annotation" "4.2 :: Num" [num]
    , assertTerminalType
        "primitive string annotation"
        "\"this is a string literal\" :: Str"
        [str]
    , assertTerminalType "primitive declaration" "x = True; 4.2" [num]
    -- declarations
    , assertTerminalType
        "identity function declaration and application"
        "f x = x; f 42"
       [num]
    , assertTerminalType
        "snd function declaration and application"
        "snd x y = y; snd True 42"
        [num]

    , assertTerminalType
        "explicit annotation within an application"
        "f :: Num -> Num; f (42 :: Num)"
        [num]

    -- lambdas
    , assertTerminalExpr
        "functions return lambda expressions"
        "\\x -> 42"
        (LamE (EV "x") (NumE 42.0))
    , assertTerminalType
        "functions can be passed"
        "g f = f 42; g"
        [forall ["a"] (fun [(fun [num, var "a"]), var "a"])]
    , assertTerminalType
        "function with parameterized types"
        "f :: a b -> c; f"
        [fun [arr "a" [var "b"], var "c"]]
    , assertTerminalType "fully applied lambda (1)" "(\\x y -> x) 1 True" [num]
    , assertTerminalType "fully applied lambda (2)" "(\\x -> True) 42" [bool]
    , assertTerminalType "fully applied lambda (3)" "(\\x -> (\\y -> True) x) 42" [bool]
    , assertTerminalType "fully applied lambda (4)" "(\\x -> (\\y -> x) True) 42" [num]
    , assertTerminalType
        "unapplied lambda, polymorphic (1)"
        "(\\x -> True)"
        [forall ["a"] (fun [var "a", bool])]
    , assertTerminalType
        "unapplied lambda, polymorphic (2)"
        "(\\x y -> x) :: forall a b . a -> b -> a"
        [forall ["a", "b"] (fun [var "a", var "b", var "a"])]
    , assertTerminalType
        "annotated, fully applied lambda"
        "((\\x -> x) :: forall a . a -> a) True"
        [bool]
    , assertTerminalType
        "annotated, partially applied lambda"
        "((\\x y -> x) :: forall a b . a -> b -> a) True"
        [forall ["a"] (fun [var "a", bool])]
    , assertTerminalType
        "recursive functions are A-OK"
        "\\f -> f 5"
        [forall ["a"] (fun [fun [num, var "a"], var "a"])]

    -- applications
    , assertTerminalType
        "primitive variable in application"
        "x = True; (\\y -> y) x"
        [bool]
    , assertTerminalType
        "function variable in application"
        "f = (\\x y -> x); f 42"
        [forall ["a"] (fun [var "a", num])]
    , assertTerminalType
        "partially applied function variable in application"
        "f = (\\x y -> x); x = f 42; x"
        [forall ["a"] (fun [var "a", num])]
    , exprTestBad
        "applications with too many arguments fail"
        "f :: forall a . a; f Bool 12"
    , exprTestBad
        "applications with mismatched types fail (1)"
        "abs :: Num -> Num; abs True"
    , exprTestBad
        "applications with mismatched types fail (2)"
        "f = 14; g = \\x h -> h x; (g True) f"
    , expectError
        "applications of non-functions should fail (1)"
        NonFunctionDerive
        "f = 5; g = \\x -> f x; g 12"
    , expectError
        "applications of non-functions should fail (2)"
        NonFunctionDerive
        "f = 5; g = \\h -> h 5; g f"

    -- evaluation within containers
    , expectError
        "arguments to a function are monotypes"
        (SubtypeError num bool)
        "f :: forall a . a -> a; g = \\h -> (h 42, h True); g f"
    , assertTerminalType
        "polymorphism under lambdas (203f8c) (1)"
        "f :: forall a . a -> a; g = \\h -> (h 42, h 1234); g f"
        [tuple [num, num]]
    , assertTerminalType
        "polymorphism under lambdas (203f8c) (2)"
        "f :: forall a . a -> a; g = \\h -> [h 42, h 1234]; g f"
        [lst num]

    -- binding
    , assertTerminalType
        "annotated variables without definition are legal"
        "x :: Num"
        [num]
    , assertTerminalType
        "unannotated variables with definition are legal"
        "x = 42; x"
        [num]
    , exprTestBad
        "unannotated variables without definitions are illegal ('\\x -> y')"
        "\\x -> y"

    -- parameterized types
    , assertTerminalType
        "parameterized type (n=1)"
        "xs :: Foo a"
        [arr "Foo" [var "a"]]
    , assertTerminalType
        "parameterized type (n=2)"
        "xs :: Foo a b"
        [arr "Foo" [var "a", var "b"]]
    , assertTerminalType
        "nested parameterized type"
        "xs :: Foo (Bar a) [b]"
        [arr "Foo" [arr "Bar" [var "a"], arr "List" [var "b"]]]

    -- type signatures and higher-order functions
    , assertTerminalType
        "type signature: identity function"
        "f :: forall a . a -> a; f 42"
        [num]
    , assertTerminalType
        "type signature: apply function with primitives"
        "apply :: (Num -> Bool) -> Num -> Bool; f :: Num -> Bool; apply f 42"
        [bool]
    , assertTerminalType
        "type signature: generic apply function"
        "apply :: forall a b . (a->b) -> a -> b; f :: Num -> Bool; apply f 42"
        [bool]
    , assertTerminalType
        "type signature: map"
        "map :: forall a b . (a->b) -> [a] -> [b]; f :: Num -> Bool; map f [5,2]"
        [lst bool]
    , assertTerminalType
        "type signature: sqrt with realizations"
        "sqrt :: Num -> Num; sqrt R :: numeric -> numeric; sqrt"
        [ fun [num, num]
        , fun [varc RLang "numeric", varc RLang "numeric"]]

    -- shadowing
    , assertTerminalType
        "name shadowing in lambda expressions"
        "f x = (14,x); g x f = f x; g True f"
        [tuple [num, bool]]
    , assertTerminalType
        "function passing without shadowing"
        "f x = (14,x); g foo = foo True; g f"
        [tuple [num, bool]]
    , assertTerminalType
        "shadowed qualified type variables (7ffd52a)"
        "f :: forall a . a -> a; g :: forall a . a -> Num; g f"
        [num]
    , assertTerminalType
        "non-shadowed qualified type variables (7ffd52a)"
        "f :: forall a . a -> a; g :: forall b . b -> Num; g f"
        [num]

    -- lists
    , assertTerminalType "list of primitives" "[1,2,3]" [lst num]
    , assertTerminalType
        "list containing an applied variable"
        "f :: forall a . a -> a; [53, f 34]"
        [lst num]
    , assertTerminalType "empty list" "[]" [forall ["a"] (lst (var "a"))]
    , assertTerminalType
        "list in function signature and application"
        "f :: [Num] -> Bool; f [1]"
        [bool]
    , assertTerminalType
        "list in generic function signature and application"
        "f :: forall a . [a] -> Bool; f [1]"
        [bool]
    , exprTestBad "failure on heterogenous list" "[1,2,True]"

    -- tuples
    , assertTerminalType
        "tuple of primitives"
        "(4.2, True)"
        [arr "Tuple2" [num, bool]]
    , assertTerminalType
        "tuple containing an applied variable"
        "f :: forall a . a -> a; (f 53, True)"
        [tuple [num, bool]]
    , assertTerminalType
        "check 2-tuples type signature"
        "f :: (Num, Str)"
        [tuple [num, str]]
    , assertTerminalType "1-tuples are just for grouping" "f :: (Num)" [num]

    -- -- TODO: reconsider what an empty tuple is
    -- -- I am inclined to cast it as the unit type
    -- , assertTerminalType "empty tuples are of unit type" "f :: ()" UniT

    -- records
    , assertTerminalType
        "primitive record statement"
        "{x=42, y=\"yolo\"}"
        [record [("x", num), ("y", str)]]
    , assertTerminalType
        "primitive record signature"
        "Foo :: {x :: Num, y :: Str}"
        [record [("x", num), ("y", str)]]
    , assertTerminalType
        "primitive record declaration"
        "foo = {x = 42, y = \"yolo\"}; foo"
        [record [("x", num), ("y", str)]]
    , assertTerminalType
        "nested records"
        "Foo :: {x :: Num, y :: {bob :: Num, tod :: Str}}"
        [record [("x", num), ("y", record [("bob", num), ("tod", str)])]]
    , assertTerminalType
        "records with variables"
        "a=42; b={x=a, y=\"yolo\"}; f=\\b->b; f b"
        [record [("x", num), ("y", str)]]

    -- extra space
    , assertTerminalType "leading space" " 42" [num]
    , assertTerminalType "trailing space" "42 " [num]

    -- adding signatures to declarations
    , assertTerminalType
        "declaration with a signature (1)"
        "f :: forall a . a -> a; f x = x; f 42"
        [num]
    , assertTerminalType
        "declaration with a signature (2)"
        "f :: Num -> Bool; f x = True; f 42"
        [bool]
    , assertTerminalType
        "declaration with a signature (3)"
        "f :: Num -> Bool; f x = True; f"
        [fun [num, bool]]
    , expectError
        "primitive type mismatch should raise error"
        (SubtypeError num bool)
        "f :: Num -> Bool; f x = 9999"

    -- tags
    , exprEqual "variable tags" "F :: Int" "F :: foo:Int"
    , exprEqual "list tags" "F :: [Int]" "F :: foo:[Int]"
    , exprEqual
        "record tags"
        "F :: {x::Int, y::Str}"
        "F :: foo:{x::Int, y::Str}"
    , exprEqual
        "nested tags (tuple)"
        "F :: (Int, Str)"
        "F :: foo:(i:Int, s:Str)"
    , exprEqual "nested tags (list)" "F :: [Int]" "F :: xs:[x:Int]"
    , exprEqual
        "nested tags (record)"
        "F :: {x::Int, y::Str}"
        "F :: foo:{x::(i:Int), y::Str}"

    -- properties
    , assertTerminalType "property syntax (1)" "f :: Foo => Num; f" [num]
    , assertTerminalType "property syntax (2)" "f :: Foo bar => Num; f" [num]
    , assertTerminalType "property syntax (3)" "f :: Foo a, Bar b => Num; f" [num]
    , assertTerminalType "property syntax (4)" "f :: (Foo a) => Num; f" [num]
    , assertTerminalType "property syntax (5)" "f :: (Foo a, Bar b) => Num; f" [num]
    -- constraints
    , assertTerminalType "constraint syntax (1)" "f :: Num where {ladida}; f" [num]
    , assertTerminalType
        "constraint syntax (1)"
        "f :: Num where { ladida ; foo }; f"
        [num]

    -- tests modules
    , assertTerminalType "basic Main module" "module Main {[1,2,3]}" [lst num]
    , (flip $ assertTerminalType "import/export") [lst num] $
      T.unlines
        [ "module Foo {export x; x = 42};"
        , "module Bar {export f; f :: forall a . a -> [a]};"
        , "module Main {import Foo (x); import Bar (f); f x}"
        ]
    , assertTerminalType
        "Allow gross overuse of semicolons"
        ";;;;;module foo{;42;  ;};"
        [num]
    , expectError
        "fail on import of Main"
        CannotImportMain $
        T.unlines
          ["module Main {export x; x = 42};", "module Foo {import Main (x)}"]
    , expectError
        "fail on import of non-existing variable"
        (BadImport (MV "Foo") (EV "x")) $
        T.unlines
          ["module Foo {export y; y = 42};", "module Main {import Foo (x); x}"]
    , expectError
        "fail on cyclic dependency"
        CyclicDependency $
        T.unlines
          [ "module Foo {import Bar (y); export x; x = 42};"
          , "module Bar {import Foo (x); export y; y = 88}"
          ]
    , expectError
        "fail on redundant module declaration"
        (MultipleModuleDeclarations (MV "Foo")) $
        T.unlines ["module Foo {x = 42};", "module Foo {x = 88}"]
    , expectError "fail on self import"
        (SelfImport (MV "Foo")) $
        T.unlines ["module Foo {import Foo (x); x = 42}"]
    , expectError
        "fail on import of non-exported variable"
        (BadImport (MV "Foo") (EV "x")) $
        T.unlines ["module Foo {x = 42};", "module Main {import Foo (x); x}"]

    -- test realization integration
    , assertTerminalType
        "a realization can be defined following general type signature"
        (T.unlines ["f :: Num -> Num;", "f r :: integer -> integer;", "f 44"])
        [num, varc RLang "integer"]
    , assertTerminalType
        "realizations can map one general type to multiple specific ones"
        (T.unlines ["f :: Num -> Num;", "f r :: integer -> numeric;", "f 44"])
        [num, varc RLang "numeric"]
    , assertTerminalType
        "realizations can map multiple general type to one specific one"
        (T.unlines ["f :: Num -> Nat;", "f r :: integer -> integer;", "f 44"])
        [var "Nat", varc RLang "integer"]
    , assertTerminalType
        "multiple realizations for different languages can be defined"
        (T.unlines
          [ "f :: Num -> Num;"
          , "f r :: integer -> integer;"
          , "f c :: int -> int;"
          , "f 44"
          ])
        [num, varc CLang "int", varc RLang "integer"]
    , assertTerminalType
        "realizations with parameterized variables"
        (T.unlines
          [ "f :: [Num] -> Num;"
          , "f r :: integer -> integer;"
          , "f cpp :: \"std::vector<$1>\" int -> int;"
          , "f [44]"
          ])
        [num, varc CppLang "int", varc RLang "integer"]
    , assertTerminalType
        "realizations can use quoted variables"
        (T.unlines
          [ "sum :: [Num] -> Num;"
          , "sum c :: \"double*\" -> double;"
          , "sum cpp :: \"std::vector<$1>\" double -> double;"
          , "sum [1,2]"
          ])
        [num, varc CLang "double", varc CppLang "double"]
    , assertTerminalType
        "the order of general signatures and realizations does not matter (1)"
        (T.unlines
          [ "f r :: integer -> integer;"
          , "f :: Num -> Num;"
          , "f c :: int -> int;"
          , "f 44"
          ])
        [num, varc CLang "int", varc RLang "integer"]
    , assertTerminalType
        "the order of general signatures and realizations does not matter (2)"
        (T.unlines
          [ "f r :: integer -> integer;"
          , "f c :: int -> int;"
          , "f :: Num -> Num;"
          , "f 44"
          ])
        [num, varc CLang "int", varc RLang "integer"]
    , assertTerminalType
        "multiple realizations for a single language cannot be defined"
        (T.unlines
          [ "f r :: a -> b;"
          , "f r :: c -> d;"
          , "f 1"
          ])
        [varc RLang "b", varc RLang "d"]
    , assertTerminalType
        "general signatures are optional"
        (T.unlines ["f r :: integer -> integer;", "f 44"])
        [varc RLang "integer"]
    , assertTerminalType 
        "compositions can have concrete realizations"
        "f r :: integer -> integer; f x = 42; f 44"
        [varc RLang "integer", num]
    , expectError
       "arguments number in realizations must equal the general case (1)"
        BadRealization $
        T.unlines
          ["f :: Num -> String -> Num;", "f r :: integer -> integer;", "f 44"]
    , expectError
         "arguments number in realizations must equal the general case (2)"
         BadRealization $
         T.unlines
           ["f   :: Num -> Num;", "f r :: integer -> integer -> string;", "f 44"]
    , assertTerminalType
        "multiple realizations for one type"
        (T.unlines
          [ "foo :: Num -> Num;"
          , "foo r :: a -> b;"
          , "foo c :: c -> d;"
          , "bar c :: c -> c;"
          , "foo (bar 1);"
          ])
        [num, varc CLang "d", varc RLang "b"]
    , assertTerminalType
      "concrete snd: simple test with containers"
      (T.unlines
        [ "snd :: forall a b . (a, b) -> b;"
        , "snd r :: forall a b . list a b -> b;"
        , "snd (1, True);"
        ])
        [bool, forallc RLang ["a"] (varc RLang "a")]
    , assertTerminalType
      "concrete map: single map, single f"
      (T.unlines
        [ "map cpp :: forall a b . (a -> b) -> \"std::vector<$1>\" a -> \"std::vector<$1>\" b;"
        , "f cpp :: double -> double;"
        , "map f [1,2]"
        ])
      [arrc CppLang "std::vector<$1>" [varc CppLang "double"]]
    , assertTerminalType
      "concrete map: multiple maps, single f"
      (T.unlines
        [ "map :: forall a b . (a -> b) -> [a] -> [b];"
        , "map c :: forall a b . (a -> b) -> \"std::vector<$1>\" a -> \"std::vector<$1>\" b;"
        , "map r :: forall a b . (a -> b) -> vector a -> vector b;"
        , "f c :: double -> double;"
        , "map f [1,2]"
        ])
      [ forall ["a"] (arr "List" [var "a"])
      , forallc RLang ["a"] (arrc RLang "vector" [varc RLang "a"])
      , arrc CLang "std::vector<$1>" [varc CLang "double"]
      ]
    , assertTerminalType
      "infer type signature from concrete functions"
      (T.unlines
        [ "sqrt :: Num -> Num;" 
        , "sqrt R :: numeric -> numeric;"
        , "foo x = sqrt x;"
        , "sqrt 42"
        ])
      [num, varc RLang "numeric"]
    , assertTerminalType
      "calls cross-language"
      (T.unlines
        [ "f R :: a -> b;"
        , "g C :: b -> c;"
        , "g (f 4);"
        ])
      [varc CLang "c"]
    , assertTerminalType
      "language branching"
      (T.unlines
        [ "id R :: forall a . a -> a;"
        , "sqrt C :: double -> double;"
        , "sqrt R :: numeric -> numeric;"
        , "id (sqrt 4);"
        ])
      [varc RLang "numeric"]
    , assertTerminalType
      "obligate foreign call"
      (T.unlines
        [ "foo r :: forall a . (a -> a) -> a -> a;"
        , "f c :: b -> b;"
        , "foo f 1"
        ])
      [forallc RLang ["a"] (varc RLang "a")]
    , assertTerminalType
      "declarations represent all realizations"
      (T.unlines
        [ "sqrt :: Num -> Num;"
        , "sqrt r :: integer -> numeric;"
        , "foo x = sqrt x;"
        , "foo"
        ])
      [fun [num, num], fun [varc RLang "integer", varc RLang "numeric"]]

    -- internal
    , exprTestFull
        "every sub-expression should be annotated in output"
        "f :: forall a . a -> Bool; f 42"
        "f :: forall a . a -> Bool; (((f :: Num -> Bool) (42 :: Num)) :: Bool)"
    ]
