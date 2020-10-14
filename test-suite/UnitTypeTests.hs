{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module UnitTypeTests
  ( unitTypeTests
  , typeOrderTests
  , typeAliasTests
  , jsontype2jsonTests
  ) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Parser
import Text.RawString.QQ
import Morloc.CodeGenerator.Grammars.Common (jsontype2json)
import qualified Morloc.Data.Doc as Doc
import qualified Morloc.Data.DAG as MDD
import Morloc.Frontend.Infer hiding(typecheck)
import Morloc.Frontend.Desugar (desugar)
import Morloc (typecheck)
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.PartialOrder as MP

import qualified Data.Text as T
import qualified Data.PartialOrd as DP
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

main :: Ord k => (n -> a) -> DAG k e n -> a
main f d = case MDD.roots d of
  [] -> error "Missing or circular module"
  [k] -> case Map.lookup k d of
    (Just (m,_)) -> f m
  _ -> error "Cannot handle multiple roots"

mainDecMap :: TypedDag -> [(EVar, Expr)]
mainDecMap d = [(v, e) | (Declaration v e) <- main typedNodeBody d]

-- get the toplevel type of a fully annotated expression
typeof :: [Expr] -> [UnresolvedType]
typeof es = f' . head . reverse $ es
  where
    f' (Signature _ e) = [etype e]
    f' e@(AnnE _ ts) = ts
    f' t = error ("No annotation found for: " <> show t)

run :: T.Text -> IO (Either MorlocError TypedDag)
run code = do
  ((x, _), _) <- MM.runMorlocMonad 0 emptyConfig (typecheck Nothing (Code code))
  return x
  where
    emptyConfig =  Config
        { configHome = Path ""
        , configLibrary = Path ""
        , configTmpDir = Path ""
        , configLangPython3 = Path ""
        , configLangR = Path ""
        , configLangPerl = Path ""
        }

assertTerminalType :: String -> T.Text -> [UnresolvedType] -> TestTree
assertTerminalType msg code t = testCase msg $ do
  result <- run code
  case result of
    -- the order of the list is not important, so sort before comparing
    (Right es') -> assertEqual "" (sort t) (sort (typeof (main typedNodeBody es')))
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

-- assert the full expression with all annotations removed
assertTerminalExpr :: String -> T.Text -> Expr -> TestTree
assertTerminalExpr = assertTerminalExpr' unannotate

-- assert the full expression with complete sub-expression annotations
assertTerminalExprWithAnnot :: String -> T.Text -> Expr -> TestTree
assertTerminalExprWithAnnot = assertTerminalExpr' id 

-- assert the last expression in the main module, process the expression with f
assertTerminalExpr' :: ([Expr] -> [Expr]) -> String -> T.Text -> Expr -> TestTree
assertTerminalExpr' f msg code expr = testCase msg $ do
  result <- run code
  case result of
    -- the order of the list is not important, so sort before comparing
    (Right es') ->
      assertEqual "" expr (head . reverse . sort . f . main typedNodeBody $ es')
    (Left err) -> error $
      "The following error was raised: " <> show err <> "\nin:\n" <> show code

exprEqual :: String -> T.Text -> T.Text -> TestTree
exprEqual msg code1 code2 =
  testCase msg $ do
  result1 <- run code1
  result2 <- run code2
  case (result1, result2) of
    (Right e1, Right e2) -> assertEqual "" e1 e2
    _ -> error $ "Expected equal"

exprTestFull :: String -> T.Text -> T.Text -> TestTree
exprTestFull msg code expCode =
  testCase msg $ do
  result <- run code
  case result of
    (Right e)
      -> assertEqual ""
            (main typedNodeBody e)
            (main parserNodeBody $ readProgram Nothing expCode Map.empty)
    (Left err) -> error (show err)

-- assert the exact expressions
exprTestFullDec :: String -> T.Text -> [(EVar, Expr)] -> TestTree
exprTestFullDec msg code expCode =
  testCase msg $ do
  result <- run code
  case result of
    (Right e) -> assertEqual "" (mainDecMap e) expCode
    (Left err) -> error (show err)

exprTestBad :: String -> T.Text -> TestTree
exprTestBad msg code =
  testCase msg $ do
  result <- run code
  case result of
    (Right _) -> assertFailure . T.unpack $ "Expected '" <> code <> "' to fail"
    (Left _) -> return ()

expectError :: String -> MorlocError -> T.Text -> TestTree
expectError msg err code =
  testCase msg $ do
  result <- run code
  case result of
    (Right _) -> assertFailure . T.unpack $ "Expected failure"
    (Left err) -> return ()

testPasses :: String -> T.Text -> TestTree
testPasses msg code =
  testCase msg $ do
  result <- run code
  case result of
    (Right _) -> return ()
    (Left e) ->
      assertFailure $
      "Expected this test to pass, but it failed with the message: " <> show e

testEqual :: (Eq a, Show a) => String -> a -> a -> TestTree
testEqual msg x y =
  testCase msg $ assertEqual "" x y

testNotEqual :: Eq a => String -> a -> a -> TestTree
testNotEqual msg x y =
  testCase msg $ assertEqual "" (x == y) False

testTrue :: String -> Bool -> TestTree
testTrue msg x =
  testCase msg $ assertEqual "" x True

testFalse :: String -> Bool -> TestTree
testFalse msg x =
  testCase msg $ assertEqual "" x False

bool = VarU (TV Nothing "Bool")

num = VarU (TV Nothing "Num")

str = VarU (TV Nothing "Str")

fun [] = error "Cannot infer type of empty list"
fun [t] = t
fun (t:ts) = FunU t (fun ts)

forall [] t = t
forall (s:ss) t = ForallU (TV Nothing s) (forall ss t)

forallc _ [] t = t
forallc lang (s:ss) t = ForallU (TV (Just lang) s) (forall ss t)

var s = VarU (TV Nothing s)
varc l s = VarU (TV (Just l) s)

arrc l s ts = ArrU (TV (Just l) s) ts

arr s ts = ArrU (TV Nothing s) ts

lst t = arr "List" [t]

tuple ts = ArrU v ts
  where
    v = (TV Nothing . T.pack) ("Tuple" ++ show (length ts))

record rs = NamU (TV Nothing "Record") rs

jsontype2jsonTests =
  testGroup
    "Test conversion of JsonType's to JSON text"
    [ jsontest "value"
        (VarJ "int")
        [r|"int"|]
    , jsontest "array(value)"
        (ArrJ "list" [VarJ "int"])
        [r|{"list":["int"]}|]
    , jsontest "object(value)"
        (NamJ "Person" [("name", VarJ "Str"), ("age", VarJ "Int")])
        [r|{"Person":{"name":"Str","age":"Int"}}|]
    , jsontest "array(array)"
        (ArrJ "list" [ArrJ "matrix" [VarJ "int"]])
        [r|{"list":[{"matrix":["int"]}]}|]
    , jsontest "array(object)"
        (ArrJ "list" [(NamJ "Person" [("name", VarJ "Str"), ("age", VarJ "Int")])])
        [r|{"list":[{"Person":{"name":"Str","age":"Int"}}]}|]
    , jsontest "object(array)"
        (NamJ "Person" [("name", VarJ "Str"), ("friends", ArrJ "list" [VarJ "Str"])])
        [r|{"Person":{"name":"Str","friends":{"list":["Str"]}}}|]
    , jsontest "object(object)"
        (NamJ "Person"
          [ ("name", VarJ "Str")
          , ("pet", NamJ "Animal" [("name", VarJ "Str"), ("species", VarJ "Str")])
          ])
        [r|{"Person":{"name":"Str","pet":{"Animal":{"name":"Str","species":"Str"}}}}|]
    ]
  where
    jsontest msg t j = testEqual msg (Doc.render $ jsontype2json t) j

typeAliasTests =
  testGroup
    "Test type alias substitutions"
    [ assertTerminalType
        "non-parametric, general type alias"
        (T.unlines
          [ "type Foo = A;"
          , "f :: Foo -> B;"
          , "f"
          ]
        )
        [fun [var "A", var "B"]]
    , assertTerminalType
        "deep type substitution: `[Foo] -> B`"
        (T.unlines
          [ "type Foo = A;"
          , "f :: [Foo] -> B;"
          , "f"
          ]
        )
        [fun [lst (var "A"), var "B"]]
    , assertTerminalType
        "deep type substitution: `[Foo] -> Foo`"
        (T.unlines
          [ "type Foo = A;"
          , "f :: [Foo] -> Foo;"
          , "f"
          ]
        )
        [fun [lst (var "A"), var "A"]]
    , assertTerminalType
        "deep type substitution: `[Foo] -> { a = Foo }`"
        (T.unlines
          [ "type Foo = A;"
          , "f :: [Foo] -> { a :: Foo } ;"
          , "f"
          ]
        )
        [fun [lst (var "A"), record [("a", var "A")]]]
    , assertTerminalType
        "parametric alias, general type alias"
        (T.unlines
          [ "type (Foo a b) = (a,b);"
          , "f :: Foo X Y -> Z;"
          , "f"
          ]
        )
        [fun [tuple [var "X", var "Y"], var "Z"]]
    , assertTerminalType
        "non-parametric alias, concrete type alias"
        (T.unlines
          [ "type C Num = double;"
          , "f C :: Num -> \"int\";"
          , "f"
          ]
        )
        [fun [varc CLang "double", varc CLang "int"]]
    , assertTerminalType
        "language-specific types are be nested"
        (T.unlines
          [ "type R Num = \"numeric\";"
          , "f R :: [Num] -> \"integer\";"
          , "f"
          ]
        )
        [fun [arrc RLang "list" [varc RLang "numeric"], varc RLang "integer"]]
    , assertTerminalType
        "no substitution is across languages"
        (T.unlines
          [ "type Num = \"numeric\";"
          , "f R :: [Num] -> \"integer\";"
          , "f"
          ]
        )
        [fun [arrc RLang "list" [varc RLang "Num"], varc RLang "integer"]]
    , assertTerminalType
        "parametric alias, concrete type alias"
        (T.unlines
          [ "type Cpp (Map a b) = \"std::map<$1,$2>\" a b;"
          , "f Cpp :: Map \"int\" \"double\" -> \"int\";"
          , "f"
          ]
        )
        [ fun [arrc CppLang "std::map<$1,$2>" [varc CppLang "int", varc CppLang "double"]
              , varc CppLang "int"]]
    , assertTerminalType
        "nested types"
        (T.unlines
          [ "type A = B;"
          , "type B = C;"
          , "foo :: A -> B -> C;"
          , "foo"
          ]
        )
        [fun [var "C", fun [var "C", var "C"]]]
    , expectError
        "fail neatly for self-recursive type aliases"
        (SelfRecursiveTypeAlias (TV Nothing "A"))
        (T.unlines
          [ "type A = (A,A);"
          , "foo :: A -> B -> C;"
          , "foo"
          ]
        )
    , expectError
        "fail neatly for mutually-recursive type aliases"
        (MutuallyRecursiveTypeAlias [TV Nothing "A", TV Nothing "B"])
        (T.unlines
          [ "type A = B;"
          , "type B = A;"
          , "foo :: A -> B -> C;"
          , "foo"
          ]
        )
    , expectError
        "fail on too many type aliases parameters"
        (BadTypeAliasParameters (TV Nothing "A") 0 1)
        (T.unlines
          [ "type A = B;"
          , "foo :: A Int -> C;"
          , "foo"
          ]
        )
    , expectError
        "fail on too few type aliases parameters"
        (BadTypeAliasParameters (TV Nothing "A") 1 0)
        (T.unlines
          [ "type (A a) = (a,a);"
          , "foo :: A -> C;"
          , "foo"
          ]
        )
    -- import tests ---------------------------------------
    , assertTerminalType
        "non-parametric, general type alias, imported"
        (T.unlines
          [ "module M1 { type Foo = A; export Foo;}"
          , "module Main { import M1 (Foo); f :: Foo -> B;  f;}"
          ]
        )
        [fun [var "A", var "B"]]
    , assertTerminalType
        "non-parametric, general type alias, reimported"
        (T.unlines
          [ "module M3 { type Foo = A; export Foo;}"
          , "module M2 { import M3 (Foo); export Foo;}"
          , "module M1 { import M2 (Foo); export Foo;}"
          , "module Main { import M1 (Foo); f :: Foo -> B;  f;}"
          ]
        )
        [fun [var "A", var "B"]]
    , assertTerminalType
        "non-parametric, general type alias, imported aliased"
        (T.unlines
          [ "module M1 { type Foo = A; export Foo;}"
          , "module Main { import M1 (Foo as Bar); f :: Bar -> B;  f;}"
          ]
        )
        [fun [var "A", var "B"]]
    , assertTerminalType
        "non-parametric, general type alias, reimported aliased"
        (T.unlines
          [ "module M3 { type Foo1 = A; export Foo1;}"
          , "module M2 { import M3 (Foo1 as Foo2); export Foo2;}"
          , "module M1 { import M2 (Foo2 as Foo3); export Foo3;}"
          , "module Main { import M1 (Foo3 as Foo4); f :: Foo4 -> B;  f;}"
          ]
        )
        [fun [var "A", var "B"]]
    , assertTerminalType
        "non-parametric, concrete type alias, reimported aliased"
        (T.unlines
          [ "module M3 { type Cpp Foo1 = \"int\"; type R Foo1 = \"integer\"; export Foo1;}"
          , "module M2 { import M3 (Foo1 as Foo2); export Foo2;}"
          , "module M1 { import M2 (Foo2 as Foo3); export Foo3;}"
          , "module Main { import M1 (Foo3 as Foo4); f Cpp :: Foo4 -> \"double\";  f;}"
          ]
        )
        [ fun [varc CppLang "int", varc CppLang "double"] ]
    , assertTerminalType
        "non-parametric, general type alias, duplicate import"
        (T.unlines
          [ "module M2 { type Foo = A; export Foo;}"
          , "module M1 { type Foo = A; export Foo;}"
          , "module Main { import M1 (Foo); import M2 (Foo); f :: Foo -> B;  f;}"
          ]
        )
        [fun [var "A", var "B"]]
    , assertTerminalType
        "parametric alias, general type alias, duplicate import"
        (T.unlines
          [ "module M2 { type (Foo a b) = (a,b); export Foo; }"
          , "module M1 { type (Foo c d) = (c,d); export Foo; }"
          , "module Main { import M1 (Foo); import M2 (Foo); f :: Foo X Y -> Z; f; }"
          ]
        )
        [fun [tuple [var "X", var "Y"], var "Z"]]
    ]

typeOrderTests =
  testGroup
    "Tests of type partial ordering (subtype)"
    [ testTrue
        "a <: Num"
        (MP.isSubtypeOf (forall ["a"] (var "a")) num)
    , testFalse
        "Num !< forall a . a"
        (MP.isSubtypeOf num (forall ["a"] (var "a")))
    , testTrue
        "forall a . (Num, a) <: (Num, Str)"
        (MP.isSubtypeOf (forall ["a"] (tuple [num, var "a"])) (tuple [num, str]))
    , testTrue
        "forall a b . (a, b) <: (Num, Str)"
        (MP.isSubtypeOf (forall ["a", "b"] (tuple [var "a", var "b"])) (tuple [num, str]))
    , testTrue
        "forall a . (Num, a) <: forall b . (Num, b)"
        (MP.isSubtypeOf
          (forall ["a"] (tuple [num, var "a"]))
          (forall ["b"] (tuple [num, var "b"])))
    , testTrue
        "forall a . a <: (Num, Str)"
        (MP.isSubtypeOf (forall ["a"] (var "a")) (tuple [num, str]))
    , testTrue
        "forall a . a <: forall a b . (a, b)"
        (MP.isSubtypeOf (forall ["a"] (var "a")) (forall ["a", "b"] (tuple [var "a", var "b"])))
    -- cannot compare
    , testFalse
        "[Num] !< Num"
        (MP.isSubtypeOf (lst num) num)
    , testFalse
        "Num !< [Num]"
        (MP.isSubtypeOf num (lst num))
    -- partial order of types
    , testTrue
        "forall a . [a] <= [Int]"
        ((forall ["a"] (lst (var "a"))) MP.<= (lst (var "a")))
    , testFalse
        "[Int] !< forall a . [a]"
        ((lst (var "a")) MP.<= (forall ["a"] (lst (var "a"))))
    , testTrue
        "forall a . (Num, a) <= (Num, Bool)"
        ((forall ["a"] (tuple [num, var "a"])) MP.<= (tuple [num, bool]))
    , testFalse
        "(Num, Bool) !<= forall a . (Num, a)"
        ((tuple [num, bool]) MP.<= (forall ["a"] (tuple [num, var "a"])))
    , testTrue
        "forall a b . (a, b) <= forall c . (Num, c)"
        ((forall ["a", "b"] (tuple [var "a", var "b"])) MP.<= (forall ["c"] (tuple [num, var "c"])))
    , testFalse
        "forall c . (Num, c) !<= forall a b . (a, b)"
        ((forall ["c"] (tuple [num, var "c"])) MP.<= (forall ["a", "b"] (tuple [var "a", var "b"])))
    , testTrue
        "forall a . a <= forall a b . (a, b)"
        ((forall ["a"] (var "a")) MP.<= (forall ["a", "b"] (tuple [var "a", var "b"])))
    -- test "mostSpecific"
    , testEqual
        "mostSpecific [Num, Str, forall a . a] = [Num, Str]"
        (MP.mostSpecific [num, str, forall ["a"] (var "a")])
        [num, str]
    -- test "mostGeneral"
    , testEqual
        "mostGeneral [Num, Str, forall a . a] = forall a . a"
        (MP.mostGeneral [num, str, forall ["a"] (var "a")])
        [forall ["a"] (var "a")]
    -- test mostSpecificSubtypes
    , testEqual
        "mostSpecificSubtypes: Num against [forall a . a]"
        (MP.mostSpecificSubtypes num [forall ["a"] (var "a")])
        [forall ["a"] (var "a")]

    -- test mostSpecificSubtypes different languages
    , testEqual
        "mostSpecificSubtypes: different languages"
        (MP.mostSpecificSubtypes (varc RLang "num") [forallc CLang ["a"] (var "a")])
        []

    -- test mostSpecificSubtypes for tuples
    , testEqual
        "mostSpecificSubtypes: tuples"
        (MP.mostSpecificSubtypes
          (tuple [num, num])
          [ forall ["a"] (var "a")
          , forall ["a", "b"] (tuple [var "a", var "b"])
          , forall ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
          ]
        )
        [forall ["a", "b"] (tuple [var "a", var "b"])]

    -- test mostSpecificSubtypes for tuples
    , testEqual
        "mostSpecificSubtypes: with partially generic tuples"
        (MP.mostSpecificSubtypes
          (forall ["a"] (tuple [num, var "a"]))
          [ forall ["a"] (var "a")
          , forall ["a", "b"] (tuple [var "a", var "b"])
          , forall ["a"] (tuple [num, var "a"])
          , forall ["a"] (tuple [num, bool])
          , forall ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
          ]
        )
        [forall ["a"] (tuple [num, var "a"])]
    ]

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
        (LamE (EVar "x") (NumE 42.0))
    , assertTerminalType
        "functions can be passed"
        "g f = f 42; g"
        [forall ["a"] (fun [(fun [num, var "a"]), var "a"])]
    , assertTerminalType
        "function with parameterized types"
        "f :: A B -> C; f"
        [fun [arr "A" [var "B"], var "C"]]
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
        "(\\x y -> x) :: a -> b -> a"
        [forall ["a", "b"] (fun [var "a", var "b", var "a"])]
    , assertTerminalType
        "annotated, fully applied lambda"
        "((\\x -> x) :: a -> a) True"
        [bool]
    , assertTerminalType
        "annotated, partially applied lambda"
        "((\\x y -> x) :: a -> b -> a) True"
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
        "f :: a; f Bool 12"
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
        (SubtypeError (unresolvedType2type num) (unresolvedType2type bool))
        "f :: a -> a; g = \\h -> (h 42, h True); g f"
    , assertTerminalType
        "polymorphism under lambdas (203f8c) (1)"
        "f :: a -> a; g = \\h -> (h 42, h 1234); g f"
        [tuple [num, num]]
    , assertTerminalType
        "polymorphism under lambdas (203f8c) (2)"
        "f :: a -> a; g = \\h -> [h 42, h 1234]; g f"
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
        "xs :: Foo A"
        [arr "Foo" [var "A"]]
    , assertTerminalType
        "parameterized type (n=2)"
        "xs :: Foo A B"
        [arr "Foo" [var "A", var "B"]]
    , assertTerminalType
        "nested parameterized type"
        "xs :: Foo (Bar A) [B]"
        [arr "Foo" [arr "Bar" [var "A"], arr "List" [var "B"]]]
    , assertTerminalType
        "language inference in lists #1"
        (T.unlines
          [ "bar Cpp :: \"float\" -> \"std::vector<$1>\" \"float\";"
          , "bar x = [x];"
          , "bar 5;"
          ])
        [arrc CppLang "std::vector<$1>" [varc CppLang "float"], lst (var "Num")]
    , assertTerminalType
        "language inference in lists #2"
        (T.unlines
          [ "mul :: Num -> Num -> Num;"
          , "mul Cpp :: \"int\" -> \"int\" -> \"int\";"
          , "foo = mul 2;"
          , "bar Cpp :: \"int\" -> \"std::vector<$1>\" \"int\";"
          , "bar x = [foo x, 42];"
          , "bar 5"
          ])
        [lst (var "Num"), arrc CppLang "std::vector<$1>" [varc CppLang "int"]]

    -- type signatures and higher-order functions
    , assertTerminalType
        "type signature: identity function"
        "f :: a -> a; f 42"
        [num]
    , assertTerminalType
        "type signature: apply function with primitives"
        "apply :: (Num -> Bool) -> Num -> Bool; f :: Num -> Bool; apply f 42"
        [bool]
    , assertTerminalType
        "type signature: generic apply function"
        "apply :: (a->b) -> a -> b; f :: Num -> Bool; apply f 42"
        [bool]
    , assertTerminalType
        "type signature: map"
        "map :: (a->b) -> [a] -> [b]; f :: Num -> Bool; map f [5,2]"
        [lst bool]
    , assertTerminalType
        "type signature: sqrt with realizations"
        "sqrt :: Num -> Num; sqrt R :: \"numeric\" -> \"numeric\"; sqrt"
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
        "f :: a -> a; g :: a -> Num; g f"
        [num]
    , assertTerminalType
        "non-shadowed qualified type variables (7ffd52a)"
        "f :: a -> a; g :: b -> Num; g f"
        [num]

    -- lists
    , assertTerminalType "list of primitives" "[1,2,3]" [lst num]
    , assertTerminalType
        "list containing an applied variable"
        "f :: a -> a; [53, f 34]"
        [lst num]
    , assertTerminalType "empty list" "[]" [forall ["a"] (lst (var "a"))]
    , assertTerminalType
        "list in function signature and application"
        "f :: [Num] -> Bool; f [1]"
        [bool]
    , assertTerminalType
        "list in generic function signature and application"
        "f :: [a] -> Bool; f [1]"
        [bool]
    , exprTestBad "failure on heterogenous list" "[1,2,True]"

    -- tuples
    , assertTerminalType
        "tuple of primitives"
        "(4.2, True)"
        [tuple [num, bool]]
    , assertTerminalType
        "tuple containing an applied variable"
        "f :: a -> a; (f 53, True)"
        [tuple [num, bool]]
    , assertTerminalType
        "check 2-tuples type signature"
        "f :: (Num, Str)"
        [tuple [num, str]]
    , assertTerminalType "1-tuples are just for grouping" "f :: (Num)" [num]

    --- FIXME - distinguish between Unit an Null
    -- unit type
    , assertTerminalType
        "unit as input"
        "f :: () -> Bool"
        [fun [VarU (TV Nothing "Unit"), bool]]

    , assertTerminalType
        "unit as output"
        "f :: Bool -> ()"
        [fun [bool, VarU (TV Nothing "Unit")]]

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
    , assertTerminalType
        "records with bound variables"
        "foo a = {x=a, y=\"yolo\"}; foo 42;"
        [record [("x", num), ("y", str)]]

    -- extra space
    , assertTerminalType "leading space" " 42" [num]
    , assertTerminalType "trailing space" "42 " [num]

    -- adding signatures to declarations
    , assertTerminalType
        "declaration with a signature (1)"
        "f :: a -> a; f x = x; f 42"
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
        (SubtypeError (unresolvedType2type num) (unresolvedType2type bool))
        "f :: Num -> Bool; f x = 9999"

    -- tags
    , exprEqual "variable tags" "F :: Int" "F :: foo:Int"
    , exprEqual "list tags" "F :: [Int]" "F :: foo:[Int]"
    , exprEqual "tags on parenthesized types" "F :: Int" "F :: f:(Int)"
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
        , "module Bar {export f; f :: a -> [a]};"
        , "module Main {import Foo (x); import Bar (f); f x}"
        ]
    , (flip $ assertTerminalType "import/export") [varc RLang "numeric"] $
      T.unlines
        [ "module Foo {export x; x = [1,2,3]};"
        , "module Bar {export f; f R :: [\"numeric\"] -> \"numeric\"};"
        , "module Main {import Foo (x); import Bar (f); f x}"
        ]

    , (flip $ assertTerminalType "multiple imports") [varc Python3Lang "float", varc RLang "numeric"] $
      T.unlines
        [ "module Foo {export f; f py :: [\"float\"] -> \"float\"};"
        , "module Bar {export f; f R :: [\"numeric\"] -> \"numeric\"};"
        , "module Main {import Foo (f); import Bar (f); f [1,2,3]}"
        ]

    , assertTerminalType
        "Allow gross overuse of semicolons"
        ";;;;;module foo{;42;  ;};"
        [num]
    , expectError
        "fail on import of non-existing variable"
        (BadImport (MVar "Foo") (EVar "x")) $
        T.unlines
          ["module Foo {export y; y = 42};", "module Main {import Foo (x); x}"]
    , expectError
        "fail on cyclic dependency"
        CyclicDependency $
        T.unlines
          [ "module Foo {import Bar (y); export x; x = 42};"
          , "module Bar {import Foo (x); export y; y = 88}"
          ]
    , expectError "fail on self import"
        (SelfImport (MVar "Foo")) $
        T.unlines ["module Foo {import Foo (x); x = 42}"]
    , expectError
        "fail on import of non-exported variable"
        (BadImport (MVar "Foo") (EVar "x")) $
        T.unlines ["module Foo {x = 42};", "module Main {import Foo (x); x}"]

    -- test realization integration
    , assertTerminalType
        "a realization can be defined following general type signature"
        (T.unlines ["f :: Num -> Num;", "f r :: \"integer\" -> \"integer\";", "f 44"])
        [num, varc RLang "integer"]
    , assertTerminalType
        "realizations can map one general type to multiple specific ones"
        (T.unlines ["f :: Num -> Num;", "f r :: \"integer\" -> \"numeric\";", "f 44"])
        [num, varc RLang "numeric"]
    , assertTerminalType
        "realizations can map multiple general type to one specific one"
        (T.unlines ["f :: Num -> Nat;", "f r :: \"integer\" -> \"integer\";", "f 44"])
        [var "Nat", varc RLang "integer"]
    , assertTerminalType
        "multiple realizations for different languages can be defined"
        (T.unlines
          [ "f :: Num -> Num;"
          , "f r :: \"integer\" -> \"integer\";"
          , "f c :: \"int\" -> \"int\";"
          , "f 44"
          ])
        [num, varc CLang "int", varc RLang "integer"]
    , assertTerminalType
        "realizations with parameterized variables"
        (T.unlines
          [ "f :: [Num] -> Num;"
          , "f r :: \"$1\" \"integer\" -> \"integer\";"
          , "f cpp :: \"std::vector<$1>\" \"int\" -> \"int\";"
          , "f [44]"
          ])
        [num, varc CppLang "int", varc RLang "integer"]
    , assertTerminalType
        "realizations can use quoted variables"
        (T.unlines
          [ "sum :: [Num] -> Num;"
          , "sum c :: \"$1*\" \"double\" -> \"double\";"
          , "sum cpp :: \"std::vector<$1>\" \"double\" -> \"double\";"
          , "sum [1,2]"
          ])
        [num, varc CLang "double", varc CppLang "double"]
    , assertTerminalType
        "the order of general signatures and realizations does not matter (1)"
        (T.unlines
          [ "f r :: \"integer\" -> \"integer\";"
          , "f :: Num -> Num;"
          , "f c :: \"int\" -> \"int\";"
          , "f 44"
          ])
        [num, varc CLang "int", varc RLang "integer"]
    , assertTerminalType
        "the order of general signatures and realizations does not matter (2)"
        (T.unlines
          [ "f r :: \"integer\" -> \"integer\";"
          , "f c :: \"int\" -> \"int\";"
          , "f :: Num -> Num;"
          , "f 44"
          ])
        [num, varc CLang "int", varc RLang "integer"]
    , assertTerminalType
        "multiple realizations for a single language cannot be defined"
        (T.unlines
          [ "f r :: A -> B;"
          , "f r :: C -> D;"
          , "f 1"
          ])
        [varc RLang "B", varc RLang "D"]
    , assertTerminalType
        "general signatures are optional"
        (T.unlines ["f r :: \"integer\" -> \"integer\";", "f 44"])
        [varc RLang "integer"]
    , assertTerminalType 
        "compositions can have concrete realizations"
        "f r :: \"integer\" -> \"integer\"; f x = 42; f 44"
        [varc RLang "integer", num]
    , expectError
       "arguments number in realizations must equal the general case (1)"
        BadRealization $
        T.unlines
          ["f :: Num -> String -> Num;", "f r :: \"integer\" -> \"integer\";", "f 44"]
    , expectError
         "arguments number in realizations must equal the general case (2)"
         BadRealization $
         T.unlines
           ["f   :: Num -> Num;", "f r :: \"integer\" -> \"integer\" -> string;", "f 44"]
    , assertTerminalType
        "multiple realizations for one type"
        (T.unlines
          [ "foo :: Num -> Num;"
          , "foo r :: A -> B;"
          , "foo c :: C -> D;"
          , "bar c :: C -> C;"
          , "foo (bar 1);"
          ])
        [num, varc CLang "D", varc RLang "B"]
    , assertTerminalType
      "concrete snd: simple test with containers"
      (T.unlines
        [ "snd :: (a, b) -> b;"
        , "snd r :: list a b -> b;"
        , "snd (1, True);"
        ])
        [bool, varc RLang "logical"]
    , assertTerminalType
      "concrete map: single map, single f"
      (T.unlines
        [ "map cpp :: (a -> b) -> \"std::vector<$1>\" a -> \"std::vector<$1>\" b;"
        , "f cpp :: \"double\" -> \"double\";"
        , "map f [1,2]"
        ])
      [arrc CppLang "std::vector<$1>" [varc CppLang "double"]]
    , assertTerminalType
      "concrete map: multiple maps, single f"
      (T.unlines
        [ "map :: (a -> b) -> [a] -> [b];"
        , "map c :: (a -> b) -> \"std::vector<$1>\" a -> \"std::vector<$1>\" b;"
        , "map r :: (a -> b) -> vector a -> vector b;"
        , "f c :: \"double\" -> \"double\";"
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
        , "sqrt R :: \"numeric\" -> \"numeric\";"
        , "foo x = sqrt x;"
        , "sqrt 42"
        ])
      [num, varc RLang "numeric"]
    , assertTerminalType
      "calls cross-language"
      (T.unlines
        [ "f R :: A -> B;"
        , "g Cpp :: B -> C;"
        , "g (f 4);"
        ])
      [varc CppLang "C"]
    , assertTerminalType
      "language branching"
      (T.unlines
        [ "id R :: a -> a;"
        , "sqrt C :: \"double\" -> \"double\";"
        , "sqrt R :: \"numeric\" -> \"numeric\";"
        , "id (sqrt 4);"
        ])
      [varc RLang "numeric"]
    , assertTerminalType
      "obligate foreign call"
      (T.unlines
        [ "foo r :: (a -> a) -> a -> a;"
        , "f c :: \"int\" -> \"int\";"
        , "foo f 42"
        ])
      [varc RLang "numeric"]
    , assertTerminalType
      "obligate foreign call - tupled"
      (T.unlines
        [ "foo r :: (a -> a) -> a -> (a,a);"
        , "f c :: \"int\" -> \"int\";"
        , "foo f 42"
        ])
      [arrc RLang "tuple" [varc RLang "numeric", varc RLang "numeric"]]
    , assertTerminalType
      "declarations represent all realizations"
      (T.unlines
        [ "sqrt :: Num -> Num;"
        , "sqrt r :: \"integer\" -> \"numeric\";"
        , "foo x = sqrt x;"
        , "foo"
        ])
      [fun [num, num], fun [varc RLang "integer", varc RLang "numeric"]]

    , assertTerminalType
      "all internal concrete and general types are right"
      (T.unlines
        [ "snd :: a -> b -> b;"
        , "snd Cpp :: a -> b -> b;"
        , "sqrt :: Num -> Num;"
        , "sqrt Cpp :: \"double\" -> \"double\";"
        , "foo x = snd x (sqrt x);"
        , "foo"
        ])
      [fun [num, num], fun [varc CppLang "double", varc CppLang "double"]]

    , assertTerminalType
      "declaration general type signatures are respected"
      (T.unlines
        [ "sqrt cpp :: \"double\" -> \"double\";"
        , "sqrt :: a -> a;"
        , "foo :: Num -> Num;"
        , "foo x = sqrt x;"
        , "foo"
        ])
      [fun [num, num], fun [varc CppLang "double", varc CppLang "double"]]

    , assertTerminalExprWithAnnot
      "all internal concrete and general types are right"
      (T.unlines
        [ "snd :: a -> b -> b;"
        , "snd Cpp :: a -> b -> b;"
        , "sqrt :: Num -> Num;"
        , "sqrt Cpp :: \"double\" -> \"double\";"
        , "foo x = snd x (sqrt x);"
        ])
      (Declaration (EVar "foo")
        (AnnE (LamE (EVar "x")
          (AnnE (AppE
            (AnnE (AppE
              (AnnE (VarE (EVar "snd"))
                [ fun [num, num, num]
                , fun [varc CppLang "double", varc CppLang "double", varc CppLang "double"]])
              (AnnE (VarE (EVar "x"))
                [num,varc CppLang "double"]))
              [ FunU num num
              , FunU (varc CppLang "double") (varc CppLang "double")])
            (AnnE (AppE
              (AnnE (VarE (EVar "sqrt"))
                [ FunU num num
                , FunU (varc CppLang "double") (varc CppLang "double")])
              (AnnE (VarE (EVar "x"))
                [ num
                , varc CppLang "double"]))
              [num,varc CppLang "double"]))
            [num,varc CppLang "double"]))
          [ FunU num num
          , FunU (varc CppLang "double") (varc CppLang "double")]))

    -- internal
    , exprTestFull
        "every sub-expression should be annotated in output"
        "f :: a -> Bool; f 42"
        "f :: a -> Bool; (((f :: Num -> Bool) (42 :: Num)) :: Bool)"

    -- -- TODO: resurrect to test github issue #7
    -- , exprTestFullDec
    --     "concrete types should be inferred for declared variables"
    --     (T.unlines
    --       [ "id :: Num -> Num;"
    --       , "id C :: \"int\" -> \"int\";"
    --       , "id x = x;"
    --       , "y = 40;"
    --       , "foo = id y;"
    --       ]
    --     )
    --     [ (EVar "foo",
    --       AnnE (AppE
    --           (AnnE (VarE (EVar "id")) [fun [num, num], fun [varc CLang "int", varc CLang "int"]])
    --           (AnnE (VarE (EVar "y")) [num, varc CLang "int"])
    --                                      -- ^ The purpose of this test is to assert that the above
    --                                      -- type is defined. As of commit 'c31660a0', `y` was assigned
    --                                      -- only the general type Num.
    --         )
    --       [num, varc CLang "int"]
    --       )
    --     , (EVar "id",
    --       AnnE (LamE (EVar "x")
    --           (AnnE (VarE (EVar "x"))
    --             [num, varc CLang "int"]))
    --         [fun [num, num], fun [varc CLang "int", varc CLang "int"]])
    --     , (EVar "y", AnnE (NumE 40.0) [num])
    --     ]

    -- default list evaluation of arguments
    , assertTerminalType
        "can infer multiple argument types"
        (T.unlines
          [ "ith :: [Num] -> Num -> Num;"
          , "ith R :: [\"numeric\"] -> \"numeric\" -> \"numeric\";"
          , "snd x = ith x 2;"
          , "snd [1,2,3];"
          ])
        [num, varc RLang "numeric"]
    ]
