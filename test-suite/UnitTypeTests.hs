{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module UnitTypeTests
  ( subtypeTests
  , substituteTVarTests
  , unitTypeTests
  , typeOrderTests
  , typeAliasTests
  , jsontype2jsonTests
  , packerTests
  , recordAccessTests
  , whereTests
  , orderInvarianceTests
  , whitespaceTests
  ) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Parser
import Morloc.CodeGenerator.Namespace
import Text.RawString.QQ
import Morloc.CodeGenerator.Grammars.Common (jsontype2json)
import qualified Morloc.Data.Doc as Doc
import qualified Morloc.Data.DAG as MDD
import Morloc (typecheck)
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.PartialOrder as MP
import qualified Morloc.Typecheck.Internal as MTI

import qualified Data.Text as T
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

-- get the toplevel general type of a typechecked expression
gtypeof :: (SAnno (Indexed TypeU) f c) -> TypeU
gtypeof (SAnno _ (Idx _ t)) = t

runFront :: T.Text -> IO (Either MorlocError [SAnno (Indexed TypeU) Many Int])
runFront code = do
  ((x, _), _) <- MM.runMorlocMonad Nothing 0 emptyConfig (typecheck Nothing (Code code))
  return x
  where
    emptyConfig =  Config
        { configHome = ""
        , configLibrary = ""
        , configTmpDir = ""
        , configLangPython3 = ""
        , configLangR = ""
        , configLangPerl = ""
        }

assertGeneralType :: String -> T.Text -> TypeU -> TestTree
assertGeneralType msg code t = testCase msg $ do
  result <- runFront code
  case result of
    (Right [x]) -> assertEqual "" t (gtypeof x)
    (Right _) -> error "Expected exactly one export from Main for assertGeneralType"
    (Left e) -> error $
      "The following error was raised: " <> show e <> "\nin:\n" <> show code

assertSubtypeGamma :: String -> [GammaIndex] -> TypeU -> TypeU -> [GammaIndex] -> TestTree
assertSubtypeGamma msg gs1 a b gs2 = testCase msg $ do
  let g0 = Gamma {gammaCounter = 0, gammaContext = gs1}
  case MTI.subtype a b g0 of
    Left err -> error $ show err
    Right (Gamma _ gs2') -> assertEqual "" gs2 gs2'

exprEqual :: String -> T.Text -> T.Text -> TestTree
exprEqual msg code1 code2 = undefined
  -- testCase msg $ do
  -- result1 <- runFront code1
  -- result2 <- runFront code2
  -- case (result1, result2) of
  --   (Right e1, Right e2) -> assertEqual "" e1 e2
  --   _ -> error $ "Expected equal"

exprTestFull :: String -> T.Text -> T.Text -> TestTree
exprTestFull = undefined
-- exprTestFull msg code expCode =
--   testCase msg $ do
--   result <- run code
--   case result of
--     (Left e) -> error (show e)
--     (Right e)
--       -> case readProgram Nothing expCode Map.empty of
--            (Left e') -> error (show e')
--            (Right x) -> assertEqual ""
--               (main typedNodeBody e)
--               (main parserNodeBody x)

assertPacker :: String -> T.Text -> Map.Map (TVar, Int) [UnresolvedPacker] -> TestTree
assertPacker = undefined
-- assertPacker msg code expPacker =
--   testCase msg $ do
--   result <- run code
--   case result of
--     (Right e)
--       -> assertEqual ""
--             (main typedNodePackers e)
--             expPacker
--     (Left e) -> error (show e)

exprTestBad :: String -> T.Text -> TestTree
exprTestBad msg code =
  testCase msg $ do
  result <- runFront code
  case result of
    (Right _) -> assertFailure . T.unpack $ "Expected '" <> code <> "' to fail"
    (Left _) -> return ()

-- FIXME: check that the correct error type is raised, but don't check message
-- (tweaking messages shouldn't break tests)
expectError :: String -> MorlocError -> T.Text -> TestTree
expectError msg _ code =
  testCase msg $ do
  result <- runFront code
  case result of
    (Right _) -> assertFailure . T.unpack $ "Expected failure"
    (Left _) -> return ()


testEqual :: (Eq a, Show a) => String -> a -> a -> TestTree
testEqual msg x y =
  testCase msg $ assertEqual "" x y

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
fun ts = FunU (init ts) (last ts)

forall [] t = t
forall (s:ss) t = ForallU (TV Nothing s) (forall ss t)

exist v = ExistU (TV Nothing v) [] []

forallc _ [] t = t
forallc lang (s:ss) t = ForallU (TV (Just lang) s) (forallc lang ss t)

v s = TV Nothing s 
var s = VarU (TV Nothing s)
varc l s = VarU (TV (Just l) s)

arrc l s ts = AppU (VarU (TV (Just l) s)) ts

arr s ts = AppU (VarU (TV Nothing s)) ts

lst t = arr "List" [t]

tuple ts = AppU v ts
  where
    v = VarU . TV Nothing . T.pack $ "Tuple" ++ show (length ts)

record rs = NamU NamRecord (TV Nothing "Record") [] rs

record' n rs = NamU NamRecord (TV Nothing n) [] rs

subtypeTests =
  testGroup
    "Test subtype within context"
    [ -- basic general cases
      assertSubtypeGamma "G -| A <: A |- G" [] a a []
    , assertSubtypeGamma "<a>, <b> -| <a> <: <b> |- <a>:<b>, <b>" [eag, ebg] ea eb [solvedA eb, ebg]
    , assertSubtypeGamma "<a>, <b> -| <b> <: <a> |- <a>:<b>, <b>" [eag, ebg] ea eb [solvedA eb, ebg]
    , assertSubtypeGamma "G -| (A -> B) <: (A -> B) |- G" [] (fun [a, b]) (fun [a, b]) []
    , assertSubtypeGamma "G -| [A] <: [A] |- G" [] (lst a) (lst a) []
    , assertSubtypeGamma "G -| {K :: a, L :: b} <: {K :: a, L :: b}" []
        (record' "Foo" [("K", a), ("L", b)]) 
        (record' "Foo" [("K", a), ("L", b)]) []
    , assertSubtypeGamma "<a> -| <a> <: A |- <a>:A" [eag] ea a [solvedA a]
    , assertSubtypeGamma "<a> -| A <: <a> |- <a>:A" [eag] a ea [solvedA a]
    , assertSubtypeGamma "<b> -| [A] <: <b> |- <b>:[A]" [ebg] (lst a) (eb) [solvedB (lst a)]
    , assertSubtypeGamma "<a> -| <a> <: [B] |- <a>:[B]" [eag] (lst b) (ea) [solvedA (lst b)]
    , assertSubtypeGamma "<a>, <b> -| <a> <b> <: [C] |- <a>:[C], <b>:C" [eag, ebg]
        (ExistU (v "x1") [eb] []) (lst c) [solvedA (lst c), solvedB c]
    , assertSubtypeGamma "<a>, <b> -|[C] <: <a> <b> |- <a>:[C], <b>:C" [eag, ebg]
        (lst c) (ExistU (v "x1") [eb] []) [solvedA (lst c), solvedB c]
    , assertSubtypeGamma "[] -| forall a . a <: A -| a:A" [] (forall ["a"] (var "a")) a [SolvedG (v "a") a]
    , assertSubtypeGamma "[] -| A <: forall a . a -| a:A" [] (forall ["a"] (var "a")) a [SolvedG (v "a") a]
      -- nested types
    , assertSubtypeGamma "<b> -| [A] <: [<b>] |- <b>:A" [ebg] (lst a) (lst eb) [solvedB a]
    , assertSubtypeGamma "<a> -| [<a>] <: [B] |- <a>:B" [eag] (lst b) (lst ea) [solvedA b]
    , assertSubtypeGamma "<a>, <b> -| (A, B) <: (<a>, <b>) |- <a>:A, <b>:B"
      [eag, ebg] (tuple [a, b]) (tuple [ea, eb]) [solvedA a, solvedB b]
    , assertSubtypeGamma "<a>, <b> -| (<a>, <b>) <: (A, B) |- <a>:A, <b>:B"
      [eag, ebg] (tuple [ea, eb]) (tuple [a, b]) [solvedA a, solvedB b]
    , assertSubtypeGamma "<a>, <b>, <c>, <d> -| (<a>, <b>) <: (<c>, <d>) -| <a>:<c>, <b>:<d>, <c>, <d>"
      [eag, ebg, ecg, edg] (tuple [ea, eb]) (tuple [ec, ed]) [solvedA ec, solvedB ed, ecg, edg]
    ]
  where
    a = var "A"
    b = var "B"
    c = var "C"
    ea = ExistU (v "x1") [] []
    eb = ExistU (v "x2") [] []
    ec = ExistU (v "x3") [] []
    ed = ExistU (v "x4") [] []
    eag = ExistG (v "x1") [] []
    ebg = ExistG (v "x2") [] []
    ecg = ExistG (v "x3") [] []
    edg = ExistG (v "x4") [] []
    solvedA t = SolvedG (v "x1") t
    solvedB t = SolvedG (v "x2") t
    solvedC t = SolvedG (v "x3") t
    solvedD t = SolvedG (v "x4") t

substituteTVarTests =
  testGroup
    "test variable substitution"
    [ testEqual "[x/y]Num" (substituteTVar (v "x") (var "y") num) num
    , testEqual "[y/x]([x] -> x)" (substituteTVar (v "x") (var "y") (fun [lst (var "x"), var "x"]))
        (fun [lst (var "y"), var "y"]) 
    ]

whitespaceTests =
  testGroup
    "Tests whitespace handling for modules"
    [ assertGeneralType
      "module indent == 1 and top indent == module indent"
      "module Foo\nx = 1\ny = 2\nexport y"
      num
    , assertGeneralType
      "module indent == 1 and top indent > module indent"
      "module Foo\n  x = 1\n  y = 2\n  export y"
      num
    , assertGeneralType
      "module indent > 1 and top indent > module indent"
      " module Foo\n   x = 1\n   y = 2\n   export y"
      num
    , assertGeneralType
      "module indent > 1 and top indent = module indent"
      "  module Foo\n  x = 1\n  y = 2\n  export y"
      num
    -- indenting main
    , assertGeneralType
      "main indent == 1"
      "x = 1\ny = 2\nexport y"
      num
    , assertGeneralType
      "main indent > 1"
      "  x = 1\n  y = 2\n  export y"
      num
    -- multiple modules
    , assertGeneralType
      "multiple modules at pos 1 with pos > 1 exprs"
      [r|
module Foo
  x = True
  export x
module Bar
  import Foo
  y = True
  export y
module Main
  import Bar
  z = 1
  export z
      |]
      num
    ]

recordAccessTests =
  testGroup
    "Test record access"
    [ assertGeneralType
      "Access into anonymous record"
      [r|{a = 5, b = "asdf"}@b|]
      str
    , assertGeneralType
      "Access record variable"
      [r|
          record Person = Person {a :: Num, b :: Str}
          bar :: Person
          bar@b
      |]
      str
    , assertGeneralType
      "Access record-returning expression"
      [r|
          record Person = Person {a :: Num, b :: Str}
          bar :: Num -> Person
          (bar 5)@b
      |]
      str
    , assertGeneralType
      "Access into tupled"
      [r|
         record Person = Person {a :: Num, b :: Str}
         bar :: Num -> Person
         ((bar 5)@a, (bar 6)@b)
      |]
      (tuple [num, str])
    -- , assertGeneralType
    --   "Access multiple languages"
    --   [r|
    --       record Person = Person {a :: Num, b :: Str}
    --       record R Person = Person {a :: "numeric", b :: "character"}
    --       bar :: Person
    --       bar R :: Person
    --       bar@b
    --   |]
    --   [str, varc RLang "character"]
    ]

packerTests =
  testGroup
    "Test building of packer maps"
    [ testEqual "packer test" 1 1 ]
--     [ assertPacker "no import packer"
--         [r|
--             source Cpp from "map.h" ( "mlc_packMap" as packMap
--                                     , "mlc_unpackMap" as unpackMap)
--             packMap :: pack => ([a],[b]) -> Map a b
--             unpackMap :: unpack => Map a b -> ([a],[b])
--             packMap Cpp :: pack => ([a],[b]) -> "std::map<$1,$2>" a b
--             unpackMap Cpp :: unpack => "std::map<$1,$2>" a b -> ([a],[b])
--             export Map
--         |]
--         ( Map.singleton
--             (TV (Just CppLang) "std::map<$1,$2>", 2)
--             [ UnresolvedPacker {
--                 unresolvedPackerTerm = (Just (EV [] "Map"))
--               , unresolvedPackerCType
--                 = forallc CppLang ["a","b"]
--                   ( arrc CppLang "std::tuple<$1,$2>" [ arrc CppLang "std::vector<$1>" [varc CppLang "a"]
--                                                      , arrc CppLang "std::vector<$1>" [varc CppLang "b"]])
--               , unresolvedPackerForward
--                 = [Source (Name "mlc_packMap") CppLang (Just "map.h") (EV [] ("packMap"))]
--               , unresolvedPackerReverse
--                 = [Source (Name "mlc_unpackMap") CppLang (Just "map.h") (EV [] ("unpackMap"))]
--               }
--             ]
--         )
--
--     , assertPacker "with importing and aliases"
--         [r|
-- module A
-- source Cpp from "map.h" ( "mlc_packMap" as packMap
--                         , "mlc_unpackMap" as unpackMap)
-- packMap :: pack => ([a],[b]) -> Map a b
-- unpackMap :: unpack => Map a b -> ([a],[b])
-- packMap Cpp :: pack => ([a],[b]) -> "std::map<$1,$2>" a b
-- unpackMap Cpp :: unpack => "std::map<$1,$2>" a b -> ([a],[b])
-- export Map
--
-- module Main
-- import A (Map as Hash)
--         |]
--         ( Map.singleton
--             (TV (Just CppLang) "std::map<$1,$2>", 2)
--             [ UnresolvedPacker {
--                 unresolvedPackerTerm = (Just (EV [] "Hash"))
--               , unresolvedPackerCType
--                 = forallc CppLang ["a","b"]
--                   ( arrc CppLang "std::tuple<$1,$2>" [ arrc CppLang "std::vector<$1>" [varc CppLang "a"]
--                                                      , arrc CppLang "std::vector<$1>" [varc CppLang "b"]])
--               , unresolvedPackerForward
--                 = [Source (Name "mlc_packMap") CppLang (Just "map.h") (EV [] ("packMap"))]
--               , unresolvedPackerReverse
--                 = [Source (Name "mlc_unpackMap") CppLang (Just "map.h") (EV [] ("unpackMap"))]
--               }
--             ]
--         )
--     ]

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
    [ assertGeneralType
        "general type alias"
        (T.unlines
          [ "type Foo = A"
          , "f :: Foo"
          , "export f"
          ]
        )
        (var "A")
    , assertGeneralType
        "parameterized generic"
        [r|
        f :: m (a -> b)
        export f
        |]
        (forall ["q0", "q1", "q2"] (arr "q0" [fun [var "q1", var "q2"]]))
    , assertGeneralType
        "non-parametric, general type alias"
        (T.unlines
          [ "type Foo = A"
          , "f :: Foo -> B"
          , "export f"
          ]
        )
        (fun [var "A", var "B"])
    , assertGeneralType
        "deep type substitution: `[Foo] -> B`"
        (T.unlines
          [ "type Foo = A"
          , "f :: [Foo] -> B"
          , "export f"
          ]
        )
        (fun [lst (var "A"), var "B"])
    , assertGeneralType
        "deep type substitution: `[Foo] -> Foo`"
        (T.unlines
          [ "type Foo = A"
          , "f :: [Foo] -> Foo"
          , "export f"
          ]
        )
        (fun [lst (var "A"), var "A"])
    , assertGeneralType
        "deep type substitution: `[Foo] -> { a = Foo }`"
        (T.unlines
          [ "type Foo = A"
          , "f :: [Foo] -> { a :: Foo }"
          , "export f"
          ]
        )
        (fun [lst (var "A"), record [("a", var "A")]])
    , assertGeneralType
        "parametric alias, general type alias"
        [r|
        type (Foo a b) = (a,b)
        f :: Foo X Y -> Z
        export f
        |]
        (fun [tuple [var "X", var "Y"], var "Z"])
    , assertGeneralType
        "nested types"
        [r|
           type A = B
           type B = C
           foo :: A -> B -> C
           export foo
        |]
        (fun [var "C", var "C", var "C"])
    , assertGeneralType
        "state is preserved across binding"
        [r|
           type Foo = A
           g :: Foo -> Int
           f = g
           export f
        |]
        (fun [var "A", var "Int"])
    , assertGeneralType
        "state is inherited across binding"
        [r|
           type Foo = A
           f :: Foo -> Int
           f = g  {- yes, g isn't defined -}
           export f
        |]
        (fun [var "A", var "Int"])
    -- , assertGeneralType
    --     "non-parametric alias, concrete type alias"
    --     [r|
    --        type C Num = double
    --        f C :: Num -> "int"
    --        f
    --     |]
    --     (fun [varc CLang "double", varc CLang "int"])
    -- , assertGeneralType
    --     "language-specific types are be nested"
    --     [r|
    --        type R Num = "numeric"
    --        f R :: [Num] -> "integer"
    --        f
    --     |]
    --     [fun [arrc RLang "list" [varc RLang "numeric"], varc RLang "integer"]]
    -- , assertGeneralType
    --     "no substitution is across languages"
    --     [r|
    --        type Num = "numeric"
    --        f R :: [Num] -> "integer"
    --        f
    --     |]
    --     [fun [arrc RLang "list" [varc RLang "Num"], varc RLang "integer"]]
    -- , assertGeneralType
    --     "parametric alias, concrete type alias"
    --     [r|
    --        type Cpp (Map a b) = "std::map<$1,$2>" a b
    --        f Cpp :: Map "int" "double" -> "int"
    --        f
    --     |]
    --     [ fun [arrc CppLang "std::map<$1,$2>" [varc CppLang "int", varc CppLang "double"]
    --           , varc CppLang "int"]]
    -- , assertGeneralType
    --     "nested in signature"
    --     [r|
    --        type Cpp (Map a b) = "std::map<$1,$2>" a b
    --        f Cpp :: Map "string" (Map "double" "int") -> "int"
    --        f
    --     |]
    --     [ fun [arrc CppLang "std::map<$1,$2>" [varc CppLang "string"
    --           , arrc CppLang "std::map<$1,$2>" [varc CppLang "double", varc CppLang "int"]]
    --           , varc CppLang "int"]]

    -- , assertGeneralType
    --     "existentials are resolved"
    --     [r|
    --        type Cpp (A a b) = "map<$1,$2>" a b
    --        foo Cpp :: A D [B] -> X
    --        foo
    --     |]
    --     [fun [ arrc CppLang "map<$1,$2>" [varc CppLang "D", arrc CppLang "std::vector<$1>" [varc CppLang "B"]]
    --          , varc CppLang "X"]]
    , expectError
        "fail neatly for self-recursive type aliases"
        (SelfRecursiveTypeAlias (TV Nothing "A"))
        [r|
           type A = (A,A)
           foo :: A -> B -> C
           foo
        |]
    -- -- TODO: find a way to catch mutually recursive type aliases
    -- , expectError
    --     "fail neatly for mutually-recursive type aliases"
    --     (MutuallyRecursiveTypeAlias [TV Nothing "A", TV Nothing "B"])
    --     (T.unlines
    --       [ "type A = B"
    --       , "type B = A"
    --       , "foo :: A -> B -> C"
    --       , "foo"
    --       ]
    --     )
    , expectError
        "fail on too many type aliases parameters"
        (BadTypeAliasParameters (TV Nothing "A") 0 1)
        [r|
           type A = B
           foo :: A Int -> C
           foo
        |]
    , expectError
        "fail on too few type aliases parameters"
        (BadTypeAliasParameters (TV Nothing "A") 1 0)
        [r|
           type (A a) = (a,a)
           foo :: A -> C
           foo
        |]

    -- import tests ---------------------------------------
    , assertGeneralType
        "non-parametric, general type alias, imported"
        [r|
           module M1
             type Foo = A
             export Foo
           module Main
             import M1 (Foo)
             f :: Foo -> B
             export f
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "non-parametric, general type alias, reimported"
        [r|
           module M3
             type Foo = A
             export Foo
           module M2
             import M3 (Foo)
             export Foo
           module M1
             import M2 (Foo)
             export Foo
           module Main
             import M1 (Foo)
             f :: Foo -> B
             export f
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "non-parametric, general type alias, imported aliased"
        [r|
           module M1
             type Foo = A
             export Foo
           module Main
             import M1 (Foo as Bar)
             f :: Bar -> B
             export f
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "non-parametric, general type alias, reimported aliased"
        [r|
           module M3
             type Foo1 = A
             export Foo1

           module M2
             import M3 (Foo1 as Foo2)
             export Foo2

           module M1
             import M2 (Foo2 as Foo3)
             export Foo3

           module Main
             import M1 (Foo3 as Foo4)
             f :: Foo4 -> B
             export f
        |]
        (fun [var "A", var "B"])
    -- , assertGeneralType
    --     "non-parametric, concrete type alias, reimported aliased"
    --     [r|
    --        module M3
    --        type Cpp Foo1 = "int"
    --        type R Foo1 = "integer"
    --        export Foo1
    --
    --        module M2
    --        import M3 (Foo1 as Foo2)
    --        export Foo2
    --
    --        module M1
    --        import M2 (Foo2 as Foo3)
    --        export Foo3
    --
    --        module Main
    --        import M1 (Foo3 as Foo4)
    --        f Cpp :: Foo4 -> "double"
    --        f
    --     |]
    --     [ fun [varc CppLang "int", varc CppLang "double"] ]
    , assertGeneralType
        "non-parametric, general type alias, duplicate import"
        [r|
           module M2
             type Foo = A
             export Foo

           module M1
             type Foo = A
             export Foo

           module Main
             import M1 (Foo)
             import M2 (Foo)
             f :: Foo -> B
             export f
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "parametric alias, general type alias, duplicate import"
        [r|
           module M2
             type (Foo a b) = (a,b)
             export Foo

           module M1
             type (Foo c d) = (c,d)
             export Foo

           module Main
             import M1 (Foo)
             import M2 (Foo)
             f :: Foo X Y -> Z
             export f
        |]
        (fun [tuple [var "X", var "Y"], var "Z"])
    ]


whereTests =
  testGroup
  "Test of where statements"
  [
      assertGeneralType
        "simple where"
        [r|
            f :: Num
            f = z where
                z = 42
            f
        |]
        num
    , assertGeneralType
        "calling simple where"
        [r|
            inc :: Num -> Num
            f = inc z where
                z = 42
            f
        |]
        num
    , assertGeneralType
        "calling deeper where"
        [r|
            id :: a -> a
            inc :: Num -> Num
            f = id z where
                z = inc y where
                  y = 42
            f
        |]
        num
  ]

orderInvarianceTests =
  testGroup
  "Test order invariance"
  [ assertGeneralType
      "definitions work"
      "x = 42\nx"
      num
  , assertGeneralType
      "terms may be defined before they are used"
      "y = 42\nx = y\nx"
      num
  , assertGeneralType
      "long chains of substitution are OK too"
      "z = 42\ny = z\nx = y\nx"
      num
  --   , assertTerminalType
  --       "declarations before use gain concrete types"
  --       [r|
  --         add :: Num -> Num -> Num
  --         add c :: "int" -> "int" -> "int"
  --         b = 5
  --         f = add 1 b
  --         b
  --       |]
  --       [num, varc CLang "int"]
  --   , assertTerminalType
  --       "declarations after use gains concrete types"
  --       [r|
  --         add :: Num -> Num -> Num
  --         add c :: "int" -> "int" -> "int"
  --         f = add 1 b
  --         b = 5
  --         b
  --       |]
  --       [num, varc CLang "int"]
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
    [ assertGeneralType "block comments (1)" "{- -} 42" num
    , assertGeneralType "block comments (2)" " {--} 42{-   foo -} " num
    , assertGeneralType "line comments (3)" "-- foo\n 42" num
    -- primitives
    , assertGeneralType "primitive integer" "42" num
    , assertGeneralType "primitive big integer" "123456789123456789123456789" num
    , assertGeneralType "primitive decimal" "4.2" num
    , assertGeneralType "primitive negative number" "-4.2" num
    , assertGeneralType "primitive positive number (with sign)" "+4.2" num
    , assertGeneralType "primitive scientific large exponent" "4.2e3000" num
    , assertGeneralType
        "primitive scientific irregular"
        "123456789123456789123456789e-3000"
       num
    , assertGeneralType
        "primitive big real"
        "123456789123456789123456789.123456789123456789123456789"
       num
    , assertGeneralType "primitive boolean" "True" bool
    , assertGeneralType "primitive string" "\"this is a string literal\"" str
    , assertGeneralType "primitive integer annotation" "42 :: Num" num
    , assertGeneralType "primitive boolean annotation" "True :: Bool" bool
    , assertGeneralType "primitive double annotation" "4.2 :: Num" num
    , assertGeneralType
        "primitive string annotation"
        "\"this is a string literal\" :: Str"
        str
    , assertGeneralType "primitive declaration" "x = True\n4.2" num
    -- containers
    -- - lists
    , assertGeneralType "list of one primitive" "[1]" (lst num)
    , assertGeneralType "list of many primitives" "[1,2,3]" (lst num)
    , assertGeneralType "list of many containers" "[(True,1),(False,2)]" (lst (tuple [bool, num]))
    -- - tuples
    , assertGeneralType "tuple of primitives" "(1,2,True)" (tuple [num, num, bool])
    , assertGeneralType "tuple with containers" "(1,(2,True))" (tuple [num, tuple [num, bool]])
    -- - records
    , assertGeneralType
        "primitive record statement"
        [r|
        {x=42, y="yolo"}
        |]
        (record [("x", num), ("y", str)])
    , assertGeneralType
        "primitive record signature"
        [r|
        record Foo = Foo {x :: Num, y :: Str}
        f :: Num -> Foo
        f 42
        |]
        (record' "Foo" [("x", num), ("y", str)])
    , assertGeneralType
        "primitive record declaration"
        [r|
        foo = {x = 42, y = "yolo"}
        foo
        |]
        (record [("x", num), ("y", str)])
    , assertGeneralType
        "nested records"
        [r|
        {x = 42, y = {bob = 24601, tod = "listen now closely and hear how I've planned it"}}
        |]
        (record [("x", num), ("y", record [("bob", num), ("tod", str)])])
    , assertGeneralType
        "records with bound variables"
        [r|
        foo a = {x=a, y="yolo"}
        foo 42
        |]
        (record [("x", num), ("y", str)])

    -- functions
    , assertGeneralType
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
        f :: Num -> Bool
        f 42
        |]
        bool
    , assertGeneralType
        "2-arg function signature without declaration"
        [r|
        f :: Num -> Bool -> Str
        f 42 True
        |]
        str
    , assertGeneralType
        "partial 1-2 function signature without declaration"
        [r|
        f :: Num -> Bool -> Str
        f 42
        |]
        (fun [bool, str])
    , assertGeneralType
        "identity function declaration and application"
        [r|
        f x = x
        f 42
        |]
        num
    , assertGeneralType
        "const declared function"
        [r|
        const x y = x
        const 42 True
        |]
        num
    , assertGeneralType
        "identity signature function"
        [r|
        id :: a -> a
        id 42
        |]
        num
    , assertGeneralType
        "const signature function"
        [r|
        const :: a -> b -> a
        const 42 True
        |]
        num
    , assertGeneralType
        "fst signature function"
        [r|
        fst :: (a,b) -> a
        fst (42,True)
        |]
        num
    , assertGeneralType
        "value to list function"
        [r|
        single :: a -> [a]
        single 42
        |]
        (lst num)
    , assertGeneralType
        "head function"
        [r|
        head :: [a] -> a
        head [1,2,3]
        |]
        num

    , assertGeneralType
        "make list function"
        [r|
        f :: a -> [a]
        f 1
        |]
        (lst num)

    , assertGeneralType
        "make list function"
        [r|
        single :: a -> [a]
        single 1
        |]
        (lst num)

    , assertGeneralType
        "existential application"
        "f 1"
        (exist "v2")

    , assertGeneralType
        "existential function passing"
        [r|
        g f = f True
        export g
        |]
        (fun [fun [bool, exist "v4"], exist "v4"])

    , assertGeneralType
        "app single function"
        [r|
        app :: (a -> b) -> a -> b
        f :: a -> [a]
        app f 42
        |]
        (lst num)

    , assertGeneralType
        "app head function"
        [r|
        app :: (a -> b) -> a -> b
        f :: [a] -> a
        app f [42]
        |]
        num

    , assertGeneralType
      "simple nested call"
      [r|
      f x = x
      g x = f x
      g 1
      |]
      num

    , assertGeneralType
      "nested calls"
      [r|
      f x y = (x, y)
      g x y = (x, f 1 y)
      g True "hi"
      |]
      (tuple [bool, tuple [num, str]])

    , assertGeneralType
      "zip pair"
      [r|
      pair x y = (x, y)
      zip :: (x -> y -> z) -> [x] -> [y] -> [z]
      zip pair [1,2] [True, False]
      |]
      (lst (tuple [num, bool]))

    , assertGeneralType
      "nested identity"
      [r|
      id :: a -> a
      id (id (id 1))
      |]
      num

    , assertGeneralType
      "head (head [[1]])"
      [r|
      head :: [a] -> a
      head (head [[42]])
      |]
      num

    , assertGeneralType
      "snd (snd (1,(1,True)))"
      [r|
      snd :: (a, b) -> b
      snd (snd (1, (1, True)))
      |]
      bool

    -- -- This should give a straight error, none of that nonsense about within language conversion
    -- f x = [x, 1]
    -- f True


    -- -- This fails to fail
    -- f x y = [x, y]
    -- f 1 True


    , assertGeneralType
        "f x y = [x, y]"
        [r|
        f x y = [x, y]
        f 1
        |]
        (fun [num, lst num])

    , assertGeneralType
        "map head function"
        [r|
        map :: (a -> b) -> [a] -> [b]
        head :: [a] -> a
        map head [[1],[1,2,3]]
        |]
        (lst num)

    , assertGeneralType
        "f a -> a"
        [r|
        out :: f a -> a
        out (G 1)
        |]
        num
    , assertGeneralType
        "f a b -> b"
        [r|
        snd :: f a b -> b
        snd (G 1 True)
        |]
        bool 
    , assertGeneralType
        "map id over number list"
        [r|
        map :: (a -> b) -> [a] -> [b]
        id :: a -> a
        map id [1,2,3]
        |]
        (lst num)
    , assertGeneralType
        "map fst over tuple list"
        [r|
        map :: (a -> b) -> [a] -> [b]
        fst :: (a,b) -> a
        map fst [(1,True),(2,False)]
        |]
        (lst num)
    , assertGeneralType
        "map fstG over (G a b) list"
        [r|
        map :: (a -> b) -> [a] -> [b]
        fstF :: G a b -> a
        map fstF [G 1 True, G 2 False]
        |]
        (lst num)
    , assertGeneralType
        "fmap fstG over functor"
        [r|
        fmap :: (a -> b) -> f a -> f b
        fstG :: f a b -> a
        fmap fst [G 1 True]
        |]
        (lst num)
    , assertGeneralType
        "fmap generic fst over functor"
        [r|
        fmap :: (a -> b) -> f a -> f b
        fst :: f a b -> a
        fmap fst [G 1 True]
        |]
        (lst num)

    , assertGeneralType
        "variable annotation"
        [r|
        f :: Foo
        export f
        |]
        (var "Foo")
    -- , assertGeneralType
    --     "explicit annotation within an application"
    --     "f :: Num -> Num\nf (42 :: Num)"
    --     num

    -- lambdas
    , assertGeneralType
        "function with parameterized types"
        [r|
        f :: A B -> C
        export f
        |]
        (fun [arr "A" [var "B"], var "C"])
    , assertGeneralType "fully applied lambda (1)" "(\\x y -> x) 1 True" num
    , assertGeneralType "fully applied lambda (2)" "(\\x -> True) 42" bool
    , assertGeneralType "fully applied lambda (3)" "(\\x -> (\\y -> True) x) 42" bool
    , assertGeneralType "fully applied lambda (4)" "(\\x -> (\\y -> x) True) 42" num
    , assertGeneralType
        "unapplied lambda, polymorphic (1)"
        [r|\x -> True|]
        (fun [exist "v1", bool])
    , assertGeneralType
        "unapplied lambda, polymorphic (2)"
        "(\\x y -> x) :: a -> b -> a"
        (fun [exist "v2", exist "v3", exist "v2"])
    , assertGeneralType
        "annotated, fully applied lambda"
        "((\\x -> x) :: a -> a) True"
        bool
    , assertGeneralType
        "annotated, partially applied lambda"
        "((\\x y -> x) :: a -> b -> a) True"
        (fun [exist "v3", bool])
    , assertGeneralType
        "recursive functions are A-OK"
        "\\f -> f 5"
        (fun [fun [num, exist "v4"], exist "v4"])

    -- applications
    , assertGeneralType
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
        num
    , assertGeneralType
        "partially applied function variable in application"
        [r|
        f x y = x
        x = f 42
        x
        |]
        (fun [exist "v3", num])
    , exprTestBad
        "applications with too many arguments fail"
        [r|
        f :: a -> a
        f True 12
        |]
    , exprTestBad
        "applications with mismatched types fail (1)"
        [r|
        abs :: Num -> Num
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
        (GeneralTypeError ApplicationOfNonFunction)
        [r|
        f = 5
        g = \x -> f x
        g 12
        |]
    , expectError
        "applications of non-functions should fail (2)"
        (GeneralTypeError ApplicationOfNonFunction)
        [r|
        f = 5
        g = \h -> h 5
        g f
        |]

    -- evaluation within containers
    , expectError
        "arguments to a function are monotypes"
        (GeneralTypeError (SubtypeError num bool "Expect monotype"))
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
        (tuple [num, num])
    , assertGeneralType
        "polymorphism under lambdas (203f8c) (2)"
        [r|
        f :: a -> a
        g = \h -> [h 42, h 1234]
        g f
        |]
        (lst num)

    -- binding
    , assertGeneralType
        "annotated variables without definition are legal"
        [r|
        x :: Num
        export x
        |]
        num
    , assertGeneralType
        "unannotated variables with definition are legal"
        [r|
        x = 42
        x
        |]
        num
    -- , exprTestBad
    --     "unannotated variables without definitions are illegal ('x')"
    --     "x"

    -- parameterized types
    , assertGeneralType
        "parameterized type (n=1)"
        [r|
        xs :: Foo A
        export xs
        |]
        (arr "Foo" [var "A"])
    , assertGeneralType
        "parameterized type (n=2)"
        [r|
        xs :: Foo A B
        export xs
        |]
        (arr "Foo" [var "A", var "B"])
    , assertGeneralType
        "nested parameterized type"
        [r|
        xs :: Foo (Bar A) [B]
        export xs
        |]
        (arr "Foo" [arr "Bar" [var "A"], arr "List" [var "B"]])
    -- , assertTerminalType
    --     "language inference in lists #1"
    --     [r|
    --        bar Cpp :: "float" -> "std::vector<$1>" "float"
    --        bar x = [x]
    --        bar 5
    --     |]
    --     [arrc CppLang "std::vector<$1>" [varc CppLang "float"], lst (var "Num")]
    -- , assertTerminalType
    --     "language inference in lists #2"
    --     [r|
    --        mul :: Num -> Num -> Num
    --        mul Cpp :: "int" -> "int" -> "int"
    --        foo = mul 2
    --        bar Cpp :: "int" -> "std::vector<$1>" "int"
    --        bar x = [foo x, 42]
    --        bar 5
    --     |]
    --     [lst (var "Num"), arrc CppLang "std::vector<$1>" [varc CppLang "int"]]

    -- type signatures and higher-order functions
    , assertGeneralType
        "type signature: identity function"
        [r|
        f :: a -> a
        f 42
        |]
        num
    , assertGeneralType
        "type signature: apply function with primitives"
        [r|
        apply :: (Num -> Bool) -> Num -> Bool
        f :: Num -> Bool
        apply f 42
        |]
        bool
    , assertGeneralType
        "type signature: generic apply function"
        [r|
        apply :: (a->b) -> a -> b
        f :: Num -> Bool
        apply f 42
        |]
        bool
    , assertGeneralType
        "type signature: map"
        [r|
        map :: (a->b) -> [a] -> [b]
        f :: Num -> Bool
        map f [5,2]
        |]
        (lst bool)
    -- , assertGeneralType
    --     "type signature: sqrt with realizations"
    --     "sqrt :: Num -> Num\nsqrt R :: \"numeric\" -> \"numeric\"\nsqrt"
    --     [ fun [num, num]
    --     , fun [varc RLang "numeric", varc RLang "numeric"]]

    -- shadowing
    , assertGeneralType
        "name shadowing in lambda expressions"
        [r|
        f x = (14, x)
        g x f = f x
        g True f
        |]
        (tuple [num, bool])
    , assertGeneralType
        "function passing without shadowing"
        [r|
        f x = (14, x)
        g foo = foo True
        g f
        |]
        (tuple [num, bool])
    , assertGeneralType
        "shadowed qualified type variables (7ffd52a)"
        [r|
        f :: a -> a
        g :: a -> Num
        g f
        |]
        num
    , assertGeneralType
        "non-shadowed qualified type variables (7ffd52a)"
        [r|
        f :: a -> a
        g :: b -> Num
        g f
        |]
        num

    -- lists
    , assertGeneralType "list of primitives" "[1,2,3]" (lst num)
    , assertGeneralType
        "list containing an applied variable"
        [r|
        f :: a -> a
        [53, f 34]
        |]
        (lst num)
      -- NOTE: this test relies on internal renaming implementation
    , assertGeneralType "empty list" "[]" (lst (exist "v0"))
    , assertGeneralType
        "list in function signature and application"
        [r|
        f :: [Num] -> Bool
        f [1]
        |]
        bool
    -- , assertGeneralType
    --     "list in generic function signature and application"
    --     "f :: [a] -> Bool\nf [1]"
    --     [bool]
    -- , exprTestBad "failure on heterogenous list" "[1,2,True]"

    -- tuples
    , assertGeneralType
        "tuple of primitives"
        [r|
        (4.2, True)
        |]
        (tuple [num, bool])
    , assertGeneralType
        "tuple containing an applied variable"
        [r|
        f :: a -> a
        (f 53, True)
        |]
        (tuple [num, bool])
    , assertGeneralType
        "check 2-tuples type signature"
        [r|
        f :: (Num, Str)
        export f
        |]
        (tuple [num, str])
    , assertGeneralType "1-tuples are just for grouping" "f :: (Num)\nexport f" num

    --- FIXME - distinguish between Unit an Null
    -- unit type
    , assertGeneralType
        "unit as input"
        [r|
        f :: () -> Bool
        export f
        |]
        (fun [VarU (TV Nothing "Unit"), bool])

    , assertGeneralType
        "unit as output"
        [r|
        f :: Bool -> ()
        export f
        |]
        (fun [bool, VarU (TV Nothing "Unit")])

    -- FIXME - I really don't like "Unit" being a normal var ...
    -- I am inclined to cast it as the unit type
    , assertGeneralType "empty tuples are of unit type" "f :: ()\nexport f" (var "Unit")

    -- extra space
    , assertGeneralType "leading space" " 42" num
    , assertGeneralType "trailing space" "42 " num

    -- adding signatures to declarations
    , assertGeneralType
        "declaration with a signature (1)"
        [r|
        f :: a -> a
        f x = x
        f 42
        |]
        num
    , assertGeneralType
        "declaration with a signature (2)"
        [r|
        f :: Num -> Bool
        f x = True
        f 42
        |]
        bool
    , assertGeneralType
        "declaration with a signature (3)"
        [r|
        f :: Num -> Bool
        f x = True
        f
        |]
        (fun [num, bool])
    , expectError
        "primitive type mismatch should raise error"
        (GeneralTypeError (SubtypeError num bool "mismatch"))
        [r|
        f :: Num -> Bool
        f x = 9999
        export f"
        |]

    -- -- tags
    -- , exprEqual
    --     "variable tags"
    --     "F :: Int"
    --     "F :: foo:Int"
    -- , exprEqual
    --     "list tags"
    --     "F :: [Int]"
    --     "F :: foo:[Int]"
    -- , exprEqual
    --     "tags on parenthesized types"
    --     "F :: Int"
    --     "F :: f:(Int)"
    -- , exprEqual
    --     "record tags"
    --     "F :: {x::Int, y::Str}"
    --     "F :: foo:{x::Int, y::Str}"
    -- , exprEqual
    --     "nested tags (tuple)"
    --     "F :: (Int, Str)"
    --     "F :: foo:(i:Int, s:Str)"
    -- , exprEqual "nested tags (list)" "F :: [Int]" "F :: xs:[x:Int]"
    -- , exprEqual
    --     "nested tags (record)"
    --     "F :: {x::Int, y::Str}"
    --     "F :: foo:{x::(i:Int), y::Str}"

    -- properties
    , assertGeneralType "property syntax (1)" "f :: Foo => Num\nexport f" num
    , assertGeneralType "property syntax (2)" "f :: Foo bar => Num\nexport f" num
    , assertGeneralType "property syntax (3)" "f :: Foo a, Bar b => Num\nexport f" num
    , assertGeneralType "property syntax (4)" "f :: (Foo a) => Num\nf" num
    , assertGeneralType "property syntax (5)" "f :: (Foo a, Bar b) => Num\nexport f" num

    -- constraints
    , assertGeneralType
        "constraint syntax (1)"
        [r|
           f :: Num where
             ladida
           f
        |]
        num
    , assertGeneralType
        "constraint syntax (2)"
        [r|
           f :: Num where
             first relation
               and more
             second relation
           f
        |]
        num

    -- tests modules
    , assertGeneralType
        "basic Main module"
        [r|
          module Main
            x = [1,2,3]
            export x
        |]
        (lst num)
    , (flip $ assertGeneralType "import/export") (lst num) $
        [r|
          module Foo
            export x
            x = 42
          module Bar
            export f
            f :: a -> [a]
          module Main
            import Foo (x)
            import Bar (f)
            z = f x
            export z
        |]
    , (flip $ assertGeneralType "complex parse (1)") num $
      [r|
         module Foo
           export x
           add :: Num -> Num -> Num
           x = add a y where
             a = 1
             y = add b z where
               b = 42
           z = 19
      |]
    -- , (flip $ assertTerminalType "import/export") [varc RLang "numeric"] $
    --   [r|
    --      module Foo
    --      export x
    --      x = [1,2,3]
    --
    --      module Bar
    --      export f
    --      f R :: ["numeric"] -> "numeric"
    --
    --      module Main
    --      import Foo (x)
    --      import Bar (f)
    --      f x
    --   |]
    --
    -- , (flip $ assertTerminalType "multiple imports") [varc Python3Lang "float", varc RLang "numeric"] $
    --   [r|
    --      module Foo
    --      export f
    --      f py :: ["float"] -> "float"
    --
    --      module Bar
    --      export f
    --      f R :: ["numeric"] -> "numeric"
    --
    --      module Main
    --      import Foo (f)
    --      import Bar (f)
    --      f [1,2,3]
    --   |]
    --
    -- , expectError
    --     "fail on import of non-existing variable"
    --     (BadImport (MVar "Foo") (EV [] "x")) $
    --     [r|
    --        module Foo
    --        export y
    --        y = 42
    --
    --        module Main
    --        import Foo (x)
    --        x
    --     |]
    -- , expectError
    --     "fail on cyclic dependency"
    --     CyclicDependency $
    --     [r|
    --        module Foo
    --        import Bar (y)
    --        export x
    --        x = 42
    --
    --        module Bar
    --        import Foo (x)
    --        export y
    --        y = 88
    --     |]
    -- , expectError "fail on self import"
    --     (SelfImport (MVar "Foo")) $
    --     [r|
    --        module Foo
    --        import Foo (x)
    --        x = 42
    --     |]
    -- , expectError
    --     "fail on import of non-exported variable"
    --     (BadImport (MVar "Foo") (EV [] "x")) $
    --     [r|
    --         module Foo {x = 42}
    --         module Main
    --         import Foo (x)
    --         x
    --     |]
    --
    -- -- test realization integration
    -- , assertTerminalType
    --     "a realization can be defined following general type signature"
    --     [r|
    --        f :: Num -> Num
    --        f r :: "integer" -> "integer"
    --        f 44
    --     |]
    --     [num, varc RLang "integer"]
    -- , assertTerminalType
    --     "realizations can map one general type to multiple specific ones"
    --     [r|
    --        f :: Num -> Num
    --        f r :: "integer" -> "numeric"
    --        f 44
    --     |]
    --     [num, varc RLang "numeric"]
    -- , assertTerminalType
    --     "realizations can map multiple general type to one specific one"
    --     [r|
    --        f :: Num -> Nat
    --        f r :: "integer" -> "integer"
    --        f 44
    --     |]
    --     [var "Nat", varc RLang "integer"]
    -- , assertTerminalType
    --     "multiple realizations for different languages can be defined"
    --     [r|
    --        f :: Num -> Num
    --        f r :: "integer" -> "integer"
    --        f c :: "int" -> "int"
    --        f 44
    --     |]
    --     [num, varc CLang "int", varc RLang "integer"]
    -- , assertTerminalType
    --     "realizations with parameterized variables"
    --     [r|
    --        f :: [Num] -> Num
    --        f r :: "$1" "integer" -> "integer"
    --        f cpp :: "std::vector<$1>" "int" -> "int"
    --        f [44]
    --     |]
    --     [num, varc CppLang "int", varc RLang "integer"]
    -- , assertTerminalType
    --     "realizations can use quoted variables"
    --     [r|
    --        sum :: [Num] -> Num
    --        sum c :: "$1*" "double" -> "double"
    --        sum cpp :: "std::vector<$1>" "double" -> "double"
    --        sum [1,2]
    --     |]
    --     [num, varc CLang "double", varc CppLang "double"]
    -- , assertTerminalType
    --     "the order of general signatures and realizations does not matter (1)"
    --     [r|
    --        f r :: "integer" -> "integer"
    --        f :: Num -> Num
    --        f c :: "int" -> "int"
    --        f 44
    --     |]
    --     [num, varc CLang "int", varc RLang "integer"]
    -- , assertTerminalType
    --     "the order of general signatures and realizations does not matter (2)"
    --     [r|
    --        f r :: "integer" -> "integer"
    --        f c :: "int" -> "int"
    --        f :: Num -> Num
    --        f 44
    --     |]
    --     [num, varc CLang "int", varc RLang "integer"]
    -- , assertTerminalType
    --     "multiple realizations for a single language cannot be defined"
    --     [r|
    --        f r :: A -> B
    --        f r :: C -> D
    --        f 1
    --     |]
    --     [varc RLang "B", varc RLang "D"]
    -- , assertTerminalType
    --     "general signatures are optional"
    --     [r|
    --        f r :: "integer" -> "integer"
    --        f 44
    --     |]
    --     [varc RLang "integer"]
    -- , assertTerminalType
    --     "compositions can have concrete realizations"
    --     [r|
    --        f r :: "integer" -> "integer"
    --        f x = 42
    --        f 44
    --     |]
    --     [varc RLang "integer", num]
    -- , expectError
    --    "arguments number in realizations must equal the general case (1)"
    --     BadRealization $
    --     [r|
    --        f :: Num -> String -> Num
    --        f r :: "integer" -> "integer"
    --        f 44
    --     |]
    -- , expectError
    --      "arguments number in realizations must equal the general case (2)"
    --      BadRealization $
    --      [r|
    --         f :: Num -> Num
    --         f r :: "integer" -> "integer" -> "string"
    --         f 44
    --      |]
    -- , assertTerminalType
    --     "multiple realizations for one type"
    --     [r|
    --        foo :: Num -> Num
    --        foo r :: A -> B
    --        foo c :: C -> D
    --        bar c :: C -> C
    --        foo (bar 1)
    --     |]
    --     [num, varc CLang "D", varc RLang "B"]
    -- , assertTerminalType
    --   "concrete snd: simple test with containers"
    --   [r|
    --      snd :: (a, b) -> b
    --      snd r :: list a b -> b
    --      snd (1, True)
    --   |]
    --   [bool, varc RLang "logical"]
    -- , assertTerminalType
    --   "concrete map: single map, single f"
    --   [r|
    --      map cpp :: (a -> b) -> "std::vector<$1>" a -> "std::vector<$1>" b
    --      f cpp :: "double" -> "double"
    --      map f [1,2]
    --   |]
    --   [arrc CppLang "std::vector<$1>" [varc CppLang "double"]]
    -- , assertTerminalType
    --   "concrete map: multiple maps, single f"
    --   [r|
    --      map :: (a -> b) -> [a] -> [b]
    --      map c :: (a -> b) -> "std::vector<$1>" a -> "std::vector<$1>" b
    --      map r :: (a -> b) -> vector a -> vector b
    --      f c :: "double" -> "double"
    --      map f [1,2]
    --   |]
    --   [ forall ["a"] (arr "List" [var "a"])
    --   , forallc RLang ["a"] (arrc RLang "vector" [varc RLang "a"])
    --   , arrc CLang "std::vector<$1>" [varc CLang "double"]
    --   ]
    -- , assertTerminalType
    --   "infer type signature from concrete functions"
    --   [r|
    --      sqrt :: Num -> Num
    --      sqrt R :: "numeric" -> "numeric"
    --      foo x = sqrt x
    --      sqrt 42
    --   |]
    --   [num, varc RLang "numeric"]
    -- , assertTerminalType
    --   "calls cross-language"
    --   [r|
    --      f R :: A -> B
    --      g Cpp :: B -> C
    --      g (f 4)
    --   |]
    --   [varc CppLang "C"]
    -- , assertTerminalType
    --   "language branching"
    --   [r|
    --      id R :: a -> a
    --      sqrt C :: "double" -> "double"
    --      sqrt R :: "numeric" -> "numeric"
    --      id (sqrt 4)
    --   |]
    --   [varc RLang "numeric"]
    -- , assertTerminalType
    --   "obligate foreign call"
    --   [r|
    --      foo r :: (a -> a) -> a -> a
    --      f c :: "int" -> "int"
    --      foo f 42
    --   |]
    --   [varc RLang "numeric"]
    -- , assertTerminalType
    --   "obligate foreign call - tupled"
    --   [r|
    --      foo r :: (a -> a) -> a -> (a,a)
    --      f c :: "int" -> "int"
    --      foo f 42
    --   |]
    --   [arrc RLang "tuple" [varc RLang "numeric", varc RLang "numeric"]]
    -- , assertTerminalType
    --   "declarations represent all realizations"
    --   [r|
    --      sqrt :: Num -> Num
    --      sqrt r :: "integer" -> "numeric"
    --      foo x = sqrt x
    --      foo
    --   |]
    --   [fun [num, num], fun [varc RLang "integer", varc RLang "numeric"]]
    --
    -- , assertTerminalType
    --   "all internal concrete and general types are right"
    --   [r|
    --      snd :: a -> b -> b
    --      snd Cpp :: a -> b -> b
    --      sqrt :: Num -> Num
    --      sqrt Cpp :: "double" -> "double"
    --      foo x = snd x (sqrt x)
    --      foo
    --   |]
    --   [fun [num, num], fun [varc CppLang "double", varc CppLang "double"]]
    --
    -- , assertTerminalType
    --   "declaration general type signatures are respected"
    --   [r|
    --      sqrt cpp :: "double" -> "double"
    --      sqrt :: a -> a
    --      foo :: Num -> Num
    --      foo x = sqrt x
    --      foo
    --   |]
    --   [fun [num, num], fun [varc CppLang "double", varc CppLang "double"]]
    --
    -- , assertTerminalExprWithAnnot
    --   "all internal concrete and general types are right"
    --   [r|
    --      snd :: a -> b -> b
    --      snd Cpp :: a -> b -> b
    --      sqrt :: Num -> Num
    --      sqrt Cpp :: "double" -> "double"
    --      foo x = snd x (sqrt x)
    --   |]
    --   (Declaration (EV [] "foo")
    --     (AnnE (LamE (EV ["foo"] "x")
    --       (AnnE (AppE
    --         (AnnE (AppE
    --           (AnnE (VarE (EV [] "snd"))
    --             [ fun [num, num, num]
    --             , fun [varc CppLang "double", varc CppLang "double", varc CppLang "double"]])
    --           (AnnE (VarE (EV ["foo"] "x"))
    --             [num,varc CppLang "double"]))
    --           [ FunU num num
    --           , FunU (varc CppLang "double") (varc CppLang "double")])
    --         (AnnE (AppE
    --           (AnnE (VarE (EV [] "sqrt"))
    --             [ FunU num num
    --             , FunU (varc CppLang "double") (varc CppLang "double")])
    --           (AnnE (VarE (EV ["foo"] "x"))
    --             [ num
    --             , varc CppLang "double"]))
    --           [num,varc CppLang "double"]))
    --         [num,varc CppLang "double"]))
    --       [ FunU num num
    --       , FunU (varc CppLang "double") (varc CppLang "double")]))
    --
    -- -- internal
    -- , exprTestFull
    --     "every sub-expression should be annotated in output"
    --     "f :: a -> Bool\nf 42"
    --     "f :: a -> Bool\n(((f :: Num -> Bool) (42 :: Num)) :: Bool)"
    --
    -- -- -- TODO: resurrect to test github issue #7
    -- -- , exprTestFullDec
    -- --     "concrete types should be inferred for declared variables"
    -- --     (T.unlines
    -- --       [ "id :: Num -> Num;"
    -- --       , "id C :: \"int\" -> \"int\";"
    -- --       , "id x = x;"
    -- --       , "y = 40;"
    -- --       , "foo = id y;"
    -- --       ]
    -- --     )
    -- --     [ (EV [] "foo",
    -- --       AnnE (AppE
    -- --           (AnnE (VarE (EV [] "id")) [fun [num, num], fun [varc CLang "int", varc CLang "int"]])
    -- --           (AnnE (VarE (EV [] "y")) [num, varc CLang "int"])
    -- --                                      -- ^ The purpose of this test is to assert that the above
    -- --                                      -- type is defined. As of commit 'c31660a0', `y` was assigned
    -- --                                      -- only the general type Num.
    -- --         )
    -- --       [num, varc CLang "int"]
    -- --       )
    -- --     , (EV [] "id",
    -- --       AnnE (LamE (EV [] "x")
    -- --           (AnnE (VarE (EV [] "x"))
    -- --             [num, varc CLang "int"]))
    -- --         [fun [num, num], fun [varc CLang "int", varc CLang "int"]])
    -- --     , (EV [] "y", AnnE (NumE 40.0) [num])
    -- --     ]
    --
    -- -- default list evaluation of arguments
    -- , assertTerminalType
    --     "can infer multiple argument types"
    --     [r|
    --        ith :: [Num] -> Num -> Num
    --        ith R :: ["numeric"] -> "numeric" -> "numeric"
    --        snd x = ith x 2
    --        snd [1,2,3]
    --     |]
    --     [num, varc RLang "numeric"]
    ]
