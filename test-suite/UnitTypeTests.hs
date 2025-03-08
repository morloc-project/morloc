{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns #-}

module UnitTypeTests
  ( subtypeTests
  , substituteTVarTests
  , unitTypeTests
  , unitValuecheckTests
  , typeOrderTests
  , typeAliasTests
  , packerTests
  , recordAccessTests
  , whereTests
  , orderInvarianceTests
  , whitespaceTests
  ) where

import Morloc.Frontend.Namespace
import Text.RawString.QQ
import Morloc (typecheckFrontend, typecheck)
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes)
import qualified System.Directory as SD
import qualified Morloc.Monad as MM
import qualified Morloc.Typecheck.Internal as MTI

import qualified Data.Text as MT
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

-- get the toplevel general type of a typechecked expression
gtypeof :: AnnoS (Indexed TypeU) f c -> TypeU
gtypeof (AnnoS (Idx _ t) _ _) = t

runFront :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFront code = do
  config <- emptyConfig
  ((x, _), _) <- MM.runMorlocMonad Nothing 0 config defaultValue (typecheckFrontend Nothing (Code code) >>= mapM evaluateAnnoSTypes)
  return x

runMiddle
  :: MT.Text
  -> IO (Either MorlocError
          ( [AnnoS (Indexed Type) One ()]
          , [AnnoS (Indexed Type) One (Indexed Lang)]
          )
        )
runMiddle code = do
  config <- emptyConfig
  ((x, _), _) <- MM.runMorlocMonad Nothing 0 config defaultValue (typecheck Nothing (Code code))
  return x


emptyConfig :: IO Config
emptyConfig = do
  home <- SD.getHomeDirectory
  return $ Config
    { configHome        = home <> "/.morloc"
    , configLibrary     = home <> "/.morloc/src/morloc"
    , configPlane       = "morloclib"
    , configTmpDir      = home <> "/.morloc/tmp"
    , configBuildConfig = home <> "/.morloc/.build-config.yaml"
    , configLangPython3 = "python3"
    , configLangR       = "Rscript"
    , configLangPerl    = "perl"
    }


assertGeneralType :: String -> MT.Text -> TypeU -> TestTree
assertGeneralType msg code t = testCase msg $ do
  result <- runFront code
  case result of
    (Right [x]) -> assertEqual "" t (renameExistentials (gtypeof x))
    (Right _) -> error "Expected exactly one export from Main for assertGeneralType"
    (Left e) -> error $
      "The following error was raised: " <> show e <> "\nin:\n" <> show code

renameExistentials :: TypeU -> TypeU
renameExistentials = snd . f (0 :: Int, Map.empty) where
 f s (VarU v) = (s, VarU v)
 f (i,m) (ExistU v ps rs) =
  case Map.lookup v m of
    (Just v') -> ((i, m), ExistU v' ps rs)
    Nothing ->
      let v' = TV ("e" <> MT.pack (show i))
          i' = i+1
          m' = Map.insert v v' m
          (s', ps') = statefulMap f (i', m') ps 
          (s'', vs') = statefulMap f s' (map snd rs)
      in (s'', ExistU v' ps' (zip (map fst rs) vs'))
 f s (ForallU v t) =
  let (s', t') = f s t
  in (s', ForallU v t') 
 f s (FunU ts t) =
  let (s', ts') = statefulMap f s ts
      (s'', t') = f s' t
  in (s'', FunU ts' t')
 f s (AppU t ts) =
  let (s', t') = f s t
      (s'', ts') = statefulMap f s' ts
  in (s'', AppU t' ts')
 f s (NamU o n vs rs) =
  let (s', ts') = statefulMap f s (map snd rs)
  in (s', NamU o n vs (zip (map fst rs) ts'))


assertSubtypeGamma :: String -> [GammaIndex] -> TypeU -> TypeU -> [GammaIndex] -> TestTree
assertSubtypeGamma msg gs1 a b gs2 = testCase msg $ do
  let g0 = Gamma {gammaCounter = 0, gammaContext = gs1}
  case MTI.subtype Map.empty a b g0 of
    Left e -> error $ show e
    Right (Gamma _ gs2') -> assertEqual "" gs2 gs2'

exprTestBad :: String -> MT.Text -> TestTree
exprTestBad msg code =
  testCase msg $ do
  result  <- runFront code
  case result of
    (Right _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to fail"
    (Left _) -> return ()

valuecheckFail :: String -> MT.Text -> TestTree
valuecheckFail msg code =
  testCase msg $ do
  result  <- runMiddle code
  case result of
    (Right _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to fail"
    (Left _) -> return ()

valuecheckPass :: String -> MT.Text -> TestTree
valuecheckPass msg code =
  testCase msg $ do
  result  <- runMiddle code
  case result of
    (Right _) -> return ()
    (Left _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to pass"


-- FIXME: check that the correct error type is raised, but don't check message
-- (tweaking messages shouldn't break tests)
expectError :: String -> MorlocError -> MT.Text -> TestTree
expectError msg _ code =
  testCase msg $ do
  result <- runFront code
  case result of
    (Right _) -> assertFailure . MT.unpack $ "Expected failure"
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

bool :: TypeU
bool = VarU (TV "Bool")

real :: TypeU
real = VarU (TV "Real")

int :: TypeU
int = VarU (TV "Int")

str :: TypeU
str = VarU (TV "Str")

fun :: [TypeU] -> TypeU
fun [] = error "Cannot infer type of empty list"
fun [t] = FunU [] t
fun ts = FunU (init ts) (last ts)

forall :: [MT.Text] -> TypeU -> TypeU
forall ss t = foldr (ForallU . TV) t ss

exist :: MT.Text -> TypeU
exist v = ExistU (TV v) [] []

var :: MT.Text -> TypeU
var s = VarU (TV s)

arr :: MT.Text -> [TypeU] -> TypeU
arr s = AppU (VarU (TV s))

lst :: TypeU -> TypeU
lst t = arr "List" [t]

tuple :: [TypeU] -> TypeU
tuple ts = AppU v ts
  where
    v = VarU . TV . MT.pack $ "Tuple" ++ show (length ts)

record' :: MT.Text -> [(Key, TypeU)] -> TypeU
record' n = NamU NamRecord (TV n) []

subtypeTests :: TestTree
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
        (record' "Foo" [(Key "K", a), (Key "L", b)]) 
        (record' "Foo" [(Key "K", a), (Key "L", b)]) []
    , assertSubtypeGamma "<a> -| <a> <: A |- <a>:A" [eag] ea a [solvedA a]
    , assertSubtypeGamma "<a> -| A <: <a> |- <a>:A" [eag] a ea [solvedA a]
    , assertSubtypeGamma "<b> -| [A] <: <b> |- <b>:[A]" [ebg] (lst a) (eb) [solvedB (lst a)]
    , assertSubtypeGamma "<a> -| <a> <: [B] |- <a>:[B]" [eag] (lst b) (ea) [solvedA (lst b)]
    , assertSubtypeGamma "<a>, <b> -| <a> <b> <: [C] |- <a>:[C], <b>:C" [eag, ebg]
        (ExistU (TV "x1") [eb] []) (lst c) [solvedA (lst c), solvedB c]
    , assertSubtypeGamma "<a>, <b> -|[C] <: <a> <b> |- <a>:[C], <b>:C" [eag, ebg]
        (lst c) (ExistU (TV "x1") [eb] []) [solvedA (lst c), solvedB c]
    , assertSubtypeGamma "[] -| forall a . a <: A -| a:A" [] (forall ["a"] (var "a")) a [SolvedG (TV "a") a]
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
    ea = ExistU (TV "x1") [] []
    eb = ExistU (TV "x2") [] []
    ec = ExistU (TV "x3") [] []
    ed = ExistU (TV "x4") [] []
    eag = ExistG (TV "x1") [] []
    ebg = ExistG (TV "x2") [] []
    ecg = ExistG (TV "x3") [] []
    edg = ExistG (TV "x4") [] []
    solvedA t = SolvedG (TV "x1") t
    solvedB t = SolvedG (TV "x2") t

substituteTVarTests :: TestTree
substituteTVarTests =
  testGroup
    "test variable substitution"
    [ testEqual "[x/y]Int" (substituteTVar (TV "x") (var "y") int) int
    , testEqual "[y/x]([x] -> x)" (substituteTVar (TV "x") (var "y") (fun [lst (var "x"), var "x"]))
        (fun [lst (var "y"), var "y"]) 
    ]

whitespaceTests :: TestTree
whitespaceTests =
  testGroup
    "Tests whitespace handling for modules"
    [ assertGeneralType
      "module indent == 1 and top indent == module indent"
      "module Foo (y)\nx = 1\ny = 2"
      int
    , assertGeneralType
      "module indent == 1 and top indent > module indent"
      "module Foo (y)\n  x = 1\n  y = 2"
      int
    , assertGeneralType
      "module indent > 1 and top indent > module indent"
      " module Foo (y)\n   x = 1\n   y = 2"
      int
    , assertGeneralType
      "module indent > 1 and top indent = module indent"
      "  module Foo (y)\n  x = 1\n  y = 2"
      int
    -- indenting main
    , assertGeneralType
      "main indent == 1"
      "module main (y)\nx = 1\ny = 2"
      int
    , assertGeneralType
      "main indent > 1"
      "module main (y)\n  x = 1\n  y = 2"
      int
    -- multiple modules
    , assertGeneralType
      "multiple modules at pos 1 with pos > 1 exprs"
      [r|
module Foo (x)
  x = True
module Bar (y)
  import Foo
  y = True
module Main (z)
  import Bar
  z = 1
      |]
      int
    ]

recordAccessTests :: TestTree
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
          record Person = Person {a :: Int, b :: Str}
          bar :: Person
          bar@b
      |]
      str
    , assertGeneralType
      "Access record-returning expression"
      [r|
          record Person = Person {a :: Int, b :: Str}
          bar :: Int -> Person
          (bar 5)@b
      |]
      str
    , assertGeneralType
      "Access into tupled"
      [r|
         record Person = Person {a :: Int, b :: Str}
         bar :: Int -> Person
         ((bar 5)@a, (bar 6)@b)
      |]
      (tuple [int, str])
    ]

packerTests :: TestTree
packerTests =
  testGroup
    "Test building of packer maps"
    [ testEqual "packer test" (1 :: Int) 1 ]

typeAliasTests :: TestTree
typeAliasTests =
  testGroup
    "Test type alias substitutions"
    [ assertGeneralType
        "general type alias"
        [r|
        module main (f)
        type Foo = A
        f :: Foo
        |]
        (var "A")
    , assertGeneralType
        "parameterized generic"
        [r|
        module main (f)
        f m a b :: m (a -> b)
        |]
        (forall ["m___q0", "a___q1", "b___q2"] (arr "m___q0" [fun [var "a___q1", var "b___q2"]]))
    , assertGeneralType
        "non-parametric, general type alias"
        [r|
        module main (f)
        type Foo = A
        f :: Foo -> B
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "deep type substitution: `[Foo] -> B`"
        [r|
        module main (f)
        type Foo = A
        f :: [Foo] -> B
        |]
        (fun [lst (var "A"), var "B"])
    , assertGeneralType
        "deep type substitution: `[Foo] -> Foo`"
        [r|
        module main (f)
        type Foo = A
        f :: [Foo] -> Foo
        |]
        (fun [lst (var "A"), var "A"])
    , assertGeneralType
        "parametric alias, general type alias"
        [r|
        module main (f)
        type (Foo a b) = (a,b)
        f :: Foo X Y -> Z
        |]
        (fun [tuple [var "X", var "Y"], var "Z"])
    , assertGeneralType
        "nested types"
        [r|
           module main (foo)
           type A = B
           type B = C
           foo :: A -> B -> C
        |]
        (fun [var "C", var "C", var "C"])
    , assertGeneralType
        "state is preserved across binding"
        [r|
           module main (f)
           type Foo = A
           g :: Foo -> Int
           f = g
        |]
        (fun [var "A", var "Int"])
    , assertGeneralType
        "state is inherited across binding"
        [r|
           module main (f)
           type Foo = A
           f :: Foo -> Int
           f = g  {- yes, g isn't defined -}
        |]
        (fun [var "A", var "Int"])

    , expectError
        "fail neatly for self-recursive type aliases"
        (SelfRecursiveTypeAlias (TV "A"))
        [r|
           type A = (A,A)
           foo :: A -> B -> C
           foo
        |]
    -- -- TODO: find a way to catch mutually recursive type aliases
    -- , expectError
    --     "fail neatly for mutually-recursive type aliases"
    --     (MutuallyRecursiveTypeAlias [TV "A", TV "B"])
    --     (MT.unlines
    --       [ "type A = B"
    --       , "type B = A"
    --       , "foo :: A -> B -> C"
    --       , "foo"
    --       ]
    --     )
    , expectError
        "fail on too many type aliases parameters"
        (BadTypeAliasParameters (TV "A") 0 1)
        [r|
           type A = B
           foo :: A Int -> C
           foo
        |]
    , expectError
        "fail on too few type aliases parameters"
        (BadTypeAliasParameters (TV "A") 1 0)
        [r|
           type (A a) = (a,a)
           foo :: A -> C
           foo
        |]
    , expectError
        "fail on conflicting types (Int vs Str)"
        (ConflictingTypeAliases int str)
        [r|
           type A = Int
         
           module b (A)
           type A = Str
         
           module main (foo)
           import a (A)
           import b (A)
         
           foo :: A -> A -> A
        |]
    , expectError
        "fail on conflicting types (Map vs List)"
        (ConflictingTypeAliases (forall ["a", "b"] $ lst (tuple [var "a", var "b"]))
                                (forall ["a", "b"] $ arr "Map" [var "a", var "b"]))
        [r|
           module a (A)
           type A a b = Map a b
           
           module b (A)
           type A a b = List (Tuple2 a b)
           
           module main (foo)
           import a (A)
           import b (A)
           
           foo a b :: A a b -> A a b -> A a b
        |]

    -- import tests ---------------------------------------
    , assertGeneralType
        "non-parametric, general type alias, imported"
        [r|
           module M1 (Foo)
             type Foo = A
           module Main (f)
             import M1 (Foo)
             f :: Foo -> B
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "non-parametric, general type alias, reimported"
        [r|
           module M3 (Foo)
             type Foo = A
           module M2 (Foo)
             import M3 (Foo)
           module M1 (Foo)
             import M2 (Foo)
           module Main (f)
             import M1 (Foo)
             f :: Foo -> B
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "non-parametric, general type alias, imported aliased"
        [r|
           module M1 (Foo)
             type Foo = A
           module Main (f)
             import M1 (Foo as Bar)
             f :: Bar -> B
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "non-parametric, general type alias, reimported aliased"
        [r|
           module M3 (Foo1)
             type Foo1 = A

           module M2 (Foo2)
             import M3 (Foo1 as Foo2)

           module M1 (Foo3)
             import M2 (Foo2 as Foo3)

           module Main (f)
             import M1 (Foo3 as Foo4)
             f :: Foo4 -> B
        |]
        (fun [var "A", var "B"])

    , assertGeneralType
        "non-parametric, general type alias, duplicate import"
        [r|
           module M2 (Foo)
             type Foo = A

           module M1 (Foo)
             type Foo = A

           module Main (f)
             import M1 (Foo)
             import M2 (Foo)
             f :: Foo -> B
        |]
        (fun [var "A", var "B"])
    , assertGeneralType
        "parametric alias, general type alias, duplicate import"
        [r|
           module M2 (Foo)
             type (Foo a b) = (a,b)

           module M1 (Foo)
             type (Foo c d) = (c,d)

           module Main (f)
             import M1 (Foo)
             import M2 (Foo)
             f :: Foo X Y -> Z
        |]
        (fun [tuple [var "X", var "Y"], var "Z"])
    ]


whereTests :: TestTree
whereTests =
  testGroup
  "Test of where statements"
  [
      assertGeneralType
        "simple where"
        [r|
            f :: Int
            f = z where
                z = 42
            f
        |]
        int
    , assertGeneralType
        "calling simple where"
        [r|
            inc :: Int -> Int
            f = inc z where
                z = 42
            f
        |]
        int
    , assertGeneralType
        "calling deeper where"
        [r|
            id a :: a -> a
            inc :: Int -> Int
            f = id z where
                z = inc y where
                  y = 42
            f
        |]
        int
  ]


orderInvarianceTests :: TestTree
orderInvarianceTests =
  testGroup
  "Test order invariance"
  [ assertGeneralType
      "definitions work"
      "x = 42\nx"
      int
  , assertGeneralType
      "terms may be defined before they are used"
      "y = 42\nx = y\nx"
      int
  , assertGeneralType
      "long chains of substitution are OK too"
      "z = 42\ny = z\nx = y\nx"
      int
  ]

typeOrderTests :: TestTree
typeOrderTests =
  testGroup
    "Tests of type partial ordering (subtype)"
    [ testFalse
        "Str !< Real"
        (isSubtypeOf str real)
    , testFalse
        "Real !< Str"
        (isSubtypeOf real str)
    , testFalse
        "[Real] !< [Str]"
        (isSubtypeOf (lst real) (lst str))
    , testFalse
        "[Str] !< [Real]"
        (isSubtypeOf (lst str) (lst real))
    , testFalse
        "Str -> Str -> Str !< Real -> Real -> Real"
        (isSubtypeOf (fun [str, str, str]) (fun [real, real, real]))
    , testFalse
        "Real -> Real -> Real !< Str -> Str -> Str"
        (isSubtypeOf (fun [real, real, real]) (fun [str, str, str]))
    , testFalse
        "Str -> Str !< Int -> Int -> Int"
        (isSubtypeOf (fun [str, str]) (fun [int, int, int]))
    , testTrue
        "a <: Int"
        (isSubtypeOf (forall ["a"] (var "a")) int)
    , testFalse
        "Int !< forall a . a"
        (isSubtypeOf int (forall ["a"] (var "a")))
    , testTrue
        "forall a . (Int, a) <: (Int, Str)"
        (isSubtypeOf (forall ["a"] (tuple [int, var "a"])) (tuple [int, str]))
    , testTrue
        "forall a b . (a, b) <: (Int, Str)"
        (isSubtypeOf (forall ["a", "b"] (tuple [var "a", var "b"])) (tuple [int, str]))
    , testTrue
        "forall a . (Int, a) <: forall b . (Int, b)"
        (isSubtypeOf
          (forall ["a"] (tuple [int, var "a"]))
          (forall ["b"] (tuple [int, var "b"])))
    , testTrue
        "forall a . a <: (Int, Str)"
        (isSubtypeOf (forall ["a"] (var "a")) (tuple [int, str]))
    , testTrue
        "forall a . a <: forall a b . (a, b)"
        (isSubtypeOf (forall ["a"] (var "a")) (forall ["a", "b"] (tuple [var "a", var "b"])))
    -- cannot compare
    , testFalse
        "[Int] !< Int"
        (isSubtypeOf (lst int) int)
    , testFalse
        "Int !< [Int]"
        (isSubtypeOf int (lst int))
    -- partial order of types
    , testTrue
        "forall a . [a] <= [Int]"
        ((forall ["a"] (lst (var "a"))) <= (lst (var "a")))
    , testFalse
        "[Int] !< forall a . [a]"
        ((lst (var "a")) <= (forall ["a"] (lst (var "a"))))
    , testTrue
        "forall a . (Int, a) <= (Int, Bool)"
        ((forall ["a"] (tuple [int, var "a"])) <= (tuple [int, bool]))
    , testFalse
        "(Int, Bool) !<= forall a . (Int, a)"
        ((tuple [int, bool]) <= (forall ["a"] (tuple [int, var "a"])))
    , testTrue
        "forall a b . (a, b) <= forall c . (Int, c)"
        ((forall ["a", "b"] (tuple [var "a", var "b"])) <= (forall ["c"] (tuple [int, var "c"])))
    , testFalse
        "forall c . (Int, c) !<= forall a b . (a, b)"
        ((forall ["c"] (tuple [int, var "c"])) <= (forall ["a", "b"] (tuple [var "a", var "b"])))
    , testTrue
        "forall a . a <= forall a b . (a, b)"
        ((forall ["a"] (var "a")) <= (forall ["a", "b"] (tuple [var "a", var "b"])))
    -- test "mostSpecific"
    , testEqual
        "mostSpecific [Int, Str, forall a . a] = [Int, Str]"
        (mostSpecific [int, str, forall ["a"] (var "a")])
        [int, str]
    -- test "mostGeneral"
    , testEqual
        "mostGeneral [Int, Str, forall a . a] = forall a . a"
        (mostGeneral [int, str, forall ["a"] (var "a")])
        [forall ["a"] (var "a")]
    -- test mostSpecificSubtypes
    , testEqual
        "mostSpecificSubtypes: Int against [forall a . a]"
        (mostSpecificSubtypes int [forall ["a"] (var "a")])
        [forall ["a"] (var "a")]
    , testEqual
        "mostSpecificSubtypes: (Int -> Int)"
        (mostSpecificSubtypes (fun [int, int]) [fun [str,str], fun [int, int], forall ["a"] (fun [var "a", var "a"])])
        [fun [int, int]]
    , testEqual
        "mostSpecificSubtypes: empty"
        (mostSpecificSubtypes (fun [str, str, str]) [fun [real, real, real]])
        []

    -- test mostSpecificSubtypes for tuples
    , testEqual
        "mostSpecificSubtypes: tuples"
        (mostSpecificSubtypes
          (tuple [int, int])
          [ forall ["a"] (var "a")
          , forall ["a", "b"] (tuple [var "a", var "b"])
          , forall ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
          ]
        )
        [forall ["a", "b"] (tuple [var "a", var "b"])]

    -- test mostSpecificSubtypes for tuples
    , testEqual
        "mostSpecificSubtypes: with partially generic tuples"
        (mostSpecificSubtypes
          (forall ["a"] (tuple [int, var "a"]))
          [ forall ["a"] (var "a")
          , forall ["a", "b"] (tuple [var "a", var "b"])
          , forall ["a"] (tuple [int, var "a"])
          , forall ["a"] (tuple [int, bool])
          , forall ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
          ]
        )
        [forall ["a"] (tuple [int, var "a"])]
    ]

unitTypeTests :: TestTree
unitTypeTests =
  testGroup
    "Typechecker unit tests"
    -- comments
    [ assertGeneralType "block comments (1)" "{- -} 42" int
    , assertGeneralType "block comments (2)" " {--} 42{-   foo -} " int
    , assertGeneralType "line comments (3)" "-- foo\n 42" int
    -- reals versus integers
    , assertGeneralType "0 is an int" "0" int
    , assertGeneralType "42 is an int" "42" int
    , assertGeneralType "-42 is an int" "-42" int
    , assertGeneralType "big integers are OK" "123456789123456789123456789123456789123456789123456789" int
    , assertGeneralType "big negative integers are OK" "-123456789123456789123456789123456789123456789123456789" int
    , assertGeneralType "0.0 is a real" "0.0" real
    , assertGeneralType "4.2 is a real" "4.2" real
    , assertGeneralType "-4.2 is a real" "-4.2" real
    , assertGeneralType "4e1 is a real (scientific notation is real)" "4e1" real
    , assertGeneralType "-4e1 is a real" "-4e1" real
    , assertGeneralType "-4e-1 is a real" "-4e-1" real
    , assertGeneralType "4.2e3000 is a real" "4.2e3000" real
    , assertGeneralType "irregular scientific notation is OK" "123456789123456789123456789e-3000" real
    , assertGeneralType "reals may be big" "123456789123456789123456789.123456789123456789123456789" real
    -- other primitives
    , assertGeneralType "primitive boolean" "True" bool
    , assertGeneralType "primitive string" "\"this is a string literal\"" str
    , assertGeneralType "primitive integer annotation" "42 :: Int" int
    , assertGeneralType "primitive boolean annotation" "True :: Bool" bool
    , assertGeneralType "primitive double annotation" "4.2 :: Real" real
    , assertGeneralType
        "primitive string annotation"
        "\"this is a string literal\" :: Str"
        str
    , assertGeneralType "primitive declaration" "x = True\n4.2" real
    -- containers
    -- - lists
    , assertGeneralType "list of one primitive" "[1]" (lst int)
    , assertGeneralType "list of many primitives" "[1,2,3]" (lst int)
    , assertGeneralType "list of many containers" "[(True,1),(False,2)]" (lst (tuple [bool, int]))
    -- - tuples
    , assertGeneralType "tuple of primitives" "(1,2,True)" (tuple [int, int, bool])
    , assertGeneralType "tuple with containers" "(1,(2,True))" (tuple [int, tuple [int, bool]])
    -- - records
    , assertGeneralType
        "primitive record statement"
        [r|
        {x=42, y="yolo"}
        |]
        (ExistU (TV "e0") [] [(Key "x", int), (Key "y", str)])
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
        (ExistU (TV "e0") [] [(Key "x", int), (Key "y", str)])
    , assertGeneralType
        "nested records"
        [r|
        {x = 42, y = {bob = 24601, tod = "listen now closely and hear how I've planned it"}}
        |]
        (ExistU (TV "e0") [] [(Key "x", int),(Key "y",ExistU (TV "e1") [] [(Key "bob",int),(Key "tod",str)])])

    , assertGeneralType
        "records with bound variables"
        [r|
        foo a = {x=a, y="yolo"}
        foo 42
        |]
        (ExistU (TV "e0") [] [(Key "x", int),(Key "y", str)])

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
        id a :: a -> a
        id 42
        |]
        int
    , assertGeneralType
        "const signature function"
        [r|
        const a b :: a -> b -> a
        const 42 True
        |]
        int
    , assertGeneralType
        "fst signature function"
        [r|
        fst a b :: (a,b) -> a
        fst (42,True)
        |]
        int
    , assertGeneralType
        "value to list function"
        [r|
        single a :: a -> [a]
        single 42
        |]
        (lst int)
    , assertGeneralType
        "head function"
        [r|
        head a :: [a] -> a
        head [1,2,3]
        |]
        int

    , assertGeneralType
        "make list function"
        [r|
        f a :: a -> [a]
        f 1
        |]
        (lst int)

    , assertGeneralType
        "make list function"
        [r|
        single a :: a -> [a]
        single 1
        |]
        (lst int)

    , assertGeneralType
        "existential application"
        "f 1"
        (exist "e0")

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
        app a b :: (a -> b) -> a -> b
        f a :: a -> [a]
        app f 42
        |]
        (lst int)

    , assertGeneralType
        "app head function"
        [r|
        app a b :: (a -> b) -> a -> b
        f a :: [a] -> a
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
      zip x y z :: (x -> y -> z) -> [x] -> [y] -> [z]
      zip pair [1,2] [True, False]
      |]
      (lst (tuple [int, bool]))

    , assertGeneralType
      "nested identity"
      [r|
      id a :: a -> a
      id (id (id 1))
      |]
      int

    , assertGeneralType
      "head (head [[1]])"
      [r|
      head a :: [a] -> a
      head (head [[42]])
      |]
      int

    , assertGeneralType
      "snd (snd (1,(1,True)))"
      [r|
      snd a b :: (a, b) -> b
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
        map a b :: (a -> b) -> [a] -> [b]
        head a :: [a] -> a
        map head [[1],[1,2,3]]
        |]
        (lst int)

    , assertGeneralType
        "t a -> a"
        [r|
        gify a :: a -> G a
        out f a :: f a -> a
        out (gify 1)
        |]
        int
    , assertGeneralType
        "f a b -> b"
        [r|
        gify a b :: a -> b -> G a b
        snd f a b :: f a b -> b
        snd (gify 1 True)
        |]
        bool 
    , assertGeneralType
        "map id over number list"
        [r|
        map a b :: (a -> b) -> [a] -> [b]
        id a :: a -> a
        map id [1,2,3]
        |]
        (lst int)
    , assertGeneralType
        "map fst over tuple list"
        [r|
        map a b :: (a -> b) -> [a] -> [b]
        fst a b :: (a,b) -> a
        map fst [(1,True),(2,False)]
        |]
        (lst int)
    , assertGeneralType
        "map fstG over (G a b) list"
        [r|
        gify a b :: a -> b -> G a b
        map a b :: (a -> b) -> [a] -> [b]
        fstF f a b :: f a b -> a
        map fstF [gify 1 True, gify 2 False]
        |]
        (lst int)
    , assertGeneralType
        "fmap generic fst over functor"
        [r|
        gify a :: a -> G a
        fmap f a b :: (a -> b) -> f a -> f b
        out f a :: f a -> a
        fmap out (gify [1])
        |]
        (arr "G" [int])
    , assertGeneralType
        "generic parameter reordering"
        [r|
        module m (biz)
        type M a b c = R b a c
        foo a b c :: M a b c -> N b c
        bar a b c :: a -> b -> c -> R a b c
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
        f :: Foo
        |]
        (var "Foo")

    -- lambdas
    , assertGeneralType
        "function with parameterized types"
        [r|
        module main (f)
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
        f a :: a -> a
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
        (GeneralTypeError (SubtypeError int bool "Expect monotype"))
        [r|
        f a :: a -> a
        g = \h -> (h 42, h True)
        g f
        |]
    , assertGeneralType
        "polymorphism under lambdas (203f8c) (1)"
        [r|
        f a :: a -> a
        g = \h -> (h 42, h 1234)
        g f
        |]
        (tuple [int, int])
    , assertGeneralType
        "polymorphism under lambdas (203f8c) (2)"
        [r|
        f a :: a -> a
        g = \h -> [h 42, h 1234]
        g f
        |]
        (lst int)

    -- binding
    , assertGeneralType
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
    -- , exprTestBad
    --     "unannotated variables without definitions are illegal ('x')"
    --     "x"

    -- parameterized types
    , assertGeneralType
        "parameterized type (n=1)"
        [r|
        module main (xs)
        xs :: Foo A
        |]
        (arr "Foo" [var "A"])
    , assertGeneralType
        "parameterized type (n=2)"
        [r|
        module main (xs)
        xs :: Foo A B
        |]
        (arr "Foo" [var "A", var "B"])
    , assertGeneralType
        "nested parameterized type"
        [r|
        module main (xs)
        xs :: Foo (Bar A) [B]
        |]
        (arr "Foo" [arr "Bar" [var "A"], arr "List" [var "B"]])

    -- type signatures and higher-order functions
    , assertGeneralType
        "type signature: identity function"
        [r|
        f a :: a -> a
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
        apply a b :: (a->b) -> a -> b
        f :: Int -> Bool
        apply f 42
        |]
        bool
    , assertGeneralType
        "type signature: map"
        [r|
        map a b :: (a->b) -> [a] -> [b]
        f :: Int -> Bool
        map f [5,2]
        |]
        (lst bool)

    -- shadowing
    , assertGeneralType
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
        f a :: a -> a
        g a :: a -> Int
        g f
        |]
        int
    , assertGeneralType
        "non-shadowed qualified type variables (7ffd52a)"
        [r|
        f a :: a -> a
        g b :: b -> Int
        g f
        |]
        int

    -- lists
    , assertGeneralType "list of primitives" "[1,2,3]" (lst int)
    , assertGeneralType
        "list containing an applied variable"
        [r|
        f a :: a -> a
        [53, f 34]
        |]
        (lst int)
      -- NOTE: this test relies on internal renaming implementation
    , assertGeneralType "empty list" "[]" (lst (exist "e0"))
    , assertGeneralType
        "list in function signature and application"
        [r|
        f :: [Int] -> Bool
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
        (tuple [real, bool])
    , assertGeneralType
        "tuple containing an applied variable"
        [r|
        f a :: a -> a
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

    -- unit type
    , assertGeneralType
        "unit as input"
        [r|
        module main (f)
        f :: () -> Bool
        |]
        (fun [bool])

    , assertGeneralType
        "unit as 2rd input"
        [r|
        module main (f)
        f :: Int -> () -> Bool
        |]
        (fun [int, bool])

    , assertGeneralType
        "unit as output"
        [r|
        module main (f)
        f :: Bool -> ()
        |]
        (fun [bool, VarU (TV "Unit")])

    -- FIXME - I really don't like "Unit" being a normal var ...
    -- I am inclined to cast it as the unit type
    , assertGeneralType "empty tuples are of unit type" "module main (f)\nf :: ()" (var "Unit")

    -- extra space
    , assertGeneralType "leading space" " 42" int
    , assertGeneralType "trailing space" "42 " int

    -- adding signatures to declarations
    , assertGeneralType
        "declaration with a signature (1)"
        [r|
        f a :: a -> a
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
        (GeneralTypeError (SubtypeError int bool "mismatch"))
        [r|
        module main (f)
        f :: Int -> Bool
        f x = 9999
        |]

    , expectError
        "catch infinite recursion of list"
        (GeneralTypeError InfiniteRecursion)
        [r|
        module main (f)
        g a :: [a] -> a
        f a :: a -> a
        f x = g x
        |]
    , expectError
        "catch infinite recursion of tuple"
        (GeneralTypeError InfiniteRecursion)
        [r|
        module main (f)
        g a b :: (a, b) -> a
        f a :: a -> a
        f x = g x
        |]
    , expectError
        "check signatures under supposed identity"
        (GeneralTypeError InfiniteRecursion)
        [r|
        module main (f)
        g a b :: (a -> b) -> a
        f a :: a -> a
        f x = g x
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
    , assertGeneralType "property syntax (1)" "module main (f)\nf :: Foo => Int" int
    , assertGeneralType "property syntax (2)" "module main (f)\nf :: Foo bar => Int" int
    , assertGeneralType "property syntax (3)" "module main (f)\nf :: Foo a, Bar b => Int" int
    , assertGeneralType "property syntax (4)" "module main (f)\nf :: (Foo a) => Int" int
    , assertGeneralType "property syntax (5)" "module main (f)\nf :: (Foo a, Bar b) => Int" int

    -- constraints
    , assertGeneralType
        "constraint syntax (1)"
        [r|
           f :: Int where
             ladida
           f
        |]
        int
    , assertGeneralType
        "constraint syntax (2)"
        [r|
           f :: Int where
             first relation
               and more
             second relation
           f
        |]
        int

    -- tests modules
    , assertGeneralType
        "basic Main module"
        [r|
          module Main(x)
          x = [1,2,3]
        |]
        (lst int)
    , (flip $ assertGeneralType "import/export") (lst int) $
        [r|
          module Foo (x)
            x = 42
          module Bar (f)
            f a :: a -> [a]
          module Main (z)
            import Foo (x)
            import Bar (f)
            z = f x
        |]
    , (flip $ assertGeneralType "complex parse (1)") int $
      [r|
         module Foo (x)
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
  testGroup
    "Valuechecker unit tests"
    [ valuecheckFail "unequal primitives fail"
    -- primitives
      [r|
         module foo (x)
           x = 1
           x = 2
      |]
    , valuecheckPass "equal primitives pass"
      [r|
         module foo (x)
           x = 1
           x = 1
      |]
    -- containers
    ,  valuecheckFail "lists with unequal values fail"
      [r|
         module foo (x)
           x = [1,3]
           x = [1,2]
      |]
    ,  valuecheckFail "lists of unequal length fail"
      [r|
         module foo (x)
           x = [1]
           x = [1,2]
      |]
    ,  valuecheckPass "identical lists pass"
      [r|
         module foo (x)
           x = [1,2]
           x = [1,2]
      |]
    -- bound terms in simple expressions
    ,  valuecheckFail "argument constraints"
      [r|
         module foo (f)
           f x y = x
           f a b = b
      |]
    ,  valuecheckFail "lambda var mismatches"
      [r|
         module foo (f)
           f x y = [x,y]
           f a b = [b,a]
      |]
    ,  valuecheckPass "identical lambda passes"
      [r|
         module foo (f)
           f x y = [x,y]
           f a b = [a,b]
      |]
    -- comparisons of simple and non-simple
    ,  valuecheckFail "constrained values fail"
      [r|
         module foo (x)
           source Py ("sum")
           sum :: [Int] -> Int
           x = sum [1, 2]
           x = 3
      |]
    ,  valuecheckFail "unequal types"
      [r|
         module foo (f)
           source Py ("sum")
           sum :: [Int] -> Int
           f xs = [1, sum xs]
           f xs = [2, sum xs]
      |]
    ]
