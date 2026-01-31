{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module UnitTypeTests
  ( subtypeTests
  , substituteTVarTests
  , unitTypeTests
  , unitValuecheckTests
  , typeOrderTests
  , typeAliasTests
  , packerTests
  , whereTests
  , orderInvarianceTests
  , whitespaceTests
  , infixOperatorTests
  ) where

import Morloc (typecheck, typecheckFrontend)
import Morloc.Frontend.Namespace
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes)
import qualified Morloc.Monad as MM
import qualified Morloc.Typecheck.Internal as MTI
import qualified System.Directory as SD
import Text.RawString.QQ

import qualified Data.Map as Map
import qualified Data.Text as MT
import Test.Tasty (TestTree, localOption, testGroup, mkTimeout)
import Test.Tasty.HUnit

-- get the toplevel general type of a typechecked expression
gtypeof :: AnnoS (Indexed TypeU) f c -> TypeU
gtypeof (AnnoS (Idx _ t) _ _) = t

runFront :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFront code = do
  config <- emptyConfig
  ((x, _), _) <-
    MM.runMorlocMonad
      Nothing
      0
      config
      defaultValue
      (typecheckFrontend Nothing (Code code) >>= mapM evaluateAnnoSTypes)
  return x

runMiddle ::
  MT.Text ->
  IO
    ( Either
        MorlocError
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
  return $
    Config
      { configHome = home <> "/.local/share/morloc"
      , configLibrary = home <> "/.local/share/src/morloc"
      , configPlane = "default"
      , configPlaneCore = "morloclib"
      , configTmpDir = home <> "/.morloc/tmp"
      , configBuildConfig = home <> "/.morloc/.build-config.yaml"
      , configLangPython3 = "python3"
      , configLangR = "Rscript"
      }

assertGeneralType :: String -> MT.Text -> TypeU -> TestTree
assertGeneralType msg code t = testCase msg $ do
  result <- runFront code
  case result of
    (Right [x]) -> assertEqual "" (closeExistentials t) (closeExistentials . renameExistentials . gtypeof $ x)
    (Right _) -> error "Expected exactly one export from main for assertGeneralType"
    (Left e) ->
      error $
        "The following error was raised: " <> show e <> "\nin:\n" <> show code

renameExistentials :: TypeU -> TypeU
renameExistentials = snd . f (0 :: Int, Map.empty)
  where
    f s (VarU v) = (s, VarU v)
    f (i, m) (ExistU v (ps, pc) (rs, rc)) =
      case Map.lookup v m of
        (Just v') -> ((i, m), ExistU v' (ps, pc) (rs, rc))
        Nothing ->
          let v' = TV ("e" <> MT.pack (show i))
              i' = i + 1
              m' = Map.insert v v' m
              (s', ps') = statefulMap f (i', m') ps
              (s'', vs') = statefulMap f s' (map snd rs)
           in (s'', ExistU v' (ps', pc) (zip (map fst rs) vs', rc))
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

closeExistentials :: TypeU -> TypeU
closeExistentials = f
  where
    f (ExistU v (ts, _) (rs, _)) = ExistU v (map f ts, Closed) (map (second f) rs, Closed)
    f t@(VarU _) = t
    f (ForallU v t) = ForallU v (f t)
    f (FunU ts t) = FunU (map f ts) (f t)
    f (AppU t ts) = AppU (f t) (map f ts)
    f (NamU o v ts rs) = NamU o v (map f ts) (map (second f) rs)

assertSubtypeGamma :: String -> [GammaIndex] -> TypeU -> TypeU -> [GammaIndex] -> TestTree
assertSubtypeGamma msg gs1 a b gs2 = testCase msg $ do
  let g0 = Gamma {gammaCounter = 0, gammaContext = gs1}
  case MTI.subtype Map.empty a b g0 of
    Left e -> error $ show e
    Right (Gamma _ gs2') -> assertEqual "" gs2 gs2'

exprTestBad :: String -> MT.Text -> TestTree
exprTestBad msg code =
  testCase msg $ do
    result <- runFront code
    case result of
      (Right _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to fail"
      (Left _) -> return ()

valuecheckFail :: String -> MT.Text -> TestTree
valuecheckFail msg code =
  testCase msg $ do
    result <- runMiddle code
    case result of
      (Right _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to fail"
      (Left _) -> return ()

valuecheckPass :: String -> MT.Text -> TestTree
valuecheckPass msg code =
  testCase msg $ do
    result <- runMiddle code
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

forallu :: [MT.Text] -> TypeU -> TypeU
forallu ss t = foldr (ForallU . TV) t ss

exist :: MT.Text -> TypeU
exist e = ExistU (TV e) ([], Open) ([], Open)

tuple :: [TypeU] -> TypeU
tuple xs = AppU (VarU (TV "List")) xs

lst :: TypeU -> TypeU
lst = AppU (VarU (TV "List")) . (: [])

var :: MT.Text -> TypeU
var = VarU . TV

vars :: [MT.Text] -> [TypeU]
vars = map var

unitTypeTests :: TestTree
unitTypeTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "Unit type tests"
    [ assertGeneralType
        "export int literal"
        "x = 5\nx"
        int
    , assertGeneralType
        "export real literal"
        "x = 5.5\nx"
        real
    , assertGeneralType
        "export string"
        [r|x = "yolo"\nx|]
        str
    , assertGeneralType
        "export bool"
        [r|x = True\nx|]
        bool
    , assertGeneralType
        "export list"
        [r|x = [True]\nx|]
        (lst bool)
    , assertGeneralType
        "export list of int (concrete)"
        [r|
          xs :: [Int]
          xs = [1,2,3]
          xs
        |]
        (lst int)
    , assertGeneralType
        "export list of int (inferred)"
        [r|x = [1,2,3]\nx|]
        (lst int)
    , assertGeneralType
        "export list of reals (inferred)"
        [r|x = [1.1,2.2,3.3]\nx|]
        (lst real)
    , assertGeneralType
        "export tuple"
        [r|x = [1.1,"hi",True]\nx|]
        (AppU (var "[]") [real, str, bool])
    , assertGeneralType
        "export simple lambda"
        [r|x = (\x -> 5)\nx|]
        (forallu ["a"] (fun [var "a", int]))
    , assertGeneralType
        "export annotated simple lambda"
        [r|
          x :: Int -> Int
          x = (\x -> 5)
          x
        |]
        (fun [int, int])
    , assertGeneralType
        "export annotated generic lambda"
        [r|
          x :: a -> Int
          x = (\x -> 5)
          x
        |]
        (forallu ["a"] (fun [var "a", int]))
    , assertGeneralType
        "export annotated binary lambda"
        [r|
          x :: a -> b -> a
          x = (\x -> (\y -> x))
          x
        |]
        (forallu ["a", "b"] (fun [var "a", var "b", var "a"]))
    , assertGeneralType
        "lambda id"
        [r|
          x :: a -> a
          x = (\x -> x)
          x
        |]
        (forallu ["a"] (fun [var "a", var "a"]))
    , assertGeneralType
        "lambda const"
        [r|
          x :: a -> b -> a
          x = (\x -> (\y -> x))
          x
        |]
        (forallu ["a", "b"] (fun [var "a", var "b", var "a"]))
    , assertGeneralType
        "tuple literal"
        [r|x = [1, "hi"]\nx|]
        (tuple [int, str])
    , assertGeneralType
        "list literal (all same)"
        [r|x = [1, 2, 3]\nx|]
        (lst int)
    , assertGeneralType
        "record"
        [r|x = <a=1, b="hi">\nx|]
        (NamU NamRecord (TV "{}") [] [(Key "a", int), (Key "b", str)])
    , assertGeneralType
        "empty list of int"
        [r|
          xs :: [Int]
          xs = []
          xs
        |]
        (lst int)
    , assertGeneralType
        "empty list of real"
        [r|
          xs :: [Real]
          xs = []
          xs
        |]
        (lst real)
    , assertGeneralType
        "empty list of Str"
        [r|
          xs :: [Str]
          xs = []
          xs
        |]
        (lst str)
    ]

substituteTVarTests :: TestTree
substituteTVarTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "substituteTVar"
    [ testEqual
        "int is unchanged"
        int
        (substituteTVar (TV "a") (var "b") int)
    , testEqual
        "a is unchanged when a is not given"
        (var "a")
        (substituteTVar (TV "b") (var "c") (var "a"))
    , testEqual
        "identity substitution"
        (var "a")
        (substituteTVar (TV "a") (var "a") (var "a"))
    , testEqual
        "substitute the argument position"
        (fun [int, str])
        (substituteTVar (TV "a") int (fun [var "a", str]))
    , testEqual
        "substitute the result position"
        (fun [str, int])
        (substituteTVar (TV "a") int (fun [str, var "a"]))
    , testEqual
        "substitute function argument and result"
        (fun [int, int])
        (substituteTVar (TV "a") int (fun [var "a", var "a"]))
    , testEqual
        "substitute an inner type variable"
        (lst int)
        (substituteTVar (TV "a") int (lst (var "a")))
    , testEqual
        "substitute a tuple argument"
        (tuple [int, int])
        (substituteTVar (TV "a") int (tuple [var "a", var "a"]))
    , testEqual
        "do not change shadowed var in forall"
        (forallu ["a"] (fun [var "a", var "a"]))
        (substituteTVar (TV "a") int (forallu ["a"] (fun [var "a", var "a"])))
    , testEqual
        "do not change shadowed var in forall 2"
        (forallu ["b"] (fun [int, var "b"]))
        (substituteTVar (TV "a") int (forallu ["b"] (fun [var "a", var "b"])))
    ]

subtypeTests :: TestTree
subtypeTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "Subtyping tests"
    [ assertSubtypeGamma
        "forall r . r ~ r"
        []
        (var "r")
        (var "r")
        []
    , assertSubtypeGamma
        "Int ~ Int"
        []
        int
        int
        []
    ]

unitValuecheckTests :: TestTree
unitValuecheckTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "Unit valuecheck tests"
    [ valuecheckPass
        "Two identical definitions"
        [r|
         module foo (x)
           f :: Int -> Int
           f x = x
           x = 42
           x = 42
      |]
    , valuecheckPass
        "Two identical lambda defs"
        [r|
         module foo (x)
           id :: a -> a
           id x = x
           f y = id y
           f z = id z
           x = 42
      |]
    , valuecheckPass
        "two identical lambdas with wheres"
        [r|
         module foo (x)
           id :: a -> a
           id x = x
           f y = id y where
             x :: Int
             x = 42
           f z = id z where
             y :: Int
             y = 42
           x = 42
      |]
    , valuecheckFail
        "distinct values"
        [r|
         module foo (x)
           x = 41
           x = 42
      |]
    , valuecheckFail
        "distinct functions"
        [r|
         module foo (x)
           f y = y
           f y = 1
           x = 42
      |]
    , valuecheckPass
        "identical lambda passes"
        [r|
         module foo (f)
           f x y = [x,y]
           f a b = [a,b]
      |]
    , -- comparisons of simple and non-simple
      valuecheckFail
        "constrained values fail"
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
    ]

typeAliasTests :: TestTree
typeAliasTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "Type alias tests"
    [ assertGeneralType
        "simple int alias"
        [r|
          type Id = Int
          x :: Id
          x = 5
          x
        |]
        int
    , assertGeneralType
        "generic type alias"
        [r|
          type Foo a = a
          x :: Foo Int
          x = 5
          x
        |]
        int
    ]

packerTests :: TestTree
packerTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "record packer tests"
    [ assertGeneralType
        "simple packer"
        [r|
          x = .<a=5>
          x
        |]
        (forallu ["r"] (fun [var "r", NamU NamRecord (TV "{}") [var "r"] [(Key "a", int)]]))
    , assertGeneralType
        "simple unpacker"
        [r|
          x = .a
          x
        |]
        (forallu ["a", "r"] (fun [NamU NamRecord (TV "{}") [var "r"] [(Key "a", var "a")], var "a"]))
    ]

whereTests :: TestTree
whereTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "where tests"
    [ assertGeneralType
        "simple where"
        [r|
            x = y where
                y = 42
            x
        |]
        int
    , assertGeneralType
        "simple where, reverse order"
        [r|
            x = y where
                z = 42
                y = z
            x
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
  localOption (mkTimeout 1000000) $  -- 1 second timeout
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
  localOption (mkTimeout 1000000) $  -- 1 second timeout
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
        (isSubtypeOf (forallu ["a"] (var "a")) int)
    , testFalse
        "Int !< forall a . a"
        (isSubtypeOf int (forallu ["a"] (var "a")))
    , testTrue
        "forall a . (Int, a) <: (Int, Str)"
        (isSubtypeOf (forallu ["a"] (tuple [int, var "a"])) (tuple [int, str]))
    , testTrue
        "forall a b . (a, b) <: (Int, Str)"
        (isSubtypeOf (forallu ["a", "b"] (tuple [var "a", var "b"])) (tuple [int, str]))
    , testTrue
        "forall a . (Int, a) <: forall b . (Int, b)"
        ( isSubtypeOf
            (forallu ["a"] (tuple [int, var "a"]))
            (forallu ["b"] (tuple [int, var "b"]))
        )
    , testTrue
        "forall a . a <: (Int, Str)"
        (isSubtypeOf (forallu ["a"] (var "a")) (tuple [int, str]))
    , testTrue
        "forall a . a <: forall a b . (a, b)"
        (isSubtypeOf (forallu ["a"] (var "a")) (forallu ["a", "b"] (tuple [var "a", var "b"])))
    , -- cannot compare
      testFalse
        "[Int] !< Int"
        (isSubtypeOf (lst int) int)
    , testFalse
        "Int !< [Int]"
        (isSubtypeOf int (lst int))
    , -- partial order of types
      testTrue
        "forall a . a <: Int"
        (isSubtypeOf (forallu ["a"] (var "a")) int)
    , testTrue
        "forall a . a <: Int -> Int"
        (isSubtypeOf (forallu ["a"] (var "a")) (fun [int, int]))
    , testFalse
        "Int !< forall a . a"
        (isSubtypeOf int (forallu ["a"] (var "a")))
    ]

whitespaceTests :: TestTree
whitespaceTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout
    testGroup
      "Whitespace tests"
    [ assertGeneralType
        "tabs and spaces in assignments"
        "x = 42\nx"
        int
    , assertGeneralType
        "newlines in expressions"
        [r|
          x = [1,
               2,
               3]
          x
        |]
        (lst int)
    ]

-- | Tests for infix operator functionality
-- All tests have a 1-second timeout to prevent infinite loops
infixOperatorTests :: TestTree
infixOperatorTests =
  localOption (mkTimeout 1000000) $  -- 1 second timeout in microseconds
    testGroup
      "Infix operator tests"
    [ -- Basic precedence tests
      assertGeneralType
        "default precedence: multiplication before addition"
        [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Int
          (+) x y = x
          (*) :: Int -> Int -> Int
          (*) x y = y
          z = 1 + 2 * 3
          z
        |]
        int
    , assertGeneralType
        "custom precedence: higher binds tighter"
        [r|
          infixl 3 #
          infixl 8 @
          (#) :: Int -> Int -> Int
          (#) x y = x
          (@) :: Int -> Int -> Int
          (@) x y = y
          x = 1 # 2 @ 3
          x
        |]
        int
    , -- Associativity tests
      assertGeneralType
        "left associative operators"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2 + 3
          x
        |]
        int
    , assertGeneralType
        "right associative operators"
        [r|
          infixr 5 ++
          (++) :: [Int] -> [Int] -> [Int]
          (++) xs ys = xs
          x = [1] ++ [2] ++ [3]
          x
        |]
        (lst int)
    , -- Operators in prefix position
      assertGeneralType
        "operator used prefix"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = (+) 1 2
          x
        |]
        int
    , assertGeneralType
        "operator in lambda"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          f :: Int -> Int -> Int
          f = \x y -> x + y
          f
        |]
        (fun [int, int, int])
    , -- Default precedence tests
      assertGeneralType
        "default * has precedence 7"
        [r|
          infixl 6 +
          (*) :: Int -> Int -> Int
          (*) x y = y
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2 * 3
          x
        |]
        int
    , assertGeneralType
        "default + has precedence 6"
        [r|
          infixl 7 *
          (*) :: Int -> Int -> Int
          (*) x y = y
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2 * 3
          x
        |]
        int
    , -- Multiple operators in one declaration
      assertGeneralType
        "multiple operators same fixity"
        [r|
          infixl 6 +, -
          (+) :: Int -> Int -> Int
          (+) x y = x
          (-) :: Int -> Int -> Int
          (-) x y = x
          x = 1 + 2 - 3
          x
        |]
        int
    , -- Polymorphic operators
      assertGeneralType
        "polymorphic operator"
        [r|
          infixl 6 +
          (+) :: a -> a -> a
          (+) x y = x
          x = 1
          x
        |]
        int
    , assertGeneralType
        "polymorphic list append"
        [r|
          infixl 6 ++
          (++) :: [a] -> [a] -> [a]
          (++) xs ys = xs
          x = [1] ++ [2]
          x
        |]
        (lst int)
    , -- Complex expressions
      assertGeneralType
        "nested operations with parens"
        [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Int
          (+) x y = x
          (*) :: Int -> Int -> Int
          (*) x y = y
          x = (1 + 2) * (3 + 4)
          x
        |]
        int
    , -- Operators in different contexts
      assertGeneralType
        "operator in where clause"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = y + z where
            y = 1
            z = 2
          x
        |]
        int
    , assertGeneralType
        "operator in list"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          xs = [1 + 2, 3 + 4]
          xs
        |]
        (lst int)
    , assertGeneralType
        "operator in tuple"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = (1 + 2, "hi")
          x
        |]
        (AppU (VarU (TV "Tuple2")) [int, str])
    , -- Edge cases
      assertGeneralType
        "operator precedence 0 (lowest)"
        [r|
          infixr 0 $
          ($) :: (Int -> Int) -> Int -> Int
          ($) f x = f x
          g :: Int -> Int
          x = g $ 5
          x
        |]
        int
    , assertGeneralType
        "operator precedence 9 (highest)"
        [r|
          infixl 9 !!!
          (!!!) :: Int -> Int -> Int
          (!!!) x y = x
          x = 1 !!! 2
          x
        |]
        int
    , -- Operators with both parens and bare syntax in fixity decls
      assertGeneralType
        "fixity with parentheses"
        [r|
          infixl 6 (+)
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2
          x
        |]
        int
    , assertGeneralType
        "fixity without parentheses"
        [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2
          x
        |]
        int
    ]
