{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Subtyping
  ( subtypeTests
  , substituteTVarTests
  , typeOrderTests
  , typeRenderParenTests
  ) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc (pretty, render)
import qualified Morloc.Typecheck.Internal as MTI

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as MT
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Types.Common

subtypeTests :: TestTree
subtypeTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test subtype within context"
      [ -- basic general cases
        assertSubtypeGamma "G -| A <: A |- G" [] a a []
      , assertSubtypeIndexed
          "articulating a nested function type keeps every existential at its own slot"
          [eag]
          ea
          (fun [fun [a, b], a, b])
      , assertArticulatedInOrder
          "articulating at a band top with no free slots respreads the band"
          [ (MTI.slotSpacing, ExistG (TV "x1") ([], Open) ([], Open))
          , (MTI.slotSpacing - 1, ExistG (TV "y1") ([], Open) ([], Open))
          , (MTI.slotSpacing - 2, ExistG (TV "y2") ([], Open) ([], Open))
          , (0, ExistG (TV "x2") ([], Open) ([], Open))
          ]
          (TV "x1")
          [TV "x2", TV "y2", TV "y1"]
      , assertArticulatedInOrder
          "articulating inside a band with no free slots respreads the band"
          [ (MTI.slotSpacing, ExistG (TV "x0") ([], Open) ([], Open))
          , (MTI.slotSpacing - 1, ExistG (TV "x1") ([], Open) ([], Open))
          , (MTI.slotSpacing - 2, ExistG (TV "y1") ([], Open) ([], Open))
          , (0, ExistG (TV "x2") ([], Open) ([], Open))
          ]
          (TV "x1")
          [TV "x2", TV "y1"]
      , testCase "a trim at a saved slot drops the extras of entries added after it" $ do
          let g0 = listToGamma [eag]
              saved = gammaSlot g0
              g1 = g0 MTI.+> ExistG (TV "z") ([], Open) ([], Open)
          case MTI.subtype Map.empty (exist "z") (fun [a, b]) g1 of
            Left e -> assertFailure (show e)
            Right g2 -> do
              let g3 = MTI.gammaTrimAfter saved g2
              assertEqual "unsolved entries" [TV "x1"] (Map.keys (gammaExist g3))
              assertEqual "context entries" 1 (IntMap.size (gammaContext g3))
      , assertSubtypeIndexed
          "articulating a doubly nested function type keeps every existential at its own slot"
          [eag]
          ea
          (fun [fun [fun [a, b], a, b], fun [a, b], a, b])
      , assertSubtypeGamma "<a>, <b> -| <a> <: <b> |- <a>:<b>, <b>" [eag, ebg] ea eb [solvedA eb, ebg]
      , assertSubtypeGamma "<a>, <b> -| <b> <: <a> |- <a>:<b>, <b>" [eag, ebg] ea eb [solvedA eb, ebg]
      , assertSubtypeGamma "G -| (A -> B) <: (A -> B) |- G" [] (fun [a, b]) (fun [a, b]) []
      , assertSubtypeGamma "G -| [A] <: [A] |- G" [] (lst a) (lst a) []
      , assertSubtypeGamma
          "G -| {K :: a, L :: b} <: {K :: a, L :: b}"
          []
          (record' "Foo" [(Key "K", a), (Key "L", b)])
          (record' "Foo" [(Key "K", a), (Key "L", b)])
          []
      , assertSubtypeGamma "<a> -| <a> <: A |- <a>:A" [eag] ea a [solvedA a]
      , assertSubtypeGamma "<a> -| A <: <a> |- <a>:A" [eag] a ea [solvedA a]
      , assertSubtypeGamma "<b> -| [A] <: <b> |- <b>:[A]" [ebg] (lst a) (eb) [solvedB (lst a)]
      , assertSubtypeGamma "<a> -| <a> <: [B] |- <a>:[B]" [eag] (lst b) (ea) [solvedA (lst b)]
      , assertSubtypeGamma
          "<a>, <b> -| <a> <b> <: [C] |- <a>:[C], <b>:C"
          [eag, ebg]
          (existP "x1" [eb] [])
          (lst c)
          [solvedA (lst c), solvedB c]
      , assertSubtypeGamma
          "<a>, <b> -|[C] <: <a> <b> |- <a>:[C], <b>:C"
          [eag, ebg]
          (lst c)
          (existP "x1" [eb] [])
          [solvedA (lst c), solvedB c]
      , assertSubtypeGamma
          "[] -| forall a . a <: A -| a:A"
          []
          (forallu ["a"] (var "a"))
          a
          [SolvedG (TV "a") a]
      , -- nested types
        assertSubtypeGamma "<b> -| [A] <: [<b>] |- <b>:A" [ebg] (lst a) (lst eb) [solvedB a]
      , assertSubtypeGamma "<a> -| [<a>] <: [B] |- <a>:B" [eag] (lst b) (lst ea) [solvedA b]
      , assertSubtypeGamma
          "<a>, <b> -| (A, B) <: (<a>, <b>) |- <a>:A, <b>:B"
          [eag, ebg]
          (tuple [a, b])
          (tuple [ea, eb])
          [solvedA a, solvedB b]
      , assertSubtypeGamma
          "<a>, <b> -| (<a>, <b>) <: (A, B) |- <a>:A, <b>:B"
          [eag, ebg]
          (tuple [ea, eb])
          (tuple [a, b])
          [solvedA a, solvedB b]
      , assertSubtypeGamma
          "<a>, <b>, <c>, <d> -| (<a>, <b>) <: (<c>, <d>) -| <a>:<c>, <b>:<d>, <c>, <d>"
          [eag, ebg, ecg, edg]
          (tuple [ea, eb])
          (tuple [ec, ed])
          [solvedA ec, solvedB ed, ecg, edg]
      ]
  where
    a = var "A"
    b = var "B"
    c = var "C"
    ea = exist "x1"
    eb = exist "x2"
    ec = exist "x3"
    ed = exist "x4"
    eag = ExistG (TV "x1") ([], Open) ([], Open)
    ebg = ExistG (TV "x2") ([], Open) ([], Open)
    ecg = ExistG (TV "x3") ([], Open) ([], Open)
    edg = ExistG (TV "x4") ([], Open) ([], Open)
    solvedA t = SolvedG (TV "x1") t
    solvedB t = SolvedG (TV "x2") t

substituteTVarTests :: TestTree
substituteTVarTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "test variable substitution"
      [ testEqual "[x/y]Int" (substituteTVar (TV "x") (var "y") int) int
      , testEqual
          "[y/x]([x] -> x)"
          (substituteTVar (TV "x") (var "y") (fun [lst (var "x"), var "x"]))
          (fun [lst (var "y"), var "y"])
      ]

typeOrderTests :: TestTree
typeOrderTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Tests of type partial ordering (subtype)"
      [ testTrue
          "Int -> (Str -> Real) == Int -> Str -> Real (APP-1)"
          (alphaEq (FunU [int] (FunU [str] real)) (FunU [int, str] real))
      , testTrue
          "Int -> (Str -> Real) <: Int -> Str -> Real (APP-1)"
          (isSubtypeOf (FunU [int] (FunU [str] real)) (FunU [int, str] real))
      , testFalse
          "a suspension of Int -> Int is not Int -> Int"
          (alphaEq (FunU [] (FunU [int] int)) (FunU [int] int))
      , testFalse
          "a suspension of Int -> Int !< Int -> Int"
          (isSubtypeOf (FunU [] (FunU [int] int)) (FunU [int] int))
      , testFalse
          "Int -> Int !< a suspension of Int -> Int"
          (isSubtypeOf (FunU [int] int) (FunU [] (FunU [int] int)))
      , testTrue
          "typecheck: Int -> (Str -> Real) <: Int -> Str -> Real (APP-1)"
          (MTI.isSubtypeOf2 Map.empty (FunU [int] (FunU [str] real)) (FunU [int, str] real))
      , testFalse
          "typecheck: a suspension of Int -> Int !< Int -> Int"
          (MTI.isSubtypeOf2 Map.empty (FunU [] (FunU [int] int)) (FunU [int] int))
      , testFalse
          "typecheck: Int -> Int !< a suspension of Int -> Int"
          (MTI.isSubtypeOf2 Map.empty (FunU [int] int) (FunU [] (FunU [int] int)))
      , testFalse
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
          "forall a . [a] <= [Int]"
          ((forallu ["a"] (lst (var "a"))) <= (lst (var "a")))
      , testFalse
          "[Int] !< forall a . [a]"
          ((lst (var "a")) <= (forallu ["a"] (lst (var "a"))))
      , testTrue
          "forall a . (Int, a) <= (Int, Bool)"
          ((forallu ["a"] (tuple [int, var "a"])) <= (tuple [int, bool]))
      , testFalse
          "(Int, Bool) !<= forall a . (Int, a)"
          ((tuple [int, bool]) <= (forallu ["a"] (tuple [int, var "a"])))
      , testTrue
          "forall a b . (a, b) <= forall c . (Int, c)"
          ((forallu ["a", "b"] (tuple [var "a", var "b"])) <= (forallu ["c"] (tuple [int, var "c"])))
      , testFalse
          "forall c . (Int, c) !<= forall a b . (a, b)"
          ((forallu ["c"] (tuple [int, var "c"])) <= (forallu ["a", "b"] (tuple [var "a", var "b"])))
      , testTrue
          "forall a . a <= forall a b . (a, b)"
          ((forallu ["a"] (var "a")) <= (forallu ["a", "b"] (tuple [var "a", var "b"])))
      , -- test "mostSpecific"
        testEqual
          "mostSpecific [Int, Str, forall a . a] = [Int, Str]"
          (mostSpecific [int, str, forallu ["a"] (var "a")])
          [int, str]
      , -- test "mostGeneral"
        testEqual
          "mostGeneral [Int, Str, forall a . a] = forall a . a"
          (mostGeneral [int, str, forallu ["a"] (var "a")])
          [forallu ["a"] (var "a")]
      , -- test mostSpecificSubtypes
        testEqual
          "mostSpecificSubtypes: Int against [forall a . a]"
          (mostSpecificSubtypes int [forallu ["a"] (var "a")])
          [forallu ["a"] (var "a")]
      , testEqual
          "mostSpecificSubtypes: (Int -> Int)"
          ( mostSpecificSubtypes
              (fun [int, int])
              [fun [str, str], fun [int, int], forallu ["a"] (fun [var "a", var "a"])]
          )
          [fun [int, int]]
      , testEqual
          "mostSpecificSubtypes: empty"
          (mostSpecificSubtypes (fun [str, str, str]) [fun [real, real, real]])
          []
      , -- test mostSpecificSubtypes for tuples
        testEqual
          "mostSpecificSubtypes: tuples"
          ( mostSpecificSubtypes
              (tuple [int, int])
              [ forallu ["a"] (var "a")
              , forallu ["a", "b"] (tuple [var "a", var "b"])
              , forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              ]
          )
          [forallu ["a", "b"] (tuple [var "a", var "b"])]
      , -- test mostSpecificSubtypes for tuples
        testEqual
          "mostSpecificSubtypes: with partially generic tuples"
          ( mostSpecificSubtypes
              (forallu ["a"] (tuple [int, var "a"]))
              [ forallu ["a"] (var "a")
              , forallu ["a", "b"] (tuple [var "a", var "b"])
              , forallu ["a"] (tuple [int, var "a"])
              , forallu ["a"] (tuple [int, bool])
              , forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              ]
          )
          [forallu ["a"] (tuple [int, var "a"])]
      , -- A specialization chain has a unique most specific member for
        -- every query, so instance selection is well defined.
        testEqual
          "mostSpecificSubtypes: specialization chain resolves uniquely"
          ( mostSpecificSubtypes
              (tuple [int, str, real])
              [ forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              , forallu ["b", "c"] (tuple [int, var "b", var "c"])
              , forallu ["c"] (tuple [int, str, var "c"])
              ]
          )
          [forallu ["c"] (tuple [int, str, var "c"])]
      , -- Two heads that overlap without either subsuming the other leave
        -- the query with two maximal matches and no maximum. Selection is
        -- ambiguous and must be reported rather than resolved.
        testEqual
          "mostSpecificSubtypes: incomparable overlap is ambiguous"
          ( mostSpecificSubtypes
              (tuple [int, int, str])
              [ forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              , forallu ["b", "c"] (tuple [int, var "b", var "c"])
              , forallu ["a", "c"] (tuple [var "a", int, var "c"])
              ]
          )
          [ forallu ["b", "c"] (tuple [int, var "b", var "c"])
          , forallu ["a", "c"] (tuple [var "a", int, var "c"])
          ]
      , -- Adding the unifier of the two overlapping heads restores a unique
        -- maximum: the instance set is closed under unification.
        testEqual
          "mostSpecificSubtypes: unification closure restores a maximum"
          ( mostSpecificSubtypes
              (tuple [int, int, str])
              [ forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              , forallu ["b", "c"] (tuple [int, var "b", var "c"])
              , forallu ["a", "c"] (tuple [var "a", int, var "c"])
              , forallu ["c"] (tuple [int, int, var "c"])
              ]
          )
          [forallu ["c"] (tuple [int, int, var "c"])]
      , -- Alpha-equivalence must not depend on the order in which two types
        -- happen to quantify the same variables: adjacent universals commute.
        -- Two of these fail; see reports/0054.
        testTrue
          "equivalent: renaming"
          ( equivalent
              (forallu ["a", "b"] (tuple [var "a", var "b"]))
              (forallu ["x", "y"] (tuple [var "x", var "y"]))
          )
      , testTrue
          "equivalent: quantifier order swapped"
          ( equivalent
              (forallu ["a", "b"] (tuple [var "a", var "b"]))
              (forallu ["b", "a"] (tuple [var "a", var "b"]))
          )
      , testTrue
          "equivalent: mirrored function types"
          ( equivalent
              (forallu ["a", "b"] (fun [tuple [var "b"], var "a"]))
              (forallu ["b", "a"] (fun [tuple [var "b"], var "a"]))
          )
      , testFalse
          "equivalent: repeated variable is not the same as two"
          ( equivalent
              (forallu ["a"] (tuple [var "a", var "a"]))
              (forallu ["a", "b"] (tuple [var "a", var "b"]))
          )
      , -- Unification: the substitution that makes two types one, used to ask
        -- whether two instance heads can ever describe the same use site.
        testTrue
          "unify: a variable takes the other side"
          (unifies ["a"] (lst (var "a")) (lst int))
      , testTrue
          "unify: variables on both sides"
          (unifies ["a", "b"] (tuple [var "a", str]) (tuple [int, var "b"]))
      , testFalse
          "unify: rigid heads must agree"
          (unifies ["a"] (lst (var "a")) (tuple [int, int]))
      , testFalse
          "unify: a variable used twice must take one value"
          (unifies ["a"] (tuple [var "a", var "a"]) (tuple [int, str]))
      , testTrue
          "unify: a variable used twice may take one value"
          (unifies ["a"] (tuple [var "a", var "a"]) (tuple [int, int]))
      , testFalse
          "unify: occurs check"
          (unifies ["a"] (var "a") (lst (var "a")))
      , -- 'unifyU' shares one namespace between its arguments, so a name used
        -- on both sides is one variable. Separating two types that were
        -- quantified independently is the caller's job.
        testFalse
          "unify: one namespace, so a shared name is one variable"
          (unifies ["a"] (tuple [var "a", int]) (tuple [str, var "a"]))
      , -- The wire forms of two instances agree exactly when the substitution
        -- unifying their heads makes the two forms one. See reports/0051.
        testTrue
          "wireAgrees: a specialization of a generic instance"
          (wireAgrees (["a"], lst (var "a"), lst (var "a")) ([], lst int, lst int))
      , testFalse
          "wireAgrees: a list against a pair"
          (wireAgrees (["a"], lst (var "a"), lst (var "a")) (["b"], lst (var "b"), tuple [var "b", var "b"]))
      , testFalse
          "wireAgrees: a list against a list of lists"
          (wireAgrees (["a"], lst (var "a"), lst (var "a")) (["b"], lst (var "b"), lst (lst (var "b"))))
      , testTrue
          "wireAgrees: heads that never meet are unconstrained"
          (wireAgrees ([], lst int, lst int) ([], lst str, tuple [str, str]))
      , -- Both pairs quantify 'a', and they are not the same 'a'. Without
        -- renaming them apart the heads would fail to unify and the
        -- disagreement below would be missed.
        testFalse
          "wireAgrees: independently quantified names are separated"
          ( wireAgrees
              (["a"], tuple [var "a", int], lst (var "a"))
              (["a"], tuple [str, var "a"], lst int)
          )
      ]

-- | A nullary named type is one token, so it never takes parentheses, in
-- any position. Every printer of a type agrees: the 'Pretty' instances for
-- 'Type' and 'TypeU', the typecheck-diagnostic printer 'prettyTypeU', and
-- the constraint-argument test 'isAtomicType'. A parameterized named type
-- in argument position still takes them.
typeRenderParenTests :: TestTree
typeRenderParenTests =
  testGroup "nullary named types render bare"
    [ testCase "Pretty Type: record argument of an application" $
        render (pretty (AppT box [TyA todoT])) @?= "Box Todo"
    , testCase "Pretty Type: record under an optional" $
        render (pretty (OptionalT todoT)) @?= "?Todo"
    , testCase "Pretty Type: applied record nested in an application" $
        render (pretty (AppT box [TyA (NamT NamRecord (TV "Box") [TyA todoT] [])])) @?= "Box (Box Todo)"
    , testCase "Pretty TypeU: record argument of an application" $
        render (pretty (AppU boxU [todoU])) @?= "Box Todo"
    , testCase "Pretty TypeU: record under an optional" $
        render (pretty (OptionalU todoU)) @?= "?Todo"
    , testCase "prettyTypeU: record argument of an application" $
        render (MTI.prettyTypeU (AppU boxU [todoU])) @?= "Box Todo"
    , testCase "prettyTypeU: applied record nested in an application" $
        render (MTI.prettyTypeU (AppU boxU [NamU NamRecord (TV "Box") [todoU] []])) @?= "Box (Box Todo)"
    , testCase "Pretty Type: suspension argument of an application" $
        render (pretty (AppT box [TyA (EffectT ioSet (VarT (TV "Int")))])) @?= "Box (<IO> Int)"
    , testCase "Pretty TypeU: suspension argument of an application" $
        render (pretty (AppU boxU [EffectU ioEffectSet (VarU (TV "Int"))])) @?= "Box (<IO> Int)"
    , testCase "Pretty Type: arrow with one space each side" $
        render (pretty (FunT [intT] intT)) @?= "Int -> Int"
    , testCase "Pretty Type: application in arrow position" $
        render (pretty (FunT [todoT, AppT box [TyA todoT]] (AppT box [TyA todoT]))) @?= "Todo -> Box Todo -> Box Todo"
    , testCase "Pretty Type: function argument keeps its parentheses" $
        render (pretty (FunT [FunT [intT] intT] intT)) @?= "(Int -> Int) -> Int"
    , testCase "Pretty TypeU: application in arrow position" $
        render (pretty (FunU [todoU, AppU boxU [todoU]] (AppU boxU [todoU]))) @?= "Todo -> Box Todo -> Box Todo"
    , testCase "Pretty TypeU: function argument keeps its parentheses" $
        render (pretty (FunU [FunU [intU] intU] intU)) @?= "(Int -> Int) -> Int"
    , testCase "prettyTypeU: application in arrow position" $
        render (MTI.prettyTypeU (FunU [todoU, AppU boxU [todoU]] (AppU boxU [todoU]))) @?= "Todo -> Box Todo -> Box Todo"
    , testCase "prettyTypeU: function argument keeps its parentheses" $
        render (MTI.prettyTypeU (FunU [FunU [intU] intU] intU)) @?= "(Int -> Int) -> Int"
    , testCase "Pretty Type: a wide tuple stays on one line" $
        render (pretty (AppT (VarT (TV "Tuple5")) (replicate 5 (TyA longT)))) @?= wideTuple
    , testCase "Pretty TypeU: a wide tuple stays on one line" $
        render (pretty (AppU (VarU (TV "Tuple5")) (replicate 5 longU))) @?= wideTuple
    , testCase "prettyTypeU: a wide tuple stays on one line" $
        render (MTI.prettyTypeU (AppU (VarU (TV "Tuple5")) (replicate 5 longU))) @?= wideTuple
    , testCase "isAtomicType: nullary record is atomic" $
        MTI.isAtomicType todoU @?= True
    , testCase "isAtomicType: applied record is not atomic" $
        MTI.isAtomicType (NamU NamRecord (TV "Box") [todoU] []) @?= False
    ]
  where
    box = VarT (TV "Box")
    boxU = VarU (TV "Box")
    todoT = NamT NamRecord (TV "Todo") [] [(Key "title", VarT (TV "Str"))]
    todoU = NamU NamRecord (TV "Todo") [] [(Key "title", VarU (TV "Str"))]
    ioSet = Set.singleton "IO"
    intT = VarT (TV "Int")
    intU = VarU (TV "Int")
    longT = VarT (TV "LongNamedRecordNumberOne")
    longU = VarU (TV "LongNamedRecordNumberOne")
    wideTuple = "(" <> MT.intercalate ", " (replicate 5 "LongNamedRecordNumberOne") <> ")"
