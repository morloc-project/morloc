{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Nat
  ( natErrorTests
  , natDimTests
  , natArithTests
  , natLabelTests
  , natKindPromotionTests
  ) where

import Morloc.Frontend.Namespace
import qualified Morloc.Typecheck.Internal as MTI
import qualified Morloc.Typecheck.NatSolver as NS
import qualified Morloc.Typecheck.NatDecide as ND
import qualified Data.Set as DSet
import Text.RawString.QQ

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Types.Common

natErrorTests :: TestTree
natErrorTests =
  testGroup
    "nat typecheck errors"
    [ expectError
        "add dimension mismatch (4 != 5)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      add :: Tensor2 m n Real -> Tensor2 m n Real -> Tensor2 m n Real
      a :: Tensor2 3 4 Real
      b :: Tensor2 3 5 Real
      x = add a b
        |]
    , expectError
        "matmul inner dimension mismatch (4 != 5)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      matmul :: Tensor2 m k Real -> Tensor2 k n Real -> Tensor2 m n Real
      a :: Tensor2 3 4 Real
      b :: Tensor2 5 6 Real
      x = matmul a b
        |]
    , expectError
        "trace requires square matrix (3 != 4)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      trace :: Tensor2 n n Real -> Real
      a :: Tensor2 3 4 Real
      x = trace a
        |]
    , expectError
        "dot product length mismatch (3 != 5)"
        [r|
      module main (x)
      type Tensor1 d1 a
      dot :: Tensor1 n Real -> Tensor1 n Real -> Real
      a :: Tensor1 3 Real
      b :: Tensor1 5 Real
      x = dot a b
        |]
    , expectError
        "vstack column dimension mismatch (3 != 4)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      vstack :: Tensor2 m n Real -> Tensor2 p n Real -> Tensor2 m n Real
      a :: Tensor2 2 3 Real
      b :: Tensor2 4 4 Real
      x = vstack a b
        |]
    , expectError
        "nat arithmetic mismatch: (2+3) != 4"
        [r|
      module main (x)
      newtype SizedList n a = [a]
      append :: SizedList m a -> SizedList n a -> SizedList (m + n) a
      a :: SizedList 2 Int
      b :: SizedList 3 Int
      x :: SizedList 4 Int
      x = append a b
        |]
    ]

natDimTests :: TestTree
natDimTests =
  testGroup
    "nat dimension checking on list literals"
    [ expectError
        "list literal dimension mismatch (3 != 4)"
        [r|
      module main (foo)
      newtype Matrix (m :: Nat) (n :: Nat) a = [[a]]
      foo = [[1,2,3],[4,5,6]] :: Matrix 2 4 Int
        |]
    , testCase "list literal dimensions match" $ do
        result <- runFront [r|
          module main (foo)
          newtype Matrix (m :: Nat) (n :: Nat) a = [[a]]
          foo = [[1,2,3],[4,5,6]] :: Matrix 2 3 Int
            |]
        case result of
          Right _ -> return ()
          Left e -> assertFailure $ "Expected success, got: " <> show e
    , testCase "nat dimensions are checked against a list literal" $ do
        result <- runFrontRaw [r|
          module main (foo)
          newtype Matrix (m :: Nat) (n :: Nat) a = [[a]]
          foo :: Matrix 2 3 Int
          foo = [[1,2,3],[4,5,6]]
            |]
        case result of
          Right [x] ->
            let t = gtypeof x
            in case t of
              AppU (VarU (TV "Matrix")) [NatLitU 2, NatLitU 3, _] -> return ()
              _ -> assertFailure $ "Expected Matrix 2 3 Int, got: " <> show t
          Right _ -> assertFailure "Expected exactly one export"
          Left e -> assertFailure $ "Expected success, got: " <> show e
    , testCase "a list literal is not every size (POLY-7)" $ do
        result <- runFrontRaw [r|
          module main (foo)
          newtype Matrix (m :: Nat) (n :: Nat) a = [[a]]
          foo :: Matrix m n Int -> Matrix m n Int
          foo _ = [[1,2,3],[4,5,6]]
            |]
        case result of
          Left _ -> return ()
          Right _ -> assertFailure "Expected a rejection"
    ]

natArithTests :: TestTree
natArithTests =
  testGroup
    "nat arithmetic (sub, div, solver fix)"
    [ -- NatSolver unit tests
      testCase "ground subtraction: (10 - 3) ~ 7" $
        let e1 = NS.NatSub (NS.NatLit 10) (NS.NatLit 3)
            e2 = NS.NatLit 7
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" subs Map.empty
             Left err -> assertFailure $ "Expected success, got: " ++ show err
    , testCase "ground division: (12 / 4) ~ 3" $
        let e1 = NS.NatDiv (NS.NatLit 12) (NS.NatLit 4)
            e2 = NS.NatLit 3
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" subs Map.empty
             Left err -> assertFailure $ "Expected success, got: " ++ show err
    , testCase "subtraction mismatch: (10 - 3) ~ 8" $
        let e1 = NS.NatSub (NS.NatLit 10) (NS.NatLit 3)
            e2 = NS.NatLit 8
        in case NS.solveNat e1 e2 of
             Left NS.Contradiction -> return ()
             other -> assertFailure $ "Expected Contradiction, got: " ++ show other
    , testCase "division mismatch: (12 / 4) ~ 4" $
        let e1 = NS.NatDiv (NS.NatLit 12) (NS.NatLit 4)
            e2 = NS.NatLit 4
        in case NS.solveNat e1 e2 of
             Left NS.Contradiction -> return ()
             other -> assertFailure $ "Expected Contradiction, got: " ++ show other
    , testCase "a quotient the solver cannot reduce is not zero: n / 2 + 3 ~ 3" $
        let e1 = NS.NatAdd (NS.NatDiv (NS.NatVar (TV "n")) (NS.NatLit 2)) (NS.NatLit 3)
            e2 = NS.NatLit 3
        in case NS.solveNat e1 e2 of
             Left (NS.Deferred _) -> return ()
             other -> assertFailure $ "Expected Deferred, got: " ++ show other
    , testCase "a ground quotient truncates: 7 / 2 ~ 3" $
        case NS.solveNat (NS.NatDiv (NS.NatLit 7) (NS.NatLit 2)) (NS.NatLit 3) of
          Right subs -> assertEqual "" Map.empty subs
          other -> assertFailure $ "Expected success, got: " ++ show other
    , testCase "an open linear equation with large constants is decided: n + m ~ 400" $
        assertBool "" (holds (ND.decide DSet.empty [ND.NatEq (NS.NatAdd (NS.NatVar (TV "n")) (NS.NatVar (TV "m"))) (NS.NatLit 400)]))
    , testCase "a linear equation off the coefficients' lattice has no solution: 2n + 4m ~ 7" $
        assertBool "" (refuted (ND.decide DSet.empty [ND.NatEq (NS.NatAdd (NS.NatMul (NS.NatLit 2) (NS.NatVar (TV "n"))) (NS.NatMul (NS.NatLit 4) (NS.NatVar (TV "m")))) (NS.NatLit 7)]))
    , testCase "the coin problem below its bound: 3n + 5m ~ 7 has no solution" $
        assertBool "" (refuted (ND.decide DSet.empty [ND.NatEq (NS.NatAdd (NS.NatMul (NS.NatLit 3) (NS.NatVar (TV "n"))) (NS.NatMul (NS.NatLit 5) (NS.NatVar (TV "m")))) (NS.NatLit 7)]))
    , testCase "the coin problem with large coefficients is decided quickly: 1000n + 999m ~ 998001" $
        assertBool "" (holds (ND.decide DSet.empty [ND.NatEq (NS.NatAdd (NS.NatMul (NS.NatLit 1000) (NS.NatVar (TV "n"))) (NS.NatMul (NS.NatLit 999) (NS.NatVar (TV "m")))) (NS.NatLit 998001)]))
    , testCase "a large linear equation with a lower bound is decided: 1000n + 999m ~ 998001, m >= 1" $
        assertBool "" (holds (ND.decide DSet.empty
          [ ND.NatEq (NS.NatAdd (NS.NatMul (NS.NatLit 1000) (NS.NatVar (TV "n"))) (NS.NatMul (NS.NatLit 999) (NS.NatVar (TV "m")))) (NS.NatLit 998001)
          , ND.NatGe (NS.NatSub (NS.NatVar (TV "m")) (NS.NatLit 1)) (NS.NatLit 0) ]))
    , testCase "a part of a Nat may be negative: (3 - n) * (3 - n) ~ 16 at n = 7" $
        let d = NS.NatSub (NS.NatLit 3) (NS.NatVar (TV "n"))
        in assertBool "" (holds (ND.decide DSet.empty [ND.NatEq (NS.NatMul d d) (NS.NatLit 16)]))
    , testCase "a witness keeps each side non-negative: n - 4 ~ -(m + 1)" $
        let rhs = NS.NatSub (NS.NatLit 0) (NS.NatAdd (NS.NatVar (TV "m")) (NS.NatLit 1))
            lhs = NS.NatSub (NS.NatVar (TV "n")) (NS.NatLit 4)
        in assertBool "" (refuted (ND.decide DSet.empty [ND.NatEq lhs rhs, ND.NatGe lhs (NS.NatLit 0), ND.NatGe rhs (NS.NatLit 0)]))
    , testCase "a solved Nat is never negative: m + 1 ~ 0" $
        case NS.solveNat (NS.NatAdd (NS.NatVar (TV "m")) (NS.NatLit 1)) (NS.NatLit 0) of
          Left NS.Contradiction -> return ()
          other -> assertFailure $ "Expected Contradiction, got: " ++ show other
    , testCase "an open equation keeps no solution that may be negative: n + m ~ 4" $
        case NS.solveNat (NS.NatAdd (NS.NatVar (TV "n")) (NS.NatVar (TV "m"))) (NS.NatLit 4) of
          Left (NS.Deferred _) -> return ()
          other -> assertFailure $ "Expected Deferred, got: " ++ show other
    , testCase "subtraction with variable: n - 3 ~ 5 => n = 8" $
        let e1 = NS.NatSub (NS.NatVar (TV "n")) (NS.NatLit 3)
            e2 = NS.NatLit 5
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" (Map.singleton (TV "n") (NS.NatLit 8)) subs
             Left err -> assertFailure $ "Expected n=8, got: " ++ show err
    , testCase "division by constant: n / 3 with n = 9 / 3 ~ 3" $
        let e1 = NS.NatDiv (NS.NatLit 9) (NS.NatLit 3)
            e2 = NS.NatLit 3
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" subs Map.empty
             Left err -> assertFailure $ "Expected success, got: " ++ show err
    -- Polynomial substitution. When the caller passes a @(v, rhs)@ the
    -- assertion also checks that @v@ is the chosen substituted variable;
    -- passing @Nothing@ checks only correctness (satisfaction), useful
    -- when the direction depends on sort-order tie-breaking.
    , testCase "polynomial: i*j ~ n => n = i*j" $
        assertSolvesTo (nMul (nVar "i") (nVar "j")) (nVar "n")
                       (Just (TV "n", nMul (nVar "i") (nVar "j")))
    , testCase "polynomial: n ~ i + j => n = i + j" $
        assertSolvesTo (nVar "n") (nAdd (nVar "i") (nVar "j"))
                       (Just (TV "n", nAdd (nVar "i") (nVar "j")))
    , testCase "polynomial: n ~ 2*i + 3 => n = 2*i + 3" $
        assertSolvesTo (nVar "n") (nAdd (nMul (nLit 2) (nVar "i")) (nLit 3))
                       (Just (TV "n", nAdd (nMul (nLit 2) (nVar "i")) (nLit 3)))
    , testCase "asymmetric: 2*n ~ i => i = 2*n (only division-exact direction)" $
        assertSolvesTo (nMul (nLit 2) (nVar "n")) (nVar "i")
                       (Just (TV "i", nMul (nLit 2) (nVar "n")))
    , testCase "variable unify: n1 ~ n2 (correctness only; direction ties)" $
        assertSolvesTo (nVar "n1") (nVar "n2") Nothing
    , testCase "divisible: 2*n ~ 2*i (correctness only; direction ties)" $
        assertSolvesTo (nMul (nLit 2) (nVar "n")) (nMul (nLit 2) (nVar "i")) Nothing
    -- Genuinely undecidable cases: no linear substitution captures them.
    , testCase "undecidable: n*m ~ 5 is Deferred" $
        assertDeferred (nMul (nVar "n") (nVar "m")) (nLit 5)
    , testCase "undecidable Diophantine: 2*n + 3*m ~ 5 is Deferred" $
        assertDeferred (nAdd (nMul (nLit 2) (nVar "n")) (nMul (nLit 3) (nVar "m"))) (nLit 5)
    , testCase "linear solving still works: n + 3 ~ 8 => n = 5" $
        let e1 = NS.NatAdd (NS.NatVar (TV "n")) (NS.NatLit 3)
            e2 = NS.NatLit 8
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" (Map.singleton (TV "n") (NS.NatLit 5)) subs
             Left err -> assertFailure $ "Expected n=5, got: " ++ show err
    , testCase "simple variable solving: n ~ 5" $
        let e1 = NS.NatVar (TV "n")
            e2 = NS.NatLit 5
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" (Map.singleton (TV "n") (NS.NatLit 5)) subs
             Left err -> assertFailure $ "Expected n=5, got: " ++ show err
    -- Typechecker integration tests for sub/div syntax
    , expectError
        "ground subtraction mismatch: (10-3) != 8 in type annotation"
        [r|
      module main (x)
      newtype SizedList n a = [a]
      a :: SizedList (10 - 3) Int
      x :: SizedList 8 Int
      x = a
        |]
    , expectError
        "ground division mismatch: (12/4) != 4 in type annotation"
        [r|
      module main (x)
      newtype SizedList n a = [a]
      a :: SizedList (12 / 4) Int
      x :: SizedList 4 Int
      x = a
        |]
    -- deferred constraint re-checking: variable arithmetic caught after solving
    , expectError
        "deferred subtraction mismatch: m=8, n=3, but m-n used as 7"
        [r|
      module main (x)
      newtype SizedList n a = [a]
      take :: SizedList (m - n) a -> SizedList n a -> SizedList m a
      a :: SizedList 7 Int
      b :: SizedList 3 Int
      x :: SizedList 8 Int
      x = take a b
        |]
    , expectError
        "deferred multiplication mismatch: n*m=12 but n=5 (no integer m)"
        [r|
      module main (x)
      newtype SizedList n a = [a]
      split :: SizedList (n * m) a -> SizedList n a
      a :: SizedList 12 Int
      x :: SizedList 5 Int
      x = split a
        |]
    ]
  where
    nVar s = NS.NatVar (TV s)
    nLit  = NS.NatLit
    nAdd  = NS.NatAdd
    nMul  = NS.NatMul

    assertSolvesTo :: NS.NatExpr -> NS.NatExpr -> Maybe (TVar, NS.NatExpr) -> Assertion
    assertSolvesTo e1 e2 mExpected = case NS.solveNat e1 e2 of
      Left err -> assertFailure $ "Expected solve, got: " ++ show err
      Right subs -> do
        assertBool ("Substitution does not satisfy: " ++ show subs)
          (NS.natEqual (NS.substituteNat subs e1) (NS.substituteNat subs e2))
        case mExpected of
          Nothing -> return ()
          Just (v, rhs) -> case Map.lookup v subs of
            Just e | NS.natEqual e rhs -> return ()
            Just e  -> assertFailure $ "Direction: expected " ++ show v ++ " = "
                                     ++ show rhs ++ ", got " ++ show e
            Nothing -> assertFailure $ "Direction: expected " ++ show v
                                     ++ " in subs, got: " ++ show subs

    assertDeferred :: NS.NatExpr -> NS.NatExpr -> Assertion
    assertDeferred e1 e2 = case NS.solveNat e1 e2 of
      Left (NS.Deferred _) -> return ()
      Right subs -> assertFailure $ "Expected Deferred, got solved: " ++ show subs
      Left NS.Contradiction -> assertFailure "Expected Deferred, got Contradiction"

natLabelTests :: TestTree
natLabelTests =
  testGroup
    "nat labeled params (m@Int syntax)"
    [ -- === Positive: literal int args resolve nat vars ===
      assertRawType
        "labeled literal resolves dimension: makeVec 5 :: Tensor1 5 Real"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec 5
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "labeled literal zero dimension: makeVec 0 :: Tensor1 0 Real"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec 0
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 0, VarU (TV "Real")])
    , assertRawType
        "two labeled params resolve: makeMat 3 4 :: Tensor2 3 4 Real"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeMat :: m@Int -> n@Int -> Tensor2 m n Real
      x = makeMat 3 4
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
    , assertRawType
        "labeled dims flow through generic op: id_ (makeVec 7) :: Tensor1 7 Real"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      id_ :: Tensor1 n Real -> Tensor1 n Real
      x = id_ (makeVec 7)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 7, VarU (TV "Real")])
    , assertRawType
        "labeled dims with nat arithmetic: conv output dims computed"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      type Tensor3 (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a
      makeImg :: h@Int -> w@Int -> Tensor2 h w Real
      makeK :: k@Int -> fh@Int -> fw@Int -> Tensor3 k fh fw Real
      newtype Tensor1 (d :: Nat) a = [a]
      makeB :: k@Int -> Tensor1 k Real
      conv :: Tensor2 h w Real -> Tensor3 k fh fw Real -> Tensor1 k Real -> Tensor3 k (h - fh + 1) (w - fw + 1) Real
      x = conv (makeImg 5 5) (makeK 2 3 3) (makeB 2)
        |]
        (AppU (VarU (TV "Tensor3")) [NatLitU 2, NatLitU 3, NatLitU 3, VarU (TV "Real")])
    , assertRawType
        "labeled + flatten nat arithmetic: 2*3*3 = 18"
        [r|
      module main (x)
      type Tensor3 (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a
      newtype Tensor1 (d :: Nat) a = [a]
      makeT :: a@Int -> b@Int -> c@Int -> Tensor3 a b c Real
      flatten :: Tensor3 a b c Real -> Tensor1 (a * b * c) Real
      x = flatten (makeT 2 3 3)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 18, VarU (TV "Real")])
    , assertRawType
        "mixed labeled and unlabeled args"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeT :: m@Int -> n@Int -> Tensor2 m n Real
      scale :: Real -> Tensor2 m n Real -> Tensor2 m n Real
      x = scale 2.0 (makeT 3 4)
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
    , assertRawType
        "same label var used in two positions (diagonal)"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      eye :: n@Int -> Tensor2 n n Real
      x = eye 4
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 4, NatLitU 4, VarU (TV "Real")])

    -- === Positive: let-bound integers resolve nat labels ===
    , assertRawType
        "let-bound int resolves label: let n = 5 in makeVec n"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = let n = 5 in makeVec n
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "chained let-bound: let a = 7 in let b = a in makeVec b"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = let a = 7 in let b = a in makeVec b
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 7, VarU (TV "Real")])
    , assertRawType
        "multiple let-bound dims: let m=3, n=4 in makeMat m n"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeMat :: m@Int -> n@Int -> Tensor2 m n Real
      x = let m = 3 in let n = 4 in makeMat m n
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])

    -- === Positive: tuple accessor evaluation ===
    , assertRawType
        "tuple accessor resolves: makeVec (.0 (5, 6))"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec (.0 (5, 6))
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "let-bound tuple + accessor: let dims = (3,4) in makeMat (.0 dims) (.1 dims)"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeMat :: m@Int -> n@Int -> Tensor2 m n Real
      x = let dims = (3, 4) in makeMat (.0 dims) (.1 dims)
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
    , assertRawType
        "chained let + accessor: let d=(8,9); let n=.0 d in makeVec n"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = let d = (8, 9) in let n = .0 d in makeVec n
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 8, VarU (TV "Real")])

    -- === Positive: lambda application evaluation ===
    , assertRawType
        "identity lambda: makeVec ((\\x -> x) 5)"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec ((\n -> n) 5)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "lambda + accessor: makeVec ((\\t -> .1 t) (1,2,3))"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec ((\t -> .1 t) (1, 2, 3))
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 2, VarU (TV "Real")])
    , assertRawType
        "lambda selects first of two args: (\\x y -> x) 7 99"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec ((\a b -> a) 7 99)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 7, VarU (TV "Real")])

    -- === Negative: dimension mismatches caught despite labels ===
    , expectError
        "labeled dim mismatch: add (makeT 3 4) (makeT 3 5) fails"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeT :: m@Int -> n@Int -> Tensor2 m n Real
      add :: Tensor2 m n Real -> Tensor2 m n Real -> Tensor2 m n Real
      x = add (makeT 3 4) (makeT 3 5)
        |]
    , expectError
        "labeled dim mismatch: dot product length mismatch"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      dot :: Tensor1 n Real -> Tensor1 n Real -> Real
      x = dot (makeVec 3) (makeVec 5)
        |]
    , expectError
        "labeled dim mismatch through arithmetic: conv wrong kernel size"
        [r|
      module main (x)
      newtype Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      type Tensor3 (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a
      newtype Tensor1 (d :: Nat) a = [a]
      makeImg :: h@Int -> w@Int -> Tensor2 h w Real
      makeK :: k@Int -> fh@Int -> fw@Int -> Tensor3 k fh fw Real
      makeB :: k@Int -> Tensor1 k Real
      conv :: Tensor2 h w Real -> Tensor3 k fh fw Real -> Tensor1 k Real -> Tensor3 k (h - fh + 1) (w - fw + 1) Real
      x :: Tensor3 2 3 4 Real
      x = conv (makeImg 5 5) (makeK 2 3 3) (makeB 2)
        |]
    , expectError
        "annotated return type contradicts labeled resolution"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x :: Tensor1 99 Real
      x = makeVec 5
        |]

    -- === Interesting edge cases ===
    , assertRawType
        "no labels: plain nat vars remain generic"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      id_ :: Tensor1 n Real -> Tensor1 n Real
      a :: Tensor1 5 Real
      x = id_ a
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "label on non-first param position"
        [r|
      module main (x)
      newtype Tensor1 (d :: Nat) a = [a]
      makeFrom :: Real -> n@Int -> Tensor1 n Real
      x = makeFrom 1.0 10
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 10, VarU (TV "Real")])
    ]

natKindPromotionTests :: TestTree
natKindPromotionTests =
  testGroup
    "nat kind promotion and cross-feature interaction"
    [
    -- ================================================================
    -- (:: Nat) annotation effects on type variable promotion
    -- ================================================================
    -- When a typedef has (d :: Nat), variables at that position MUST be
    -- promoted from VarU to NatVarU. Without this promotion, nat label
    -- resolution and nat constraint solving both silently fail.

      assertRawType
        "with (:: Nat): labels resolve to concrete dimensions"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      makeVec :: n@Int -> T1 n Real
      x = makeVec 5
        |]
        (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")])

    , assertRawType
        "without (:: Nat): labels do NOT resolve (dim stays generic)"
        [r|
      module main (x)
      newtype T1 d a = [a]
      makeVec :: n@Int -> T1 n Real
      x = makeVec 5
        |]
        -- Without :: Nat, n is VarU not NatVarU, so resolveNatLabels cannot
        -- find nat vars to solve. The type stays generic (existential).
        (AppU (VarU (TV "T1")) [ExistU (TV "a") ([], Open) ([], Open), VarU (TV "Real")])

    -- ================================================================
    -- Nat constraint solving requires (:: Nat) promotion
    -- ================================================================

    , expectError
        "with (:: Nat): dimension mismatch is caught (4 != 5)"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      dot :: T1 n Real -> T1 n Real -> Real
      a :: T1 4 Real
      b :: T1 5 Real
      x = dot a b
        |]

    -- Without (:: Nat), the params are still NatLitU from the literal syntax.
    -- The typechecker catches the mismatch through existential solving since
    -- the NatLit values 4 and 5 cannot unify. This is actually sound.
    , expectError
        "without (:: Nat): dimension mismatch still caught via existentials"
        [r|
      module main (x)
      newtype T1 d a = [a]
      dot :: T1 n Real -> T1 n Real -> Real
      a :: T1 4 Real
      b :: T1 5 Real
      x = dot a b
        |]

    -- ================================================================
    -- Multi-step composition: labels propagate through chains
    -- ================================================================

    , assertRawType
        "labeled dims propagate through 3-function chain"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      f :: T1 n Real -> T1 n Real
      g :: T1 n Real -> T1 n Real
      x = g (f (makeVec 9))
      makeVec :: n@Int -> T1 n Real
      x = g (f (make 9))
        |]
        (AppU (VarU (TV "T1")) [NatLitU 9, VarU (TV "Real")])

    , assertRawType
        "labeled dims propagate through let chain"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      f :: T1 n Real -> T1 n Real
      x = let a = make 4
          in let b = f a
          in f b
        |]
        (AppU (VarU (TV "T1")) [NatLitU 4, VarU (TV "Real")])

    -- ================================================================
    -- Nat arithmetic with labeled params
    -- ================================================================

    , assertRawType
        "labeled subtraction: h=10 w=3, h-w+1 = 8"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: h@Int -> w@Int -> T1 (h - w + 1) Real
      x = make 10 3
        |]
        (AppU (VarU (TV "T1")) [NatLitU 8, VarU (TV "Real")])

    , assertRawType
        "labeled multiplication: m=3 n=4, m*n = 12"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: m@Int -> n@Int -> T1 (m * n) Real
      x = make 3 4
        |]
        (AppU (VarU (TV "T1")) [NatLitU 12, VarU (TV "Real")])

    , assertRawType
        "labeled division: n=12 d=4, n/d = 3"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> d@Int -> T1 (n / d) Real
      x = make 12 4
        |]
        (AppU (VarU (TV "T1")) [NatLitU 3, VarU (TV "Real")])

    , assertRawType
        "compound arithmetic: a=6 b=2 c=1, (a*b)-c = 11"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: a@Int -> b@Int -> c@Int -> T1 (a * b - c) Real
      x = make 6 2 1
        |]
        (AppU (VarU (TV "T1")) [NatLitU 11, VarU (TV "Real")])

    -- ================================================================
    -- Negative: arithmetic mismatches with labels
    -- ================================================================

    , expectError
        "labeled arithmetic mismatch: 10-3+1=8 but annotated as 7"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: h@Int -> w@Int -> T1 (h - w + 1) Real
      x :: T1 7 Real
      x = make 10 3
        |]

    , expectError
        "labeled multiplication mismatch: 3*4=12 but used where 11 expected"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: m@Int -> n@Int -> T1 (m * n) Real
      consume :: T1 11 Real -> Int
      x = consume (make 3 4)
        |]

    -- ================================================================
    -- Cross-feature: nat dims with optional types
    -- ================================================================

    , assertRawType
        "optional tensor: ?(T1 n Real) with labeled dim"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      tryMake :: n@Int -> ?(T1 n Real)
      x = tryMake 5
        |]
        (OptionalU (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")]))

    -- ================================================================
    -- Cross-feature: nat dims with effect types
    -- ================================================================

    , assertRawType
        "effectful tensor: <IO> T1 n Real with labeled dim"
        [r|
      module main (x)
      effect IO
      newtype T1 (d :: Nat) a = [a]
      ioMake :: n@Int -> <IO> T1 n Real
      x = ioMake 5
        |]
        (EffectU (EffectSet (Set.singleton "IO"))
          (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")]))

    -- ================================================================
    -- Nat dims nested inside other type constructors
    -- ================================================================

    , assertRawType
        "list of nat-parameterized type: [T1 n Real]"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> [T1 n Real]
      x = make 5
        |]
        (AppU (VarU (TV "List")) [AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")]])

    , assertRawType
        "tuple of nat-parameterized types"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: m@Int -> n@Int -> (T1 m Real, T1 n Real)
      x = make 3 7
        |]
        (AppU (VarU (TV "Tuple2"))
          [ AppU (VarU (TV "T1")) [NatLitU 3, VarU (TV "Real")]
          , AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")]
          ])

    -- ================================================================
    -- Multiple nat-parameterized typedefs interacting
    -- ================================================================

    , assertRawType
        "conversion between two nat-parameterized types"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      newtype T2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      flatten :: T2 m n Real -> T1 (m * n) Real
      make :: m@Int -> n@Int -> T2 m n Real
      x = flatten (make 3 4)
        |]
        (AppU (VarU (TV "T1")) [NatLitU 12, VarU (TV "Real")])

    , expectError
        "cross-type dim mismatch: flatten 3x4=12 but consume expects 11"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      newtype T2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      flatten :: T2 m n Real -> T1 (m * n) Real
      make :: m@Int -> n@Int -> T2 m n Real
      consume :: T1 11 Real -> Int
      x = consume (flatten (make 3 4))
        |]

    -- ================================================================
    -- Partial nat annotation: some params Nat, some not
    -- ================================================================

    , assertRawType
        "mixed params: first is Nat, second is Type"
        [r|
      module main (x)
      newtype Sized (n :: Nat) a = [a]
      make :: n@Int -> Sized n Int
      x = make 10
        |]
        (AppU (VarU (TV "Sized")) [NatLitU 10, VarU (TV "Int")])

    -- ================================================================
    -- Negative: bad label syntax and semantics
    -- ================================================================

    -- Label n resolves nothing in return type (m is a different var), so
    -- m stays generic. This is valid — the label just has no effect.
    -- m is promoted to NatVarU (it's in a :: Nat position) but stays unresolved.
    , assertRawType
        "label on param that doesn't appear in return type: dim stays generic"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 m Real
      x = make 5
        |]
        (AppU (VarU (TV "T1")) [NatVarU (TV "a"), VarU (TV "Real")])

    -- ================================================================
    -- Edge: nat literal 0 in arithmetic
    -- ================================================================

    , assertRawType
        "nat literal 0 in subtraction: n-0 = n"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 (n - 0) Real
      x = make 7
        |]
        (AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")])

    , assertRawType
        "nat literal 1 in multiplication: n*1 = n"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 (n * 1) Real
      x = make 7
        |]
        (AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")])

    , assertRawType
        "nat literal 0 in multiplication: n*0 = 0"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 (n * 0) Real
      x = make 7
        |]
        (AppU (VarU (TV "T1")) [NatLitU 0, VarU (TV "Real")])

    -- ================================================================
    -- Regression: multiple exports with nat dims
    -- ================================================================

    , testCase "multiple exports all resolve independently" $ do
        result <- runFrontRaw [r|
      module main (x, y)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      x = make 3
      y = make 7
          |]
        case result of
          Right [xExpr, yExpr] -> do
            let xt = MTI.cleanTypeName . renameExistentials . gtypeof $ xExpr
                yt = MTI.cleanTypeName . renameExistentials . gtypeof $ yExpr
                expected3 = MTI.cleanTypeName $ AppU (VarU (TV "T1")) [NatLitU 3, VarU (TV "Real")]
                expected7 = MTI.cleanTypeName $ AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")]
            assertEqual "first export" (closeExistentials expected3) (closeExistentials xt)
            assertEqual "second export" (closeExistentials expected7) (closeExistentials yt)
          Right other -> assertFailure $ "Expected 2 exports, got " ++ show (length other)
          Left e -> assertFailure $ "Unexpected error: " ++ show e

    -- ================================================================
    -- Regression: large nat literals
    -- ================================================================

    , assertRawType
        "large nat literal: 1000000"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      x = make 1000000
        |]
        (AppU (VarU (TV "T1")) [NatLitU 1000000, VarU (TV "Real")])

    -- ================================================================
    -- Negative: nat constraint contradictions caught through labels
    -- ================================================================

    , expectError
        "labeled: same label used for different values"
        [r|
      module main (x)
      newtype T1 (d :: Nat) a = [a]
      combine :: T1 n Real -> T1 n Real -> T1 n Real
      make :: n@Int -> T1 n Real
      x = combine (make 3) (make 5)
        |]

    -- ================================================================
    -- Forward-declared type (no RHS) with :: Nat
    -- ================================================================

    , assertRawType
        "forward-declared type with :: Nat resolves labels"
        [r|
      module main (x)
      type Opaque (d :: Nat) a
      make :: n@Int -> Opaque n Real
      x = make 42
        |]
        (AppU (VarU (TV "Opaque")) [NatLitU 42, VarU (TV "Real")])

    , expectError
        "forward-declared type: dim mismatch caught"
        [r|
      module main (x)
      type Opaque (d :: Nat) a
      make :: n@Int -> Opaque n Real
      consume :: Opaque 10 Real -> Int
      x = consume (make 5)
        |]
    ]
