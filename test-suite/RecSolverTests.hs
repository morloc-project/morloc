{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : RecSolverTests
Description : Unit tests for the ordered-row solver

A Rec is an ordered row: field order is part of type identity, and a row
variable may sit anywhere in the sequence, not only at the end. These tests
pin both properties down at the solver boundary.
-}
module RecSolverTests (recSolverTests) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Namespace.Type (TypeU (..))
import Morloc.Typecheck.RecSolver
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tInt, tStr, tReal :: TypeU
tInt = VarU (TV "Int")
tStr = VarU (TV "Str")
tReal = VarU (TV "Real")

-- | A ground row written left to right: @row [("a", tInt)] == {a = Int}@.
row :: [(Text, TypeU)] -> RecExpr
row = foldr (\(k, t) rest -> RecExtend k t rest) RecEmpty

var :: Text -> RecExpr
var = RecVar . TV

segsOf :: RecExpr -> Either RecError [RecSeg]
segsOf e = recSegs <$> normalize e

-- | Just the row-variable bindings, for tests that are about alignment.
subsOf :: RecExpr -> RecExpr -> Either RecError (Map.Map TVar RecExpr)
subsOf a b = recSubs <$> solveRec a b

-- | Just the field types the caller is asked to unify.
eqsOf :: RecExpr -> RecExpr -> Either RecError [(TypeU, TypeU)]
eqsOf a b = recFieldEqs <$> solveRec a b

ground :: [(Text, TypeU)] -> RecSeg
ground = RecGround

tailv :: Text -> RecSeg
tailv = RecTail . TV

recSolverTests :: TestTree
recSolverTests =
  testGroup
    "RecSolver: a Rec is an ordered row"
    [ testGroup
        "normalize preserves the written order"
        [ testCase "a two-field row is not alphabetized" $
            segsOf (row [("b", tInt), ("a", tStr)])
              @?= Right [ground [("b", tInt), ("a", tStr)]]
        , testCase "a reversed row keeps its order" $
            segsOf (row [("c", tReal), ("b", tInt), ("a", tStr)])
              @?= Right [ground [("c", tReal), ("b", tInt), ("a", tStr)]]
        , testCase "the empty row has no segments" $
            segsOf RecEmpty @?= Right []
        , testCase "a duplicate field is rejected" $
            assertContradiction (segsOf (row [("a", tInt), ("a", tStr)]))
        ]
    , testGroup
        "a row variable keeps its position in the sequence"
        [ testCase "{a | r} puts the field before the variable" $
            segsOf (RecUnion (row [("a", tStr)]) (var "r"))
              @?= Right [ground [("a", tStr)], tailv "r"]
        , -- The shape every column operation in the stdlib actually uses.
          -- A (fields, tail) canonical form cannot express it and would
          -- silently move the ground block to the front.
          testCase "r + {z} puts the variable before the field" $
            segsOf (RecUnion (var "r") (row [("z", tReal)]))
              @?= Right [tailv "r", ground [("z", tReal)]]
        , testCase "l + {f} + r keeps the variable on each side" $
            segsOf (RecUnion (var "l") (RecUnion (row [("f", tInt)]) (var "r")))
              @?= Right [tailv "l", ground [("f", tInt)], tailv "r"]
        , testCase "(r - f) + {f} is setCol's schema, variable first" $
            segsOf (RecUnion (RecDiff (var "r") ["f"]) (row [("f", tStr)]))
              @?= Right [tailv "r", ground [("f", tStr)]]
        , testCase "adjacent ground runs are merged into one segment" $
            segsOf (RecUnion (row [("a", tStr)]) (row [("b", tInt)]))
              @?= Right [ground [("a", tStr), ("b", tInt)]]
        , testCase "an empty ground run leaves no segment behind" $
            segsOf (RecUnion RecEmpty (var "r")) @?= Right [tailv "r"]
        ]
    , testGroup
        "order is part of identity"
        [ testCase "{a,b} and {b,a} are distinct canonical forms" $
            assertBool "expected distinct canonical forms" $
              normalize (row [("a", tInt), ("b", tStr)])
                /= normalize (row [("b", tStr), ("a", tInt)])
        , testCase "an equation between two permuted ground rows fails" $
            assertContradiction
              (subsOf (row [("a", tInt), ("b", tStr)])
                        (row [("b", tStr), ("a", tInt)]))
        , testCase "an equation between two identical rows succeeds" $
            subsOf (row [("a", tInt), ("b", tStr)])
                     (row [("a", tInt), ("b", tStr)])
              @?= Right Map.empty
        , testCase "differing key sets are reported as such" $
            assertContradiction
              (subsOf (row [("a", tInt)]) (row [("a", tInt), ("b", tStr)]))
        ]
    , testGroup
        "a row variable is pinned by the fields around it"
        [ testCase "{a | r} ~ {a,b,c} solves r = {b,c}" $
            subsOf (RecUnion (row [("a", tStr)]) (var "r"))
                     (row [("a", tStr), ("b", tInt), ("c", tReal)])
              @?= Right (Map.singleton (TV "r") (row [("b", tInt), ("c", tReal)]))
        , -- The case a prefix-only rule rejects. It must solve.
          testCase "r + {z} ~ {x,z} solves r = {x}" $
            subsOf (RecUnion (var "r") (row [("z", tReal)]))
                     (row [("x", tInt), ("z", tReal)])
              @?= Right (Map.singleton (TV "r") (row [("x", tInt)]))
        , testCase "l + {f} + r ~ {a,f,b} solves both sides" $
            subsOf (RecUnion (var "l") (RecUnion (row [("f", tInt)]) (var "r")))
                     (row [("a", tStr), ("f", tInt), ("b", tReal)])
              @?= Right (Map.fromList
                    [ (TV "l", row [("a", tStr)])
                    , (TV "r", row [("b", tReal)])
                    ])
        , testCase "a pinning field that is absent is a contradiction" $
            assertContradiction
              (subsOf (RecUnion (var "l") (row [("zz", tInt)]))
                        (row [("a", tStr), ("b", tInt)]))
        , testCase "a bare row variable takes the whole row" $
            subsOf (var "r") (row [("a", tStr), ("b", tInt)])
              @?= Right (Map.singleton (TV "r") (row [("a", tStr), ("b", tInt)]))
        , testCase "a variable pinned to an empty span solves to the empty row" $
            subsOf (RecUnion (var "r") (row [("a", tStr)]))
                     (row [("a", tStr)])
              @?= Right (Map.singleton (TV "r") RecEmpty)
        , testCase "a ground block out of order is a contradiction" $
            assertContradiction
              (subsOf (RecUnion (row [("b", tInt)]) (var "r"))
                        (row [("a", tStr), ("b", tInt)]))
        , testCase "two adjacent row variables defer" $
            subsOf (RecUnion (var "r1") (var "r2"))
                     (row [("a", tStr), ("b", tInt)])
              @?= Left RecDeferred
        ]
    , testGroup
        "field types are reported to the caller, not decided here"
        [ testCase "identical field types raise no obligation" $
            eqsOf (row [("a", tInt)]) (row [("a", tInt)]) @?= Right []
        , -- One of these is typically an existential the typechecker still
          -- has to solve, so the solver must not rule on it.
          testCase "a differing field type becomes an obligation" $
            eqsOf (row [("a", tInt)]) (row [("a", tStr)])
              @?= Right [(tInt, tStr)]
        , testCase "aligning a row variable still reports the field types" $
            eqsOf (RecUnion (var "r") (row [("z", tInt)]))
                  (row [("x", tStr), ("z", tReal)])
              @?= Right [(tInt, tReal)]
        , testCase "and still binds the row variable" $
            subsOf (RecUnion (var "r") (row [("z", tInt)]))
                   (row [("x", tStr), ("z", tReal)])
              @?= Right (Map.singleton (TV "r") (row [("x", tStr)]))
        , testCase "a differing key set is still a contradiction, not an obligation" $
            assertContradiction
              (eqsOf (row [("a", tInt)]) (row [("b", tInt)]))
        , -- subtype is directional, so an obligation must keep the order of
          -- the arguments it came from. The known side can be either operand.
          testCase "obligations keep argument order with the ground row on the right" $
            eqsOf (RecUnion (var "r") (row [("z", tInt)]))
                  (row [("x", tStr), ("z", tReal)])
              @?= Right [(tInt, tReal)]
        , testCase "obligations keep argument order with the ground row on the left" $
            eqsOf (row [("x", tStr), ("z", tReal)])
                  (RecUnion (var "r") (row [("z", tInt)]))
              @?= Right [(tReal, tInt)]
        , testCase "the row variable is bound either way round" $
            subsOf (row [("x", tStr), ("z", tReal)])
                   (RecUnion (var "r") (row [("z", tInt)]))
              @?= Right (Map.singleton (TV "r") (row [("x", tStr)]))
        ]
    , testGroup
        "union concatenates; it does not merge into sorted order"
        [ testCase "{b} + {a} is {b,a}" $
            segsOf (RecUnion (row [("b", tInt)]) (row [("a", tStr)]))
              @?= Right [ground [("b", tInt), ("a", tStr)]]
        , testCase "union with an overlapping key is rejected" $
            assertContradiction
              (segsOf (RecUnion (row [("a", tInt)]) (row [("a", tStr)])))
        ]
    , testGroup
        "difference keeps the order of the survivors"
        [ testCase "{c,a,b} - [a] is {c,b}" $
            segsOf (RecDiff (row [("c", tReal), ("a", tStr), ("b", tInt)]) ["a"])
              @?= Right [ground [("c", tReal), ("b", tInt)]]
        , testCase "dropping an absent key is a no-op that preserves order" $
            segsOf (RecDiff (row [("c", tReal), ("a", tStr)]) ["zz"])
              @?= Right [ground [("c", tReal), ("a", tStr)]]
        , testCase "dropping every field leaves no segment" $
            segsOf (RecDiff (row [("a", tStr)]) ["a"]) @?= Right []
        ]
    , testGroup
        "intersection keeps the order of the left operand"
        [ testCase "{c,b,a} & {a,b} keeps b,a in the left operand's order" $
            segsOf (RecIntersect (row [("c", tReal), ("b", tInt), ("a", tStr)])
                                 (row [("a", tStr), ("b", tInt)]))
              @?= Right [ground [("b", tInt), ("a", tStr)]]
        , testCase "intersection defers when either side has a row variable" $
            segsOf (RecIntersect (var "r") (row [("a", tStr)]))
              @?= Left RecDeferred
        ]
    , testGroup
        "groundFields reports the whole row only when it is known"
        [ testCase "a ground row yields its fields" $
            (groundFields <$> normalize (row [("a", tStr), ("b", tInt)]))
              @?= Right (Just [("a", tStr), ("b", tInt)])
        , testCase "a row with a variable yields nothing" $
            (groundFields <$> normalize (RecUnion (var "r") (row [("a", tStr)])))
              @?= Right Nothing
        ]
    , testGroup
        "canonToRecExpr round-trips without sorting"
        [ testCase "a ground row round-trips in order" $
            let e = row [("c", tReal), ("a", tStr), ("b", tInt)]
             in (normalize e >>= normalize . canonToRecExpr) @?= normalize e
        , testCase "a row with a trailing variable round-trips" $
            let e = RecUnion (var "r") (row [("z", tReal)])
             in (normalize e >>= normalize . canonToRecExpr) @?= normalize e
        , testCase "a row with a variable on each side round-trips" $
            let e = RecUnion (var "l") (RecUnion (row [("f", tInt)]) (var "r"))
             in (normalize e >>= normalize . canonToRecExpr) @?= normalize e
        ]
    ]

assertContradiction :: Show a => Either RecError a -> Assertion
assertContradiction (Left (RecContradiction _)) = return ()
assertContradiction other =
  assertFailure $ "expected a RecContradiction, got " <> show other
