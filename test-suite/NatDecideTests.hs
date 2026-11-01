{- |
Module      : NatDecideTests
Description : Examples and properties of the Nat decision procedure (DIM-7)

The properties compare every verdict of 'decide' with evaluation: a
condition with no variables is decided exactly, a verdict of holds is
checked at sampled values of the rigid variables through its witness, and a
refutation is checked by searching the other variables at its
counterexample. Under assumptions the samples are those that satisfy them,
and a counterexample must satisfy them too.
-}
module NatDecideTests (natDecideTests) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.Namespace.Prim (TVar (..))
import Morloc.Typecheck.NatDecide
import Morloc.Typecheck.NatSolver (NatExpr (..), freeNatVars)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

v :: String -> NatExpr
v = NatVar . TV . T.pack

tv :: String -> TVar
tv = TV . T.pack

lit :: Integer -> NatExpr
lit = NatLit

isHolds, isRefuted :: Decision -> Bool
isHolds (Holds _) = True
isHolds _ = False
isRefuted (Refuted _) = True
isRefuted _ = False

rigid :: [String] -> Set.Set TVar
rigid = Set.fromList . map tv

natDecideTests :: TestTree
natDecideTests =
  testGroup
    "Nat decision procedure (DIM-7)"
    [ testGroup "examples" examples
    , testGroup "properties" properties
    ]

examples :: [TestTree]
examples =
  [ testCase "a true ground equation holds" $
      assertBool "" (isHolds (decide Set.empty [NatEq (NatAdd (lit 2) (lit 3)) (lit 5)]))
  , testCase "a false ground equation is refuted" $
      assertBool "" (isRefuted (decide Set.empty [NatEq (lit 2) (lit 3)]))
  , testCase "a negative ground difference is refuted (KIND-4)" $
      assertBool "" (isRefuted (decide Set.empty [NatEq (NatSub (lit 3) (lit 10)) (v "x")]))
  , testCase "division by zero is refuted" $
      assertBool "" (isRefuted (decide Set.empty [NatEq (NatDiv (lit 3) (lit 0)) (lit 0)]))
  , testCase "a rigid n - 1 is refuted at n = 0 (KIND-4)" $
      decide (rigid ["n"]) [NatGe (NatSub (v "n") (lit 1)) (lit 0)]
        @?= Refuted (Map.fromList [(tv "n", 0)])
  , testCase "multiplication commutes over rigid variables" $
      assertBool "" (isHolds (decide (rigid ["m", "n"]) [NatEq (NatMul (v "m") (v "n")) (NatMul (v "n") (v "m"))]))
  , testCase "distribution over rigid variables" $
      assertBool "" (isHolds (decide (rigid ["a", "b", "c"])
        [NatEq (NatMul (v "a") (NatAdd (v "b") (v "c"))) (NatAdd (NatMul (v "a") (v "b")) (NatMul (v "c") (v "a")))]))
  , testCase "a rigid n cannot meet m + 1 (POLY-7)" $
      decide (rigid ["n"]) [NatEq (v "n") (NatAdd (v "m") (lit 1))]
        @?= Refuted (Map.fromList [(tv "n", 0)])
  , testCase "an inferred m is defined by a rigid n" $
      case decide (rigid ["n"]) [NatEq (NatAdd (v "n") (lit 1)) (v "m")] of
        Holds w -> (Map.lookup (tv "m") w >>= evalNat (Map.fromList [(tv "n", 5)])) @?= Just 6
        d -> assertFailure (show d)
  , testCase "a square minus one is refuted at zero" $
      assertBool "" (isRefuted (decide (rigid ["n"]) [NatEq (NatMul (v "n") (v "n")) (NatAdd (v "m") (lit 1))]))
  , testCase "two inferred variables with a solution hold (DIM-6)" $
      assertBool "" (isHolds (decide Set.empty [NatEq (NatAdd (v "n") (v "m")) (lit 4)]))
  , testCase "an equation with no natural solution is refuted" $
      assertBool "" (isRefuted (decide Set.empty [NatEq (NatMul (lit 2) (v "m")) (lit 3)]))
  , testCase "a rigid sum is at least its part" $
      assertBool "" (isHolds (decide (rigid ["a", "b"]) [NatGe (NatAdd (v "a") (v "b")) (v "a")]))
  , testCase "a linear inequality is refuted beyond any search width" $
      decide (rigid ["n"]) [NatGe (lit 30000) (v "n")] @?= Refuted (Map.fromList [(tv "n", 30001)])
  , testCase "a linear equation with a large solution holds" $
      assertBool "" (isHolds (decide Set.empty [NatEq (NatAdd (NatMul (lit 2) (v "x")) (NatMul (lit 3) (v "y"))) (lit 100001)]))
  , testCase "a large linear equation with subtraction is never refuted" $
      assertBool "" (not (isRefuted (decide Set.empty [NatEq (NatSub (NatMul (lit 2) (v "x")) (NatMul (lit 3) (v "y"))) (lit 1000001)])))
  , testCase "a mixed condition proven by its coefficients holds" $
      assertBool "" (isHolds (decide (rigid ["r"]) [NatGe (NatAdd (v "x") (v "r")) (v "r")]))
  , testCase "a nonlinear rigid equation is refuted on the degree grid" $
      assertBool "" (isRefuted (decide (rigid ["a", "b"]) [NatEq (NatMul (v "a") (v "a")) (NatMul (v "a") (v "b"))]))
  , localOption (mkTimeout 2000000) $ testCase "a mixed nonlinear condition is decided quickly" $
      case decide (rigid ["r"]) [NatEq (NatMul (v "x") (v "x")) (NatAdd (v "r") (lit 100000))] of
        Holds w -> assertFailure ("no x squares to 100000 at r = 0: " <> show w)
        _ -> return ()
  , testCase "one linear equation over inferred variables is exact past the search width" $
      assertBool "" (isHolds (decide Set.empty [NatEq (NatAdd (v "a") (lit 30000)) (v "b")]))
  , testCase "a ground false conjunct refutes a mixed group" $
      assertBool "" (isRefuted (decide (rigid ["r"]) [NatGe (v "r") (lit 3), NatGe (v "x") (v "y")]))
  , testCase "a rigid conjunct is refuted beside many inferred variables" $
      assertBool "" (isRefuted (decide (rigid ["r"])
        [NatEq (NatMul (v "r") (v "r")) (v "r"), NatGe (v "x") (v "y"), NatGe (v "y") (v "z"), NatGe (v "z") (v "w")]))
  , testCase "an inferred lower bound over a rigid variable holds" $
      assertBool "" (isHolds (decide (rigid ["r"]) [NatGe (v "x") (v "r")]))
  , localOption (mkTimeout 2000000) $ testCase "a large rigid product is decided quickly" $
      assertBool "" (not (isHolds (decide (rigid (map (: []) ['a' .. 'v'])) [NatEq (foldr1 NatMul (map (v . (: [])) ['a' .. 'v'])) (lit 0)])))
  , testCase "an inferred variable written equal to a quotient is eliminated" $
      assertBool "" (isHolds (decide (rigid ["n"]) [NatEq (v "k") (NatDiv (v "n") (lit 2)), NatGe (NatDiv (v "n") (lit 2)) (lit 0)]))
  , testCase "a condition that is an assumption holds" $
      let slot = NatAdd (NatSub (v "n") (v "w")) (lit 1)
       in assertBool "" (isHolds (decideAssuming (rigid ["n", "w"]) [NatGe slot (lit 0)] [NatGe slot (lit 0)]))
  , testCase "an assumed n - 1 >= 0 lets m = n - 1 be natural (POLY-7)" $
      assertBool "" (isHolds (decideAssuming (rigid ["n"]) [NatGe (NatSub (v "n") (lit 1)) (lit 0)] [NatEq (v "n") (NatAdd (v "m") (lit 1))]))
  , testCase "without the assumption, m = n - 1 is refuted at n = 0" $
      decide (rigid ["n"]) [NatEq (v "n") (NatAdd (v "m") (lit 1))] @?= Refuted (Map.fromList [(tv "n", 0)])
  , testCase "a sum of two assumptions discharges their sum" $
      let a1 = NatSub (v "n") (v "w")
          a2 = NatSub (v "m") (v "u")
       in assertBool "" (isHolds (decideAssuming (rigid ["n", "w", "m", "u"]) [NatGe a1 (lit 0), NatGe a2 (lit 0)] [NatGe (NatAdd a1 a2) (lit 0)]))
  , testCase "an empty domain makes every condition hold" $
      assertBool "" (isHolds (decideAssuming (rigid ["n"]) [NatGe (NatSub (lit 0) (lit 3)) (lit 0)] [NatEq (v "n") (lit 5)]))
  , testCase "a refutation under an assumption satisfies it" $
      case decideAssuming (rigid ["n"]) [NatGe (NatSub (v "n") (lit 2)) (lit 0)] [NatGe (lit 4) (v "n")] of
        Refuted env -> assertBool (show env) (maybe False (\x -> x >= 2 && x > 4) (Map.lookup (tv "n") env))
        d -> assertFailure (show d)
  , testCase "a quotient inside a sum is a natural" $
      assertBool "" (isHolds (decide (rigid ["n"]) [NatGe (NatAdd (NatDiv (v "n") (lit 2)) (v "n")) (lit 0)]))
  , testCase "an Int variable may be negative" $
      decide (rigid ["i"]) [NatGe (IntVar (tv "i")) (lit 0)] @?= Refuted (Map.fromList [(tv "i", -1)])
  , testCase "a Nat plus an Int is checked as a whole" $
      assertBool "" (isHolds (decideAssuming (rigid ["n", "k"]) [NatGe (NatAdd (v "n") (IntVar (tv "k"))) (lit 0)] [NatGe (NatAdd (NatAdd (v "n") (IntVar (tv "k"))) (lit 1)) (lit 0)]))
  , testCase "an inferred Nat equal to a rigid Int needs it non-negative" $
      case decide (rigid ["h"]) [NatEq (v "m") (IntVar (tv "h"))] of
        Refuted env -> Map.lookup (tv "h") env @?= Just (-1)
        d -> assertFailure (show d)
  , testCase "a square of a difference is proven by cases: (n - 1) * (n - 1) >= 0" $
      let d = NatSub (v "n") (lit 1)
       in assertBool "" (isHolds (decide (rigid ["n"]) [NatGe (NatMul d d) (lit 0)]))
  , testCase "a count of pairs is proven by cases: n * (n - 1) / 2 >= 0" $
      assertBool "" (isHolds (decide (rigid ["n"]) [NatGe (NatDiv (NatMul (v "n") (NatSub (v "n") (lit 1))) (lit 2)) (lit 0)]))
  , testCase "no square is 7" $
      assertBool "" (isRefuted (decide Set.empty [NatEq (NatMul (v "m") (v "m")) (lit 7)]))
  , testCase "not every n is a square" $
      case decide (rigid ["n"]) [NatEq (v "n") (NatMul (v "m") (v "m"))] of
        Refuted env -> case Map.lookup (tv "n") env of
          Just n -> assertBool (show env) (n >= 0 && all (\m -> m * m /= n) [0 .. n])
          Nothing -> assertFailure ("the counterexample names no n: " <> show env)
        d -> assertFailure (show d)
  , testCase "a rigid variable is not at least a larger one" $
      assertBool "" (isRefuted (decide (rigid ["a", "b"]) [NatGe (v "a") (v "b")]))
  ]

properties :: [TestTree]
properties =
  [ testProperty "a condition without variables is decided by evaluation" prop_ground
  , testProperty "a ring identity holds over rigid variables" prop_identity
  , testProperty "a holds verdict is satisfied through its witness" prop_holds_sound
  , testProperty "a refutation has no solution at its counterexample" prop_refuted_sound
  , testProperty "under assumptions, a holds verdict is satisfied inside them" prop_assuming_holds_sound
  , testProperty "under assumptions, a refutation lies inside them" prop_assuming_refuted_sound
  , testProperty "with Int variables, a holds verdict is satisfied" prop_int_holds_sound
  , testProperty "with Int variables, a refutation has no solution" prop_int_refuted_sound
  ]

names :: [String]
names = ["a", "b", "c"]

genExpr :: Bool -> Int -> Gen NatExpr
genExpr partial n
  | n <= 0 = leaf
  | otherwise =
      frequency $
        [ (3, leaf)
        , (3, NatAdd <$> sub <*> sub)
        , (2, NatMul <$> sub <*> sub)
        ]
          <> (if partial then [(1, NatSub <$> sub <*> sub), (1, NatDiv <$> sub <*> sub)] else [])
  where
    sub = genExpr partial (n `div` 2)
    leaf = oneof [lit <$> choose (0, 4), v <$> elements names]

genGround :: Int -> Gen NatExpr
genGround n
  | n <= 0 = lit <$> choose (0, 6)
  | otherwise =
      oneof
        [ lit <$> choose (0, 6)
        , NatAdd <$> sub <*> sub
        , NatMul <$> sub <*> sub
        , NatSub <$> sub <*> sub
        , NatDiv <$> sub <*> sub
        ]
  where
    sub = genGround (n `div` 2)

genCondition :: Gen Expr2 -> Gen Condition
genCondition g = do
  (a, b) <- g
  elements [NatEq a b, NatGe a b]

type Expr2 = (NatExpr, NatExpr)

holdsAtEnv :: Map TVar Integer -> Condition -> Bool
holdsAtEnv env c = case c of
  NatEq a b -> defined ((==) <$> evalNat env a <*> evalNat env b)
  NatGe a b -> defined ((>=) <$> evalNat env a <*> evalNat env b)
  where
    defined = (== Just True)

prop_ground :: Property
prop_ground =
  forAll (genCondition ((,) <$> sized genGround <*> sized genGround)) $ \c ->
    let truth = holdsAtEnv Map.empty c
     in case decide Set.empty [c] of
          Holds _ -> truth
          Refuted _ -> not truth
          Unknown -> False

-- | Rewrites that preserve the value of a +/* expression.
rewrite :: NatExpr -> Gen NatExpr
rewrite e = case e of
  NatAdd a b -> do
    a' <- rewrite a
    b' <- rewrite b
    elements [NatAdd a' b', NatAdd b' a']
  NatMul a (NatAdd b c) -> do
    a' <- rewrite a
    b' <- rewrite b
    c' <- rewrite c
    elements [NatMul a' (NatAdd b' c'), NatAdd (NatMul a' b') (NatMul c' a')]
  NatMul a b -> do
    a' <- rewrite a
    b' <- rewrite b
    elements [NatMul a' b', NatMul b' a']
  _ -> return e

prop_identity :: Property
prop_identity =
  forAll (sized (genExpr False)) $ \e ->
    forAll (rewrite e) $ \e' ->
      isHolds (decide (freeNatVars e <> freeNatVars e') [NatEq e e'])

genRigid :: Gen (Set.Set TVar)
genRigid = Set.fromList . map tv <$> sublistOf names

samples :: [TVar] -> [Map TVar Integer]
samples rs = [Map.fromList (zip rs xs) | xs <- mapM (const [0 .. 3]) rs]

prop_holds_sound :: Property
prop_holds_sound =
  forAll genRigid $ \rs ->
    forAll (listOf1 (genCondition ((,) <$> resize 4 (sized (genExpr True)) <*> resize 4 (sized (genExpr True))))) $ \cs ->
      case decide rs cs of
        Holds witness ->
          let inferred = Set.toList (Set.unions [vars c | c <- cs] `Set.difference` rs)
           in conjoin
                [ counterexample (show env) (any (\inf -> all (holdsAtEnv inf) cs) (completions env witness inferred))
                | env <- samples (Set.toList rs)
                ]
        _ -> property True
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

-- | Natural assignments of the inferred variables at a rigid sample: those
-- with a witness take its value, the rest are searched.
completions :: Map TVar Integer -> Map TVar NatExpr -> [TVar] -> [Map TVar Integer]
completions env witness inferred =
  let free = [x | x <- inferred, not (Map.member x witness)]
   in [ full
      | xs <- mapM (const [0 .. 12]) free
      , let base = Map.union env (Map.fromList (zip free xs))
      , let full = Map.union base (Map.mapMaybe (evalNat base) witness)
      , all (>= 0) (Map.elems full)
      ]

prop_refuted_sound :: Property
prop_refuted_sound =
  forAll genRigid $ \rs ->
    forAll (listOf1 (genCondition ((,) <$> resize 4 (sized (genExpr True)) <*> resize 4 (sized (genExpr True))))) $ \cs ->
      case decide rs cs of
        Refuted env ->
          let inferred = Set.toList (Set.unions [vars c | c <- cs] `Set.difference` Map.keysSet env)
           in not (any (\inf -> all (holdsAtEnv inf) cs) (completions env Map.empty inferred))
        _ -> True
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

-- | Assumptions over the rigid variables: inequalities whose sides may
-- subtract, as a signature's Nat positions do.
genAssumptions :: [TVar] -> Gen [Condition]
genAssumptions rs
  | null rs = return []
  | otherwise = resize 2 (listOf (NatGe <$> resize 4 (sized rigidExpr) <*> pure (lit 0)))
  where
    rigidExpr n
      | n <= 0 = oneof [lit <$> choose (0, 4), NatVar <$> elements rs]
      | otherwise =
          frequency
            [ (2, oneof [lit <$> choose (0, 4), NatVar <$> elements rs])
            , (2, NatAdd <$> rigidExpr (n `div` 2) <*> rigidExpr (n `div` 2))
            , (2, NatSub <$> rigidExpr (n `div` 2) <*> rigidExpr (n `div` 2))
            , (1, NatMul <$> rigidExpr (n `div` 2) <*> rigidExpr (n `div` 2))
            ]

prop_assuming_holds_sound :: Property
prop_assuming_holds_sound =
  forAll genRigid $ \rs ->
    forAll (genAssumptions (Set.toList rs)) $ \asm ->
      forAll (listOf1 (genCondition ((,) <$> resize 4 (sized (genExpr True)) <*> resize 4 (sized (genExpr True))))) $ \cs ->
        case decideAssuming rs asm cs of
          Holds witness ->
            let inferred = Set.toList (Set.unions [vars c | c <- cs] `Set.difference` rs)
             in conjoin
                  [ counterexample (show env) (any (\inf -> all (holdsAtEnv inf) cs) (completions env witness inferred))
                  | env <- samples (Set.toList rs)
                  , all (holdsAtEnv env) asm
                  ]
          _ -> property True
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

prop_assuming_refuted_sound :: Property
prop_assuming_refuted_sound =
  forAll genRigid $ \rs ->
    forAll (genAssumptions (Set.toList rs)) $ \asm ->
      forAll (listOf1 (genCondition ((,) <$> resize 4 (sized (genExpr True)) <*> resize 4 (sized (genExpr True))))) $ \cs ->
        case decideAssuming rs asm cs of
          Refuted env
            | not (null asm) ->
                let inferred = Set.toList (Set.unions [vars c | c <- cs] `Set.difference` Map.keysSet env)
                    -- an assumption over the assigned variables holds there
                    relevant = [a | a <- asm, vars a `Set.isSubsetOf` Map.keysSet env]
                 in counterexample (show env)
                      ( all (holdsAtEnv env) relevant
                          && not (any (\inf -> all (holdsAtEnv inf) cs) (completions env Map.empty inferred))
                      )
          _ -> property True
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

-- | Expressions over the Nat variables of 'names' and the Int variables
-- "i" and "j", with small literals, so that the values a condition needs
-- stay inside the window 'intCompletions' searches.
genIntExpr :: Int -> Gen NatExpr
genIntExpr n
  | n <= 0 = leafI
  | otherwise =
      frequency
        [ (3, leafI)
        , (3, NatAdd <$> sub <*> sub)
        , (2, NatMul <$> sub <*> sub)
        , (2, NatSub <$> sub <*> sub)
        , (1, NatDiv <$> sub <*> sub)
        ]
  where
    sub = genIntExpr (n `div` 2)
    leafI = oneof [lit <$> choose (0, 2), v <$> elements names, IntVar . tv <$> elements ["i", "j"]]

genIntRigid :: Gen (Set.Set TVar)
genIntRigid = Set.fromList . map tv <$> sublistOf (names <> ["i", "j"])

-- | Values of the rigid variables: naturals for Nat ones, integers for Int.
intSamples :: [TVar] -> [Map TVar Integer]
intSamples rs = [Map.fromList (zip rs xs) | xs <- mapM range rs]
  where
    range r = if r `elem` [tv "i", tv "j"] then [-3 .. 3] else [0 .. 3]

-- | Assignments of the inferred variables at a sample: Nat ones natural,
-- Int ones any integer in a window.
intCompletions :: Map TVar Integer -> Map TVar NatExpr -> [TVar] -> [Map TVar Integer]
intCompletions env witness inferred =
  let free = [x | x <- inferred, not (Map.member x witness)]
      range x = if x `elem` [tv "i", tv "j"] then [-16 .. 16] else [0 .. 16]
   in [ full
      | xs <- mapM range free
      , let base = Map.union env (Map.fromList (zip free xs))
      , let full = Map.union base (Map.mapMaybe (evalNat base) witness)
      , and [x >= 0 | (k, x) <- Map.toList full, k `notElem` [tv "i", tv "j"]]
      ]

prop_int_holds_sound :: Property
prop_int_holds_sound =
  forAll genIntRigid $ \rs ->
    forAll (listOf1 (genCondition ((,) <$> resize 4 (sized genIntExpr) <*> resize 4 (sized genIntExpr)))) $ \cs ->
      case decide rs cs of
        Holds witness ->
          let inferred = Set.toList (Set.unions [vars c | c <- cs] `Set.difference` rs)
           in if length inferred > 2 then property True else conjoin
                [ counterexample (show env) (any (\inf -> all (holdsAtEnv inf) cs) (intCompletions env witness inferred))
                | env <- intSamples (Set.toList rs)
                ]
        _ -> property True
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b

prop_int_refuted_sound :: Property
prop_int_refuted_sound =
  forAll genIntRigid $ \rs ->
    forAll (listOf1 (genCondition ((,) <$> resize 4 (sized genIntExpr) <*> resize 4 (sized genIntExpr)))) $ \cs ->
      case decide rs cs of
        Refuted env ->
          let inferred = Set.toList (Set.unions [vars c | c <- cs] `Set.difference` Map.keysSet env)
           in if length inferred > 2 then property True else counterexample (show env) (not (any (\inf -> all (holdsAtEnv inf) cs) (intCompletions env Map.empty inferred)))
        _ -> property True
  where
    vars (NatEq a b) = freeNatVars a <> freeNatVars b
    vars (NatGe a b) = freeNatVars a <> freeNatVars b
