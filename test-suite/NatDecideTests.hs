{- |
Module      : NatDecideTests
Description : Examples and properties of the Nat decision procedure (DIM-7)

The properties compare every verdict of 'decide' with evaluation: a
condition with no variables is decided exactly, a verdict of holds is
checked at sampled values of the rigid variables through its witness, and a
refutation is checked by searching the other variables at its
counterexample.
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
  , testCase "a rigid variable is not at least a larger one" $
      assertBool "" (isRefuted (decide (rigid ["a", "b"]) [NatGe (v "a") (v "b")]))
  ]

properties :: [TestTree]
properties =
  [ testProperty "a condition without variables is decided by evaluation" prop_ground
  , testProperty "a ring identity holds over rigid variables" prop_identity
  , testProperty "a holds verdict is satisfied through its witness" prop_holds_sound
  , testProperty "a refutation has no solution at its counterexample" prop_refuted_sound
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

-- | Assignments of the inferred variables at a rigid sample: those with a
-- witness take its value, the rest are searched.
completions :: Map TVar Integer -> Map TVar NatExpr -> [TVar] -> [Map TVar Integer]
completions env witness inferred =
  let free = [x | x <- inferred, not (Map.member x witness)]
   in [ Map.union base (Map.mapMaybe (evalNat base) witness)
      | xs <- mapM (const [0 .. 12]) free
      , let base = Map.union env (Map.fromList (zip free xs))
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
