{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : LastReadTests
Description : Which reads of a loop's values 'markLastReads' marks as their last
-}
module LastReadTests (lastReadTests) where

import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Translator.Imperative (RNode (..), loopBorrowLocals, markLastReads)
import Morloc.CodeGenerator.Namespace

lastReadTests :: TestTree
lastReadTests = testGroup "ownership in native loops" [lastReadCases, borrowCases]

-- | Which locals of a loop over borrowed parameter 5 hold a borrow (OWN-6).
borrowCases :: TestTree
borrowCases =
  testGroup
    "borrowed loop locals"
    -- a walk into what it was lent carries a borrow
    [ testCase "spec-own-6-2" $
        borrows [(1, par 5)] (LoopIf (rd 1) (LoopBase (IntN fv 0)) (LoopNLet 10 (field 1) (LoopContinue [rd 10])))
          @?= [1, 10]
    -- an alias of a borrow is a borrow
    , testCase "spec-own-6-3" $
        borrows [(1, par 5)] (LoopNLet 10 (rd 1) (LoopNLet 11 (field 10) (LoopContinue [rd 11])))
          @?= [1, 10, 11]
    -- a fresh value in any continue makes the local own its value
    , testCase "spec-own-6-4" $
        borrows [(1, par 5)] (LoopIf (rd 1) (LoopContinue [field 1]) (LoopContinue [tup [rd 1]]))
          @?= []
    -- a local replaced by one that owns its value owns its own
    , testCase "spec-own-6-5" $
        borrows [(1, par 5), (2, par 5)] (LoopContinue [rd 2, tup [rd 1]])
          @?= []
    -- a local started from a value it was not lent owns it
    , testCase "spec-own-6-6" $
        borrows [(1, IntN fv 0)] (LoopContinue [rd 1])
          @?= []
    -- a let naming part of an owned value owns its value
    , testCase "spec-own-6-7" $
        borrows [(1, par 5), (2, par 5)] (LoopNLet 10 (field 2) (LoopContinue [rd 1, tup [rd 10]]))
          @?= [1]
    -- a part of a type that may not be borrowed is a value
    , testCase "spec-own-6-9" $
        borrowsIf (/= cheap) [(1, par 5)] (LoopNLet 10 (IntrinsicN cheap IntrCtorField Nothing [rd 1, StrN fv "Link", IntN fv 0]) (LoopNLet 11 (field 1) (LoopContinue [rd 11])))
          @?= [1, 11]
    -- an index bound twice is never a borrow, nor is a local replaced by it
    , testCase "spec-own-6-8" $
        borrows [(1, par 5)] (LoopIf (rd 1) (LoopNLet 10 (field 1) (LoopContinue [rd 10])) (LoopNLet 10 (tup [rd 1]) (LoopBase (rd 10))))
          @?= []
    ]
  where
    par i = BndVarN ty i KeptRead
    cheap = VarF (FV (TV "i") (CV "i"))
    field i = IntrinsicN ty IntrCtorField Nothing [rd i, StrN fv "Link", IntN fv 1]
    borrows = borrowsIf (const True)
    borrowsIf ok starts b = Set.toList (loopBorrowLocals ok (Set.singleton 5) (RN (LoopN ty starts b)))

lastReadCases :: TestTree
lastReadCases =
  testGroup
    "last reads"
    -- a read with none after it is marked
    [ testCase "spec-own-2-1" $
        marks (loop [1] (LoopNLet 10 (rd 1) (LoopBase (rd 10))))
          @?= [[1], [10]]
    -- only the later of two reads is marked
    , testCase "spec-own-2-2" $
        marks (loop [1] (LoopNLet 10 (rd 1) (LoopBase (tup [rd 10, rd 1]))))
          @?= [[], [10, 1]]
    -- two reads in one step are both kept
    , testCase "spec-own-2-3" $
        marks (loop [1] (LoopBase (tup [rd 1, rd 1])))
          @?= [[]]
    -- the continue's values are one step
    , testCase "spec-own-2-4" $
        marks (loop [1, 2] (LoopContinue [rd 1, tup [rd 2, rd 1]]))
          @?= [[2]]
    -- a read in a closure is kept and keeps every earlier read
    , testCase "spec-own-3-1" $
        marks (loop [1] (LoopNLet 10 (rd 1) (LoopBase (DoBlockN ty (rd 1)))))
          @?= [[], []]
    -- each branch marks its own last read
    , testCase "spec-own-2-5" $
        marks (loop [1] (LoopIf (rd 2) (LoopBase (rd 1)) (LoopContinue [tup [rd 1]])))
          @?= [[], [1], [1]]
    -- a guard is never marked
    , testCase "spec-own-4-1" $
        marks (loop [1] (LoopIf (rd 1) (LoopBase (IntN fv 0)) (LoopBase (IntN fv 1))))
          @?= [[], [], []]
    -- a value the loop does not own is never marked
    , testCase "spec-own-2-6" $
        marks (loop [1] (LoopBase (rd 7)))
          @?= [[]]
    -- a marked read is the only read of its step and none follows on any path
    , testProperty "spec-own-1-1" $
        forAll genBody $ \b ->
          let b' = case markLastReads (loop carriedVars b) of
                LoopN _ _ m -> m
                _ -> b
           in checkCoverage $
                cover 30 (any (\(Step _ _ x) -> not (null (marked x))) (concat (paths b'))) "marks a read" $
                  all pathSound (paths b')
    ]

ty :: TypeF
ty = VarF fv

fv :: FVar
fv = FV (TV "a") (CV "a")

rd :: Int -> NativeExpr
rd i = LetVarN ty i KeptRead

tup :: [NativeExpr] -> NativeExpr
tup = TupleN (FV (TV "t") (CV "t"))

loop :: [Int] -> LoopBody NativeExpr SerialExpr NativeExpr -> NativeExpr
loop ids = LoopN ty [(i, IntN fv 0) | i <- ids]

-- | The variables each step marks, in the order the steps are written.
marks :: NativeExpr -> [[Int]]
marks e = case markLastReads e of
  LoopN _ _ b -> steps b
  _ -> []
  where
    steps (LoopIf g t f) = marked g : steps t <> steps f
    steps (LoopNLet _ x b) = marked x : steps b
    steps (LoopSLet _ _ b) = steps b
    steps (LoopBase x) = [marked x]
    steps (LoopContinue xs) = [concatMap marked xs]

marked :: NativeExpr -> [Int]
marked (LetVarN _ i LastRead) = [i]
marked (TupleN _ xs) = concatMap marked xs
marked (DoBlockN _ x) = marked x
marked _ = []

carriedVars :: [Int]
carriedVars = [1, 2, 3]

-- | A step of one path through an iteration: whether it is a guard, the
-- variable it binds, and its expression.
data Step = Step Bool (Maybe Int) NativeExpr

paths :: LoopBody NativeExpr SerialExpr NativeExpr -> [[Step]]
paths (LoopIf g t f) = map (Step True Nothing g :) (paths t <> paths f)
paths (LoopNLet i x b) = map (Step False (Just i) x :) (paths b)
paths (LoopSLet _ _ b) = paths b
paths (LoopBase x) = [[Step False Nothing x]]
paths (LoopContinue xs) = [[Step False Nothing (tup xs)]]

-- | Reads of each variable, a read inside a closure counting twice.
reads' :: NativeExpr -> Map.Map Int Int
reads' (LetVarN _ i _) = Map.singleton i 1
reads' (TupleN _ xs) = Map.unionsWith (+) (map reads' xs)
reads' (DoBlockN _ x) = Map.map (* 2) (reads' x)
reads' _ = Map.empty

closureMarks :: NativeExpr -> [Int]
closureMarks (TupleN _ xs) = concatMap closureMarks xs
closureMarks (DoBlockN _ x) = marked x
closureMarks _ = []

-- | Run one path, moving each variable at its marked read: no step may read
-- a moved variable, and a step that moves one reads it nowhere else. A let
-- binds its variable afresh. The continue reassigns every carried variable
-- and a base leaf ends the loop, so one path is one whole iteration.
pathSound :: [Step] -> Bool
pathSound = go []
  where
    go _ [] = True
    go moved (Step guard binds x : rest) =
      let r = reads' x
          ms = marked x
       in null (closureMarks x)
            && all (`Map.notMember` r) moved
            && (null ms || not guard)
            && all (\v -> Map.lookup v r == Just 1) ms
            && go (filter (\v -> Just v /= binds) (moved <> ms)) rest

genBody :: Gen (LoopBody NativeExpr SerialExpr NativeExpr)
genBody = sized (go (10 :: Int))
  where
    go next n
      | n <= 0 = leaf
      | otherwise =
          frequency
            [ (2, leaf)
            , (2, LoopIf <$> genExpr <*> go next (n `div` 2) <*> go next (n `div` 2))
            , (3, LoopNLet next <$> genExpr <*> go (next + 1) (n - 1))
            ]
    leaf = oneof [LoopBase <$> genExpr, LoopContinue <$> vectorOf (length carriedVars) genExpr]

genExpr :: Gen NativeExpr
genExpr = sized (\n -> go (min n 4))
  where
    go :: Int -> Gen NativeExpr
    go 0 = oneof [rd <$> elements (carriedVars <> [10, 11, 12, 7]), pure (IntN fv 0)]
    go n =
      frequency
        [ (3, go 0)
        , (2, tup <$> (choose (0, 3) >>= \k -> vectorOf k (go (n - 1))))
        , (1, DoBlockN ty <$> go (n - 1))
        ]
