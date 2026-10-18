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
import Morloc.CodeGenerator.Grammars.Common (D (..), sealLoop)
import Morloc.CodeGenerator.Grammars.Translator.Imperative (RNode (..), loopBorrowLocals, markLastReads, markLastReadsInBody, markLastReadsInNative, markLastReadsWith)
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
    -- a local bound to a match whose every arm is a part, or fails, is a borrow
    , testCase "spec-own-6-10" $
        borrows [(1, par 5)] (LoopNLet 10 (IfN ty (tag 1 "End") (rd 1) (IfN ty (tag 1 "Link") (NativeLetN 11 (field 1) (rd 11)) failed)) (LoopContinue [rd 10]))
          @?= [1, 10, 11]
    -- a match with an arm that builds a value owns its value
    , testCase "spec-own-6-11" $
        borrows [(1, par 5)] (LoopNLet 10 (IfN ty (tag 1 "End") (tup [rd 1]) (field 1)) (LoopContinue [rd 10]))
          @?= []
    -- a match whose arm binds a type that may not be borrowed owns its value
    , testCase "spec-own-6-12" $
        borrowsIf (/= cheap) [(1, par 5)] (LoopNLet 10 (IfN ty (tag 1 "Link") (NativeLetN 11 (IntrinsicN cheap IntrCtorField Nothing [rd 1, StrN fv "Link", IntN fv 0]) (rd 11)) (rd 1)) (LoopContinue [rd 10]))
          @?= []
    -- a conditional on anything but constructor tags owns its value
    , testCase "spec-own-6-13" $
        borrows [(1, par 5)] (LoopNLet 10 (IfN ty (rd 2) (field 1) (rd 1)) (LoopContinue [rd 10]))
          @?= []
    -- a let outside a loop naming part of a borrow is a borrow, and so is
    -- one naming part of it
    , testCase "spec-own-6-15" $
        Set.toList (loopBorrowLocals (const True) (Set.singleton 5) (RN (NativeLetN 10 (field 5) (NativeLetN 11 (field 10) (rd 11)))))
          @?= [10, 11]
    -- a nested pattern: a test of both tags, bound to a local, then a field
    -- of a field
    , testCase "spec-own-6-16" $
        borrows [(1, par 5)] (LoopContinue [IfN ty (NativeLetN 12 (IfN ty (tag 1 "Link") (IntrinsicN ty IntrTagTest Nothing [field 1, StrN fv "Link"]) (LogN fv False)) (rd 12)) (NativeLetN 11 (IntrinsicN ty IntrCtorField Nothing [field 1, StrN fv "Link", IntN fv 1]) (rd 11)) (rd 1)])
          @?= [1, 11]
    ]
  where
    par i = BndVarN ty i KeptRead
    tag i n = IntrinsicN ty IntrTagTest Nothing [rd i, StrN fv n]
    failed = IntrinsicN ty IntrThrow Nothing [StrN fv "no clause matched the argument"]
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
    -- a read of a local that refers into a value is a read of the value, in
    -- its own step
    , testCase "spec-own-2-7" $
        marksWith (Map.fromList [(10, 1)]) (loop [1] (LoopNLet 10 (rd 1) (LoopBase (tup [rd 10, rd 1]))))
          @?= [[], [10]]
    -- and after it
    , testCase "spec-own-2-8" $ do
        marks (loop [1] (LoopNLet 10 (rd 1) (LoopNLet 11 (rd 1) (LoopBase (rd 10)))))
          @?= [[], [1], [10]]
        marksWith (Map.fromList [(10, 1)]) (loop [1] (LoopNLet 10 (rd 1) (LoopNLet 11 (rd 1) (LoopBase (rd 10)))))
          @?= [[], [], [10]]
    -- a chain of locals referring into one another counts toward each link
    , testCase "spec-own-2-9" $
        marksWith (Map.fromList [(10, 1), (11, 10)]) (loop [1] (LoopNLet 10 (rd 1) (LoopNLet 11 (rd 10) (LoopNLet 12 (rd 10) (LoopBase (rd 11))))))
          @?= [[], [], [], [11]]
    -- outside a loop, the later of two reads in different steps is the last
    , testCase "spec-own-2-10" $
        bodyMarks (lets [IntN fv 0, tup [rd 10], tup [rd 10, rd 11]])
          @?= [[], [], [10, 11]]
    -- two reads in one step are both kept
    , testCase "spec-own-2-11" $
        bodyMarks (lets [IntN fv 0, tup [rd 10, rd 10]])
          @?= [[], []]
    -- a conditional's reads are counted along its paths: one read in each
    -- arm is the last read in each
    , testCase "spec-own-2-12" $ do
        bodyMarks (lets [IntN fv 0, IfN ty (rd 2) (rd 10) (tup [rd 10, IntN fv 1])])
          @?= [[], [10, 10]]
        bodyMarks (lets [IntN fv 0, IfN ty (rd 2) (rd 10) (tup [rd 10, rd 10])])
          @?= [[], [10]]
    -- a conditional bound by a let marks each arm as a body of its own, its
    -- lets included
    , testCase "spec-own-2-13" $
        bodyMarks (lets [IntN fv 0, IfN ty (rd 2) (NativeLetN 20 (rd 10) (tup [rd 20, rd 10])) (rd 10)])
          @?= [[], [20, 10, 10]]
    -- a read after a let-bound conditional keeps the reads in its arms
    , testCase "spec-own-2-14" $
        bodyMarks (lets [IntN fv 0, IfN ty (rd 2) (NativeLetN 20 (rd 10) (tup [rd 20, rd 10])) (rd 10), tup [rd 10]])
          @?= [[], [20], [10]]
    -- the same in a loop body
    , testCase "spec-own-2-15" $
        marks (loop [1] (LoopNLet 10 (IfN ty (rd 2) (NativeLetN 20 (rd 1) (tup [rd 20])) (rd 1)) (LoopBase (tup [rd 10, rd 1]))))
          @?= [[20], [10, 1]]
    -- and in a native manifold's body, where a let's right-hand side that is
    -- a body of lets marks its own lets
    , testCase "spec-own-2-16" $
        nativeMarks (NativeLetN 10 (IntN fv 0) (NativeLetN 11 (NativeLetN 20 (rd 10) (tup [rd 20])) (tup [rd 11])))
          @?= [10, 20, 11]
    -- a loop's next values run the lets they depend on first, one value
    -- after another, each a step of its own
    , testCase "spec-own-2-17" $ do
        sealed (sealLoop (LoopContinue [D (rd 10) [(10, Right (tup [rd 1]))], D (rd 12) [(12, Right (tup [rd 11])), (11, Right (rd 1))]]))
          @?= ([10, 11, 12], [False, False])
        -- a let two values share is bound once
        sealed (sealLoop (LoopContinue [D (rd 10) [(10, Right (rd 1))], D (tup [rd 10]) [(10, Right (rd 1))]]))
          @?= ([10], [False, False])
        -- a leaf that depends on a serial let keeps its lets in its values
        sealed (sealLoop (LoopContinue [D (rd 10) [(10, Right (rd 1))], D (rd 11) [(11, Left (LetVarS Nothing 3))]]))
          @?= ([], [True, False])
    -- a do block a let's value holds through a conditional's arm holds what
    -- it reads
    , testCase "spec-own-3-3" $
        bodyMarks (lets [IntN fv 0, IfN ty (rd 2) (DoBlockN ty (rd 10)) (DoBlockN ty (IntN fv 1)), tup [rd 10, rd 11]])
          @?= [[], [], [11]]
    -- a value a do block bound by a let reads is never moved, before the
    -- block or after it
    , testCase "spec-own-3-2" $
        bodyMarks (lets [IntN fv 0, DoBlockN ty (rd 10), tup [rd 10, rd 11]])
          @?= [[], [], [11]]
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
    -- the same, where lets refer into the values they are bound to
    , testProperty "spec-own-1-14" $
        forAll ((,) <$> genBody <*> genAliases) $ \(b, aliases) ->
          let b' = case markLastReadsWith aliases Set.empty (loop carriedVars b) of
                LoopN _ _ m -> m
                _ -> b
           in all (pathSoundWith aliases) (paths b')
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
marks = marksWith Map.empty

-- | A serial body of native lets 10, 11, ... bound to the given right-hand
-- sides in turn.
lets :: [NativeExpr] -> SerialExpr
lets = go 10
  where
    go i (x : xs) = NativeLetS i x (go (i + 1) xs)
    go _ [] = ReturnS (LetVarS Nothing 0)

-- | The variables each let of a body marks, in order.
bodyMarks :: SerialExpr -> [[Int]]
bodyMarks = spine . markLastReadsInBody
  where
    spine (NativeLetS _ x b) = marked x : spine b
    spine _ = []

-- | The loop lets in front of a sealed continue, in order, and whether each
-- of its values still binds a let of its own.
sealed :: LoopBody NativeExpr SerialExpr NativeExpr -> ([Int], [Bool])
sealed (LoopNLet i _ k) = let (is, vs) = sealed k in (i : is, vs)
sealed (LoopContinue xs) = ([], [case x of NativeLetN {} -> True; _ -> False | x <- xs])
sealed _ = ([], [])

-- | The variables a native body marks, in the order they are written.
nativeMarks :: NativeExpr -> [Int]
nativeMarks = marked . markLastReadsInNative

-- | 'marks' with locals that refer into other values.
marksWith :: Map.Map Int Int -> NativeExpr -> [[Int]]
marksWith aliases e = case markLastReadsWith aliases Set.empty e of
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
marked (IfN _ c a b) = marked c <> marked a <> marked b
marked (NativeLetN _ x b) = marked x <> marked b
marked _ = []

carriedVars :: [Int]
carriedVars = [1, 2, 3]

-- | A step of one path through an iteration: whether it is a guard, the
-- variable it binds, and its expression.
data Step = Step Bool (Maybe Int) NativeExpr

paths :: LoopBody NativeExpr SerialExpr NativeExpr -> [[Step]]
paths (LoopIf g t f) = map (Step True Nothing g :) (paths t <> paths f)
paths (LoopNLet i x b) = [p <> q | p <- spinePaths i x, q <- paths b]
paths (LoopSLet _ _ b) = paths b
paths (LoopBase x) = [[Step False Nothing x]]
paths (LoopContinue xs) = [[Step False Nothing (tup xs)]]

-- | The paths through a right-hand side bound to @i@: a conditional's test is
-- a step and each arm a path, a let's right-hand side a body of its own, and
-- the value the body ends in a step that binds @i@.
spinePaths :: Int -> NativeExpr -> [[Step]]
spinePaths i (IfN _ c a b) = map (Step False Nothing c :) (spinePaths i a <> spinePaths i b)
spinePaths i (NativeLetN j x b) = [p <> q | p <- spinePaths j x, q <- spinePaths i b]
spinePaths i x = [[Step False (Just i) x]]

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
pathSound = pathSoundWith Map.empty

-- | 'pathSound' where a read of a local is also a read of every value its
-- chain of aliases passes through.
pathSoundWith :: Map.Map Int Int -> [Step] -> Bool
pathSoundWith aliases = go []
  where
    -- genAliases only refers to lower indices, so a chain ends.
    through j = j : maybe [] through (Map.lookup j aliases)
    go _ [] = True
    go moved (Step guard binds x : rest) =
      let r = Map.fromListWith (+) [(v, k) | (j, k) <- Map.toList (reads' x), v <- through j]
          ms = marked x
       in null (closureMarks x)
            && all (`Map.notMember` r) moved
            && (null ms || not guard)
            && all (\v -> Map.lookup v r == Just 1) ms
            && go (filter (\v -> Just v /= binds) (moved <> ms)) rest

-- | Lets 10 and up referring into a carried value or an earlier let.
genAliases :: Gen (Map.Map Int Int)
genAliases = Map.fromList . concat <$> mapM pick [10 .. 14]
  where
    pick i = frequency [(1, pure []), (1, (\t -> [(i, t)]) <$> elements (carriedVars <> [10 .. i - 1]))]

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
            , (2, LoopNLet next <$> genSpine (100 * next) 2 <*> go (next + 1) (n - 1))
            ]
    leaf = oneof [LoopBase <$> genExpr, LoopContinue <$> vectorOf (length carriedVars) genExpr]

-- | A right-hand side that is a conditional or a let, its lets numbered from
-- @base@ and read by what follows them.
genSpine :: Int -> Int -> Gen NativeExpr
genSpine base depth
  | depth <= 0 = genExprWith (inner base)
  | otherwise =
      frequency
        [ (1, genExprWith (inner base))
        , (2, IfN ty <$> genExprWith (inner base) <*> genSpine (base + 10) (depth - 1) <*> genSpine (base + 20) (depth - 1))
        , (2, NativeLetN base <$> genExprWith (inner base) <*> genSpine (base + 1) (depth - 1))
        ]
  where
    inner b = [b - k | k <- [1 .. 3], b - k > 100 * (b `div` 100)]

genExpr :: Gen NativeExpr
genExpr = genExprWith []

-- | 'genExpr', also reading the given locals.
genExprWith :: [Int] -> Gen NativeExpr
genExprWith extra = sized (\n -> go (min n 4))
  where
    go :: Int -> Gen NativeExpr
    go 0 = oneof [rd <$> elements (carriedVars <> [10, 11, 12, 7] <> extra), pure (IntN fv 0)]
    go n =
      frequency
        [ (3, go 0)
        , (2, tup <$> (choose (0, 3) >>= \k -> vectorOf k (go (n - 1))))
        , (1, DoBlockN ty <$> go (n - 1))
        ]
