{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : OwnershipCheckTests
Description : Which reads 'checkNative' reports as seeing a taken payload
-}
module OwnershipCheckTests (ownershipCheckTests) where

import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Ownership (LetMode (..), changedSlots)
import Morloc.CodeGenerator.OwnershipCheck (checkNative)

-- | OWN-1 over the take and borrow decisions a member renders: let 10 takes
-- a field of 1, let 11 binds a reference to a field of 1.
ownershipCheckTests :: TestTree
ownershipCheckTests = testGroup "ownership checks" [takeCases, slotCases]

-- | Which carried slots a continue reassigns.
slotCases :: TestTree
slotCases =
  testGroup
    "carried slots a continue changes"
    [ testCase "a slot given itself is left alone" $
        slots (changedSlots [1, 2] (Just [rd 1, IntN fv 0]) "ab") @?= [(2, 'b')]
    , testCase "a slot given another slot is changed" $
        slots (changedSlots [1, 2] (Just [rd 2, rd 1]) "ab") @?= [(1, 'a'), (2, 'b')]
    , testCase "without the IR every slot is changed" $
        slots (changedSlots [1, 2] Nothing "ab") @?= [(1, 'a'), (2, 'b')]
    ]
  where
    slots xs = [(i, v) | (i, v, _) <- xs]
    fv = FV (TV "a") (CV "a")
    rd i = LetVarN (VarF fv) i KeptRead

takeCases :: TestTree
takeCases =
  testGroup
    "no read sees a taken payload"
    -- a read of a value after its payload was taken
    [ testCase "spec-own-1-2" $
        bad (NativeLetN 10 (field 1) (tup [rd 10, rd 1]))
    -- a tag test after a take reads only the tag, where the member allows it
    , testCase "spec-own-1-3" $ do
        ok (NativeLetN 10 (field 1) (tup [rd 10, tag 1]))
        assertBool "no tag test after a take" (not (null (checkNative mode False Set.empty (NativeLetN 10 (field 1) (tup [rd 10, tag 1])))))
    -- the arm that does not take may read the value
    , testCase "spec-own-1-4" $
        ok (IfN ty (rd 2) (NativeLetN 10 (field 1) (rd 10)) (rd 1))
    -- a take and a read in one step, whose parts run in either order
    , testCase "spec-own-1-5" $
        bad (tup [NativeLetN 10 (field 1) (rd 10), rd 1])
    -- a reference into a value, read after the value's payload was taken
    , testCase "spec-own-1-6" $
        bad (NativeLetN 11 (field 1) (NativeLetN 10 (field 1) (tup [rd 10, rd 11])))
    -- a loop slot taken from is rebound by the back edge
    , testCase "spec-own-1-7" $ do
        ok (loop [(1, IntN fv 0)] (LoopNLet 10 (field 1) (LoopContinue [rd 10])))
        bad (loop [(1, IntN fv 0)] (LoopNLet 10 (field 1) (LoopContinue [tup [rd 10, rd 1]])))
    -- a body that may run more than once may not take from outside itself
    , testCase "spec-own-1-8" $
        bad (DoBlockN ty (NativeLetN 10 (field 1) (rd 10)))
    -- a value bound again after a take is a new value
    , testCase "spec-own-1-9" $
        ok (NativeLetN 10 (field 1) (NativeLetN 1 (IntN fv 0) (tup [rd 10, rd 1])))
    -- a body that may run more than once may take from what it binds itself
    , testCase "spec-own-1-10" $
        ok (DoBlockN ty (NativeLetN 1 (tup [IntN fv 0]) (NativeLetN 10 (field 1) (rd 10))))
    -- a copy of a value shares its storage, so reading the copy after a take
    -- reads the taken payload
    , testCase "spec-own-1-11" $
        bad (NativeLetN 2 (rd 1) (NativeLetN 10 (field 1) (tup [rd 10, rd 2])))
    -- a body that may run later captures what it reads, so a later take is
    -- seen when it runs
    , testCase "spec-own-1-12" $
        bad (NativeLetN 3 (DoBlockN ty (rd 1)) (NativeLetN 10 (field 1) (tup [rd 10, rd 3])))
    -- a reference to an element of a list is a reference into the list
    , testCase "spec-own-1-13" $
        bad (NativeLetN 11 (IntrinsicN ty IntrListElem Nothing [rd 1, IntN fv 0]) (NativeLetN 10 (field 1) (tup [rd 10, rd 11])))
    -- a let moved at its last read in a value being built is not read again
    , testCase "spec-own-1-15" $ do
        okMoving (NativeLetN 2 (IntN fv 0) (tup [lastRd 2]))
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (tup [lastRd 2]) (rd 2)))
    -- a move and another read of the same local in one step
    , testCase "spec-own-1-16" $
        badMoving (NativeLetN 2 (IntN fv 0) (tup [lastRd 2, rd 2]))
    -- a reference into a moved local sees the move; a copy of it does not
    , testCase "spec-own-1-17" $ do
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 11 (field 2) (NativeLetN 3 (tup [lastRd 2]) (rd 11))))
        okMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 12 (field 2) (NativeLetN 3 (tup [lastRd 2]) (rd 12))))
    -- every keeping position moves: a constructor, a record, an optional and
    -- a loop's next values
    , testCase "spec-own-1-18" $ do
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (VariantN ty "Link" 1 [lastRd 2]) (rd 2)))
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (RecordN NamRecord fv [] [(Key "a", lastRd 2)]) (rd 2)))
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (CoerceN CoerceToOptional ty (lastRd 2)) (rd 2)))
        badMoving (LoopN ty [(1, IntN fv 0)] (LoopNLet 2 (IntN fv 0) (LoopContinue [tup [lastRd 2, rd 2]])))
    -- a copy of a moved local is its own value; a reference into the copy is
    -- not a reference into what was moved, and the reverse
    , testCase "spec-own-1-19" $ do
        okMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 12 (rd 2) (tup [lastRd 2, rd 12])))
        badMoving (NativeLetN 1 (IntN fv 0) (NativeLetN 2 (rd 1) (NativeLetN 11 (field 2) (NativeLetN 3 (tup [lastRd 2]) (rd 11)))))
    -- a value moved after a body that may run later read it
    , testCase "spec-own-1-20" $
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 4 (DoBlockN ty (rd 2)) (tup [lastRd 2, rd 4])))
    -- a tag test after a move
    , testCase "spec-own-1-21" $
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (tup [lastRd 2]) (tag 2)))
    -- a let bound to a local at its last read, directly or at the end of a
    -- chain of lets, moves it, and the new let holds the value itself
    , testCase "spec-own-1-22" $ do
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (lastRd 2) (rd 2)))
        badMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (NativeLetN 20 (IntN fv 1) (lastRd 2)) (rd 2)))
        okMoving (NativeLetN 2 (IntN fv 0) (NativeLetN 3 (lastRd 2) (tup [rd 3, rd 3])))
    ]
  where
    ok e = checkNative mode True Set.empty e @?= []
    okMoving e = checkNative mode True (Set.fromList [2]) e @?= []
    badMoving e = assertBool "a read sees a moved local" (not (null (checkNative mode True (Set.fromList [2]) e)))
    lastRd i = LetVarN ty i LastRead
    bad e = assertBool "a read sees a taken payload" (not (null (checkNative mode True Set.empty e)))
    mode i _
      | i == 10 = LetTake
      | i == 11 = LetBorrow
      | otherwise = LetCopy
    fv = FV (TV "a") (CV "a")
    ty = VarF fv
    rd i = LetVarN ty i KeptRead
    tup = TupleN (FV (TV "t") (CV "t"))
    field i = IntrinsicN ty IntrCtorField Nothing [rd i, StrN fv "Link", IntN fv 1]
    tag i = IntrinsicN ty IntrTagTest Nothing [rd i, StrN fv "Link"]
    loop starts body = LoopN ty starts body
