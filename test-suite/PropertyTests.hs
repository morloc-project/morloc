module PropertyTests
  ( propertyTests
  ) where

import Morloc.Namespace

import qualified Control.Monad as CM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import Safe (headMay)
import Test.Tasty
import Test.Tasty.QuickCheck as TQC

propertyTests =
  testGroup
    "internal list function properties"
    [ TQC.testProperty "unique makes unique lists" prop_unique_unique
    , TQC.testProperty "unique preserves original order" prop_unique_preserves_order
    , TQC.testProperty "duplicates makes unique lists" prop_duplicates_unique
    , TQC.testProperty "duplicates preserves original order" prop_duplicates_preserves_order
    ]

-- for the uniq family of functions (unique, duplicates, isSorted), I will test
-- on the numbers 1 to 5. If the desired property holds over this set, they
-- will hold over any ordered set. 
one2five :: [Int] -> [Int]
one2five = map (\x -> mod (abs x) 5)

prop_unique_unique :: [Int] -> Bool
prop_unique_unique [] = True
prop_unique_unique xs =
  let xs' = one2five xs
  in length (unique xs') == Set.size (Set.fromList xs') 

-- This test asserts that the first element in the original and unique list is
-- the same. This guarantee alone does not entirely guantee that the original
-- order is preserved, but it is close.
prop_unique_preserves_order :: [Int] -> Bool
prop_unique_preserves_order xs = headMay xs == headMay (unique xs)

-- Each element in the duplicates return list is unique
prop_duplicates_unique :: [Int] -> Bool
prop_duplicates_unique [] = True
prop_duplicates_unique xs =
  let xs' = duplicates (one2five xs)
  in length xs' == Set.size (Set.fromList xs')

prop_duplicates_preserves_order :: [Int] -> Bool
prop_duplicates_preserves_order xs = f Set.empty xs (duplicates xs) where
  f _ _ [] = True
  f _ [] _ = False
  f skipped (y:rs) (y':rs')
    -- if the original and duplicated elements match:
    | y == y' =
      -- if the current element was previously skipped
      if Set.member y' skipped
      -- then the duplicates function failed to respect the initial order
      then False
      -- else continue checking on the next elements
      else f skipped rs rs'
    -- otherwise store record the skipped value and continue
    | otherwise = f (Set.insert y skipped) rs (y':rs')
