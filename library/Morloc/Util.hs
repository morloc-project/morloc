module Morloc.Util
(
    ifelse
  , conmap
  , unique
  , sort
  , repeated
) where

import qualified Data.List as DL

conmap :: (a -> [b]) -> [a] -> [b]
conmap f = concat . map f

ifelse :: Bool -> a -> a -> a
ifelse True  x _ = x
ifelse False _ y = y

unique :: Eq a => [a] -> [a]
unique = DL.nub

sort :: Ord a => [a] -> [a]
sort = DL.sort

repeated :: Ord a => [a] -> [a]
repeated xs = [y | (y:(_:_)) <- (DL.group . DL.sort) xs]
