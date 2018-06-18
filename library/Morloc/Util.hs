module Morloc.Util
(
    which
) where

-- Find the 1-based indices of all true values
which :: [Bool] -> [Int]
which = map fst . filter snd . zip [0..]
