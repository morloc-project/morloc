module Morloc.Util
(
    which
  , zipA
  , indent
) where

-- Find the 1-based indices of all true values
which :: [Bool] -> [Int]
which = map fst . filter snd . zip [0..]

-- This function has the same form as an applicative <*>, but is NOT the same,
-- since the applicative list typeclass takes the product of fs and xs.
zipA :: [(a -> b)] -> [a] -> [b]
zipA (f:fs) (x:xs) = (f x):(zipA fs xs)
zipA _ _ = []

indent :: Int -> String -> String
indent i s
  | i <= 0    = s
  | otherwise = unlines . map ((++) (take i (repeat ' '))) . lines $ s
