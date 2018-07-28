module Morloc.Util
(
    ifelse
  , conmap
  , unique
  , sort
  , repeated
  , indent
  , maybe2bool
  , either2bool
  , maybeOne
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

indent :: Int -> String -> String
indent i s
  | i <= 0    = s
  | otherwise = unlines . map ((++) (take i (repeat ' '))) . lines $ s

maybe2bool :: Maybe a -> Bool
maybe2bool (Just _) = True
maybe2bool Nothing = False

either2bool :: Either a b -> Bool
either2bool (Left _) = False
either2bool (Right _) = True

maybeOne :: [a] -> Maybe a
maybeOne [x] = Just x
maybeOne _  = Nothing
