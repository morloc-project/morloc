module Morloc.Util
(
    ifelse
) where

ifelse :: Bool -> a -> a -> a
ifelse True  x _ = x
ifelse False _ y = y
