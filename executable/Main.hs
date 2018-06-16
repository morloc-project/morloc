module Main (main) where

import Morloc 

main :: IO ()
main = interact (show . build)
