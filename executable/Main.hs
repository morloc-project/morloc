module Main (main) where

import Morloc 

main :: IO ()
main = interact (parseMorloc "stdin")
