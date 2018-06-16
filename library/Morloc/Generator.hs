-- Generator.hs

{-| Generate target code

The role of the generator is the reverse of the role of the parser. A
parser takes a string and builds a data structure. The generator takes a
data structure and builds a string.

Around statements - wrap the function, can be nested to arbitrary depth.
assertions, effects, caches, filters -- all these are subsets of the
Around statement.
 * assert - determine whether to run the funtion
 * before - do something before running the function
 * after  - do something after
 * cache  - output is cached ? return cache : run function
 * filter - perform a function on the output
Since these wrap the function, they can be applied seperately
-}

module Morloc.Generator (
      generate
    , Nexus
    , Pool
  ) where

import Data.List (intercalate)
import qualified Data.Char as DC 

import Morloc.Graph
import Morloc.Data
import Morloc.Syntax
import Morloc.EvalError

data Script = Script {
      scriptBase :: String -- script basename (no extension)
    , scriptLang :: String -- script language
    , scriptCode :: String -- full script source code
  }
  deriving(Show, Ord, Eq)

type Nexus = Script
type Pool  = Script

generate :: Program -> ThrowsError (Nexus, [Pool])
generate p = return $ (generateNexus p, generatePools p)

-- | Create a script that calls the root node
generateNexus :: Program -> Nexus
generateNexus p = Script {
      scriptBase = "nexus"
    , scriptLang = "bash"
    , scriptCode = nexusCode' p
  }
  where
    nexusCode' :: Program -> String
    nexusCode' _ =  unlines [
          "# Bash"
        , ""
        , "Rscript pool.R m1"
      ]

-- | Create the code for each function pool
generatePools :: Program -> [Pool]
generatePools (Program w _ ps) = map (generatePool w) (zip ps [1..])

generatePool :: [FunctionTree] -> (Source, Integer) -> Pool 
generatePool fs ((Source lang lines), i) = Script {
      scriptBase = "pool" ++ show i
    , scriptLang = lang
    , scriptCode = poolCode lang fs lines
  }

poolCode :: String -> [FunctionTree] -> [String] -> String
poolCode = undefined
