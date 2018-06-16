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
import Morloc.Language
import Morloc.Syntax
import Morloc.EvalError

data Script = Script {
      scriptBasename :: String     -- script basename (no extension)
    , scriptLang :: Lang
    , scriptCode :: String
  }
  deriving(Show, Ord, Eq)

type Nexus = Script
type Pool  = Script

generate :: Program -> ThrowsError (Nexus, [Pool])
generate p = return $ (generateNexus p, generatePools p)

-- | Create a script that calls the root node
generateNexus :: Program -> Nexus
generateNexus p = Script {
      scriptBasename    = "nexus"
    , scriptLang        = LangBash
    , scriptCode        = nexusCode' p
  }
  where
    nexusCode' :: Program -> String
    nexusCode' _ = "hello world"

-- | Create the code for each function pool
generatePools :: Program -> [Pool]
generatePools (Program _ _ ps) = map generatePool' (zip ps [1..])
  where
    generatePool' :: (Source, Integer) -> Pool 
    generatePool' ((Source lang lines), i) = Script {
          scriptBasename = "pool" ++ show i
        , scriptLang = parseLang lang
        , scriptCode = unlines lines
      }

    lower = map DC.toLower

    parseLang :: String -> Lang
    parseLang s
      | lower s == "r"       = LangR
      | lower s == "python"  = LangPython3
      | lower s == "python3" = LangPython3
      | lower s == "py"      = LangPython3
      | lower s == "sh"      = LangBash
      | lower s == "bash"    = LangBash
      | otherwise            = LangOther s
