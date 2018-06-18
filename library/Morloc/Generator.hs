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
import Morloc.Error

data Script = Script {
      scriptBase :: String -- script basename (no extension)
    , scriptLang :: String -- script language
    , scriptCode :: String -- full script source code
  }
  deriving(Show, Ord, Eq)

type Nexus = Script
type Pool  = Script

generate :: Program -> ThrowsError (Nexus, [Pool])
generate p = (,) <$> generateNexus p <*> generatePools p

-- | Create a script that calls the root node
generateNexus :: Program -> ThrowsError Nexus
generateNexus p = pure $ Script {
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
generatePools :: Program -> ThrowsError [Pool]
generatePools (Program w _ ps)
  = sequence $ map
      (generatePool w)
      (zip ps [1..]) -- associate each source with a number for uniquely
                     -- identifying the pool that will be created around it

generatePool :: [FunctionTree] -> (Source, Integer) -> ThrowsError Pool
generatePool fs ((Source lang path imports), i)
  = Script
  <$> pure ("pool" ++ show i)        -- scriptBase
  <*> pure lang                      -- scriptLang
  <*> poolCode lang fs path imports  -- scriptCode

-- generate the code required for a specific `source` statement
poolCode
  :: String                   -- language
  -> [FunctionTree]           -- list of functions
  -> Maybe [String]           -- path to source code (if needed)
  -> [(String, Maybe String)] -- list of imported functions
  -> ThrowsError String       -- complete code for the pool
poolCode "R" ftree Nothing flist
  = undefined
  -- TODO
  -- Map each node in each workflow to a pool
  -- Map parents and children to build code for each function call
  -- Write the function calls into the pool templates

poolCode "R" _ (Just _) _
  = Left $ NotImplemented "cannot yet read source"
poolCode lang _ Nothing  _
  = Left $ NotSupported ("the language '" ++ lang ++ "' is not yet supported")

isFromSources :: [Source] -> Graph WNode -> Graph [Bool]
isFromSources srcs g = combineG $ map (isFromSource g) srcs
  where
    isFromSource :: Graph WNode -> Source -> Graph Bool 
    isFromSource g (Source _ _ names) = fmap (elem' $ functionNames names) g

    elem' :: [String] -> WNode -> Bool
    elem' names (WNodeVar name _) = elem name names

isVar :: Graph WNode -> Graph Bool
isVar = fmap isVar' where
  isVar' (WNodeVar _ _) = True
  isVar' _ = False

functionNames :: Functor f => f (String, Maybe String) -> f String
functionNames = fmap f
  where
    f :: (String, Maybe String) -> String
    f (_, Just s) = s -- if there is an alias, use it
    f (s, _     ) = s -- otherwise use the original name

numberTrees :: [Graph a] -> [Graph (Int, a)]
numberTrees gs = numberTrees' 1 gs
  where
    numberTrees' :: Int -> [Graph a] -> [Graph (Int, a)]
    numberTrees' _ [] = []
    numberTrees' i [g] = [suczip (+ 1) i g]
    numberTrees' i (g:gs) = case (suczip (+ 1) i g) of
      g' -> g' : (numberTrees' (i + length g' + 1) gs)
