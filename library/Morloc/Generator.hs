-- Generator.hs

{-|

Generate target code

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

module Morloc.Generator (generate) where

import Data.List (intercalate)

import Morloc.Evaluator (eval)
import Morloc.Type (Lang(..))
import qualified Morloc.NodeAttribute as Attr
import Morloc.Graph

-- These types are mostly for readability
type Code = String
type Nexus = Code
type Pool = (Lang, Code)
type UniqName = String

data Arg = Positional String | Keyword String String

generate :: Graph Attr.NodeAttr -> (Nexus, [Pool])
generate g = (generateNexus g, generatePools g)

-- | Transform each node into a funtion in the target language
encodeNodes :: Graph Attr.NodeAttr -> Graph (Lang, UniqName, Code)
encodeNodes = familyMap translate

translate :: Attr.NodeAttr -> [Attr.NodeAttr] -> (Lang, UniqName, Code)
translate p inputs = (lang, name, code) where
  lang = R  -- hard-coded for now
  name = Attr.showNodeValue p
  args = []
  body = generateFunctionCall name (map makeFunctionName inputs)
  code = generateFunction (makeFunctionName p) args body

-- | Make a function name for a node. This name needs to be a valid identifier
-- in the target language. Usually just prefixing the node id with a character
-- works fine. Alternatively I could use a more descriptive name, such as the
-- bound function.
makeFunctionName :: Attr.NodeAttr -> UniqName
makeFunctionName g = "m" ++ Attr.showNodeID g

-- | Create a script that calls the root node
generateNexus :: Graph Attr.NodeAttr -> Nexus
generateNexus _ = unlines [
      "#!/usr/bin/bash"
    , ""
    , "./pool.R"
  ]

-- | Create the code for each function pool
generatePools :: Graph Attr.NodeAttr -> [Pool]
generatePools g = [(R, collapse g)] where

  collapse :: Graph Attr.NodeAttr -> String
  collapse g = unlines [prologue, extractFunctions g, epilogue]

  prologue = "#!/usr/bin/Rscript --vanilla\n"

  epilogue = unlines
    [
        "args <- commandArgs(TRUE)"
      , "m <- args[1]"
      , "if(exists(m)){"
      , "  print(get(m)())"
      , "} else {"
      , "  quit(status=1)"
      , "}"
    ]

  extractFunctions :: Graph Attr.NodeAttr -> String
  extractFunctions = unlines . toList . fmap third . encodeNodes

  third :: (a,b,c) -> c
  third (_,_,x) = x

argstr :: String -> Arg -> String
argstr sep (Keyword    k v) = k ++ sep ++ v
argstr _   (Positional   v) = v

generateFunction :: String -> [Arg] -> Code -> Code
generateFunction name args body = concat [name, " <- function(", arglist, ") {", body, "}"] where
  arglist = intercalate "," . map (argstr "=") $ args

-- | Generate a function call where all arguments are function calls. For
-- example, `foo(bar(),baz())`.
generateFunctionCall :: String -> [String] -> Code
generateFunctionCall s ss = concat [s, "(", funlist ss, ")"] where
  funlist = intercalate "," . map (++ "()")
