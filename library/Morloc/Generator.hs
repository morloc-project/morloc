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

import Morloc.Type (Lang(..))
import qualified Morloc.NodeAttribute as Attr
import Morloc.Graph

-- These types are mostly for readability
type Code = String
type Nexus = Code
type UniqName = String
type Pool = (UniqName, Code)

data Arg = Positional String | Keyword String String

generate :: Graph Attr.NodeAttr -> (Nexus, [Pool])
generate g = (generateNexus g, generatePools g)

-- | Transform each node into a funtion in the target language
encodeNodes :: Graph Attr.NodeAttr -> Graph (Lang, UniqName, Code)
encodeNodes = familyMap translate

translate :: Attr.NodeAttr -> [Attr.NodeAttr] -> (Lang, UniqName, Code)
translate p inputs  = case Attr.primitive p of
  (Just False) -> (lang, name, code) where
    lang = R  -- hard-coded for now
    name = Attr.showNodeValue p
    args = []
    body = generateFunctionCall name (map callNode inputs)
    code = generateFunction (makeName p) args body
  _ -> (R, Attr.showNodeValue p, "")

-- | Make a function name for a node. This name needs to be a valid identifier
-- in the target language. Usually just prefixing the node id with a character
-- works fine. Alternatively I could use a more descriptive name, such as the
-- bound function.
makeName :: Attr.NodeAttr -> UniqName
makeName g = case Attr.primitive g of
  (Just False) -> "m" ++ Attr.showNodeID g
  (Just True) -> generateValue g
  Nothing -> generateValue g -- this shouldn't ever happen
                             -- indeed, why is primitive a Maybe?

generateValue :: Attr.NodeAttr -> String
generateValue a
  | Attr.showNodeType a == "String" = "\"" ++ Attr.showNodeValue a ++ "\""
  | Attr.showNodeType a == "Bool" = case Attr.showNodeValue a of
      "True"  -> "TRUE"
      "False" -> "FALSE"
      _       -> "NA" -- TODO: replace value strings with something sane
  | otherwise = Attr.showNodeValue a
  

callNode :: Attr.NodeAttr -> String
callNode g = case Attr.primitive g of
  (Just False) -> (makeName g) ++ "()"
  (Just True) -> makeName g
  Nothing -> makeName g

-- | Create a script that calls the root node
generateNexus :: Graph Attr.NodeAttr -> Nexus
generateNexus _ = unlines [
      "#!/usr/bin/bash"
    , ""
    , "./pool.R m0"
  ]

-- | Create the code for each function pool
generatePools :: Graph Attr.NodeAttr -> [Pool]
generatePools g = [("pool.R", collapse g)] where

  collapse :: Graph Attr.NodeAttr -> String
  collapse node = unlines [prologue, extractFunctions node, epilogue]

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

-- | Generate a function call. For example, `foo(bar(),1)`.
generateFunctionCall :: String -> [String] -> Code
generateFunctionCall s ss = concat [s, "(", intercalate "," ss, ")"]
