-- Generator.hs

{-|

Generate target code

The role of the generator is the reverse of the role of the parser. A
parser takes a string and builds a data structure. The generator takes a
data structure and builds a string.

-}

module Morloc.Generator (generate) where

import Morloc.Evaluator (eval)
import Morloc.Type (Lang(..))
import Morloc.NodeAttribute (NodeAttr)
import Morloc.Graph

type Code  = String
type Nexus = Code
type Pool  = (Lang, Code)

generate :: Graph NodeAttr -> (Nexus, [Pool])
generate _ = (
    "this is the nexus code",
    [
      (R,    "this is R code"),
      (Bash, "this is Bash code")
    ]
  )

{-
 - data Arg = Positional String | Keyword String String
 -
 - argstr :: String -> Arg -> String
 - argstr sep (Keyword k  v) = k ++ sep ++ v
 - argstr _   (Positional v) = v
 -
 - generateFunction :: Lang -> String -> [Args] -> Code -> Code
 - generateFunction R name args body = name ++ " <- function(" ++ concatMap (argstr "=") args ++ ") {" ++ body ++ "}"
 -
 -
 - Program :: Program Prologue Source [Function] Epilogue
 - Function :: Function Name [Arg] Body
 - Name :: Name Prefix String Suffix
 - Arg :: Positional String | Keyword String String
 - Body :: Body Input
 -
 -
 - Input :: (Lang, Source, [Function]) -> Code
 -}
