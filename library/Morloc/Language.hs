module Morloc.Language (
    CodeGenerator(..)
  , Arg(..)
  , rCodeGenerator
) where

import Morloc.Syntax
import Morloc.Data
import Data.List (intercalate)

indent :: Int -> String -> String
indent i s
  | i <= 0    = s
  | otherwise = unlines . map ((++) (take i (repeat ' '))) . lines $ s

data Arg
  = Positional String
  | Keyword String String
  | NoArgument

data CodeGenerator = CodeGenerator {
      makePool
        :: [String] -- any required global declarations
        -> [String] -- any input source code
        -> [String] -- the node function declarations
        -> String   -- entire pool script

    , makeSource
        :: Source
        -> String

    , makeFunction
        :: String  -- function name
        -> String  -- argument string (output of `makeArgs`)
        -> String  -- body
        -> String  -- complete function

      -- | make a function call. For example, `foo(bar(),1)`.
    , makeFunctionCall
        :: String -- function name
        -> String -- argument string (output of `makeArgs`)
        -> String -- function call

    , makeArgs
        :: [Arg]  -- a list of arguments
        -> String -- argument string

      -- | Make a function name for a node. This name needs to be a valid identifier
      -- in the target language. Usually just prefixing the node id with a character
      -- works fine. Alternatively I could use a more descriptive name, such as the
      -- wrapped function with a suffix.
    , makeNode
        :: WNode
        -> String -- the function name of the node

    , makeAssignment
        :: String -- lhs
        -> String -- rhs
        -> String -- assignment expression

    , makeMData :: MData -> String
  }

rCodeGenerator :: CodeGenerator
rCodeGenerator = CodeGenerator {
    makePool         = makePool'
  , makeSource       = makeSource'
  , makeFunction     = makeFunction'
  , makeFunctionCall = makeFunctionCall'
  , makeArgs         = makeArgs'
  , makeNode         = makeNode'
  , makeAssignment   = makeAssignment'
  , makeMData        = makeMData'
  }
  where

    makePool' :: [String] -> [String] -> [String] -> String
    makePool' gs is fs = unlines . concat $ [begin', gs, is, fs, end']

    makeSource' :: Source -> String
    makeSource' (SourceFile _ path _) = "source(" ++ (intercalate "/" path) ++ ")"
    makeSource' _ = ""

    makeFunction' :: String -> String -> String -> String
    makeFunction' f a b = f ++ " <- function(" ++ a ++ "){\n" ++ (indent 2 b) ++ "}\n"

    makeFunctionCall' :: String -> String -> String
    makeFunctionCall' f args = f ++ "(" ++ args ++ ")"

    makeArgs' :: [Arg] -> String
    makeArgs' = intercalate ", " . map showArg

    showArg (Positional s) = s
    showArg (Keyword n s)  = n ++ "=" ++ s
    showArg NoArgument     = ""

    makeNode' (WNode (Just i) _ _) = "m" ++ show i
    makeNode' (WLeaf (Just i)   _) = "m" ++ show i
    makeNode' _                    = "FUUUUUUUCK!"

    makeAssignment' :: String -> String -> String
    makeAssignment' l r = l ++ " <- " ++ r

    makeMData' :: MData -> String
    makeMData' (MInt x)     = show x ++ "L" -- longs in R are formatted as: 42L
    makeMData' (MNum x)     = show x
    makeMData' (MLog True)  = "TRUE"
    makeMData' (MLog False) = "FALSE"
    makeMData' (MLst xs)    = "c(" ++ (intercalate ", " . map makeMData') xs ++ ")"
    makeMData' (MTup xs)    = "list(" ++ (intercalate ", " . map makeMData') xs ++ ")"
    makeMData' (MRec rs)    = "list(" ++ (intercalate ", " . map (genEq makeMData') $ rs) ++ ")"
    makeMData' (MStr s)     = "\"" ++ s ++ "\""

    genEq :: (b -> String) -> (String, b) -> String
    genEq f (n, b) = n ++ " = " ++ f b

    begin' = ["#!/usr/bin/env Rscript"]
    end'   =
      [ "args <- commandArgs(trailingOnly=TRUE)"
      , "if(length(args) == 0){"
      , "  stop(\"Expected 1 or more arguments\")"
      , "} else if(exists(args[[1]])){"
      , "  x <- get(args[[1]])"
      , "  result <- if(class(x) == \"function\"){"
      , "    par <- lapply(args[-1], function(s) eval(parse(text=s)))"
      , "    do.call(get(args[[1]]), par)"
      , "  } else {"
      , "    x"
      , "  }"
      , "  cat(result, \"\\n\")"
      , "} else {"
      , "  stop(\"Could not find function '\", f, \"'\")"
      , "}"
      ]
