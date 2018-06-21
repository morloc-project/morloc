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

rCodeGenerator = CodeGenerator {
    makePool         = \gs is fs -> unlines . concat $ [begin', gs, is, fs, end']
  , makeSource       = rSource
  , makeFunction     = \f a b -> f ++ " function(" ++ a ++ "){" ++ (indent 2 b) ++ "}"
  , makeFunctionCall = \f args -> f ++ "(" ++ args ++ ")"
  , makeArgs         = intercalate ", " . map showArg
  , makeNode         = makeNode'
  , makeAssignment   = \l r -> l ++ " <- " ++ r
  , makeMData        = showRData
  }
  where

    makeNode' (WNode (Just i) _ _) = "m" ++ show i
    makeNode' (WLeaf (Just i)   _) = "m" ++ show i
    makeNode' _                    = "FUUUUUUUCK!"

    rSource :: Source -> String
    rSource (SourceFile _ path _) = "source(" ++ (intercalate "/" path) ++ ")"
    rSource (SourceLang _      _) = ""

    begin' = []
    end'   = []

    -- data Arg = Positional String | Keyword String String

    showArg (Positional s) = s
    showArg (Keyword n s)  = n ++ "=" ++ s
    showArg NoArgument     = ""

    showRData :: MData -> String
    showRData (MInt x)     = show x
    showRData (MNum x)     = show x
    showRData (MLog True)  = "TRUE"
    showRData (MLog False) = "FALSE"
    showRData (MLst xs)    = "c(" ++ (intercalate ", " . map showRData) xs ++ ")"
    showRData (MRec rs)    = "list(" ++ (intercalate ", " . map (genEq showRData) $ rs) ++ ")"
    showRData (MStr s)     = "\"" ++ s ++ "\""

    genEq :: (b -> String) -> (String, b) -> String
    genEq f (n, b) = n ++ " = " ++ f b
