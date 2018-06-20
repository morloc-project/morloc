module Morloc.Language (
    CodeGenerator(..)
  , Arg(..)
  , rCodeGenerator
) where

import Morloc.Syntax (MData, Name)

data Arg = Positional String | Keyword String String

data CodeGenerator = CodeGenerator {
      generatePool
        :: [String] -- any required global declarations
        -> [String] -- any input source code
        -> [String] -- the node function declarations
        -> String   -- entire pool script

    , generateFunction
        :: String  -- function name
        -> String  -- argument string (output of `generateArgs`)
        -> String  -- body
        -> String  -- complete function

      -- | Generate a function call. For example, `foo(bar(),1)`.
    , generateFunctionCall
        :: String -- function name
        -> String -- argument string (output of `generateArgs`)
        -> String -- function call

    , generateArgs
        :: [Arg]  -- a list of arguments
        -> String -- argument string

      -- | Make a function name for a node. This name needs to be a valid identifier
      -- in the target language. Usually just prefixing the node id with a character
      -- works fine. Alternatively I could use a more descriptive name, such as the
      -- wrapped function with a suffix.
    , generateNode
        :: Int    -- the index of a node
        -> String -- the function name of the node

    , generateArray
        :: [MData] -- a heterogenous list
        -> String  -- stringified heterogenous list (homogenized if possible)
                   -- TODO Should het lists even be legal?

    , generateRecord
        :: [(Name, MData)] -- a named, nested list
        -> String          -- whatever structure is appropriate

    , generateBool
        :: Bool   -- boolean primitive
        -> String -- strigified boolean

    , generateString
        :: String -- string primitive
        -> String -- string literal (quoted as needed)

    , generateNum
        :: Double -- double primitive
        -> String -- stringified double

    , generateInt
        :: Integer -- integer primitive
        -> String  -- stringified integer
  }


indent :: Int -> String -> String
indent i s
  | i <= 0    = s
  | otherwise = unlines . map ((++) (take i (repeat ' '))) . lines $ ss 

rCodeGenerator = CodeGenerator {
    -- generatePool
    --   :: [String] -- any required global declarations
    --   -> [String] -- any input source code
    --   -> [String] -- the node function declarations
    --   -> String   -- entire pool script
    generatePool gs is fs = (concat . map unlines) [gs, is, fs]

    -- generateFunction
    --   :: String  -- function name
    --   -> String  -- argument string (output of `generateArgs`)
    --   -> String  -- body
    --   -> String  -- complete function
  , generateFunction f a b = f ++ " function(" ++ a ++ "){" ++ (indent 2 b) ++ "}"

    -- -- | Generate a function call. For example, `foo(bar(),1)`.
    -- generateFunctionCall
    --   :: String -- function name
    --   -> String -- argument string (output of `generateArgs`)
    --   -> String -- function call
    --
    -- generateArgs
    --   :: [Arg]  -- a list of arguments
    --   -> String -- argument string
    --
    -- -- | Make a function name for a node. This name needs to be a valid identifier
    -- -- in the target language. Usually just prefixing the node id with a character
    -- -- works fine. Alternatively I could use a more descriptive name, such as the
    -- -- wrapped function with a suffix.
    -- generateNode
    --   :: Int    -- the index of a node
    --   -> String -- the function name of the node
    --
    -- generateArray
    --   :: [MData] -- a heterogenous list
    --   -> String  -- stringified heterogenous list (homogenized if possible)
    --              -- TODO Should het lists even be legal?
    --
    -- generateRecord
    --   :: [(Name, MData)] -- a named, nested list
    --   -> String          -- whatever structure is appropriate
    --
    -- generateBool
    --   :: Bool   -- boolean primitive
    --   -> String -- strigified boolean
    --
    -- generateString
    --   :: String -- string primitive
    --   -> String -- string literal (quoted as needed)
    --
    -- generateNum
    --   :: Double -- double primitive
    --   -> String -- stringified double
    --
    -- generateInt
    --   :: Integer -- integer primitive
    --   -> String  -- stringified integer
  }
