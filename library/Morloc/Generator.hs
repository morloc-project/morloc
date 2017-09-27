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

module Morloc.Generator (generate) where

import Data.List (intercalate)

import Morloc.Graph
import Morloc.Data

-- These types are mostly for readability
type Code = String
type Nexus = Code
type UniqName = String
type Pool = (UniqName, Code)

data Arg = Positional String | Keyword String String

generate :: Graph MData -> (Nexus, [Pool])
generate g = (generateNexus g, generatePools g)

-- | Create a script that calls the root node
generateNexus :: Graph MData -> Nexus
generateNexus _ = unlines [
      "#!/usr/bin/env bash"
    , ""
    , "./pool.R m1"
  ]

-- | Create the code for each function pool
generatePools :: Graph MData -> [Pool]
generatePools g = [("pool.R", collapse g)] where

  collapse :: Graph MData -> String
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

  extractFunctions :: Graph MData -> String
  extractFunctions = unlines . toList . encodeNodes

  -- | Transform each node into a funtion in the target language
  encodeNodes :: Graph MData -> Graph Code
  encodeNodes = familyMap translate . suczip (+ 1) 1 where
    translate :: (Int, MData) -> [(Int, MData)] -> Code
    translate (i, MFunc name) inputs = code where
        code = generateFunction (generateNode i) [] body
        body = generateFunctionCall name (map callNode inputs)
    translate _ _ = []

generateFunction :: String -> [Arg] -> Code -> Code
generateFunction name args body = concat [name, " <- function(", generateArgs args, ") {", body, "}"]

-- | Generate a function call. For example, `foo(bar(),1)`.
generateFunctionCall :: String -> [String] -> Code
generateFunctionCall s ss = concat [s, "(", intercalate "," ss, ")"]

generateArgs :: [Arg] -> String
generateArgs = intercalate "," . map (argstr "=") where
  argstr :: String -> Arg -> String
  argstr sep (Keyword    k v) = k ++ sep ++ v
  argstr _   (Positional   v) = v

-- | This is a wrapper for generateValue, only MFunc needs the node id
callNode :: (Int, MData) -> String
callNode (i, MFunc _) = generateNode i ++ "()"
callNode (_, x      ) = generateValue x

-- | Make a function name for a node. This name needs to be a valid identifier
-- in the target language. Usually just prefixing the node id with a character
-- works fine. Alternatively I could use a more descriptive name, such as the
-- wrapped function with a suffix.
generateNode :: Int -> String
generateNode i = "m" ++ show i

generateValue :: MData -> String
generateValue (MInt     x) = generateInt    x
generateValue (MNum     x) = generateNum    x
generateValue (MString  x) = generateString x
generateValue (MBool    x) = generateBool   x
generateValue (MInts    x) = generateArray $ map MInt    x
generateValue (MNums    x) = generateArray $ map MNum    x
generateValue (MStrings x) = generateArray $ map MString x
generateValue (MBools   x) = generateArray $ map MBool   x
generateValue _            = "WTF???" -- I'll fix this ... 

generateArray :: [MData] -> String
generateArray xs = "c(" ++ (intercalate ", " . map generateValue) xs ++ ")"

generateBool :: Bool -> String
generateBool b = if b then "TRUE" else "FALSE"

generateString :: String -> String
generateString s = "\"" ++ s ++ "\""

generateNum :: Double -> String
generateNum = show

generateInt :: Integer -> String
generateInt = show


{- -- see Note 1                                                       -}
{- setid :: Graph NodeAttr -> Graph NodeAttr                           -}
{- setid g = fst <$> propagate base (zipG zeroed gcount) where         -}
{-                                                                     -}
{-   zeroed = fmap (\attr -> attr { nodeID = Just 0 }) g               -}
{-                                                                     -}
{-   -- base :: a -> [a] -> [a]                                        -}
{-   base (_,i) gs' = zipWith set_child_id gs' child_ids where         -}
{-     set_child_id (attr,_) j = (attr { nodeID = Just j }, j)         -}
{-     child_ids = map (+ i) $ scanl1 (+) (map snd gs')                -}
{-                                                                     -}
{-   -- gcount :: Graph Int -- graph with descendent counts            -}
{-   gcount = pullG (+) $ ifelseG (isTerminal g) (const 1) (const 0) g -}

------- NOTE 1 ------------------------------------------------------
--   s0 := setid :: Graph NodeAttr -> Graph NodeAttr
--   ===============================================
--   s1 := zipG   :: Graph a -> Graph b -> Graph (a,b)
-- > s2 := g      :: Graph NodeAttr
--   s3 := gcount :: Graph Int
--   -----------------------------------------------
--   a :: NodeAttr  ;  b :: Int
--   -----------------------------------------------
--   s4 := propagate :: (a -> [a] -> [a]) -> Graph a -> Graph a
--   s5 := base      :: a -> [a] -> [a]
--   s6 := s1 s2 s3  :: Graph (NodeAttr, Int)
--   -----------------------------------------------
--   s7 := s4 s5 s6 :: Graph (NodeAttr, Int)
-- < s8 := fmap fst :: Graph (NodeAttr, Int) -> Graph NodeAttr
--   -----------------------------------------------
--   s8 s6 :: Graph NodeAttr  
--   ===============================================
--
--
--   gcount :: Graph Int
--   ==========================================================
--   s9  := pullG      :: Monoid a => (a -> a -> a) -> Graph a -> Graph a
--   s10 := ifelseG    :: Graph Bool -> (a -> b) -> (a -> b) -> Graph a -> Graph b
--   s11 := isTerminal :: Graph a -> Graph Bool
--   s12 := const      :: a -> b -> a
--   ----------------------------------------------------------
--   s13 := pullG (+)    :: Num a => Graph a -> Graph a
--   s14 := isTerminal g :: Bool
--   s15 := const 1      :: b -> Int
--   ----------------------------------------------------------
--                       b :: Int
--   ----------------------------------------------------------
--   s16 := ifelseG (isTermiminal g) (const 1) (const 0) ::
--            Graph NodeAttr -> Graph Int
--   ----------------------------------------------------------
-- < s16 g := Graph Int
---------------------------------------------------------------------
