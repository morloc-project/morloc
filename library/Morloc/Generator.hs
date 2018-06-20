module Morloc.Generator (
      generate
    , Nexus
    , Pool
  ) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Char as DC 
import Safe (atMay)
import Control.Monad (join)

import Morloc.Graph
import Morloc.Data
import Morloc.Syntax
import Morloc.Error
import Morloc.Util (which)
import Morloc.Language

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

generatePools :: Program -> ThrowsError [Pool]
generatePools (Program ws _ ss) = join . fmap sequence $ makePooler ws ss <*> pure ss
  where

    makePooler :: [Function WNode] -> [Source] -> ThrowsError ([Source] -> [ThrowsError Pool])
    makePooler ws ss =
        fmap map                          -- ThrowsError ([Source] -> [ThrowsError Pool])
      . fmap generatePool                 -- ThrowsError (Source -> ThrowsError Pool)
      . (fmap . fmap) replaceGraph        -- ThrowsError [Function SNode]
      . fmap (zip ws)                     -- ThrowsError [(Function WNode, Graph SNode)]
      . join                              -- ThrowsError [Graph SNode]
      . fmap sequence                     -- ThrowsError ThrowsError [Graph SNode]
      . (fmap . fmap) sequence            -- ThrowsError [ThrowsError (Graph SNode)]
      . (fmap . fmap) (familyMap toSNode) -- ThrowsError [Graph (ThrowsError SNode)]
      . (fmap . fmap) foo                 -- ThrowsError [Graph (WNode, Maybe Source)]
      . fmap (zip ws)                     -- ThrowsError [(Function WNode, Graph (Maybe Source))]
      . sequence                          -- ThrowsError [Graph (Maybe Source)]
      . map (toSource ss)                 -- [ThrowsError (Graph (Maybe Source))]
      $ [g | (Function _ _ g) <- ws]

    foo :: (Function WNode, Graph (Maybe Source)) -> Graph (WNode, Maybe Source)
    foo ((Function _ _ gnode), gsrc) = zipG gnode gsrc

    toSNode :: (WNode, Maybe Source) -> [(WNode, Maybe Source)] -> ThrowsError SNode
    toSNode (WLeaf x, Nothing) [] = Right (SLeaf x)
    toSNode (WLeaf _, Nothing) _  = Left (BadApplication "Data cannot be given arguments")
    toSNode (WLeaf _, _      ) _  = Left (VeryBadBug "Data was associated with a source")
    toSNode x kids = Right (SNode x kids)

    replaceGraph :: (Function a, Graph b) -> Function b
    replaceGraph (Function s ss _, x) = Function s ss x

generatePool :: [Function SNode] -> Source -> ThrowsError Pool
generatePool fs src
  =   Script
  <$> pure (poolName'    src)
  <*> pure (poolLang'    src)
  <*> poolCode' fs src
  where
    poolName' :: Source -> String
    poolName' _ = "pool" -- TODO append a number to make unique

    poolLang' :: Source -> String
    poolLang' (Source lang _ _) = lang


    -- generate the code required for a specific `source` statement
    poolCode'
      :: [Function SNode]         -- list of functions
      -> Source
      -> ThrowsError String       -- complete code for the pool

    poolCode' fs (Source _ (Just _) _ )
      = Left $ NotImplemented "cannot yet read source"
    poolCode' fs (Source "R" Nothing i)
      = generatePoolCode (Source "R" Nothing i) fs rCodeGenerator 
    poolCode' fs (Source lang   _ _)
      = Left $ NotSupported ("ERROR: the language '" ++ lang ++ "' is not yet supported")

-- The top level
generatePoolCode :: Source -> [Function SNode] -> CodeGenerator -> ThrowsError String
generatePoolCode src fs g = Right $ (makePool g) global' source' (functions' fs)
  where
    global' :: [String]
    global' = []

    source' :: [String]
    source' = [(makeSource g) src]

    functions' :: [Function SNode] -> [String]
    functions' fs
      = map function' -- [String]
      . concat        -- [(Int, SNode)]
      . map toList    -- [[(Int, Snode)]] 
      . numberTrees   -- [Graph (Int, SNode)]
      . map getGraph  -- [Graph (SNode)]
      $ fs


    getGraph :: Function SNode -> Graph SNode
    getGraph (Function _ _ g) = g

    function' :: (Int, SNode) -> String
    function' (i, (SNode (w, Just s) ss))
      | s == src  = (makeFunction g) (makeNode g $ i) (arguments' ss) "internal call"
      | otherwise = (makeFunction g) (makeNode g $ i) (arguments' ss) "foreign call"
    function' (i, (SNode (w, Nothing) ss))
      = (makeFunction g) (makeNode g $ i) (arguments' ss) "internal call"
    function' (i, SLeaf d) = (makeAssignment g) (makeNode g $ i) (makeMData g $ d)
    function' x = show x

    -- TODO I need to keep track of the node indices for all the data,
    -- including the children. Maybe I should just ditch the indexing and use
    -- mangled names? Why do I use them anyway? Just legacy, really.
    
    arguments' _ = "args"

    -- arguments' :: [(Int, WNode)] -> String
    -- arguments' ss = (makeArgs g) $ map argument' ss
    --
    -- argument' :: (Int, WNode) -> Arg
    -- argument' (i, SNode _ _) = Positional "function_call"
    -- argument' (i, SLeaf _)   = Positional "data_variable"

toSource :: [Source] -> Graph WNode -> ThrowsError (Graph (Maybe Source))
toSource srcs =
      sequence              -- ThrowsError (Graph (Maybe Source)) 
    . fmap (toSource' srcs) -- Graph (ThrowsError (Maybe Source))
  where

    toSource' :: [Source] -> WNode -> ThrowsError (Maybe Source)
    toSource' ss (WNode n a) = case catMaybes (map (mSource (WNode n a)) ss) of
      []  -> Right Nothing
      [s] -> Right (Just s)
      ss  -> Left (NameConflict n [n' | (Source n' _ _) <- ss])
    toSource' _ (WLeaf _) = Right Nothing
      
    mSource :: WNode -> Source -> Maybe Source
    mSource node src 
      | elem' node src = Just src
      | otherwise = Nothing
    mSource _ _ = Nothing

    elem' :: WNode -> Source -> Bool
    elem' (WNode name _) (Source _ _ ns) = elem name (functionNames ns)

isVar :: Graph WNode -> Graph Bool
isVar = fmap isVar' where
  isVar' (WNode _ _) = True
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
    numberTrees' i [g] = [numberG i g]
    numberTrees' i (g:gs) = case (numberG i g) of
      g' -> g' : (numberTrees' (i + length g' + 1) gs)
