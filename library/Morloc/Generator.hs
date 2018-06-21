module Morloc.Generator (
      generate
    , Nexus
    , Pool
  ) where

import Data.List (intercalate)
import qualified Data.Char as DC 
import Safe (atMay)
import Control.Monad (join)

import Morloc.Tree
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
      . (fmap . fmap) replaceTree         -- ThrowsError [Function SNode]
      . fmap (zip ws)                     -- ThrowsError [(Function WNode, Tree SNode)]
      . join                              -- ThrowsError [Tree SNode]
      . fmap sequence                     -- ThrowsError ThrowsError [Tree SNode]
      . (fmap . fmap) sequence            -- ThrowsError [ThrowsError (Tree SNode)]
      . (fmap . fmap) (familyMap toSNode) -- ThrowsError [Tree (ThrowsError SNode)]
      . (fmap . fmap) foo                 -- ThrowsError [Tree (WNode, Source)]
      . fmap (zip ws)                     -- ThrowsError [(Function WNode, Tree (Source))]
      . sequence                          -- ThrowsError [Tree (Source)]
      . map (toSource ss)                 -- [ThrowsError (Tree (Source))]
      $ [g | (Function _ _ g) <- ws]

    -- tWNode' :: [Tree WNode]
    -- tWNode' = [t | (Function _ _ t) <- ws]
    --
    -- tSource' :: ThrowsError [Tree (Maybe Source)]
    -- tSource' = map (toSource ss) ws
    --
    -- tNumber' ::

    foo :: (Function WNode, Tree Source) -> Tree (WNode, Source)
    foo ((Function _ _ gnode), gsrc) = zipT gnode gsrc

    toSNode :: (WNode, Source) -> [(WNode, Source)] -> ThrowsError SNode
    toSNode (WLeaf x, SourceLocal) [] = Right (SLeaf x)
    toSNode (WLeaf _, SourceLocal) _  = Left (BadApplication "Data cannot be given arguments")
    toSNode (WLeaf _, _      ) _      = Left (VeryBadBug "Data was associated with a source")
    toSNode x kids = Right (SNode x kids)

    replaceTree :: (Function a, Tree b) -> Function b
    replaceTree (Function s ss _, x) = Function s ss x

generatePool :: [Function SNode] -> Source -> ThrowsError Pool
generatePool fs src
  =   Script
  <$> pure (poolName'    src)
  <*> poolLang'    src
  <*> poolCode' fs src
  where
    poolName' :: Source -> String
    poolName' _ = "pool" -- TODO append a number to make unique

    poolLang' :: Source -> ThrowsError String
    poolLang' (SourceLang lang   _) = Right lang
    poolLang' (SourceFile lang _ _) = Right lang
    poolLang' _                     = Left $ VeryBadBug "Cannot build script from local source"

    -- generate the code required for a specific `source` statement
    poolCode'
      :: [Function SNode]         -- list of functions
      -> Source
      -> ThrowsError String       -- complete code for the pool
    poolCode' fs (SourceLang "R" i)
      = generatePoolCode (SourceLang "R" i) fs rCodeGenerator 
    poolCode' _  (SourceFile _ _ _ )
      = Left $ NotImplemented "cannot yet read source"
    poolCode' _  (SourceLang lang   _)
      = Left $ NotSupported ("ERROR: the language '" ++ lang ++ "' is not yet supported")
    poolCode' _ SourceLocal
      = Left $ VeryBadBug "Cannot build script from local source"

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
      . numberTrees   -- [Tree (Int, SNode)]
      . map getTree  -- [Tree (SNode)]
      $ fs


    getTree :: Function SNode -> Tree SNode
    getTree (Function _ _ g) = g

    function' :: (Int, SNode) -> String
    function' (i, (SNode (w, SourceLocal) ss))
      = (makeFunction g) (makeNode g $ i) (arguments' ss) "internal call"
    function' (i, (SNode (w, s) ss))
      | s == src  = (makeFunction g) (makeNode g $ i) (arguments' ss) "internal call"
      | otherwise = (makeFunction g) (makeNode g $ i) (arguments' ss) "foreign call"
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

toSource :: [Source] -> Tree WNode -> ThrowsError (Tree Source)
toSource srcs =
      sequence              -- ThrowsError (Tree Source) 
    . fmap (toSource' srcs) -- Tree (ThrowsError Source)
  where

    toSource' :: [Source] -> WNode -> ThrowsError Source
    toSource' ss (WNode n a) = case map (mSource (WNode n a)) ss of
      []  -> Right SourceLocal  -- the contents of WNode where not imported
      [s] -> Right s
      ss  -> Left (NameConflict n (map sourceNames ss))
    toSource' _ (WLeaf _) = Right SourceLocal

    sourceNames :: Source -> String 
    sourceNames (SourceLang n _)   = n
    sourceNames (SourceFile n _ _) = n
    sourceNames SourceLocal        = "<local>"

    mSource :: WNode -> Source -> Source
    mSource node src 
      | elem' node src = src
      | otherwise = SourceLocal
    mSource _ _ = SourceLocal

    elem' :: WNode -> Source -> Bool
    elem' (WNode name _) (SourceLang _   ns) = elem name (functionNames ns)
    elem' (WNode name _) (SourceFile _ _ ns) = elem name (functionNames ns)
    elem' _              _                   = False

isVar :: Tree WNode -> Tree Bool
isVar = fmap isVar' where
  isVar' (WNode _ _) = True
  isVar' _ = False

functionNames :: Functor f => f (String, Maybe String) -> f String
functionNames = fmap f
  where
    f :: (String, Maybe String) -> String
    f (_, Just s) = s -- if there is an alias, use it
    f (s, _     ) = s -- otherwise use the original name

numberTrees :: [Tree a] -> [Tree (Int, a)]
numberTrees gs = numberTrees' 1 gs
  where
    numberTrees' :: Int -> [Tree a] -> [Tree (Int, a)]
    numberTrees' _ [] = []
    numberTrees' i [g] = [numberT i g]
    numberTrees' i (g:gs) = case (numberT i g) of
      g' -> g' : (numberTrees' (i + length g' + 1) gs)
