module Morloc.Generator (
      generate
    , Nexus
    , Pool
  ) where

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
generatePools (Program ws _ ss) = join . fmap sequence $ makePooler <*> pure ss
  where

    makePooler :: ThrowsError ([Source] -> [ThrowsError Pool])
    makePooler
      = fmap map                          -- ThrowsError ([Source] -> [ThrowsError Pool])
      . fmap generatePool                 -- ThrowsError (Source -> ThrowsError Pool)
      . (fmap . fmap) replaceTree         -- ThrowsError [Function SNode]
      . fmap (zip ws)                     -- ThrowsError [(Function WNode, Tree SNode)]
      . join                              -- ThrowsError [Tree SNode]
      . fmap sequence                     -- ThrowsError ThrowsError [Tree SNode]
      . (fmap . fmap) sequence            -- ThrowsError [ThrowsError (Tree SNode)]
      . (fmap . fmap) (familyMap toSNode) -- ThrowsError [Tree (ThrowsError SNode)]
      $ zipWith zipTree
        <$> pure wTrees
        <*> sTrees                        -- ThrowsError [Tree (WNode, Source)]

    sTrees :: ThrowsError [Tree Source]
    sTrees = sequence $ map (toSource ss) wTrees

    idTrees :: [Tree Int]
    idTrees = numberTrees wTrees

    wTrees :: [Tree WNode]
    wTrees
      = zipWith                      -- (a -> b -> c) -> [a] -> [b] -> [c]
        (zipWithTree setID)          -- Tree Wnode -> Tree Int -> Tree Wnode
        idTrees                      -- [Tree Int]
        [t | (Function _ _ t) <- ws] -- [Tree WNode]

    setID :: Int -> WNode -> WNode
    setID i (WNode _ x y) = WNode (Just i) x y 
    setID i (WLeaf _ x  ) = WLeaf (Just i) x  

    foo :: (Function WNode, Tree Source) -> Tree (WNode, Source)
    foo ((Function _ _ gnode), gsrc) = zipTree gnode gsrc

    toSNode :: (WNode, Source) -> [(WNode, Source)] -> ThrowsError SNode
    toSNode (WLeaf i x, SourceLocal) [] = Right (SLeaf i x)
    toSNode (WLeaf _ _, SourceLocal) _  = Left (BadApplication "Data cannot be given arguments")
    toSNode (WLeaf _ _, _      ) _      = Left (VeryBadBug "Data was associated with a source")
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
      . concat        -- [SNode]
      . map toList    -- [[Snode]]
      . map getTree   -- [Tree SNode]
      $ fs

    getTree :: Function SNode -> Tree SNode
    getTree (Function _ _ g) = g

    function' :: SNode -> String
    function' (SNode (w, SourceLocal) ss)
      = (makeFunction g) (makeNode g $ w) "" (cisBody' w ss)
    function' (SNode (w, s) ss)
      | s == src  = (makeFunction g) (makeNode g $ w) "" (cisBody' w ss)
      | otherwise = (makeFunction g) (makeNode g $ w) "" (transBody' w ss)
    function' (SLeaf i d) = (makeAssignment g) (makeNode g $ WLeaf i d) (makeMData g $ d)
    function' x = show x

    cisBody' (WNode _ n _) ss = (makeFunctionCall g) n "..."

    transBody' _ _ = "TRANS_STUB"

    -- arguments' :: [(WNode, Source)] -> String
    -- arguments' ss = (makeArgs g) $ map argument' (map fst ss)
    --
    -- -- TODO need error handling
    -- argument' :: WNode -> Arg
    -- argument' (WNode (Just i) n a)
    --   = Positional ((makeFunctionCall g) (makeNode g $ WNode (Just i) n a) "")
    -- argument' (WLeaf (Just i) d)
    --   = Positional (makeNode g $ WLeaf (Just i) d)
    -- argument' _ = Positional "ERROR"

toSource :: [Source] -> Tree WNode -> ThrowsError (Tree Source)
toSource srcs =
      sequence              -- ThrowsError (Tree Source) 
    . fmap (toSource' srcs) -- Tree (ThrowsError Source)
  where

    toSource' :: [Source] -> WNode -> ThrowsError Source
    toSource' ss (WNode i n a) = case map (mSource (WNode i n a)) ss of
      []  -> Right SourceLocal  -- the contents of WNode where not imported
      [s] -> Right s
      xs -> Left (NameConflict n (map sourceNames xs))
    toSource' _ (WLeaf _ _) = Right SourceLocal

    sourceNames :: Source -> String 
    sourceNames (SourceLang n _)   = n
    sourceNames (SourceFile n _ _) = n
    sourceNames SourceLocal        = "<local>"

    mSource :: WNode -> Source -> Source
    mSource node src 
      | elem' node src = src
      | otherwise = SourceLocal

    elem' :: WNode -> Source -> Bool
    elem' (WNode _ name _) (SourceLang _   ns) = elem name (functionNames ns)
    elem' (WNode _ name _) (SourceFile _ _ ns) = elem name (functionNames ns)
    elem' _              _                   = False

functionNames :: Functor f => f (String, Maybe String) -> f String
functionNames = fmap f
  where
    f :: (String, Maybe String) -> String
    f (_, Just s) = s -- if there is an alias, use it
    f (s, _     ) = s -- otherwise use the original name

numberTrees :: [Tree a] -> [Tree Int]
numberTrees gs = numberTrees' 1 gs
  where
    numberTrees' :: Int -> [Tree a] -> [Tree Int]
    numberTrees' _ [] = []
    numberTrees' i [g] = [indexTree i g]
    numberTrees' i (g:gs') = case (indexTree i g) of
      g' -> g' : (numberTrees' (i + length g' + 1) gs')
