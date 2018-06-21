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
        , "Rscript pool.R $@"
      ]


generatePools :: Program -> ThrowsError [Pool]
generatePools (Program ws _ ss) = join . fmap sequence $ makePooler <*> pure ss
  where

    makePooler :: ThrowsError ([Source] -> [ThrowsError Pool])
    makePooler
      = fmap map                          -- ThrowsError ([Source] -> [ThrowsError Pool])
      . fmap generatePool                 -- ThrowsError (Source -> ThrowsError Pool)
      . (fmap . map) setBoundVar          -- ThrowsError [Function SNode]
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
    idTrees = numberTrees [t | (Function _ _ t) <- ws]

    wTrees :: [Tree WNode]
    wTrees
      = zipWith                      -- (a -> b -> c) -> [a] -> [b] -> [c]
        (zipWithTree setID)          -- Tree Wnode -> Tree Int -> Tree Wnode
        idTrees                      -- [Tree Int]
        [t | (Function _ _ t) <- ws] -- [Tree WNode]

    setID :: Int -> WNode -> WNode
    setID i (WNode _ x y) = WNode (Just i) x y
    setID i (WLeaf _ x  ) = WLeaf (Just i) x

    -- TODO fix this ugliness ...
    setBoundVar :: Function SNode -> Function SNode
    setBoundVar (Function n vars tree) = Function n vars (fmap (setBoundVar' vars) tree)

    setBoundVar' :: [String] -> SNode -> SNode
    setBoundVar' vars (SNode x _ xs) = SNode x vars xs 
    setBoundVar' _ n = n

    toSNode :: (WNode, Source) -> [(WNode, Source)] -> ThrowsError SNode
    toSNode (WLeaf i x, SourceLocal) [] = Right (SLeaf i x)
    toSNode (WLeaf _ _, SourceLocal) _  = Left (BadApplication "Data cannot be given arguments")
    toSNode (WLeaf _ _, _      ) _      = Left (VeryBadBug "Data was associated with a source")
    toSNode x kids = Right (SNode x [] kids)

    replaceTree :: (Function a, Tree b) -> Function b
    replaceTree (Function s ss' _, x) = Function s ss' x


generatePool :: [Function SNode] -> Source -> ThrowsError Pool
generatePool fs src
  =   Script
  <$> pure (poolName'    src)
  <*> poolLang'    src
  <*> generatePoolCode fs src
  where
    poolName' :: Source -> String
    poolName' _ = "pool" -- TODO append a number to make unique

    poolLang' :: Source -> ThrowsError String
    poolLang' (SourceLang lang   _) = Right lang
    poolLang' (SourceFile lang _ _) = Right lang
    poolLang' _                     = Left $ VeryBadBug "Cannot build script from local source"


-- generate the code required for a specific `source` statement
generatePoolCode
  :: [Function SNode]         -- list of functions
  -> Source
  -> ThrowsError String       -- complete code for the pool
generatePoolCode fs (SourceLang "R" i)
  = Right $
    (makePool g)
    (generateGlobal g)
    (generateSource g src)
    (generateFunctions g src fs)
  where
    g   = rCodeGenerator
    src = (SourceLang "R" i)
generatePoolCode _  (SourceFile _ _ _ )
  = Left $ NotImplemented "cannot yet read source"
generatePoolCode _  (SourceLang lang   _)
  = Left $ NotSupported ("ERROR: the language '" ++ lang ++ "' is not yet supported")
generatePoolCode _ SourceLocal
  = Left $ VeryBadBug "Cannot build script from local source"


generateGlobal :: CodeGenerator -> [String]
generateGlobal _ = []


generateSource :: CodeGenerator -> Source -> [String]
generateSource g src = [(makeSource g) src]


generateFunctions :: CodeGenerator -> Source -> [Function SNode] -> [String]
generateFunctions g src fs
  = map generateFunction -- [String]
  . concat               -- [SNode]
  . map toList           -- [[Snode]]
  . map getTree          -- [Tree SNode]
  $ fs
  where
    getTree :: Function SNode -> Tree SNode
    getTree (Function _ _ tree) = tree

    generateFunction :: SNode -> String
    generateFunction (SNode (w, SourceLocal) vars ss)
      = (makeFunction g)
        (makeNode g $ w)
        (generateManifoldArgs g vars)
        (generateCisBody g w vars ss)
    generateFunction (SNode (w, s) vars ss)
      | s == src = (makeFunction g)
                   (makeNode g $ w)
                   (generateManifoldArgs g vars)
                   (generateCisBody g w vars ss)
      | otherwise = (makeFunction g)
                    (makeNode g $ w)
                    (generateManifoldArgs g vars)
                    (generateTransBody g w vars ss)
    generateFunction (SLeaf i d)
      = (makeAssignment g)
        (makeNode g $ WLeaf i d)
        (makeMData g $ d)


generateManifoldArgs :: CodeGenerator -> [String] -> String
generateManifoldArgs g ss = makeArgs g . map Positional $ ss 


generateCisBody :: CodeGenerator -> WNode -> [String] -> [(WNode, Source)] -> String
generateCisBody g (WNode _ n _) vars ss
  | elem n vars = n
  | otherwise   = (makeFunctionCall g) n (generateArguments g vars ss) -- the pure function call
generateCisBody _ _ _ _ = undefined


generateTransBody :: CodeGenerator -> WNode -> [String] -> [(WNode, Source)] -> String
generateTransBody _ _ _ _ = "TRANS_STUB"


generateArguments :: CodeGenerator -> [String] -> [(WNode, Source)] -> String
generateArguments g vars ss = makeArgs g $ map (argument') (map fst ss)
  where
    argument' :: WNode -> Arg
    argument' (WNode (Just i) n a)
      | elem n vars = Positional n
      | otherwise   = Positional $
          (makeFunctionCall g)
          (makeNode g $ WNode (Just i) n a)
          (makeArgs g $ map Positional vars)
    argument' (WLeaf (Just i) d)
      = Positional (makeNode g $ WLeaf (Just i) d)
    argument' _ = Positional "ERROR"


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
