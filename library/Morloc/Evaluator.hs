module Morloc.Evaluator (eval) where

import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)
import Control.Applicative

import Morloc.Data
import qualified Morloc.Syntax as S
import qualified Morloc.Error as E

import qualified Morloc.Tree as G
import Morloc.Parser (morlocScript)

eval :: String -> E.ThrowsError Program
eval s = morlocScript s >>= script2program

script2program :: [S.Top] -> E.ThrowsError Program
script2program xs
  =   Program
  <$> workflow' xs
  <*> ontology' xs
  <*> packages' xs

ontology' :: [S.Top] -> E.ThrowsError [(String, TNode)]
ontology' xs
  = pure [
    (n, TNodeSignature i o c) |
    S.TopStatement (S.Signature n i o c) <- xs
  ]

packages' :: [S.Top] -> E.ThrowsError [S.Source]
packages' xs = pure [x | (S.TopSource x) <- xs]

workflow' :: [S.Top] -> E.ThrowsError [Function WNode]
workflow' xs
  = fmap addIDs
  . sequence
  $ [Function <$> pure n <*> pure args <*> callTree expr |
      (S.TopStatement (S.Declaration n args expr)) <- xs]
  where

    callTree :: S.Expression -> E.ThrowsError (G.Tree WNode)

    -- TODO: currently I allow heterogenous lists and only allows primitive data
    -- in Morloc containers. For example, `{ a = foo "yolo" }`, is illegal.
    -- Having workflows nested in Morloc containers is something that should be
    -- legal. But for now I won't allow it. 
    -- Anyway, as it is, the following will never raise an error. 
    callTree (S.ExprData mdata)
      = pure $ G.Node (WLeaf Nothing mdata) []

    -- parse an application
    callTree (S.ExprApplication (S.ExprVariable name tag) xs)
      = G.Node <$> pure (WNode Nothing name tag) <*> sequence (map callTree xs) 

    -- TODO: Make it work. I will need to recurse to the rightmost node in the
    -- composition and then set its children to `xs`. The the leftmost node
    -- will be the top WNode object that is returned by callTree.
    callTree (S.ExprApplication (S.ExprComposition l r) xs)
      = Left $ E.NotImplemented "Compositions cannot yet take arguments"

    callTree (S.ExprApplication _ _)
      = Left $ E.BadApplication "Must start with a function or composition" 

    -- parse a variable
    callTree (S.ExprVariable name tag)
      = G.Node <$> pure (WNode Nothing name tag) <*> pure []

    -- parse composition
    callTree (S.ExprComposition g f)
      = case (callTree g) of 
        Right (G.Node (WLeaf _ _) _)
          -> Left (E.BadComposition "Only functions can be composed")
        Right (G.Node v kids)
          -> G.Node <$> pure v <*> (app <$> pure kids <*> callTree f)
        err -> err

    -- add an element to the end of a list
    app :: [a] -> a -> [a]
    app xs x = xs ++ [x]

    addIDs :: [Function WNode] -> [Function WNode]
    addIDs fs'
      = zipWith
        (\(Function n vs _) tree -> Function n vs tree)
        fs'
        (addIDs' fs')

    addIDs' :: [Function WNode] -> [G.Tree WNode]
    addIDs' ws'
      = zipWith                         -- (a -> b -> c) -> [a] -> [b] -> [c]
        (G.zipWithTree setID)           -- (Tree Wnode -> Tree Int -> Tree Wnode)
        (numberTrees
          [t | (Function _ _ t) <- ws']) -- [Tree Int]
        [t | (Function _ _ t) <- ws']    -- [Tree WNode]

    setID :: Int -> WNode -> WNode
    setID i (WNode _ x y) = WNode (Just i) x y
    setID i (WLeaf _ x  ) = WLeaf (Just i) x

    numberTrees :: [G.Tree a] -> [G.Tree Int]
    numberTrees gs = numberTrees' 1 gs
      where
        numberTrees' :: Int -> [G.Tree a] -> [G.Tree Int]
        numberTrees' _ [] = []
        numberTrees' i [g] = [G.indexTree i g]
        numberTrees' i (g:gs') = case (G.indexTree i g) of
          g' -> g' : (numberTrees' (i + length g' + 1) gs')
