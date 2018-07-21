module Morloc.Tree
(
    Tree(..)
  , rdf2tree
  , getKids
  , getStrings
  , hasRelation
  , recursiveApply
) where

import Morloc.Triple
import Morloc.Error
import Morloc.Util (ifelse)

data Tree
  = Node Subject [(Relation, Tree)]
  | Leaf Object
  deriving(Ord, Eq, Show)

rdf2tree :: RDF -> Tree
rdf2tree (RDF i ts) = Node i [(r, f ts o) | (j,r,o) <- ts, j == i]
  where
    f :: [Triple] -> Object -> Tree
    f ts' (Id' j) = rdf2tree (RDF j ts')
    f _    o'     = Leaf o'

getKids :: Relation -> Tree -> [Tree]
getKids r (Node _ xs) = [t | (r', t) <- xs, r == r']
getKids _ _ = []

getStrings :: Relation -> Tree -> ThrowsError [String]
getStrings r' t' = sequence . map getStrings' $ getKids r' t' where
  getStrings' :: Tree -> ThrowsError String
  getStrings' (Leaf (Str' s)) = Right s
  getStrings' _ = Left (InvalidRDF ("Expected a string"))

hasRelation :: Relation -> Tree -> Tree -> Bool
hasRelation r o (Node _ xs) = any (\(r', o') -> r' == r && o' == o) xs
hasRelation _ _ _ = False

recursiveApply
  :: (Tree -> Bool) -- Is this the subtree we are looking for?
  -> (Tree -> a)    -- If yes, use this to process the tree
  -> Tree           -- This is the tree we see at the moment 
  -> [a]            -- Results from all the trees we've processed
recursiveApply _ _ (Leaf _) = [] 
recursiveApply cond f (Node i xs)
  = ifelse
      (cond (Node i xs))
      [f (Node i xs)]
      (concat . map (recursiveApply cond f . snd) $ xs)
