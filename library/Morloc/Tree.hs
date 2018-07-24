module Morloc.Tree
(
    Tree(..)
  , rdf2tree
  , getKids
  , getStrings
  , hasRelation
  , recursiveApply
) where

import qualified Morloc.Triple as M3
import qualified Morloc.Error as ME
import qualified Morloc.Util as MU

data Tree
  = Node M3.Subject [(M3.Relation, Tree)]
  | Leaf M3.Object
  deriving(Ord, Eq, Show)

-- For the moment, this never fails. I wrap it the error monad to
-- 1) make space for extensions later to check the conversion to RDF
-- 2) be compatibile with the monadic pipeline

rdf2tree :: M3.RDF -> ME.ThrowsError Tree
rdf2tree rdf = Right $ rdf2tree' rdf
  where 
    rdf2tree' :: M3.RDF -> Tree
    rdf2tree' (M3.RDF i ts) = Node i [(r, f ts o) | (j,r,o) <- ts, j == i]

    f :: [M3.Triple] -> M3.Object -> Tree
    f ts' (M3.Id' j) = rdf2tree' (M3.RDF j ts')
    f _    o'     = Leaf o'

getKids :: M3.Relation -> Tree -> [Tree]
getKids r (Node _ xs) = [t | (r', t) <- xs, r == r']
getKids _ _ = []

getStrings :: M3.Relation -> Tree -> ME.ThrowsError [String]
getStrings r' t' = sequence . map getStrings' $ getKids r' t' where
  getStrings' :: Tree -> ME.ThrowsError String
  getStrings' (Leaf (M3.Str' s)) = Right s
  getStrings' _ = Left (ME.InvalidRDF ("Expected a string"))

hasRelation :: M3.Relation -> Tree -> Tree -> Bool
hasRelation r o (Node _ xs) = any (\(r', o') -> r' == r && o' == o) xs
hasRelation _ _ _ = False

recursiveApply
  :: (Tree -> Bool) -- Is this the subtree we are looking for?
  -> (Tree -> a)    -- If yes, use this to process the tree
  -> Tree           -- This is the tree we see at the moment 
  -> [a]            -- Results from all the trees we've processed
recursiveApply _ _ (Leaf _) = [] 
recursiveApply cond f (Node i xs)
  = MU.ifelse
      (cond (Node i xs))
      [f (Node i xs)]
      (concat . map (recursiveApply cond f . snd) $ xs)
