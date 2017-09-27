module Morloc.Graph
(
      Graph(..)
    , toList
    , familyMap
    , childMap
    , parentChildMap
    , parentChildMapI
    , isomorphic
    , zipWithG
    , safeZipWithG
    , zipG
    , safeZipG
    , isTerminal
    , ifelseG
    , pullG
    , propagate
    , replaceValue
    , suczip
) where

import Data.List (union)

data Graph a = Node a [Graph a] deriving(Show, Eq)

-- utilities ----------------------------------------------
values :: [Graph a] -> [a]
values = map v where
  v (Node x _) = x 

value :: Graph a -> a
value (Node x _) = x

{- kids :: Graph a -> [a]       -}
{- kids (Node _ xs) = values xs -}
-----------------------------------------------------------


instance Functor Graph where
  fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)

instance Foldable Graph where
  foldr f z (Node a []) = f a z
  foldr f z (Node a (x:xs)) = foldr f (foldr f z x) (Node a xs)

zipWithG :: (a -> b -> c) -> Graph a -> Graph b -> Graph c
zipWithG f (Node x xs) (Node y ys) = Node (f x y) (zipWith (zipWithG f) xs ys)

isomorphic :: Graph a -> Graph b -> Bool
isomorphic (Node _ xs) (Node _ ys) = cmp_this && cmp_kids where
  cmp_this = length xs == length ys
  cmp_kids = and $ zipWith isomorphic xs ys

safeZipWithG :: (a -> b -> c) -> Graph a -> Graph b -> Maybe (Graph c)
safeZipWithG f a b =
  if isomorphic a b then
    Just (zipWithG f a b)
  else
    Nothing

zipG :: Graph a -> Graph b -> Graph (a,b)
zipG = zipWithG (,)

safeZipG :: Graph a -> Graph b -> Maybe (Graph (a,b))
safeZipG = safeZipWithG (,)

pullG :: (a -> a -> a) -> Graph a -> Graph a
pullG f (Node x xs) = Node (foldr f x (values xs')) xs' where
  xs' = map (pullG f) xs

propagate :: (a -> [a] -> [a]) -> Graph a -> Graph a
propagate f (Node x xs) = Node x (map (propagate f) newkids) where
  newkids = zipWith replaceValue (f x (values xs)) xs

replaceValue :: a -> Graph a -> Graph a 
replaceValue x (Node _ xs) = Node x xs

isTerminal :: Graph a -> Graph Bool
isTerminal (Node _ []) = Node True []
isTerminal (Node _ xs) = Node False (map isTerminal xs)

ifelseG :: Graph Bool -> (a -> b) -> (a -> b) -> Graph a -> Graph b
ifelseG gcond fa fb gx = zipWithG ternary' gx gcond where
  ternary' x cond = if cond then fa x else fb x

-- | graph to list, just a list of all a
toList :: Eq a => Graph a -> [a]
toList (Node x xs) = [x] `union` (xs >>= toList)

-- | modify parent by comparing to children
familyMap :: (a -> [a] -> b) -> Graph a -> Graph b
familyMap f (Node t ts) = Node new_val new_kids  where
  new_val = f t $ values ts
  new_kids = map (familyMap f) ts

-- | modify parents based only on children
childMap :: ([a] -> b) -> Graph a -> Graph b
childMap f (Node _ ts) = Node new_val new_kids where
  new_val = f $ values ts
  new_kids = map (childMap f) ts

-- | replace node values with parent/child relation lists
parentChildMap :: (a -> a -> b) -> Graph a -> Graph [b]
parentChildMap f (Node t ts) = Node new_val new_kids where
  new_val = map (f t) (values ts)
  new_kids = map (parentChildMap f) ts
  
-- | like parentChildMap, but includes child order index
parentChildMapI :: (a -> (Int, a) -> b) -> Graph a -> Graph [b]
parentChildMapI f (Node t ts) = Node new_val new_kids where
  new_val = map (f t) (zip [1..] (values ts))
  new_kids = map (parentChildMapI f) ts

-- | A zip function that zips elements from a series (defined by a successor
-- function and an initial element) to elements of the graph. This function can
-- be used to map unique ids to nodes: `suczip (+ 1) 1 g`.
suczip :: (a -> a) -> a -> Graph b -> Graph (a,b)
suczip f x (Node y kids) = Node (x,y) (mapzip' f (f x) kids) where
  mapzip' :: (a -> a) -> a -> [Graph b] -> [Graph (a,b)]
  mapzip' _ _ [] = []
  mapzip' f' x' (t:ts) = [top] ++ mapzip' f' (fst . value $ top) ts where
    top = suczip f' x' t
