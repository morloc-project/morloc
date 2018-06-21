module Morloc.Tree
(
      Tree(..)
    , toList
    , familyMap
    , childMap
    , parentChildMap
    , parentChildMapI
    , isomorphic
    , zipWithT
    , safeZipWithT
    , zipT
    , safeZipT
    , isTerminal
    , ifelseT
    , pullT
    , propagate
    , replaceValue
    , suczip
    , combinT
    , numberT
) where

import Data.List (union, transpose)

data Tree a = Node a [Tree a] deriving(Show, Eq)

instance Functor Tree where
  fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)

instance Foldable Tree where
  foldr f z (Node a []) = f a z
  foldr f z (Node a (x:xs)) = foldr f (foldr f z x) (Node a xs)

instance Traversable Tree where
-- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
   traverse f (Node x xs) = Node <$> f x <*> (traverse . traverse) f xs

instance Applicative Tree where
  pure x = Node x []
  -- (<*>) :: f (a -> b) -> f a -> f b
  (Node f fs) <*> (Node x xs) = Node (f x) (zipWith (<*>) fs xs)

-- TODO this algorithm skips numbers sometimes, don't know why ...
-- need to fix the damn thing
numberT :: Int -> Tree a -> Tree (Int, a)
numberT i (Node x [])     = Node (i, x) []
numberT i (Node x (k:ks)) = Node (i, x) (kids' (i+1) k ks) where
  kids' i x [] = [numberT i x]
  kids' i x (y:ys) = case numberT i x of 
    g' -> g' : kids' (length g' + i) y ys

-- utilities ----------------------------------------------
values :: [Tree a] -> [a]
values = map v where
  v (Node x _) = x 

value :: Tree a -> a
value (Node x _) = x

kids :: Tree a -> [Tree a]
kids (Node _ xs) = xs
-----------------------------------------------------------

zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithT f (Node x xs) (Node y ys) = Node (f x y) (zipWith (zipWithT f) xs ys)

isomorphic :: Tree a -> Tree b -> Bool
isomorphic (Node _ xs) (Node _ ys) = cmp_this && cmp_kids where
  cmp_this = length xs == length ys
  cmp_kids = and $ zipWith isomorphic xs ys

safeZipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
safeZipWithT f a b =
  if isomorphic a b then
    Just (zipWithT f a b)
  else
    Nothing

zipT :: Tree a -> Tree b -> Tree (a,b)
zipT = zipWithT (,)

safeZipT :: Tree a -> Tree b -> Maybe (Tree (a,b))
safeZipT = safeZipWithT (,)

pullT :: (a -> a -> a) -> Tree a -> Tree a
pullT f (Node x xs) = Node (foldr f x (values xs')) xs' where
  xs' = map (pullT f) xs

propagate :: (a -> [a] -> [a]) -> Tree a -> Tree a
propagate f (Node x xs) = Node x (map (propagate f) newkids) where
  newkids = zipWith replaceValue (f x (values xs)) xs

replaceValue :: a -> Tree a -> Tree a 
replaceValue x (Node _ xs) = Node x xs

isTerminal :: Tree a -> Tree Bool
isTerminal (Node _ []) = Node True []
isTerminal (Node _ xs) = Node False (map isTerminal xs)

ifelseT :: Tree Bool -> (a -> b) -> (a -> b) -> Tree a -> Tree b
ifelseT gcond fa fb gx = zipWithT ternary' gx gcond where
  ternary' x cond = if cond then fa x else fb x

-- | tree to list, just a list of all a
toList :: Eq a => Tree a -> [a]
toList (Node x xs) = [x] `union` (xs >>= toList)

-- | modify parent by comparing to children
familyMap :: (a -> [a] -> b) -> Tree a -> Tree b
familyMap f (Node t ts) = Node new_val new_kids  where
  new_val = f t (values ts)
  new_kids = map (familyMap f) ts

-- | modify parents based only on children
childMap :: ([a] -> b) -> Tree a -> Tree b
childMap f (Node _ ts) = Node new_val new_kids where
  new_val = f $ values ts
  new_kids = map (childMap f) ts

-- | replace node values with parent/child relation lists
parentChildMap :: (a -> a -> b) -> Tree a -> Tree [b]
parentChildMap f (Node t ts) = Node new_val new_kids where
  new_val = map (f t) (values ts)
  new_kids = map (parentChildMap f) ts
  
-- | like parentChildMap, but includes child order index
parentChildMapI :: (a -> (Int, a) -> b) -> Tree a -> Tree [b]
parentChildMapI f (Node t ts) = Node new_val new_kids where
  new_val = map (f t) (zip [1..] (values ts))
  new_kids = map (parentChildMapI f) ts

-- | A zip function that zips elements from a series (defined by a successor
-- function and an initial element) to elements of the tree. This function can
-- be used to map unique ids to nodes: `suczip (+ 1) 1 g`.
suczip :: (a -> a) -> a -> Tree b -> Tree (a,b)
suczip f x (Node y kids) = Node (x,y) (mapzip' f (f x) kids) where
  mapzip' :: (a -> a) -> a -> [Tree b] -> [Tree (a,b)]
  mapzip' _ _ [] = []
  mapzip' f' x' (t:ts) = top : mapzip' f' (fst . value $ top) ts where
    top = suczip f' x' t

-- | Given a list of isomorphic trees, combine all into a single tree.
combinT :: [Tree a] -> Tree [a]
combinT gs = Node (map value gs) (map combinT . transpose . map kids $ gs)
