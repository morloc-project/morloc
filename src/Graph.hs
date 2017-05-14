module Graph
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
) where

import qualified Data.List as DL

data Graph a = Leaf a | Node a [Graph a] deriving(Show, Eq)

-- utilities ----------------------------------------------
values :: [Graph a] -> [a]
values = map v where
  v (Leaf x  ) = x 
  v (Node x _) = x 

{- kids :: Graph a -> [a]       -}
{- kids (Leaf _) = []           -}
{- kids (Node _ xs) = values xs -}
-----------------------------------------------------------


instance Functor Graph where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)

instance Foldable Graph where
  foldr f z (Leaf a) = f a z
  foldr f z (Node a []) = f a z
  foldr f z (Node a (x:xs)) = foldr f (foldr f z x) (Node a xs)

zipWithG :: (a -> b -> c) -> Graph a -> Graph b -> Graph c
zipWithG f (Leaf x) (Leaf y) = Leaf (f x y)
zipWithG f (Node x xs) (Node y ys) = Node (f x y) (zipWith (zipWithG f) xs ys)

isomorphic :: Graph a -> Graph b -> Bool
isomorphic (Leaf _) (Leaf _) = True
isomorphic (Node _ xs) (Node _ ys) = cmp_this && cmp_kids where
  cmp_this = length xs == length ys
  cmp_kids = foldr (&&) True $ zipWith isomorphic xs ys
isomorphic _ _ = False

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
pullG _ (Leaf x) = Leaf x
pullG f (Node x xs) = Node (foldr f x (values xs')) xs' where
  xs' = map (pullG f) xs

propagate :: (a -> [a] -> [a]) -> Graph a -> Graph a
propagate _ (Leaf x) = Leaf x  
propagate f (Node x xs) = Node x (map (propagate f) newkids) where
  newkids = zipWith replaceValue (f x (values xs)) xs

replaceValue :: a -> Graph a -> Graph a 
replaceValue x (Leaf _) = Leaf x
replaceValue x (Node _ xs) = Node x xs

-- My implementation is currently a bit wonky here, I have "nodes" that have no
-- children. These ought to be called leafs, but I have awkwardly interwoven
-- the idea of function into the definition of node. Where "primitive" and
-- "leaf" are the same. This might be close to being true, were it not if
-- impure functions, that say read a database (have input from outside).

isTerminal :: Graph a -> Graph Bool
isTerminal (Leaf _) = Leaf True
isTerminal (Node _ []) = Node True []
isTerminal (Node _ xs) = Node False (map isTerminal xs)

ifelseG :: Graph Bool -> (a -> b) -> (a -> b) -> Graph a -> Graph b
ifelseG gcond fa fb gx = zipWithG ternary' gx gcond where
  ternary' x cond = if cond then fa x else fb x

-- Graph to list, just a list of all a
toList :: Eq a => Graph a -> [a]
toList (Leaf x) = [x]
toList (Node x xs) = DL.union [x] (xs >>= toList)

-- modify parent by comparing to children
familyMap :: (a -> [a] -> b) -> Graph a -> Graph b
familyMap f (Leaf a) = Leaf $ f a []
familyMap f (Node t ts) = Node new_val new_kids  where
  new_val = f t $ values ts
  new_kids = map (familyMap f) ts

-- modify parents based only on children
childMap :: ([a] -> b) -> Graph a -> Graph b
childMap f (Leaf _) = Leaf (f [])
childMap f (Node _ ts) = Node new_val new_kids where
  new_val = f $ values ts
  new_kids = map (childMap f) ts

-- replace node values with parent/child relation lists
parentChildMap :: (a -> a -> b) -> Graph a -> Graph [b]
parentChildMap _ (Leaf _) = Leaf []
parentChildMap f (Node t ts) = Node new_val new_kids where
  new_val = map (f t) (values ts)
  new_kids = map (parentChildMap f) ts
  
-- like parentChildMap, but includes child order index
parentChildMapI :: (a -> (Int, a) -> b) -> Graph a -> Graph [b]
parentChildMapI _ (Leaf _) = Leaf []
parentChildMapI f (Node t ts) = Node new_val new_kids where
  new_val = map (f t) (zip [1..] (values ts))
  new_kids = map (parentChildMapI f) ts

{- popChild :: Graph a -> Maybe (Graph a)          -}
{- popChild (Leaf _)    = Nothing                  -}
{- popChild (Node _ []) = Nothing                  -}
{- popChild (Node n ts) = Just $ Node n (init ts)  -}
{-                                                 -}
{- topChild :: Graph a -> Maybe (Graph a)          -}
{- topChild (Leaf _)    = Nothing                  -}
{- topChild (Node n []) = Nothing                  -}
{- topChild (Node n ts) = Just $ head $ reverse ts -}


{- -- DF.concat :: t [a] -> [a]                                  -}
{- -- CM.liftM :: (a -> r) -> m a -> m r                         -}
{- -- g :: Graph -> [String]                                     -}
{- -- return :: a -> m a                                         -}
{- -- (>>=) :: m a -> (a -> m b) -> m b                          -}
{- showGraph :: (Graph a -> Maybe String) -> Graph a -> [String] -}
{- showGraph f t =                                               -}
{-      (DF.toList . f) t                                        -}
{-   ++ (DF.concat . g) (return t >>= popChild)                  -}
{-   ++ (DF.concat . g) (return t >>= topChild)                  -}
{-   where                                                       -}
{-     g = CM.liftM (showGraph f)                                -}
