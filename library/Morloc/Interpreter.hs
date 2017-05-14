module Morloc.Interpreter
(
    eval
  , toLIL
) where

import qualified Data.List as DL
import qualified Control.Monad as CM
import qualified Control.Monad.Except as CE

import qualified Morloc.Syntax as S
import qualified Morloc.EvalError as E
import Morloc.Graph
import Morloc.NodeAttribute


eval :: S.Expr -> E.ThrowsError (Graph NodeAttr)
eval e = CM.liftM setid $ expr2tree e

--   s0 := setid :: Graph NodeAttr -> Graph NodeAttr
--   ===============================================
--   s1 := zipG   :: Graph a -> Graph b -> Graph (a,b)
-- > s2 := g      :: Graph NodeAttr
--   s3 := gcount :: Graph Int
--   -----------------------------------------------
--   a :: NodeAttr  ;  b :: Int
--   -----------------------------------------------
--   s4 := propagate :: (a -> [a] -> [a]) -> Graph a -> Graph a
--   s5 := base      :: a -> [a] -> [a]
--   s6 := s1 s2 s3  :: Graph (NodeAttr, Int)
--   -----------------------------------------------
--   s7 := s4 s5 s6 :: Graph (NodeAttr, Int)
-- < s8 := fmap fst :: Graph (NodeAttr, Int) -> Graph NodeAttr
--   -----------------------------------------------
--   s8 s6 :: Graph NodeAttr  
--   ===============================================
--
--
--   gcount :: Graph Int
--   ==========================================================
--   s9  := pullG      :: Monoid a => (a -> a -> a) -> Graph a -> Graph a
--   s10 := ifelseG    :: Graph Bool -> (a -> b) -> (a -> b) -> Graph a -> Graph b
--   s11 := isTerminal :: Graph a -> Graph Bool
--   s12 := const      :: a -> b -> a
--   ----------------------------------------------------------
--   s13 := pullG (+)    :: Num a => Graph a -> Graph a
--   s14 := isTerminal g :: Bool
--   s15 := const 1      :: b -> Int
--   ----------------------------------------------------------
--                       b :: Int
--   ----------------------------------------------------------
--   s16 := ifelseG (isTermiminal g) (const 1) (const 0) ::
--            Graph NodeAttr -> Graph Int
--   ----------------------------------------------------------
-- < s16 g := Graph Int
--   ==========================================================
setid :: Graph NodeAttr -> Graph NodeAttr
setid g = fmap fst $ propagate base (zipG zeroed gcount) where
  zeroed = fmap (\attr -> attr { node_id = Just 0 }) g
  -- base :: a -> [a] -> [a]
  base (_,i) gs' = zipWith set_child_id gs' child_ids where
    set_child_id (attr,_) j = (attr { node_id = Just j }, j)
    child_ids = map (+ i) $ scanl1 (+) (map snd gs')
  -- gcount :: Graph Int -- graph with descendent counts
  gcount = pullG (+) $ ifelseG (isTerminal g) (const 1) (const 0) g


-- see Note
expr2tree :: S.Expr -> E.ThrowsError (Graph NodeAttr)
-- curried nodes outside of compositions
expr2tree (S.Apply (S.Node s) es) =
  CM.liftM (Node $ nodeAttrS s) $ CM.sequence $ CM.liftM expr2tree $ es where
-- simple nodes, composition without application
expr2tree (S.BinOp S.Dot (S.Node s) e) =
  CM.liftM (Node $ nodeAttrS s) $ CM.sequence $ [expr2tree e]
-- simple nodes, composition with application
expr2tree (S.BinOp S.Dot (S.Apply (S.Node s) es) e) =
  CM.liftM (Node $ nodeAttrS s) $ CM.sequence $ CM.liftM expr2tree $ es ++ [e]
-- singletons
expr2tree (S.Node    x) = return $ Node (nodeAttrS x) []
expr2tree (S.Float   x) = return $ Node ((nodeAttrS $ show x) {node_type = Just "Float",   primitive = True}) []
expr2tree (S.Integer x) = return $ Node ((nodeAttrS $ show x) {node_type = Just "Integer", primitive = True}) []
expr2tree (S.String  x) = return $ Node ((nodeAttrS        x) {node_type = Just "String",  primitive = True}) []
-- throw error on all kinds of compositions not handled above
expr2tree (S.BinOp S.Dot _ _) = CE.throwError $ E.BadComposition msg where
  msg = "Primitives cannot be on the left side of a composition"
-- throw error on all kinds of applicaitons not handled above
expr2tree (S.Apply _ _) = CE.throwError $ E.BadApplication msg where
  msg = "Primitives cannot take arguments"


toLIL :: Graph NodeAttr -> String 
toLIL g = unlines $ foldr1 (++) $ parentChildMapI topLIL g where
  -- connect the parent to the top child 
  -- this function will be used by Graph.familyMap
  topLIL :: NodeAttr -> (Int, NodeAttr) -> String
  topLIL p (i, c) = join [pval', pid', pos', typ', nam']
      where
      pval' = case node_value p of
        Just x -> x
        Nothing -> "NO_VALUE" -- should throw error
      pid' = case node_id p of
        Just s -> show s
        Nothing -> "NO_ID" -- should throw error
      pos' = show i
      typ' = case node_type c of
        Just s -> s
        Nothing -> "*"
      nam' = case node_value c of
        Just s -> s
        Nothing -> "NO_VALUE" -- should throw error
      join :: [String] -> String
      join = foldr (++) "" . DL.intersperse "\t"

{- Note

     [MorlocError a], if any element in the list is a failure, return a failure
     otherwise, extract the pure list, e.g. [MorlocError a] -> [a]
     The [a] is fed as input to the Node constructor

  let
    a := Expr
    b := Graph
    l := list monad
    e := error monad

  Overall:
      (0)    l a -> e b

  Simple components:
      (1)    l b -> b
      (2)    a -> e a

  (1) is the constructor Node (builds a node from inputs). I'll ignore the
      String argument (it can just be partially applied away)
  (2) is a expr2tree itself

  Derived:
      (3)    l a -> l (e b)
      (4)    l (e b) -> e (l b)
      (5)    e (l b) -> e b

  (3) is expr2tree lifted into l
  (4) matches the general case (Data.Traversable):
        sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
      It also matches the more specific function from Control.Monad:
        sequence :: Monad m => [m a] -> m [a]
  (5) is just (1) lifted into e

  Putting all this together:
       (5) . (4) . (3) :: (0)
  filling in the functions we get:
  (liftM Node) . sequence . (liftM expr2tree)
-}
