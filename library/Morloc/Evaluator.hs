module Morloc.Evaluator (eval) where

import Control.Monad.Except (throwError)

import Morloc.Syntax as Syntax
import Morloc.EvalError as Error
import Morloc.Graph as Graph
import Morloc.NodeAttribute
import Morloc.Parser (parseExpr)


-- see Note 0
eval :: String -> Error.ThrowsError (Graph NodeAttr)
eval x = setid <$> (parseExpr x >>= expr2tree)


-- see Note 1
setid :: Graph NodeAttr -> Graph NodeAttr
setid g = fst <$> propagate base (zipG zeroed gcount) where

  zeroed = fmap (\attr -> attr { nodeID = Just 0 }) g

  -- base :: a -> [a] -> [a]
  base (_,i) gs' = zipWith set_child_id gs' child_ids where
    set_child_id (attr,_) j = (attr { nodeID = Just j }, j)
    child_ids = map (+ i) $ scanl1 (+) (map snd gs')

  -- gcount :: Graph Int -- graph with descendent counts
  gcount = pullG (+) $ ifelseG (isTerminal g) (const 1) (const 0) g


-- see Note 2
expr2tree :: Syntax.Expr -> Error.ThrowsError (Graph NodeAttr)
-- curried nodes outside of compositions
expr2tree (Syntax.Apply (Syntax.Node s) es) =
  Graph.Node (nodeAttrS s) <$> traverse expr2tree es
-- simple nodes, composition without application
expr2tree (Syntax.BinOp Syntax.Dot (Syntax.Node s) e) =
  Graph.Node (nodeAttrS s) <$> traverse expr2tree [e]
-- simple nodes, composition with application
expr2tree (Syntax.BinOp Syntax.Dot (Syntax.Apply (Syntax.Node s) es) e) =
  Graph.Node (nodeAttrS s) <$> traverse expr2tree (es ++ [e])
-- singletons
expr2tree (Syntax.Node    x) = return $ Graph.Node (nodeAttrS x) []
expr2tree (Syntax.Float   x) = return $ Graph.Node ((nodeAttrS $ show x) {nodeType = Just "Float",   primitive = Just True}) []
expr2tree (Syntax.Integer x) = return $ Graph.Node ((nodeAttrS $ show x) {nodeType = Just "Integer", primitive = Just True}) []
expr2tree (Syntax.String  x) = return $ Graph.Node ((nodeAttrS        x) {nodeType = Just "String",  primitive = Just True}) []
-- throw error on all kinds of compositions not handled above
expr2tree (Syntax.BinOp Syntax.Dot _ _) = throwError $ Error.BadComposition msg where
  msg = "Primitives cannot be on the left side of a composition"
-- throw error on all kinds of applicaitons not handled above
expr2tree (Syntax.Apply _ _) = throwError $ Error.BadApplication msg where
  msg = "Primitives cannot take arguments"


------- NOTE 0 ------------------------------------------------------
-- parseExpr :: String -> ThrowsError Expr
-- expr2tree :: Expr -> ThrowsError (Graph NodeAttr)
-- setid :: Graph NodeAttr -> GraphNodeAttr
--
--    a -> e c
-- ===========================
--    (1) parseExpr :: a -> e b
--    (2) expr2tree :: b -> e c
--    (3) setid     :: c -> c
-- ---------------------------
--    (4) fmap 3 :: e c -> e c
--    (5) bind :: e b -> (b -> e c) -> e c
-- ------------------------------------------
--    (6) 5 (1 a) 2 :: e c
--    (7) 4 6 :: e c
-- > setid <$> parseExpr x >>= expr2tree
---------------------------------------------------------------------


------- NOTE 1 ------------------------------------------------------
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
---------------------------------------------------------------------



------- NOTE 2 ------------------------------------------------------
--
--     [MorlocError a], if any element in the list is a failure, return a failure
--     otherwise, extract the pure list, e.g. [MorlocError a] -> [a]
--     The [a] is fed as input to the Node constructor
--
--  let
--    a := Expr
--    b := Graph
--    l := list monad
--    e := error monad
--
--  Overall:
--      (0)    l a -> e b
--
--  Simple components:
--      (1)    l b -> b
--      (2)    a -> e a
--
--  (1) is the constructor Node (builds a node from inputs). I'll ignore the
--      String argument (it can just be partially applied away)
--  (2) is a expr2tree itself
--
--  Derived:
--      (3)    l a -> l (e b)
--      (4)    l (e b) -> e (l b)
--      (5)    e (l b) -> e b
--
--  (3) is expr2tree lifted into l
--  (4) matches the general case (Data.Traversable):
--        sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
--      It also matches the more specific function from Control.Monad:
--        sequence :: Monad m => [m a] -> m [a]
--  (5) is just (1) lifted into e
--
--  Putting all this together:
--       (5) . (4) . (3) :: (0)
--  filling in the functions we get:
--  (liftM Node) . sequence . (liftM expr2tree)
---------------------------------------------------------------------
