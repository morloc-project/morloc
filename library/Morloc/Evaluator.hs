module Morloc.Evaluator (eval) where

import Control.Monad.Except (throwError)

import Morloc.Data
import Morloc.Syntax as Syntax
import Morloc.EvalError as Error
import Morloc.Graph as Graph
import Morloc.Parser (parseExpr)


eval :: String -> Error.ThrowsError (Graph MData)
eval x = parseExpr x >>= expr2tree


expr2tree :: Syntax.Expr -> Error.ThrowsError (Graph MData)

-- curried nodes outside of compositions
--   e.g.  foo x y z
expr2tree (Syntax.Apply (Syntax.Value (MFunc s)) es) =
  Graph.Node (MFunc s) <$> traverse expr2tree es

-- simple nodes, composition without application
--   e.g.  foo . bar
expr2tree (Syntax.BinOp Syntax.Dot (Syntax.Value (MFunc s)) e) =
  Graph.Node (MFunc s) <$> traverse expr2tree [e]

-- simple nodes, composition with application
--   e.g.  foo x y . bar z
expr2tree (Syntax.BinOp Syntax.Dot (Syntax.Apply (Syntax.Value (MFunc s)) es) e) =
  Graph.Node (MFunc s) <$> traverse expr2tree (es ++ [e])

-- data
--   e.g.  [1,2,3]
expr2tree (Syntax.Value d) = return $ Graph.Node d []

-- throw error on all kinds of compositions not handled above
--   e.g.  foo . 1
expr2tree (Syntax.BinOp Syntax.Dot _ _) = throwError $ Error.BadComposition msg where
  msg = "Primitives cannot be on the left side of a composition"

-- throw error on all kinds of applications not handled above
--   e.g.  12 foo
expr2tree (Syntax.Apply _ _) = throwError $ Error.BadApplication msg where
  msg = "Primitives cannot take arguments"
