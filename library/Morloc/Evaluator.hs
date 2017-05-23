module Morloc.Evaluator (eval) where

import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)

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

-- atomic data - Int, Num, Str, Bool, etc
expr2tree (Syntax.Value d) = return $ Graph.Node d []

-- array
--   e.g. [1,1.4,12], [True,False],  etc
expr2tree (Syntax.Array xs)
  | all (atype "Int")    xs = return $ Graph.Node ( MInts    $ mapMaybe e2mints    xs ) []
  | all (atype "String") xs = return $ Graph.Node ( MStrings $ mapMaybe e2mstrings xs ) []
  | all (atype "Bool")   xs = return $ Graph.Node ( MBools   $ mapMaybe e2mbools   xs ) []
  | all (atype "Num")    xs = return $ Graph.Node ( MNums    $ mapMaybe e2mnums    xs ) []
  | otherwise = throwError $ Error.BadArray "Arrays must be homogenous atomic collections"
  where
    e2mints :: Expr -> Maybe Integer
    e2mints (Value (MInt x)) = Just x
    e2mints _                = Nothing

    e2mstrings :: Expr -> Maybe String
    e2mstrings (Value (MString x)) = Just x
    e2mstrings _                   = Nothing

    e2mbools :: Expr -> Maybe Bool
    e2mbools (Value (MBool x)) = Just x
    e2mbools _                 = Nothing

    e2mnums :: Expr -> Maybe Double
    e2mnums (Value (MInt x)) = Just (read $ show x :: Double)
    e2mnums (Value (MNum x)) = Just x
    e2mnums _                = Nothing

    atype :: String -> Expr -> Bool
    atype "Int"    (Value (MInt    _)) = True
    atype "Num"    (Value (MInt    _)) = True
    atype "Num"    (Value (MNum    _)) = True
    atype "String" (Value (MString _)) = True
    atype "Bool"   (Value (MBool   _)) = True
    atype _ _ = False


-- throw error on all kinds of compositions not handled above
--   e.g.  foo . 1
expr2tree (Syntax.BinOp Syntax.Dot _ _) = throwError $ Error.BadComposition msg where
  msg = "Primitives cannot be on the left side of a composition"

-- throw error on all kinds of applications not handled above
--   e.g.  12 foo
expr2tree (Syntax.Apply _ _) = throwError $ Error.BadApplication msg where
  msg = "Primitives cannot take arguments"
