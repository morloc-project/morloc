{-|
Module      : Morloc.Frontend.AST
Description : Functions for parsing the Expr abstract syntax trees
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.AST
  ( findEdges
  , findExports
  ) where

import Morloc.Frontend.Namespace


-- | In the DAG, the two MVar are the two keys, Import is the edge data, Expr is the node data
-- Imports may only be at the top level (FIXME: allow local imports in declaration where statements)
findEdges :: Expr -> (MVar, [(MVar, Import)], Expr)
findEdges e@(ModE n es) = (n, [(importModuleName i, i)| (ImpE i) <- es], e)
findEdges _ = error "Expected a module"

findExports :: Expr -> [EVar]
findExports (ExpE v) = [v]
findExports (ModE _ es) = conmap findExports es
findExports _ = []

findTypedefs :: Expr -> [(TVar, [TVar], UnresolvedType)]
findTypedefs (TypE v vs t) = [(v, vs, t)] 
findTypedefs (ModE _ es) = conmap findTypedefs es
findTypedefs _ = []
