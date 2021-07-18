{-|
Module      : Morloc.Frontend.Treeify
Description : Translate from the frontend DAG to the backend SAnno AST forest
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Treeify (treeify) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import Morloc.Frontend.PartialOrder ()
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Data.Map as Map
import qualified Data.Set as Set

data TermOrigin = Declared Expr | Sourced Source
  deriving(Show, Ord, Eq)

treeify
  :: DAG MVar [(EVar, EVar)] Expr
  -> MorlocMonad [SAnno GU Many [UnresolvedType]]
treeify d
  | Map.size d == 0 = return []
  | otherwise = case MDD.roots d of
    -- if no parentless element exists, then the graph must be empty or cyclic
    [] -> MM.throwError CyclicDependency
    [k] -> case MDD.lookupNode k d of
      Nothing -> MM.throwError . DagMissingKey . render $ pretty k
      (Just e) -> do
        -- initialize state counter to 0, used to index manifolds
        MM.startCounter
        mapM (collect d k e) (Set.toList (AST.findExportSet e))
    -- There is no currently supported use case that exposes multiple roots in
    -- one compilation process. The compiler executable takes a single morloc
    -- file as input, therefore this MUST be the root. In the future compiling
    -- multiple projects in parallel with potentially shared information and
    -- constraints could be valuable.
    _ -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"

-- | Build the call tree for a single nexus command. The result is ambiguous,
-- with 1 or more possible tree topologies, each with one or more possible for
-- each function.
collect
  :: DAG MVar [(EVar, EVar)] Expr
  -> MVar
  -> Expr
  -> EVar
  -> MorlocMonad (SAnno GU Many [UnresolvedType])
collect = undefined

-- | Find the user provided, or module imported, general type annotations and
-- collect info needed for the GMeta object
collectSAnno
  :: DAG MVar [(EVar, EVar)] Expr
  -> MVar
  -> Expr
  -> MorlocMonad (SAnno GU Many [UnresolvedType])
collectSAnno = undefined

-- | Find all definitions of a term and collect their type annotations, if available
collectSExpr
  :: DAG MVar [(EVar, EVar)] Expr
  -> MVar
  -> Expr
  -> MorlocMonad [(SExpr GU Many [UnresolvedType], [UnresolvedType])]
collectSExpr = undefined
