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
  , findExportSet
  , findSignatures
  , findTypedefs
  , findSignatureTypeTerms
  , checkExpr
  , findSources
  ) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import qualified Data.Map as Map


-- | In the DAG, the two MVar are the two keys, Import is the edge data, Expr is the node data
-- Imports may only be at the top level (FIXME: allow local imports in declaration where statements)
findEdges :: Expr -> (MVar, [(MVar, Import)], Expr)
findEdges e@(ModE n es) = (n, [(importModuleName i, i)| (ImpE i) <- es], e)
findEdges _ = error "Expected a module"

findExportSet :: Expr -> Set.Set EVar
findExportSet = Set.fromList . findExports

findExports :: Expr -> [EVar]
findExports (ExpE v) = [v]
findExports (ModE _ es) = conmap findExports es
findExports _ = []

findSources :: Expr -> [Source]
findSources (SrcE ss) = ss
findSources (ModE _ es) = conmap findSources es
findSources _ = []

findTypedefs :: Expr -> Map.Map TVar ([TVar], UnresolvedType)
findTypedefs (TypE v vs t) = Map.singleton v (vs, t)
findTypedefs (ModE _ es) = Map.unions (map findTypedefs es)
findTypedefs _ = Map.empty

findSignatureTypeTerms :: Expr -> [TVar]
findSignatureTypeTerms = unique . f where
  f :: Expr -> [TVar]
  f (ModE _ es) = conmap f es
  f (Signature _ (EType t _ _)) = findTypeTerms t
  f (Declaration _ _ es) = conmap f es
  f _ = []

-- | find all the non-generic terms in an unresolved type
findTypeTerms :: UnresolvedType -> [TVar]
findTypeTerms (VarU v)
  | isGeneric v = [ ]
  | otherwise   = [v]
findTypeTerms (ExistU _ es1 es2) = conmap findTypeTerms (es1 ++ es2)
findTypeTerms (ForallU _ e) = findTypeTerms e
findTypeTerms (FunU e1 e2) = findTypeTerms e1 ++ findTypeTerms e2
findTypeTerms (ArrU v es) = findTypeTerms (VarU v) ++ conmap findTypeTerms es
findTypeTerms (NamU _ v es rs) = findTypeTerms (VarU v) ++ conmap findTypeTerms (es ++ map snd rs)

-- | Find type signatures that are in the scope of the input expression. Do not
-- descend recursively into declaration where statements except if the input
-- expression is a declaration.
findSignatures :: Expr -> [(EVar, EType)]
findSignatures (ModE _ es) = [(v, t) | (Signature v t) <- es]
findSignatures (Declaration _ _ es) = [(v, t) | (Signature v t) <- es]
findSignatures (Signature v t) = [(v,t)]
findSignatures _ = []

checkExpr :: Monad m => (Expr -> m ()) -> Expr -> m ()
checkExpr f e@(ModE _ es) = f e >> mapM_ (checkExpr f) es
checkExpr f e@(AccE e' _) = f e >> checkExpr f e'
checkExpr f e@(AnnE e' _) = f e >> checkExpr f e'
checkExpr f e@(AppE e1 e2) = f e >> checkExpr f e1 >> checkExpr f e2
checkExpr f e@(Declaration _ e' es') = f e >> checkExpr f e' >> mapM_ f es'
checkExpr f e@(LamE _ e') = f e >> checkExpr f e'
checkExpr f e@(ListE es') = f e >> mapM_ f es'
checkExpr f e@(RecE rs) = f e >> mapM_ f (map snd rs)
checkExpr f e@(TupleE es') = f e >> mapM_ f es'
checkExpr f e = f e
