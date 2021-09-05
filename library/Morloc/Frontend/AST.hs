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
  , checkExprI
  , findSources
  , maxIndex
  ) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Morloc.Data.Text as MT


-- | In the DAG, the two MVar are the two keys, Import is the edge data, Expr is the node data
-- Imports may only be at the top level (FIXME: allow local imports in declaration where statements)
findEdges :: ExprI -> (MVar, [(MVar, Import)], ExprI)
findEdges e@(ExprI _ (ModE n es)) = (n, [(importModuleName i, i)| (ExprI _ (ImpE i)) <- es], e)
findEdges _ = error "Expected a module"

findExportSet :: ExprI -> Set.Set EVar
findExportSet = Set.fromList . map snd . findExports

findExports :: ExprI -> [(Int, EVar)]
findExports (ExprI i (ExpE v)) = [(i, v)]
findExports (ExprI i (ModE _ es)) = conmap findExports es
findExports _ = []

findSources :: ExprI -> [Source]
findSources (ExprI _ (SrcE ss)) = [ss]
findSources (ExprI _ (ModE _ es)) = conmap findSources es
findSources _ = []

findTypedefs :: ExprI -> Map.Map TVar ([TVar], TypeU)
findTypedefs (ExprI _ (TypE v vs t)) = Map.singleton v (vs, t)
findTypedefs (ExprI _ (ModE _ es)) = Map.unions (map findTypedefs es)
findTypedefs _ = Map.empty

findSignatureTypeTerms :: ExprI -> [TVar]
findSignatureTypeTerms = unique . f where
  f :: ExprI -> [TVar]
  f (ExprI _ (ModE _ es)) = conmap f es
  f (ExprI _ (Signature _ _ (EType t _ _))) = findTypeTerms t
  f (ExprI _ (Declaration _ _ es)) = conmap f es
  f _ = []

-- | find all the non-generic terms in an unresolved type
findTypeTerms :: TypeU -> [TVar]
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
findSignatures :: ExprI -> [(EVar, Maybe MT.Text, EType)]
findSignatures (ExprI _ (ModE _ es)) = [(v, l, t) | (ExprI _ (Signature v l t)) <- es]
findSignatures (ExprI _ (Declaration _ _ es)) = [(v, l, t) | (ExprI _ (Signature v l t)) <- es]
findSignatures (ExprI _ (Signature v l t)) = [(v, l, t)]
findSignatures _ = []

checkExprI :: Monad m => (ExprI -> m ()) -> ExprI -> m ()
checkExprI f e@(ExprI _ (ModE _ es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (AccE e' _)) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AnnE e' _)) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AppE g es)) = f e >> checkExprI f g >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (Declaration _ e' es')) = f e >> checkExprI f e' >> mapM_ f es'
checkExprI f e@(ExprI _ (LamE _ e')) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (ListE es')) = f e >> mapM_ f es'
checkExprI f e@(ExprI _ (RecE rs)) = f e >> mapM_ f (map snd rs)
checkExprI f e@(ExprI _ (TupleE es')) = f e >> mapM_ f es'
checkExprI f e = f e

maxIndex :: ExprI -> Int
maxIndex (ExprI i (ModE _ es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (AccE e _)) = max i (maxIndex e)
maxIndex (ExprI i (AnnE e _)) = max i (maxIndex e)
maxIndex (ExprI i (AppE f es)) = maximum ([i, maxIndex f] <> map maxIndex es)
maxIndex (ExprI i (Declaration _ e es)) = maximum (i : map maxIndex (e:es))
maxIndex (ExprI i (LamE _ e)) = max i (maxIndex e)
maxIndex (ExprI i (ListE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (TupleE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (RecE rs)) = maximum (i : map maxIndex (map snd rs))
maxIndex (ExprI i _) = i
