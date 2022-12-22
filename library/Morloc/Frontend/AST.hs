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
  , getIndices
  ) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Morloc.Data.Text as MT


-- | In the DAG, the two MVar are the two keys, Import is the edge data, Expr is the node data
findEdges :: ExprI -> (MVar, [(MVar, Import)], ExprI)
findEdges e@(ExprI _ (ModE n es)) = (n, [(importModuleName i, i) | (ExprI _ (ImpE i)) <- es], e)
findEdges _ = error "Expected a module"

findExportSet :: ExprI -> Set.Set Symbol
findExportSet = Set.fromList . map snd . findExports

findExports :: ExprI -> [(Int, Symbol)]
findExports (ExprI i (ExpE v)) = [(i, v)]
findExports (ExprI _ (ModE _ es)) = concatMap findExports es
findExports _ = []

findSources :: ExprI -> [Source]
findSources (ExprI _ (SrcE ss)) = [ss]
findSources (ExprI _ (ModE _ es)) = concatMap findSources es
findSources _ = []

findTypedefs :: ExprI -> Map.Map TVar ([TVar], TypeU)
findTypedefs (ExprI _ (TypE v vs t)) = Map.singleton v (vs, t)
findTypedefs (ExprI _ (ModE _ es)) = Map.unions (map findTypedefs es)
findTypedefs _ = Map.empty

findSignatureTypeTerms :: ExprI -> [TVar]
findSignatureTypeTerms = unique . f where
  f :: ExprI -> [TVar]
  f (ExprI _ (ModE _ es)) = concatMap f es
  f (ExprI _ (SigE _ _ (EType t _ _))) = findTypeTerms t
  f (ExprI _ (AssE _ _ es)) = concatMap f es
  f _ = []

-- | find all the non-generic terms in an unresolved type
findTypeTerms :: TypeU -> [TVar]
findTypeTerms (VarU v)
  | isGeneric v = [ ]
  | otherwise   = [v]
findTypeTerms (ExistU _ es1 es2) = concatMap findTypeTerms (es1 ++ es2)
findTypeTerms (ForallU _ e) = findTypeTerms e
findTypeTerms (FunU ts t) = concatMap findTypeTerms ts <> findTypeTerms t
findTypeTerms (AppU t ts) = findTypeTerms t <> concatMap findTypeTerms ts
findTypeTerms (NamU _ _ ps rs) = concatMap findTypeTerms (map snd rs <> ps)

-- | Find type signatures that are in the scope of the input expression. Do not
-- descend recursively into declaration where statements except if the input
-- expression is a declaration.
findSignatures :: ExprI -> [(EVar, Maybe MT.Text, EType)]
-- v is the name of the type
-- l is the optional label for the signature
-- t is the type
findSignatures (ExprI _ (ModE _ es)) = [(v, l, t) | (ExprI _ (SigE v l t)) <- es]
findSignatures (ExprI _ (AssE _ _ es)) = [(v, l, t) | (ExprI _ (SigE v l t)) <- es]
findSignatures (ExprI _ (SigE v l t)) = [(v, l, t)]
findSignatures _ = []

checkExprI :: Monad m => (ExprI -> m ()) -> ExprI -> m ()
checkExprI f e@(ExprI _ (ModE _ es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (AccE e' _)) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AnnE e' _)) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AssE _ e' es')) = f e >> checkExprI f e' >> mapM_ f es'
checkExprI f e@(ExprI _ (LamE _ e')) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AppE e' es)) = f e >> checkExprI f e' >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (LstE es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (TupE es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (NamE rs)) = f e >> mapM_ (checkExprI f . snd) rs
checkExprI f e = f e

maxIndex :: ExprI -> Int
maxIndex (ExprI i (ModE _ es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (AccE e _)) = max i (maxIndex e)
maxIndex (ExprI i (AnnE e _)) = max i (maxIndex e)
maxIndex (ExprI i (AssE _ e es)) = maximum (i : map maxIndex (e:es))
maxIndex (ExprI i (LamE _ e)) = max i (maxIndex e)
maxIndex (ExprI i (AppE e es)) = maximum (i : map maxIndex (e:es))
maxIndex (ExprI i (LstE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (TupE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (NamE rs)) = maximum (i : map (maxIndex . snd) rs)
maxIndex (ExprI i _) = i

getIndices :: ExprI -> [Int]
getIndices (ExprI i (ModE _ es)) = i : concatMap getIndices es
getIndices (ExprI i (AccE e _)) = i : getIndices e
getIndices (ExprI i (AnnE e _)) = i : getIndices e
getIndices (ExprI i (AssE _ e es)) = i : concatMap getIndices (e:es)
getIndices (ExprI i (LamE _ e)) = i : getIndices e
getIndices (ExprI i (AppE e es)) = i : concatMap getIndices (e:es)
getIndices (ExprI i (LstE es)) = i : concatMap getIndices es
getIndices (ExprI i (TupE es)) = i : concatMap getIndices es
getIndices (ExprI i (NamE rs)) = i : concatMap (getIndices . snd) rs
getIndices (ExprI i _) = [i]
