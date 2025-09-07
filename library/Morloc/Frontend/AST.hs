{-|
Module      : Morloc.Frontend.AST
Description : Functions for parsing the Expr abstract syntax trees
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.AST
  ( findEdges
  , findExport
  , findExportSet
  , setExport
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
import qualified Morloc.Data.Map as Map


-- | In the DAG, the two MVar are the two keys, Import is the edge data, Expr is the node data
findEdges :: ExprI -> (MVar, [(MVar, Import)], ExprI)
findEdges e@(ExprI _ (ModE n es)) = (n, [(importModuleName i, i) | (ExprI _ (ImpE i)) <- es], e)
findEdges _ = error "Expected a module"

findExportSet :: ExprI -> Set.Set Symbol
findExportSet e = case findExport e of
    (ExportMany ss) -> Set.map snd ss
    _ -> Set.empty

findExport :: ExprI -> Export
findExport e0 = case f e0 of
    (Just export) -> export
    Nothing -> ExportMany Set.empty
    where
    f (ExprI _ (ExpE export)) = Just export
    f (ExprI i (ModE j (e:es))) = case f e of
        (Just export) -> Just export
        Nothing -> f (ExprI i (ModE j es))
    f _ = Nothing

setExport :: Export -> ExprI -> ExprI
setExport export e0 = f e0
    where
    f (ExprI i (ExpE _)) = ExprI i (ExpE export)
    f (ExprI i (ModE m es)) = ExprI i (ModE m (map f es))
    f e = e

findSources :: ExprI -> [Source]
findSources (ExprI _ (SrcE ss)) = [ss]
findSources (ExprI _ (ModE _ es)) = concatMap findSources es
findSources _ = []


-- find all top-level concrete and general type functions in a module
findTypedefs
  :: ExprI
  -> (               Map.Map TVar [([Either TVar TypeU], TypeU, Bool)]
     , Map.Map Lang (Map.Map TVar [([Either TVar TypeU], TypeU, Bool)])
     )
findTypedefs (ExprI _ (TypE Nothing v vs t)) = (Map.singleton v [(vs, t, False)], Map.empty)
findTypedefs (ExprI _ (TypE (Just (lang, isTerminal)) v vs t)) = (Map.empty, Map.singleton lang (Map.singleton v [(vs, t, isTerminal)]))
findTypedefs (ExprI _ (ModE _ es)) = foldl combine (Map.empty, Map.empty) (map findTypedefs es) where
  combine (g1, c1) (g2, c2)
    = ( Map.unionWith (<>) g1 g2
      , Map.unionWith (Map.unionWith (<>)) c1 c2
      )
findTypedefs _ = (Map.empty, Map.empty)

findSignatureTypeTerms :: ExprI -> [TVar]
findSignatureTypeTerms = unique . f where
  f :: ExprI -> [TVar]
  f (ExprI _ (ModE _ es)) = concatMap f es
  f (ExprI _ (SigE (Signature _ _ (EType t _ _ _ _)))) = findTypeTerms t
  f (ExprI _ (AssE _ _ es)) = concatMap f es
  f _ = []

-- | find all the non-generic terms in an unresolved type
findTypeTerms :: TypeU -> [TVar]
findTypeTerms (VarU v@(TV x))
  | isGeneric x = [ ]
  | otherwise   = [v]
findTypeTerms (ExistU _ ps1 rs2) = concatMap findTypeTerms (ps1 ++ map snd rs2)
findTypeTerms (ForallU _ e) = findTypeTerms e
findTypeTerms (FunU ts t) = concatMap findTypeTerms ts <> findTypeTerms t
findTypeTerms (AppU t ts) = findTypeTerms t <> concatMap findTypeTerms ts
findTypeTerms (NamU _ _ ps rs) = concatMap findTypeTerms (map snd rs <> ps)

-- | Find type signatures that are in the scope of the input expression. Do not
-- descend recursively into declaration where statements except if the input
-- expression is a declaration.
findSignatures :: ExprI -> [(EVar, Maybe Label, EType)]
-- v is the name of the type
-- l is the optional label for the signature
-- t is the type
findSignatures (ExprI _ (ModE _ es)) = [(v, l, t) | (ExprI _ (SigE (Signature v l t))) <- es]
findSignatures (ExprI _ (AssE _ _ es)) = [(v, l, t) | (ExprI _ (SigE (Signature v l t))) <- es]
findSignatures (ExprI _ (SigE (Signature v l t))) = [(v, l, t)]
findSignatures _ = []

checkExprI :: Monad m => (ExprI -> m ()) -> ExprI -> m ()
checkExprI f e@(ExprI _ (ModE _ es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (AccE _ e')) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AnnE e' _)) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AssE _ e' es')) = f e >> checkExprI f e' >> mapM_ f es'
checkExprI f e@(ExprI _ (IstE _ _ es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (LamE _ e')) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AppE e' es)) = f e >> checkExprI f e' >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (LstE es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (TupE es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (NamE rs)) = f e >> mapM_ (checkExprI f . snd) rs
checkExprI f e = f e

maxIndex :: ExprI -> Int
maxIndex (ExprI i (ModE _ es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (AccE _ e)) = max i (maxIndex e)
maxIndex (ExprI i (AnnE e _)) = max i (maxIndex e)
maxIndex (ExprI i (IstE _ _ es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (AssE _ e es)) = maximum (i : map maxIndex (e:es))
maxIndex (ExprI i (LamE _ e)) = max i (maxIndex e)
maxIndex (ExprI i (AppE e es)) = maximum (i : map maxIndex (e:es))
maxIndex (ExprI i (LstE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (TupE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (NamE rs)) = maximum (i : map (maxIndex . snd) rs)
maxIndex (ExprI i (ExpE ExportAll)) = i
maxIndex (ExprI i (ExpE (ExportMany ss))) = maximum (i : (map fst (Set.toList ss)))
maxIndex (ExprI i _) = i

getIndices :: ExprI -> [Int]
getIndices (ExprI i (ModE _ es)) = i : concatMap getIndices es
getIndices (ExprI i (AccE _ e)) = i : getIndices e
getIndices (ExprI i (AnnE e _)) = i : getIndices e
getIndices (ExprI i (AssE _ e es)) = i : concatMap getIndices (e:es)
getIndices (ExprI i (IstE _ _ es)) = i : concatMap getIndices es
getIndices (ExprI i (LamE _ e)) = i : getIndices e
getIndices (ExprI i (AppE e es)) = i : concatMap getIndices (e:es)
getIndices (ExprI i (LstE es)) = i : concatMap getIndices es
getIndices (ExprI i (TupE es)) = i : concatMap getIndices es
getIndices (ExprI i (NamE rs)) = i : concatMap (getIndices . snd) rs
getIndices (ExprI i (ExpE ExportAll)) = [i]
getIndices (ExprI i (ExpE (ExportMany ss))) = i : [j | (j, _) <- Set.toList ss]
getIndices (ExprI i _) = [i]
