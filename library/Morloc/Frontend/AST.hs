{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.AST
Description : Query and traversal functions over the indexed 'ExprI' AST
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides structural queries over the indexed expression tree ('ExprI'):
extracting module edges, exports, type definitions, signatures, sources,
fixity declarations, and index ranges. Used by 'Restructure', 'Link',
and 'Treeify' to inspect and manipulate the AST before code generation.
-}
module Morloc.Frontend.AST
  ( findEdges
  , findExport
  , findExportSet
  , findFixityMap
  , setExport
  , findSignatures
  , findTypedefs
  , findSignatureTypeTerms
  , checkExprI
  , findSources
  , maxIndex
  , getIndices
  ) where

import qualified Data.Set as Set
import qualified Morloc.Data.Map as Map
import Morloc.Data.Doc
import qualified Morloc.Monad as MM
import Morloc.Frontend.Namespace

-- | In the DAG, the two MVar are the two keys, Import is the edge data, Expr is the node data
findEdges :: ExprI -> (MVar, [(MVar, Import)], ExprI)
findEdges e@(ExprI _ (ModE n es)) = (n, [(importModuleName i, i) | (ExprI _ (ImpE i)) <- es], e)
findEdges _ = error "Expected a module"

-- | Collect all exported symbols into a flat set (ignoring indices).
findExportSet :: ExprI -> Set.Set Symbol
findExportSet e = case findExport e of
  (ExportMany ss gs) -> Set.map snd ss `Set.union` Set.unions [Set.map snd (exportGroupMembers g) | g <- gs]
  _ -> Set.empty

-- | Extract the 'Export' declaration from a module expression.
findExport :: ExprI -> Export
findExport e0 = case f e0 of
  (Just export) -> export
  Nothing -> ExportMany Set.empty []
  where
    f (ExprI _ (ExpE export)) = Just export
    f (ExprI i (ModE j (e : es))) = case f e of
      (Just export) -> Just export
      Nothing -> f (ExprI i (ModE j es))
    f _ = Nothing

-- | Replace the 'Export' declaration in a module expression.
setExport :: Export -> ExprI -> ExprI
setExport export = f
  where
    f (ExprI i (ExpE _)) = ExprI i (ExpE export)
    f (ExprI i (ModE m es)) = ExprI i (ModE m (map f es))
    f e = e

-- | Collect all 'Source' declarations from a module.
findSources :: ExprI -> [Source]
findSources (ExprI _ (SrcE ss)) = [ss]
findSources (ExprI _ (ModE _ es)) = concatMap findSources es
findSources _ = []

-- | Find all top-level type aliases in a module, split into general
-- (language-independent) and concrete (language-specific) maps.
findTypedefs ::
  ExprI ->
  ( Map.Map TVar [([Either TVar TypeU], TypeU, ArgDoc, Bool)]
  , Map.Map Lang (Map.Map TVar [([Either TVar TypeU], TypeU, ArgDoc, Bool)])
  )
findTypedefs (ExprI _ (TypE (ExprTypeE Nothing v vs t d))) = (Map.singleton v [(vs, t, d, False)], Map.empty)
findTypedefs (ExprI _ (TypE (ExprTypeE (Just (lang, isTerminal)) v vs t d))) = (Map.empty, Map.singleton lang (Map.singleton v [(vs, t, d, isTerminal)]))
findTypedefs (ExprI _ (ModE _ es)) = foldl combine (Map.empty, Map.empty) (map findTypedefs es)
  where
    combine (g1, c1) (g2, c2) =
      ( Map.unionWith (<>) g1 g2
      , Map.unionWith (Map.unionWith (<>)) c1 c2
      )
findTypedefs _ = (Map.empty, Map.empty)

-- | Collect all non-generic type names used in signatures.
findSignatureTypeTerms :: ExprI -> [TVar]
findSignatureTypeTerms = unique . f
  where
    f :: ExprI -> [TVar]
    f (ExprI _ (ModE _ es)) = concatMap f es
    f (ExprI _ (SigE (Signature _ _ (EType t _ _)))) = findTypeTerms t
    f (ExprI _ (AssE _ _ es)) = concatMap f es
    f _ = []

-- | find all the non-generic terms in an unresolved type
findTypeTerms :: TypeU -> [TVar]
findTypeTerms (VarU v@(TV x))
  | isGeneric x = []
  | otherwise = [v]
findTypeTerms (ExistU _ (ps1, _) (rs2, _)) = concatMap findTypeTerms (ps1 ++ map snd rs2)
findTypeTerms (ForallU _ e) = findTypeTerms e
findTypeTerms (FunU ts t) = concatMap findTypeTerms ts <> findTypeTerms t
findTypeTerms (AppU t ts) = findTypeTerms t <> concatMap findTypeTerms ts
findTypeTerms (NamU _ _ ps rs) = concatMap findTypeTerms (map snd rs <> ps)
findTypeTerms (ThunkU t) = findTypeTerms t

-- | Build the fixity map from top-level fixity declarations.
findFixityMap :: ExprI -> MorlocMonad (Map.Map EVar (Associativity, Int))
findFixityMap (ExprI _ (ModE _ es)) = do
  -- collect all fixity terms.
  -- these are allowed only at the top level, so no need for recursion.
  let allTerms = concat [ [(op, (ass, pre)) | op <- ops]
                        | (ExprI _ (FixE (Fixity ass pre ops))) <- es]

  foldlM tryAddTerm Map.empty allTerms
  where

  tryAddTerm :: Map.Map EVar (Associativity, Int) -> (EVar, (Associativity, Int)) -> MorlocMonad (Map.Map EVar (Associativity, Int))
  tryAddTerm m (k, v)
    | Map.member k m = MM.throwSystemError $ "Conflicting fixity definitions for" <+> pretty k
    | otherwise = return $ Map.insert k v m
findFixityMap _ = return Map.empty


{- | Find type signatures that are in the scope of the input expression. Do not
descend recursively into declaration where statements except if the input
expression is a declaration.
-}
findSignatures :: ExprI -> [(EVar, Maybe Label, EType)]
-- v is the name of the type
-- l is the optional label for the signature
-- t is the type
findSignatures (ExprI _ (ModE _ es)) = [(v, l, t) | (ExprI _ (SigE (Signature v l t))) <- es]
findSignatures (ExprI _ (AssE _ _ es)) = [(v, l, t) | (ExprI _ (SigE (Signature v l t))) <- es]
findSignatures (ExprI _ (SigE (Signature v l t))) = [(v, l, t)]
findSignatures _ = []

-- | Apply a monadic check function to every node in an 'ExprI' tree.
checkExprI :: (Monad m) => (ExprI -> m ()) -> ExprI -> m ()
checkExprI f e@(ExprI _ (ModE _ es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (AnnE e' _)) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AssE _ e' es')) = f e >> checkExprI f e' >> mapM_ f es'
checkExprI f e@(ExprI _ (IstE _ _ es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (LamE _ e')) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (AppE e' es)) = f e >> checkExprI f e' >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (LstE es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (TupE es)) = f e >> mapM_ (checkExprI f) es
checkExprI f e@(ExprI _ (NamE rs)) = f e >> mapM_ (checkExprI f . snd) rs
checkExprI f e@(ExprI _ (BopE e1 _ _ e2)) = f e >> mapM_ (checkExprI f) [e1, e2]
checkExprI f e@(ExprI _ (LetE bindings body)) = f e >> mapM_ (checkExprI f . snd) bindings >> checkExprI f body
checkExprI f e@(ExprI _ (SuspendE e')) = f e >> checkExprI f e'
checkExprI f e@(ExprI _ (ForceE e')) = f e >> checkExprI f e'
checkExprI f e = f e

-- | Find the largest index used in an 'ExprI' tree.
maxIndex :: ExprI -> Int
maxIndex (ExprI i (ModE _ es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (AnnE e _)) = max i (maxIndex e)
maxIndex (ExprI i (IstE _ _ es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (AssE _ e es)) = maximum (i : map maxIndex (e : es))
maxIndex (ExprI i (LamE _ e)) = max i (maxIndex e)
maxIndex (ExprI i (AppE e es)) = maximum (i : map maxIndex (e : es))
maxIndex (ExprI i (LstE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (TupE es)) = maximum (i : map maxIndex es)
maxIndex (ExprI i (NamE rs)) = maximum (i : map (maxIndex . snd) rs)
maxIndex (ExprI i (ExpE ExportAll)) = i
maxIndex (ExprI i (ExpE (ExportMany ss gs))) = maximum (i : map fst (Set.toList ss) ++ concatMap (map fst . Set.toList . exportGroupMembers) gs)
maxIndex (ExprI i (BopE e1 j _ e2)) = maximum [i, j, maxIndex e1, maxIndex e2]
maxIndex (ExprI i (LetE bindings body)) = maximum (i : maxIndex body : map (maxIndex . snd) bindings)
maxIndex (ExprI i (SuspendE e)) = max i (maxIndex e)
maxIndex (ExprI i (ForceE e)) = max i (maxIndex e)
maxIndex (ExprI i _) = i

-- | Collect all indices from an 'ExprI' tree.
getIndices :: ExprI -> [Int]
getIndices (ExprI i (ModE _ es)) = i : concatMap getIndices es
getIndices (ExprI i (AnnE e _)) = i : getIndices e
getIndices (ExprI i (AssE _ e es)) = i : concatMap getIndices (e : es)
getIndices (ExprI i (IstE _ _ es)) = i : concatMap getIndices es
getIndices (ExprI i (LamE _ e)) = i : getIndices e
getIndices (ExprI i (AppE e es)) = i : concatMap getIndices (e : es)
getIndices (ExprI i (LstE es)) = i : concatMap getIndices es
getIndices (ExprI i (TupE es)) = i : concatMap getIndices es
getIndices (ExprI i (NamE rs)) = i : concatMap (getIndices . snd) rs
getIndices (ExprI i (ExpE ExportAll)) = [i]
getIndices (ExprI i (ExpE (ExportMany ss gs))) = i : [j | (j, _) <- Set.toList ss] ++ concatMap (\g -> [j | (j, _) <- Set.toList (exportGroupMembers g)]) gs
getIndices (ExprI i (BopE e1 j _ e2)) = [i, j] <> getIndices e1 <> getIndices e2
getIndices (ExprI i (LetE bindings body)) = i : concatMap (getIndices . snd) bindings <> getIndices body
getIndices (ExprI i (SuspendE e)) = i : getIndices e
getIndices (ExprI i (ForceE e)) = i : getIndices e
getIndices (ExprI i _) = [i]
