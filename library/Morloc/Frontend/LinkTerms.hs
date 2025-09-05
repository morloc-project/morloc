{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.LinkTerms
Description : Transfer all term types and sources to state
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.LinkTerms (linkTerms) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.GMap as GMap
import Morloc.Frontend.Merge (mergeTermTypes, mergeEType, mergeSignatureSet)


{-
                ,--n-> Source <-m--n-> Concrete Signature
term --<i>--.--<
             \  `--m-> Declaration
              `------------------------n--> General Signature
-}

linkTerms :: DAG MVar [(EVar, EVar)] ExprI -> MorlocMonad ()
linkTerms d0 = do
    _ <- DAG.synthesizeDAG linkTermsFun d0
    return ()

-- in each scope (top of a module or after descending into a where statement)
--  1 collect all type signatures (Map EVar [EType])
--  2 find all equivalent appearences of a given term across modules (including across aliases)
linkTermsFun
  :: MVar
  -> ExprI
  -> [(MVar, [(EVar, EVar)], Map.Map EVar TermTypes)]
  -- ^ This is a list of
  -> MorlocMonad (Map.Map EVar TermTypes)
linkTermsFun m0 (ExprI _ (ModE v es)) edges
  -- a map from alias to all signatures associated with the alias
  = Map.mapM (foldlM mergeTermTypes (TermTypes Nothing [] []))
              (Map.unionsWith (<>) [unalias es' m' | (_, es', m') <- edges]) >>=
    linkSignatures v es
  where
  -- map from a srcname linking to an alias linking
  -- NOTE: This is NOT a one to one mapping. Many sources may map to one alias.
  --   import A (foo as f, bar as f, baz as g)
  --   [("f", [x, y]), ("g", [z])] -- where x,y, and z values of type `a`
  unalias :: [(EVar, EVar)] -> Map.Map EVar a -> Map.Map EVar [a]
  unalias es0 m =
    let aliases = Map.fromList . groupSort $ [(alias, srcname) | (srcname, alias) <- es0]
    in Map.map (mapMaybe (`Map.lookup` m)) aliases
linkTermsFun _ _ _ = MM.throwError . CallTheMonkeys $ "Expected a module at the top level"


linkSignatures
  :: MVar -- ^ the current module name
  -> [ExprI] -- ^ all expressions in the module
  -> Map.Map EVar TermTypes -- ^ the inherited termtypes form imported modules
  -> MorlocMonad (Map.Map EVar TermTypes)
linkSignatures v es m = do
  -- find all top-level terms in this module
  --   terms :: Map.Map EVar TermTypes
  -- there is a lot of checking happening here, we ensure that
  --   - concrete type signatures appear only where there is a source statement
  --   - there can be only one general type in a given scope for a given term
  terms <- unifyTermTypes v es m

  -- Map EVar (Int, TermTypes)
  iterms <- Map.mapM indexTerm terms

  -- link terms to types
  _ <- linkVariablesToTermTypes v iterms es

  return terms

indexTerm :: a -> MorlocMonad (Int, a)
indexTerm x = do
  i <- MM.getCounter
  return (i, x)


linkVariablesToTermTypes
  :: MVar -- ^ the current module
  -> Map.Map EVar (Int, TermTypes) -- ^ a map term terms to types, Int is the inner GMAp key
  -> [ExprI] -- ^ list of expressions in the module
  -> MorlocMonad ()
linkVariablesToTermTypes mv m0 = mapM_ (link m0) where
  link :: Map.Map EVar (Int, TermTypes) -> ExprI -> MorlocMonad ()
  -- The following have terms associated with them:
  -- 1. exported terms (but not exported types)
  link m (ExprI _ (ExpE export)) = linkExports m export
  -- 2. variables
  link m (ExprI i (VarE _ v)) = setMonomorphicType m i v
  -- 3. assignments
  link m (ExprI i (AssE v (ExprI _ (LamE ks e)) es)) = do
    setMonomorphicType m i v
    -- shadow all terms bound under the lambda
    let m' = foldr Map.delete m ks
    -- then link the assignment term and all local "where" statements (es)
    _ <- linkSignatures mv (e:es) (Map.map snd m')
    return ()
  -- 4. assignments that have no parameters
  link m (ExprI i (AssE v e es)) = do
    _ <- setMonomorphicType m i v
    -- then link the assignment term and all local "where" statements (es)
    _ <- linkSignatures mv (e:es) (Map.map snd m)
    return ()
  -- 5. instances
  link m (ExprI i (IstE cls ts es)) = do
    _ <- linkSignatures mv es (Map.map snd m)
    return ()
  -- modules currently cannot be nested (should this be allowed?)
  link _ (ExprI _ (ModE v _)) = MM.throwError $ NestedModule v
  -- everything below boilerplate
  link m (ExprI _ (AccE _ e)) = link m e
  link m (ExprI _ (LstE xs)) = mapM_ (link m) xs
  link m (ExprI _ (TupE xs)) = mapM_ (link m) xs
  link m (ExprI _ (LamE vs e)) = link (foldr Map.delete m vs) e
  link m (ExprI _ (AppE f es)) = link m f >> mapM_ (link m) es
  link m (ExprI _ (AnnE e _)) = link m e
  link m (ExprI _ (NamE rs)) = mapM_ (link m . snd) rs
  link _ _ = return ()

  linkExports :: Map.Map EVar (Int, TermTypes) -> Export -> MorlocMonad ()
  linkExports _ ExportAll = error "All exports should have been resolved"
  linkExports m (ExportMany ss) = mapM_ linkSymbol (Set.toList ss) where
    linkSymbol :: (Int, Symbol) -> MorlocMonad ()
    linkSymbol (_, TypeSymbol _) = return ()
    linkSymbol (i, TermSymbol v) = setMonomorphicType m i v

  setMonomorphicType :: Map.Map EVar (Int, TermTypes) -> Int -> EVar -> MorlocMonad ()
  setMonomorphicType m i v = case Map.lookup v m of
    (Just (j, t)) -> do
      s <- MM.get
      newSigs <- GMap.insertWithM mergeSignatureSet i j (Monomorphic t) (stateSignatures s)
      MM.put (s { stateSignatures = newSigs
                 , stateName = Map.insert i v (stateName s) } )
      return ()
    Nothing -> return ()

unifyTermTypes :: MVar -> [ExprI] -> Map.Map EVar TermTypes -> MorlocMonad (Map.Map EVar TermTypes)
unifyTermTypes mv xs m0
  = Map.mergeMapsM fb fc fbc sigs srcs
  >>= Map.mapKeysWithM mergeTermTypes (\(v,_,_) -> v)
  >>= Map.unionWithM mergeTermTypes m0
  >>= Map.unionWithM mergeTermTypes decs
  where
  sigs = Map.fromListWith (<>) [((v, l, Nothing), [t]) | (ExprI _ (SigE (Signature v l t))) <- xs]
  srcs = Map.fromListWith (<>) [((srcAlias s, srcLabel s, langOf s), [(s, i)]) | (ExprI i (SrcE s)) <- xs]
  decs = Map.map (TermTypes Nothing []) $ Map.fromListWith (<>) [(v, [e]) | (ExprI _ (AssE v e _)) <- xs]

  -- generate a TermType object from only signatures
  fb :: [EType] -> MorlocMonad TermTypes
  fb [] = MM.throwError . CallTheMonkeys $ "This case should not appear given the construction of the map"
  fb (e0:es) = do
    e' <- foldlM mergeEType e0 es
    return $ TermTypes (Just e') [] []

  -- Should we even allow concrete terms with no type signatures?
  -- Yes, their types may be inferrable by usage or (eventually) static analysis
  -- of the source code.
  fc :: [(Source, Int)] -> MorlocMonad TermTypes
  fc srcs' = return $ TermTypes Nothing [(mv, Idx i src) | (src, i) <- srcs'] []

  fbc :: [EType] -> [(Source, Int)] -> MorlocMonad TermTypes
  fbc sigs' srcs' = do
    gt <- case sigs' of
      [e] -> return (Just e)
      [] -> return Nothing
      _ -> MM.throwError . CallTheMonkeys $ "Expected a single general type - I don't know how to merge them"
    return $ TermTypes gt [(mv, Idx i src) | (src, i) <- srcs'] []
