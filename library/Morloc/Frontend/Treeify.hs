{-# LANGUAGE OverloadedStrings #-}

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
import qualified Morloc.Frontend.PartialOrder as PO
import Morloc.Pretty ()
import qualified Control.Monad.State as CMS
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.GMap as GMap

-- | Every term must either be sourced or declared.
data TermOrigin = Declared ExprI | Sourced Source
  deriving(Show, Ord, Eq)

-- When I see a term, I need to look it up. To do so, I need to walk up through
-- scope until I find a source/declaration and all type annotations. This involves
-- walking up through where statements (shadowing is possible), up through lambdas
-- (where again shadowing is possible), to the module scope, and to the imported
-- scope (where shadowing is not allowed).
--
-- Generate integers for all positions in the tree, use these to map into a table that includes:
--  * manual type annotations or signatures
--  * inferred type annotations

-- all expressions are mapped to integers that serve as indices linking
-- expressions to their ultimate type annotations. The indices also match terms
-- to their signatures or (eventually) to locations in source code.
treeify
  :: DAG MVar [(EVar, EVar)] ExprI
  -> MorlocMonad [SAnno Int Many Int]
treeify d
 | Map.size d == 0 = return []
 | otherwise = case DAG.roots d of
   -- if no parentless element exists, then the graph must be empty or cyclic
   [] -> MM.throwError CyclicDependency
   -- else if exactly one module name key (k) is found
   [k] -> do
     -- find all expressions with annotations and link the expression index to its type
     -- in the typecheckers, these types will be trigger a switch to checking mode.
     d' <- DAG.mapNodeM linkAndRemoveAnnotations d

     -- start counter for type keys
     MM.startCounter

     -- build a map
     -- - fill stateSignatures map in the MorlocState reader monad
     --       stateSignatures :: GMap Int Int [EType]
     -- - this is a map from term (VarE) indices to signature sets
     -- - after this step, all signatures and type annotation expressions are redundant
     -- - the map won't be used until the type inference step in Typecheck.hs
     _ <- DAG.synthesizeDAG linkSignaturesModule d'

     {- example d' for
      -  x = 42
      -  x
        fromList [
          ( MV {unMVar = "Main"}
          , (ExprI 4 (ModE (MV {unMVar = "Main"})
              [ ExprI 5 (ExpE (EV "__main__"))
              , ExprI 6 (AssE (EV "__main__") (ExprI 3 (VarE (EV "x"))) [])
              , ExprI 2 (AssE (EV "x") (ExprI 1 (RealE 42.0)) [])
              ]
          ),[]))]
     -}

     case DAG.lookupNode k d' of
       -- if the key is not in the DAG, then something is dreadfully wrong codewise
       Nothing -> MM.throwError . DagMissingKey . render $ pretty k
       (Just e) -> do

         {- Following the prior example, here e is the root module
              ExprI 4 (ModE (MV {unMVar = "Main"})
                [ ExprI 5 (ExpE (EV "__main__"))
                , ExprI 6 (AssE (EV "__main__") (ExprI 3 (VarE (EV "x"))) [])
                , ExprI 2 (AssE (EV "x") (ExprI 1 (RealE 42.0)) [])
                ])
         -}

         -- set counter for reindexing expressions in collect
         MM.setCounter $ maximum (map AST.maxIndex (DAG.nodes d)) + 1

         let exports = [(i, EV (symbolName v)) | (i, v) <- AST.findExports e]

         -- - store all exported indices in state
         -- - Add the export name to state. Failing to do so here, will lose
         --   the name of terms that are exported but not defined, this leads
         --   to cryptic error messages.
         MM.modify (\s -> s { stateExports = map fst exports
                            , stateName = Map.union (stateName s) (Map.fromList exports)})

         -- dissolve modules, imports, and sources, leaving behind only a tree for each term exported from main
         mapM (collect . fst) exports




-- storeExports :: DAG MVar [(EVar, EVar)] ExprI -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
-- storeExports d = do
--     s <- MM.get

   -- There is no currently supported use case that exposes multiple roots in
   -- one compilation process. The compiler executable takes a single morloc
   -- file as input, therefore this MUST be the root. In the future compiling
   -- multiple projects in parallel with potentially shared information and
   -- constraints could be valuable.
   roots -> MM.throwError . CallTheMonkeys . render $ "How did you end up with so many roots?" <+> tupled (map pretty roots)


{-
                ,--n-> Source <-m--n-> Concrete Signature
term --<i>--.--<
             \  `--m-> Declaration
              `------------------------n--> General Signature
-}



-- in each scope (top of a module or after descending into a where statement) 
--  1 collect all type signatures (Map EVar [EType])
--  2 find all equivalent appearences of a given term across modules (including across aliases)
linkSignaturesModule
  :: MVar
  -> ExprI
  -> [(MVar, [(EVar, EVar)], Map.Map EVar TermTypes)]
  -- ^ This is a list of 
  -> MorlocMonad (Map.Map EVar TermTypes)
linkSignaturesModule _ (ExprI _ (ModE v es)) edges
  -- a map from alias to all signatures associated with the alias
  = Map.mapM (foldlM combineTermTypes (TermTypes Nothing [] []))
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
linkSignaturesModule _ _ _ = MM.throwError . CallTheMonkeys $ "Expected a module at the top level" 


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
  s <- CMS.get
  let i = stateCounter s
  CMS.put (s { stateCounter = i + 1 })
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
  link m (ExprI i (ExpE (TermSymbol v))) = setType m i (EV v)
  -- 2. variables
  link m (ExprI i (VarE v)) = setType m i v
  -- 3. assignments
  link m (ExprI i (AssE v (ExprI _ (LamE ks e)) es)) = do
    setType m i v
    -- shadow all terms bound under the lambda
    let m' = foldr Map.delete m ks
    -- then link the assignment term and all local "where" statements (es)
    linkSignatures mv (e:es) (Map.map snd m')
    return ()
  -- 4. assignments that have no parameters
  link m (ExprI i (AssE v e es)) = do
    setType m i v
    -- then link the assignment term and all local "where" statements (es)
    linkSignatures mv (e:es) (Map.map snd m)
    return ()
  -- modules currently cannot be nested (should this be allowed?)
  link _ (ExprI _ (ModE v _)) = MM.throwError $ NestedModule v
  -- everything below boilerplate
  link m (ExprI _ (AccE e _)) = link m e
  link m (ExprI _ (LstE xs)) = mapM_ (link m) xs
  link m (ExprI _ (TupE xs)) = mapM_ (link m) xs
  link m (ExprI _ (LamE vs e)) = link (foldr Map.delete m vs) e
  link m (ExprI _ (AppE f es)) = link m f >> mapM_ (link m) es
  link m (ExprI _ (AnnE e _)) = link m e
  link m (ExprI _ (NamE rs)) = mapM_ (link m . snd) rs
  link _ _ = return ()

  setType :: Map.Map EVar (Int, TermTypes) -> Int -> EVar -> MorlocMonad ()
  setType m i v = case Map.lookup v m of 
    (Just (j, t)) -> do
      s <- CMS.get
      CMS.put (s { stateSignatures = GMap.insert i j t (stateSignatures s)
                 , stateName = Map.insert i v (stateName s) } )
      return ()
    Nothing -> return ()

unifyTermTypes :: MVar -> [ExprI] -> Map.Map EVar TermTypes -> MorlocMonad (Map.Map EVar TermTypes)
unifyTermTypes mv xs m0
  = Map.mergeMapsM fb fc fbc sigs srcs
  >>= Map.mapKeysWithM combineTermTypes (\(v,_,_) -> v)
  >>= Map.unionWithM combineTermTypes m0
  >>= Map.unionWithM combineTermTypes decs
  where
  sigs = Map.fromListWith (<>) [((v, l, langOf t), [t]) | (ExprI _ (SigE v l t)) <- xs]
  srcs = Map.fromListWith (<>) [((srcAlias s, srcLabel s, langOf s), [(s, i)]) | (ExprI i (SrcE s)) <- xs]
  decs = Map.map (TermTypes Nothing []) $ Map.fromListWith (<>) [(v, [e]) | (ExprI _ (AssE v e _)) <- xs]

  fb :: [EType] -> MorlocMonad TermTypes
  fb [] = MM.throwError . CallTheMonkeys $ "This case should not appear given the construction of the map"
  fb (e0:es) = do
    e' <- foldlM mergeEType e0 es
    case langOf e' of
      (Just _) -> return $ TermTypes Nothing [(mv, [e'], Nothing)] []
      _ -> return $ TermTypes (Just e') [] []

  -- Should we even allow concrete terms with no type signatures?
  fc :: [(Source, Int)] -> MorlocMonad TermTypes
  fc srcs' = return $ TermTypes Nothing [(mv, [], Just (Idx i src)) | (src, i) <- srcs'] []

  fbc :: [EType] -> [(Source, Int)] -> MorlocMonad TermTypes
  fbc sigs' srcs' = do
    let csigs = [t | t <- sigs', isJust (langOf t)]
    let gsigs = [t | t <- sigs', isNothing (langOf t)]
    gt <- case gsigs of
      [e] -> return (Just e)
      [] -> return Nothing
      -- TODO: don't call the monkeys
      _ -> MM.throwError . CallTheMonkeys $ "Expected a single general type"
    return $ TermTypes gt [(mv, csigs, Just (Idx i src)) | (src, i) <- srcs'] []


combineTermTypes :: TermTypes -> TermTypes -> MorlocMonad TermTypes
combineTermTypes (TermTypes g1 cs1 es1) (TermTypes g2 cs2 es2)
  = TermTypes
  <$> maybeCombine mergeEType g1 g2
  <*> pure (unique (cs1 <> cs2))
  <*> pure (unique (es1 <> es2))
  where
  -- either combine terms or take the first on that is defined, or whatever
  maybeCombine :: Monad m => (a -> a -> m a) -> Maybe a -> Maybe a -> m (Maybe a)
  maybeCombine f (Just a) (Just b) = Just <$> f a b
  maybeCombine _ (Just a) _ = return $ Just a
  maybeCombine _ _ (Just b) = return $ Just b
  maybeCombine _ _ _ = return Nothing 

-- | This function defines how general types are merged. There are decisions
-- encoded in this function that should be vary carefully considered.
--  * Can properties simply be concatenated?
--  * What if constraints are contradictory?
mergeEType :: EType -> EType -> MorlocMonad EType
mergeEType (EType t1 ps1 cs1) (EType t2 ps2 cs2)
  = EType <$> mergeTypeUs t1 t2 <*> pure (ps1 <> ps2) <*> pure (cs1 <> cs2)


-- merge two general types
mergeTypeUs :: TypeU -> TypeU -> MorlocMonad TypeU
mergeTypeUs t1@(VarU v1) t2@(VarU v2)
  | v1 == v2 = return (VarU v1)
  | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2 
mergeTypeUs t1@(ExistU v@(TV l1 _) ps1 ds1) t2@(ExistU (TV l2 _) ps2 _)
  | l1 == l2 = ExistU v <$> zipWithM mergeTypeUs ps1 ps2 <*> pure ds1
  | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2 
mergeTypeUs ExistU {} t = return t
mergeTypeUs t ExistU {} = return t

-- Two universally qualified types may be merged if they are the same up to
-- named of bound variables, for example:
--  mergeTypeUs (forall a . a) (forall b . b) --> forall b . b
mergeTypeUs (ForallU v1 t1) (ForallU v2 t2)
  = ForallU v1 <$> mergeTypeUs (substituteTVar v2 (VarU v1) t2) t1
mergeTypeUs (FunU ts1 t1) (FunU ts2 t2) = FunU <$> zipWithM mergeTypeUs ts1 ts2 <*> mergeTypeUs t1 t2 
mergeTypeUs (AppU t1 ps1) (AppU t2 ps2) = AppU <$> mergeTypeUs t1 t2 <*> zipWithM mergeTypeUs ps1 ps2
mergeTypeUs t1@(NamU o1 n1 ps1 ks1) t2@(NamU o2 n2 ps2 ks2)
  | o1 == o2 && n1 == n2 && length ps1 == length ps2 = do
      ts1 <- zipWithM mergeTypeUs (map snd ks1) (map snd ks2)
      ps' <- zipWithM mergeTypeUs ps1 ps2 
      return $ NamU o1 n1 ps' (zip (map fst ks1) ts1)
  | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2
mergeTypeUs t1 t2 = MM.throwError $ IncompatibleGeneralType t1 t2
-- mergeTypeUs t1 t2
--   | t1 `PO.isSubtypeOf` t2 = return t2
--   | t2 `PO.isSubtypeOf` t1 = return t1
--   | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2


linkAndRemoveAnnotations :: ExprI -> MorlocMonad ExprI
linkAndRemoveAnnotations = f where
  f :: ExprI -> MorlocMonad ExprI
  f (ExprI _ (AnnE e@(ExprI i _) ts)) = do
    --     ^                ^-- this one is connected to the given types
    --     '-- this index disappears with the lost annotation node
    s <- CMS.get
    CMS.put $ s {stateAnnotations = Map.insert i ts (stateAnnotations s)}
    f e -- notice the topology change
  -- everything below is boilerplate (this is why I need recursion schemes)
  f (ExprI i (ModE v es)) = ExprI i <$> (ModE v <$> mapM f es)
  f (ExprI i (AssE v e es)) = ExprI i <$> (AssE v <$> f e <*> mapM f es)
  f (ExprI i (AccE e k)) = ExprI i <$> (AccE <$> f e <*> pure k)
  f (ExprI i (LstE es)) = ExprI i <$> (LstE <$> mapM f es)
  f (ExprI i (TupE es)) = ExprI i <$> (TupE <$> mapM f es)
  f (ExprI i (NamE rs)) = do
    es' <- mapM (f . snd) rs
    return . ExprI i $ NamE (zip (map fst rs) es')
  f (ExprI i (AppE e es)) = ExprI i <$> (AppE <$> f e <*> mapM f es)
  f (ExprI i (LamE vs e)) = ExprI i <$> (LamE vs <$> f e)
  f e@(ExprI _ _) = return e

-- | Build the call tree for a single nexus command. The result is ambiguous,
-- with 1 or more possible tree topologies, each with one or more possible
-- implementations for each function.
--
-- Recursion
--   [ ] handle recursion and mutual recursion
--       - to detect recursion, I need to remember every term that has been expanded, 
-- collect v declarations or sources
collect
  :: Int -- ^ the index for the export term
  -> MorlocMonad (SAnno Int Many Int)
collect i = do
  t0 <- MM.metaTermTypes i
  case t0 of
    -- if Nothing, then the term is a bound variable
    Nothing -> return (SAnno (Many []) i)
    -- otherwise is an alias that should be replaced with its value(s)
    (Just t1) -> do
      let calls = [(CallS src, i') | (_, _, Just (Idx i' src)) <- termConcrete t1]
      declarations <- mapM (replaceExpr i) (termDecl t1) |>> concat
      return $ SAnno (Many (calls <> declarations)) i

collectSAnno :: ExprI -> MorlocMonad (SAnno Int Many Int)
collectSAnno e@(ExprI i (VarE v)) = do
  t0 <- MM.metaTermTypes i
  es <- case t0 of
    -- if Nothing, then the term is a bound variable
    Nothing -> return <$> collectSExpr e
    -- otherwise is an alias that should be replaced with its value(s)
    (Just t1) -> do
      -- collect all the concrete calls with this name
      let calls = [(CallS src, i') | (_, _, Just (Idx i' src)) <- termConcrete t1]
      -- collect all the morloc compositions with this name
      declarations <- mapM reindexExprI (termDecl t1) >>= mapM (replaceExpr i) |>> concat
      -- link this index to the name that is removed
      s <- CMS.get
      CMS.put (s { stateName = Map.insert i v (stateName s) })
      -- pool all the calls and compositions with this name
      return (calls <> declarations)
  case es of
    [] -> do
      j <- MM.getCounter
      return $ SAnno (Many [(VarS v, j)]) i
    es' -> return $ SAnno (Many es') i

-- expression type annotations should have already been accounted for, so ignore
collectSAnno (ExprI _ (AnnE e _)) = collectSAnno e
collectSAnno e@(ExprI i _) = do
  e' <- collectSExpr e 
  return $ SAnno (Many [e']) i

-- | This function will handle terms that have been set to be equal
replaceExpr :: Int -> ExprI -> MorlocMonad [(SExpr Int Many Int, Int)]
-- this will be a nested variable
-- e.g.:
--   foo = bar
replaceExpr i e@(ExprI j (VarE _)) = do
  x <- collectSAnno e
  -- unify the data between the equated terms
  tiMay <- MM.metaTermTypes i
  tjMay <- MM.metaTermTypes j
  t <- case (tiMay, tjMay) of
    (Just ti, Just tj) -> combineTermTypes ti tj 
    (Just ti, _) -> return ti
    (_, Just tj) -> return tj
    _ -> error "You shouldn't have done that"

  st <- MM.get

  case GMap.change i t (stateSignatures st) of
    (Just m) -> MM.modify (\s -> s {stateSignatures = m})
    _ -> error "impossible"

  case GMap.yIsX (stateSignatures st) j i of
    (Just m) -> MM.put (st {stateSignatures = m})
    Nothing -> return ()

  -- pass on just the children
  case x of
    (SAnno (Many es) _) -> return es
-- -- two terms may also be equivalent when applied, for example:
-- --   foo x = bar x
-- -- this would be rewritten in the parse as `foo = \x -> bar x`
-- -- meaning foo and bar are equivalent after eta-reduction
-- replaceExpr i e@(ExprI _ (LamE vs (ExprI _ (AppE e2@(ExprI _ (VarE _)) xs))))
--     | map VarE vs == [v | (ExprI _ v) <- xs] = replaceExpr i e2
--     | otherwise = return <$> collectSExpr e
replaceExpr _ e = return <$> collectSExpr e

-- | Translate ExprI to SExpr tree
collectSExpr :: ExprI -> MorlocMonad (SExpr Int Many Int, Int)
collectSExpr (ExprI i e0) = (,) <$> f e0 <*> pure i
  where
  f (VarE v) = return (VarS v) -- this must be a bound variable
  f (AccE e x) = AccS <$> collectSAnno e <*> pure x
  f (LstE es) = LstS <$> mapM collectSAnno es
  f (TupE es) = TupS <$> mapM collectSAnno es
  f (NamE rs) = do
    xs <- mapM (collectSAnno . snd) rs
    return $ NamS (zip (map fst rs) xs)
  f (LamE v e) = LamS v <$> collectSAnno e
  f (AppE e es) = AppS <$> collectSAnno e <*> mapM collectSAnno es
  f UniE = return UniS
  f (RealE x) = return (RealS x)
  f (IntE x) = return (IntS x)
  f (LogE x) = return (LogS x)
  f (StrE x) = return (StrS x)

  -- none of the following cases should every occur
  f (AnnE _ _) = error "impossible"
  f (ModE _ _) = error "impossible"
  f TypE {} = error "impossible"
  f (ImpE _) = error "impossible"
  f (ExpE _) = error "impossible"
  f (SrcE _) = error "impossible"
  f SigE {} = error "impossible"
  f AssE {} = error "impossible"

reindexExprI :: ExprI -> MorlocMonad ExprI
reindexExprI (ExprI i e) = ExprI <$> newIndex i <*> reindexExpr e

reindexExpr :: Expr -> MorlocMonad Expr
reindexExpr (ModE m es) = ModE m <$> mapM reindexExprI es
reindexExpr (AccE e x) = AccE <$> reindexExprI e <*> pure x
reindexExpr (AnnE e ts) = AnnE <$> reindexExprI e <*> pure ts
reindexExpr (AppE e es) = AppE <$> reindexExprI e <*> mapM reindexExprI es
reindexExpr (AssE v e es) = AssE v <$> reindexExprI e <*> mapM reindexExprI es
reindexExpr (LamE vs e) = LamE vs <$> reindexExprI e
reindexExpr (LstE es) = LstE <$> mapM reindexExprI es
reindexExpr (NamE rs) = NamE <$> mapM (\(k, e) -> (,) k <$> reindexExprI e) rs
reindexExpr (TupE es) = TupE <$> mapM reindexExprI es
reindexExpr e = return e


-- FIXME: when I add linking to line numbers, I'll need to update that map
-- also. The trace should be recorded.
newIndex :: Int -> MorlocMonad Int
newIndex i = do
  i' <- MM.getCounter
  copyState i i'
  return i'
