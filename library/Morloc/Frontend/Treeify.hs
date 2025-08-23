{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Treeify
Description : Translate from the frontend DAG to the backend AnnoS AST forest
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Treeify (treeify) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.GMap as GMap
import Morloc.Frontend.Classify (linkTypeclasses)
import Morloc.Frontend.Merge (mergeTermTypes, mergeEType, mergeSignatureSet)


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
--
-- All expressions are mapped to integer indices linking expressions to their
-- ultimate type annotations. The indices also match terms to their signatures
-- and locations in source code.
treeify
  :: DAG MVar [(EVar, EVar)] ExprI
  -> MorlocMonad [AnnoS Int ManyPoly Int]
treeify d
 | Map.size d == 0 = return []
 | otherwise = case DAG.roots d of
   -- if no parentless element exists, then the graph must be empty or cyclic
   [] -> MM.throwError CyclicDependency
   -- else if exactly one module name key (k) is found
   [k] -> do

     -- find all expressions with annotations and link the expression index to its type
     -- in the typecheckers, these types will trigger a switch to checking mode.
     d' <- DAG.mapNodeM linkAndRemoveAnnotations d

     -- build a map
     -- - fill stateSignatures map in the MorlocState reader monad
     --       stateSignatures :: GMap Int Int [EType]
     -- - this is a map from term (VarE) indices to signature sets
     -- - after this step, all signatures and type annotation expressions are redundant
     -- - the map won't be used until the type inference step in Typecheck.hs
     _ <- DAG.synthesizeDAG linkSignaturesModule d'

     -- build typeclasses and instance map
     _ <- DAG.synthesizeDAG linkTypeclasses d'

     case DAG.lookupNode k d' of
       -- if the key is not in the DAG, then something is dreadfully wrong codewise
       Nothing -> MM.throwError . DagMissingKey . render $ pretty k
       (Just e) -> do

         -- find all term exports (do not include type exports)
         symbols <- case AST.findExport e of
            (ExportMany ss) -> return $ Set.toList ss
            ExportAll -> error "This should not be possible, all ExportAll cases should have been removed in Restructure.hs"
         let exports = [(i, v) | (i, TermSymbol v) <- symbols]

         -- - store all exported indices in state
         -- - Add the export name to state. Failing to do so here, will lose
         --   the name of terms that are exported but not defined, this leads
         --   to cryptic error messages.
         MM.modify (\s -> s { stateExports = map fst exports
                            , stateName = Map.union (stateName s) (Map.fromList exports)})

         -- dissolve modules, imports, and sources, leaving behind only a tree for each term exported from main
         mapM (uncurry collect) exports

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


linkAndRemoveAnnotations :: ExprI -> MorlocMonad ExprI
linkAndRemoveAnnotations = f where
  f :: ExprI -> MorlocMonad ExprI
  f (ExprI _ (AnnE e@(ExprI i _) ts)) = do
    --     ^                ^-- this one is connected to the given types
    --     '-- this index disappears with the lost annotation node
    s <- MM.get
    MM.put $ s {stateAnnotations = Map.insert i ts (stateAnnotations s)}
    f e -- notice the topology change
  -- everything below is boilerplate (this is why I need recursion schemes)
  f (ExprI i (ModE v es)) = ExprI i <$> (ModE v <$> mapM f es)
  f (ExprI i (AssE v e es)) = ExprI i <$> (AssE v <$> f e <*> mapM f es)
  f (ExprI i (AccE k e)) = ExprI i <$> (AccE k <$> f e)
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
  :: Int -- ^ the general index for the term
  -> EVar
  -> MorlocMonad (AnnoS Int ManyPoly Int)
collect gi v = do
  MM.sayVVV $ "collect"
            <> "\n  gi:" <+> pretty gi
            <> "\n  v:" <+> pretty v
  AnnoS gi gi <$> collectExprS (ExprI gi (VarE defaultValue v))


collectAnnoS :: ExprI -> MorlocMonad (AnnoS Int ManyPoly Int)
collectAnnoS e@(ExprI gi _) = AnnoS gi gi <$> collectExprS e


-- | Translate ExprI to ExprS tree
collectExprS :: ExprI -> MorlocMonad (ExprS Int ManyPoly Int)
collectExprS (ExprI gi e0) = f e0 where
  f (VarE _ v) = do
    MM.sayVVV $ "collectExprS VarE"
              <> "\n  gi:" <+> pretty gi
              <> "\n  v:" <+> pretty v
    sigs <- MM.gets stateSignatures
    case GMap.lookup gi sigs of

      -- A monomorphic term will have a type if it is linked to any source
      -- since sources require signatures. But if it associated only with a
      -- declaration, then it will have no type.
      (GMapJust (Monomorphic t)) -> do
        MM.sayVVV $ "  monomorphic:" <+> maybe "?" pretty (termGeneral t)
        es <- termtypesToAnnoS t
        return $ VarS v (MonomorphicExpr (termGeneral t) es)

      -- A polymorphic term should always have a type.
      (GMapJust (Polymorphic cls clsName t ts)) -> do
        MM.sayVVV $ "  polymorphic:" <+> list (map (maybe "?" pretty . termGeneral) ts)
        ess <- mapM termtypesToAnnoS ts
        let etypes = map (fromJust . termGeneral) ts
        return $ VarS v (PolymorphicExpr cls clsName t (zip etypes ess))

      -- Terms not associated with TermTypes objects must be lambda-bound
      _ -> do
        MM.sayVVV "bound term"
        return $ BndS v
    where
      termtypesToAnnoS :: TermTypes -> MorlocMonad [AnnoS Int ManyPoly Int]
      termtypesToAnnoS t = do
        let calls = [AnnoS gi ci (CallS src) | (_, Idx ci src) <- termConcrete t]
        declarations <- mapM (\ e@(ExprI ci _) -> reindexExprI e >>= collectExprS |>> AnnoS gi ci) (termDecl t)
        return (calls <> declarations)

  f (AccE k e) = AccS k <$> collectAnnoS e
  f (LstE es) = LstS <$> mapM collectAnnoS es
  f (TupE es) = TupS <$> mapM collectAnnoS es
  f (NamE rs) = do
    xs <- mapM (collectAnnoS . snd) rs
    return $ NamS (zip (map fst rs) xs)
  f (LamE v e) = LamS v <$> collectAnnoS e
  f (AppE e es) = AppS <$> collectAnnoS e <*> mapM collectAnnoS es
  f UniE = return UniS
  f (RealE x) = return (RealS x)
  f (IntE x) = return (IntS x)
  f (LogE x) = return (LogS x)
  f (StrE x) = return (StrS x)
-- none of the following cases should ever occur
  f ClsE{} = undefined
  f IstE{} = undefined
  f AnnE{} = undefined
  f ModE{} = undefined
  f TypE{} = undefined
  f ImpE{} = undefined
  f ExpE{} = undefined
  f SrcE{} = undefined
  f SigE{} = undefined
  f (AssE v _ _) = error $ "Found an unexpected ass in collectExprS: " <> show v

reindexExprI :: ExprI -> MorlocMonad ExprI
reindexExprI (ExprI i e) = ExprI <$> newIndex i <*> reindexExpr e

reindexExpr :: Expr -> MorlocMonad Expr
reindexExpr (ModE m es) = ModE m <$> mapM reindexExprI es
reindexExpr (AccE k e) = AccE k <$> reindexExprI e
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
  MM.sayVVV $ "Set indices " <> pretty i <> " = " <> pretty i'
  return i'
