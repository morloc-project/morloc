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
import Morloc.Pretty ()
import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Frontend.PartialOrder as PO

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

     -- build typeclasses and instance map
     _ <- DAG.synthesizeDAG linkTypeclasses d'

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

         -- set counter for reindexing expressions in collect.
         -- since d is the entire tree, the initizalized counter will start at global maximum.
         MM.setCounter $ maximum (map AST.maxIndex (DAG.nodes d)) + 1

         -- find all term exports (do not include type exports)
         let exports = [(i, v) | (i, TermSymbol v) <- AST.findExports e]

         -- - store all exported indices in state
         -- - Add the export name to state. Failing to do so here, will lose
         --   the name of terms that are exported but not defined, this leads
         --   to cryptic error messages.
         MM.modify (\s -> s { stateExports = map fst exports
                            , stateName = Map.union (stateName s) (Map.fromList exports)})

         -- dissolve modules, imports, and sources, leaving behind only a tree for each term exported from main
         mapM (collect . fst) exports

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
  link m (ExprI i (ExpE (TermSymbol v))) = setMonomorphicType m i v
  -- 2. variables
  link m (ExprI i (VarE v)) = setMonomorphicType m i v
  -- 3. assignments
  link m (ExprI i (AssE v (ExprI _ (LamE ks e)) es)) = do
    setMonomorphicType m i v
    -- shadow all terms bound under the lambda
    let m' = foldr Map.delete m ks
    -- then link the assignment term and all local "where" statements (es)
    linkSignatures mv (e:es) (Map.map snd m')
    return ()
  -- 4. assignments that have no parameters
  link m (ExprI i (AssE v e es)) = do
    setMonomorphicType m i v
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

  setMonomorphicType :: Map.Map EVar (Int, TermTypes) -> Int -> EVar -> MorlocMonad ()
  setMonomorphicType m i v = case Map.lookup v m of
    (Just (j, t)) -> do
      s <- CMS.get
      CMS.put (s { stateSignatures = GMap.insert i j (Monomorphic t) (stateSignatures s)
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
mergeTypeUs t1 t2
  | PO.equivalent t1 t2 = return t1
  | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2


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
  t0 <- MM.metaMonomorphicTermTypes i
  case t0 of
    -- if Nothing, then the term is a bound variable
    Nothing -> return (SAnno (Many []) i)
    -- otherwise is an alias that should be replaced with its value(s)
    (Just t1) -> do
      let calls = [(CallS src, i') | (_, Idx i' src) <- termConcrete t1]
      declarations <- mapM (replaceExpr i) (termDecl t1) |>> concat
      return $ SAnno (Many (calls <> declarations)) i

collectSAnno :: ExprI -> MorlocMonad (SAnno Int Many Int)
collectSAnno e@(ExprI i (VarE v)) = do
  MM.sayVVV $ "collectSAnno VarE:" <+> pretty v
  maybeTermTypes <- MM.metaTermTypes i
  MM.sayVVV $ "maybeTermTypes:" <+> pretty maybeTermTypes

  es <- case maybeTermTypes of
    -- if Nothing, then the term is a bound variable
    Nothing -> return <$> collectSExpr e
    (Just []) -> error "No instances"
    -- otherwise is an alias that should be replaced with its value(s)
    (Just ts) -> do
      -- collect all the concrete calls across all instances
      let calls = [(CallS src, i') | (_, Idx i' src) <- concatMap termConcrete ts]
      -- collect all the morloc compositions with this name across all instances
      declarations <- mapM reindexExprI (concatMap termDecl ts) >>= mapM (replaceExpr i) |>> concat
      -- link this index to the name that is removed
      s <- CMS.get
      CMS.put (s { stateName = Map.insert i v (stateName s) })
      -- pool all the calls and compositions with this name
      return (calls <> declarations)
  case es of
    -- TODO: will this case every actually be reached?
    -- Should all attributes of i be mapped to j, as done with newIndex?
    -- Should the general type be the j instead?
    -- Need to dig into this.
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
  tiMay <- MM.metaMonomorphicTermTypes i
  tjMay <- MM.metaMonomorphicTermTypes j
  t <- case (tiMay, tjMay) of
    (Just ti, Just tj) -> combineTermTypes ti tj
    (Just ti, _) -> return ti
    (_, Just tj) -> return tj
    _ -> error "You shouldn't have done that"

  st <- MM.get

  case GMap.change i (Monomorphic t) (stateSignatures st) of
    (Just m) -> MM.modify (\s -> s {stateSignatures = m})
    _ -> error "impossible"

  case GMap.yIsX j i (stateSignatures st) of
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
  f (AssE v _ _) = error $ "Found AssE in collectSExpr: " <> show v 

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


linkTypeclasses
  :: MVar
  -> ExprI
  -> [(m, e, Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))]
  -> MorlocMonad (Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))
linkTypeclasses _ e es
  -- Merge the typeclasses and instances from all imported modules
  -- These are inherited implicitly, so import terms are ignored
  = Map.unionsWithM mergeTypeclasses [x | (_,_,x) <- es]
  -- Augment the inherited map with the typeclasses and instances in this module
  >>= findTypeclasses e

findTypeclasses
  :: ExprI
  -> Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
  -> MorlocMonad (Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))
findTypeclasses (ExprI _ (ModE moduleName es0)) priorClasses = do

  -- first we collect all typeclass definitions in this module
  -- typeclasses are defined only at the top-level, so no descent into sub-expressions
  localClasses <- Map.unionsWithM mergeTypeclasses
                . map makeClass
                $ [(cls, vs, sigs) | (ExprI _ (ClsE cls vs sigs)) <- es0]

  -- then merge them with all prior typeclasses and instances
  allClasses <- Map.unionWithM mergeTypeclasses priorClasses localClasses

  -- find instances in this module
  -- The (IstE cls ts es) terms refer to
  --   cls: typeclass, such as "Packable"
  --   ts: types, such as ["Map a b", "[(a,b)]"]
  --   es: instance definitions, such as source statements (the only ones
  --       allowed at the moment)
  let instances = [(cls, ts, es) | (ExprI _ (IstE cls ts es)) <- es0]

  -- fold the instances into the current typeclass map and return
  moduleClasses <- foldlM addInstance allClasses instances

  MM.sayVVV $ "moduleClasses:"
         <+> list (
           map ( \ (v, (cls,vs,et,ts))
                 -> pretty v <+> "="
                 <+> pretty cls
                 <+> pretty vs
                 <+> parens (pretty (etype et))
                 <+> list (map pretty ts)
              ) (Map.toList moduleClasses)
         )

  mapM_ (linkVariablesToTypeclasses moduleClasses) es0

  return moduleClasses

  where
    -- make a map of all terms that are defined in a typeclass (these will all
    -- be general term)
    makeClass :: (Typeclass, [TVar], [Signature]) -> Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
    makeClass (cls, vs, sigs) = Map.fromList $ map makeClassTerm sigs where
      makeClassTerm :: Signature -> (EVar, (Typeclass, [TVar], EType, [TermTypes]))
      makeClassTerm (Signature v _ t) = (v, (cls, vs, t, []))

    addInstance
      :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
      -> (Typeclass, [TypeU], [ExprI])
      -> MorlocMonad (Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))
    addInstance clsmap (_, _, []) = return clsmap
    addInstance clsmap (cls0, ts0, es) = mapM f es |>> Map.fromListWith mergeInstances where
      f :: ExprI -> MorlocMonad (EVar, (Typeclass, [TVar], EType, [TermTypes]))
      f (ExprI srcIndex (SrcE src)) =
        case Map.lookup (srcAlias src) clsmap of
          (Just (cls1, vs, generalType, otherInstances)) -> do
            MM.sayVVV $ "Adding SrcE instance:" <+> pretty (srcAlias src) <+> pretty srcIndex
            when (cls1 /= cls0) (error "Conflicting instances")
            when (length vs /= length ts0) (error "Conflicting class and instance parameter count")
            let instanceType = generalType { etype = foldl (\t (v,r) -> substituteTVar v r t) (requalify vs (etype generalType)) (zip vs ts0) }
            let newTerm = TermTypes (Just instanceType) [(moduleName, Idx srcIndex src)] []
            let typeterms = mergeTermTypes newTerm otherInstances
            return (srcAlias src, (cls0, vs, generalType, typeterms))
          Nothing -> error "No typeclass found for instance"

      f (ExprI assIdx (AssE v e _)) =
        case Map.lookup v clsmap of
          (Just (cls1, vs, generalType, otherInstances)) -> do
            MM.sayVVV $ "Adding AssE instance:" <+> pretty v <+> pretty assIdx
            when (cls1 /= cls0) (error "Conflicting instances")
            when (length vs /= length ts0) (error "Conflicting class and instance parameter count")
            let instanceType = generalType { etype = foldl (\t (v',r) -> substituteTVar v' r t) (requalify vs (etype generalType)) (zip vs ts0) }
            let newTerm = TermTypes (Just instanceType) [] [e]
            let typeterms = mergeTermTypes newTerm otherInstances
            return (v, (cls0, vs, generalType, typeterms))
          Nothing -> error "No typeclass found for instance"

      f _ = error "Only source statements are currently allowed in instances (generalization is in development)"

      mergeInstances
        :: (Typeclass, [TVar], EType, [TermTypes])
        -> (Typeclass, [TVar], EType, [TermTypes])
        -> (Typeclass, [TVar], EType, [TermTypes])
      mergeInstances (cls1, vs1, e1, ts1) (cls2, vs2, e2, ts2)
        | cls1 == cls2, length vs1 == length vs2, PO.equivalent (etype e1) (etype e2) = (cls1, vs1, e1, unionTermTypes ts1 ts2)
        | otherwise = error "failed to merge"

      requalify :: [TVar] -> TypeU -> TypeU
      requalify (v:vs) (ForallU v' t)
        | v == v' = requalify vs t
        | otherwise = ForallU v' (requalify vs t)
      requalify _ t = t

    linkVariablesToTypeclasses
      :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
      -> ExprI
      -> MorlocMonad ()
    linkVariablesToTypeclasses = link where
      link :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]) -> ExprI -> MorlocMonad ()
      -- The following may have terms from typeclasses
      -- 1. variables
      link m (ExprI i (VarE v)) = setClass m i v
      -- recurse into assignments, allow shadowing of typeclass functions (TODO: warn)
      link m (ExprI _ (AssE _ (ExprI _ (LamE ks e)) es)) = do
        -- shadow all terms bound under the lambda
        let m' = foldr Map.delete m ks
        mapM_ (link m') (e:es)
      link m (ExprI _ (AssE _ e es)) = mapM_ (link m) (e:es)
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

      setClass :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]) -> Int -> EVar -> MorlocMonad ()
      setClass m termIndex v = case Map.lookup v m of
        (Just (cls, _, t, ts)) -> do

          MM.sayVVV $ "setClass map:" <+> viaShow m

          mapM_ (mapSources cls v t) ts
          mapM_ (mapExpressions cls v t) ts

          s <- CMS.get
          -- Yes, both indices are termIndex. After typechecking, the
          -- polymorphic type will resolve to monomorphic. Each may resolve
          -- differently, so instances must not all point to the same signature.
          newMap <- GMap.insertWithM mergeSignatureSet termIndex termIndex (Polymorphic cls v t ts) (stateSignatures s)
          CMS.put (s { stateSignatures = newMap
                     , stateName = Map.insert termIndex v (stateName s)})
          return ()
        Nothing -> return ()

      mapSources :: Typeclass -> EVar -> EType -> TermTypes -> MorlocMonad ()
      mapSources cls v gt t = mapM_ (mapSource . snd) (termConcrete t) where
        mapSource :: Indexed Source -> MorlocMonad ()
        mapSource (Idx i src) = do
          let t' = TermTypes (termGeneral t) [(mv, srcidx) | (mv, srcidx) <- termConcrete t, val srcidx == src] []
          MM.sayVVV $ "mapSource" <+> pretty i <+> pretty src
                    <> "\n  termGeneral t:" <+> pretty (termGeneral t)
                    <> "\n  termGeneral t':" <+> pretty (termGeneral t')
                    <> "\n  length (termConcrete t):" <+> pretty (length (termConcrete t))
                    <> "\n  length (termConcrete t'):" <+> pretty (length (termConcrete t'))
          s <- CMS.get
          newMap <- GMap.insertWithM mergeSignatureSet i i (Polymorphic cls v gt [t']) (stateSignatures s)
          CMS.put (s { stateSignatures = newMap })
          return ()

      mapExpressions :: Typeclass -> EVar -> EType -> TermTypes -> MorlocMonad ()
      mapExpressions cls v gt t = mapM_ mapExpression (termDecl t) where
        mapExpression :: ExprI -> MorlocMonad ()
        mapExpression (ExprI i _) = do
          MM.sayVVV $ "mapExpression" <+> pretty i
          s <- CMS.get
          let t' = TermTypes (termGeneral t) [] [e | e@(ExprI i' _) <- termDecl t, i' == i]
          newMap <- GMap.insertWithM mergeSignatureSet i i (Polymorphic cls v gt [t']) (stateSignatures s)
          CMS.put (s { stateSignatures = newMap })
          return ()

      mergeSignatureSet :: SignatureSet -> SignatureSet -> MorlocMonad SignatureSet
      mergeSignatureSet (Polymorphic cls1 v1 t1 ts1) (Polymorphic cls2 v2 t2 ts2)
        | cls1 == cls2 && PO.equivalent (etype t1) (etype t2) && v1 == v2 = return $ Polymorphic cls1 v1 t1 (unionTermTypes ts1 ts2)
        | otherwise = error "Invalid SignatureSet merge"
      mergeSignatureSet (Monomorphic ts1) (Monomorphic ts2) = Monomorphic <$> combineTermTypes ts1 ts2
      mergeSignatureSet _ _ = undefined
findTypeclasses _ _ = undefined

unionTermTypes :: [TermTypes] -> [TermTypes] -> [TermTypes]
unionTermTypes ts1 ts2 = foldr mergeTermTypes ts2 ts1

mergeTermTypes :: TermTypes -> [TermTypes] -> [TermTypes]
mergeTermTypes t1@(TermTypes (Just gt1) srcs1 es1) (t2@(TermTypes (Just gt2) srcs2 es2):ts)
  | PO.equivalent (etype gt1) (etype gt2) = TermTypes (Just gt1) (unique (srcs1 <> srcs2)) (es1 <> es2) : ts
  | otherwise = t2 : mergeTermTypes t1 ts
mergeTermTypes (TermTypes Nothing srcs1 es1) ((TermTypes e2 srcs2 es2):ts2) =
  mergeTermTypes (TermTypes e2 (srcs1 <> srcs2) (es1 <> es2)) ts2
mergeTermTypes TermTypes{} (TermTypes{}:_) = error "what the why?"
mergeTermTypes t1 [] = [t1]



mergeTypeclasses
  :: (Typeclass, [TVar], EType, [TermTypes])
  -> (Typeclass, [TVar], EType, [TermTypes])
  -> MorlocMonad (Typeclass, [TVar], EType, [TermTypes])
mergeTypeclasses (cls1, vs1, t1, ts1) (cls2, vs2, t2, ts2)
  | cls1 /= cls2 = error "Conflicting typeclasses"
  | not (PO.equivalent (etype t1) (etype t2)) = error "Conflicting typeclass term general type"
  | length vs1 /= length vs2 = error "Conflicting typeclass parameter count"
  -- here I should do reciprocal subtyping
  | otherwise = return (cls1, vs1, t1, unionTermTypes ts1 ts2)
