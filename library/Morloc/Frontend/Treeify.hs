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
import qualified Control.Monad.State as CMS
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.GMap as GMap

-- | Every term must either be sourced or declared.
data TermOrigin = Declared ExprI | Sourced Source
  deriving(Show, Ord, Eq)

-- all expressions are mapped to integers that serve as indices linking
-- expressions to their ultimate type annotations. The indices also match terms
-- to their signatures or (eventually) to locations in source code.
treeify
  :: DAG MVar [(EVar, EVar)] ExprI
  -> MorlocMonad [SAnno GU Many [Int]]
treeify d
 | Map.size d == 0 = return []
 | otherwise = case MDD.roots d of
   -- if no parentless element exists, then the graph must be empty or cyclic
   [] -> MM.throwError CyclicDependency
   -- else if exactly one module name key (k) is found
   [k] -> case MDD.lookupNode k d of
     -- if the key is not in the DAG, then something is dreadfully wrong codewise
     Nothing -> MM.throwError . DagMissingKey . render $ pretty k
     (Just e) -> do
       -- start counter for type keys
       MM.startCounter

       -- build a map
       -- - fill stateSignatures map in the MorlocState reader monad
       --       stateSignatures :: GMap Int Int [EType]
       -- - this is a map from term (VarE) indices to signature sets
       -- - after this step, all signatures and type annotation expressions are redundant
       -- - the map won't be used until the type inference step in Infer.hs
       _ <- MDD.synthesizeDAG linkSignaturesModule d

       -- dissolve modules, imports, and sources, leaving behind only a tree for each term exported from main
       mapM (collect d e) (Set.toList (AST.findExportSet e))

   -- There is no currently supported use case that exposes multiple roots in
   -- one compilation process. The compiler executable takes a single morloc
   -- file as input, therefore this MUST be the root. In the future compiling
   -- multiple projects in parallel with potentially shared information and
   -- constraints could be valuable.
   _ -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"


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
  = mapMapM (foldlM combineTermTypes (TermTypes Nothing []))
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
    in Map.map (mapMaybe (flip Map.lookup $ m)) aliases
linkSignaturesModule _ _ _ = MM.throwError . CallTheMonkeys $ "Expected a module at the top level" 

linkSignatures :: MVar -> [ExprI] -> Map.Map EVar TermTypes -> MorlocMonad (Map.Map EVar TermTypes)
linkSignatures v es m = do
  -- find all top-level terms in this module
  --   terms :: Map.Map EVar TermTypes
  -- there is a lot of checking happening here, we ensure that
  --   - concrete type signatures appear only where there is a source statement
  --   - there can be only one general type in a given scope for a given term
  terms <- unifyTermTypes v es m

  -- Map EVar (Int, TermTypes)
  iterms <- mapMapM indexTerm terms

  -- link terms to types
  _ <- linkVariablesToTermTypes v iterms es

  return terms

indexTerm :: a -> MorlocMonad (Int, a)
indexTerm x = do
  s <- CMS.get
  let i = stateCounter s
  CMS.put (s { stateCounter = i + 1 })
  return (i, x)

linkVariablesToTermTypes :: MVar -> Map.Map EVar (Int, TermTypes) -> [ExprI] -> MorlocMonad ()
linkVariablesToTermTypes mod m = mapM_ (link m) where 
  link :: Map.Map EVar (Int, TermTypes) -> ExprI -> MorlocMonad ()
  link m (ExprI _ (ModE v es)) = undefined -- TODO: how should nested modules behave?
  -- shadow all bound terms
  link m (ExprI i (Declaration v (ExprI _ (LamE k e)) es)) = link (Map.delete k m) (ExprI i (Declaration v e es))
  link m (ExprI _ (Declaration _ e es)) = linkSignatures mod (e:es) (Map.map snd m) >> return ()
  link m (ExprI i (VarE v)) = case Map.lookup v m of 
    (Just (j, t)) -> do
      s <- CMS.get
      CMS.put (s {stateSignatures = GMap.insert i j t (stateSignatures s)})
      return ()
    Nothing -> return ()
  link m (ExprI _ (AccE e _)) = link m e
  link m (ExprI _ (ListE xs)) = mapM_ (link m) xs
  link m (ExprI _ (TupleE xs)) = mapM_ (link m) xs
  link m (ExprI _ (LamE v e)) = link (Map.delete v m) e
  link m (ExprI _ (AppE f e)) = link m f >> link m e
  link m (ExprI _ (AnnE e _)) = link m e
  link m (ExprI _ (RecE rs)) = mapM_ (link m) (map snd rs)
  link _ _ = return ()

unifyTermTypes :: MVar -> [ExprI] -> Map.Map EVar TermTypes -> MorlocMonad (Map.Map EVar TermTypes)
unifyTermTypes mod xs m0
  = mergeMapsM fb fc fbc sigs srcs
  >>= mapKeysWithM combineTermTypes (\(v,_,_) -> v)
  >>= unionWithM combineTermTypes m0
  where
  sigs = Map.fromListWith (<>) [((v, l, langOf t), [t]) | (ExprI _ (Signature v l t)) <- xs] 
  srcs = Map.fromListWith (<>) [((srcAlias s, srcLabel s, langOf s), [s]) | s <- concat [ss | (ExprI _ (SrcE ss)) <- xs]]

  fb :: [EType] -> MorlocMonad TermTypes
  fb [] = MM.throwError . CallTheMonkeys $ "This case should not appear given the construction of the map"
  fb [e] = return $ TermTypes (Just e) []
  -- TODO: clean up the error messages to say exactly what went wrong (and
  -- don't call the monkeys, this is not an internal error).
  fb es = MM.throwError . CallTheMonkeys $ "Either you wrote a concrete type signature with no associated source function or you wrote multiple general type signatures for a single term in a single scope - either way, you can't do that."

  -- Should we even allow concrete terms with no type signatures?
  fc :: [Source] -> MorlocMonad TermTypes
  fc srcs = return $ TermTypes Nothing [(mod, src, []) | src <- srcs]

  fbc :: [EType] -> [Source] -> MorlocMonad TermTypes
  fbc sigs srcs = do
    let gsigs = [t | t <- sigs, isJust (langOf t)]
    let csigs = [t | t <- sigs, isNothing (langOf t)]
    gtype <- case gsigs of
      [e] -> return (Just e)
      [] -> return Nothing
      -- TODO: don't call the monkeys
      _ -> MM.throwError . CallTheMonkeys $ "Expected a single general type"
    return $ TermTypes gtype [(mod, src, csigs) | src <- srcs]


unionWithM :: (Monad m, Ord a) => (b -> b -> m b) -> Map.Map a b -> Map.Map a b -> m (Map.Map a b)
unionWithM f m1 m2 = do
  pairs <- mapM (onSndM (uncurry f)) (Map.toList $ Map.intersectionWith (,) m1 m2)

  return $ Map.unions
    [ Map.difference m1 m2
    , Map.fromList pairs
    , Map.difference m2 m1
    ]

unionsWithM :: (Monad m, Ord a) => (b -> b -> m b) -> [Map.Map a b] -> m (Map.Map a b)
unionsWithM f = foldlM (unionWithM f) Map.empty


mapKeysWithM
  :: (Monad m, Ord k1, Ord k2)
  => (a -> a -> m a)
  -> (k1 -> k2)
  -> Map.Map k1 a
  -> m (Map.Map k2 a)
mapKeysWithM f g m
  = fmap Map.fromList
  $ mapM (\(k, (v:vs)) -> (,) k <$> foldM f v vs)
         (groupSort $ map (\(k,x) -> (g k, x)) (Map.toList m))
   

mergeMaps
  :: Ord a
  => (b -> d)
  -> (c -> d)
  -> (b -> c -> d)
  -> Map.Map a b
  -> Map.Map a c
  -> Map.Map a d
mergeMaps fb fc fbc m1 m2 = Map.unions
  [ Map.map fb (Map.difference m1 m2)
  , Map.mapMaybeWithKey (\k v -> fbc v <$> Map.lookup k m2) (Map.intersection m1 m2)
  , Map.map fc (Map.difference m2 m1)
  ]


mergeMapsM
  :: (Ord a, Monad m)
  => (b -> m d)
  -> (c -> m d)
  -> (b -> c -> m d)
  -> Map.Map a b
  -> Map.Map a c
  -> m (Map.Map a d)
mergeMapsM fb fc fbc m1 m2 = do
  bs <- mapM (onSndM fb) . Map.toList $ Map.difference m1 m2
  bcs <- mapM (onSndM (uncurry fbc)) . Map.toList $ Map.intersectionWith (,) m1 m2
  cs <- mapM (onSndM fc) . Map.toList $ Map.difference m2 m1
  return $ Map.fromList (bs <> bcs <> cs)

onSndM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
onSndM f (x, y) = (,) x <$> f y


mapMapM :: (Ord k, Monad m) => (a -> m b) -> Map.Map k a -> m (Map.Map k b)
mapMapM f m = do
  let xs = Map.toList m
  xs' <- mapM (\(k,x) -> (,) k <$> f x) xs
  return $ Map.fromList xs'


combineTermTypes :: TermTypes -> TermTypes -> MorlocMonad TermTypes
combineTermTypes (TermTypes g1 cs1) (TermTypes g2 cs2)
  = TermTypes
  <$> (sequence $ mergeEType <$> g1 <*> g2)
  <*> pure (unique (cs1 <> cs2))


-- | This function defines who general types are merged. There are decisions
-- encoded in this function that should be vary carefully considered.
--  * Can properties simply be concatenated?
--  * What if constraints are contradictory?
mergeEType :: EType -> EType -> MorlocMonad EType
mergeEType (EType t1 ps1 cs1) (EType t2 ps2 cs2)
  = EType <$> mergeUnresolvedTypes t1 t2 <*> pure (ps1 <> ps2) <*> pure (cs1 <> cs2)


mergeUnresolvedTypes :: UnresolvedType -> UnresolvedType -> MorlocMonad UnresolvedType
mergeUnresolvedTypes t1@(VarU v1) t2@(VarU v2)
  | v1 == v2 = return (VarU v1)
  | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2 
mergeUnresolvedTypes t@(ExistU _ _ _) (ExistU _ _ _) = return t
mergeUnresolvedTypes (ExistU _ _ _) t = return t
mergeUnresolvedTypes t (ExistU _ _ _) = return t
mergeUnresolvedTypes (ForallU v1 t1) (ForallU v2 t2) = undefined
mergeUnresolvedTypes (FunU f1 x1) (FunU f2 x2) = undefined
mergeUnresolvedTypes (ArrU v1 ps1) (ArrU v2 ps2) = undefined
mergeUnresolvedTypes (NamU t1 v1 ps1 ks1) (NamU t2 v2 ps2 ks2) = undefined
mergeUnresolvedTypes t1 t2 = MM.throwError $ IncompatibleGeneralType t1 t2


-- -- -- each of these inherits imported context, includes global context, and tracks
-- -- -- local context (i.e., signatures and declarations in `where` blocks).
-- -- typemap :: Map.Map Int [UnresolvedType]
-- --
-- -- declmap :: Map.Map Int [TermOrigin]
-- -- packmap :: Map.Map (TVar, Int) [UnresolvedPacker]
--
--
-- -- Two steps,
-- --  1) move type annotations into MorlocState stateSignatures
-- --  2) move translate every module from ExprI into SAnno


-- When I see a term, I need to look it up. To do so, I need to walk up through
-- scope until I find a source/declaration and all type annotations. This involves
-- walking up through where statements (shadowing is possible), up through lambdas
-- (where again shadowing is possible), to the module scope, and to the imported
-- scope (where shadowing is not allowed).
--
--
-- Generate integers for all positions in the tree, use these to map into a table that includes:
--  * manual type annotations or signatures
--  * inferred type annotations
--  *

-- | Build the call tree for a single nexus command. The result is ambiguous,
-- with 1 or more possible tree topologies, each with one or more possible for
-- each function.
collect
  :: DAG MVar [(EVar, EVar)] ExprI
  -> ExprI
  -> EVar
  -> MorlocMonad (SAnno GU Many [Int])
collect = undefined

-- collect d (ModE m es) v = undefined
--   -- build module scope
--   --   * find all imports, call collect on each var
--   --   * find all declarations
--   --   * find all signatures
--   --   * handle recursion and mutual recursion
--   --   * support arbitrary order
--   -- collect v declarations or sources
--
-- -- | Find the user provided, or module imported, general type annotations and
-- -- collect info needed for the GMeta object
-- collectSAnno
--   :: Map.Map EVar ([TermOrigin], [EType])
--   -> ExprI
--   -> MorlocMonad (SAnno GU Many [Int])
-- collectSAnno d e = do
--   i <- MM.getCounter
--   xs <- collectSExpr d e
--   gmeta <- makeGMeta d e
--   return $ SAnno (Many xs) gmeta
--
-- -- | Find all definitions of a term and collect their type annotations, if available
-- collectSExpr
--   :: Map.Map EVar ([TermOrigin], [EType])
--   -> Expr
--   -> MorlocMonad [(SExpr Int Many [Int], [Int])]
-- collectSExpr d0 e0 = (,) <$> f d0 e0 <*> MM.getCounter where
--   f _ (TypE _ _ _) = return []
--   f _ (ImpE _) = return []
--   f _ (ExpE _) = return []
--   f _ (SrcE _) = return []
--   f _ (Signature _ _) = return []
--   f d (Declaration v e wheres) = undefined -- roll where statements into scope with shadowing
--   f _ (UniE) = return UniS
--   f d (VarE v) = undefined -- lookup v, this is the one expression that may return multiple values
--   f d (AccE e x) = AccS <$> collectSAnno d e <*> pure x
--   f d (ListE es) = ListS <$> mapM collectSAnno d es
--   f d (TupleE es) = TupleS <$> mapM collectSAnno d es
--   f d (RecE rs) = do
--     xs <- mapM (collectSAnno d) (map snd rs)
--     return $ RecS zip (map fst rs) xs
--   f d (LamE v e) = undefined -- replace `v` in scope with bound term
--   f d (AppE e1 e2) = AppS <$> collectSAnno d e1 <*> collectSAnno d e2
--   f d (AnnE e ts) = undefined -- add `ts` to the symbol table for `e`
--   f d (NumE x) = return (NumS x)
--   f d (LogE x) = return (LogS x)
--   f d (StrE x) = return (StrS x)

statefulMapM :: Monad m => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
statefulMapM _ s [] = return (s, [])
statefulMapM f s (x:xs) = do
  (s', x') <- f s x
  (s'', xs') <- statefulMapM f s' xs
  return (s'', x':xs')

yIsX' :: (Ord a) => GMap a b c -> a -> a -> MorlocMonad (GMap a b c)
yIsX' m k1 k2 = case GMap.yIsX m k1 k2 of
  Nothing -> MM.throwError . CallTheMonkeys $ "Internal key error"
  (Just m') -> return m'

reindex :: GMap Int Int [EType] -> SAnno GU Many [Int] -> MorlocMonad (GMap Int Int [EType], SAnno GU Many [Int])
reindex m (SAnno (Many xs) g) = do
  i <- MM.getCounter
  m' <- yIsX' m (metaId g) i
  let g' = g {metaId = i}
  (m'', xs') <- statefulMapM reindexSExpr m' xs
  return (m'', SAnno (Many xs') g')

reindexSExpr :: GMap Int Int [EType] -> (SExpr GU Many [Int], [Int]) -> MorlocMonad (GMap Int Int [EType], (SExpr GU Many [Int], [Int]))
reindexSExpr m0 (s0, ts0) = do
  (m', ts') <- statefulMapM reindexOne m0 ts0 
  (m'', s') <- f m' s0
  return (m'', (s', ts'))
  where
  f :: GMap Int Int [EType] -> SExpr GU Many [Int] -> MorlocMonad (GMap Int Int [EType], SExpr GU Many [Int])
  f m (AccS x t) = do
    (m', x') <- reindex m x
    return (m', AccS x' t)
  f m (ListS xs) = do
    (m', xs') <- statefulMapM reindex m xs
    return (m', ListS xs')
  f m (TupleS xs) = do
    (m', xs') <- statefulMapM reindex m xs
    return (m', TupleS xs')
  f m (LamS v x) = do
    (m', x') <- reindex m x
    return (m', LamS v x')
  f m (AppS x xs) = do
    (m', x') <- reindex m x
    (m'', xs') <- statefulMapM reindex m' xs
    return (m'', AppS x' xs')
  f m (RecS rs) = do
    (m', xs') <- statefulMapM reindex m (map snd rs)
    return (m', RecS (zip (map fst rs) xs'))
  f m x = return (m, x)

  reindexOne :: GMap Int Int [EType] -> Int -> MorlocMonad (GMap Int Int [EType], Int)
  reindexOne m i = do
    i' <- MM.getCounter
    m' <- yIsX' m i i'
    return (m', i')
