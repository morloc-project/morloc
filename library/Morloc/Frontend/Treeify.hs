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
treeify = undefined
-- treeify d = do
--   h = DAG.synthesizeDAG collect

--  -- | Map.size d == 0 = return []
--  -- | otherwise = case MDD.roots d of
--  --   -- if no parentless element exists, then the graph must be empty or cyclic
--  --   [] -> MM.throwError CyclicDependency
--  --   [k] -> case MDD.lookupNode k d of
--  --     Nothing -> MM.throwError . DagMissingKey . render $ pretty k
--  --     (Just e) -> do
--  --       -- initialize state counter to 0, used to index manifolds
--  --       MM.startCounter
--  --       mapM (collect d e) (Set.toList (AST.findExportSet e))
--  --   -- There is no currently supported use case that exposes multiple roots in
--  --   -- one compilation process. The compiler executable takes a single morloc
--  --   -- file as input, therefore this MUST be the root. In the future compiling
--  --   -- multiple projects in parallel with potentially shared information and
--  --   -- constraints could be valuable.
--  --   _ -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"

-- -- each of these inherits imported context, includes global context, and tracks
-- -- local context (i.e., signatures and declarations in `where` blocks).
-- typemap :: Map.Map Int [UnresolvedType]
--
-- declmap :: Map.Map Int [TermOrigin]
-- packmap :: Map.Map (TVar, Int) [UnresolvedPacker]



{-


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
--
-- -}
--
-- -- | Build the call tree for a single nexus command. The result is ambiguous,
-- -- with 1 or more possible tree topologies, each with one or more possible for
-- -- each function.
-- collect
--   :: DAG MVar [(EVar, EVar)] (Map.Map EVar ([TermOrigin], [EType]))
--   -> ExprI
--   -> EVar
--   -> MorlocMonad (SAnno GU Many [Int], GMap.GMap Int Int [EType])
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

yIsX' :: (Ord a) => GMap.GMap a b c -> a -> a -> MorlocMonad (GMap.GMap a b c)
yIsX' m k1 k2 = case GMap.yIsX m k1 k2 of
  Nothing -> MM.throwError . CallTheMonkeys $ "Internal key error"
  (Just m') -> return m'

reindex :: GMap.GMap Int Int [EType] -> SAnno GU Many [Int] -> MorlocMonad (GMap.GMap Int Int [EType], SAnno GU Many [Int])
reindex m (SAnno (Many xs) g) = do
  i <- MM.getCounter
  m' <- yIsX' m (metaId g) i
  let g' = g {metaId = i}
  (m'', xs') <- statefulMapM reindexSExpr m' xs
  return (m'', SAnno (Many xs') g')

reindexSExpr :: GMap.GMap Int Int [EType] -> (SExpr GU Many [Int], [Int]) -> MorlocMonad (GMap.GMap Int Int [EType], (SExpr GU Many [Int], [Int]))
reindexSExpr m0 (s0, ts0) = do
  (m', ts') <- statefulMapM reindexOne m0 ts0 
  (m'', s') <- f m' s0
  return (m'', (s', ts'))
  where
  f :: GMap.GMap Int Int [EType] -> SExpr GU Many [Int] -> MorlocMonad (GMap.GMap Int Int [EType], SExpr GU Many [Int])
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

  reindexOne :: GMap.GMap Int Int [EType] -> Int -> MorlocMonad (GMap.GMap Int Int [EType], Int)
  reindexOne m i = do
    i' <- MM.getCounter
    m' <- yIsX' m i i'
    return (m', i')
