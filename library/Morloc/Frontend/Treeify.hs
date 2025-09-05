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
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.GMap as GMap
import Morloc.Frontend.Classify (linkTypeclasses)
import Morloc.Frontend.LinkTerms (linkTerms)
import Morloc.Frontend.Merge (mergeTermTypes, mergeEType, mergeSignatureSet)


-- | Every term must either be sourced or declared.
data TermOrigin = Declared ExprI | Sourced Source
  deriving(Show, Ord, Eq)

-- Manage unique naming in each tree
data Namer = Namer
    { namerMap :: Map.Map EVar EVar
    , namerIndex :: Int
    }

-- When I see a term, I need to look it up. To do so, I need to walk up through
-- scope and fine all sources/declarations and type annotations. This involves
-- walking up through where statements (shadowing is possible), up through lambdas
-- (where again shadowing is possible), to the module scope, and to the imported
-- scope (where terms are merged, not shadowed).
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
     _ <- linkTerms d'

     -- build typeclasses and instance map
     _ <- linkTypeclasses d'

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
-- Rewrite all lambda-bound variables to the unique names
-- "<name>@<index>". Where "<name>" is the original name and "<index>" is an
-- auto-incrementing integer. This solves naming conflicts while avoiding
-- excessive traversal of the tree.
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
  -- FIXME: this macro expansion strategy needs to be replaced
  MM.sayVVV $ "collect"
            <> "\n  gi:" <+> pretty gi
            <> "\n  v:" <+> pretty v
  (_, e) <- collectExprS (Namer Map.empty 0) (ExprI gi (VarE defaultValue v))
  return $ AnnoS gi gi e


collectAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
collectAnnoS namer e@(ExprI gi _) = collectExprS namer e |>> second (AnnoS gi gi)


-- | Translate ExprI to ExprS tree
collectExprS :: Namer -> ExprI -> MorlocMonad (Namer, ExprS Int ManyPoly Int)
collectExprS namer (ExprI gi e0) = f namer e0 where
  f namer (VarE _ v) = do
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
        (namer', es) <- termtypesToAnnoS gi namer t
        return $ (namer', VarS v (MonomorphicExpr (termGeneral t) es))

      -- A polymorphic term should always have a type.
      (GMapJust (Polymorphic cls clsName t ts)) -> do
        MM.sayVVV $ "  polymorphic:" <+> list (map (maybe "?" pretty . termGeneral) ts)
        (namer', ess) <- statefulMapM (termtypesToAnnoS gi) namer ts
        let etypes = map (fromJust . termGeneral) ts
        return $ (namer', VarS v (PolymorphicExpr cls clsName t (zip etypes ess)))

      -- Terms not associated with TermTypes objects must be lambda-bound
      -- These terms will be renamed for uniqueness
      _ -> do
        MM.sayVVV "bound term"
        case Map.lookup v (namerMap namer) of
            (Just v') -> return (namer, BndS v')
            Nothing -> MM.throwError $ UndefinedVariable v
    where
      termtypesToAnnoS :: Int -> Namer -> TermTypes -> MorlocMonad (Namer, [AnnoS Int ManyPoly Int])
      termtypesToAnnoS gi namer t = do
        let calls = [AnnoS gi ci (CallS src) | (_, Idx ci src) <- termConcrete t]

        (namer', declarations) <- statefulMapM termExprToAnnoS namer (termDecl t)
        return (namer', (calls <> declarations))

      termExprToAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
      termExprToAnnoS namer e@(ExprI ci _) = do
        (namer', e') <- reindexExprI e >>= collectExprS namer
        return $ (namer', AnnoS gi ci e')

  f namer (AccE k e) = collectAnnoS namer e |>> second (AccS k)
  f namer (LstE es) = statefulMapM collectAnnoS namer es |>> second LstS
  f namer (TupE es) = statefulMapM collectAnnoS namer es |>> second TupS
  f namer (NamE rs) = do
    (namer', vals) <- statefulMapM collectAnnoS namer (map snd rs)
    let keys = map fst rs
    return (namer', NamS (zip keys vals))
  f namer (LamE vs e) = do
    let namer' = foldr updateRenamer namer vs
        vs' = map (fromJust . (flip Map.lookup) (namerMap namer')) vs
    (namer'', e') <- collectAnnoS namer' e
    return (namer'', LamS vs' e')
    where
        updateRenamer :: EVar -> Namer -> Namer
        updateRenamer v (Namer rmap ridx) =
            let v' = EV (unEVar v <> "@" <> MT.show' ridx)
            in Namer { namerMap = Map.insert v v' rmap
                     , namerIndex = ridx + 1
                     }

  f namer (AppE e es) = do
    (namer', e') <- collectAnnoS namer e
    (namer'', es') <- statefulMapM collectAnnoS namer es
    return (namer'', AppS e' es')
  f namer UniE = return (namer, UniS)
  f namer (RealE x) = return (namer, RealS x)
  f namer (IntE x) = return (namer, IntS x)
  f namer (LogE x) = return (namer, LogS x)
  f namer (StrE x) = return (namer, StrS x)
  -- all other expressions are strictly illegal here and represent compiler bugs
  f _ _ = error $ "Bug in collectExprS "

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
