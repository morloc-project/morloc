{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

{- |
Module      : Morloc.Frontend.Treeify
Description : Translate from the frontend DAG to the backend AnnoS AST forest
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Treeify (treeify) where

import qualified Data.Set as Set
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.DAG as DAG
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Frontend.Link as MFL
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM

-- | Every term must either be sourced or declared.
data TermOrigin = Declared ExprI | Sourced Source
  deriving (Show, Ord, Eq)

data BindKind = LambdaBound | LetBound
  deriving (Show, Eq)

-- Manage unique naming in each tree
data Namer = Namer
  { namerMap :: Map.Map EVar (EVar, BindKind)
  , namerIndex :: Int
  }
  deriving (Show)

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
treeify ::
  DAG MVar [AliasedSymbol] ExprI ->
  MorlocMonad [AnnoS Int ManyPoly Int]
treeify d
  | Map.size d == 0 = return []
  | otherwise = case DAG.roots d of
      -- if no parentless element exists, then the graph must be empty or cyclic
      [] -> MM.throwSystemError "cyclic import dependency in treeify"
      -- else if exactly one module name key (k) is found
      [k] -> do
        case DAG.lookupNode k d of
          -- if the key is not in the DAG, then something is dreadfully wrong codewise
          Nothing -> MM.throwSystemError $ "Compiler bug (__FILE__:__LINE__): Module DAG is missing key" <+> pretty k
          (Just (AST.findExport -> ExportMany symbols)) -> do
            d' <- DAG.mapNodeM linkAndRemoveAnnotations d |>> nullify

            -- move all to state, after this the DAG will no longer be needed
            _ <- MFL.link d'

            -- find all term exports (do not include type exports)
            let exports = [(i, v) | (i, TermSymbol v) <- Set.toList symbols]

            -- - store all exported indices in state
            -- - Add the export name to state. Failing to do so here, will lose
            --   the name of terms that are exported but not defined, this leads
            --   to cryptic error messages.
            MM.modify
              ( \s ->
                  s
                    { stateExports = map fst exports
                    , stateName = Map.union (stateName s) (Map.fromList exports)
                    }
              )

            -- dissolve modules, imports, and sources, leaving behind only a tree for each term exported from main
            statefulMapM collect (Namer Map.empty 0) exports |>> snd
          (Just _) ->
            error "This should not be possible, all ExportAll cases should have been removed in Restructure.hs"

      -- There is no currently supported use case that exposes multiple roots in
      -- one compilation process. The compiler executable takes a single morloc
      -- file as input, therefore this MUST be the root. In the future compiling
      -- multiple projects in parallel with potentially shared information and
      -- constraints could be valuable.
      roots ->
        MM.throwSystemError $ "Compiler bug (__FILE__:__LINE__): unsupported multi-rooted module DAG:" <+> tupled (map pretty roots)

-- TODO: document
nullify :: DAG m e ExprI -> DAG m e ExprI
nullify = DAG.mapNode f
  where
    f :: ExprI -> ExprI
    f (ExprI i (SigE (Signature v n (EType t ps cs docs)))) = ExprI i (SigE (Signature v n (EType (nullifyT t) ps cs docs)))
    f (ExprI i (ModE m es)) = ExprI i (ModE m (map f es))
    f (ExprI i (AssE v e es)) = ExprI i (AssE v (f e) (map f es))
    f e = e

    nullifyT :: TypeU -> TypeU
    nullifyT (FunU ts t) = FunU (filter (not . isNull) (map nullifyT ts)) (nullifyT t)
    nullifyT (ExistU v (ts, tc) (rs, rc)) = ExistU v (map nullifyT ts, tc) (map (second nullifyT) rs, rc)
    nullifyT (ForallU v t) = ForallU v (nullifyT t)
    nullifyT (AppU t ts) = AppU (nullifyT t) (map nullifyT ts)
    nullifyT (NamU o v ds rs) = NamU o v (map nullifyT ds) (map (second nullifyT) rs)
    nullifyT t = t

    isNull :: TypeU -> Bool
    isNull t = t == BT.unitU

linkAndRemoveAnnotations :: ExprI -> MorlocMonad ExprI
linkAndRemoveAnnotations = f
  where
    f :: ExprI -> MorlocMonad ExprI
    f (ExprI _ (AnnE e@(ExprI i _) t)) = do
      --     ^                ^-- this one is connected to the given types
      --     '-- this index disappears with the lost annotation node
      s <- MM.get
      MM.put $ s {stateAnnotations = Map.insert i t (stateAnnotations s)}
      f e -- notice the topology change
      -- everything below is boilerplate (this is why I need recursion schemes)
    f (ExprI i (ModE v es)) = ExprI i <$> (ModE v <$> mapM f es)
    f (ExprI i (AssE v e es)) = ExprI i <$> (AssE v <$> f e <*> mapM f es)
    f (ExprI i (LstE es)) = ExprI i <$> (LstE <$> mapM f es)
    f (ExprI i (TupE es)) = ExprI i <$> (TupE <$> mapM f es)
    f (ExprI i (NamE rs)) = do
      es' <- mapM (f . snd) rs
      return . ExprI i $ NamE (zip (map fst rs) es')
    f (ExprI i (AppE e es)) = ExprI i <$> (AppE <$> f e <*> mapM f es)
    f (ExprI i (LamE vs e)) = ExprI i <$> (LamE vs <$> f e)
    f (ExprI i (LetE bindings body)) = ExprI i <$> (LetE <$> mapM (\(v, e) -> (,) v <$> f e) bindings <*> f body)
    f e@(ExprI _ _) = return e

{- | Build the call tree for a single nexus command. The result is ambiguous,
with 1 or more possible tree topologies, each with one or more possible
implementations for each function.

Rewrite all lambda-bound variables to the unique names
"<name>@<index>". Where "<name>" is the original name and "<index>" is an
auto-incrementing integer. This solves naming conflicts while avoiding
excessive traversal of the tree.

Recursion
  [ ] handle recursion and mutual recursion
      - to detect recursion, I need to remember every term that has been expanded,
collect v declarations or sources
-}
collect ::
  Namer ->
  ( Int -- the general index for the term
  , EVar -- name of root expression
  ) ->
  MorlocMonad (Namer, AnnoS Int ManyPoly Int)
collect namer0 (gi, v) = do
  -- FIXME: this macro expansion strategy needs to be replaced
  MM.sayVVV $
    "collect"
      <> "\n  gi:" <+> pretty gi
      <> "\n  v:" <+> pretty v
  (namer, e) <- collectExprS namer0 (ExprI gi (VarE defaultValue v))
  return (namer, AnnoS gi gi e)

collectAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
collectAnnoS namer e@(ExprI gi _) = collectExprS namer e |>> second (AnnoS gi gi)

-- | Translate ExprI to ExprS tree
collectExprS :: Namer -> ExprI -> MorlocMonad (Namer, ExprS Int ManyPoly Int)
collectExprS namer0 (ExprI gi0 e0) = f namer0 e0
  where
    f namer (VarE _ v) = do
      MM.sayVVV $
        "collectExprS VarE"
          <> "\n  gi:" <+> pretty gi0
          <> "\n  v:" <+> pretty v
      sigs <- MM.gets stateSignatures

      case GMap.lookup gi0 sigs of
        -- A monomorphic term will have a type if it is linked to any source
        -- since sources require signatures. But if it associated only with a
        -- declaration, then it will have no type.
        (GMapJust (Monomorphic t)) -> do
          MM.sayVVV $ "  searchged gi " <+> pretty gi0 <+> "for" <+> pretty v

          MM.sayVVV $ "  monomorphic term" <+> pretty v <> ":" <+> maybe "?" pretty (termGeneral t)
          (namer', es) <- termtypesToAnnoS gi0 namer t
          return $ (namer', VarS v (MonomorphicExpr (termGeneral t) es))

        -- A polymorphic term should always have a type.
        (GMapJust (Polymorphic cls clsName t ts)) -> do
          MM.sayVVV $
            "  polymorphic term" <+> pretty v <> ":" <+> list (map (maybe "?" pretty . termGeneral) ts)
          (namer', ess) <- statefulMapM (termtypesToAnnoS gi0) namer ts
          let etypes = map (fromJust . termGeneral) ts
          return $ (namer', VarS v (PolymorphicExpr cls clsName t (zip etypes ess)))

        -- Terms not associated with TermTypes objects must be lambda-bound or let-bound
        -- These terms will be renamed for uniqueness
        _ -> do
          MM.sayVVV $ "bound term" <+> pretty v
          case Map.lookup v (namerMap namer) of
            (Just (v', LambdaBound)) -> return (namer, BndS v')
            (Just (v', LetBound)) -> return (namer, LetBndS v')
            Nothing -> MM.throwSourcedError gi0 $ "Undefined term in namer map:" <+> pretty v
      where
        termtypesToAnnoS :: Int -> Namer -> TermTypes -> MorlocMonad (Namer, [AnnoS Int ManyPoly Int])
        termtypesToAnnoS gi n t = do
          let calls = [AnnoS gi ci (ExeS (SrcCall src)) | (_, Idx ci src) <- termConcrete t]

          (n', declarations) <- statefulMapM termExprToAnnoS n (termDecl t)
          return (n', (calls <> declarations))

        termExprToAnnoS :: Namer -> ExprI -> MorlocMonad (Namer, AnnoS Int ManyPoly Int)
        termExprToAnnoS n e@(ExprI ci _) = do
          (n', e') <- reindexExprI e >>= collectExprS n
          return $ (n', AnnoS gi0 ci e')
    f namer (LstE es) = statefulMapM collectAnnoS namer es |>> second LstS
    f namer (TupE es) = statefulMapM collectAnnoS namer es |>> second TupS
    f namer (NamE rs) = do
      (namer', vals) <- statefulMapM collectAnnoS namer (map snd rs)
      let keys = map fst rs
      return (namer', NamS (zip keys vals))
    f namer (LamE vs e) = do
      let namer' = foldr (updateRenamer LambdaBound) namer vs
          vs' = map (fst . fromJust . (flip Map.lookup) (namerMap namer')) vs
      (_, e') <- collectAnnoS namer' e
      -- return the original name, the lambda bound terms are defined only below
      return (namer, LamS vs' e')
    f namer (LetE ((v, e1) : rest) body) = do
      (namer1, e1') <- collectAnnoS namer e1
      let namer2 = updateRenamer LetBound v namer1
          v' = fst $ fromJust $ Map.lookup v (namerMap namer2)
          innerBody = case rest of
            [] -> body
            _  -> ExprI (exprIIdx body) (LetE rest body)
      (_, body') <- collectAnnoS namer2 innerBody
      return (namer, LetS v' e1' body')
    f _ (LetE [] _) = error "Bug in collectExprS: empty let bindings"
    f namer (AppE e es) = do
      (namer', e') <- collectAnnoS namer e
      (namer'', es') <- statefulMapM collectAnnoS namer' es
      return (namer'', AppS e' es')
    f namer UniE = return (namer, UniS)
    f namer (RealE x) = return (namer, RealS x)
    f namer (IntE x) = return (namer, IntS x)
    f namer (LogE x) = return (namer, LogS x)
    f namer (StrE x) = return (namer, StrS x)
    f namer (PatE p) = return (namer, ExeS (PatCall p))
    -- all other expressions are strictly illegal here and represent compiler bugs
    f _ e = error $ "Bug in collectExprS: " <> show (render (pretty e))

updateRenamer :: BindKind -> EVar -> Namer -> Namer
updateRenamer kind v (Namer rmap ridx) =
  let v' = EV (unEVar v <> "@" <> MT.show' ridx)
   in Namer
        { namerMap = Map.insert v (v', kind) rmap
        , namerIndex = ridx + 1
        }

exprIIdx :: ExprI -> Int
exprIIdx (ExprI i _) = i

reindexExprI :: ExprI -> MorlocMonad ExprI
reindexExprI (ExprI i e) = ExprI <$> newIndex i <*> reindexExpr e

reindexExpr :: Expr -> MorlocMonad Expr
reindexExpr (ModE m es) = ModE m <$> mapM reindexExprI es
reindexExpr (AnnE e ts) = AnnE <$> reindexExprI e <*> pure ts
reindexExpr (AppE e es) = AppE <$> reindexExprI e <*> mapM reindexExprI es
reindexExpr (AssE v e es) = AssE v <$> reindexExprI e <*> mapM reindexExprI es
reindexExpr (LamE vs e) = LamE vs <$> reindexExprI e
reindexExpr (LstE es) = LstE <$> mapM reindexExprI es
reindexExpr (NamE rs) = NamE <$> mapM (\(k, e) -> (,) k <$> reindexExprI e) rs
reindexExpr (TupE es) = TupE <$> mapM reindexExprI es
reindexExpr (LetE bindings body) = LetE <$> mapM (\(v, e) -> (,) v <$> reindexExprI e) bindings <*> reindexExprI body
reindexExpr e = return e

-- FIXME: when I add linking to line numbers, I'll need to update that map
-- also. The trace should be recorded.
newIndex :: Int -> MorlocMonad Int
newIndex i = do
  i' <- MM.getCounter
  copyState i i'
  MM.sayVVV $ "Set indices " <> pretty i <> " = " <> pretty i'
  return i'
