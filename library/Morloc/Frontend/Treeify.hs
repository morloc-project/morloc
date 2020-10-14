{-|
Module      : Morloc.Frontend.Treeify
Description : I am groot
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Treeify (treeify) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import Morloc.Frontend.PartialOrder
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Data.Map as Map
import qualified Data.Set as Set

data TermOrigin = Declared Expr | Sourced Source
  deriving(Show, Ord, Eq)

treeify
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> MorlocMonad [SAnno GMeta Many [CType]]
treeify d
  | Map.size d == 0 = return []
  | otherwise = case MDD.roots d of
    [] -> MM.throwError CyclicDependency
    [k] -> case MDD.lookupNode k d of
      Nothing -> MM.throwError . DagMissingKey . render $ pretty k
      (Just n) -> do
        -- initialize state counter to 0, used to index manifolds
        MM.startCounter
        mapM (collect d n) (Set.toList (typedNodeExports n))
    _ -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"

-- -- | Build the call tree for a single nexus command. The result is ambiguous,
-- -- with 1 or more possible tree topologies, each with one or more possible for
-- -- each function.
collect
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> TypedNode
  -> EVar
  -> MorlocMonad (SAnno GMeta Many [CType])
collect d n v = do
  trees <- collectSExprs d n v

  -- Just look at one x, since any should emit the same GMeta (if not, then
  -- something is broken upstream of GMeta is not general enough)
  gmeta <- makeGMeta (Just v) (typedNodeTypeMap n) Nothing

  return $ SAnno (Many trees) gmeta

collectSExprs
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> TypedNode
  -> EVar
  -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
collectSExprs d n v = do
  -- DAG MVar None (EVar, (TypedNode, [TermOrigin]))
  let termTree = MDD.lookupAliasedTerm v (typedNodeModuleName n) (makeTermOrigin v) d

  -- DAG MVar None [(SExpr GMeta Many [CType], [CType])]
  sexprTree <- MDD.mapNodeM (\(v,(n,ts)) -> collectTerms d v n ts) termTree

  -- [(SExpr GMeta Many [CType], [CType])]
  let trees = concat . MDD.nodes $ sexprTree

  return trees


-- | Find info common across realizations of a given term in a given module
makeGMeta
  :: Maybe EVar
  -> Map.Map EVar TypeSet
  -> Maybe GType
  -> MorlocMonad GMeta
makeGMeta name typemap gtype = do
  i <- MM.getCounter
  case name >>= (flip Map.lookup) typemap of
    (Just (TypeSet (Just e) _)) -> do
      return $ GMeta
        { metaId = i
        , metaName = name
        , metaGType = maybe (Just . GType . unresolvedType2type $ etype e) Just gtype
        , metaProperties = eprop e
        , metaConstraints = econs e
        }
    _ -> do
      return $ GMeta
        { metaId = i
        , metaName = name
        , metaGType = gtype
        , metaProperties = Set.empty
        , metaConstraints = Set.empty
        }


makeTermOrigin
  :: EVar
  -> TypedNode
  -> (TypedNode, [TermOrigin])
makeTermOrigin v n = (n, declared ++ sourced) where
  declared = [Declared e | (Declaration v' e) <- typedNodeBody n, v' == v]
  sourced = map Sourced
          $ filter (\s -> srcAlias s == v) (Map.elems $ typedNodeSourceMap n)


collectTerms
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> EVar
  -> TypedNode
  -> [TermOrigin]
  -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
collectTerms d v n ts = mapM (collectTerm d v n) ts


-- Notice that `args` is NOT an input to collectTerm. Morloc uses lexical
-- scoping, and the input to collectTerm is the origin of a term, so the
-- definition of the term is outside the scope of the parent expression.
collectTerm
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> EVar
  -> TypedNode
  -> TermOrigin
  -> MorlocMonad (SExpr GMeta Many [CType], [CType])
collectTerm d v n (Sourced src)
  = case Map.lookup v (typedNodeTypeMap n) of
    Nothing -> MM.throwError . CallTheMonkeys $ "No type found for this"
    (Just (TypeSet g es)) -> do
      let ts = [CType ((unresolvedType2type . etype) e) | e <- es, Just (srcLang src) == langOf e]
      return (CallS src, ts)
collectTerm d v n (Declared (AnnE e ts)) = do
      xs <- collectExpr d Set.empty n (getCTypes ts) e
      case xs of
        [x] -> return x
        _ -> MM.throwError . GeneratorError $
          "Expected exactly one topology for a declared term"
collectTerm _ _ _ (Declared _) = MM.throwError . GeneratorError $
  "Invalid expression in CollectTerm Declared, expected AnnE"


collectAnno
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> Set.Set EVar
  -> TypedNode
  -> Expr
  -> MorlocMonad (SAnno GMeta Many [CType])
collectAnno d args n (AnnE e ts) = do
  gtype <- getGType ts
  gmeta <- makeGMeta (getExprName e) (typedNodeTypeMap n) gtype
  trees <- collectExpr d args n (getCTypes ts) e
  return $ SAnno (Many trees) gmeta
collectAnno _ _ _ _ = error "impossible bug - unannotated expression"


collectExpr
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> Set.Set EVar
  -> TypedNode
  -> [CType]
  -> Expr
  -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
collectExpr _ _ _ ts (UniE)   = return [(UniS, ts)]
collectExpr _ _ _ ts (NumE x) = return [(NumS x, ts)]
collectExpr _ _ _ ts (LogE x) = return [(LogS x, ts)]
collectExpr _ _ _ ts (StrE x) = return [(StrS x, ts)]
collectExpr d args n ts (VarE v)
  | Set.member v args = return [(VarS v, ts)]
  | otherwise = do
      xs <- collectSExprs d n v
      let chosen = map (chooseTypes ts) xs
      return chosen
  where
    -- FIXME: The typesystem should handle this. It should unroll every
    -- type as far as it can be unrolled, and infer specialized types all
    -- the way down. Multiple declarations of every term within a given
    -- language should be allowed. The function below will only work in
    -- special cases where there is A) a single instance of the term in
    -- each language and B) types beneath the term (if this is a
    -- composition) do not depend on the type on top.
    chooseTypes
      :: [CType]
      -> (SExpr GMeta Many [CType], [CType])
      -> (SExpr GMeta Many [CType], [CType])
    chooseTypes ts (x, ts') =
      (x, [ t
          | t <- ts
          , t' <- ts'
          , langOf t == langOf t'])
collectExpr d args n ts (ListE es) = do
  es' <- mapM (collectAnno d args n) es
  return [(ListS es', ts)]
collectExpr d args n ts (TupleE es) = do
  es' <- mapM (collectAnno d args n) es
  return [(TupleS es', ts)]
collectExpr d args n ts (RecE entries) = do
  es' <- mapM (collectAnno d args n) (map snd entries)
  let entries' = zip (map fst entries) es'
  return [(RecS entries', ts)]
collectExpr d args n ts e@(LamE v x) = do
  case unrollLambda e of
    (args', e') -> do
      -- say $ "in LamE:" <+> prettyExpr x
      e'' <- collectAnno d (Set.union args (Set.fromList args')) n e'
      return [(LamS args' e'', ts)]
collectExpr d args n ts (AppE e1 e2) = do
  -- say $ "in AppE:" <+> parens (prettyExpr e1) <+> parens (prettyExpr e2)
  -- The topology of e1' may vary. It could be a direct binary function. Or
  -- it could be a partially applied function. So it is necessary to map
  -- over the Many.
  e1'@(SAnno (Many fs) g1) <- collectAnno d args n e1
  e2' <- collectAnno d args n e2
  -- say $ "in AppE e1':" <+> writeManyAST e1'
  -- say $ "in AppE e2':" <+> writeManyAST e2'
  mapM (app g1 e2') fs
collectExpr _ _ _ _ _ = MM.throwError . GeneratorError $
  "Unexpected expression in collectExpr"



app
  :: GMeta
  -> SAnno GMeta Many [CType]
  -> (SExpr GMeta Many [CType], [CType])
  -> MorlocMonad (SExpr GMeta Many [CType], [CType])
app _ e2 ((AppS f es), ts) = do
  ts' <- mapM partialApplyConcrete ts
  return (AppS f (es ++ [e2]), ts')
app g e2 (f, ts) = do
  ts' <- mapM partialApplyConcrete ts
  return (AppS (SAnno (Many [(f, ts)]) g) [e2], ts')

partialApplyConcrete :: CType -> MorlocMonad CType
partialApplyConcrete t =
  fmap CType $ partialApply (unCType t)

partialApply :: Type -> MorlocMonad Type
partialApply (FunT _ t) = return t
partialApply (Forall v t) = do
  t' <- partialApply t
  return $ if varIsUsed v t' then Forall v t' else t'
  where
    varIsUsed :: TVar -> Type -> Bool
    varIsUsed v (VarT v') = v == v'
    varIsUsed v (ExistT v' ts ds)
      =  v == v'
      || any (varIsUsed v) ts
      || any (varIsUsed v) (map unDefaultType ds)
    varIsUsed v (Forall v' t)
      | v == v' = False
      | otherwise = varIsUsed v t
    varIsUsed v (FunT t1 t2) = varIsUsed v t1 || varIsUsed v t2
    varIsUsed v (ArrT v' ts) = any (varIsUsed v) ts
    varIsUsed v (NamT v' entries) = any (varIsUsed v) (map snd entries)
partialApply _ = MM.throwError . GeneratorError $
  "Cannot partially apply a non-function type"

getCTypes :: [UnresolvedType] -> [CType]
getCTypes ts = [CType . unresolvedType2type $ t | t <- ts, isJust (langOf t)]

getExprName :: Expr -> Maybe EVar
getExprName (VarE v) = Just v
getExprName _ = Nothing

getGType :: [UnresolvedType] -> MorlocMonad (Maybe GType)
getGType ts = case [GType . unresolvedType2type $ t | t <- ts, isNothing (langOf t)] of
  [] -> return Nothing
  [x] -> return $ Just x
  xs -> MM.throwError . GeneratorError $
    "Expected 0 or 1 general types, found " <> MT.show' (length xs)

unrollLambda :: Expr -> ([EVar], Expr)
unrollLambda (LamE v e2) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda (AnnE (LamE v e2) _) = case unrollLambda e2 of
  (vs, e) -> (v:vs, e)
unrollLambda e = ([], e)
