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
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Data.Map as Map
import qualified Data.Set as Set

-- -- | Node description after type checking. This will later be fed into
-- -- `treeify` to make the SAnno objects that will be passed to Generator.
-- data TypedNode = TypedNode {
--     typedNodeModuleName :: MVar
--   , typedNodePath :: Maybe Path
--   , typedNodeBody :: [Expr]
--   , typedNodeTypeMap :: Map EVar TypeSet
--   , typedNodeSourceMap :: Map (EVar, Lang) Source
--   , typedNodeExports :: Set EVar
--   , typedNodeTypedefs :: Map TVar (Type, [TVar])
--   , typedNodePackers :: Map (TVar, Int) [UnresolvedPacker]
--   , typedNodeConstructors :: Map TVar Source
--   -- ^ The (un)packers available in this module scope.
-- } deriving (Show, Ord, Eq)
-- type TypedDag = DAG MVar [(EVar, EVar)] TypedNode


data TermOrigin = Declared Expr | Sourced Source
  deriving(Show, Ord, Eq)

treeify
  :: DAG MVar [(EVar, EVar)] PreparedNode
  -> MorlocMonad [SAnno (GMeta UnresolvedType) Many [UnresolvedType]]
treeify d
  | Map.size d == 0 = return []
  | otherwise = case MDD.roots d of
    -- if no parentless element exists, then the graph must be empty or cyclic
    [] -> MM.throwError CyclicDependency
    [k] -> case MDD.lookupNode k d of
      Nothing -> MM.throwError . DagMissingKey . render $ pretty k
      (Just n) -> do
        -- initialize state counter to 0, used to index manifolds
        MM.startCounter
        mapM (collect d n) (Set.toList (preparedNodeExports n))
    -- There is no currently supported use case that exposes multiple roots in
    -- one compilation process. The compiler executable takes a single morloc
    -- file as input, therefore this MUST be the root. In the future compiling
    -- multiple projects in parallel with potentially shared information and
    -- constraints could be valuable.
    _ -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"


-- | Build the call tree for a single nexus command. The result is ambiguous,
-- with 1 or more possible tree topologies, each with one or more possible for
-- each function.
collect
  :: DAG MVar [(EVar, EVar)] PreparedNode
  -> PreparedNode
  -> EVar
  -> MorlocMonad (SAnno (GMeta UnresolvedType) Many [UnresolvedType])
collect d n v = do
  trees <- collectSExprs d n v

  -- Just look at one x, since any should emit the same GMeta (if not, then
  -- something is broken upstream of GMeta is not general enough)
  gmeta <- makeGMeta (Just v) n Nothing

  return $ SAnno (Many trees) gmeta

collectSExprs
  :: DAG MVar [(EVar, EVar)] PreparedNode
  -> PreparedNode
  -> EVar
  -> MorlocMonad [(SExpr (GMeta UnresolvedType) Many [UnresolvedType], [UnresolvedType])]
collectSExprs d n v = do
  -- DAG MVar None (EVar, (PreparedNode, [TermOrigin]))
  let termTree = MDD.lookupAliasedTerm v m (makeTermOrigin v) d

  -- DAG MVar None [(SExpr GMeta Many [CType], [CType])]
  sexprTree <- MDD.mapNodeM (\(v',(n',ts)) -> collectTerms d v' n' ts) termTree

  -- [(SExpr GMeta Many [CType], [CType])]
  let trees = concat . MDD.nodes $ sexprTree

  return trees


-- | Find info common across realizations of a given term in a given module
makeGMeta
  :: Maybe EVar
  -> PreparedNode
  -> Maybe UnresolvedType
  -> MorlocMonad GMeta
makeGMeta name n gtype = do
  i <- MM.getCounter
  case name >>= (flip Map.lookup) (typedNodeTypeMap n) of
    (Just (TypeSet (Just e) _)) -> do
      let g = (Just . GType) $ resolve (etype e)
      return $ GMeta
        { metaId = i
        , metaGType = maybe g Just gtype
        , metaName = name
        , metaProperties = eprop e
        , metaConstraints = econs e
        , metaPackers = typedNodePackers n
        , metaConstructors = typedNodeConstructors n
        , metaTypedefs = typedNodeTypedefs n
        }
    _ -> do
      return $ GMeta
        { metaId = i
        , metaGType = gtype
        , metaName = name
        , metaProperties = Set.empty
        , metaConstraints = Set.empty
        , metaPackers = typedNodePackers n
        , metaConstructors = typedNodeConstructors n
        , metaTypedefs = typedNodeTypedefs n
        }

-- makeTermOrigin
--   :: EVar
--   -> PreparedNode
--   -> (PreparedNode, [TermOrigin])
-- makeTermOrigin v n = (n, declared ++ sourced) where
--   declared = [Declared e | (Declaration v' e) <- typedNodeBody n, v' == v]
--   sourced = map Sourced
--           $ filter (\s -> srcAlias s == v) (Map.elems $ typedNodeSourceMap n)
--
--
-- collectTerms
--   :: DAG MVar [(EVar, EVar)] PreparedNode
--   -> EVar
--   -> PreparedNode
--   -> [TermOrigin]
--   -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
-- collectTerms d v n ts = mapM (collectTerm d v n) ts
--
--
-- -- Notice that `args` is NOT an input to collectTerm. Morloc uses lexical
-- -- scoping, and the input to collectTerm is the origin of a term, so the
-- -- definition of the term is outside the scope of the parent expression.
-- collectTerm
--   :: DAG MVar [(EVar, EVar)] PreparedNode
--   -> EVar
--   -> PreparedNode
--   -> TermOrigin
--   -> MorlocMonad (SExpr GMeta Many [CType], [CType])
-- collectTerm _ v n (Sourced src)
--   = case Map.lookup v (typedNodeTypeMap n) of
--     Nothing -> MM.throwError . CallTheMonkeys $ "No type found for this"
--     (Just (TypeSet _ es)) -> do
--       let ts = [etype e | e <- es, Just (srcLang src) == langOf e]
--           ts' = map resolve ts
--       return (CallS src, map CType ts')
-- collectTerm d _ n (Declared (AnnE e ts)) = do
--   ts' <- getCTypes ts
--   xs <- collectExpr d Set.empty n ts' e
--   case xs of
--     [x] -> return x
--     _ -> MM.throwError . GeneratorError $
--       "Expected exactly one topology for declared term, no language-specific type found for expression: " <> render (viaShow e <+> " ; ts':" <+> viaShow ts')
--       -- "Expected exactly one topology for declared term, no language-specific type found for expression:" <+> render ((prettyExpr e) <+> "ts':" <+> viaShow ts' <+> "ts" <+> viaShow ts
-- collectTerm _ _ _ (Declared _) = MM.throwError . GeneratorError $
--   "Invalid expression in CollectTerm Declared, expected AnnE"
--
--
-- collectAnno
--   :: DAG MVar [(EVar, EVar)] PreparedNode
--   -> Set.Set EVar
--   -> PreparedNode
--   -> Expr
--   -> MorlocMonad (SAnno GMeta Many [CType])
-- collectAnno d args n (AnnE e ts) = do
--   gtype <- getGType ts
--   gmeta <- makeGMeta (getExprName e) n gtype
--   ts' <- getCTypes ts
--   trees <- collectExpr d args n ts' e
--   return $ SAnno (Many trees) gmeta
-- collectAnno _ _ _ _ = error "impossible bug - unannotated expression"
--
-- getCTypes :: [UnresolvedType] -> MorlocMonad [CType]
-- getCTypes ts = do
--   let ts' = map resolve [t | t <- ts, isJust (langOf t)]
--   return $ map CType ts'
--
--
--
-- collectExpr
--   :: DAG MVar [(EVar, EVar)] PreparedNode
--   -> Set.Set EVar
--   -> PreparedNode
--   -> [CType]
--   -> Expr
--   -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
-- collectExpr _ _ _ ts (UniE)   = return [(UniS, ts)]
-- collectExpr _ _ _ ts (NumE x) = return [(NumS x, ts)]
-- collectExpr _ _ _ ts (LogE x) = return [(LogS x, ts)]
-- collectExpr _ _ _ ts (StrE x) = return [(StrS x, ts)]
-- collectExpr d args n ts (VarE v)
--   | Set.member v args = return [(VarS v, ts)]
--   | otherwise = do
--       xs <- collectSExprs d n v
--       let chosen = map (chooseTypes ts) xs
--       return chosen
--   where
--     -- FIXME: The typesystem should handle this. It should unroll every
--     -- type as far as it can be unrolled, and infer specialized types all
--     -- the way down. Multiple declarations of every term within a given
--     -- language should be allowed. The function below will only work in
--     -- special cases where there is A) a single instance of the term in
--     -- each language and B) types beneath the term (if this is a
--     -- composition) do not depend on the type on top.
--     chooseTypes
--       :: [CType]
--       -> (SExpr GMeta Many [CType], [CType])
--       -> (SExpr GMeta Many [CType], [CType])
--     chooseTypes ts1 (x, ts2) =
--       (x, [ t
--           | t <- ts1
--           , t' <- ts2
--           , langOf t == langOf t'])
-- collectExpr d args n ts (AccE e k) = do
--   e' <- collectAnno d args n e
--   return [(AccS e' k, ts)]
-- collectExpr d args n ts (ListE es) = do
--   es' <- mapM (collectAnno d args n) es
--   return [(ListS es', ts)]
-- collectExpr d args n ts (TupleE es) = do
--   es' <- mapM (collectAnno d args n) es
--   return [(TupleS es', ts)]
-- collectExpr d args n ts (RecE entries) = do
--   es' <- mapM (collectAnno d args n) (map snd entries)
--   let entries' = zip (map fst entries) es'
--   return [(RecS entries', ts)]
-- collectExpr d args n ts e@(LamE _ _) = do
--   case unrollLambda e of
--     (args', e') -> do
--       e'' <- collectAnno d (Set.union args (Set.fromList args')) n e'
--       return [(LamS args' e'', ts)]
-- collectExpr d args n _ (AppE e1 e2) = do
--   -- The topology may vary. It could be a direct binary function. Or
--   -- it could be a partially applied function. So it is necessary to map
--   -- over the Many.
--   (SAnno (Many fs) g1) <- collectAnno d args n e1
--   e2' <- collectAnno d args n e2
--   mapM (app g1 e2') fs
-- -- None of these should occur unless there is a bug in the code
-- collectExpr _ _ _ _ x@(AnnE _ _) = error $ show x
-- collectExpr _ _ _ _ x@(SrcE _) = error $ show x
-- collectExpr _ _ _ _ x@(Signature _ _) = error $ show x
-- collectExpr _ _ _ _ x@(Declaration _ _) = error $ show x
--
--
--
-- app
--   :: GMeta
--   -> SAnno GMeta Many [CType]
--   -> (SExpr GMeta Many [CType], [CType])
--   -> MorlocMonad (SExpr GMeta Many [CType], [CType])
-- app _ e2 ((AppS f es), ts) = do
--   ts' <- mapM partialApplyConcrete ts
--   return (AppS f (es ++ [e2]), ts')
-- app g e2 (f, ts) = do
--   ts' <- mapM partialApplyConcrete ts
--   return (AppS (SAnno (Many [(f, ts)]) g) [e2], ts')
--
-- partialApplyConcrete :: CType -> MorlocMonad CType
-- partialApplyConcrete t =
--   fmap CType $ partialApply (unCType t)
--
-- partialApply :: Type -> MorlocMonad Type
-- partialApply (FunT _ t) = return t
-- partialApply _ = MM.throwError . GeneratorError $
--   "Cannot partially apply a non-function type"
--
-- getExprName :: Expr -> Maybe EVar
-- getExprName (VarE v) = Just v
-- getExprName _ = Nothing
--
-- getGType :: [UnresolvedType] -> MorlocMonad (Maybe GType)
-- getGType ts = do
--   let ts' = map resolve [t | t <- ts, isNothing (langOf t)]
--   case map GType ts' of
--     [] -> return Nothing
--     [x] -> return $ Just x
--     xs -> MM.throwError . GeneratorError $
--       "Expected 0 or 1 general types, found " <> MT.show' (length xs)
--
-- unrollLambda :: Expr -> ([EVar], Expr)
-- unrollLambda (LamE v e2) = case unrollLambda e2 of
--   (vs, e) -> (v:vs, e)
-- unrollLambda (AnnE (LamE v e2) _) = case unrollLambda e2 of
--   (vs, e) -> (v:vs, e)
-- unrollLambda e = ([], e)
