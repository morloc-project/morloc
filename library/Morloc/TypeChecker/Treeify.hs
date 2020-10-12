{-|
Module      : Morloc.TypeChecker.Treeify
Description : I am groot
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Treeify (treeify) where

import Morloc.Namespace
import Morloc.Data.Doc
import Morloc.TypeChecker.PartialOrder
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
        mapM (collect d k n) (Set.toList (typedNodeExports n))
    _ -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"

-- -- | Build the call tree for a single nexus command. The result is ambiguous,
-- -- with 1 or more possible tree topologies, each with one or more possible for
-- -- each function.
collect
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> MVar
  -> TypedNode
  -> EVar
  -> MorlocMonad (SAnno GMeta Many [CType])
collect d k n v = do
  -- Just look at one x, since any should emit the same GMeta (if not, then
  -- something is broken upstream of GMeta is not general enough)
  gmeta <- makeGMeta (Just v) (typedNodeTypeMap n) Nothing

  -- DAG MVar None (EVar, (TypedNode, [TermOrigin]))
  let termTree = MDD.lookupAliasedTerm v k (makeTermOrigin v) d

  -- DAG MVar None [(SExpr GMeta Many [CType], [CType])]
  sexprTree <- MDD.mapNodeM (\(v,(n,ts)) -> collectTerms d v n ts) termTree

  -- [(SExpr GMeta Many [CType], [CType])]
  let trees = concat . MDD.nodes $ sexprTree

  return $ SAnno (Many trees) gmeta


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
        , metaGType = maybe (Just . GType $ etype e) Just gtype
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
collectTerm d v n (Declared (AnnE x ts)) = undefined
      -- xs <- collectExpr Set.empty m (getCTypes ts) x
      -- case xs of
      --   [x] -> return x
      --   _ -> MM.throwError . GeneratorError $
      --     "Expected exactly one topology for a declared term"
collectTerm _ _ _ (Declared _) = MM.throwError . GeneratorError $
  "Invalid expression in CollectTerm Declared, expected AnnE"
collectTerm d v n (Sourced src)
  = case Map.lookup v (typedNodeTypeMap n) of
    Nothing -> MM.throwError . CallTheMonkeys $ "No type found for this"
    (Just (TypeSet g es)) -> do
      let ts = [CType (etype e) | e <- es, Just (srcLang src) == langOf e]
      return (CallS src, ts)


--     collectAnno
--       :: Set.Set EVar
--       -> Module
--       -> Expr
--       -> MorlocMonad (SAnno GMeta Many [CType])
--     collectAnno args m (AnnE e ts) = do
--       gtype <- getGType ts
--       gmeta <- makeGMeta (getExprName e) m gtype
--       trees <- collectExpr args m (getCTypes ts) e
--       return $ SAnno (Many trees) gmeta
--     collectAnno _ _ _ = error "impossible bug - unannotated expression"
--
--     getExprName :: Expr -> Maybe EVar
--     getExprName (VarE v) = Just v
--     getExprName _ = Nothing
--
--     collectExpr
--       :: Set.Set EVar
--       -> Module
--       -> [CType]
--       -> Expr
--       -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
--     collectExpr args m ts (UniE) = return [(UniS, ts)]
--     collectExpr args m ts (NumE x) = return [(NumS x, ts)]
--     collectExpr args m ts (LogE x) = return [(LogS x, ts)]
--     collectExpr args m ts (StrE x) = return [(StrS x, ts)]
--     collectExpr args m ts (VarE v)
--       | Set.member v args = return [(VarS v, ts)]
--       | otherwise = do
--           let terms = findTerm True ms m v
--           xs <- mapM collectTerm terms
--           let chosen = map (chooseTypes ts) xs
--           return chosen
--       where
--         -- FIXME: The typesystem should handle this. It should unroll every
--         -- type as far as it can be unrolled, and infer specialized types all
--         -- the way down. Multiple declarations of every term within a given
--         -- language should be allowed. The function below will only work in
--         -- special cases where there is A) a single instance of the term in
--         -- each language and B) types beneath the term (if this is a
--         -- composition) do not depend on the type on top.
--         chooseTypes
--           :: [CType]
--           -> (SExpr GMeta Many [CType], [CType])
--           -> (SExpr GMeta Many [CType], [CType])
--         chooseTypes ts (x, ts') =
--           (x, [ t
--               | t <- ts
--               , t' <- ts'
--               , langOf t == langOf t'])
--     collectExpr args m ts (ListE es) = do
--       es' <- mapM (collectAnno args m) es
--       return [(ListS es', ts)]
--     collectExpr args m ts (TupleE es) = do
--       es' <- mapM (collectAnno args m) es
--       return [(TupleS es', ts)]
--     collectExpr args m ts (RecE entries) = do
--       es' <- mapM (collectAnno args m) (map snd entries)
--       let entries' = zip (map fst entries) es'
--       return [(RecS entries', ts)]
--     collectExpr args m ts e@(LamE v x) =
--       case unrollLambda e of
--         (args', e') -> do
--           -- say $ "in LamE:" <+> prettyExpr x
--           e'' <- collectAnno (Set.union args (Set.fromList args')) m e'
--           return [(LamS args' e'', ts)]
--     -- AppS (SAnno g f c) [SAnno g f c]
--     collectExpr args m ts (AppE e1 e2) = do
--       -- say $ "in AppE:" <+> parens (prettyExpr e1) <+> parens (prettyExpr e2)
--       -- The topology of e1' may vary. It could be a direct binary function. Or
--       -- it could be a partially applied function. So it is necessary to map
--       -- over the Many.
--       e1'@(SAnno (Many fs) g1) <- collectAnno args m e1
--       e2' <- collectAnno args m e2
--       -- say $ "in AppE e1':" <+> writeManyAST e1'
--       -- say $ "in AppE e2':" <+> writeManyAST e2'
--       mapM (app g1 e2') fs
--     collectExpr _ _ _ _ = MM.throwError . GeneratorError $
--       "Unexpected expression in collectExpr"
--     app
--       :: GMeta
--       -> SAnno GMeta Many [CType]
--       -> (SExpr GMeta Many [CType], [CType])
--       -> MorlocMonad (SExpr GMeta Many [CType], [CType])
--     app _ e2 ((AppS f es), ts) = do
--       ts' <- mapM partialApplyConcrete ts
--       return (AppS f (es ++ [e2]), ts')
--     app g e2 (f, ts) = do
--       ts' <- mapM partialApplyConcrete ts
--       return (AppS (SAnno (Many [(f, ts)]) g) [e2], ts')
--
--     partialApplyConcrete :: CType -> MorlocMonad CType
--     partialApplyConcrete t =
--       fmap CType $ partialApply (unCType t)





-- collect
--   :: Map.Map MVar Module
--   -> (EVar, [TermOrigin])
--   -> MorlocMonad (SAnno GMeta Many [CType])
-- collect ms (v, []) = MM.throwError . GeneratorError $
--   "No origin found for variable '" <> unEVar v <> "'"
-- collect ms (evar', xs@(x:_)) = do
--   -- Just look at one x, since any should emit the same GMeta (if not, then
--   -- something is broken upstream of GMeta is not general enough)
--   gmeta <- makeGMeta (Just evar') (getTermModule x) Nothing
--   trees <- mapM collectTerm xs
--   return $ SAnno (Many trees) gmeta
--   where
--
--     -- Notice that `args` is NOT an input to collectTerm. Morloc uses lexical
--     -- scoping, and the input to collectTerm is the origin of a term, so the
--     -- definition of the term is outside the scope of the parent expression.
--     collectTerm
--       :: TermOrigin
--       -> MorlocMonad (SExpr GMeta Many [CType], [CType])
--     collectTerm (Declared m _ (AnnE x ts)) = do
--       xs <- collectExpr Set.empty m (getCTypes ts) x
--       case xs of
--         [x] -> return x
--         _ -> MM.throwError . GeneratorError $
--           "Expected exactly one topology for a declared term"
--     collectTerm (Declared _ _ _) = MM.throwError . GeneratorError $
--       "Invalid expression in CollectTerm Declared, expected AnnE"
--     collectTerm term@(Sourced m src) = do
--       ts <- getTermTypes term |>> getCTypes
--       return (CallS src, ts)
--       where
--         getTermTypes :: TermOrigin -> MorlocMonad [Type]
--         getTermTypes t = do
--           (TypeSet _ es) <- getTermTypeSet t
--           return $ [etype e | e <- es, Just (srcLang src) == langOf e]
--
--     collectAnno
--       :: Set.Set EVar
--       -> Module
--       -> Expr
--       -> MorlocMonad (SAnno GMeta Many [CType])
--     collectAnno args m (AnnE e ts) = do
--       gtype <- getGType ts
--       gmeta <- makeGMeta (getExprName e) m gtype
--       trees <- collectExpr args m (getCTypes ts) e
--       return $ SAnno (Many trees) gmeta
--     collectAnno _ _ _ = error "impossible bug - unannotated expression"
--
--     getExprName :: Expr -> Maybe EVar
--     getExprName (VarE v) = Just v
--     getExprName _ = Nothing
--
--     collectExpr
--       :: Set.Set EVar
--       -> Module
--       -> [CType]
--       -> Expr
--       -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
--     collectExpr args m ts (UniE) = return [(UniS, ts)]
--     collectExpr args m ts (NumE x) = return [(NumS x, ts)]
--     collectExpr args m ts (LogE x) = return [(LogS x, ts)]
--     collectExpr args m ts (StrE x) = return [(StrS x, ts)]
--     collectExpr args m ts (VarE v)
--       | Set.member v args = return [(VarS v, ts)]
--       | otherwise = do
--           let terms = findTerm True ms m v
--           xs <- mapM collectTerm terms
--           let chosen = map (chooseTypes ts) xs
--           return chosen
--       where
--         -- FIXME: The typesystem should handle this. It should unroll every
--         -- type as far as it can be unrolled, and infer specialized types all
--         -- the way down. Multiple declarations of every term within a given
--         -- language should be allowed. The function below will only work in
--         -- special cases where there is A) a single instance of the term in
--         -- each language and B) types beneath the term (if this is a
--         -- composition) do not depend on the type on top.
--         chooseTypes
--           :: [CType]
--           -> (SExpr GMeta Many [CType], [CType])
--           -> (SExpr GMeta Many [CType], [CType])
--         chooseTypes ts (x, ts') =
--           (x, [ t
--               | t <- ts
--               , t' <- ts'
--               , langOf t == langOf t'])
--     collectExpr args m ts (ListE es) = do
--       es' <- mapM (collectAnno args m) es
--       return [(ListS es', ts)]
--     collectExpr args m ts (TupleE es) = do
--       es' <- mapM (collectAnno args m) es
--       return [(TupleS es', ts)]
--     collectExpr args m ts (RecE entries) = do
--       es' <- mapM (collectAnno args m) (map snd entries)
--       let entries' = zip (map fst entries) es'
--       return [(RecS entries', ts)]
--     collectExpr args m ts e@(LamE v x) =
--       case unrollLambda e of
--         (args', e') -> do
--           -- say $ "in LamE:" <+> prettyExpr x
--           e'' <- collectAnno (Set.union args (Set.fromList args')) m e'
--           return [(LamS args' e'', ts)]
--     -- AppS (SAnno g f c) [SAnno g f c]
--     collectExpr args m ts (AppE e1 e2) = do
--       -- say $ "in AppE:" <+> parens (prettyExpr e1) <+> parens (prettyExpr e2)
--       -- The topology of e1' may vary. It could be a direct binary function. Or
--       -- it could be a partially applied function. So it is necessary to map
--       -- over the Many.
--       e1'@(SAnno (Many fs) g1) <- collectAnno args m e1
--       e2' <- collectAnno args m e2
--       -- say $ "in AppE e1':" <+> writeManyAST e1'
--       -- say $ "in AppE e2':" <+> writeManyAST e2'
--       mapM (app g1 e2') fs
--
--     collectExpr _ _ _ _ = MM.throwError . GeneratorError $
--       "Unexpected expression in collectExpr"
--     app
--       :: GMeta
--       -> SAnno GMeta Many [CType]
--       -> (SExpr GMeta Many [CType], [CType])
--       -> MorlocMonad (SExpr GMeta Many [CType], [CType])
--     app _ e2 ((AppS f es), ts) = do
--       ts' <- mapM partialApplyConcrete ts
--       return (AppS f (es ++ [e2]), ts')
--     app g e2 (f, ts) = do
--       ts' <- mapM partialApplyConcrete ts
--       return (AppS (SAnno (Many [(f, ts)]) g) [e2], ts')
--
--     partialApplyConcrete :: CType -> MorlocMonad CType
--     partialApplyConcrete t =
--       fmap CType $ partialApply (unCType t)


-- getTermModule :: TermOrigin -> Module
-- getTermModule (Sourced m _) = m
-- getTermModule (Declared m _ _) = m
--
--
-- getTermEVar :: TermOrigin -> EVar
-- getTermEVar (Sourced _ src) = srcAlias src
-- getTermEVar (Declared _ v _) = v
--
--
-- getTermTypeSet :: TermOrigin -> MorlocMonad TypeSet
-- getTermTypeSet t =
--   case Map.lookup (getTermEVar t) (moduleTypeMap (getTermModule t)) of
--     (Just ts) -> return ts
--     Nothing -> MM.throwError . GeneratorError $ "Expected the term to have a typeset"
--
--
-- unrollLambda :: Expr -> ([EVar], Expr)
-- unrollLambda (LamE v e2) = case unrollLambda e2 of
--   (vs, e) -> (v:vs, e)
-- unrollLambda (AnnE (LamE v e2) _) = case unrollLambda e2 of
--   (vs, e) -> (v:vs, e)
-- unrollLambda e = ([], e)
--
--
-- getGType :: [Type] -> MorlocMonad (Maybe GType)
-- getGType ts = case [GType t | t <- ts, isNothing (langOf t)] of
--   [] -> return Nothing
--   [x] -> return $ Just x
--   xs -> MM.throwError . GeneratorError $
--     "Expected 0 or 1 general types, found " <> MT.show' (length xs)
--
--
-- getCTypes :: [Type] -> [CType]
-- getCTypes ts = [CType t | t <- ts, isJust (langOf t)]
--
--
-- partialApply :: Type -> MorlocMonad Type
-- partialApply (FunT _ t) = return t
-- partialApply (Forall v t) = do
--   t' <- partialApply t
--   return $ if varIsUsed v t' then Forall v t' else t'
--   where
--     varIsUsed :: TVar -> Type -> Bool
--     varIsUsed v (VarT v') = v == v'
--     varIsUsed v (ExistT v' ts ds)
--       =  v == v'
--       || any (varIsUsed v) ts
--       || any (varIsUsed v) (map unDefaultType ds)
--     varIsUsed v (Forall v' t)
--       | v == v' = False
--       | otherwise = varIsUsed v t
--     varIsUsed v (FunT t1 t2) = varIsUsed v t1 || varIsUsed v t2
--     varIsUsed v (ArrT v' ts) = any (varIsUsed v) ts
--     varIsUsed v (NamT v' entries) = any (varIsUsed v) (map snd entries)
-- partialApply _ = MM.throwError . GeneratorError $
--   "Cannot partially apply a non-function type"
--
--
-- partialApplyN :: Int -> Type -> MorlocMonad Type
-- partialApplyN i t
--   | i < 0 = MM.throwError . GeneratorError $
--     "Do you really want to apply a negative number of arguments?"
--   | i == 0 = return t
--   | i > 0 = do
--     appliedType <- partialApply t
--     partialApplyN (i-1) appliedType


-- {- | Find exported expressions.
--
-- Terms may be declared or sourced in the current module or they may be imported
-- from a different module. If they are imported, ascend through modules to the
-- original declaration, returning the module where they are defined.
--
-- For each input term (EVar) a list is returned. Each element in the list
-- describes a specific implementation of the term. These implementations may have
-- different topologies and languages. A given language may have more than one
-- implementation. However, all implementations share the same general type.
--
-- Each element in the return list is a tuple of two values. The module where the
-- term is exported and the source/declaration information needed to uniquely
-- specify it (within an Either monad). If the term is sourced, then a (Left
-- Source) data constructor holds the required source information. If the term is
-- declared, a (EVar, Expr) tuple stores the left and right sides of a declaration
-- (the same information that is stored in the Declaration data constructor of
-- Expr).
-- -}
-- findTerm
--   :: Bool -- ^ should non-exported terms be included?
--   -> Map.Map MVar Module
--   -> Module -- ^ a module where EVar is used
--   -> EVar -- ^ the variable name in the top level module
--   -> [TermOrigin]
-- findTerm includeInternal ms m v
--   | includeInternal || Set.member v (moduleExports m)
--       = evarDeclared
--       ++ evarSourced
--       ++ evarImported
--   | otherwise = []
--   where
--     evarDeclared :: [TermOrigin]
--     evarDeclared = concat [findDecl e | (Declaration v' e) <- moduleBody m, v' == v]
--
--     findDecl :: Expr -> [TermOrigin]
--     -- If a term is defined as being equal to another term, find this other term.
--     findDecl (VarE v')
--       | v /= v' = findTerm False ms m v'
--       | v == v' = error "found term of type `x = x`, the typechecker should have died on this ..."
--     findDecl e = [Declared m v e]
--
--     evarSourced :: [TermOrigin]
--     evarSourced = map (\(_, src) -> Sourced m src)
--                 . Map.toList
--                 . Map.filterWithKey (\(v',_) _ -> v' == v)
--                 $ moduleSourceMap m
--
--     evarImported :: [TermOrigin]
--     evarImported =
--       concat [findTerm False ms m' v | m' <- mapMaybe (flip Map.lookup $ ms) (listMVars m)]
--
--     typeEVar :: EVar -> Expr
--     typeEVar name = case Map.lookup name (moduleTypeMap m) of
--       (Just (TypeSet t ts)) -> AnnE (VarE name) (map etype (maybe ts (\t' -> t':ts) t))
--       Nothing -> error $ "Variable '" <> MT.unpack (unEVar name) <> "' is not defined"
--
--     listMVars :: Module -> [MVar]
--     listMVars = map importModuleName . filter (inImport v) . moduleImports
--
--     inImport :: EVar -> Import -> Bool
--     inImport v imp = case (importInclude imp, importExclude imp) of
--         (Nothing, ex) -> not (elem v ex)
--         (Just included, _) -> elem v (map snd included)
