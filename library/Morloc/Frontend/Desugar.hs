{-|
Module      : Morloc.Frontend.Desugar
Description : Write Module objects to resolve type aliases and such
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Desugar (desugar, desugarType) where

import Morloc.Frontend.Namespace
import Morloc.Pretty ()
import Morloc.Data.Doc
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.GMap as GMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Frontend.PartialOrder as MTP


-- | Resolve type aliases, term aliases and import/exports
desugar
  :: DAG MVar Import ExprI
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
desugar s
  = resolveImports s -- rewrite DAG edges to map imported terms to their aliases
  >>= checkForSelfRecursion -- modules should not import themselves
  >>= desugarDag -- substitute type aliases
  >>= addPackerMap -- add the packers to state


-- | Consider export/import information to determine which terms are imported
-- into each module. This step reduces the Import edge type from an m-to-n
-- source name to an alias map.
resolveImports
  :: DAG MVar Import ExprI
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
resolveImports = MDD.mapEdgeWithNodeM resolveImport where
  resolveImport
    :: ExprI
    -> Import
    -> ExprI
    -> MorlocMonad [(EVar, EVar)]
  resolveImport _ (Import _ Nothing exc _) n2
    = return
    . map (\x -> (x,x)) -- alias is identical
    . Set.toList
    $ Set.difference (AST.findExportSet n2) (Set.fromList exc)
  resolveImport _ (Import _ (Just inc) exc _) n2
    | length contradict > 0
        = MM.throwError . CallTheMonkeys
        $ "Error: The following terms are both included and excluded: " <>
          render (tupledNoFold $ map pretty contradict)
    | length missing > 0
        = MM.throwError . CallTheMonkeys
        $ "Error: The following terms are not exported: " <>
          render (tupledNoFold $ map pretty missing)
    | otherwise = return inc
    where
      missing = [n | (n, _) <- inc, not $ Set.member n (AST.findExportSet n2)]
      contradict = [n | (n, _) <- inc, elem n exc]

checkForSelfRecursion :: DAG MVar [(EVar, EVar)] ExprI -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
checkForSelfRecursion d = do
  MDD.mapNodeM (AST.checkExprI isExprSelfRecursive) d
  return d
  where
    -- A typedef is self-recursive if its name appears in its definition
    isExprSelfRecursive :: ExprI -> MorlocMonad ()
    isExprSelfRecursive (ExprI _ (TypE v _ t))
      | hasTerm v t = MM.throwError . SelfRecursiveTypeAlias $ v 
      | otherwise = return ()
    isExprSelfRecursive _ = return ()

    -- check if a given term appears in a type
    hasTerm :: TVar -> TypeU -> Bool
    hasTerm v (VarU v') = v == v'
    hasTerm v (ForallU _ t) = hasTerm v t

    hasTerm v (FunU (t1:rs) t2) = hasTerm v t1 || hasTerm v (FunU rs t2)
    hasTerm v (FunU [] t) = hasTerm v t

    hasTerm v (AppU t1 (t2:rs)) = hasTerm v t2 || hasTerm v (AppU t1 rs)
    hasTerm v (AppU t1 []) = hasTerm v t1

    hasTerm v (NamU o n ps ((_, t):rs)) = hasTerm v t || hasTerm v (NamU o n ps rs)
    hasTerm _ (NamU _ _ _ []) = False

    hasTerm _ (ExistU _ _ _) = error "There should not be existentionals in typedefs"

desugarDag
  :: DAG MVar [(EVar, EVar)] ExprI
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
desugarDag m = MDD.mapNodeWithKeyM (desugarExpr m) m where

desugarExpr
  :: DAG MVar [(EVar, EVar)] ExprI
  -> MVar
  -> ExprI
  -> MorlocMonad ExprI
desugarExpr d k e0 = mapExprM f e0 where

  f :: Expr -> MorlocMonad Expr
  f (SigE v l t) = SigE v l <$> desugarEType termmap d k t
  f (AnnE e ts) = AnnE e <$> mapM (desugarType termmap d k) ts
  f e = return e

  termmap :: Map.Map TVar [([TVar], TypeU)]
  termmap =
    let terms = AST.findSignatureTypeTerms e0
    in Map.fromList $ zip terms (map lookupTypedefs terms)

  lookupTypedefs
    :: TVar
    -> [([TVar], TypeU)]
  lookupTypedefs (TV lang v)
    = catMaybes
    . MDD.nodes
    . MDD.mapNode (\(EV v', typemap) -> Map.lookup (TV lang v') typemap)
    $ MDD.lookupAliasedTerm (EV v) k AST.findTypedefs d


desugarEType
  :: Map.Map TVar [([TVar], TypeU)]
  -> DAG MVar [(EVar, EVar)] ExprI
  -> MVar -> EType -> MorlocMonad EType
desugarEType h d k (EType t ps cs) = EType <$> desugarType h d k t <*> pure ps <*> pure cs


desugarType
  :: Map.Map TVar [([TVar], TypeU)]
  -> DAG MVar [(EVar, EVar)] ExprI
  -> MVar
  -> TypeU
  -> MorlocMonad TypeU
desugarType h _ _ = f
  where

  f :: TypeU -> MorlocMonad TypeU

  --   (Just []) -> return (t0, [])
  --   (Just ts'@(t':_)) -> do
  --     (_, t) <- foldlM (mergeAliases v 0) t' ts'
  --     f t
  --   Nothing -> MM.throwError . CallTheMonkeys $ "Type term in VarU missing from type map"
  f (ExistU v ps ds) = do
    ps' <- mapM f ps
    ds' <- mapM f ds
    return $ ExistU v ps' ds'
  f (FunU ts t) = FunU <$> mapM f ts <*> f t
  f (NamU o n ps rs) = do 
    ts <- mapM (f . snd) rs
    return $ NamU o n ps (zip (map fst rs) ts)

  -- type Cpp (A a b) = "map<$1,$2>" a b
  -- foo Cpp :: A D [B] -> X
  -- -----------------------------------
  -- foo :: "map<$1,$2>" D [B] -> X
  --
  -- type Foo a = (a, A)
  -- f :: Foo Int -> B
  -- -----------------
  -- f :: (Int, A) -> B
  --
  f t@(AppU (VarU v) ts) =
    case Map.lookup v h of
      (Just []) -> AppU (VarU v) <$> mapM f ts
      (Just (t':ts')) -> do
        (vs, t) <- foldlM (mergeAliases v (length ts)) t' ts'
        if length ts == length vs
          -- substitute parameters into alias
          then f (foldr parsub (chooseExistential t) (zip vs (map chooseExistential ts)))
          else MM.throwError $ BadTypeAliasParameters v (length vs) (length ts)
      -- default types like "Int" or "Tuple2" won't be in the map
      Nothing -> return t

  -- type Foo = A     
  -- f :: Foo -> B    
  -- -----------------
  -- f :: A -> B      
  f t0@(VarU v) =
     case Map.lookup v h of
      (Just []) -> return t0
      (Just ts1@(t1:_)) -> do
        (_, t2) <- foldlM (mergeAliases v 0) t1 ts1
        f t2
      Nothing -> return t0

  f (ForallU v t) = ForallU v <$> f t

  parsub :: (TVar, TypeU) -> TypeU -> TypeU
  parsub (v, t2) t1@(VarU v0)
    | v0 == v = t2 -- substitute
    | otherwise = t1 -- keep the original
  parsub _ (ExistU _ _ _) = error "What the bloody hell is an existential doing down here?"
  parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
  parsub pair (FunU ts t) = FunU (map (parsub pair) ts) (parsub pair t)
  parsub pair (AppU t ts) = AppU (parsub pair t) (map (parsub pair) ts)
  parsub pair (NamU o n ps rs) = NamU o n ps [(k', parsub pair t) | (k', t) <- rs]


  -- When a type alias is imported from two places, this function reconciles them, if possible
  mergeAliases
    :: TVar
    -> Int
    -> ([TVar], TypeU)
    -> ([TVar], TypeU)
    -> MorlocMonad ([TVar], TypeU)
  mergeAliases v i t@(ts1, t1) (ts2, t2)
    | i /= length ts1 = MM.throwError $ BadTypeAliasParameters v i (length ts1)
    |    MTP.isSubtypeOf t1' t2'
      && MTP.isSubtypeOf t2' t1'
      && length ts1 == length ts2 = return t
    | otherwise = MM.throwError (ConflictingTypeAliases (unresolvedType2type t1) (unresolvedType2type t2))
    where
      t1' = foldl (\t' v' -> ForallU v' t') t1 ts1
      t2' = foldl (\t' v' -> ForallU v' t') t2 ts2

-- | Resolve existentials by choosing the first default type (if it exists)
-- FIXME: How this is done (and why) is of deep relevance to understanding morloc, the decision should not be arbitrary
-- FIXME: And why is it done? Resloving existentials before typechecking seems sketch
chooseExistential :: TypeU -> TypeU
chooseExistential (VarU v) = VarU v
chooseExistential (ExistU _ _ (t:_)) = (chooseExistential t)
chooseExistential (ExistU _ _ []) = error "Existential with no default value"
chooseExistential (ForallU v t) = ForallU v (chooseExistential t)
chooseExistential (FunU ts t) = FunU (map chooseExistential ts) (chooseExistential t)
chooseExistential (AppU t ts) = AppU (chooseExistential t) (map chooseExistential ts)
chooseExistential (NamU o n ps rs) = NamU o n ps [(k, chooseExistential t) | (k,t) <- rs]

-- | Packers need to be passed along with the types the pack, they are imported
-- explicitly with the type and they pack. Should packers be universal? The
-- packers describe how a term may be simplified. But often there are multiple
-- reasonable ways to simplify a term, for example `Map a b` could simplify to
-- `[(a,b)]` or `([a],[b])`. The former is semantically richer (since it
-- naturally maintains the one-to-one variant), but the latter may be more
-- efficient or natural in some languages. For any interface, both sides must
-- adopt the same forms. The easiest way to enforce this is to require one
-- global packer, but ultimately it would be better to resolve packers
-- case-by-base as yet another optimization degree of freedom.
addPackerMap
  :: (DAG MVar [(EVar, EVar)] ExprI)
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
addPackerMap d = do
  maybeDAG <- MDD.synthesizeDAG gatherPackers d
  case maybeDAG of
    Nothing -> MM.throwError CyclicDependency
    (Just d') -> return $ MDD.mapNode fst d'


gatherPackers
  :: MVar -- the importing module name (currently unused)
  -> ExprI -- data about the importing module
  -> [( MVar -- the name of an imported module
      , [(EVar , EVar)]
      , (ExprI, (Map.Map (TVar, Int) [UnresolvedPacker])) -- data about the imported module
     )]
  -> MorlocMonad (ExprI, (Map.Map (TVar, Int) [UnresolvedPacker]))
gatherPackers mv e xs =
  case findPackers e of
    (Left err') -> MM.throwError err'
    (Right m0) -> do
      let m2 = Map.unionsWith (<>) (m0 : [m1 | (_, _, (_, m1)) <- xs])
      attachPackers mv e m2
      return (e, m2)

attachPackers :: MVar -> ExprI -> Map.Map (TVar, Int) [UnresolvedPacker] -> MorlocMonad ()
attachPackers mv e m = do
  s <- MM.get
  let p = GMap.insertMany (collectIndices e) mv m (statePackers s)
  MM.put (s {statePackers = p})

collectIndices :: ExprI -> [Int]
collectIndices (ExprI i e) = i : collectIndicesExpr e

collectIndicesExpr :: Expr -> [Int]
collectIndicesExpr (ModE _ es) = conmap collectIndices es
collectIndicesExpr (AssE _ e es) = collectIndices e <> conmap collectIndices es
collectIndicesExpr (AccE e _) = collectIndices e
collectIndicesExpr (LstE es) = conmap collectIndices es
collectIndicesExpr (TupE es) = conmap collectIndices es
collectIndicesExpr (NamE es) = conmap collectIndices (map snd es)
collectIndicesExpr (AppE e es) = collectIndices e <> conmap collectIndices es
collectIndicesExpr (LamE _ e) = collectIndices e
collectIndicesExpr (AnnE e _) = collectIndices e
collectIndicesExpr _ = []


findPackers :: ExprI -> Either MorlocError (Map.Map (TVar, Int) [UnresolvedPacker])
findPackers expr
  = fmap (Map.fromList . groupSort . map toPackerPair . groupSort)
  $ mapM toPair
    [ (src, t)
    | (alias1, t) <- packers
    , src@(Source _ lang2 _ alias2 _) <- sources
    , alias1 == alias2
    , langOf t == Just lang2
    ]
  where
    sources :: [Source]
    sources = AST.findSources expr

    packers :: [(EVar, EType)]
    packers = [ (v, e)
              | (v, _, e) <- AST.findSignatures expr -- drop the label (eventually this may need to be added back in
              ,  isPacker e || isUnpacker e]

    isPacker :: EType -> Bool
    isPacker e = Set.member Pack (eprop e)

    isUnpacker :: EType -> Bool
    isUnpacker e = Set.member Unpack (eprop e)

    toPackerPair :: ((TVar, Int), [(Property, TypeU, Source)]) -> ((TVar, Int), UnresolvedPacker)
    toPackerPair (k@(v, _), xs) = (,) k $
      UnresolvedPacker
        { unresolvedPackerTerm = Just (EV "Bob") -- TODO: replace this with the general name
        , unresolvedPackerCType = unifyTypes [t | (_, t, _) <- xs, langOf t == langOf v ]
        , unresolvedPackerForward = [src | (Pack, _, src) <- xs]
        , unresolvedPackerReverse = [src | (Unpack, _, src) <- xs]
        }

    toPair :: (Source, EType) -> Either MorlocError ((TVar, Int), (Property, TypeU, Source))
    toPair (src, e) = case packerKeyVal e of
      (Right (Just (key, t, p))) -> return (key, (p, t, src))
      (Right Nothing) -> error "impossible" -- this is called after filtering away general types
      Left err' -> Left err'

    packerKeyVal :: EType -> Either MorlocError (Maybe ((TVar, Int), TypeU, Property))
    packerKeyVal e@(EType t _ _) = case unqualify t of
      (vs, t@(FunU [a] b)) ->  case (isPacker e, isUnpacker e) of
        (True, True) -> Left $ CyclicPacker (qualify vs t)
        (True, False) -> Right (Just ((packerKey b, length vs), qualify vs b, Pack))
        (False, True) -> Right (Just ((packerKey a, length vs), qualify vs a, Unpack))
        (False, False) -> Right Nothing
      (vs, t) -> Left $ IllegalPacker (qualify vs t)

    packerKey :: TypeU -> TVar
    packerKey (VarU v)   = v
    packerKey (AppU (VarU v) _) = v
    packerKey (NamU _ v _ _) = v
    packerKey t = error $ "bad packer: " <> show t

    -- FIXME: this is a place where real user errors will be caught, so needs good error reporting
    unifyTypes :: [TypeU] -> TypeU
    unifyTypes [] = error "impossible" -- This cannot occur since the right hand list accumulated in groupSort is never empty
    unifyTypes (x:_) = x -- FIXME: need to actually check that they all agree


packerTypesMatch :: TypeU -> TypeU -> Bool
packerTypesMatch t1 t2 = case (splitArgs t1, splitArgs t2) of
  ((vs1@[_,_], [t11, t12]), (vs2@[_,_], [t21, t22]))
    -> MTP.equivalent (qualify vs1 t11) (qualify vs2 t22)
    && MTP.equivalent (qualify vs1 t12) (qualify vs2 t21)
  _ -> False

qualify :: [TVar] -> TypeU -> TypeU
qualify [] t = t
qualify (v:vs) t = ForallU v (qualify vs t)

unqualify :: TypeU -> ([TVar], TypeU)
unqualify (ForallU v (unqualify -> (vs, t))) = (v:vs, t)
unqualify t = ([], t)

splitArgs :: TypeU -> ([TVar], [TypeU])
splitArgs (ForallU v u) =
  let (vs, ts) = splitArgs u
  in (v:vs, ts)
splitArgs (FunU ts t) = ([], ts <> [t])
splitArgs t = ([], [t])
