{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Desugar
Description : Write Module objects to resolve type aliases and such
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Desugar (desugar, desugarType, desugarEType) where

import Morloc.Frontend.Namespace
import Morloc.Pretty ()
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Frontend.PartialOrder as MTP
import Morloc.Typecheck.Internal (qualify, unqualify)

-- | Resolve type aliases, term aliases and import/exports
desugar
  :: DAG MVar Import ExprI
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
desugar s
  = checkForSelfRecursion s -- modules should not import themselves
  >>= resolveImports -- rewrite DAG edges to map imported terms to their aliases
  >>= desugarDag -- substitute type aliases
  |>> nullify
  >>= removeTypeImports -- Remove type imports and exports
  >>= addPackerMap -- add the packers to state

-- | Check for infinitely expanding self-recursive types
--
-- There are cases were the defined term may appear on the right. For example:
--
--   type Py (Tree n e l) = "Tree" n e l
--
-- Here the general type Tree is mapped to the concrete type "Tree" in Python.
-- The fact that the general and concrete names are the same is fine. They are
-- different languages. But what about:
--
--   type (Tree n) = Node n [Tree n]
--
-- This type probably should be legal, but currently it is not supported. Which
-- is why I need to raise an explicit error to avoid infinite loops.
checkForSelfRecursion :: Ord k => DAG k e ExprI -> MorlocMonad (DAG k e ExprI)
checkForSelfRecursion d = do
  MDD.mapNodeM (AST.checkExprI isExprSelfRecursive) d
  return d
  where
    -- A typedef is self-recursive if its name appears in its definition
    isExprSelfRecursive :: ExprI -> MorlocMonad ()
    isExprSelfRecursive (ExprI _ (TypE v@(TV Nothing _) _ t))
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
    hasTerm v (NamU o n (p:ps) []) = hasTerm v p || hasTerm v (NamU o n ps [])
    hasTerm _ (NamU _ _ [] []) = False

    hasTerm _ (ExistU _ _ _ _) = error "There should not be existentionals in typedefs"


-- | Consider export/import information to determine which terms are imported
-- into each module. This step reduces the Import edge type to an alias map.
resolveImports
  :: DAG MVar Import ExprI
  -> MorlocMonad (DAG MVar [AliasedSymbol] ExprI)
resolveImports = MDD.mapEdgeWithNodeAndKeyM resolveImport where
  resolveImport
    :: MVar
    -> ExprI
    -> Import
    -> ExprI
    -> MorlocMonad [AliasedSymbol]
  -- import everything except the excluded and use no aliases
  resolveImport _ _ (Import _ Nothing exc _) n2
    = return
    . map toAliasedSymbol
    . Set.toList
    $ Set.difference (AST.findExportSet n2) (Set.fromList exc)
  -- import only the selected values with (possibly identical) aliases
  resolveImport m1 _ (Import m2 (Just inc) exc _) n2
    | not (null contradict)
        = MM.throwError . ImportExportError m1
        $ "The following terms imported from module '" <> unMVar m2 <> "' are both included and excluded: " <>
          render (tupledNoFold $ map pretty contradict)
    | not (null missing)
        = MM.throwError . ImportExportError m1
        $ "The following imported terms are not exported from module '" <> unMVar m2 <> "': " <>
          render (tupledNoFold $ map pretty missing)
    | otherwise = return inc
    where
      exportSet = AST.findExportSet n2
      -- terms that are imported from n2 but that n2 does not export
      missing = filter (not . (`Set.member` exportSet)) (map unalias inc)
      -- terms that are both included and excluded
      contradict = filter (`elem` exc) (map unalias inc)

  unalias :: AliasedSymbol -> Symbol
  unalias (AliasedType x _) = TypeSymbol x
  unalias (AliasedTerm x _) = TermSymbol x

  toAliasedSymbol :: Symbol -> AliasedSymbol
  toAliasedSymbol (TypeSymbol x) = AliasedType x x
  toAliasedSymbol (TermSymbol x) = AliasedTerm x x

desugarDag
  :: DAG MVar [AliasedSymbol] ExprI
  -> MorlocMonad (DAG MVar [AliasedSymbol] ExprI)
desugarDag m = MDD.mapNodeWithKeyM (desugarExpr m) m

desugarExpr
  :: DAG MVar [AliasedSymbol] ExprI -- ^ The DAG of all modules
  -> MVar -- ^ The name of the current module
  -> ExprI -- ^ The syntax tree for the current module
  -> MorlocMonad ExprI
desugarExpr d k e0 = do
  s <- MM.get

  -- Here we are creating links from every indexed term in the module to the module
  -- sources and aliases. When the module abstractions are factored out later,
  -- this will be the only way to access module-specific info.
  MM.put (s { stateSources = GMap.insertMany indices k objSources (stateSources s)
            , stateTypedefs = GMap.insertMany indices k typedefs (stateTypedefs s) } )

  case mapExprM f e0 of
    (Right x) -> return x
    (Left  e) -> MM.throwError e
  where

  f :: Expr -> Either MorlocError Expr
  f (SigE v l t) = SigE v l <$> desugarEType termmap t
  f (AnnE e ts) = AnnE e <$> mapM (desugarType termmap) ts
  f e = return e

  objSources = [src | src <- AST.findSources e0]

  -- Find all type terms used in this module
  -- These are the terms that may need alias expansion
  terms :: [TVar]
  terms = AST.findSignatureTypeTerms e0

  -- Map of type variables to their definitions/aliases (TypE terms)
  -- This includes simple aliases, such as:
  --   type Foo = Int
  -- As well as complex data declarations, such as:
  --   object (Person a) = Person {name :: Str, age :: Int}
  termmap :: Map.Map TVar [([TVar], TypeU)]
  termmap = Map.fromList $ zip terms (map lookupTypedefs terms)

  lookupTypedefs
    :: TVar
    -> [([TVar], TypeU)]
  lookupTypedefs (TV lang v)
    = catMaybes
    -- Maybe ([TVar], TypeU)
    . MDD.nodes
    -- DAG MVar None (Maybe ([TVar], TypeU))
    . MDD.mapNode (\(v', typemap) -> Map.lookup (TV lang v') typemap)
    -- DAG MVar None (Text, Map.Map TVar ([TVar], TypeU))
    . MDD.lookupAliasedTerm v k AST.findTypedefs
    -- DAG MVar [(Text,Text)] ExprI
    . MDD.mapEdge (\xs -> [(x,y) | AliasedType x y <- xs])
    -- DAG MVar [AliasedSymbol] ExprI
    $ d

  indices = AST.getIndices e0

  typedefs :: Map.Map TVar [([TVar], TypeU)]
  typedefs 
    = Map.fromList
    . groupSort
    . (<>) (Map.toList $ AST.findTypedefs e0)
    . concatMap
      ( (\(v, es) -> [(TV (langOf t) v, (vs, t)) | (vs, t) <- es])
      . second (concatMap (uncurry lookupName) . MDD.nodes)
      )
    . MDD.inherit k AST.findTypedefs
    . MDD.mapEdge (\es -> [(a, b) | AliasedType a b <- es])
    $ d

  lookupName :: MT.Text -> Map.Map TVar a -> [a] 
  lookupName k1 m = [x | (TV _ k2, x) <- Map.toList m, k1 == k2]


desugarEType :: Map.Map TVar [([TVar], TypeU)] -> EType -> Either MorlocError EType
desugarEType h (EType t ps cs) = EType <$> desugarType h t <*> pure ps <*> pure cs

desugarType
  :: Map.Map TVar [([TVar], TypeU)]
  -> TypeU
  -> Either MorlocError TypeU
desugarType h = f
  where

  f :: TypeU -> Either MorlocError TypeU

  --   (Just []) -> return (t0, [])
  --   (Just ts'@(t':_)) -> do
  --     (_, t) <- foldlM (mergeAliases v 0) t' ts'
  --     f t
  --   Nothing -> MM.throwError . CallTheMonkeys $ "Type term in VarU missing from type map"
  f (ExistU v ps ds rs) = do
    ps' <- mapM f ps
    ds' <- mapM f ds
    rs' <- mapM (\(k,v) -> (,) k <$> f v) rs
    return $ ExistU v ps' ds' rs'
  f (FunU ts t) = FunU <$> mapM f ts <*> f t
  f (NamU o n ps rs) = do
    ts <- mapM (f . snd) rs
    ps' <- mapM f ps
    return $ NamU o n ps' (zip (map fst rs) ts)

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
  f (AppU (VarU v) ts) =
    case Map.lookup v h of
      (Just (t':ts')) -> do
        (vs, t) <- foldlM (mergeAliases v (length ts)) t' ts'
        if length ts == length vs
          -- substitute parameters into alias
          then f (foldr parsub (chooseExistential t) (zip vs (map chooseExistential ts)))
          else MM.throwError $ BadTypeAliasParameters v (length vs) (length ts)
      -- default types like "Int" or "Tuple2" won't be in the map
      _ -> AppU (VarU v) <$> mapM f ts

  -- Can only apply VarU? Will need to fix this when we get lambdas.
  f (AppU _ _) = undefined

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
  parsub _ (ExistU _ _ _ _) = error "What the bloody hell is an existential doing down here?"
  parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
  parsub pair (FunU ts t) = FunU (map (parsub pair) ts) (parsub pair t)
  parsub pair (AppU t ts) = AppU (parsub pair t) (map (parsub pair) ts)
  parsub pair (NamU o n ps rs) = NamU o n (map (parsub pair) ps) [(k', parsub pair t) | (k', t) <- rs]


  -- When a type alias is imported from two places, this function reconciles them, if possible
  mergeAliases
    :: TVar
    -> Int
    -> ([TVar], TypeU)
    -> ([TVar], TypeU)
    -> Either MorlocError ([TVar], TypeU)
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
chooseExistential (ExistU _ _ _ (_:_)) = error "Existentials with keys cannot be resolved yet"
chooseExistential (ExistU _ _ (t:_) _) = chooseExistential t
chooseExistential (ExistU _ _ [] _) = error "Existential with no default value"
chooseExistential (ForallU v t) = ForallU v (chooseExistential t)
chooseExistential (FunU ts t) = FunU (map chooseExistential ts) (chooseExistential t)
chooseExistential (AppU t ts) = AppU (chooseExistential t) (map chooseExistential ts)
chooseExistential (NamU o n ps rs) = NamU o n (map chooseExistential ps) [(k, chooseExistential t) | (k,t) <- rs]

nullify :: DAG m e ExprI -> DAG m e ExprI
nullify = MDD.mapNode f where
    f :: ExprI -> ExprI
    f (ExprI i (SigE v n (EType t ps cs))) = ExprI i (SigE v n (EType (nullifyT t) ps cs))
    f (ExprI i (ModE m es)) = ExprI i (ModE m (map f es))
    f (ExprI i (AssE v e es)) = ExprI i (AssE v (f e) (map f es))
    f e = e

    nullifyT :: TypeU -> TypeU
    nullifyT (FunU ts t) = FunU (filter (not . isNull) (map nullifyT ts)) (nullifyT t)
    nullifyT (ExistU v ts ds rs) = ExistU v (map nullifyT ts) (map nullifyT ds) (map (second nullifyT) rs)
    nullifyT (ForallU v t) = ForallU v (nullifyT t)
    nullifyT (AppU t ts) = AppU (nullifyT t) (map nullifyT ts)
    nullifyT (NamU o v ds rs) = NamU o v (map nullifyT ds) (map (second nullifyT) rs)
    nullifyT t = t


    isNull :: TypeU -> Bool
    isNull (ExistU _ _ (t:_) _) = t `elem` MLD.defaultNull (langOf t)
    isNull t = t `elem` MLD.defaultNull (langOf t)


removeTypeImports :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
removeTypeImports d = case MDD.roots d of
  [root] -> return 
          . MDD.shake root
          . MDD.filterEdge filterEmpty
          . MDD.mapEdge (mapMaybe maybeEVar)
          $ d
  roots -> MM.throwError $ NonSingularRoot roots
  where 
    maybeEVar :: AliasedSymbol -> Maybe (EVar, EVar)
    maybeEVar (AliasedTerm x y) = Just (EV x, EV y)
    maybeEVar (AliasedType _ _) = Nothing -- remove type symbols, they have already been used

    filterEmpty :: k -> n -> k -> [a] -> Bool
    filterEmpty _ _ _ [] = False
    filterEmpty _ _ _ _ = True

-- | Packers need to be passed along with the types they pack, they are imported
-- explicitly with the type they pack. Should packers be universal? The
-- packers describe how a term may be simplified. But often there are multiple
-- reasonable ways to simplify a term, for example `Map a b` could simplify to
-- `[(a,b)]` or `([a],[b])`. The former is semantically more precise (since it
-- naturally maintains the one-to-one variant), but the latter may be more
-- efficient or natural in some languages. For any interface, both sides must
-- adopt the same forms. The easiest way to enforce this is to require one
-- global packer, but ultimately it would be better to resolve packers
-- case-by-base as yet another optimization degree of freedom.
--
-- There is another case where multiple packers may make sense. Some types, such
-- as the generic "Tree node edge leaf" type may not be so generic in idiomatic
-- representations in some languages. For the phylogenetics case study in the
-- ICFL2023 paper, the C++ Tree type is fully generic, but the R type is a
-- "phylo", where edges are branch lengths (numeric) and nodes and leafs are
-- text labels. Thus the "phylo" type in R is a specialized Tree. This should be
-- supported. There may be multiple concrete types representing a given general
-- types. So we should also not unify to a single packer. What will be constant
-- is the number of type parameters. So the "phylo" type, if it is an instance
-- of Tree, will be represented as ("phylo" "character" "numeric"
-- "character"). Like Tree, it takes 3 parameters, but unlike Tree, they are not
-- generic. If any use of an R function of "phylo" with different type
-- parameters will fail at compile time, since there is no path to synthesizing
-- such a type.
addPackerMap
  :: DAG MVar [(EVar, EVar)] ExprI
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
      , (ExprI, Map.Map TVar [UnresolvedPacker]) -- data about the imported module
     )]
  -> MorlocMonad (ExprI, Map.Map TVar [UnresolvedPacker])
gatherPackers mv e xs =
  case findPackers e of
    (Left err') -> MM.throwError err'
    (Right m0) -> do
      let m2 = Map.unionsWith (<>) (m0 : [m1 | (_, _, (_, m1)) <- xs])
      attachPackers mv e m2
      return (e, m2)

attachPackers :: MVar -> ExprI -> Map.Map TVar [UnresolvedPacker] -> MorlocMonad ()
attachPackers mv e m = do
  s <- MM.get
  let p = GMap.insertMany (AST.getIndices e) mv m (statePackers s)
  MM.put (s {statePackers = p})

-- | This function should 1) couple matching pack/unpack functions, 2) find the
-- general types for the packed and unpacked forms, and 3) tie all this info
-- together
findPackers :: ExprI -> Either MorlocError (Map.Map TVar [UnresolvedPacker])
findPackers expr
  = Map.fromList
  . groupSort
  . map (second toPackerPair)
  . groupSort
  <$> mapM toPair
    [ (alias1, src, t)
    | (alias1, t) <- packers
    , src@(Source _ lang2 _ alias2 _) <- sources
    , alias1 == alias2
    , langOf t == Just lang2
    ]

  where

  sources :: [Source]
  sources = AST.findSources expr

  generalMap
    = Map.fromList
    . mapMaybe (\(l, ts) -> (,) l <$> unifyGeneralTypes ts)
    . groupSort
    $ [ (v, t)
      | (v, t) <- packers
      , isNothing (langOf t)
      ]

  unifyGeneralTypes :: [EType] -> Maybe (TypeU, TypeU) -- (packed type, unpacked type)
  unifyGeneralTypes (x@(unqualify . etype -> (vs, FunU [a] b)):_)
      | isPacker x   = Just (qualify vs b, qualify vs a)
      | isUnpacker x = Just (qualify vs a, qualify vs b)
  unifyGeneralTypes _ = Nothing

  -- pulls out the packed type name, this key is used to group together all the
  -- packers for that language-specific type
  toPair :: (EVar, Source, EType) -> Either MorlocError (TVar, (EVar, Property, TypeU, TypeU, Source))
  toPair (fname, src, e) = case packerKeyVal e of
    (Right (Just (key, packedType, unpackedType, p))) -> return (key, (fname, p, packedType, unpackedType, src))
    (Right Nothing) -> error "impossible" -- this is called after filtering away general types
    Left err' -> Left err'

  packerKeyVal :: EType -> Either MorlocError (Maybe (TVar, TypeU, TypeU, Property))
  packerKeyVal e@(EType t0 _ _) = case unqualify t0 of
    (vs, t@(FunU [a] b)) ->  case (isPacker e, isUnpacker e) of
      (True, True) -> Left $ CyclicPacker (qualify vs t)
      (True, False) -> Right (Just (packerKey b, qualify vs b, qualify vs a, Pack))
      (False, True) -> Right (Just (packerKey a, qualify vs a, qualify vs b, Unpack))
      (False, False) -> Right Nothing
    (vs, t) -> Left $ IllegalPacker (qualify vs t)

  packerKey :: TypeU -> TVar
  packerKey (VarU v)   = v
  packerKey (AppU (VarU v) _) = v
  packerKey (NamU _ v _ _) = v
  packerKey t = error $ "bad packer: " <> show t

  toPackerPair :: [(EVar, Property, TypeU, TypeU, Source)] -> UnresolvedPacker
  toPackerPair xs =
    let (genMay, packedType, unpackedType) = unifyTypes [(f, t, u) | (f, _, t, u, _) <- xs]
    in UnresolvedPacker
         { unresolvedPackerTerm = Nothing
         , unresolvedPackedType = packedType
         , unresolvedUnpackedType = unpackedType
         , unresolvedPackerForward = [src | (_, Pack, _, _, src) <- xs]
         , unresolvedPackerReverse = [src | (_, Unpack, _, _, src) <- xs]
         , unresolvedPackerGeneralTypes = genMay
         }

  packers :: [(EVar, EType)]
  packers = [ (v, e)
            | (v, _, e) <- AST.findSignatures expr
            ,  isPacker e || isUnpacker e
            ]

  isPacker :: EType -> Bool
  isPacker e = Set.member Pack (eprop e)

  isUnpacker :: EType -> Bool
  isUnpacker e = Set.member Unpack (eprop e)

  -- FIXME: this is a place where real user errors will be caught, so needs good error reporting
  unifyTypes :: [(EVar, TypeU, TypeU)] -> (Maybe (TypeU, TypeU), TypeU, TypeU)
  unifyTypes [] = error "impossible" -- This cannot occur since the right hand list accumulated in groupSort is never empty
  unifyTypes [(_, p, u)] = (Nothing, p, u)
  unifyTypes ((f, p, u):xs) = case Map.lookup f generalMap of
    Nothing -> unifyTypes xs
    mayGen -> (mayGen, p, u)
