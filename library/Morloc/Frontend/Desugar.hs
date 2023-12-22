{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Desugar
Description : Write Module objects to resolve type aliases and such
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Desugar (desugar, evaluateType, evaluateEType) where

import Morloc.Frontend.Namespace
import Morloc.Pretty ()
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.Map as Map
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
  >>= doM collectTypeFunctions
  >>= addPackerMap -- add the packers to state
  >>= removeTypeImports -- Remove type imports and exports
  |>> nullify -- TODO: unsus and document


doM :: Monad m => (a -> m ()) -> a -> m a
doM f x = f x >> return x

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
    isExprSelfRecursive (ExprI _ (TypE _ v _ t))
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

    hasTerm _ (ExistU _ _ _) = error "There should not be existentionals in typedefs"


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


type GCMap = (               Map.Map TVar [([TVar], TypeU)]  -- child general map
             , Map.Map Lang (Map.Map TVar [([TVar], TypeU)]) -- child concrete ap
             )

collectTypeFunctions :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad ()
collectTypeFunctions fullDag = do
  let typeDAG = MDD.mapEdge (\xs -> [(x,y) | AliasedType x y <- xs]) fullDag
  _ <- MDD.synthesizeDAG formTypes typeDAG
  return ()
  where

  formTypes
    :: MVar
    -> ExprI
    -> [( MVar -- child module name
        , [(TVar, TVar)] -- alias map
        , GCMap
        )]
    -> MorlocMonad GCMap
  formTypes m e0 childImports = do

    let (generalTypemap, concreteTypemaps) = foldl inherit (AST.findTypedefs e0) childImports

    -- collect and store sources (should this be done here?)
    let objSources = [src | src <- AST.findSources e0]

    -- Here we are creating links from every indexed term in the module to the module
    -- sources and aliases. When the module abstractions are factored out later,
    -- this will be the only way to access module-specific info.
    let indices = AST.getIndices e0

    s <- MM.get
    MM.put (s { stateSources = GMap.insertMany indices m objSources (stateSources s)
              , stateGeneralTypedefs = GMap.insertMany indices m generalTypemap (stateGeneralTypedefs s)
              , stateConcreteTypedefs = GMap.insertMany indices m concreteTypemaps (stateConcreteTypedefs s)
              } )

    return (generalTypemap, concreteTypemaps)

  inherit :: GCMap -> (key, [(TVar, TVar)], GCMap) -> GCMap
  inherit (thisGmap, thisCmap) (_, links, (gmap, cmap)) =
    let gmap' = filterAndSubstitute links gmap
        cmap' = Map.map (filterAndSubstitute links) cmap
    in ( Map.unionWith mergeEntries gmap' thisGmap
       , Map.unionWith (Map.unionWith mergeEntries) cmap' thisCmap
       )

-- merge type functions, names of generics do not matter
mergeEntries :: [([TVar], TypeU)] -> [([TVar], TypeU)] -> [([TVar], TypeU)]
mergeEntries xs0 ys0 = filter (isNovel ys0) xs0 <> ys0
  where
  isNovel :: [([TVar], TypeU)] -> ([TVar], TypeU) -> Bool
  isNovel [] _ =  True
  isNovel ((vs2, t2):ys) x@(vs1, t1)
    | length vs1 == length vs2 &&
      t1 == foldl (\t (v1, v2) -> rename v2 v1 t) t2 (zip vs1 vs2) = False
    | otherwise = isNovel ys x

-- clean imports
--   * only keep the exports of a module that are explicitly imported
--   * resolve any aliases
filterAndSubstitute
  :: [(TVar, TVar)]
  -> Map.Map TVar [([TVar], TypeU)]
  -> Map.Map TVar [([TVar], TypeU)]
filterAndSubstitute links typemap =
  let importedTypes = Map.filterWithKey (\k _ -> k `elem` map fst links) typemap
  in foldl typeSubstitute importedTypes links
  where
  typeSubstitute
    :: Map.Map TVar [([TVar], TypeU)] -- imported map
    -> (TVar, TVar) -- source name and local alias
    -> Map.Map TVar [([TVar], TypeU)] -- renamed map
  typeSubstitute typedefs (sourceName, localAlias)
    = case Map.lookup sourceName typedefs of
      (Just xs) -> Map.insert localAlias (map (second (rename sourceName localAlias)) xs) (Map.delete sourceName typedefs)
      Nothing -> typedefs

-- Rename a variable. For example:
--   import maps (Map as HashMap, foo, bar)
--
-- Here all uses `Map` in anything imported from `maps` needs to 
-- be renamed to `HashMap`. So we call:
--   rename (TV "Map") (TV "HashMap") x
-- where `x` is any term
rename :: TVar -> TVar -> TypeU -> TypeU
rename sourceName localAlias t0
  | sourceName == localAlias = t0
  | otherwise = f t0 where
      f (VarU v)
        | v == sourceName = VarU localAlias
        | otherwise = VarU v
      f (ExistU v ts rs)
        | v == sourceName = ExistU localAlias ts rs
        | otherwise = ExistU v ts rs
      f (ForallU v t) = ForallU v (f t)
      f (FunU ts t) = FunU (map f ts) (f t)
      f (AppU t ts) = AppU (f t) (map f ts)
      f (NamU o v ts rs) = NamU o v (map f ts) (map (second f) rs)


evaluateEType :: Map.Map TVar [([TVar], TypeU)] -> EType -> Either MorlocError EType
evaluateEType h (EType t ps cs) = EType <$> evaluateType h t <*> pure ps <*> pure cs


evaluateType
  :: Map.Map TVar [([TVar], TypeU)]
  -> TypeU
  -> Either MorlocError TypeU
evaluateType h = f Set.empty
  where

  f :: Set.Set TVar -> TypeU -> Either MorlocError TypeU
  f bnd (ExistU v ps rs) = do
    ps' <- mapM (f bnd) ps
    rs' <- mapM (\(k,v) -> (,) k <$> f bnd v) rs
    return $ ExistU v ps' rs'
  f bnd (FunU ts t) = FunU <$> mapM (f bnd) ts <*> f bnd t
  f bnd (NamU o n ps rs) = do
    (n', o') <- case Map.lookup n h of
        -- If the record type itself is aliased, substitute the name and record form
        (Just [(_, NamU o'' n'' _ _)]) -> return (n'', o'')
        -- Otherwise, keep the record name and form and recurse only into children
        _ -> return (n, o)
    ts <- mapM (f bnd . snd) rs
    ps' <- mapM (f bnd) ps
    return $ NamU o' n' ps' (zip (map fst rs) ts)

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
  f bnd (AppU (VarU v) ts)
    | Set.member v bnd = AppU (VarU v) <$> mapM (f bnd) ts
    | otherwise =
        case Map.lookup v h of
          (Just (t':ts')) -> do
            (vs, t) <- foldlM (mergeAliases v (length ts)) t' ts' |>> renameTypedefs bnd
            if length ts == length vs
              -- substitute parameters into alias
              then f bnd (foldr parsub t (zip vs ts))
              else MM.throwError $ BadTypeAliasParameters v (length vs) (length ts)
          -- default types like "Int" or "Tuple2" won't be in the map
          _ -> AppU (VarU v) <$> mapM (f bnd) ts

  -- Can only apply VarU? Will need to fix this when we get lambdas.
  f _ (AppU _ _) = undefined

  -- type Foo = A
  -- f :: Foo -> B
  -- -----------------
  -- f :: A -> B
  f bnd t0@(VarU v)
    | Set.member v bnd = return t0
    | otherwise =
     case Map.lookup v h of
      (Just []) -> return t0
      (Just ts1@(t1:_)) -> do
        (_, t2) <- foldlM (mergeAliases v 0) t1 ts1
        f bnd t2
      Nothing -> return t0

  f bnd (ForallU v t) = ForallU v <$> f (Set.insert v bnd) t

  renameTypedefs :: Set.Set TVar -> ([TVar], TypeU) -> ([TVar], TypeU)
  renameTypedefs _ ([], t) = ([], t)
  renameTypedefs bnd (v@(TV x) : vs, t)
    | Set.member v bnd =
        let (vs', t') = renameTypedefs bnd (vs, t)
            v' = head [x' | x' <- [TV (MT.show' i <> x) | i <- [0..]], not (Set.member x' bnd), not (elem x' vs')]  
            t'' = substituteTVar v (VarU v') t'
        in (v':vs', t'')
    | otherwise = 
        let (vs', t') = renameTypedefs bnd (vs, t)
        in (v:vs', t')

  parsub :: (TVar, TypeU) -> TypeU -> TypeU
  parsub (v, t2) t1@(VarU v0)
    | v0 == v = t2 -- substitute
    | otherwise = t1 -- keep the original
  parsub _ (ExistU _ _ _) = error "What the bloody hell is an existential doing down here?"
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
      t1' = foldl (flip ForallU) t1 ts1
      t2' = foldl (flip ForallU) t2 ts2


-- TODO: document
nullify :: DAG m e ExprI -> DAG m e ExprI
nullify = MDD.mapNode f where
    f :: ExprI -> ExprI
    f (ExprI i (SigE v n (EType t ps cs))) = ExprI i (SigE v n (EType (nullifyT t) ps cs))
    f (ExprI i (ModE m es)) = ExprI i (ModE m (map f es))
    f (ExprI i (AssE v e es)) = ExprI i (AssE v (f e) (map f es))
    f e = e

    nullifyT :: TypeU -> TypeU
    nullifyT (FunU ts t) = FunU (filter (not . isNull) (map nullifyT ts)) (nullifyT t)
    nullifyT (ExistU v ts rs) = ExistU v (map nullifyT ts) (map (second nullifyT) rs)
    nullifyT (ForallU v t) = ForallU v (nullifyT t)
    nullifyT (AppU t ts) = AppU (nullifyT t) (map nullifyT ts)
    nullifyT (NamU o v ds rs) = NamU o v (map nullifyT ds) (map (second nullifyT) rs)
    nullifyT t = t


    isNull :: TypeU -> Bool
    isNull t = t == BT.unitU


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
    maybeEVar (AliasedTerm x y) = Just (x, y)
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
-- generic. So any use of an R function of "phylo" with different type
-- parameters will fail at compile time, since there is no path to synthesizing
-- such a type.
addPackerMap
  :: DAG MVar edge ExprI
  -> MorlocMonad (DAG MVar edge ExprI)
addPackerMap = undefined
-- addPackerMap d = do
--   maybeDAG <- MDD.synthesizeDAG gatherPackers d
--   case maybeDAG of
--     Nothing -> MM.throwError CyclicDependency
--     (Just d') -> return $ MDD.mapNode fst d'
--
--
-- gatherPackers
--   :: MVar -- the importing module name (currently unused)
--   -> ExprI -- data about the importing module
--   -> [( MVar -- the name of an imported module
--       , edge
--       , (ExprI, Map.Map CVar [UnresolvedPacker]) -- data about the imported module
--      )]
--   -> MorlocMonad (ExprI, Map.Map CVar [UnresolvedPacker])
-- gatherPackers mv e xs = do
--   (GMap _ mvmap) <- MM.gets stateConcreteTypedefs
--   let typedefs = fromMaybe Map.empty (Map.lookup mv mvmap)
--   MM.sayVVV $ "typedefs: " <> viaShow typedefs
--   case findPackers typedefs e of
--     (Left err') -> MM.throwError err'
--     (Right m0) -> do
--       MM.sayVVV $ "found packers in module" <+> pretty mv <> ":" <+> viaShow m0
--       let m2 = Map.unionsWith (<>) (m0 : [m1 | (_, _, (_, m1)) <- xs])
--       attachPackers mv e m2
--       return (e, m2)
--
-- attachPackers :: MVar -> ExprI -> Map.Map CVar [UnresolvedPacker] -> MorlocMonad ()
-- attachPackers mv e m = do
--   s <- MM.get
--   let p = GMap.insertMany (AST.getIndices e) mv m (statePackers s)
--   MM.put (s {statePackers = p})
--
--
-- -- | This function should:
-- --   1) couple matching pack/unpack functions
-- --   2) find the general types for the packed and unpacked forms
-- --   3) infer concrete types from general types when needed (and possible)
-- --   4) tie all this info together
-- findPackers :: Map.Map CVar [([TVar], TypeU)] -> ExprI -> Either MorlocError (Map.Map CVar [UnresolvedPacker])
-- findPackers typedefs expr =
--   mapM (linkTypes types) (AST.findSources expr)
--   |>> catMaybes
--   |>> Map.fromListWith (<>)
--   >>= Map.mapM mergePackers
--
--   where
--
--   -- find all packer and unpacker signatures
--   types :: Map.Map (EVar, Bool) EType
--   types = Map.fromList
--             [ ((v, isPacker e), e)
--             | (v, _, e) <- AST.findSignatures expr
--             , isPacker e || isUnpacker e
--             ]
--
--   isPacker :: EType -> Bool
--   isPacker e = Set.member Pack (eprop e)
--
--   isUnpacker :: EType -> Bool
--   isUnpacker e = Set.member Unpack (eprop e)
--
--   linkTypes :: Map.Map (EVar, Bool) EType
--             -> Source
--             -> Either MorlocError (Maybe (CVar, [UnresolvedPacker]))
--   linkTypes typemap src = do
--     let generalPacker = Map.lookup (srcAlias src, True) typemap
--         generalUnpacker = Map.lookup (srcAlias src, False) typemap
--     case (generalPacker, generalUnpacker) of
--       -- this is not a packer or unpacker
--       (Nothing, Nothing) -> Right Nothing
--       -- the term is either a packer or an unpacker
--       (Just gp, Nothing) -> Just <$> makePacker True src (Just $ etype gp) Nothing
--       (Nothing, Just gu) -> Just <$> makePacker False src (Just $ etype gu) Nothing
--       -- illegal cyclical cases where a term is both
--       (Just p, Just u) -> Left $ CyclicPacker (etype p) (etype u)
--
--   makePacker :: Bool -> Source -> Maybe TypeU -> Maybe TypeU -> Either MorlocError (CVar, [UnresolvedPacker])
--   makePacker ispacker src maygt mayct = do
--     gpair <- makeGeneralPair ispacker maygt
--     packerTerm <- case gpair of
--       (Just (pt, _)) -> generalKey pt
--       Nothing -> return Nothing
--     (packedType, unpackedType) <- case (maygt, mayct) of
--       (_, Just ct) -> makeConcretePair ispacker ct
--       (Just _, _) -> evaluateType typedefs gt >>= makeConcretePair ispacker
--       (Nothing, Nothing) -> error "This case should be unreachable"
--     key <- packerKey packedType
--     let (packerForward, packerReverse) = makeSources ispacker src
--         lang = srcLang src
--     return (CV lang key, [UnresolvedPacker
--         { unresolvedPackerLang = lang
--         , unresolvedPackerTerm = packerTerm
--         , unresolvedPackedType = packedType
--         , unresolvedUnpackedType = unpackedType
--         , unresolvedPackerForward = packerForward
--         , unresolvedPackerReverse = packerReverse
--         , unresolvedPackerGeneralTypes = gpair
--         }]
--       )
--
--   makeGeneralPair :: Bool -> Maybe TypeU -> Either MorlocError (Maybe (TypeU, TypeU))
--   makeGeneralPair ispacker (fmap unqualify -> Just (vs, FunU [a] b))
--     | ispacker = Right $ Just (qualify vs b, qualify vs a)
--     | otherwise = Right $ Just (qualify vs a, qualify vs b)
--   makeGeneralPair _ Nothing = Right Nothing
--   makeGeneralPair _ (Just t) = error $ show t
--
--   -- make the forward and reverse sources
--   -- e.g.: ([Source "packMap" ...], [])   -- for True
--   -- e.g.: ([], [Source "unpackMap" ...]) -- for False
--   makeSources :: Bool -> Source -> ([Source], [Source])
--   makeSources True src = ([src], [])
--   makeSources False src = ([], [src])
--
--   -- make concrete pair with packed and unpacked forms, respectively
--   -- e.g.: ("dict", ([a],[b]))
--   makeConcretePair :: Bool -> TypeU -> Either MorlocError (TypeU, TypeU)
--   makeConcretePair ispacker (unqualify -> (vs, FunU [a] b))
--     | ispacker = return (qualify vs b, qualify vs a)
--     | otherwise = return (qualify vs a, qualify vs b)
--   makeConcretePair _ t = error $ show t
--
--   -- make the concrete packer key
--   -- e.g.: "dict", the Python form of "Map"
--   packerKey :: TypeU -> Either MorlocError TVar
--   packerKey (unqualify -> (_, t0)) = f t0 where
--     f (VarU v)   = return v
--     f (AppU (VarU v) _) = return v
--     f (NamU _ v _ _) = return v
--     f t = error $ show t
--
--   -- make the general key
--   -- e.g.: "Map"
--   generalKey :: TypeU -> Either MorlocError (Maybe EVar)
--   generalKey (unqualify -> (_, t0)) = f t0 where
--     f (VarU (TV v))   = return (Just (EV v))
--     f (AppU (VarU (TV v)) _) = return (Just (EV v))
--     f (NamU _ (TV v) _ _) = return (Just (EV v))
--     f t = error $ show t
--
--   -- associate packers with unpackers to make final UnresolvedPacker objects
--   mergePackers :: [UnresolvedPacker] -> Either MorlocError [UnresolvedPacker]
--   mergePackers ps
--     = mapM (foldl1M mergeUnresolvedPackers . snd)
--     . groupSort
--     $ [(unresolvedPackerTerm p, p) | p <- ps]
--
--   -- Here we merge partial UnresolvedPackers. These will all be for the same
--   -- general type (e.g., Map) and the same concrete type (e.g. "dict"). Ideally,
--   -- there should be exactly two elements in the list, a packer and an unpacker.
--   mergeUnresolvedPackers :: UnresolvedPacker -> UnresolvedPacker -> Either MorlocError UnresolvedPacker
--   mergeUnresolvedPackers p1 p2 =
--       return $ p1
--         { unresolvedPackerForward = unresolvedPackerForward p1 <> unresolvedPackerForward p2
--         , unresolvedPackerReverse = unresolvedPackerReverse p1 <> unresolvedPackerReverse p2
--         }
