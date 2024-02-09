{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Restructure
Description : Write Module objects to resolve type aliases and such
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Restructure (restructure) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.Map as Map
import qualified Morloc.TypeEval as TE
import qualified Data.Set as Set

-- | Resolve type aliases, term aliases and import/exports
restructure
  :: DAG MVar Import ExprI
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
restructure s
  = checkForSelfRecursion s -- modules should not import themselves
  >>= resolveImports -- rewrite DAG edges to map imported terms to their aliases
  >>= doM collectTypes
  >>= doM collectSources
  >>= evaluateAllTypes
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
  _ <- MDD.mapNodeM (AST.checkExprI isExprSelfRecursive) d
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

    hasTerm _ ExistU{} = error "There should not be existentionals in typedefs"


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


type GCMap = (Scope, Map.Map Lang Scope)

collectTypes :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad ()
collectTypes fullDag = do
  let typeDAG = MDD.mapEdge (\xs -> [(x,y) | AliasedType x y <- xs]) fullDag
  _ <- MDD.synthesizeDAG formTypes typeDAG

  universalGeneralScope <- getUniversalGeneralScope
  universalConcreteScope <- getUniversalConcreteScope universalGeneralScope

  s <- MM.get
  MM.put (s { stateUniversalGeneralTypedefs = universalGeneralScope
            , stateUniversalConcreteTypedefs = universalConcreteScope
            } )

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

    let (generalTypemap, concreteTypemapsIncomplete) = foldl inherit (AST.findTypedefs e0) childImports

    -- Here we are creating links from every indexed term in the module to the module
    -- sources and aliases. When the module abstractions are factored out later,
    -- this will be the only way to access module-specific info.
    let indices = AST.getIndices e0

    -- This step links the general entries from records to their abbreviated
    -- concrete cousins. For example:
    --   record (Person a) = Person {name :: Str, info a}
    --   record Py => Person a = "dict"
    -- This syntax avoids the need to duplicate the entire entry
    let concreteTypemaps = Map.map (completeRecords generalTypemap) concreteTypemapsIncomplete

    s <- MM.get
    MM.put (s { stateGeneralTypedefs = GMap.insertMany indices m generalTypemap (stateGeneralTypedefs s)
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

  getUniversalConcreteScope :: Scope -> MorlocMonad (Map.Map Lang Scope)
  getUniversalConcreteScope gscope = do
    (GMap _ modMaps) <- MM.gets stateConcreteTypedefs
    let langs = concatMap Map.keys . Map.elems $ modMaps
    scopes <- mapM getLangScope langs
    return . Map.fromList $ zip langs scopes
    where
      getLangScope :: Lang -> MorlocMonad Scope
      getLangScope lang = do
        (GMap _ (Map.elems -> langMaps)) <- MM.gets stateConcreteTypedefs
        -- See note above, here we are completing any incomplete concrete
        -- record/table/object types
        let langMaps' = map (Map.map (completeRecords gscope)) langMaps
        return . Map.unionsWith mergeEntries . mapMaybe (Map.lookup lang) $ langMaps'

  completeRecords :: Scope -> Scope -> Scope
  completeRecords gscope cscope = Map.mapWithKey (completeRecord gscope) cscope

  completeRecord :: Scope -> TVar -> [([TVar], TypeU, Bool)] -> [([TVar], TypeU, Bool)]
  completeRecord gscope v xs = case Map.lookup v gscope of
    (Just ys) -> map (completeValue [t | (_, t, _) <- ys]) xs
    Nothing -> xs

  completeValue :: [TypeU] -> ([TVar], TypeU, Bool) -> ([TVar], TypeU, Bool)
  completeValue (NamU _ _ _ rs:_) (vs, NamU o v ps [], terminal) = (vs, NamU o v ps rs, terminal)
  completeValue _ x = x

  getUniversalGeneralScope :: MorlocMonad Scope
  getUniversalGeneralScope = do
    (GMap _ (Map.elems -> scopes)) <- MM.gets stateGeneralTypedefs
    return $ Map.unionsWith mergeEntries scopes


-- merge type functions, names of generics do not matter
mergeEntries :: [([TVar], TypeU, Bool)] -> [([TVar], TypeU, Bool)] -> [([TVar], TypeU, Bool)]
mergeEntries xs0 ys0 = filter (isNovel ys0) xs0 <> ys0
  where
  isNovel :: [([TVar], TypeU, Bool)] -> ([TVar], TypeU, Bool) -> Bool
  isNovel [] _ =  True
  isNovel ((vs2, t2, isTerminal1):ys) x@(vs1, t1, isTerminal2)
    | length vs1 == length vs2 &&
      t1 == foldl (\t (v1, v2) -> rename v2 v1 t) t2 (zip vs1 vs2) &&
      isTerminal1 == isTerminal2 = False
    | otherwise = isNovel ys x

-- clean imports
--   * only keep the exports of a module that are explicitly imported
--   * resolve any aliases
filterAndSubstitute :: [(TVar, TVar)] -> Scope -> Scope
filterAndSubstitute links typemap =
  let importedTypes = Map.filterWithKey (\k _ -> k `elem` map fst links) typemap
  in foldl typeSubstitute importedTypes links
  where
  typeSubstitute
    :: Scope -- imported map
    -> (TVar, TVar) -- source name and local alias
    -> Scope -- renamed map
  typeSubstitute typedefs (sourceName, localAlias)
    = case Map.lookup sourceName typedefs of
      (Just xs) -> Map.insert localAlias (map (\(a,b,c) -> (a, rename sourceName localAlias b, c)) xs) (Map.delete sourceName typedefs)
      Nothing -> typedefs

evaluateAllTypes :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad (DAG MVar [AliasedSymbol] ExprI)
evaluateAllTypes = MDD.mapNodeM f where
  f :: ExprI -> MorlocMonad ExprI
  f (ExprI i e0) = ExprI i <$> g e0 where
    g :: Expr -> MorlocMonad Expr
    g (SigE (Signature v l e)) = do
      gscope <- MM.metaGeneralTypedefs i
      e' <- evaluateEType gscope e
      return $ SigE (Signature v l e')
    g (AnnE e ts) = do
      gscope <- MM.metaGeneralTypedefs i
      ts' <- mapM (evaluateTypeU gscope) ts
      e' <- f e
      return (AnnE e' ts')
    g (ModE m es) = ModE m <$> mapM f es
    g (AssE v e es) = AssE v <$> f e <*> mapM f es
    g (AccE k e) = AccE k <$> f e
    g (LstE es) = LstE <$> mapM f es
    g (TupE es) = TupE <$> mapM f es
    g (NamE rs) = NamE <$> mapM (secondM f) rs
    g (AppE e es) = AppE <$> f e <*> mapM f es
    g (LamE vs e) = LamE vs <$> f e
    g e = return e

    evaluateEType :: Scope -> EType -> MorlocMonad EType
    evaluateEType gscope et =
      either MM.throwError (\t' -> return $ et {etype = t'}) $ TE.evaluateType gscope (etype et)

    evaluateTypeU :: Scope -> TypeU -> MorlocMonad TypeU
    evaluateTypeU gscope t =
      either MM.throwError return $ TE.evaluateType gscope t



collectSources :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad ()
collectSources fullDag = do
  let typeDag = MDD.mapEdge (\xs -> [(x,y) | AliasedType x y <- xs]) fullDag
  _ <- MDD.synthesizeDAG linkSources typeDag
  return ()
  where

  linkSources :: MVar -> ExprI -> a -> MorlocMonad ()
  linkSources m e0 _ = do

    -- collect and store sources (should this be done here?)
    let objSources = AST.findSources e0

    -- Here we are creating links from every indexed term in the module to the module
    -- sources and aliases. When the module abstractions are factored out later,
    -- this will be the only way to access module-specific info.
    let indices = AST.getIndices e0

    s <- MM.get
    MM.put (s { stateSources = GMap.insertManyWith (<>) indices m objSources (stateSources s) } )

    return ()



-- Rename a variable. For example:
--   import maps (Map as HashMap, foo, bar)
--
-- Here all uses `Map` in anything imported from `maps` needs to
-- be renamed to `HashMap`. So we call:
--   rename (TV "Map") (TV "HashMap") x
-- where `x` is any term
rename :: TVar -> TVar -> TypeU -> TypeU
rename sourceName localAlias = f where
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

-- TODO: document
nullify :: DAG m e ExprI -> DAG m e ExprI
nullify = MDD.mapNode f where
    f :: ExprI -> ExprI
    f (ExprI i (SigE (Signature v n (EType t ps cs)))) = ExprI i (SigE (Signature v n (EType (nullifyT t) ps cs)))
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
          . MDD.mapEdge (mapMaybe maybeEVar)
          $ d
  roots -> MM.throwError $ NonSingularRoot roots
  where
    maybeEVar :: AliasedSymbol -> Maybe (EVar, EVar)
    maybeEVar (AliasedTerm x y) = Just (x, y)
    maybeEVar (AliasedType _ _) = Nothing -- remove type symbols, they have already been used
