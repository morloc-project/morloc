{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.LinkTerms
Description : Transfer all term types and sources to state
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.LinkTerms (linkTerms) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import qualified Morloc.Frontend.AST as AST
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.GMap as GMap
import Morloc.Frontend.Merge (mergeTermTypes, mergeEType, mergeSignatureSet)


{-
                ,--n-> Source <-m--n-> Concrete Signature
term --<i>--.--<
             \  `--m-> Declaration
              `------------------------n--> General Signature
-}

linkTerms :: DAG MVar [(EVar, EVar)] ExprI -> MorlocMonad ()
linkTerms d0 = DAG.foldNeighborsWithTermsM makeAcc linkSources terminator d0 ()
  where

  -- start with an empty state
  makeAcc _ k = case Map.lookup k d0 of
    (Just (e0, edges)) -> do
        let importedTerms = concat $ map (map snd . snd) edges

        -- these are the types that are present in the current module, they will
        -- all be annotated as we go forward
        mOut <- collectTermTypes k e0 >>=
                Map.unionWithM mergeTermTypes (getAllFreeTerms e0 importedTerms)
        return (k, Map.empty, k, mOut)
    Nothing -> return (k, Map.empty, k, Map.empty)

  -- after finishing processing this module, link the resulting map to MorlocState
  terminator :: (MVar, Map.Map EVar TermTypes, MVar, Map.Map EVar TermTypes) -> MorlocMonad ()
  terminator (_, _, k, m) = do
    iterms <- Map.mapM indexTerm m
    case Map.lookup k d0 of
        (Just (e, _)) -> linkVariablesToTermTypes k iterms e
        _ -> return ()

  linkSources
    :: MVar
    -> ExprI
    -> EVar -- term name (or alias) in the original module
    -> EVar -- term name in the current module
    -> ( MVar -- the current module name
       , Map.Map EVar TermTypes -- the current module term info
       , MVar -- the original module that we are collecting data for
       , Map.Map EVar TermTypes -- the term info that is updated across this traversal
       )
    -> MorlocMonad (MVar, Map.Map EVar TermTypes, MVar, Map.Map EVar TermTypes)
  linkSources k e0 originalTerm currentAlias s@(kCur, mCur, kOri, mOri)
    -- do nothing if we are at the original node
    | k == kOri = do
        MM.sayVVV $ ">>> starting" <+> pretty kOri <+> pretty kCur <+> pretty k <+> pretty originalTerm <+> pretty currentAlias
        return s

    -- if we have just entered a new node, then collect all info and re-enter
    | k /= kCur = do
        MM.sayVVV $ ">>> entering" <+> pretty kOri <+> pretty kCur <+> pretty k <+> pretty originalTerm <+> pretty currentAlias
        mCur' <- collectTermTypes k e0
        linkSources k e0 originalTerm currentAlias (k, mCur', kOri, mOri)

    -- if we are in a non-origin node and have info, then do the work
    | otherwise = do
        MM.sayVVV $ ">>> neighbor" <+> pretty kOri <+> pretty kCur <+> pretty k <+> pretty originalTerm <+> pretty currentAlias
        case (Map.lookup currentAlias mCur, Map.lookup originalTerm mOri) of
            (Just tCur, Just tOut) -> do
                t <- mergeTermTypes tCur tOut
                return (kCur, mCur, kOri, Map.insert originalTerm t mOri)
            _ -> return s


-- | find all free terms in the expression and the list of imports, return empty TermTypes records
getAllFreeTerms :: ExprI -> [EVar] -> Map.Map EVar TermTypes
getAllFreeTerms e0 imports = Map.fromList $ [(v, TermTypes Nothing [] []) | v <- (f e0 <> imports)]
    where
        f (ExprI _ (ModE _ es)) = concatMap f es
        f (ExprI _ (IstE _ _ es)) = concatMap f es
        f (ExprI _ (ImpE imp)) = case importInclude imp of
            (Just ss) -> [alias | AliasedTerm _ alias <- ss]
            Nothing -> []
        f (ExprI _ (SrcE src)) = [srcAlias src]
        -- within where statements, do not include anything that is locally bound
        f (ExprI _ (AssE v e es)) = v : (f e <> nonlocalWhere) where
            nonlocalWhere = [v | v <- (concatMap f es), v `notElem` (concatMap local es)]
        f (ExprI _ (VarE _ v)) = [v]
        f (ExprI _ (AccE _ e)) = f e
        f (ExprI _ (LstE es )) = concatMap f es
        f (ExprI _ (TupE es)) = concatMap f es
        f (ExprI _ (NamE rs)) = concatMap f (map snd rs)
        f (ExprI _ (AppE e es)) = concatMap f (e:es)
        -- Handle shadowing
        f (ExprI _ (LamE vs e)) = [v | v <- f e, v `notElem` vs]
        f (ExprI _ (AnnE e _)) = f e
        f _ = []

        local (ExprI _ (ModE _ es)) = concatMap local es
        local (ExprI _ (IstE _ _ es)) = concatMap local es
        local (ExprI _ (ImpE imp)) = case importInclude imp of
            (Just ss) -> [alias | AliasedTerm _ alias <- ss]
            Nothing -> []
        local (ExprI _ (SrcE src)) = [srcAlias src]
        local (ExprI _ (AssE v e es)) = v : (local e <> concatMap local es)
        local (ExprI _ (VarE _ _)) = [] -- KEY DIFFERENCE: These may not be local
        local (ExprI _ (AccE _ e)) = local e
        local (ExprI _ (LstE es )) = concatMap local es
        local (ExprI _ (TupE es)) = concatMap local es
        local (ExprI _ (NamE rs)) = concatMap local (map snd rs)
        local (ExprI _ (AppE e es)) = concatMap local (e:es)
        -- Handle shadowing
        local (ExprI _ (LamE vs e)) = [v | v <- local e, v `notElem` vs]
        local (ExprI _ (AnnE e _)) = local e
        local _ = []


collectTermTypes :: MVar -> ExprI -> MorlocMonad (Map.Map EVar TermTypes)
collectTermTypes mv e0
  = Map.mergeMapsM fb fc fbc sigs srcs
  >>= Map.mapKeysWithM mergeTermTypes (\(v,_,_) -> v)
  >>= Map.unionWithM mergeTermTypes decs
  where
  sigs = Map.fromListWith (<>) [((v, l, Nothing), [t]) | (v, l, t) <- findTopSignatures e0]
  srcs = Map.fromListWith (<>) [((srcAlias s, srcLabel s, langOf s), [(s, i)]) | (s, i) <- findTopSourcesWithIndex e0]
  decs = Map.map (TermTypes Nothing []) $ Map.fromListWith (<>) [(v, [e]) | (v, e) <- findTopDeclarations e0]

  -- generate a TermType object from only signatures
  fb :: [EType] -> MorlocMonad TermTypes
  fb [] = MM.throwError . CallTheMonkeys $ "This case should not appear given the construction of the map"
  fb (e0:es) = do
    e' <- foldlM mergeEType e0 es
    return $ TermTypes (Just e') [] []

  -- Should we even allow concrete terms with no type signatures?
  -- Yes, their types may be inferrable by usage or (eventually) static analysis
  -- of the source code.
  fc :: [(Source, Int)] -> MorlocMonad TermTypes
  fc srcs' = return $ TermTypes Nothing [(mv, Idx i src) | (src, i) <- srcs'] []

  fbc :: [EType] -> [(Source, Int)] -> MorlocMonad TermTypes
  fbc sigs' srcs' = do
    gt <- case sigs' of
      [e] -> return (Just e)
      [] -> return Nothing
      _ -> MM.throwError . CallTheMonkeys $ "Expected a single general type - I don't know how to merge them"
    return $ TermTypes gt [(mv, Idx i src) | (src, i) <- srcs'] []

  findTopSignatures :: ExprI -> [(EVar, Maybe Label, EType)]
  -- v is the name of the type
  -- l is the optional label for the signature
  -- t is the type
  findTopSignatures (ExprI _ (ModE _ es)) = [(v, l, t) | (ExprI _ (SigE (Signature v l t))) <- es]
  findTopSignatures (ExprI _ (SigE (Signature v l t))) = [(v, l, t)]
  findTopSignatures _ = []

  findTopSourcesWithIndex :: ExprI -> [(Source, Int)]
  findTopSourcesWithIndex (ExprI _ (ModE _ es)) = concatMap findTopSourcesWithIndex es
  findTopSourcesWithIndex (ExprI i (SrcE ss)) = [(ss, i)]
  findTopSourcesWithIndex _ = []

  findTopDeclarations :: ExprI -> [(EVar, ExprI)]
  findTopDeclarations (ExprI _ (ModE _ es)) = concatMap findTopDeclarations es
  findTopDeclarations (ExprI _ (AssE v e _)) = [(v, e)]
  findTopDeclarations _ = []


indexTerm :: a -> MorlocMonad (Indexed a)
indexTerm x = Idx <$> MM.getCounter <*> pure x


linkVariablesToTermTypes
  :: MVar
  -> Map.Map EVar (Indexed TermTypes) -- ^ a map term terms to types, Int is the inner GMAp key
  -> ExprI -- ^ list of expressions in the module
  -> MorlocMonad ()
linkVariablesToTermTypes mv m0 = link m0 where
  link :: Map.Map EVar (Indexed TermTypes) -> ExprI -> MorlocMonad ()
  -- The following have terms associated with them:
  -- 1. exported terms (but not exported types)
  link m (ExprI _ (ExpE export)) = linkExports m export
  -- 2. variables
  link m (ExprI i (VarE _ v)) = setMonomorphicType m i v
  -- 3. assignments
  link m (ExprI i (AssE v (ExprI _ (LamE ks e)) es)) = do
    setMonomorphicType m i v
    -- shadow all terms bound under the lambda
    let m' = foldr Map.delete m ks
    -- update with local scope
    m'' <- addLocalDefs m' es
    -- then link the assignment term and all local "where" statements (es)
    mapM_ (link m'') (e:es)
  -- 4. assignments that have no parameters
  link m (ExprI i (AssE v e es)) = do
    setMonomorphicType m i v
    -- update with local scope
    m' <- addLocalDefs m es
    -- then link the assignment term and all local "where" statements (es)
    mapM_ (link m') (e:es)
  -- 5. instances
  link m (ExprI _ (IstE _ _ es)) = mapM_ (link m) es
  -- everything below boilerplate
  link m (ExprI _ (ModE _ es)) = mapM_ (link m) es
  link m (ExprI _ (AccE _ e)) = link m e
  link m (ExprI _ (LstE xs)) = mapM_ (link m) xs
  link m (ExprI _ (TupE xs)) = mapM_ (link m) xs
  link m (ExprI _ (LamE vs e)) = link (foldr Map.delete m vs) e
  link m (ExprI _ (AppE f es)) = link m f >> mapM_ (link m) es
  link m (ExprI _ (AnnE e _)) = link m e
  link m (ExprI _ (NamE rs)) = mapM_ (link m . snd) rs
  link _ _ = return ()

  addLocalDefs
    :: Map.Map EVar (Indexed TermTypes)
    -> [ExprI]
    -> MorlocMonad (Map.Map EVar (Indexed TermTypes))
  addLocalDefs m es = do
    ms <- mapM (collectTermTypes mv) es
    foldrM (\um im -> Map.mergeMapsM return indexTerm mergeWithIdx im um) m ms
    where
      mergeWithIdx :: Indexed TermTypes -> TermTypes -> MorlocMonad (Indexed TermTypes)
      mergeWithIdx (Idx i t1) t2 = Idx i <$> mergeTermTypes t1 t2

  linkExports :: Map.Map EVar (Indexed TermTypes) -> Export -> MorlocMonad ()
  linkExports _ ExportAll = error "All exports should have been resolved"
  linkExports m (ExportMany ss) = mapM_ linkSymbol (Set.toList ss) where
    linkSymbol :: (Int, Symbol) -> MorlocMonad ()
    linkSymbol (_, TypeSymbol _) = return ()
    linkSymbol (i, TermSymbol v) = setMonomorphicType m i v

  setMonomorphicType :: Map.Map EVar (Indexed TermTypes) -> Int -> EVar -> MorlocMonad ()
  setMonomorphicType m i v = case Map.lookup v m of
    (Just (Idx j t)) -> do
      s <- MM.get
      newSigs <- GMap.insertWithM mergeSignatureSet i j (Monomorphic t) (stateSignatures s)
      MM.put (s { stateSignatures = newSigs
                 , stateName = Map.insert i v (stateName s) } )
      return ()
    Nothing -> return ()
