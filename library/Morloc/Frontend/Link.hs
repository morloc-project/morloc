{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.Frontend.Link
Description : Transfer all terms, types, typeclasses and sources to state
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : Apache-2.0
Maintainer  : z@morloc.io
Stability   : experimental
-}

module Morloc.Frontend.Link (link) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as DAG
import qualified Morloc.Data.Map as Map
import Morloc.Data.Map (Map)
import qualified Data.Set as Set
import qualified Morloc.Data.Text as DT
import Data.Set (Set)
import Morloc.Frontend.Merge (mergeSignatureSet, weaveTermTypes)
import Morloc.Typecheck.Internal (unqualify, qualify)

-- The following terms are modified in morloc state
--  * stateSignatures :: GMap Int Int SignatureSet -- links terms to types
--    - update SignatureSet when ClsE or IstE is encountered
--    - add term entry when any term is encountered
--  * stateTypeclasses :: Map EVar Instance -- only used to retrieve global typeclass terms, like pack and unpack
--    - update when Packable instance is encountered
--  * stateName :: Map Int EVar
--    - update when term is encountered (same places as stateSignature term updates)

-- when each new module is entered, the keys for linkTerms and linkClasses is
-- rewritten based on the AliasedSymbol edge aliases
data LinkState = LinkState
  { linkTerms :: Map EVar Int
    -- ^ stateSignatures index 2
  , linkClasses :: Map ClassName (Int, Typeclass Signature, Map EVar Int)
    -- ^ maps class methods into stateSignatures index 2
  }

link :: DAG MVar [AliasedSymbol] ExprI -> MorlocMonad ()
link d0 = do
  mayResult <- DAG.synthesizeNodes synth d0
  case mayResult of
    Nothing -> error "cyclical"
    (Just _) -> return ()

synth
  :: MVar
  -> ExprI
  -> [(MVar, [AliasedSymbol], LinkState)]
  -> MorlocMonad LinkState
synth k0 e0 edges = do
  inheritedState <- mapM (realiasLinkState k0) edges >>= mergeLinkStates k0
  finalState <- addLocalState k0 e0 inheritedState
  _ <- linkLocalTerms k0 finalState e0
  return finalState


-- Will raise error if any import term is absent in the linkstate list
realiasLinkState :: MVar -> (MVar, [AliasedSymbol], LinkState) -> MorlocMonad (MVar, LinkState)
realiasLinkState m1 (_, ss, s) = do
  termmap <- mergeValues "terms" $ map (\(n, a) -> (a, Map.lookup n (linkTerms s))) [(n, a) | AliasedTerm n a <- ss]
  classmap <- mergeValues "classes" $ map (\n -> (n, Map.lookup n (linkClasses s))) [n | AliasedClass n <- ss]
  return (m1, LinkState termmap classmap)
  where

  mergeValues :: (Ord k, Show k) => String -> [(k, Maybe v)] -> MorlocMonad (Map k v)
  mergeValues msg xs = case [v | (v, Nothing) <- xs] of
    [] -> return $ Map.fromList [(v,i) | (v, Just i) <- xs]
    missing -> error $ "Undefined " <> msg <> " imports:" <> show missing <> "\n  ss = " <> show ss



-- All LinkState objects whould have already been renamed by realiasLinkState.
--
-- will raise error if
--   1. if multiple separate typeclasses are imported under the same name
--   2. if any imported term overlaps with a typeclass method
--   3. if multiple separate terms are imported under the same name
--
-- This function needs to iterate through the imports such that the modules
-- involved with conflicts can be identified for better error messages. So all
-- imported terms are first pooled, then iterated through one-by-one checking
-- for conflicts. Term identity is based on index, which maps one-to-one to type
-- or class signature.
mergeLinkStates :: MVar -> [(MVar, LinkState)] -> MorlocMonad LinkState
mergeLinkStates m0 imps = do
      -- Set EVar
  let terms = Set.unions $ [Map.keysSet s | (_, LinkState s _) <- imps]
      -- Set ClassName
      classes = Set.unions $ [Map.keysSet s | (_, LinkState _ s) <- imps]
      -- Map EVar [(MVar, Int)]
      termGroups = Map.fromSet (\k -> catMaybes [(,) m <$> Map.lookup k (linkTerms s) | (m, s) <- imps]) terms
      -- Map ClassName [(MVar, (Int, Map EVar Int))]
      classGroups = Map.fromSet (\k -> catMaybes [(,) m <$> Map.lookup k (linkClasses s) | (m, s) <- imps]) classes

  termmap <- Map.mapWithKeyM mergeTerms termGroups
  classmap <- Map.mapWithKeyM mergeClasses classGroups
  _ <- checkTermClassConflicts termmap classmap
  return $ LinkState termmap classmap

  where

  mergeTerms :: EVar -> [(MVar, Int)] -> MorlocMonad Int
  mergeTerms _ [] = error "This will never be empty"
  mergeTerms _ [(_, i)] = return i
  mergeTerms v ((_, i):(m2, j):xs)
    | i == j = mergeTerms v ((m2, j):xs)
    | otherwise = MM.throwError . ImportExportError m0 . render
                $ "Illegal masking of term" <+> squotes (pretty v)
                <+> "imported from" <+> pretty m2

  mergeClasses :: ClassName -> [(MVar, (Int, a, Map EVar Int))] -> MorlocMonad (Int, a, Map EVar Int)
  mergeClasses _ [] = error "This will never be empty"
  mergeClasses _ [(_, x)] = return x
  mergeClasses v ((m1, (i, _, _)):(m2, y@(j, _, _)):xs)
    | i == j = mergeClasses v ((m2, y):xs)
    | otherwise = MM.throwError . ImportExportError m0 . render
                $ "Cannot merge non-eqivalent classes imported from modules" <+> pretty m1 <+> "and" <+> pretty m2

  checkTermClassConflicts :: Map EVar Int -> Map ClassName (Int, a, Map EVar Int) -> MorlocMonad ()
  checkTermClassConflicts me mc = case catMaybes . map (checkTermClassConflict me) $ Map.toList mc of
    [] -> return ()
    ((cls, vs):_) -> MM.throwError . ImportExportError m0 . render
                  $   "The following terms in the typeclass" <+> squotes (pretty cls)
                  <+> "conflict with monomorphic terms in scope:"
                  <+> list (map pretty vs)

  checkTermClassConflict :: Map EVar Int -> (ClassName, (Int, a, Map EVar Int)) -> Maybe (ClassName, [EVar])
  checkTermClassConflict me (cls, (_, _, mc)) = case Set.toList (Set.intersection (Map.keysSet me) (Map.keysSet mc)) of
    [] -> Nothing
    conflicts -> Just (cls, conflicts)


-- updates stateSignature index 2
addLocalState :: MVar -> ExprI -> LinkState -> MorlocMonad LinkState
addLocalState m0 e0 s0 = do
  s1 <- findDefs e0 s0
  (_, s2) <- findFreeDefs e0 (Set.empty, s1)
  return s2
  where
  -- Iterate through the expression and add signatures and typeclasses. This
  -- needs to be done before the sources, declarations, and instances are
  -- added, since these all need to augment code that has already be indexed.
  findDefs (ExprI sigIndex (SigE (Signature v _ e))) lstate = do
    -- get the (GMap Int Int SigantureSet) map from state
    (GMap idmap sigmap) <- MM.gets stateSignatures
    -- define a new monomorphic term with no implementations
    let sigset = Monomorphic (TermTypes (Just e) [] [])
        sigmap' = Map.insert sigIndex sigset sigmap
    -- update state with the signature, the signature index will be linked to by
    -- all future terms of this type (even after they have been aliased)
    MM.modify (\s -> s {stateSignatures = GMap idmap sigmap'})
    -- update the map between term names and signature indices
    return $ lstate { linkTerms = Map.insert v sigIndex (linkTerms lstate) }
  -- create new entries for class definitions and type signatures
  findDefs (ExprI clsIndex (ClsE tcls@(Typeclass cls vs sigs))) lstate = do
    -- get sigmap
    (GMap idmap sigmap) <- MM.gets stateSignatures
    -- generate an index for each signature in this typeclass
    sigsIdx <- mapM (\sig -> (,) <$> MM.getCounter <*> pure sig) sigs
    -- add these new typeclass methods to stateSignatures as polymorphic entries
    let sigmap' = foldr (\(i, Signature v _ t) m -> Map.insert i (Polymorphic cls v t []) m) sigmap sigsIdx

    -- setup stateTypeclasses
    let xs = [(v, Instance cls vs et []) | Signature v _ et <- sigs]
    tmap <- MM.gets stateTypeclasses
    tmap' <- foldlM (\m (k, v) -> insertWithCheck k v m) tmap xs

    -- update morloc state
    MM.modify (\s -> s {stateSignatures = GMap idmap sigmap', stateTypeclasses = tmap'})

    -- generate the (Map EVar Int) list for LinkedState
    let vmap = Map.fromList [(v, i) | (i, Signature v _ _) <- sigsIdx]
        classes = Map.insert cls (clsIndex, tcls, vmap) (linkClasses lstate)
    return $ lstate { linkClasses = classes }
  -- We only search for definitions at the top level. This may be the top-level
  -- inside of a where statement.
  findDefs (ExprI _ (ModE _ es)) lstate = foldrM findDefs lstate es
  -- All other types return the map unchanged
  findDefs _ lstate = return lstate

  insertWithCheck :: EVar -> Instance -> Map EVar Instance -> MorlocMonad (Map EVar Instance)
  insertWithCheck k v m = case Map.lookup k m of
    (Just inst2) -> MM.throwError . ImportExportError m0 . render
                    $ "Conflict between typeclasses over term" <+> squotes (pretty k)
                    <+> "that is present the typeclasses"
                    <+> squotes (pretty (className inst2)) <+> "and"
                    <+> squotes (pretty (className v))
    Nothing -> return $ Map.insert k v m

  -- Handle assignments that do not have signatures
  findFreeDefs (ExprI _ (AssE v _ _)) (terms, lstate)
    | Set.member v terms || Map.member v (linkTerms lstate) = return (terms, lstate)
    | otherwise = do
        -- make new index to use for all definitions of this term
        idx <- MM.getCounter
        (GMap idmap sigmap) <- MM.gets stateSignatures
        -- define a new monomorphic term with no implementation and no type
        let sigset = Monomorphic (TermTypes Nothing [] [])
            sigmap' = Map.insert idx sigset sigmap
            terms' = Set.insert v terms
        MM.modify (\s -> s {stateSignatures = GMap idmap sigmap'})
        let lstate' = lstate { linkTerms = Map.insert v idx (linkTerms lstate) }
        return (terms', lstate')
  findFreeDefs (ExprI _ (ModE _ es)) s = foldrM findFreeDefs s es
  findFreeDefs _ s = return s


toCondensedState :: LinkState -> Map EVar (Int, Maybe (Typeclass Signature))
toCondensedState s = Map.union terms classes where
  terms = Map.map (\i -> (i, Nothing)) (linkTerms s)
  classes = Map.fromList . concat
          $ [ [(v, (i, Just tcls)) | (v, i) <- Map.toList emap]
            | (_, tcls, emap) <- Map.elems (linkClasses s) ]

-- link source, declaration, and instance to stateSignature index 2
-- link terms to stateSignature index 1 and stateName
linkLocalTerms :: MVar -> LinkState -> ExprI -> MorlocMonad ()
linkLocalTerms m0 s0 e0 = linkLocal Set.empty s0 (toCondensedState s0) e0 where

  -- link a new source statement to its type in morloc state
  linkLocal :: Set EVar -> LinkState -> Map EVar (Int, Maybe (Typeclass Signature)) -> ExprI -> MorlocMonad ()
  linkLocal _ _ cs (ExprI i (SrcE src)) = do
    case Map.lookup (srcAlias src) cs of
      -- A source with no associated type signature may be a constructor.
      -- If it is a term, then it must have a signature if it is to be used, but
      -- its use will raise a dedicated error later. So we let it pass for now.
      Nothing -> return ()
      (Just (_, Just (Typeclass cls _ _))) -> MM.throwError . TypeclassError . render $
        "Source term" <+> squotes (pretty (srcAlias src)) <+> " shadows the typeclass" <+> squotes (pretty cls)

      (Just (termIdx, Nothing)) -> do
        (GMap idmap sigmap) <- MM.gets stateSignatures
        case Map.lookup termIdx sigmap of
          Nothing -> error "This should be unreachable since there is an associated signature and it should have been loaded"
          (Just (Monomorphic tt)) -> do
            let srcTerm = (m0, Idx i src)
                tt' = tt { termConcrete = srcTerm : termConcrete tt}
                sigmap' = Map.insert termIdx (Monomorphic tt') sigmap
                idmap' = Map.insert i termIdx idmap
            MM.modify (\s -> s {stateSignatures = GMap idmap' sigmap'})
          (Just (Polymorphic cls _ _ _)) -> MM.throwError . TypeclassError . render $
            "Source term" <+> squotes (pretty (srcAlias src)) <+> " overlaps a term in typeclass" <+> squotes (pretty cls)

  -- link a new declaration to its type in morloc state and recurse into its
  -- local where block as needed
  linkLocal bnds c cs (ExprI i (AssE v e es)) = do
    updateName i v
    case Map.lookup v cs of
      Nothing -> error "Bug: This case should be unreachable"
      (Just (_, Just _)) -> undefined -- handle error for src that overlaps typeclass term
      (Just (termIdx, Nothing)) -> do
        (GMap idmap sigmap) <- MM.gets stateSignatures
        case Map.lookup termIdx sigmap of
          Nothing -> error "Bug: This should be unreachable since there is an associated signature and it should have been loaded"
          (Just (Monomorphic tt)) -> do
            let tt' = tt { termDecl = e : termDecl tt}
                sigmap' = Map.insert termIdx (Monomorphic tt') sigmap
                idmap' = Map.insert i termIdx idmap
            MM.modify (\ms -> ms {stateSignatures = GMap idmap' sigmap'})
            (bnds', c', _) <- case e of
              (ExprI _ (LamE vs _)) ->
                return ( foldr Set.insert bnds vs
                       , c {linkTerms = foldr Map.delete (linkTerms c) vs }
                       , foldr Map.delete cs vs
                       )
              _ -> return (bnds, c, cs)
            -- link expressions in the where statement within a local scope
            c'' <- foldrM (addLocalState m0) c' (e:es)

            mapM_ ( linkLocal bnds' c'' (toCondensedState c'')) (e:es)
          (Just (Polymorphic cls _ _ _)) -> MM.throwError . TypeclassError . render $
            "Declared term" <+> squotes (pretty v) <+> " overlaps a term in typeclass" <+> squotes (pretty cls)

  linkLocal bnds c cs (ExprI _ (IstE cls ts es)) = do
    case Map.lookup cls (linkClasses c) of
      Nothing -> MM.throwError . TypeclassError . render
              $ "There is no typeclass declaration for instance" <+> squotes (pretty cls) <+> "in the scope of module" <+> squotes (pretty m0)

      (Just (_, Typeclass _ vs sigs, emap)) ->
        if length vs /= length ts
        then MM.throwError . TypeclassError . render
          $ "In module" <+> squotes (pretty m0) <> ": the instance and typeclass definitions for" <+> squotes (pretty cls) <+> "differ in number of terms"

        else mapM_ (linkInstance (linkLocal bnds c cs) m0 cls (zip vs ts) sigs emap) es

  linkLocal bnds _ cs (ExprI termIdx (VarE _ v))
    | Set.member v bnds = return ()
    | otherwise = case Map.lookup v cs of
        -- handle both monomorphic terms and polymorphic typeclass terms
        (Just (sigIdx, _)) ->  updateSigLinks v termIdx sigIdx
        Nothing -> MM.throwError . ImportExportError m0 $ "Undefined term: " <> unEVar v

  linkLocal _ _ cs (ExprI _ (ExpE (ExportMany (Set.toList -> ss)))) =
    mapM_ linkExp [(v, termIdx, Map.lookup v cs) | (termIdx, TermSymbol v) <- ss]
    where
      linkExp :: (EVar, Int, Maybe (Int, a)) -> MorlocMonad ()
      linkExp (v, termIdx, Just (sigIdx, _)) = updateSigLinks v termIdx sigIdx
      -- TODO: give this a good error message - it is a user facing issue
      -- Is raised when an exported term, such as a sourced function, is
      -- exported with no signature in scope.
      linkExp (v, _, Nothing) = MM.throwError . ImportExportError m0 . render $
        "Undefined export" <+> squotes (pretty v)

  linkLocal _ _ _ (ExprI _ (ExpE ExportAll)) = error "Bug: ExportAll should no longer be present"

  -- Shadowing is allowed, so here all terms in `s` that are bound by the lambda
  -- need to be removed
  linkLocal bnds c cs (ExprI _ (LamE vs e)) = do
    (c', cs') <- foldlM shadow (c, cs) vs
    let bnds' = Set.union bnds (Set.fromList vs)
    linkLocal bnds' c' cs' e
    where

    shadow :: (LinkState, Map EVar (Int, Maybe (Typeclass Signature)))
           -> EVar
           -> MorlocMonad (LinkState, Map EVar (Int, Maybe (Typeclass Signature)))
    shadow (ls, cs') v = case Map.lookup v cs' of
      Nothing -> return (ls, cs)
      (Just (_, Nothing)) -> return (ls { linkTerms = Map.delete v (linkTerms ls) }, Map.delete v cs')
      (Just (_, Just _)) -> MM.throwError . TypeclassError . render $
        "Illegal shadowing of typeclass term:" <+> pretty v 

  -- The `m` should always be the same as `m0`, since modules don't next.
  -- Even if there are two modules defined in one file, they will still be
  -- unnested, same as if they were in different files.
  linkLocal bnds c cs (ExprI _ (ModE m es))
    | m /= m0 = MM.throwError . NestedModule $ m
    | otherwise = mapM_ (linkLocal bnds c cs) es

  -- simple recursive cases
  linkLocal bnds c cs (ExprI _ (LstE es )) = mapM_ (linkLocal bnds c cs) es
  linkLocal bnds c cs (ExprI _ (TupE es)) = mapM_ (linkLocal bnds c cs) es
  linkLocal bnds c cs (ExprI _ (NamE (map snd -> es))) = mapM_ (linkLocal bnds c cs) es
  linkLocal bnds c cs (ExprI _ (AppE e es)) = mapM_ (linkLocal bnds c cs) (e:es)
  linkLocal bnds c cs (ExprI _ (AnnE e _)) = linkLocal bnds c cs e
  -- terminal cases
  linkLocal _ _ _ _ = return ()


  updateSigLinks :: EVar -> Int -> Int -> MorlocMonad ()
  updateSigLinks v termIdx sigIdx = do
    (GMap idmap sigmap) <- MM.gets stateSignatures
    let idmap' = Map.insert termIdx sigIdx idmap
    MM.modify (\ms -> ms { stateSignatures = GMap idmap' sigmap
                         , stateName = Map.insert termIdx v (stateName ms)
                         } )

  updateName :: Int -> EVar -> MorlocMonad ()
  updateName i v = MM.modify (\s -> s { stateName = Map.insert i v (stateName s) } )

-- Goal:
--   for each term in [Signature], add a TermTypes instance to the correct
--   SignatureSet in stateSignatures. This will be a polymorphic case.
--     Polymorphic ClassName EVar EType [TermTypes]
linkInstance
  :: (ExprI -> MorlocMonad ())
  -> MVar
  -> ClassName
  -> [(TVar, TypeU)]
  -> [Signature]
  -> Map EVar Int
  -> ExprI
  -> MorlocMonad ()
linkInstance linker m0 cls0 params0 sigs0 emap0 e0 = linkExpr e0 where

  linkExpr (ExprI i (SrcE src)) = do
    let v = srcAlias src
    (Signature _ _ et, stateIdx) <- lookupInfo v
    t <- substituteInstanceTypes params0 (etype et)
    let et' = et { etype = t }
    let tt = TermTypes (Just et') [(m0, Idx i src)] []
    linkTermTypes v et tt stateIdx
  linkExpr (ExprI _ (AssE v e es)) = do
    mapM_ linker (e:es)
    (Signature _ _ et, stateIdx) <- lookupInfo v
    t <- substituteInstanceTypes params0 (etype et)
    let et' = et { etype = t }
    let tt = TermTypes (Just et') [] [e]
    linkTermTypes v et tt stateIdx
  linkExpr _ = error "Unreachable, instances may only contain sources and instances -- this should have been caught in the parser."

  lookupInfo :: EVar -> MorlocMonad (Signature, Int)
  lookupInfo v = case ([sig | sig@(Signature v' _ _) <- sigs0, v == v'], Map.lookup v emap0) of
    ([sig], Just i) -> return (sig, i)
    _ -> MM.throwError . TypeclassError . render
       $ "Instance of class" <+> squotes (pretty cls0) <+> "contains undefined term" <+> squotes (pretty v)

  linkTermTypes :: EVar -> EType -> TermTypes -> Int -> MorlocMonad ()
  linkTermTypes v et tt stateIdx = do
    (GMap idmap sigmap) <- MM.gets stateSignatures
    tcls <- MM.gets stateTypeclasses
    case Map.lookup stateIdx sigmap of
      Nothing -> undefined -- should be unreachable
      (Just sigset) -> do
        sigset' <- mergeSignatureSet sigset (Polymorphic cls0 v et [tt])
        let sigmap' = Map.insert stateIdx sigset' sigmap
        newInstance <- case Map.lookup v tcls of
          Nothing -> do
            return $ Instance cls0 (map fst params0) et [tt]
          (Just inst) -> do
            return $ inst {instanceTerms = weaveTermTypes tt (instanceTerms inst) }
        let tcls' = Map.insert v newInstance tcls
        MM.modify (\ms -> ms {stateSignatures = GMap idmap sigmap', stateTypeclasses = tcls'})

{- Substitute the instance types into the class function definition

Suppose we have the following class and instances:

class Reversible a b where
  forward :: a -> b
  backward :: b -> a

instance Reversible ([a],[b]) [(a,b)] where
  ...

If we are handling the single instance above for the `forward` function:

  classVars: [a, b]
  classType: forall a b . a -> b
  instanceParameters: forall a b . ([a], [b])
                      forall a b . [(a, b)]

and the return type should be

  forall a b . ([a],[b]) -> [(a,b)]

A problem here is that the instance parameters *share* qualifiers. The `a` and `b`
in the first instance parameter are the same as those in the second. But not the
same as the `a` and `b` in the class.


-}
substituteInstanceTypes :: [(TVar, TypeU)] -> TypeU -> MorlocMonad TypeU
substituteInstanceTypes (unzip -> (clsVars, instanceParameters)) clsType  = do

      -- find all qualifiers in the instance parameter list
  let instanceQualifiers = unique $ concatMap (fst . unqualify) instanceParameters

      -- rewrite the class type such that the class qualifiers appear first and
      -- do not conflict with parameter qualifiers
      cleanClassType = replaceQualifiers instanceQualifiers (putClassVarsFirst clsType)

      -- substitute in the parameter types
      finalType = qualify instanceQualifiers
                $ substituteQualifiers cleanClassType (map (snd . unqualify) instanceParameters)

  MM.sayVVV $ "substituteInstanceTypes"
            <> "\n  classVars:" <+> pretty clsVars
            <> "\n  classType:" <+> pretty clsType
            <> "\n  instanceParameters:" <+> pretty instanceParameters
            <> "\n  -------------------"
            <> "\n  instanceQualifiers:" <+> pretty instanceQualifiers
            <> "\n  cleanClassType:" <+> pretty cleanClassType
            <> "\n  finalType:" <+> pretty finalType

  return finalType

  where
    putClassVarsFirst :: TypeU -> TypeU
    putClassVarsFirst t =
      let (vs, t') = unqualify t
      in qualify (clsVars <> filter (`notElem` clsVars) vs) t'

    replaceQualifiers :: [TVar] -> TypeU -> TypeU
    replaceQualifiers vs0 t0 = f vs0 [r | r <- freshVariables, r `notElem` doNotUse] t0
      where

      -- qualifiers to avoid when replacing
      doNotUse = vs0 <> (fst . unqualify) t0

      f (v:vs) (r:rs) (ForallU v' t)
        | v == v' = ForallU r . f vs rs $ substituteTVar v' (VarU r) t
        | otherwise = ForallU v' (f (v:vs) (r:rs) t)
      f _ _ t = t

      freshVariables = [1 ..] >>= flip replicateM ['a' .. 'z'] |>> TV . DT.pack

    substituteQualifiers :: TypeU -> [TypeU] -> TypeU
    substituteQualifiers (ForallU v t) (r:rs) = substituteQualifiers (substituteTVar v r t) rs
    substituteQualifiers t _ = t
