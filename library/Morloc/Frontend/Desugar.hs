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
import qualified Morloc.Data.Text as MT
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Frontend.PartialOrder as MTP


-- | Resolve type aliases, term aliases and import/exports
desugar
  :: DAG MVar Import ExprI
  -> MorlocMonad (DAG MVar [(EVar, EVar)] ExprI)
desugar s
  = resolveImports s
  >>= checkForSelfRecursion
  >>= desugarDag


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
    hasTerm _ NulU = False
    hasTerm v (VarU v') = v == v'
    hasTerm v (ForallU _ t) = hasTerm v t
    hasTerm v (FunU t1 t2) = hasTerm v t1 || hasTerm v t2
    hasTerm v (AppU t1 t2) = hasTerm v t1 || hasTerm v t2
    hasTerm v (RecU _ t1 (_, t2)) = hasTerm v t1 || hasTerm v t2
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
  f (Signature v l t) = Signature v l <$> desugarEType termmap d k t
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
desugarType h d k t0 = f [] t0
  where

  f :: [TypeU] -> TypeU -> MorlocMonad TypeU

  --   (Just []) -> return (t0, [])
  --   (Just ts'@(t':_)) -> do
  --     (_, t) <- foldlM (mergeAliases v 0) t' ts'
  --     f t
  --   Nothing -> MM.throwError . CallTheMonkeys $ "Type term in VarU missing from type map"
  f ts (ExistU v ps ds) = do
    ps' <- mapM (f ts) ps
    ds' <- mapM (f ts) ds
    return $ ExistU v ps' ds'
  f _ (FunU t1 t2) = FunU <$> f [] t1 <*> f [] t2
  f _ (RecU r t1 k t2) = RecU r <$> f [] t1 <*> pure k <$> f [] t2

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
  f ts (AppU a b) = do
    b' <- f [] b
    -- the right terms are passed left with each unwrapped App, when the type
    -- function is reached (which should be a VarU), the the right terms will
    -- be substituted in
    f (b':ts) a

  -- type Foo = A     
  -- f :: Foo -> B    
  -- -----------------
  -- f :: A -> B      
  f ts1 t0@(VarU v) =
    case Map.lookup v h of
      Nothing -> t0
      (Just (r:rs)) -> do
        (ps2, t1) <- mergeAliases v (length ts1) r rs
        expand ps2 ts1 t1
    where
      expand :: [TVar] -> [TypeU] -> TypeU -> MorlocMonad TypeU
      expand [] [] t = return t
      expand [] (_:_) _ = MM.throwError . CallTheMonkeys $ "Test and annotate this error"
      expand (_:_) [] _ = MM.throwError . CallTheMoneksy $ "Test and annotate this error"
      expand (v:vs) (r:rs) t = expand vs rs <$> parsub (v, (chooseExistential r)) (chooseExistential t) 

  parsub :: (TVar, TypeU) -> TypeU -> TypeU
  parsub (v, t2) t1@(VarU v0)
    | v0 == v = t2 -- substitute
    | otherwise = t1 -- keep the original
  parsub _ (ExistU _ _ _) = error "What the bloody hell is an existential doing down here?"
  parsub pair (ForallU v t1) = ForallU v (parsub pair t1)
  parsub _ NulU = NulU 
  parsub pair (FunU t1 t2) = FunU (parsub pair t1) (parsub pair t1)
  parsub pair (AppU t1 t2) = AppU (parsub pair t1) (parsub pair t1)
  parsub pair (RecU r t1 k t2) = RecU r (parsub pair t1) k (parsub pair t2)


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
chooseExistential NulU = NulU
chooseExistential (VarU v) = VarU v
chooseExistential (ExistU _ _ (t:_)) = (chooseExistential t)
chooseExistential (ExistU _ _ []) = error "Existential with no default value"
chooseExistential (ForallU v t) = ForallU v (chooseExistential t)
chooseExistential (FunU t1 t2) = FunU (chooseExistential t1) (chooseExistential t2)
chooseExistential (AppU t1 t2) = AppU (chooseExistential t1) (chooseExistential t2)
chooseExistential (RecU k t1 v t2) = RecU k (chooseExistential t1) v (chooseExistential t2)


-- addPackerMap
--   :: (DAG MVar [(EVar, EVar)] Expr)
--   -> MorlocMonad (DAG MVar [(EVar, EVar)] Expr)
-- addPackerMap = undefined
-- -- addPackerMap
-- --   :: (DAG MVar [(EVar, EVar)] PreparedNode)
-- --   -> MorlocMonad (DAG MVar [(EVar, EVar)] PreparedNode)
-- -- addPackerMap d = do
-- --   maybeDAG <- MDD.synthesizeDAG gatherPackers d
-- --   case maybeDAG of
-- --     Nothing -> MM.throwError CyclicDependency
-- --     (Just d') -> return d'
-- --
-- -- gatherPackers
-- --   :: MVar -- the importing module name (currently unused)
-- --   -> PreparedNode -- data about the importing module
-- --   -> [( MVar -- the name of an imported module
-- --         , [(EVar -- the name of a term in the imported module
-- --           , EVar -- the alias in the importing module
-- --           )]
-- --         , PreparedNode -- data about the imported module
-- --      )]
-- --   -> MorlocMonad PreparedNode
-- -- gatherPackers _ n1 es = do
-- --   let packers   = starpack n1 Pack
-- --       unpackers = starpack n1 Unpack
-- --   nodepackers <- makeNodePackers packers unpackers n1
-- --   let m = Map.unionsWith (<>) $ map (\(_, e, n2) -> inheritPackers e n2) es
-- --       m' = Map.unionWith (<>) nodepackers m
-- --   return $ n1 { preparedNodePackers = m' }
-- --
-- -- starpack :: PreparedNode -> Property -> [(EVar, TypeU, [Source])]
-- -- starpack n pro
-- --   = [ (v, t, maybeToList $ lookupSource v t (preparedNodeSourceMap n))
-- --     | (Signature v _ e@(EType t p _)) <- preparedNodeBody n
-- --     , isJust (langOf e)
-- --     , Set.member pro p]
-- --   where
-- --     lookupSource :: EVar -> TypeU -> Map.Map (EVar, Lang) Source -> Maybe Source
-- --     lookupSource v t m = langOf t >>= (\lang -> Map.lookup (v, lang) m)
-- --
-- -- makeNodePackers
-- --   :: [(EVar, TypeU, [Source])]
-- --   -> [(EVar, TypeU, [Source])]
-- --   -> PreparedNode
-- --   -> MorlocMonad (Map.Map (TVar, Int) [UnresolvedPacker])
-- -- makeNodePackers xs ys n =
-- --   let xs' = map (\(x,y,z)->(x, chooseExistential y, z)) xs
-- --       ys' = map (\(x,y,z)->(x, chooseExistential y, z)) ys
-- --       items = [ ( packerKey t2
-- --                 , [UnresolvedPacker (packerTerm v2 n) (packerType t1) ss1 ss2])
-- --               | (_ , t1, ss1) <- xs'
-- --               , (v2, t2, ss2) <- ys'
-- --               , packerTypesMatch t1 t2
-- --               ]
-- --   in return $ Map.fromList items
-- --
-- -- packerTerm :: EVar -> Expr -> Maybe EVar
-- -- packerTerm v n = listToMaybe . catMaybes $
-- --   [ termOf t
-- --   | (v', t) <- AST.findSignatures n
-- --   , v == v'
-- --   , isNothing (langOf t)
-- --   ]
-- --   where
-- --     termOf :: EType -> Maybe EVar
-- --     termOf e = case splitArgs (etype e) of
-- --       -- packers are all global (right?)
-- --       (_, [VarU (TV _ term), _]) -> Just $ EV term
-- --       (_, [ArrU (TV _ term) _, _]) -> Just $ EV term
-- --       _ -> Nothing

-- packerTypesMatch :: TypeU -> TypeU -> Bool
-- packerTypesMatch t1 t2 = case (splitArgs t1, splitArgs t2) of
--   ((vs1@[_,_], [t11, t12]), (vs2@[_,_], [t21, t22]))
--     -> MTP.equivalent (qualify vs1 t11) (qualify vs2 t22)
--     && MTP.equivalent (qualify vs1 t12) (qualify vs2 t21)
--   _ -> False
--
-- packerType :: TypeU -> TypeU
-- packerType t = case splitArgs t of
--   (params, [t1, _]) -> qualify params t1
--   _ -> error "bad packer"
--
-- packerKey :: TypeU -> (TVar, Int)
-- packerKey t = case splitArgs t of
--   (params, [VarU v, _])   -> (v, length params)
--   (params, [ArrU v _, _]) -> (v, length params)
--   (params, [NamU _ v _ _, _]) -> (v, length params)
--   _ -> error "bad packer"
--
-- qualify :: [TVar] -> TypeU -> TypeU
-- qualify [] t = t
-- qualify (v:vs) t = ForallU v (qualify vs t)
--
-- splitArgs :: TypeU -> ([TVar], [TypeU])
-- splitArgs (ForallU v u) =
--   let (vs, ts) = splitArgs u
--   in (v:vs, ts)
-- splitArgs (FunU t1 t2) =
--   let (vs, ts) = splitArgs t2
--   in (vs, t1:ts)
-- splitArgs t = ([], [t])
--
--
-- -- | Packers need to be passed along with the types the pack, they are imported
-- -- explicitly with the type and they pack. Should packers be universal? The
-- -- packers describe how a term may be simplified. But often there are multiple
-- -- reasonable ways to simplify a term, for example `Map a b` could simplify to
-- -- `[(a,b)]` or `([a],[b])`. The former is semantically richer (since it
-- -- naturally maintains the one-to-one variant), but the latter may be more
-- -- efficient or natural in some languages. For any interface, both sides must
-- -- adopt the same forms. The easiest way to enforce this is to require one
-- -- global packer, but ultimately it would be better to resolve packers
-- -- case-by-base as yet another optimization degree of freedom.
-- inheritPackers
--   :: [( EVar -- key in THIS module described in the PreparedNode argument
--       , EVar -- alias used in the importing module
--       )]
--   -> Expr
--   -> Map.Map (TVar, Int) [UnresolvedPacker]
-- inheritPackers = undefined
-- -- inheritPackers
-- --   :: [( EVar -- key in THIS module descrived in the PreparedNode argument
-- --       , EVar -- alias used in the importing module
-- --       )]
-- --   -> PreparedNode
-- --   -> Map.Map (TVar, Int) [UnresolvedPacker]
-- -- inheritPackers es n =
-- --   -- names of terms exported from this module
-- --   let names = Set.fromList [ v | (EV v, _) <- es]
-- --   in   Map.map (map toAlias)
-- --      $ Map.filter (isImported names) (preparedNodePackers n)
-- --   where
-- --     toAlias :: UnresolvedPacker -> UnresolvedPacker
-- --     toAlias n' = n' { unresolvedPackerTerm = unresolvedPackerTerm n' >>= (flip lookup) es }
-- --
-- --     isImported :: Set.Set MT.Text -> [UnresolvedPacker] -> Bool
-- --     isImported _ [] = False
-- --     isImported names' (n0:_) = case unresolvedPackerTerm n0 of
-- --       (Just (EV v)) -> Set.member v names'
-- --       _ -> False
