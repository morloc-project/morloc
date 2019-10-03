{-|
Module      : Morloc.TypeChecker.Infer
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.TypeChecker.Infer
  -- * The main type checker
  ( typecheck
  -- * Internal functions used in testing
  , subtype
  , substitute
  , apply
  , applyE
  , free
  , infer
  , renameExpr
  , unrenameExpr
  , renameType
  , unrenameType
  ) where

import Morloc.Namespace
import Morloc.TypeChecker.Util
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT

typecheck :: [Module] -> Stack [Module]
typecheck ms = do
  mods <- foldM insertWithCheck Map.empty [(moduleName m, m) | m <- ms]
  -- graph :: Map MVar (Set MVar)
  let graph = Map.fromList $ map mod2pair ms
  mods' <- path graph
  case mapM (flip Map.lookup $ mods) mods' of
    (Just mods'') -> fmap reverse $ typecheckModules (Map.empty) mods''
    Nothing -> throwError UnknownError -- this shouldn't happen
  where
    mod2pair :: Module -> (MVar, Set.Set MVar)
    mod2pair m =
      (moduleName m, Set.fromList $ map importModuleName (moduleImports m))

typecheckModules :: ModularGamma -> [Module] -> Stack [Module]
typecheckModules _ [] = return []
typecheckModules mg (m:ms) = do
  g <- importFromModularGamma mg m
  (g', exprs) <- typecheckExpr g (moduleBody m)
  mg' <- extendModularGamma g' m mg
  mods <- typecheckModules mg' ms
  return (m {moduleBody = exprs} : mods)

insertWithCheck ::
     Map.Map MVar Module -> (MVar, Module) -> Stack (Map.Map MVar Module)
insertWithCheck ms (v, m) =
  case Map.insertLookupWithKey (\_ _ y -> y) v m ms of
    (Just m', _) -> throwError $ MultipleModuleDeclarations (moduleName m')
    (Nothing, ms') -> return ms'

-- produce a path from sources to pools, die on cycles
path :: (Ord a) => Map.Map a (Set.Set a) -> Stack [a]
path m
  | Map.size m == 0 = return []
  | otherwise =
    if Map.size rootMap == 0
      then throwError CyclicDependency
      else do
        rest <- path (Map.difference m rootMap)
        return (rest ++ Map.keys rootMap)
  where
    rootMap = Map.filterWithKey (isRoot m) m

isRoot :: (Ord a) => Map.Map a (Set.Set a) -> a -> Set.Set a -> Bool
isRoot m k _ = not $ Map.foldr (isChild k) False m
  where
    isChild :: (Ord a) => a -> Set.Set a -> Bool -> Bool
    isChild _ _ True = True
    isChild k' s False = Set.member k' s

typecheckExpr :: Gamma -> [Expr] -> Stack (Gamma, [Expr])
typecheckExpr g e = do
  es <- fmap sort (mapM renameExpr e)
  (g', es') <- typecheckExpr' g [] es
  let es'' = concat [toExpr v t | (AnnG (VarE v) t) <- g'] ++ reverse es'
  return $ (g', map (generalizeE . unrenameExpr . applyE g') es'')

toExpr :: EVar -> TypeSet -> [Expr]
toExpr v (TypeSet (Just e) es) = [Signature v t | t <- (e : es)]
toExpr v (TypeSet Nothing es) = [Signature v t | t <- es]

typecheckExpr' :: Gamma -> [Expr] -> [Expr] -> Stack (Gamma, [Expr])
typecheckExpr' g es [] = return (g, es)
typecheckExpr' g es (x:xs) = do
  (g', _, e') <- infer g x
  case e' of
    (Signature _ _) -> typecheckExpr' g' es xs
    _ -> typecheckExpr' g' (e' : es) xs

renameExpr :: Expr -> Stack Expr
renameExpr = mapT' renameType

renameType :: Type -> Stack Type
renameType UniT = return UniT
renameType t@(VarT _) = return t
renameType t@(ExistT _) = return t
renameType (Forall v t) = do
  v' <- newqul v
  t' <- renameType (substitute' v (VarT v') t)
  return $ Forall v' t'
renameType (FunT t1 t2) = FunT <$> renameType t1 <*> renameType t2
renameType (ArrT v ts) = ArrT <$> pure v <*> mapM renameType ts
renameType (RecT rs) =
  RecT <$> mapM (\(x, t) -> (,) <$> pure x <*> renameType t) rs

unrenameExpr :: Expr -> Expr
unrenameExpr = mapT unrenameType

unrename :: TVar -> TVar
unrename (TV t) = TV . head $ MT.splitOn "." t

unrenameType :: Type -> Type
unrenameType UniT = UniT
unrenameType (VarT v) = VarT (unrename v)
unrenameType t@(ExistT _) = t
unrenameType (Forall v t) = Forall (unrename v) (unrenameType t)
unrenameType (FunT t1 t2) = FunT (unrenameType t1) (unrenameType t2)
unrenameType (ArrT v ts) = ArrT v (map unrenameType ts)
unrenameType (RecT rs) = RecT [(x, unrenameType t) | (x, t) <- rs]

-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute' :: TVar -> Type -> Type -> Type
substitute' v r t = sub t
  where
    sub :: Type -> Type
    sub t'@(VarT v')
      | v == v' = r
      | otherwise = t'
    sub (FunT t1 t2) = FunT (sub t1) (sub t2)
    sub t'@(Forall x t'')
      | v /= x = Forall x (sub t'')
      | otherwise = t' -- allows shadowing of the variable
    sub (ArrT v' ts) = ArrT v' (map sub ts)
    sub (RecT rs) = RecT [(x, sub t') | (x, t') <- rs]
    sub t' = t'

-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> Type -> Type
substitute v t = substitute' v (ExistT v) t

-- | Apply a context to a type (See Dunfield Figure 8).
apply :: Gamma -> Type -> Type
-- [G]l = l
apply _ UniT = UniT
-- [G]a = a
apply _ a@(VarT _) = a
-- [G](A->B) = ([G]A -> [G]B)
apply g (FunT a b) = FunT (apply g a) (apply g b)
-- [G]Forall a.a = forall a. [G]a
apply g (Forall x a) = Forall x (apply g a)
-- [G[a=t]]a = [G[a=t]]t
apply g a@(ExistT v) =
  case lookupT v g of
    (Just t') -> apply g t' -- reduce an existential; strictly smaller term
    Nothing -> a
apply g (ArrT v ts) = ArrT v (map (apply g) ts)
apply g (RecT rs) = RecT (map (\(n, t) -> (n, apply g t)) rs)

applyE :: Gamma -> Expr -> Expr
applyE g e = mapT (apply g) e

occursCheck :: Type -> Type -> Stack ()
occursCheck t1 t2 =
  case Set.member t1 (free t2) of
    True -> throwError OccursCheckFail
    False -> return ()

free :: Type -> Set.Set Type
free UniT = Set.empty
free v@(VarT _) = Set.singleton v
free v@(ExistT _) = Set.singleton v
free (FunT t1 t2) = Set.union (free t1) (free t2)
free (Forall v t) = Set.delete (VarT v) (free t)
free (ArrT _ xs) = Set.unions (map free xs)
free (RecT rs) = Set.unions [free t | (_, t) <- rs]

-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: Type -> Type -> Gamma -> Stack Gamma
subtype t1 t2 g = do subtype' t1 t2 g

--
-- ----------------------------------------- Unit
--  G |- 1 <: 1 -| G
subtype' UniT UniT g = return g
--
-- ----------------------------------------- <:Var
--  G[a] |- a <: a -| G[a]
subtype' t1@(VarT a1) t2@(VarT a2) g = do
  if (a1 == a2)
    then return g
    else throwError $ SubtypeError t1 t2
subtype' a@(ExistT a1) b@(ExistT a2) g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- Ea <: Ea -| G[E.a]
  | a1 == a2 = return g
  --
  -- ----------------------------------------- <:InstantiateL/<:InstantiateR
  --  G[E.a] |- Ea <: Ea -| G[E.a]
  | otherwise
      -- formally, an `Ea notin FV(G)` check should be done here, but since the
      -- types involved are all existentials, it will always pass, so I omit
      -- it.
   = instantiate a b g
--  g1 |- B1 <: A1 -| g2
--  g2 |- [g2]A2 <: [g2]B2 -| g3
-- ----------------------------------------- <:-->
--  g1 |- A1 -> A2 <: B1 -> B2 -| g3
subtype' (FunT a1 a2) (FunT b1 b2) g1
  -- function subtypes are *contravariant* with respect to the input, that is,
  -- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
 = do
  g2 <- subtype b1 a1 g1
  subtype (apply g2 a2) (apply g2 b2) g2
--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype' (ArrT v1 []) (ArrT v2 []) g = subtype (VarT v1) (VarT v2) g
subtype' (ArrT v1 vs1) (ArrT v2 vs2) g = do
  subtype (VarT v1) (VarT v2) g
  compareArr vs1 vs2 g
  where
    compareArr :: [Type] -> [Type] -> Gamma -> Stack Gamma
    compareArr [] [] g' = return g'
    compareArr (t1':ts1') (t2':ts2') g' = do
      g'' <- subtype t1' t2' g'
      compareArr ts1' ts2' g''
    compareArr _ _ _ = throwError TypeMismatch
-- subtype unordered records
subtype' (RecT rs1) (RecT rs2) g = compareEntry (sort rs1) (sort rs2) g
  where
    compareEntry :: [(TVar, Type)] -> [(TVar, Type)] -> Gamma -> Stack Gamma
    compareEntry [] [] g2 = return g2
    compareEntry ((v1, t1):rs1') ((v2, t2):rs2') g2 = do
      g3 <- subtype (VarT v1) (VarT v2) g2
      g4 <- subtype t1 t2 g3
      compareEntry rs1' rs2' g4
    compareEntry _ _ _ = throwError TypeMismatch
--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype' a b@(ExistT _) g = occursCheck a b >> instantiate a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype' a@(ExistT _) b g = occursCheck b a >> instantiate a b g
--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
subtype' (Forall x a) b g =
  subtype (substitute x a) b (g +> MarkG x +> ExistG x) >>= cut (MarkG x)
--  g1,a |- A :> B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype' a (Forall v b) g = subtype a b (g +> VarG v) >>= cut (VarG v)
subtype' a b _ = throwError $ SubtypeError a b

-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: Type -> Type -> Gamma -> Stack Gamma
--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- A1 <=: Ea1 -| g2
--  g2 |- Ea2 <=: [g2]A2 -| g3
-- ----------------------------------------- InstLArr
--  g1[Ea] |- Ea <=: A1 -> A2 -| g3
instantiate ta@(ExistT v) (FunT t1 t2) g1 = do
  ea1 <- newvar
  ea2 <- newvar
  g2 <-
    case access1 ta g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunT ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError UnknownError
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRArr
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate (FunT t1 t2) tb@(ExistT v) g1 = do
  ea1 <- newvar
  ea2 <- newvar
  g2 <-
    case access1 tb g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunT ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError UnknownError
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--
-- ----------------------------------------- InstLAllR
--
instantiate ta@(ExistT _) (Forall v2 t2) g1 =
  instantiate ta t2 (g1 +> VarG v2) >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate ta@(ExistT v1) tb@(ExistT v2) g1 = do
  _ <- return ()
  case access2 ta tb g1 of
    -- InstLReach
    (Just (ls, _, ms, x, rs)) -> return $ ls <> (SolvedG v1 tb : ms) <> (x : rs)
    Nothing ->
      case access2 tb ta g1 of
      -- InstRReach
        (Just (ls, _, ms, x, rs)) ->
          return $ ls <> (SolvedG v2 ta : ms) <> (x : rs)
        Nothing -> return g1
--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate (Forall x b) tb@(ExistT _) g1 =
  do instantiate
       (substitute x b) -- [Eb/x]B
       tb -- Ea
       (g1 +> MarkG x +> ExistG x) -- g1[Ea],>Eb,Eb
     >>= cut (MarkG x)
--  g1 |- t
-- ----------------------------------------- InstRSolve
--  g1,Ea,g2 |- t <=: Ea -| g1,Ea=t,g2
instantiate ta tb@(ExistT v) g1 =
  case access1 tb g1 of
    (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v ta) : rs
    Nothing ->
      case access1 (SolvedG v ta) g1 of
        (Just _) -> return g1
        Nothing ->
          throwError . OtherError $
          "Error in InstRSolve: ta=(" <>
          MT.show' ta <> ") tb=(" <> MT.show' tb <> ") g1=(" <> MT.show' g1 <> ")"
--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate ta@(ExistT v) tb g1 = do
  case access1 ta g1 of
    (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v tb) : rs
    Nothing ->
      case access1 (SolvedG v tb) g1 of
        (Just _) -> return g1
        Nothing -> error "error in InstLSolve"
-- bad
instantiate _ _ g = return g

applyConcrete :: Expr -> Expr -> Type -> Stack Expr
applyConcrete (AnnE e1 _) e2@(AnnE _ a) c =
  return $ AnnE (AppE (AnnE e1 (FunT a c)) e2) c
applyConcrete e1 e2 t =
  throwError . OtherError $
  "Expected annotatated types in applyConcrete, got:\n  > " <>
  MT.show' e1 <> "\n  > " <> MT.show' e2 <> "\n  > " <> MT.show' t

isAnnG :: EVar -> GammaIndex -> Bool
isAnnG e1 (AnnG (VarE e2) _)
  | e1 == e2 = True
  | otherwise = False
isAnnG _ _ = False

appendTypeSet :: EType -> TypeSet -> Stack TypeSet
appendTypeSet e1 s =
  case (elang e1, s) of
  -- if e is a general type, and there is no conflicting type, then set e
    (Nothing, TypeSet Nothing rs) -> do
      mapM_ (checkRealization e1) rs
      return $ TypeSet (Just e1) rs
  -- if e is a realization, and no general type is set, just add e to the list
    (Just _, TypeSet Nothing rs) -> return $ TypeSet Nothing (e1 : rs)
  -- if e is a realization, and a general type exists, append it and check
    (Just _, TypeSet (Just e2) rs) -> do
      checkRealization e2 e1
      return $ TypeSet (Just e2) (e1 : rs)
  -- if e is general, and a general type exists, merge the general types
    (Nothing, TypeSet (Just e2) rs) -> do
      let e3 =
            EType
              { elang = Nothing
              , etype = etype e2
              , eprop = Set.union (eprop e1) (eprop e2)
              , econs = Set.union (econs e1) (econs e2)
              , esource = Nothing
              }
      return $ TypeSet (Just e3) rs

checkRealization :: EType -> EType -> Stack ()
checkRealization e1 e2 = f' (etype e1) (etype e2)
  where
    f' :: Type -> Type -> Stack ()
    f' (FunT x1 y1) (FunT x2 y2) = f' x1 x2 >> f' y1 y2
    f' (Forall _ x) (Forall _ y) = f' x y
    f' (Forall _ x) y = f' x y
    f' x (Forall _ y) = f' x y
    f' (FunT _ _) _ = throwError BadRealization
    f' _ (FunT _ _) = throwError BadRealization
    f' _ _ = return ()

chainInfer ::
     Gamma -> [Expr] -> [Type] -> [Expr] -> Stack (Gamma, [Type], [Expr])
chainInfer g [] ts es = return (g, ts, es)
chainInfer g (x:xs) ts es = do
  (g', t', e') <- infer g x
  chainInfer g' xs (t' : ts) (e' : es)

addSource :: Gamma -> EVar -> EType -> Stack EType
addSource g v e =
  case elang e of
    (Just l) ->
      case lookupSrc (v, l) g of
        (Just (_, _, srcfile, srcname)) ->
          return $ e {esource = Just (srcfile, srcname)}
        Nothing -> return e -- FIXME: technically, this should raise MissingSource
    Nothing -> return e

infer ::
     Gamma
  -> Expr -- ^ A subexpression from the original expression
  -> Stack ( Gamma
           , Type -- The return type
           , Expr -- The annotated expression
            )
infer g e = do infer' g e

-- --
-- ----------------------------------------- <primitive>
--  g |- <primitive expr> => <primitive type> -| g
-- --
infer' g UniE = return (g, UniT, ann UniE UniT)
-- Num=>
infer' g e@(NumE _) = return (g, t, ann e t)
  where
    t = VarT (TV "Num")
-- Str=> 
infer' g e@(StrE _) = return (g, t, ann e t)
  where
    t = VarT (TV "Str")
-- Log=> 
infer' g e@(LogE _) = return (g, t, ann e t)
  where
    t = VarT (TV "Bool")
-- Declaration=>
infer' g (Declaration v e) = do
  (g2, t1, e2) <- infer (g +> MarkEG v) e
  g3 <- cut (MarkEG v) g2
  (g4, t2, e3) <-
    case lookupE (VarE v) g >>= toType of
      (Just t) -> check g2 e2 t
      Nothing -> return (g3, t1, e2)
  let t3 = generalize t2
      g5 = g4 +> AnnG (VarE v) (fromType t3)
  return (g5, t3, Declaration v (generalizeE e3))
-- Signature=>
infer' g (Signature v e) = do
  e' <- addSource g v e
  (left, r3, right) <-
    case findIndex (isAnnG v) g of
      (Just i) ->
        case (i, g !! i) of
          (0, AnnG _ r2) ->
            appendTypeSet e' r2 >>= (\x -> return ([], x, tail g))
          (_, AnnG _ r2) ->
            appendTypeSet e' r2 >>= (\x -> return (take i g, x, drop (i + 1) g))
          (_, _) -> throwError $ OtherError "bad Gamma"
      Nothing ->
        case elang e' of
          (Just _) -> return (g, TypeSet Nothing [e'], [])
          Nothing -> return (g, TypeSet (Just e') [], [])
  return (left ++ (AnnG (VarE v) r3) : right, UniT, Signature v e')
infer' g1 s1@(SrcE l f xs) = do
  let g3 = srcList g1 xs
  return (g3, UniT, s1)
  where
    srcList :: Gamma -> [(EVar, EVar)] -> Gamma
    srcList g2 [] = g2
    srcList g2 ((e1, e2):rs) = srcList (g2 +> SrcG (e2, l, f, e1)) rs
--  (x:A) in g
-- ------------------------------------------- Var
--  g |- x => A -| g
infer' g e@(VarE v) = do
  case lookupE e g >>= toType of
    (Just t) -> return (g, t, ann e t)
    Nothing -> throwError (UnboundVariable v)
--  g1,Ea,Eb,x:Ea |- e <= Eb -| g2,x:Ea,g3
-- ----------------------------------------- -->I=>
--  g1 |- \x.e => Ea -> Eb -| g2
infer' g1 (LamE v e2) = do
  a <- newvar
  b <- newvar
  let anng = AnnG (VarE v) (fromType a)
      g2 = g1 +> a +> b +> anng
  (g3, t1, e2') <- check g2 e2 b
  case lookupE (VarE v) g3 >>= toType of
    (Just t2) -> do
      let t3 = FunT (apply g3 t2) t1
      g4 <- cut anng g3
      return (g4, t3, ann (LamE v e2') t3)
    Nothing -> throwError UnknownError
--  g1 |- e1 => A -| g2
--  g2 |- [g2]A o e2 =>> C -| g3
-- ----------------------------------------- -->E
--  g1 |- e1 e2 => C -| g3
infer' g1 (AppE e1 e2) = do
  (g2, a, e1') <- infer g1 e1
  (g3, c, e2') <- derive g2 e2 (apply g2 a)
  e3 <- applyConcrete e1' e2' c
  return (g3, c, e3)
--  g1 |- A
--  g1 |- e <= A -| g2
-- ----------------------------------------- Anno
--  g1 |- (e:A) => A -| g2
infer' g e1@(AnnE e@(VarE _) t)
  -- This is a bit questionable. If a single variable is annotated, e.g.
  -- `x::Int`, and is not declared, this would normally raise an
  -- UnboundVariable error. However, it is convenient for testing purposes, and
  -- also for Morloc where functions are imported as black boxes from other
  -- languages, to be able to simply declare a type as an axiom. Perhaps I
  -- should add dedicated syntax for axiomatic type declarations?
 = case lookupE e g of
    (Just _) -> check g e t
    Nothing -> return (g, t, e1)
infer' g (AnnE e t) = check g e t
infer' g e1@(ListE []) = do
  t <- newvar
  let t' = ArrT (TV "List") [t]
  return (g +> t, t', ann e1 t')
infer' g1 e1@(ListE (x:xs)) = do
  (g2, t', _) <- infer g1 x
  g3 <- foldM (quietCheck t') g2 xs
  let t'' = ArrT (TV "List") [t']
  return (g3, t'', ann e1 t'')
infer' _ (TupleE []) = throwError EmptyTuple
infer' _ (TupleE [_]) = throwError TupleSingleton
infer' g (TupleE xs) = do
  (g', ts, es) <- chainInfer g (reverse xs) [] []
  let v = TV . MT.pack $ "Tuple" ++ (show (length xs))
      t = ArrT v ts
      e = TupleE es
  return (g', t, AnnE e t)
-- ----------------------------------------- -->Rec
infer' _ (RecE []) = throwError EmptyRecord
infer' g1 e@(RecE rs) = do
  (g2, ts, _) <- chainInfer g1 (reverse $ map snd rs) [] []
  let t = RecT (zip [TV x | (EV x, _) <- rs] ts)
  return (g2, t, AnnE e t)

quietCheck :: Type -> Gamma -> Expr -> Stack Gamma
quietCheck t g e = do
  (g', _, _) <- check g e t
  return g'

-- | Pattern matches against each type
check ::
     Gamma
  -> Expr -- ^ An expression which should be of the type given
  -> Type -- ^ The expected type of the expression
  -> Stack ( Gamma
           , Type -- The inferred type of the expression
           , Expr -- The annotated expression
            )
check g e t = check' g e t

--
-- ----------------------------------------- 1l
--  g |- () <= 1 -| g
check' g UniE UniT = return (g, UniT, ann UniE UniT)
-- 1l-error
check' _ _ UniT = throwError TypeMismatch
--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
check' g (LamE v e) (FunT a b)
  -- define x:A
 = do
  let anng = AnnG (VarE v) (fromType a)
  -- check that e has the expected output type
  (g', t', e') <- check (g +> anng) e b
  -- ignore the trailing context and (x:A), since it is out of scope
  g2 <- cut anng g'
  let t'' = FunT a t'
  return (g2, t'', ann (LamE v e') t'')
--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
check' g1 e r2@(Forall x a) = do
  (g', _, e') <- check (g1 +> VarG x) e a
  g2 <- cut (VarG x) g'
  let t'' = apply g2 r2
  return (g2, t'', ann e' t'')
--  g1 |- e => A -| g2
--  g2 |- [g2]A <: [g2]B -| g3
-- ----------------------------------------- Sub
--  g1 |- e <= B -| g3
check' g1 e b = do
  (g2, a, e') <- infer g1 e
  g3 <- subtype (apply g2 a) (apply g2 b) g2
  let a' = apply g3 a
  return (g3, a', ann (applyE g3 e') a')

derive ::
     Gamma
  -> Expr -- the expression that is passed to the function
  -> Type -- the function type
  -> Stack ( Gamma
           , Type -- @b@, the function output type after context application
           , Expr -- @e@, with type annotation
            )
derive g e t = do derive' g e t

--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
derive' g e (FunT a b) = do
  (g', _, e') <- check g e a
  return (g', apply g' b, e')
--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
derive' g e (Forall x s) = derive (g +> ExistG x) e (substitute x s)
--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
derive' g e t@(ExistT v) = do
  ea1 <- newvar
  ea2 <- newvar
  let t' = FunT ea1 ea2
  g2 <-
    case access1 t g
    -- replace <t0> with <t0>:<ea1> -> <ea2>
          of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v t', index ea1, index ea2] ++ ls
      Nothing -> throwError UnknownError
  (g3, _, e2) <- check g2 e ea1
  return (g3, apply g3 ea2, e2)
derive' _ _ _ = throwError NonFunctionDerive
