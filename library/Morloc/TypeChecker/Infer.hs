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
  , free
  , infer
  , rename
  , unrename
  ) where

import Morloc.Namespace
import Morloc.TypeChecker.Util
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.Reader as R

import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)

typecheck :: [Module] -> Stack [Module]
typecheck ms = do
  mods <- foldM insertWithCheck Map.empty [(moduleName m, m) | m <- ms]
  -- graph :: Map MVar (Set MVar)
  let graph = Map.fromList $ map mod2pair ms
  mods' <- path graph
  case mapM (flip Map.lookup $ mods) mods' of
    (Just mods'') -> fmap reverse $ typecheckModules (Map.empty) mods''
    Nothing -> throwError $ OtherError "bad thing #1"
  where
    mod2pair :: Module -> (MVar, Set.Set MVar)
    mod2pair m =
      (moduleName m, Set.fromList $ map importModuleName (moduleImports m))

enter :: Doc AnsiStyle -> Stack ()
enter d = do
  depth <- incDepth
  debugLog $ pretty (take depth (repeat '-')) <> ">" <+> align d <> "\n"

say :: Doc AnsiStyle -> Stack ()
say d = do
  depth <- getDepth
  debugLog $ pretty (take depth (repeat ' ')) <> ":" <+> align d <> "\n"

seeGamma :: Gamma -> Stack ()
seeGamma g = say $ nest 4 $ "Gamma:" <> line <> (vsep (map prettyGammaIndex g))

leave :: Doc AnsiStyle -> Stack ()
leave d = do
  depth <- decDepth
  debugLog $ "<" <> pretty (take depth (repeat '-')) <+> align d <> "\n"

debugLog :: Doc AnsiStyle -> Stack ()
debugLog d = do
  verbosity <- R.asks stackConfigVerbosity 
  if verbosity > 0
    then (liftIO . putDoc) d
    else return ()

typecheckModules :: ModularGamma -> [Module] -> Stack [Module]
typecheckModules _ [] = return []
typecheckModules mg (m:ms) = do
  enter $ "entering module '" <> viaShow (moduleName m) <> "'"
  g <- importFromModularGamma mg m
  (g', exprs) <- typecheckExpr g (moduleBody m)
  (privateMap, mg') <- extendModularGamma g' m mg
  mods <- typecheckModules mg' ms
  leave $ "module"
  return (m {moduleBody = exprs, moduleTypeMap = privateMap} : mods)

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
  es <- fmap sort (mapM rename e)
  (g', es') <- typecheckExpr' g [] es
  let es'' = concat [toExpr v t | (AnnG (VarE v) t) <- g'] ++ reverse es'
  return $ (g', map (generalizeE . unrename . apply g') es'')
  where
    toExpr :: EVar -> TypeSet -> [Expr]
    toExpr v (TypeSet (Just e) es) = [Signature v t | t <- (e : es)]
    toExpr v (TypeSet Nothing es) = [Signature v t | t <- es]

typecheckExpr' :: Gamma -> [Expr] -> [Expr] -> Stack (Gamma, [Expr])
typecheckExpr' g es [] = return (g, es)
typecheckExpr' g es (x:xs) = do
  (g', _, e') <- infer Nothing g x
  case e' of
    (Signature _ _) -> typecheckExpr' g' es xs
    _ -> typecheckExpr' g' (e' : es) xs


class Renameable a where
  rename :: a -> Stack a
  unrename :: a -> a

instance Renameable Expr where
  rename = mapT' rename
  unrename = mapT unrename

instance Renameable Type where
  rename t@(VarT _) = return t
  rename (ExistT v ts) = ExistT <$> pure v <*> (mapM rename ts) 
  rename (Forall v t) = do
    v' <- rename v
    t' <- rename (substitute' v (VarT v') t)
    return $ Forall v' t'
  rename (FunT t1 t2) = FunT <$> rename t1 <*> rename t2
  rename (ArrT v ts) = ArrT <$> pure v <*> mapM rename ts
  rename (RecT rs) =
    RecT <$> mapM (\(x, t) -> (,) <$> pure x <*> rename t) rs

  unrename (VarT v) = VarT (unrename v)
  unrename (ExistT v ts) = ExistT v (map unrename ts)
  unrename (Forall v t) = Forall (unrename v) (unrename t)
  unrename (FunT t1 t2) = FunT (unrename t1) (unrename t2)
  unrename (ArrT v ts) = ArrT v (map unrename ts)
  unrename (RecT rs) = RecT [(x, unrename t) | (x, t) <- rs]

instance Renameable TVar where
  unrename (TV l t) = TV l . head $ MT.splitOn "." t
  rename = newqul


class Applicable a where
  apply :: Gamma -> a -> a

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable Type where
  -- [G]a = a
  apply _ a@(VarT _) = a
  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunT a b) = FunT (apply g a) (apply g b)
  -- [G]Forall a.a = forall a. [G]a
  apply g (Forall x a) = Forall x (apply g a)
  -- [G[a=t]]a = [G[a=t]]t
  apply g a@(ExistT v []) =
    case lookupT v g of
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> a
  apply g (ExistT v ts) =
    case lookupT v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t'
      Nothing -> ExistT v (map (apply g) ts)
  apply g (ArrT v ts) = ArrT v (map (apply g) ts)
  apply g (RecT rs) = RecT (map (\(n, t) -> (n, apply g t)) rs)

instance Applicable Expr where
  apply g e = mapT (apply g) e

instance Applicable EType where
  apply g e = e { etype = apply g (etype e) }

class Typed a where
  toType :: Maybe Lang -> a -> Maybe Type
  fromType :: Maybe Lang -> Type -> a

instance Typed EType where
  toType lang e
    | (langOf . etype) e == lang = Just (etype e)
    | otherwise = Nothing
  fromType lang t =
    EType
      { etype = t
      , eprop = Set.empty
      , econs = Set.empty
      , esource = Nothing
      }

instance Typed TypeSet where
  toType Nothing (TypeSet e _) = e >>= toType Nothing
  toType lang (TypeSet _ ts) = case filter (\e -> (langOf . etype) e == lang) ts of 
    [ ] -> Nothing
    [e] -> Just (etype e)
    _ -> error "a typeset can contain only one instance of each language"

  fromType Nothing t = TypeSet (Just (fromType Nothing t)) []
  fromType lang t = TypeSet Nothing [fromType lang t]

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
substitute v t = substitute' v (ExistT v []) t


-- | TODO: document
occursCheck :: Type -> Type -> Stack ()
occursCheck t1 t2 = do
  say $ "occursCheck:" <+> prettyGreenType t1 <+> prettyGreenType t2
  case Set.member t1 (free t2) of
    True -> throwError OccursCheckFail
    False -> return ()


-- | TODO: document
free :: Type -> Set.Set Type
free v@(VarT _) = Set.singleton v
free v@(ExistT _ []) = Set.singleton v
free (ExistT v ts) = Set.unions $ Set.singleton (ArrT v ts) : map free ts
free (FunT t1 t2) = Set.union (free t1) (free t2)
free (Forall v t) = Set.delete (VarT v) (free t)
free (ArrT _ xs) = Set.unions (map free xs)
free (RecT rs) = Set.unions [free t | (_, t) <- rs]

-- | fold a list of annotated expressions into one, preserving annotations
collate :: [Expr] -> Stack Expr
collate [] = throwError . OtherError $ "Nothing to collate"
collate (e:es) = do
  say $ "collating" <+> (align . vsep . map prettyExpr) (e:es)
  foldM collateOne e es

-- | Merge two annotated expressions into one, fail if the expressions are not
-- equivalent.
collateOne :: Expr -> Expr -> Stack Expr
collateOne (AnnE e1 ts1) (AnnE e2 ts2) = AnnE <$> collateOne e1 e2 <*> pure (nub $ ts1 ++ ts2)
-- 
collateOne (AppE e11 e12) (AppE e21 e22) = AppE <$> collateOne e11 e21 <*> collateOne e12 e22
collateOne (LamE v1 e1) (LamE v2 e2)
  | v1 == v2 = LamE <$> pure v1 <*> collateOne e1 e2
  | otherwise = throwError $ OtherError "collate error #1"
collateOne e@(VarE v1) (VarE v2)
  | v1 == v2 = return e
  | otherwise = throwError $ OtherError "collate error #2"
-- primitives
collateOne e@UniE UniE = return e
collateOne e@(LogE _) (LogE _) = return e
collateOne e@(NumE _) (NumE _) = return e
collateOne e@(StrE _) (StrE _) = return e
-- containers
collateOne (ListE es1) (ListE es2) = ListE <$> zipWithM collateOne es1 es2
collateOne (TupleE es1) (TupleE es2) = TupleE <$> zipWithM collateOne es1 es2
collateOne (RecE es1) (RecE es2)
  = RecE <$> (
          zip
      <$> zipWithM returnIfEqual (map fst es1) (map fst es2)
      <*> zipWithM collateOne (map snd es1) (map snd es2)
    )
  where
    returnIfEqual :: Eq a => a -> a -> Stack a
    returnIfEqual x y
      | x == y = return x
      | otherwise = throwError $ OtherError "expected them to be equal"
-- illegal
collateOne (Signature _ _) (Signature _ _) = error "the hell's a toplevel doing down here?"
collateOne (Declaration _ _) (Declaration _ _) = error "the hell's is a toplevel doing down here?"
collateOne (SrcE _ _ _) (SrcE _ _ _) = error "the hell's is a toplevel doing down here?"
collateOne e1 e2 = error $ "collation failure: (" <> show e1 <> ")  (" <> show e2 <> ")"


-- | TODO: document
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

checkup :: Gamma -> Expr -> Type -> Stack (Gamma, [Type], Expr)
checkup g e t = do
  say "checkup"
  (g', t', e') <- check g e t
  return (g', [t'], e')

typesetFromList :: [Type] -> Stack TypeSet
typesetFromList ts = do 
  say "typesetFromList"
  let gentype = [makeEType t | t <- ts, (isNothing . langOf) t]
      contype = [makeEType t | t <- ts, (isJust . langOf) t]
  case (gentype, contype) of
    ([x], cs) -> return $ TypeSet (Just x) cs
    ([], cs) -> return $ TypeSet Nothing cs
    _ -> throwError $ OtherError "ambiguous general type"
  where
    makeEType :: Type -> EType
    makeEType t = EType
      { etype = t
      , eprop = Set.empty
      , econs = Set.empty
      , esource = Nothing
      }

-- | TODO: document - allow things other than general
chainInfer :: Gamma -> [Expr] -> Stack (Gamma, [Type], [Expr])
chainInfer g0 es0 = do
  say "chainInfer"
  chainInfer' g0 (reverse es0) [] []
  where
    chainInfer' ::
         Gamma -> [Expr] -> [Type] -> [Expr] -> Stack (Gamma, [Type], [Expr])
    chainInfer' g [] ts es = return (g, ts, es)
    chainInfer' g (x:xs) ts es = do
      (g', [t'], e') <- infer Nothing g x
      chainInfer' g' xs (t' : ts) (e' : es)

-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: Type -> Type -> Gamma -> Stack Gamma
subtype t1 t2 g = do
  enter $ prettyGreenType t1 <+> ":>" <+> prettyGreenType t2
  g' <- subtype' t1 t2 g
  leave "subtype"
  return g'

-- VarT vs VarT
subtype' t1@(VarT (TV lang1 a1)) t2@(VarT (TV lang2 a2)) g
  -- If everything is the same, do nothing
  --
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l <: a_l -| G[a]
  | lang1 == lang2 && a1 == a2 = return g
  -- If languages are different, do nothing
  --  l1 != l2    b_l2 ~~> a_l1
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l1 <: b_l2 -| G[a]
  | lang1 /= lang2 = serialConstraint t1 t2 >> return g
  -- If languages are same, but types are different, raise error
  | lang1 == lang2 && a1 /= a2 = throwError $ SubtypeError t1 t2

subtype' a@(ExistT (TV l1 _) []) b@(ExistT (TV l2 _) []) g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- E.a <: E.a -| G[E.a]
  | a == b = return g
  --  l1 == l2
  -- ----------------------------------------- <:AlienExvar
  --  G[E.a,E.b] |- E.a <: E.b -| G[E.a,E.b], E.a ~~> E.b
  | l1 /= l2 = return $ g +> UnsolvedConstraint a b
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
subtype' t1@(ArrT v1@(TV l1 _) vs1) t2@(ArrT v2@(TV l2 _) vs2) g
  | length vs1 /= length vs2 = throwError . OtherError
    $ "Cannot subtype types with unequal parameter count" 
  | l1 /= l2 = serialConstraint t1 t2 >> return g
  | v1 == v2 = compareArr vs1 vs2 g
  | otherwise = do
    case access1 v1 g of
      (Just (lhs, (ExistG v1' vs1'), rhs)) -> do
        let t1' = ArrT v2 vs1'
        let g2 = lhs ++ (SolvedG v1 t1' : rhs)
        subtype t1' t2 g2
      Nothing -> case access1 v2 g of
        (Just (lhs, (ExistG v2' vs2'), rhs)) -> do
          let t2' = ArrT v1 vs2'
          let g2 = lhs ++ (SolvedG v2 t2' : rhs)
          subtype t1 t2' g2
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
    compareEntry ((v1@(TV l1 _), t1):rs1') ((v2@(TV l2 _), t2):rs2') g2
      | l1 == l2 = do
          g3 <- subtype (VarT v1) (VarT v2) g2
          g4 <- subtype t1 t2 g3
          compareEntry rs1' rs2' g4
      | l1 /= l2 = serialConstraint t1 t2 >> return g
    compareEntry _ _ _ = throwError TypeMismatch

--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype' a b@(ExistT _ _) g
  | langOf a /= langOf b = return g
  | otherwise = occursCheck a b >> instantiate a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype' a@(ExistT _ _) b g
  | langOf a /= langOf b = return g
  | otherwise = occursCheck b a >> instantiate a b g

--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
--
subtype' (Forall v@(TV lang _) a) b g
  | lang /= langOf b = return g
  | otherwise = subtype (substitute v a) b (g +> MarkG v +> ExistG v [])
              >>= cut (MarkG v)
--  g1,a |- A :> B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype' a (Forall v@(TV lang _) b) g
  | lang /= langOf a = return g
  | otherwise = subtype a b (g +> VarG v) >>= cut (VarG v)
subtype' a b _ = throwError $ SubtypeError a b


-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: Type -> Type -> Gamma -> Stack Gamma
instantiate t1 t2 g = do
  say $ prettyGreenType t1 <+> "<=:" <+> prettyGreenType t2
  g <- instantiate' t1 t2 g 
  say $ "instantiate done"
  return g

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- A1 <=: Ea1 -| g2
--  g2 |- Ea2 <=: [g2]A2 -| g3
-- ----------------------------------------- InstLArr
--  g1[Ea] |- Ea <=: A1 -> A2 -| g3
instantiate' ta@(ExistT v@(TV lang _) []) (FunT t1 t2) g1 = do
  ea1 <- newvar lang
  ea2 <- newvar lang
  g2 <-
    case access1 v g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunT ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError $ OtherError "Bad thing #2"
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRArr
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate' (FunT t1 t2) tb@(ExistT v@(TV lang _) []) g1 = do
  ea1 <- newvar lang
  ea2 <- newvar lang
  g2 <-
    case access1 v g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunT ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError $ OtherError "Bad thing #3"
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--
-- ----------------------------------------- InstLAllR
--
instantiate' ta@(ExistT _ []) tb@(Forall v2 t2) g1
  | langOf ta /= langOf tb = return g1
  | otherwise = instantiate ta t2 (g1 +> VarG v2) >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate' ta@(ExistT v1 []) tb@(ExistT v2 []) g1 = do
  _ <- return ()
  case access2 v1 v2 g1 of
    -- InstLReach
    (Just (ls, _, ms, x, rs)) -> return $ ls <> (SolvedG v1 tb : ms) <> (x : rs)
    Nothing ->
      case access2 v2 v1 g1 of
      -- InstRReach
        (Just (ls, _, ms, x, rs)) ->
          return $ ls <> (SolvedG v2 ta : ms) <> (x : rs)
        Nothing -> return g1
--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate' ta@(Forall x b) tb@(ExistT _ []) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      instantiate
        (substitute x b) -- [Eb/x]B
        tb -- Ea
        (g1 +> MarkG x +> ExistG x []) -- g1[Ea],>Eb,Eb
      >>= cut (MarkG x)
--  g1 |- t
-- ----------------------------------------- InstRSolve
--  g1,Ea,g2 |- t <=: Ea -| g1,Ea=t,g2
instantiate' ta tb@(ExistT v []) g1 =
  case access1 v g1 of
    (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v ta) : rs
    Nothing ->
      case lookupT v g1 of
        (Just _) -> return g1
        Nothing ->
          throwError . OtherError $
          "Error in InstRSolve: ta=(" <>
          MT.show' ta <> ") tb=(" <> MT.show' tb <> ") g1=(" <> MT.show' g1 <> ")"
--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate' ta@(ExistT v []) tb g1 = do
  case access1 v g1 of
    (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v tb) : rs
    Nothing ->
      case lookupT v g1 of
        (Just _) -> return g1
        Nothing -> error "error in InstLSolve"
-- bad
instantiate' _ _ g = return g

infer ::
     Maybe Lang
  -> Gamma
  -> Expr -- ^ A subexpression from the original expression
  -> Stack ( Gamma
           , [Type] -- The return types
           , Expr -- The annotated expression
           )
infer l g e = do
  enter $ "infer" <+> maybe "MLang" (viaShow . id) l <+> parens (prettyExpr e)
  seeGamma g
  o@(_, ts, _) <- infer' l g e
  leave $ "infer |-" <+> encloseSep "(" ")" ", " (map prettyGreenType ts)
  return o

--
-- ----------------------------------------- <primitive>
--  g |- <primitive expr> => <primitive type> -| g
-- 
-- Num=>
infer' lang@(Just _) g e@(NumE _) = do
  v <- newvar lang
  return (g +> v, [v], ann e v) 
infer' Nothing g e@(NumE _) = return (g, [t], ann e t)
  where
    t = VarT (TV Nothing "Num")
-- Str=> 
infer' lang@(Just _) g e@(StrE _) = do
  v <- newvar lang
  return (g +> v, [v], ann e v) 
infer' Nothing g e@(StrE _) = return (g, [t], ann e t)
  where
    t = VarT (TV Nothing "Str")
-- Log=> 
infer' lang@(Just _) g e@(LogE _) = do
  v <- newvar lang
  return (g +> v, [v], ann e v) 
infer' Nothing g e@(LogE _) = return (g, [t], ann e t)
  where
    t = VarT (TV Nothing "Bool")

-- Src=>
-- Since the expressions in a Morloc script are sorted before being
-- evaluated, the SrcE expressions will be considered before the Signature
-- and Declaration expressions. Thus every term that originates in source
-- code will be initialized here and elaborated upon with deeper type
-- information as the signatures and declarations are parsed. 
infer' (Just _) _ (SrcE _ _ _) = throwError ToplevelStatementsHaveNoLanguage
infer' Nothing g1 s1@(SrcE lang path xs) = do
  let g3 = [SrcG alias lang path srcname | (srcname, alias) <- xs] ++ g1
  return ( g3 , [] , s1)

-- Signature=>
infer' (Just _) _ (Signature _ _) = throwError ToplevelStatementsHaveNoLanguage
infer' Nothing g e0@(Signature v e) = do
  g2 <- accessWith1 isAnnG append' ifNotFound g
  return (g2, [], e0)
  where

    -- find a typeset
    isAnnG :: GammaIndex -> Bool
    isAnnG (AnnG (VarE e2) _)
      | v == e2 = True
      | otherwise = False
    isAnnG _ = False

    -- update the found typeset
    append' :: GammaIndex -> Stack GammaIndex
    append' (AnnG v r2) = AnnG <$> pure v <*> appendTypeSet e r2
    append' _ = throwError $ OtherError "Bad Gamma"

    -- create a new typeset if none was found
    ifNotFound :: Gamma -> Stack Gamma
    ifNotFound g' = case (langOf . etype) e of
        lang@(Just _) -> return $ AnnG (VarE v) (TypeSet Nothing [e]) : g'
        Nothing       -> return $ AnnG (VarE v) (TypeSet (Just e) []) : g'

    -- merge the new data from a signature with any prior type data
    appendTypeSet :: EType -> TypeSet -> Stack TypeSet
    appendTypeSet e1 s =
      case ((langOf . etype) e1, s) of
      -- if e is a general type, and there is no conflicting type, then set e
        (Nothing, TypeSet Nothing rs) -> do
          mapM_ (checkRealization e1) rs
          return $ TypeSet (Just e1) rs
      -- if e is a realization, and no general type is set, just add e to the list
        (Just lang, TypeSet Nothing rs) ->
          return $ TypeSet Nothing (e1 {esource = lookupSrc (v, lang) g} : rs)
      -- if e is a realization, and a general type exists, append it and check
        (Just lang, TypeSet (Just e2) rs) -> do
          checkRealization e2 e1
          return $ TypeSet (Just e2) (e1 {esource = lookupSrc (v, lang) g} : rs)
      -- if e is general, and a general type exists, merge the general types
        (Nothing, TypeSet (Just e2) rs) -> do
          let e3 =
                EType
                  { etype = etype e2
                  , eprop = Set.union (eprop e1) (eprop e2)
                  , econs = Set.union (econs e1) (econs e2)
                  , esource = Nothing
                  }
          return $ TypeSet (Just e3) rs

-- Declaration=>
infer' (Just _) _ (Declaration _ _) = throwError ToplevelStatementsHaveNoLanguage
infer' Nothing g (Declaration v e) =
  case lookupE (VarE v) g of 
    Nothing -> declareInfer
    (Just typeset) -> declareCheck typeset
  where
    -- declare a morloc composition where no signature is provided
    declareInfer = do
      (g2, ts1, e2) <- infer Nothing (g +> MarkEG v) e
      g3 <- cut (MarkEG v) g2
      ts2 <- typesetFromList (map generalize ts1)
      let g4 = g3 +> AnnG (VarE v) ts2
      return (g4, [], Declaration v (generalizeE e2))
    -- declare a morloc composition based on a provided signature
    declareCheck (TypeSet (Just t) []) = do
      (g2, _, e2) <- check (g +> MarkEG v) e (etype t)
      g3 <- cut (MarkEG v) g2
      return (g3, [], Declaration v e2)
    -- The elements in a composition may have realizations, but the composition
    -- itself is purely a Morloc construct. Since the variable is assigned in a
    -- Morloc script, it could not have been imported from a particular source
    -- language. 
    declareCheck (TypeSet Nothing _) = throwError CompositionsMustBeGeneral
    declareCheck (TypeSet (Just _) (_:_)) = throwError CompositionsMustBeGeneral

--  (x:A) in g
-- ------------------------------------------- Var
--  g |- x => A -| g
infer' _ g e@(VarE v) = do
  case lookupE e g of
    (Just typeset) ->
      let ts = mapTS etype typeset
      in return (g, ts, AnnE e ts)
    Nothing -> throwError (UnboundVariable v)
  where
    mapTS :: (EType -> a) -> TypeSet -> [a]
    mapTS f (TypeSet (Just a) es) = map f (a:es)
    mapTS f (TypeSet Nothing es) = map f es

--  g1,Ea,Eb,x:Ea |- e <= Eb -| g2,x:Ea,g3
-- ----------------------------------------- -->I=>
--  g1 |- \x.e => Ea -> Eb -| g2
infer' lang g1 e0@(LamE v e2) = do
  a <- newvar lang
  b <- newvar lang
  let anng = AnnG (VarE v) (fromType lang a)
      g2 = g1 +> a +> b +> anng
  (g3, t1, e2') <- check g2 e2 b
  case lookupE (VarE v) g3 >>= toType lang of
    (Just t2) -> do
      let t3 = FunT (apply g3 t2) t1
      g4 <- cut anng g3
      return (g4, [t3], ann e2' t3)
    Nothing -> throwError $ OtherError "Bad thing #4"

{-  g |- e1 => A* -| d_1
 -  { d_i |- [d_i]A_i o e2 =>> C_i -| d_{i+1} } forall i in (1,2 ... k)
 - ----------------------------------------- -->E
 -  g |- e1 e2 =>> C -| d_k
 -}
infer' _ g1 (AppE e1 e2) = do
  -- Anonymous lambda functions are currently not supported. So e1 currently will
  -- be a VarE, an AppE, or an AnnE annotating a VarE or AppE. Anonymous lambdas
  -- would roughly correspond to DeclareInfer statements while adding annotated
  -- lambdas would correspond to DeclareAnnot.

  -- @as1@ will include one entry consisting of the general type `(Nothing,t)`
  -- and one or more realizatoins `(Just lang, t)`
  (d1, as1, e1') <- infer Nothing g1 e1

  -- Map derive over every type observed for e1, the functional element. The
  -- result is a list of the types and expressions derived from e2
  (g2, fs, es2') <- foldM deriveF (d1, [], []) as1

  let cs1 = [c | FunT _ c <- fs]

  e2' <- collate es2' 

  -- * e1' - e1 with type annotations
  -- * e2' - e2 with type annotations (after being applied to e2)
  (as2, ek') <- applyConcrete e1' e2' fs

  return (g2, as2, ek')
  where
    -- pair input and output types by language and construct the function type
    applyConcrete :: Expr -> Expr -> [Type] ->  Stack ([Type], Expr)
    applyConcrete (AnnE e1 f) e2 fs' = do
      let (tas, tcs) = unzip [ (FunT a c, c) | (FunT a c) <- fs' ]
      return (tcs, AnnE (AppE (AnnE e1 tas) e2) tcs)
    applyConcrete _ _ _ = throwError $ OtherError "bad concrete"

    deriveF ::
         (Gamma, [Type], [Expr])
      -> Type
      -> Stack (Gamma, [Type], [Expr])
    deriveF (g', ts, es) t' = do
      (g'', t'', e'') <- derive g' e2 t'
      return (g'', t'':ts, e'':es)

--  g1 |- A
--  g1 |- e <= A -| g2
-- ----------------------------------------- Anno
--  g1 |- (e:A) => A -| g2
infer' _ g e1@(AnnE e@(VarE _) annot@[t]) = do
  -- Non-top-level annotations will always consist of a single general type.
  --
  -- This is a bit questionable. If a single variable is annotated, e.g.
  -- `x::Int`, and is not declared, this would normally raise an
  -- UnboundVariable error. However, it is convenient for testing purposes, and
  -- also for Morloc where functions are imported as black boxes from other
  -- languages, to be able to simply declare a type as an axiom. Perhaps I
  -- should add dedicated syntax for axiomatic type declarations?
  if langOf t == Nothing 
    then
      case lookupE e g of
        (Just _) -> checkup g e t
        Nothing -> return (g, annot, e1)
    else
      throwError IllegalConcreteAnnotation
infer' _ g (AnnE e [t]) =
  if langOf t == Nothing
    then checkup g e t
    else throwError IllegalConcreteAnnotation
infer' _ g (AnnE _ _) = throwError IllegalConcreteAnnotation

-- List=>
infer' Nothing g e1@(ListE []) = do
  t <- newvar Nothing
  let t' = ArrT (TV Nothing "List") [t]
  return (g +> t, [t'], ann e1 t')
infer' lang@(Just _) g1 e@(ListE []) = do
  (ExistT v []) <- newvar lang
  -- generate an existential variable for the unknown container parameter
  p <- newvar lang
  -- define the container type
  let t = ExistT v [p]
      -- store the container and paramter types as an existentials, since
      -- neither is yet unknown
      g2 = g1 +> t +> p
  return (g2, [t], ann e t)
infer' lang@(Just _) g1 (ListE (x:xs)) = do
  -- infer the type of the first element
  (g2, (p:_), _) <- infer lang g1 x
  -- check that every following element is a subtype of this element
  -- FIXME: this is wrong ... I should instead infer the types of all and
  -- report the most general type (or something like that). I need a
  -- `mostPolymorphic` function that given a pair of types will fail if neither
  -- is the subtype of the other or otherwise return the more polymorphic one.
  -- Then I could infer every type in the list and then foldM them with
  -- `mostPolymorphic`.
  (g3, _, es) <- chainCheck (zip (repeat p) xs) g2
  -- generate an existential variable for the unknown container parameter
  (ExistT v []) <- newvar lang
  let p' = apply g3 p
      t = ExistT v [p']
      g4 = g3 +> t
  return (g4, [t], ann (ListE es) t)
-- TODO: fix this - this should do something more reasonable
infer' Nothing g1 e1@(ListE (x:xs)) = do
  (g2, (p:_), _) <- infer Nothing g1 x
  (g3, _, es) <- chainCheck (zip (repeat p) xs) g2
  let t'' = ArrT (TV Nothing "List") [apply g3 p]
  return (g3, [t''], ann (ListE es) t'')

-- Tuple=>
infer' _ _ (TupleE []) = throwError EmptyTuple
infer' _ _ (TupleE [_]) = throwError TupleSingleton
infer' lang@(Just _) g1 e@(TupleE xs) = do
  (ExistT v []) <- newvar lang
  vs <- replicateM (length xs) (newvar lang)
  let t = ArrT v vs
      g2 = g1 +> ExistG v vs ++> vs
  (g3, ts, es) <- chainCheck (zip vs xs) g2
  let t2 = apply g3 t
      e2 = apply g3 (TupleE es)
  return (g3, [t2], ann e2 t2) 
infer' Nothing g1 (TupleE xs) = do
  (g2, ts, es) <- chainInfer g1 xs
  let v = TV Nothing . MT.pack $ "Tuple" ++ (show (length xs))
      t = ArrT v ts
      e = TupleE es
  return (g2, [t], ann e t)

-- ----------------------------------------- Record=>
infer' lang@(Just _) g e@(RecE _) = do
  v <- newvar lang
  return (g +> v, [v], ann e v) 
infer' _ _ (RecE []) = throwError EmptyRecord
infer' Nothing g1 e@(RecE rs) = do
  (g2, ts, _) <- chainInfer g1 (map snd rs)
  let t = RecT (zip [TV Nothing x | (EV x, _) <- rs] ts)
  return (g2, [t], ann e t)


chainCheck :: [(Type, Expr)] -> Gamma -> Stack (Gamma, [Type], [Expr])
chainCheck xs g = do
  (g, ts, es) <- foldM f (g, [], []) xs
  return (g, reverse ts, reverse es)
  where
    f :: (Gamma, [Type], [Expr]) -> (Type, Expr) -> Stack (Gamma, [Type], [Expr])
    f (g', ts, es) (t', e') = do 
      (g'', t'', e'') <- check g' e' t'
      return (g'', t'':ts, e'':es)


-- | Pattern matches against each type
check ::
     Gamma
  -> Expr -- ^ An expression which should be of the type given
  -> Type -- ^ The expected type of the expression
  -> Stack ( Gamma
           , Type -- The inferred type of the expression
           , Expr -- The annotated expression
           )
check g e t = do
  enter $  "check" <+> parens (prettyExpr e) <> "  " <> prettyGreenType t
  seeGamma g
  (g', t', e') <- check' g e t
  leave $ "check |-" <+> prettyType t'
  return (g', t', e')

--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
check' g1 (LamE v e1) t1@(FunT a b)
  -- define x:A
 = do
  let anng = AnnG (VarE v) (fromType (langOf t1) a)
  -- check that e has the expected output type
  (g2, t2, e2) <- check (g1 +> anng) e1 b
  -- ignore the trailing context and (x:A), since it is out of scope
  g3 <- cut anng g2
  let t3 = FunT a t2
  return (g3, t3, ann (LamE v e2) t3)
--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
check' g1 e1 t2@(Forall x a) = do
  (g2, _, e2) <- check (g1 +> VarG x) e1 a
  g3 <- cut (VarG x) g2
  let t3 = apply g3 t2
  return (g3, t3, ann e2 t3)
--  g1 |- e => A -| g2
--  g2 |- [g2]A <: [g2]B -| g3
-- ----------------------------------------- Sub
--  g1 |- e <= B -| g3
check' g1 e1 b = do
  (g2, ts, e2) <- infer (langOf b) g1 e1
  g3 <- foldM (\g t -> subtype (apply g t) (apply g b) g) g2 ts
  return (g3, apply g3 b, anns (apply g3 e2) (map (apply g3) ts))


derive ::
     Gamma
  -> Expr -- the expression that is passed to the function
  -> Type -- the function type
  -> Stack ( Gamma
           , Type -- output function type
           , Expr -- @e@, with type annotation
            )
derive g e f = do
  enter $ "derive" <+> prettyExpr e <> "  " <> prettyGreenType f
  seeGamma g
  (g', t', e') <- derive' g e f
  leave $ "derive |-" <+> prettyType t'
  return (g', t', e')


--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
derive' g e (FunT a b) = do
  (g', a', e') <- check g e a
  let b' = apply g' b
  return (g', FunT a' b', apply g' e')
--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
derive' g e (Forall x s) = derive (g +> ExistG x []) e (substitute x s)
--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
derive' g e t@(ExistT v@(TV lang _) []) =
  case access1 v g of
  -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      ea1 <- newvar lang
      ea2 <- newvar lang
      let t' = FunT ea1 ea2
          g2 = rs ++ [SolvedG v t', index ea1, index ea2] ++ ls
      (g3, a', e2) <- check g2 e ea1
      let f' = FunT a' (apply g3 ea2)
      return (g3, f', e2)
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupT v g of
      (Just (FunT t1 t2)) -> do
        (g2, _, e2) <- check g e t1
        return (g2, FunT t1 t2, e2)
      _ -> throwError . OtherError $ "Expected a function"
derive' _ _ _ = throwError NonFunctionDerive
