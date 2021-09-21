{-|
Module      : Morloc.Frontend.Internal
Description : Utilities for type checking
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Internal
  ( generalize
  ) where

import Control.Monad.Except (throwError)
import Morloc.Frontend.Namespace
import qualified Control.Monad.State as CMS
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.PartialOrder as P


-- | Deal with existentials.
-- This function is used to resolve remaining existentials when no further
-- inferences about their type can be made. If the existentials have a default
-- type, then that type can be used to replace the existential. Otherwise, the
-- existential can be cast as generic (ForallU).
generalize :: TypeU -> TypeU
generalize = (\t -> generalize' (existentialMap t) t) . setDefaults where
  generalize' :: [(TVar, Name)] -> TypeU -> TypeU
  generalize' [] t = t
  generalize' ((e, r):xs) t = generalize' xs (generalizeOne e r t)

  setDefaults :: TypeU -> TypeU
  setDefaults (ExistU v ps []) = ExistU v (map setDefaults ps) []
  setDefaults (ExistU _ _ (d:_)) = setDefaults d
  setDefaults t@(VarU _) = t
  setDefaults (ForallU v t) = ForallU v (setDefaults t)
  setDefaults (FunU ts t) = FunU (map setDefaults ts) (setDefaults t)
  setDefaults (AppU v ts) = AppU v (map setDefaults ts)
  setDefaults (NamU o n ps rs) = NamU o n ps [(k, setDefaults t) | (k, t) <- rs]


  variables = [1 ..] >>= flip replicateM ['a' .. 'z']

  existentialMap t =
    zip (Set.toList (findExistentials t)) (map (Name . MT.pack) variables)

  findExistentials :: TypeU -> Set.Set TVar
  findExistentials (VarU _) = Set.empty
  findExistentials (ExistU v ts ds) =
    Set.unions
      $ [Set.singleton v]
      ++ map findExistentials ts
      ++ map findExistentials ds
  findExistentials (ForallU v t) = Set.delete v (findExistentials t)
  findExistentials (FunU ts t) = Set.unions (findExistentials t : map findExistentials ts)
  findExistentials (AppU v ts) = Set.unions (map findExistentials ts)
  findExistentials (NamU _ _ _ rs) = Set.unions (map (findExistentials . snd) rs)

  generalizeOne :: TVar -> Name -> TypeU -> TypeU
  generalizeOne v0@(TV lang0 _) r0 t0 = ForallU (TV lang0 (unName r0)) (f v0 t0)
    where
     -- the type term that is being substitute in
      replacementTerm = TV lang0 (unName r0)

      f :: TVar -> TypeU -> TypeU
      f v t1@(ExistU v' [] _)
        | v == v' = VarU replacementTerm -- substitute
        | otherwise = t1
      f v (ExistU v' ts _)
        | v == v' = AppU replacementTerm (map (f v) ts) -- substitute
        | otherwise = AppU v (map (f v) ts)
      f v t1@(ForallU x t2)
        | v /= x = ForallU x (f v t2)
        | otherwise = t1
      f v (FunU ts t) = FunU (map (f v) ts) (f v t)
      f v (AppU v' ts) = AppU v' (map (f v) ts)
      f v (NamU o n ps rs) = NamU o n ps [(k, f v t) | (k, t) <- rs]


-- class HasManyLanguages a where
--   langsOf :: Gamma -> a -> [Maybe Lang]
--
-- instance HasManyLanguages TypeSet where
--   langsOf = undefined
--   -- langsOf _ (TypeSet Nothing es) = map langOf es
--   -- langsOf _ (TypeSet (Just e) es) = langOf e : map langOf es
--
-- instance HasManyLanguages Expr where
--   langsOf = undefined
--   -- langsOf g0 e0 = unique $ Nothing : langsOf' g0 e0 where
--   --   langsOf' _ (SrcE srcs) = map (Just . srcLang) srcs
--   --   langsOf' _ (SigE _ t) = [langOf t]
--   --   langsOf' g (AssE _ e) = langsOf' g e
--   --   langsOf' _ UniE = []
--   --   langsOf' g (VarE v) = case lookupE v g of
--   --     (Just (_, ts)) -> langsOf g ts
--   --     Nothing -> []
--   --   langsOf' g (AccE e _) = langsOf' g e
--   --   langsOf' g (ListE es) = concat . map (langsOf' g) $ es
--   --   langsOf' g (TupleE es) = concat . map (langsOf' g) $ es
--   --   langsOf' g (LamE _ e) = langsOf' g e
--   --   langsOf' g (AppE e1 e2) = langsOf' g e1 ++ langsOf' g e2
--   --   langsOf' _ (AnnE _ ts) = map langOf ts
--   --   langsOf' _ (NumE _) = []
--   --   langsOf' _ (LogE _) = []
--   --   langsOf' _ (StrE _) = []
--   --   langsOf' g (NamE entries) = concat . map (langsOf' g . snd) $ entries
--
-- class Renameable a where
--   rename :: a -> Stack a
--   unrename :: a -> a
--
-- instance Renameable Expr where
--   rename = undefined
--   unrename = undefined
--   -- rename = mapU' rename
--   -- unrename = mapU unrename
--
-- instance Renameable TypeU where
--   rename = undefined
--   unrename = undefined
--   -- rename t@(VarU _) = return t
--   -- rename (ExistU v ts ds) = ExistU <$> pure v <*> (mapM rename ts) <*> (mapM rename ds)
--   -- rename (ForallU v t) = do
--   --   v' <- rename v
--   --   t' <- rename (P.substitute v (VarU v') t)
--   --   return $ ForallU v' t'
--   -- rename (FunU t1 t2) = FunU <$> rename t1 <*> rename t2
--   -- rename (ArrU v ts) = ArrU <$> pure v <*> mapM rename ts
--   -- rename (NamU r v ts rs) =
--   --   NamU r <$> pure v <*> mapM rename ts <*> mapM (\(x, t) -> (,) <$> pure x <*> rename t) rs
--   --
--   -- unrename (VarU v) = VarU (unrename v)
--   -- unrename (ExistU v ts ds) = ExistU v (map unrename ts) (map unrename ds)
--   -- unrename (ForallU v t) = ForallU (unrename v) (unrename t)
--   -- unrename (FunU t1 t2) = FunU (unrename t1) (unrename t2)
--   -- unrename (ArrU v ts) = ArrU v (map unrename ts)
--   -- unrename (NamU r v ts rs) = NamU r v (map unrename ts) [(x, unrename t) | (x, t) <- rs]
--
-- instance Renameable TVar where
--   rename = undefined
--   unrename = undefined
--   -- unrename (TV l t) = TV l . head $ MT.splitOn "." t
--   -- rename = newqul
--
--
-- class Applicable a where
--   apply :: Gamma -> a -> a
--
-- -- | Apply a context to a type (See Dunfield Figure 8).
-- instance Applicable TypeU where
--   apply = undefined
--   -- -- [G]a = a
--   -- apply _ a@(VarU _) = a
--   -- -- [G](A->B) = ([G]A -> [G]B)
--   -- apply g (FunU a b) = FunU (apply g a) (apply g b)
--   -- -- [G]ForallU a.a = forall a. [G]a
--   -- apply g (ForallU x a) = ForallU x (apply g a)
--   -- -- [G[a=t]]a = [G[a=t]]t
--   -- apply g (ExistU v ts ds) =
--   --   case lookupU v g of
--   --     -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
--   --     (Just t') -> apply g t' -- reduce an existential; strictly smaller term
--   --     Nothing -> ExistU v (map (apply g) ts) (map (apply g) ds)
--   -- apply g (ArrU v ts) = ArrU v (map (apply g) ts)
--   -- apply g (NamU r v ts rs) = NamU r v (map (apply g) ts) (map (\(n, t) -> (n, apply g t)) rs)
--
-- instance Applicable Expr where
--   apply = undefined
--   -- apply g e = mapU (apply g) e
--
-- instance Applicable EType where
--   apply = undefined
--   -- apply g e = e { etype = apply g (etype e) }
--
--
-- class Typed a where
--   toType :: Maybe Lang -> a -> Maybe TypeU
--   fromType :: Maybe Lang -> TypeU -> a
--
-- instance Typed EType where
--   toType = undefined
--   -- toType lang e
--   --   | (langOf . etype) e == lang = Just (etype e)
--   --   | otherwise = Nothing
--
--   fromType = undefined
--   -- fromType _ t =
--   --   EType
--   --     { etype = t
--   --     , eprop = Set.empty
--   --     , econs = Set.empty
--   --     }
--
-- toEType :: TypeU -> EType
-- toEType = undefined
-- -- toEType t = EType
-- --   { etype = t
-- --   , eprop = Set.empty
-- --   , econs = Set.empty
-- --   }
--
-- instance Typed TypeSet where
--   toType = undefined
--   -- toType Nothing (TypeSet e _) = e >>= toType Nothing
--   -- toType lang (TypeSet _ ts) = case filter (\e -> (langOf . etype) e == lang) ts of
--   --   [ ] -> Nothing
--   --   [e] -> Just (etype e)
--   --   _ -> error "a typeset can contain only one instance of each language"
--
--   fromType = undefined
--   -- fromType Nothing t = TypeSet (Just (fromType Nothing t)) []
--   -- fromType lang t = TypeSet Nothing [fromType lang t]
--
-- serialConstraint :: TypeU -> TypeU -> Stack ()
-- serialConstraint = undefined
-- -- serialConstraint t1 t2 = do
-- --   s <- CMS.get
-- --   CMS.put (s {stateSer = (t1, t2):stateSer s})
--
-- incDepth :: Stack Int
-- incDepth = undefined
-- -- incDepth = do
-- --   s <- CMS.get
-- --   let depth = stateDepth s + 1
-- --   CMS.put (s {stateDepth = depth})
-- --   return depth
--
-- decDepth :: Stack Int
-- decDepth = undefined
-- -- decDepth = do
-- --   s <- CMS.get
-- --   let depth = stateDepth s - 1
-- --   CMS.put (s {stateDepth = depth})
-- --   return depth
--
-- getDepth :: Stack Int
-- getDepth = undefined
-- -- getDepth = CMS.gets stateDepth
--
-- mapU :: (TypeU -> TypeU) -> Expr -> Expr
-- mapU = undefined
-- -- mapU f (LamE v e) = LamE v (mapU f e)
-- -- mapU f (ListE es) = ListE (map (mapU f) es)
-- -- mapU f (TupleE es) = TupleE (map (mapU f) es)
-- -- mapU f (NamE rs) = NamE (zip (map fst rs) (map (mapU f . snd) rs))
-- -- mapU f (AppE e1 e2) = AppE (mapU f e1) (mapU f e2)
-- -- mapU f (AnnE e ts) = AnnE (mapU f e) (map f ts)
-- -- mapU f (AssE v e) = AssE v (mapU f e)
-- -- mapU f (SigE v e) = SigE v $ e {etype = f (etype e)}
-- -- mapU _ e = e
--
-- mapU' :: Monad m => (TypeU -> m TypeU) -> Expr -> m Expr
-- mapU' = undefined
-- -- mapU' f (LamE v e) = LamE <$> pure v <*> mapU' f e
-- -- mapU' f (ListE es) = ListE <$> mapM (mapU' f) es
-- -- mapU' f (NamE rs) = do
-- --   es' <- mapM (mapU' f . snd) rs
-- --   return $ NamE (zip (map fst rs) es')
-- -- mapU' f (TupleE es) = TupleE <$> mapM (mapU' f) es
-- -- mapU' f (AppE e1 e2) = AppE <$> mapU' f e1 <*> mapU' f e2
-- -- mapU' f (AnnE e ts) = AnnE <$> mapU' f e <*> mapM f ts
-- -- mapU' f (AssE v e) = AssE <$> pure v <*> mapU' f e
-- -- mapU' f (SigE v e) = do
-- --   t' <- f (etype e)
-- --   return $ SigE v (e {etype = t'})
-- -- mapU' _ e = return e
--
-- (+>) :: Indexable a => Gamma -> a -> Gamma
-- (+>) = undefined
-- -- (+>) xs x = (index x) : xs
--
-- (++>) :: Indexable a => Gamma -> [a] -> Gamma
-- (++>) = undefined
-- -- (++>) g xs = map index (reverse xs) ++ g
--
-- -- | remove context up to a marker
-- cut :: GammaIndex -> Gamma -> Stack Gamma
-- cut = undefined
-- -- cut _ [] = throwError EmptyCut
-- -- cut i (x:xs)
-- --   | i == x = return xs
-- --   | otherwise = cut i xs
--
-- -- | Look up a type annotated expression
-- lookupE :: EVar -> Gamma -> Maybe (Expr, TypeSet)
-- lookupE = undefined
-- -- lookupE _ [] = Nothing
-- -- lookupE v ((AnnG (AssE v' e) t):gs)
-- --   | v == v' = Just (e, t)
-- --   | otherwise = lookupE v gs
-- -- lookupE v ((AnnG e@(VarE v') t):gs)
-- --   | v == v' = Just (e, t)
-- --   | otherwise = lookupE v gs
-- -- lookupE v (_:gs) = lookupE v gs
--
-- -- | Look up a solved existential type variable
-- lookupU :: TVar -> Gamma -> Maybe TypeU
-- lookupU = undefined
-- -- lookupU _ [] = Nothing
-- -- lookupU v ((SolvedG v' t):gs)
-- --   | v == v' = Just t
-- --   | otherwise = lookupU v gs
-- -- lookupU v (_:gs) = lookupU v gs
--
-- access1 :: TVar -> Gamma -> Maybe (Gamma, GammaIndex, Gamma)
-- access1 = undefined
-- -- access1 v gs =
-- --   case findIndex (exists v) gs of
-- --     (Just 0) -> Just ([], head gs, tail gs)
-- --     (Just i) -> Just (take i gs, gs !! i, drop (i + 1) gs)
-- --     _ -> Nothing
-- --   where
-- --     exists :: TVar -> GammaIndex -> Bool
-- --     exists v1 (ExistG v2 _ _) = v1 == v2
-- --     exists _ _ = False
--
-- accessWith1 :: Monad m =>
--      (GammaIndex -> Bool) -- ^ method for finding the index
--   -> (GammaIndex -> m GammaIndex) -- ^ alter GammaIndex
--   -> (Gamma -> m Gamma) -- ^ default action if the index is not found
--   -> Gamma -- ^ context that is searched
--   -> m Gamma
-- accessWith1 = undefined
-- -- accessWith1 select make def g =
-- --   case findIndex select g of
-- --     (Just i) ->
-- --       case (i, g !! i) of
-- --         (0, x) -> make x >>= (\y -> return ([] <> (y : tail g)))
-- --         (_, x) -> make x >>= (\y -> return (take i g <> (y : drop (i + 1) g)))
-- --     Nothing -> def g
--
-- access2 ::
--      TVar -> TVar
--   -> Gamma -> Maybe (Gamma, GammaIndex, Gamma, GammaIndex, Gamma)
-- access2 = undefined
-- -- access2 lv rv gs =
-- --   case access1 lv gs of
-- --     Just (ls, x, rs) ->
-- --       case access1 rv rs of
-- --         Just (ls', y, rs') -> Just (ls, x, ls', y, rs')
-- --         _ -> Nothing
-- --     _ -> Nothing
--
-- ann :: Expr -> TypeU -> Expr
-- ann = undefined
-- -- ann (AnnE e _) t = AnnE e [t]
-- -- ann e@(AssE _ _) _ = e
-- -- ann e@(SigE _ _) _ = e
-- -- ann e t = AnnE e [t]
--
-- anns :: Expr -> [TypeU] -> Expr
-- anns = undefined
-- -- anns (AnnE e _) ts = AnnE e ts
-- -- anns e@(AssE _ _) _ = e
-- -- anns e@(SigE _ _) _ = e
-- -- anns e ts = AnnE e ts
--
-- generalizeE :: Expr -> Expr
-- generalizeE = undefined
-- -- generalizeE = mapU generalize
--
-- generalizeEType :: EType -> EType
-- generalizeEType = undefined
-- -- generalizeEType e = e {etype = generalize (etype e)}
--
-- generalizeTypeSet :: TypeSet -> TypeSet
-- generalizeTypeSet = undefined
-- -- generalizeTypeSet (TypeSet t ts) =
-- --   TypeSet (fmap generalizeEType t) (map generalizeEType ts)
--
-- newvar :: Maybe Lang -> Stack TypeU
-- newvar = undefined
-- -- newvar = newvarRich [] []
--
-- newvarRich
--   :: [TypeU]
--   -> [TypeU] -- ^ default types
--   -> Maybe Lang
--   -> Stack TypeU
-- newvarRich = undefined
-- -- newvarRich ps ds lang = do
-- --   s <- CMS.get
-- --   let v = newvars !! stateVar s
-- --   CMS.put $ s {stateVar = stateVar s + 1}
-- --   return (ExistU (TV lang v) ps ds)
-- --   where
-- --     newvars =
-- --       zipWith (\x y -> MT.pack (x ++ show y)) (repeat "t") ([0 ..] :: [Integer])
--
-- newqul :: TVar -> Stack TVar
-- newqul = undefined
-- -- newqul (TV l v) = do
-- --   s <- CMS.get
-- --   let v' = TV l (v <> "." <> (MT.pack . show $ stateQul s)) -- create a new variable such as "a.0"
-- --   CMS.put $ s {stateQul = stateQul s + 1}
-- --   return v'
