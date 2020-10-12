{-|
Module      : Morloc.TypeChecker.Internal
Description : Utilities for type checking
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Internal
  ( (+>)
  , (++>)
  , Renameable(..)
  , Applicable(..)
  , Typed(..) 
  , access1
  , access2
  , accessWith1
  , ann
  , anns
  , cut
  , generalize
  , generalizeE
  , generalizeTypeSet
  , index
  , lookupE
  , lookupT
  , mapT
  , mapT'
  , newqul
  , newvar
  , newvarRich
  , throwError
  , serialConstraint
  , incDepth
  , decDepth
  , getDepth
  , langsOf
  ) where

import Control.Monad.Except (throwError)
import Morloc.Namespace
import qualified Control.Monad.State as CMS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.TypeChecker.PartialOrder as P

class HasManyLanguages a where
  langsOf :: Gamma -> a -> [Maybe Lang]

instance HasManyLanguages TypeSet where
  langsOf _ (TypeSet Nothing es) = map langOf es
  langsOf _ (TypeSet (Just e) es) = langOf e : map langOf es

instance HasManyLanguages Expr where
  langsOf g e = nub $ Nothing : langsOf' g e where
    langsOf' _ (SrcE srcs) = map (Just . srcLang) srcs
    langsOf' _ (Signature _ t) = [langOf t] 
    langsOf' g (Declaration _ e) = langsOf' g e
    langsOf' g UniE = [] 
    langsOf' g (VarE v) = case lookupE v g of  
      (Just (_, ts)) -> langsOf g ts
      Nothing -> []
    langsOf' g (ListE es) = concat . map (langsOf' g) $ es
    langsOf' g (TupleE es) = concat . map (langsOf' g) $ es
    langsOf' g (LamE _ e) = langsOf' g e 
    langsOf' g (AppE e1 e2) = langsOf' g e1 ++ langsOf' g e2 
    langsOf' g (AnnE e ts) = map langOf ts
    langsOf' g (NumE _) = []
    langsOf' g (LogE _) = [] 
    langsOf' g (StrE _) = []
    langsOf' g (RecE entries) = concat . map (langsOf' g . snd) $ entries

class Renameable a where
  rename :: a -> Stack a
  unrename :: a -> a

instance Renameable Expr where
  rename = mapT' rename
  unrename = mapT unrename

instance Renameable Type where
  rename t@(VarT _) = return t
  rename (ExistT v ts ds) = ExistT <$> pure v <*> (mapM rename ts) <*> (mapM rename ds)
  rename (Forall v t) = do
    v' <- rename v
    t' <- rename (P.substitute v (VarT v') t)
    return $ Forall v' t'
  rename (FunT t1 t2) = FunT <$> rename t1 <*> rename t2
  rename (ArrT v ts) = ArrT <$> pure v <*> mapM rename ts
  rename (NamT v rs) =
    NamT <$> pure v <*> mapM (\(x, t) -> (,) <$> pure x <*> rename t) rs

  unrename (VarT v) = VarT (unrename v)
  unrename (ExistT v ts ds) = ExistT v (map unrename ts) (map unrename ds)
  unrename (Forall v t) = Forall (unrename v) (unrename t)
  unrename (FunT t1 t2) = FunT (unrename t1) (unrename t2)
  unrename (ArrT v ts) = ArrT v (map unrename ts)
  unrename (NamT v rs) = NamT v [(x, unrename t) | (x, t) <- rs]

instance Renameable DefaultType where
  rename dt = fmap DefaultType $ rename (unDefaultType dt) 
  unrename = DefaultType . unrename . unDefaultType

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
  apply g (ExistT v ts ds) =
    case lookupT v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistT v (map (apply g) ts) (map (DefaultType . apply g . unDefaultType) ds)
  apply g (ArrT v ts) = ArrT v (map (apply g) ts)
  apply g (NamT v rs) = NamT v (map (\(n, t) -> (n, apply g t)) rs)

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
      }


instance Typed TypeSet where
  toType Nothing (TypeSet e _) = e >>= toType Nothing
  toType lang (TypeSet _ ts) = case filter (\e -> (langOf . etype) e == lang) ts of 
    [ ] -> Nothing
    [e] -> Just (etype e)
    _ -> error "a typeset can contain only one instance of each language"

  fromType Nothing t = TypeSet (Just (fromType Nothing t)) []
  fromType lang t = TypeSet Nothing [fromType lang t]



serialConstraint :: Type -> Type -> Stack ()
serialConstraint t1 t2 = do
  s <- CMS.get
  CMS.put (s {stateSer = (t1, t2):stateSer s})

incDepth :: Stack Int
incDepth = do
  s <- CMS.get 
  let depth = stateDepth s + 1
  CMS.put (s {stateDepth = depth})
  return depth

decDepth :: Stack Int
decDepth = do
  s <- CMS.get 
  let depth = stateDepth s - 1
  CMS.put (s {stateDepth = depth})
  return depth

getDepth :: Stack Int
getDepth = CMS.gets stateDepth

mapT :: (Type -> Type) -> Expr -> Expr
mapT f (LamE v e) = LamE v (mapT f e)
mapT f (ListE es) = ListE (map (mapT f) es)
mapT f (TupleE es) = TupleE (map (mapT f) es)
mapT f (RecE rs) = RecE (zip (map fst rs) (map (mapT f . snd) rs))
mapT f (AppE e1 e2) = AppE (mapT f e1) (mapT f e2)
mapT f (AnnE e ts) = AnnE (mapT f e) (map f ts)
mapT f (Declaration v e) = Declaration v (mapT f e)
mapT f (Signature v e) = Signature v $ e {etype = f (etype e)}
mapT _ e = e

mapT' :: Monad m => (Type -> m Type) -> Expr -> m Expr
mapT' f (LamE v e) = LamE <$> pure v <*> mapT' f e
mapT' f (ListE es) = ListE <$> mapM (mapT' f) es
mapT' f (RecE rs) = do
  es' <- mapM (mapT' f . snd) rs
  return $ RecE (zip (map fst rs) es')
mapT' f (TupleE es) = TupleE <$> mapM (mapT' f) es
mapT' f (AppE e1 e2) = AppE <$> mapT' f e1 <*> mapT' f e2
mapT' f (AnnE e ts) = AnnE <$> mapT' f e <*> mapM f ts
mapT' f (Declaration v e) = Declaration <$> pure v <*> mapT' f e
mapT' f (Signature v e) = do
  t' <- f (etype e)
  return $ Signature v (e {etype = t'})
mapT' _ e = return e

(+>) :: Indexable a => Gamma -> a -> Gamma
(+>) xs x = (index x) : xs

(++>) :: Indexable a => Gamma -> [a] -> Gamma
(++>) g xs = map index (reverse xs) ++ g 

-- | remove context up to a marker
cut :: GammaIndex -> Gamma -> Stack Gamma
cut _ [] = throwError EmptyCut
cut i (x:xs)
  | i == x = return xs
  | otherwise = cut i xs

-- | Look up a type annotated expression
lookupE :: EVar -> Gamma -> Maybe (Expr, TypeSet)
lookupE _ [] = Nothing
lookupE v ((AnnG (Declaration v' e) t):gs)
  | v == v' = Just (e, t)
  | otherwise = lookupE v gs
lookupE v ((AnnG e@(VarE v') t):gs)
  | v == v' = Just (e, t)
  | otherwise = lookupE v gs
lookupE v (_:gs) = lookupE v gs

-- | Look up a solved existential type variable
lookupT :: TVar -> Gamma -> Maybe Type
lookupT _ [] = Nothing
lookupT v ((SolvedG v' t):gs)
  | v == v' = Just t
  | otherwise = lookupT v gs
lookupT v (_:gs) = lookupT v gs

access1 :: TVar -> Gamma -> Maybe (Gamma, GammaIndex, Gamma)
access1 v gs =
  case findIndex (exists v) gs of
    (Just 0) -> Just ([], head gs, tail gs)
    (Just i) -> Just (take i gs, gs !! i, drop (i + 1) gs)
    _ -> Nothing
  where
    exists :: TVar -> GammaIndex -> Bool
    exists v1 (ExistG v2 _ _) = v1 == v2
    exists _ _ = False

accessWith1 :: Monad m =>
     (GammaIndex -> Bool) -- ^ method for finding the index
  -> (GammaIndex -> m GammaIndex) -- ^ alter GammaIndex
  -> (Gamma -> m Gamma) -- ^ default action if the index is not found
  -> Gamma -- ^ context that is searched
  -> m Gamma
accessWith1 select make def g =
  case findIndex select g of
    (Just i) ->
      case (i, g !! i) of
        (0, x) -> make x >>= (\y -> return ([] <> (y : tail g)))
        (_, x) -> make x >>= (\y -> return (take i g <> (y : drop (i + 1) g)))
    Nothing -> def g

access2 ::
     TVar -> TVar
  -> Gamma -> Maybe (Gamma, GammaIndex, Gamma, GammaIndex, Gamma)
access2 lv rv gs =
  case access1 lv gs of
    Just (ls, x, rs) ->
      case access1 rv rs of
        Just (ls', y, rs') -> Just (ls, x, ls', y, rs')
        _ -> Nothing
    _ -> Nothing

ann :: Expr -> Type -> Expr
ann (AnnE e _) t = AnnE e [t] 
ann e@(Declaration _ _) _ = e
ann e@(Signature _ _) _ = e
ann e t = AnnE e [t]

anns :: Expr -> [Type] -> Expr
anns (AnnE e _) ts = AnnE e ts 
anns e@(Declaration _ _) _ = e
anns e@(Signature _ _) _ = e
anns e ts = AnnE e ts

-- | Deal with existentials.
-- This function is used to resolve remaining existentials when no further
-- inferences about their type can be made. If the existentials have a default
-- type, then that type can be used to replace the existential. Otherwise, the
-- existential can be cast as generic (Forall).
generalize :: Type -> Type
generalize t = generalize' existentialMap t'
  where

    t' = setDefaults t

    generalize' :: [(TVar, Name)] -> Type -> Type
    generalize' [] t' = t'
    generalize' ((e, r):xs) t' = generalize' xs (generalizeOne e r t')

    setDefaults :: Type -> Type
    setDefaults (ExistT v ps []) = ExistT v (map setDefaults ps) []
    setDefaults (ExistT _ _ (d:_)) = setDefaults (unDefaultType d)
    setDefaults t@(VarT _) = t
    setDefaults (Forall v t) = Forall v (setDefaults t)
    setDefaults (FunT t1 t2) = FunT (setDefaults t1) (setDefaults t2)
    setDefaults (ArrT v ts) = ArrT v (map setDefaults ts)
    setDefaults (NamT v es) = NamT v (zip (map fst es) (map (setDefaults . snd) es))

    existentialMap =
      zip (Set.toList (findExistentials t')) (map (Name . MT.pack) variables)

    variables = [1 ..] >>= flip replicateM ['a' .. 'z']

    findExistentials :: Type -> Set.Set TVar
    findExistentials (VarT _) = Set.empty
    findExistentials (ExistT v ts ds) =
      Set.unions
        $ [Set.singleton v]
        ++ map findExistentials ts
        ++ map (findExistentials . unDefaultType) ds
    findExistentials (Forall v t') = Set.delete v (findExistentials t')
    findExistentials (FunT t1 t2) =
      Set.union (findExistentials t1) (findExistentials t2)
    findExistentials (ArrT _ ts) = Set.unions (map findExistentials ts)
    findExistentials (NamT _ rs) = Set.unions (map (findExistentials . snd) rs)

    generalizeOne :: TVar -> Name -> Type -> Type
    generalizeOne v0@(TV lang _) r t0 = Forall (TV lang (unName r)) (f v0 t0)
      where
        f :: TVar -> Type -> Type
        f v t1@(ExistT v' [] _)
          | v == v' = VarT (TV lang (unName r))
          | otherwise = t1
        f v t1@(ExistT v' ts _)
          | v == v' = ArrT (TV lang (unName r)) (map (f v) ts)
          | otherwise = ArrT v (map (f v) ts)
        f v (FunT t1 t2) = FunT (f v t1) (f v t2)
        f v t1@(Forall x t2)
          | v /= x = Forall x (f v t2)
          | otherwise = t1
        f v (ArrT v' xs) = ArrT v' (map (f v) xs)
        f v (NamT v' xs) = NamT v' (map (\(v', t) -> (v', f v t)) xs)
        f _ t1 = t1

generalizeE :: Expr -> Expr
generalizeE = mapT generalize

generalizeEType :: EType -> EType
generalizeEType e = e {etype = generalize (etype e)}

generalizeTypeSet :: TypeSet -> TypeSet
generalizeTypeSet (TypeSet t ts) =
  TypeSet (fmap generalizeEType t) (map generalizeEType ts)

newvar :: Maybe Lang -> Stack Type
newvar = newvarRich [] []

newvarRich :: [Type] -> [DefaultType] -> Maybe Lang -> Stack Type
newvarRich ps ds lang = do
  s <- CMS.get
  let v = newvars !! stateVar s
  CMS.put $ s {stateVar = stateVar s + 1}
  return (ExistT (TV lang v) ps ds)
  where
    newvars =
      zipWith (\x y -> MT.pack (x ++ show y)) (repeat "t") ([0 ..] :: [Integer])

newqul :: TVar -> Stack TVar
newqul (TV l v) = do
  s <- CMS.get
  let v' = TV l (v <> "." <> (MT.pack . show $ stateQul s)) -- create a new variable such as "a.0"
  CMS.put $ s {stateQul = stateQul s + 1}
  return v'
