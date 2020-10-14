{-|
Module      : Morloc.Frontend.Internal
Description : Utilities for type checking
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Internal
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
  , lookupU
  , mapU
  , mapU'
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
import Morloc.Frontend.Namespace
import qualified Control.Monad.State as CMS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Morloc.Frontend.PartialOrder as P

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
  rename = mapU' rename
  unrename = mapU unrename

instance Renameable UnresolvedType where
  rename t@(VarU _) = return t
  rename (ExistU v ts ds) = ExistU <$> pure v <*> (mapM rename ts) <*> (mapM rename ds)
  rename (ForallU v t) = do
    v' <- rename v
    t' <- rename (P.substitute v (VarU v') t)
    return $ ForallU v' t'
  rename (FunU t1 t2) = FunU <$> rename t1 <*> rename t2
  rename (ArrU v ts) = ArrU <$> pure v <*> mapM rename ts
  rename (NamU v rs) =
    NamU <$> pure v <*> mapM (\(x, t) -> (,) <$> pure x <*> rename t) rs

  unrename (VarU v) = VarU (unrename v)
  unrename (ExistU v ts ds) = ExistU v (map unrename ts) (map unrename ds)
  unrename (ForallU v t) = ForallU (unrename v) (unrename t)
  unrename (FunU t1 t2) = FunU (unrename t1) (unrename t2)
  unrename (ArrU v ts) = ArrU v (map unrename ts)
  unrename (NamU v rs) = NamU v [(x, unrename t) | (x, t) <- rs]

instance Renameable TVar where
  unrename (TV l t) = TV l . head $ MT.splitOn "." t
  rename = newqul


class Applicable a where
  apply :: Gamma -> a -> a

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable UnresolvedType where
  -- [G]a = a
  apply _ a@(VarU _) = a
  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunU a b) = FunU (apply g a) (apply g b)
  -- [G]ForallU a.a = forall a. [G]a
  apply g (ForallU x a) = ForallU x (apply g a)
  -- [G[a=t]]a = [G[a=t]]t
  apply g (ExistU v ts ds) =
    case lookupU v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistU v (map (apply g) ts) (map (apply g) ds)
  apply g (ArrU v ts) = ArrU v (map (apply g) ts)
  apply g (NamU v rs) = NamU v (map (\(n, t) -> (n, apply g t)) rs)

instance Applicable Expr where
  apply g e = mapU (apply g) e

instance Applicable EType where
  apply g e = e { etype = apply g (etype e) }


class Typed a where
  toType :: Maybe Lang -> a -> Maybe UnresolvedType
  fromType :: Maybe Lang -> UnresolvedType -> a

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



serialConstraint :: UnresolvedType -> UnresolvedType -> Stack ()
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

mapU :: (UnresolvedType -> UnresolvedType) -> Expr -> Expr
mapU f (LamE v e) = LamE v (mapU f e)
mapU f (ListE es) = ListE (map (mapU f) es)
mapU f (TupleE es) = TupleE (map (mapU f) es)
mapU f (RecE rs) = RecE (zip (map fst rs) (map (mapU f . snd) rs))
mapU f (AppE e1 e2) = AppE (mapU f e1) (mapU f e2)
mapU f (AnnE e ts) = AnnE (mapU f e) (map f ts)
mapU f (Declaration v e) = Declaration v (mapU f e)
mapU f (Signature v e) = Signature v $ e {etype = f (etype e)}
mapU _ e = e

mapU' :: Monad m => (UnresolvedType -> m UnresolvedType) -> Expr -> m Expr
mapU' f (LamE v e) = LamE <$> pure v <*> mapU' f e
mapU' f (ListE es) = ListE <$> mapM (mapU' f) es
mapU' f (RecE rs) = do
  es' <- mapM (mapU' f . snd) rs
  return $ RecE (zip (map fst rs) es')
mapU' f (TupleE es) = TupleE <$> mapM (mapU' f) es
mapU' f (AppE e1 e2) = AppE <$> mapU' f e1 <*> mapU' f e2
mapU' f (AnnE e ts) = AnnE <$> mapU' f e <*> mapM f ts
mapU' f (Declaration v e) = Declaration <$> pure v <*> mapU' f e
mapU' f (Signature v e) = do
  t' <- f (etype e)
  return $ Signature v (e {etype = t'})
mapU' _ e = return e

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
lookupU :: TVar -> Gamma -> Maybe UnresolvedType
lookupU _ [] = Nothing
lookupU v ((SolvedG v' t):gs)
  | v == v' = Just t
  | otherwise = lookupU v gs
lookupU v (_:gs) = lookupU v gs

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

ann :: Expr -> UnresolvedType -> Expr
ann (AnnE e _) t = AnnE e [t] 
ann e@(Declaration _ _) _ = e
ann e@(Signature _ _) _ = e
ann e t = AnnE e [t]

anns :: Expr -> [UnresolvedType] -> Expr
anns (AnnE e _) ts = AnnE e ts 
anns e@(Declaration _ _) _ = e
anns e@(Signature _ _) _ = e
anns e ts = AnnE e ts

-- | Deal with existentials.
-- This function is used to resolve remaining existentials when no further
-- inferences about their type can be made. If the existentials have a default
-- type, then that type can be used to replace the existential. Otherwise, the
-- existential can be cast as generic (ForallU).
generalize :: UnresolvedType -> UnresolvedType
generalize t = generalize' existentialMap t'
  where

    t' = setDefaults t

    generalize' :: [(TVar, Name)] -> UnresolvedType -> UnresolvedType
    generalize' [] t' = t'
    generalize' ((e, r):xs) t' = generalize' xs (generalizeOne e r t')

    setDefaults :: UnresolvedType -> UnresolvedType
    setDefaults (ExistU v ps []) = ExistU v (map setDefaults ps) []
    setDefaults (ExistU _ _ (d:_)) = setDefaults d
    setDefaults t@(VarU _) = t
    setDefaults (ForallU v t) = ForallU v (setDefaults t)
    setDefaults (FunU t1 t2) = FunU (setDefaults t1) (setDefaults t2)
    setDefaults (ArrU v ts) = ArrU v (map setDefaults ts)
    setDefaults (NamU v es) = NamU v (zip (map fst es) (map (setDefaults . snd) es))

    existentialMap =
      zip (Set.toList (findExistentials t')) (map (Name . MT.pack) variables)

    variables = [1 ..] >>= flip replicateM ['a' .. 'z']

    findExistentials :: UnresolvedType -> Set.Set TVar
    findExistentials (VarU _) = Set.empty
    findExistentials (ExistU v ts ds) =
      Set.unions
        $ [Set.singleton v]
        ++ map findExistentials ts
        ++ map findExistentials ds
    findExistentials (ForallU v t') = Set.delete v (findExistentials t')
    findExistentials (FunU t1 t2) =
      Set.union (findExistentials t1) (findExistentials t2)
    findExistentials (ArrU _ ts) = Set.unions (map findExistentials ts)
    findExistentials (NamU _ rs) = Set.unions (map (findExistentials . snd) rs)

    generalizeOne :: TVar -> Name -> UnresolvedType -> UnresolvedType
    generalizeOne v0@(TV lang _) r t0 = ForallU (TV lang (unName r)) (f v0 t0)
      where
        f :: TVar -> UnresolvedType -> UnresolvedType
        f v t1@(ExistU v' [] _)
          | v == v' = VarU (TV lang (unName r))
          | otherwise = t1
        f v t1@(ExistU v' ts _)
          | v == v' = ArrU (TV lang (unName r)) (map (f v) ts)
          | otherwise = ArrU v (map (f v) ts)
        f v (FunU t1 t2) = FunU (f v t1) (f v t2)
        f v t1@(ForallU x t2)
          | v /= x = ForallU x (f v t2)
          | otherwise = t1
        f v (ArrU v' xs) = ArrU v' (map (f v) xs)
        f v (NamU v' xs) = NamU v' (map (\(v', t) -> (v', f v t)) xs)
        f _ t1 = t1

generalizeE :: Expr -> Expr
generalizeE = mapU generalize

generalizeEType :: EType -> EType
generalizeEType e = e {etype = generalize (etype e)}

generalizeTypeSet :: TypeSet -> TypeSet
generalizeTypeSet (TypeSet t ts) =
  TypeSet (fmap generalizeEType t) (map generalizeEType ts)

newvar :: Maybe Lang -> Stack UnresolvedType
newvar = newvarRich [] []

newvarRich
  :: [UnresolvedType]
  -> [UnresolvedType] -- ^ default types
  -> Maybe Lang
  -> Stack UnresolvedType
newvarRich ps ds lang = do
  s <- CMS.get
  let v = newvars !! stateVar s
  CMS.put $ s {stateVar = stateVar s + 1}
  return (ExistU (TV lang v) ps ds)
  where
    newvars =
      zipWith (\x y -> MT.pack (x ++ show y)) (repeat "t") ([0 ..] :: [Integer])

newqul :: TVar -> Stack TVar
newqul (TV l v) = do
  s <- CMS.get
  let v' = TV l (v <> "." <> (MT.pack . show $ stateQul s)) -- create a new variable such as "a.0"
  CMS.put $ s {stateQul = stateQul s + 1}
  return v'
