{-|
Module      : Morloc.TypeChecker.Util
Description : Utilities for type checking
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Util
  ( (+>)
  , access1
  , access2
  , ann
  , cut
  , extendModularGamma
  , generalize
  , generalizeE
  , importFromModularGamma
  , index
  , lookupE
  , lookupSrc
  , lookupT
  , mapT
  , mapT'
  , newqul
  , newvar
  , throwError
  ) where

import Control.Monad.Except (throwError)
import Morloc.Namespace
import qualified Control.Monad.State as MS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT

importFromModularGamma :: ModularGamma -> Module -> Stack Gamma
importFromModularGamma g m = fmap concat $ mapM lookupImport (moduleImports m)
  where
    lookupOneImport ::
         MVar -> Map.Map EVar TypeSet -> (EVar, EVar) -> Stack GammaIndex
    lookupOneImport v typemap (n, alias) =
      case Map.lookup n typemap of
        (Just t) -> return $ AnnG (VarE alias) t
        Nothing -> throwError $ BadImport v alias
    lookupImport :: Import -> Stack Gamma
    lookupImport imp
      | v == moduleName m = throwError $ SelfImport v
      | v == MV "Main" = throwError CannotImportMain
      | otherwise =
        case (importInclude imp, Map.lookup v g) of
        -- raise error if the imported module is not in the module map
          (_, Nothing) -> throwError $ CannotFindModule v
        -- handle imports of everything, i.e. @import Foo@
          (Nothing, Just g') ->
            return [AnnG (VarE e) t | (e, t) <- Map.toList g']
        -- handle limited imports, i.g. @import Foo ("f" as foo, bar)@
          (Just xs, Just g') -> mapM (lookupOneImport v g') xs
      where
        v = importModuleName imp

extendModularGamma ::
     Gamma -- ^ context generated from typechecking this module
  -> Module -- ^ the module that is being loaded into the modular context
  -> ModularGamma -- ^ the previous object
  -> Stack ModularGamma
extendModularGamma g m mg
  | Map.member v mg = throwError $ MultipleModuleDeclarations v
  | otherwise = return $ Map.insert v g' mg
  where
    v = moduleName m
    es = moduleExports m
    g' = Map.fromList [(e, t) | (AnnG (VarE e) t) <- g, elem e es]

mapT :: (Type -> Type) -> Expr -> Expr
mapT f (LamE v e) = LamE v (mapT f e)
mapT f (ListE es) = ListE (map (mapT f) es)
mapT f (TupleE es) = TupleE (map (mapT f) es)
mapT f (AppE e1 e2) = AppE (mapT f e1) (mapT f e2)
mapT f (AnnE e t) = AnnE (mapT f e) (f t)
mapT f (Declaration v e) = Declaration v (mapT f e)
mapT f (Signature v e) = Signature v $ e {etype = f (etype e)}
mapT _ e = e

mapT' :: Monad m => (Type -> m Type) -> Expr -> m Expr
mapT' f (LamE v e) = LamE <$> pure v <*> mapT' f e
mapT' f (ListE es) = ListE <$> mapM (mapT' f) es
mapT' f (TupleE es) = TupleE <$> mapM (mapT' f) es
mapT' f (AppE e1 e2) = AppE <$> mapT' f e1 <*> mapT' f e2
mapT' f (AnnE e t) = AnnE <$> mapT' f e <*> f t
mapT' f (Declaration v e) = Declaration <$> pure v <*> mapT' f e
mapT' f (Signature v e) = do
  t' <- f (etype e)
  return $ Signature v (e {etype = t'})
mapT' _ e = return e

(+>) :: Indexable a => Gamma -> a -> Gamma
(+>) xs x = (index x) : xs

-- | remove context up to a marker
cut :: GammaIndex -> Gamma -> Stack Gamma
cut _ [] = throwError EmptyCut
cut i (x:xs)
  | i == x = return xs
  | otherwise = cut i xs

-- | Look up a type annotated expression
lookupE :: Expr -> Gamma -> Maybe TypeSet
lookupE _ [] = Nothing
lookupE e ((AnnG e' t):gs)
  | e == e' = Just t
  | otherwise = lookupE e gs
lookupE e (_:gs) = lookupE e gs

-- | Look up a solved existential type variable
lookupT :: TVar -> Gamma -> Maybe Type
lookupT _ [] = Nothing
lookupT v ((SolvedG v' t):gs)
  | v == v' = Just t
  | otherwise = lookupT v gs
lookupT v (_:gs) = lookupT v gs

-- | Look up the source of a function
lookupSrc ::
     (EVar, Lang) -> Gamma -> Maybe (EVar, Lang, Maybe Path, EVar)
lookupSrc _ [] = Nothing
lookupSrc (e, l) (SrcG x@(e', l', _, _):rs)
  | e == e' && l == l' = Just x
  | otherwise = lookupSrc (e, l) rs
lookupSrc x (_:rs) = lookupSrc x rs

access1 :: Indexable a => a -> Gamma -> Maybe (Gamma, GammaIndex, Gamma)
access1 gi gs =
  case elemIndex (index gi) gs of
    (Just 0) -> Just ([], head gs, tail gs)
    (Just i) -> Just (take i gs, gs !! i, drop (i + 1) gs)
    _ -> Nothing

access2 ::
     (Indexable a)
  => a
  -> a
  -> Gamma
  -> Maybe (Gamma, GammaIndex, Gamma, GammaIndex, Gamma)
access2 lgi rgi gs =
  case access1 lgi gs of
    Just (ls, x, rs) ->
      case access1 rgi rs of
        Just (ls', y, rs') -> Just (ls, x, ls', y, rs')
        _ -> Nothing
    _ -> Nothing

ann :: Expr -> Type -> Expr
ann (AnnE e _) t = AnnE e t
ann e@(Declaration _ _) _ = e
ann e@(Signature _ _) _ = e
ann e t = AnnE e t

generalize :: Type -> Type
generalize t = generalize' existentialMap t
  where
    generalize' :: [(TVar, TVar)] -> Type -> Type
    generalize' [] t' = t'
    generalize' ((e, r):xs) t' = generalize' xs (generalizeOne e r t')
    existentialMap =
      zip (Set.toList (findExistentials t)) (map (TV Nothing . MT.pack) variables)
    variables = [1 ..] >>= flip replicateM ['a' .. 'z']
    findExistentials :: Type -> Set.Set TVar
    findExistentials UniT = Set.empty
    findExistentials (VarT _) = Set.empty
    findExistentials (ExistT v) = Set.singleton v
    findExistentials (Forall v t') = Set.delete v (findExistentials t')
    findExistentials (FunT t1 t2) =
      Set.union (findExistentials t1) (findExistentials t2)
    findExistentials (ArrT _ ts) = Set.unions (map findExistentials ts)
    findExistentials (RecT rs) = Set.unions (map (findExistentials . snd) rs)
    generalizeOne :: TVar -> TVar -> Type -> Type
    generalizeOne v0 r t0 = Forall r (f v0 t0)
      where
        f :: TVar -> Type -> Type
        f v t1@(ExistT v')
          | v == v' = VarT r
          | otherwise = t1
        f v (FunT t1 t2) = FunT (f v t1) (f v t2)
        f v t1@(Forall x t2)
          | v /= x = Forall x (f v t2)
          | otherwise = t1
        f v (ArrT v' xs) = ArrT v' (map (f v) xs)
        f v (RecT xs) = RecT (map (\(v', _) -> (v', f v t)) xs)
        f _ t1 = t1

generalizeE :: Expr -> Expr
generalizeE = mapT generalize

newvar :: Maybe Lang -> Stack Type
newvar lang = do
  s <- MS.get
  let v = newvars !! stateVar s
  MS.put $ s {stateVar = stateVar s + 1}
  return (ExistT $ TV lang v)
  where
    newvars =
      zipWith (\x y -> MT.pack (x ++ show y)) (repeat "t") ([0 ..] :: [Integer])

newqul :: TVar -> Stack TVar
newqul (TV l v) = do
  s <- MS.get
  let v' = TV l (v <> "." <> (MT.pack . show $ stateQul s)) -- create a new variable such as "a.0"
  MS.put $ s {stateQul = stateQul s + 1}
  return v'
