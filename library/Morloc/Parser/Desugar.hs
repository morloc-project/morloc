{-|
Module      : Morloc.Parser.Desugar
Description : Write Module objects to resolve type aliases and such
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Parser.Desugar (desugar, desugarType) where

import Morloc.Namespace
import qualified Morloc.Monad as MM
import Morloc.Pretty ()
import qualified Morloc.Data.Doc as MD
import qualified Morloc.Data.Text as MT
import qualified Data.Map as Map

desugar :: [Module] -> MorlocMonad [Module]
desugar = mapM desugarModule

desugarModule :: Module -> MorlocMonad Module
desugarModule m = do
  checkForSelfRecursion (moduleTypedefs m)
  expr' <- mapM (desugarExpr (moduleTypedefs m)) (moduleBody m)
  return $ m { moduleBody = expr' }

checkForSelfRecursion :: Map.Map TVar (Type, [TVar]) -> MorlocMonad ()
checkForSelfRecursion h = mapM_ (uncurry f) [(v,t) | (v,(t,_)) <- Map.toList h] where
  f :: TVar -> Type -> MorlocMonad ()
  f v (VarT v')
    | v == v' = MM.throwError . SelfRecursiveTypeAlias $ v
    | otherwise = return ()
  f _ (ExistT _ _ _) = MM.throwError $ CallTheMonkeys "existential crisis"
  f v (Forall _ t) = f v t
  f v (FunT t1 t2) = f v t1 >> f v t2
  f v (ArrT v0 ts)
    | v == v0 = MM.throwError . SelfRecursiveTypeAlias $ v
    | otherwise = mapM_ (f v) ts
  f v (NamT v0 rs)
    | v == v0 = MM.throwError . SelfRecursiveTypeAlias $ v
    | otherwise = mapM_ (f v) (map snd rs)

desugarExpr :: Map.Map TVar (Type, [TVar]) -> Expr -> MorlocMonad Expr
desugarExpr _ e@(SrcE _) = return e
desugarExpr h (Signature v t) = Signature v <$> desugarEType h t
desugarExpr h (Declaration v e) = Declaration v <$> desugarExpr h e
desugarExpr _ UniE = return UniE
desugarExpr _ e@(VarE _) = return e
desugarExpr h (ListE xs) = ListE <$> mapM (desugarExpr h) xs
desugarExpr h (TupleE xs) = TupleE <$> mapM (desugarExpr h) xs
desugarExpr h (LamE v e) = LamE v <$> desugarExpr h e
desugarExpr h (AppE e1 e2) = AppE <$> desugarExpr h e1 <*> desugarExpr h e2
desugarExpr h (AnnE e ts) = AnnE <$> desugarExpr h e <*> mapM (desugarType [] h) ts
desugarExpr _ e@(NumE _) = return e
desugarExpr _ e@(LogE _) = return e
desugarExpr _ e@(StrE _) = return e
desugarExpr h (RecE rs) = do
  es <- mapM (desugarExpr h) (map snd rs)
  return (RecE (zip (map fst rs) es))

desugarEType :: Map.Map TVar (Type, [TVar]) -> EType -> MorlocMonad EType 
desugarEType h (EType t ps cs) = EType <$> desugarType [] h t <*> pure ps <*> pure cs

desugarType :: [TVar] -> Map.Map TVar (Type, [TVar]) -> Type -> MorlocMonad Type
desugarType s h t0@(VarT v)
  | elem v s = MM.throwError . MutuallyRecursiveTypeAlias $ s 
  | otherwise = case Map.lookup v h of
      (Just (t, [])) -> desugarType (v:s) h t
      (Just (t, vs)) -> MM.throwError $ BadTypeAliasParameters v 0 (length vs)
      Nothing -> return t0 
desugarType s h (ExistT v ts ds) = ExistT v <$> mapM (desugarType s h) ts <*> pure ds
desugarType s h (Forall v t) = Forall v <$> desugarType s h t
desugarType s h (FunT t1 t2) = FunT <$> desugarType s h t1 <*> desugarType s h t2
desugarType s h t0@(ArrT v ts)
  | elem v s = MM.throwError . MutuallyRecursiveTypeAlias $ s 
  | otherwise = case Map.lookup v h of
      (Just (t, vs)) ->
        if length ts == length vs
        then desugarType (v:s) h (foldr parsub t (zip vs ts)) -- substitute parameters into alias
        else MM.throwError $ BadTypeAliasParameters v (length vs) (length ts)
      Nothing -> return t0
desugarType s h (NamT v rs) = do
  let keys = map fst rs   
  vals <- mapM (desugarType s h) (map snd rs)
  return (NamT v (zip keys vals))

parsub :: (TVar, Type) -> Type -> Type
parsub (v, t2) t1@(VarT v0)
  | v0 == v = t2 -- substitute
  | otherwise = t1 -- keep the original
parsub _ (ExistT _ _ _) = error "What the bloody hell is an existential doing down here?"
parsub pair (Forall v t1) = Forall v (parsub pair t1)
parsub pair (FunT a b) = FunT (parsub pair a) (parsub pair b)
parsub pair (ArrT v ts) = ArrT v (map (parsub pair) ts)
parsub pair (NamT v rs) = NamT v (zip (map fst rs) (map (parsub pair . snd) rs))
