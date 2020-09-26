{-|
Module      : Morloc.Parser.Desugar
Description : Write Module objects to resolve type aliases and such
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Parser.Desugar (desugar) where

import Morloc.Namespace
import qualified Data.Map as Map

desugar :: [Module] -> MorlocMonad [Module]
desugar = mapM desugarOne

desugarOne :: Module -> MorlocMonad Module
desugarOne m = do
  let (keys, vals) = unzip (Map.toList (moduleTypeMap m))
  vals' <- mapM (desugarTypeSet (moduleTypedefs m)) vals
  return $ m { moduleTypeMap = Map.fromList (zip keys vals') }

desugarTypeSet :: Map.Map (TVar, Maybe Lang) (Type, [TVar]) -> TypeSet -> MorlocMonad TypeSet
desugarTypeSet h (TypeSet gentype contypes) =
  TypeSet <$> mapM (desugarEType h) gentype <*> mapM (desugarEType h) contypes

desugarEType :: Map.Map (TVar, Maybe Lang) (Type, [TVar]) -> EType -> MorlocMonad EType 
desugarEType h (EType t ps cs) = EType <$> desugarType h t <*> pure ps <*> pure cs

desugarType :: Map.Map (TVar, Maybe Lang) (Type, [TVar]) -> Type -> MorlocMonad Type
desugarType h (VarT v) = return $ VarT v
desugarType h (ExistT v ts ds) = ExistT v <$> mapM (desugarType h) ts <*> pure ds
desugarType h (Forall v t) = Forall v <$> desugarType h t
desugarType h (FunT t1 t2) = FunT <$> desugarType h t1 <*> desugarType h t2
desugarType h (ArrT v ts) = ArrT v <$> mapM (desugarType h) ts
desugarType h (NamT v rs) = do
  let keys = map fst rs   
  vals <- mapM (desugarType h) (map snd rs)
  return (NamT v (zip keys vals))
