{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( invertSerialManifold
  , PoolDocs(..)
  , mergePoolDocs
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.Monad (Index, runIndex, newIndex, runIdentity)

-- Stores pieces of code made while building a pool
data PoolDocs = PoolDocs
  { poolCompleteManifolds :: [MDoc]
    -- ^ completely generated manifolds
  , poolExpr :: MDoc
    -- ^ the inplace expression
  , poolPriorLines :: [MDoc]
    -- ^ lines to precede the returned expression
  , poolPriorExprs :: [MDoc]
    -- ^ expressions that should precede this manifold, may include helper
    -- functions or imports
  }

-- | Merge a series of pools, keeping prior lines, expression and manifolds, but
-- merging bodies with a function. For example, merge all elements in a list and
-- process the poolExpr variales into list syntax in the given language.
mergePoolDocs :: ([MDoc] -> MDoc) -> [PoolDocs] -> PoolDocs
mergePoolDocs f ms = PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds ms
    , poolExpr = f (map poolExpr ms)
    , poolPriorLines = concatMap poolPriorLines ms
    , poolPriorExprs = concatMap poolPriorExprs ms
    }



-- Represents the dependency of a on previously bound expressions
data D a = D a [(Int, Either SerialExpr NativeExpr)]

unD :: D a -> a
unD (D a _) = a

getDeps :: D a -> [(Int, Either SerialExpr NativeExpr)]
getDeps (D _ d) = d

class Dependable a where
  weave :: D a -> a
  atomize :: a -> [(Int, Either SerialExpr NativeExpr)] -> Index (D a)
  isAtomic :: a -> Bool

instance Dependable NativeExpr where
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetN i se (typeFof x, x)) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetN i (typeFof ne, ne) (typeFof x, x)) deps
  weave (D x []) = x

  atomize e deps
    | isAtomic e = return $ D e deps
    | otherwise = do
      i <- newIndex
      return $ D (LetVarN (typeFof e) i) ((i, Right e) : deps)

  isAtomic AppSrcN{} = False
  isAtomic AppManN{} = False
  isAtomic SerialLetN{} = False
  isAtomic NativeLetN{} = False
  isAtomic DeserializeN{} = False
  isAtomic AccN{} = False
  isAtomic ListN{} = False
  isAtomic TupleN{} = False
  isAtomic _ = True

instance Dependable SerialExpr where
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetS i se x) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetS i (typeFof ne, ne) x) deps
  weave (D x []) = x

  atomize e deps
    | isAtomic e = return $ D e deps
    | otherwise = do
      i <- newIndex
      return $ D (LetVarS i) ((i, Left e) : deps)

  isAtomic (LetVarS _) = True 
  isAtomic (BndVarS _) = True 
  isAtomic (ReturnS _) = True 
  isAtomic _ = False


invertSerialManifold :: SerialManifold -> SerialManifold
invertSerialManifold sm0 =
  runIndex (maxIndex sm0) (foldSerialManifoldM fm sm0)
  where

  fm = FoldManifoldM
    { opSerialManifoldM = invertSerialManifoldM
    , opNativeManifoldM = invertNativeManifoldM
    , opSerialExprM = invertSerialExprM
    , opNativeExprM = invertNativeExprM
    , opSerialArgM = invertSerialArgM
    , opNativeArgM = invertNativeArgM
    }

  invertSerialManifoldM :: SerialManifold_ (D SerialExpr) -> Index SerialManifold
  invertSerialManifoldM (SerialManifold_ m lang form (weave -> se)) =
    return $ SerialManifold m lang form se

  invertNativeManifoldM :: NativeManifold_ (D NativeExpr) -> Index NativeManifold
  invertNativeManifoldM (NativeManifold_ m lang form (t, weave -> ne)) = return $ NativeManifold m lang form (t, ne)

  invertSerialExprM
    :: SerialExpr_ SerialManifold (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg)
    -> Index (D SerialExpr)
  invertSerialExprM (AppManS_ sm eitherArgs) = do
    let eitherArgs' = map (bimap unD unD) eitherArgs
        deps = concatMap (either getDeps getDeps) eitherArgs
    atomize (AppManS sm eitherArgs') deps
  invertSerialExprM (AppPoolS_ pool serialArgs) = do
    let serialArgs' = map unD serialArgs
        deps = concatMap getDeps serialArgs
    atomize (AppPoolS pool serialArgs') deps
  invertSerialExprM (ReturnS_ (D se lets)) = return $ D (ReturnS se) lets
  invertSerialExprM (SerialLetS_ i (D se1 lets1) (D se2 lets2)) =
    return $ D se2 (lets2 <> ((i, Left  se1) : lets1))
  invertSerialExprM (NativeLetS_ i (_, D ne1 lets1) (D se2 lets2)) =
    return $ D se2 (lets2 <> ((i, Right ne1) : lets1))
  invertSerialExprM (LetVarS_ i) = atomize (LetVarS i) []
  invertSerialExprM (BndVarS_ i) = atomize (BndVarS i) []
  invertSerialExprM (SerializeS_ s (D ne lets)) = atomize (SerializeS s ne) lets

  invertNativeExprM :: NativeExpr_ NativeManifold (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) -> Index (D NativeExpr)
  invertNativeExprM (AppSrcN_ t src nativeArgs) = do
    let nativeArgs' = map unD nativeArgs
        deps = concatMap getDeps nativeArgs
    atomize (AppSrcN t src nativeArgs') deps
  invertNativeExprM (AppManN_ t nm eitherArgs) = do
    let eitherArgs' = map (bimap unD unD) eitherArgs
        deps = concatMap (either getDeps getDeps) eitherArgs
    atomize (AppManN t nm eitherArgs') deps
  invertNativeExprM (ReturnN_ t (D ne lets)) = atomize (ReturnN t ne) lets
  invertNativeExprM (SerialLetN_ i (D se1 lets1) (_, D ne2 lets2)) =
    return $ D ne2 (lets2 <> ((i, Left  se1) : lets1))
  invertNativeExprM (NativeLetN_ i (_, D ne1 lets1) (_, D se2 lets2)) =
    return $ D se2 (lets2 <> ((i, Right ne1) : lets1))
  invertNativeExprM (LetVarN_ t i) = atomize (LetVarN t i) []
  invertNativeExprM (BndVarN_ t i) = atomize (BndVarN t i) []
  invertNativeExprM (DeserializeN_ t s (D se lets)) = atomize (DeserializeN t s se) lets
  invertNativeExprM (AccN_ t o v (D ne deps) key) = atomize (AccN t o v ne key) deps
  invertNativeExprM (SrcN_ t src) = atomize (SrcN t src) []
  invertNativeExprM (ListN_ v t nes) = atomize (ListN v t (map unD nes)) (concatMap getDeps nes)
  invertNativeExprM (TupleN_ v xs) = atomize (TupleN v (map (second unD) xs)) (concatMap (getDeps . snd) xs)
  invertNativeExprM (RecordN_ o v ps rs) = atomize (RecordN o v ps (map (second (second unD)) rs)) (concatMap (getDeps . snd . snd) rs)
  invertNativeExprM (LogN_ v x) = atomize (LogN v x) []
  invertNativeExprM (RealN_ v x) = atomize (RealN v x) []
  invertNativeExprM (IntN_ v x) = atomize (IntN v x) []
  invertNativeExprM (StrN_ v x) = atomize (StrN v x) []
  invertNativeExprM (NullN_ v) = atomize (NullN v) []

  invertSerialArgM :: SerialArg_ SerialManifold (D SerialExpr) -> Index (D SerialArg)
  invertSerialArgM (SerialArgManifold_ sm) = return $ D (SerialArgManifold sm) []
  invertSerialArgM (SerialArgExpr_ (D se deps)) = return $ D (SerialArgExpr se) deps

  invertNativeArgM :: NativeArg_ NativeManifold (D NativeExpr) -> Index (D NativeArg)
  invertNativeArgM (NativeArgManifold_ nm) = return $ D (NativeArgManifold nm) []
  invertNativeArgM (NativeArgExpr_ (D ne deps)) = return $ D (NativeArgExpr ne) deps


maxIndex :: SerialManifold -> Int
maxIndex = (+1) . runIdentity . foldSerialManifoldM fm
  where
  fm = FoldManifoldM
    { opSerialManifoldM = return . foldl max 0
    , opNativeManifoldM = return . foldl max 0
    , opSerialExprM = findSerialIndices
    , opNativeExprM = findNativeIndices
    , opSerialArgM = return . foldl max 0
    , opNativeArgM = return . foldl max 0
    }

  findSerialIndices :: Monad m => SerialExpr_ Int Int Int Int Int -> m Int
  findSerialIndices (LetVarS_ i) = return i
  findSerialIndices (BndVarS_ i) = return i
  findSerialIndices e = return $ foldlSE max 0 e

  findNativeIndices :: Monad m => NativeExpr_ Int Int Int Int Int -> m Int
  findNativeIndices (LetVarN_ _ i) = return i
  findNativeIndices (BndVarN_ _ i) = return i
  findNativeIndices e = return $ foldlNE max 0 e
