{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( invertSerialManifold
  , PoolDocs(..)
  , mergePoolDocs
  -- * Naming conventions
  , svarNamer
  , nvarNamer
  , helperNamer
  , argNamer
  , manNamer
  -- * Utilities
  , makeManifoldIndexer
  , translateManifold
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
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

instance Defaultable PoolDocs where
  defaultValue = PoolDocs
    { poolCompleteManifolds = []
    , poolExpr = ""
    , poolPriorLines = []
    , poolPriorExprs = []
    }

-- | Merge a series of pools, keeping prior lines, expression and manifolds, but
-- merging bodies with a function. For example, merge all elements in a list and
-- process the poolExpr variables into list syntax in the given language.
mergePoolDocs :: ([MDoc] -> MDoc) -> [PoolDocs] -> PoolDocs
mergePoolDocs f ms = PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds ms
    , poolExpr = f (map poolExpr ms)
    , poolPriorLines = concatMap poolPriorLines ms
    , poolPriorExprs = concatMap poolPriorExprs ms
    }


svarNamer :: Int -> MDoc
svarNamer i = "s" <> viaShow i

nvarNamer :: Int -> MDoc
nvarNamer i = "n" <> viaShow i

helperNamer :: Int -> MDoc
helperNamer i = "helper" <> viaShow i

argNamer :: HasTypeM t => Arg t -> MDoc
argNamer (Arg i (typeMof -> Native _)) = nvarNamer i
argNamer (Arg i (typeMof -> Function _ _)) = nvarNamer i
argNamer (Arg i _) = svarNamer i

-- create a name for a manifold based on a unique id
manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i


-- The surround rules control the setting of manifold ids across the recursion
makeManifoldIndexer :: Monad m => m Int -> (Int -> m ()) -> SurroundManifoldM m sm nm se ne sr nr
makeManifoldIndexer getId putId = defaultValue
  { surroundSerialManifoldM = surroundSM
  , surroundNativeManifoldM = surroundNM
  }
  where

  -- | Run a computation in a child manifold, manage manifold indices
  descend childManifoldIndex x f = do
    originalManifoldIndex <- getId
    putId childManifoldIndex
    x' <- f x
    putId originalManifoldIndex
    return x'

  surroundSM f sm@(SerialManifold i _ _ _ _) = descend i sm f

  surroundNM f nm@(NativeManifold i _ _ _) = descend i nm f


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
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetN i se x) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetN i ne x) deps
  weave (D x []) = x

  atomize e deps
    | isAtomic e = return $ D e deps
    | otherwise = do
      i <- newIndex
      return $ D (LetVarN (typeFof e) i) ((i, Right e) : deps)

  isAtomic AppSrcN{} = False
  isAtomic ManN{} = False
  isAtomic SerialLetN{} = False
  isAtomic NativeLetN{} = False
  isAtomic AccN{} = False
  isAtomic ListN{} = False
  isAtomic TupleN{} = False
  isAtomic _ = True

instance Dependable SerialExpr where
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetS i se x) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetS i ne x) deps
  weave (D x []) = x

  atomize e deps
    | isAtomic e = return $ D e deps
    | otherwise = do
      i <- newIndex
      t <- case typeMof e of
        Passthrough -> return Nothing
        (Serial ft) -> return $ Just ft
        _ -> return Nothing
        -- _ -> error "This type must be serialized"
      return $ D (LetVarS t i) ((i, Left e) : deps)

  isAtomic (LetVarS _ _) = True
  isAtomic (BndVarS _ _) = True
  isAtomic (ReturnS _) = True
  isAtomic (SerializeS _ _) = True
  isAtomic _ = False


invertSerialManifold :: SerialManifold -> SerialManifold
invertSerialManifold sm0 =
  runIndex (maxIndex sm0) (unD <$> foldSerialManifoldM fm sm0)
  where

  fm = FoldManifoldM
    { opSerialManifoldM = invertSerialManifoldM
    , opNativeManifoldM = invertNativeManifoldM
    , opSerialExprM = invertSerialExprM
    , opNativeExprM = invertNativeExprM
    , opSerialArgM = invertSerialArgM
    , opNativeArgM = invertNativeArgM
    }

  invertSerialManifoldM :: SerialManifold_ (D SerialExpr) -> Index (D SerialManifold)
  invertSerialManifoldM (SerialManifold_ m lang form headForm se) = do
    return (D (SerialManifold m lang form headForm (weave se)) [])

  invertNativeManifoldM :: NativeManifold_ (D NativeExpr) -> Index (D NativeManifold)
  invertNativeManifoldM (NativeManifold_ m lang form (weave -> ne)) = do
    return (D (NativeManifold m lang form ne) [])

  invertSerialExprM
    :: SerialExpr_ (D SerialManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg)
    -> Index (D SerialExpr)
  invertSerialExprM (ManS_ (D sm lets)) = return $ D (ManS sm) lets
  invertSerialExprM (AppPoolS_ t pool serialArgs) = do
    let serialArgs' = map unD serialArgs
        deps = concatMap getDeps serialArgs
    atomize (AppPoolS t pool serialArgs') deps
  invertSerialExprM (ReturnS_ (D se lets)) = return $ D (ReturnS se) lets
  invertSerialExprM (SerialLetS_ i (D se1 lets1) (D se2 lets2)) =
    return $ D se2 (lets2 <> ((i, Left  se1) : lets1))
  invertSerialExprM (NativeLetS_ i (D ne1 lets1) (D se2 lets2)) =
    return $ D se2 (lets2 <> ((i, Right ne1) : lets1))
  invertSerialExprM (LetVarS_ t i) = atomize (LetVarS t i) []
  invertSerialExprM (BndVarS_ t i) = atomize (BndVarS t i) []
  invertSerialExprM (SerializeS_ s (D ne lets)) = atomize (SerializeS s ne) lets

  invertNativeExprM :: NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) -> Index (D NativeExpr)
  invertNativeExprM (AppSrcN_ t src qs nativeArgs) = do
    let nativeArgs' = map unD nativeArgs
        deps = concatMap getDeps nativeArgs
    atomize (AppSrcN t src qs nativeArgs') deps
  invertNativeExprM (ManN_ (D nm lets)) = atomize (ManN nm) lets
  invertNativeExprM (ReturnN_ (D ne lets)) = atomize (ReturnN ne) lets

  invertNativeExprM (SerialLetN_ i (D se1 lets1) (D ne2 lets2)) =
    return $ D ne2 (lets2 <> ((i, Left  se1) : lets1))
  invertNativeExprM (NativeLetN_ i (D ne1 lets1) (D ne2 lets2)) =
    return $ D ne2 (lets2 <> ((i, Right ne1) : lets1))

  invertNativeExprM (LetVarN_ t i) = atomize (LetVarN t i) []
  invertNativeExprM (BndVarN_ t i) = atomize (BndVarN t i) []
  invertNativeExprM (DeserializeN_ t s (D se lets)) = atomize (DeserializeN t s se) lets
  invertNativeExprM (AccN_ o v (D ne deps) key) = atomize (AccN o v ne key) deps
  invertNativeExprM (SrcN_ t src) = atomize (SrcN t src) []
  invertNativeExprM (ListN_ v t nes) = atomize (ListN v t (map unD nes)) (concatMap getDeps nes)
  invertNativeExprM (TupleN_ v xs) = atomize (TupleN v (map unD xs)) (concatMap getDeps xs)
  invertNativeExprM (RecordN_ o v ps rs) = atomize (RecordN o v ps (map (second unD) rs)) (concatMap (getDeps . snd) rs)
  invertNativeExprM (LogN_ v x) = atomize (LogN v x) []
  invertNativeExprM (RealN_ v x) = atomize (RealN v x) []
  invertNativeExprM (IntN_ v x) = atomize (IntN v x) []
  invertNativeExprM (StrN_ v x) = atomize (StrN v x) []
  invertNativeExprM (NullN_ v) = atomize (NullN v) []

  invertSerialArgM :: SerialArg_ (D SerialManifold) (D SerialExpr) -> Index (D SerialArg)
  invertSerialArgM (SerialArgManifold_ (D sm deps)) = return $ D (SerialArgManifold sm) deps
  invertSerialArgM (SerialArgExpr_ (D se deps)) = return $ D (SerialArgExpr se) deps

  invertNativeArgM :: NativeArg_ (D NativeManifold) (D NativeExpr) -> Index (D NativeArg)
  invertNativeArgM (NativeArgManifold_ (D nm deps)) = return $ D (NativeArgManifold nm) deps
  invertNativeArgM (NativeArgExpr_ (D ne deps)) = return $ D (NativeArgExpr ne) deps


maxIndex :: SerialManifold -> Int
maxIndex = (+1) . runIdentity . foldSerialManifoldM fm
  where
  fm = FoldManifoldM
    { opSerialManifoldM = findSerialManifoldIndices
    , opNativeManifoldM = findNativeManifoldIndices
    , opSerialExprM = findSerialIndices
    , opNativeExprM = findNativeIndices
    , opSerialArgM = return . foldlSA max 0
    , opNativeArgM = return . foldlNA max 0
    }

  findSerialManifoldIndices :: Monad m => SerialManifold_ Int -> m Int
  findSerialManifoldIndices (SerialManifold_ _ _ form _ bodyMax) = do
    let formIndices = abilist const const form
    return $ foldl max bodyMax formIndices

  findNativeManifoldIndices :: Monad m => NativeManifold_ Int -> m Int
  findNativeManifoldIndices (NativeManifold_ _ _ form bodyMax) = do
    let formIndices = abilist const const form
    return $ foldl max bodyMax formIndices

  findSerialIndices :: Monad m => SerialExpr_ Int Int Int Int Int -> m Int
  findSerialIndices (LetVarS_ _ i) = return i
  findSerialIndices (BndVarS_ _ i) = return i
  findSerialIndices e = return $ foldlSE max 0 e

  findNativeIndices :: Monad m => NativeExpr_ Int Int Int Int Int -> m Int
  findNativeIndices (LetVarN_ _ i) = return i
  findNativeIndices (BndVarN_ _ i) = return i
  findNativeIndices e = return $ foldlNE max 0 e

translateManifold
  :: HasTypeM t
  => (MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> MDoc) -- make function
  -> ([Arg TypeM] -> MDoc -> MDoc)
  -> Int -> ManifoldForm (Or TypeS TypeF) t -> Maybe HeadManifoldForm -> PoolDocs -> PoolDocs
translateManifold makeFunction makeLambda m form headForm (PoolDocs completeManifolds body priorLines priorExprs) =
  let args = abiappend (\i r -> [Arg i t | t <- bilist typeMof typeMof r])
                       (\i t -> [Arg i (typeMof t)]) form
      mname = manNamer m
      newManifold = makeFunction mname args priorLines body headForm
      call = case form of
        (ManifoldPass _) -> mname
        (ManifoldFull rs) -> mname <> tupled (map argNamer (asArgs rs))
        (ManifoldPart rs vs) ->
          let variableNames = [Arg i (typeMof t) | (Arg i t) <- vs]
              functionCall = mname <> tupled
                (map argNamer $ asArgs rs <> variableNames)
          in makeLambda variableNames functionCall

  in PoolDocs
      { poolCompleteManifolds = newManifold : completeManifolds
      , poolExpr = call
      , poolPriorLines = []
      , poolPriorExprs = priorExprs
      }
  where
    asArgs :: [Arg (Or TypeS TypeF)] -> [Arg TypeM]
    asArgs rs = concat [[Arg i t | t <- bilist typeMof typeMof orT] | (Arg i orT) <- rs]
