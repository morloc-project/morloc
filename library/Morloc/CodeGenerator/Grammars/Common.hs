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
  -- * Naming conventions
  , svarNamer
  , nvarNamer
  , helperNamer
  , argNamer
  , manNamer
  -- * Utilities
  , makeManifoldIndexer
  , translateManifold
  , expandManifoldForm
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
-- process the poolExpr variales into list syntax in the given language.
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

  surroundSM f sm@(SerialManifold i _ _ _) = descend i sm f 

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
  weave (D x ((i, Left se) : deps)) = weave $ D (SerialLetN i se (typeFof x, x)) deps
  weave (D x ((i, Right ne) : deps)) = weave $ D (NativeLetN i (typeFof ne, ne) (typeFof x, x)) deps
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
      t <- case typeMof e of
        Passthrough -> return Nothing
        (Serial ft) -> return $ Just ft
        _ -> return Nothing
        -- _ -> error "This type must be serialized"
      return $ D (LetVarS t i) ((i, Left e) : deps)

  isAtomic (LetVarS _ _) = True 
  isAtomic (BndVarS _ _) = True 
  isAtomic (ReturnS _) = True 
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

  invertSerialManifoldM :: SerialManifold_ (D SerialArg) (D SerialExpr) -> Index (D SerialManifold)
  invertSerialManifoldM (SerialManifold_ m lang form (mayT, se)) = do
    let form' = first (unD . snd) form
        deps = biappend (getDeps . snd) (const []) form
    return (D (SerialManifold m lang form' (mayT, weave se)) deps)

  invertNativeManifoldM :: NativeManifold_ (D NativeArg) (D NativeExpr) -> Index (D NativeManifold)
  invertNativeManifoldM (NativeManifold_ m lang form (t, weave -> ne)) = do
    let form' = first (unD . snd) form
        deps = biappend (getDeps . snd) (const []) form
    return (D (NativeManifold m lang form' (t, ne)) deps)

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
  invertSerialExprM (NativeLetS_ i (_, D ne1 lets1) (D se2 lets2)) =
    return $ D se2 (lets2 <> ((i, Right ne1) : lets1))
  invertSerialExprM (LetVarS_ t i) = atomize (LetVarS t i) []
  invertSerialExprM (BndVarS_ t i) = atomize (BndVarS t i) []
  invertSerialExprM (SerializeS_ s (D ne lets)) = atomize (SerializeS s ne) lets

  invertNativeExprM :: NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) -> Index (D NativeExpr)
  invertNativeExprM (AppSrcN_ t src nativeArgs) = do
    let nativeArgs' = map unD nativeArgs
        deps = concatMap getDeps nativeArgs
    atomize (AppSrcN t src nativeArgs') deps
  invertNativeExprM (ManN_ t (D nm lets)) = atomize (ManN t nm) lets
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
    { opSerialManifoldM = return . foldlSM max 0
    , opNativeManifoldM = return . foldlNM max 0
    , opSerialExprM = findSerialIndices
    , opNativeExprM = findNativeIndices
    , opSerialArgM = return . foldlSA max 0
    , opNativeArgM = return . foldlNA max 0
    }

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
  => (MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> MDoc) -- make function
  -> ([Arg TypeM] -> MDoc -> MDoc)
  -> Int -> ManifoldForm (t, PoolDocs) ArgTypes -> PoolDocs -> PoolDocs
translateManifold makeFunction makeLambda m form (PoolDocs completeManifolds body priorLines priorExprs) =
  let args = abiappend (\i (t,_) -> [Arg i (typeMof t)])
                       (\i r -> [Arg i t | t <- argTypesToTypeM r]) form
      mname = manNamer m
      newManifold = makeFunction mname args priorLines body
      call = case form of
        (ManifoldPass _) -> mname
        (ManifoldFull rs) -> mname <> tupled (map (argNamer . second fst) rs)
        (ManifoldPart rs vs) ->
          let variableNames = unArgTypes vs
              functionCall = (mname <> tupled (map argNamer (map (second (typeMof . fst)) rs <> unArgTypes vs)))
          in makeLambda variableNames functionCall
  in PoolDocs
      { poolCompleteManifolds = newManifold : completeManifolds
      , poolExpr = call
      , poolPriorLines = []
      , poolPriorExprs = priorExprs
      }
  where
    unArgTypes :: [Arg ArgTypes] -> [Arg TypeM]
    unArgTypes xs = concat [[Arg i y | y <- ys] | (Arg i ys) <- map (second argTypesToTypeM) xs]

expandManifoldForm :: (a -> b) -> (TypeM -> b) -> ManifoldForm a ArgTypes -> [Arg b]
expandManifoldForm f g = concat . abilist
  (\i a -> [Arg i (f a)])
  (\i b -> [Arg i (g x) | x <- argTypesToTypeM b])
