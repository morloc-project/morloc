{-|
Module      : Morloc.Manifold
Description : Functions for dealing with Manifolds
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Manifold
  ( getManSrcs
  , filterByManifoldClass
  , getUnpackers
  , getPacker
  , getUsedManifolds
  , determineManifoldClass
  , isMorlocCall
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import qualified Data.Map.Strict as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS

-- | Get the paths to the sources 
getManSrcs ::
     Lang -> (MT.Text -> MorlocMonad MDoc) -> [Manifold] -> MorlocMonad [MDoc]
getManSrcs lang f ms = MM.mapM f . nub . mapMaybe getManSrc $ ms'
  where
    getManSrc :: Manifold -> Maybe MT.Text
    getManSrc m =
      case (mSourcePath m, mModulePath m) of
        (Just srcpath, Just modpath) ->
          case MS.takeDirectory modpath of
            "." -> Just srcpath
            path -> Just $ path <> "/" <> srcpath
        _ -> Nothing
    -- select the manifolds that are in the same language as the grammar 
    ms' = filter (\m -> lang == mLang m) ms

determineManifoldClass :: Lang -> Manifold -> ManifoldClass
determineManifoldClass lang m
  | mDefined m && not (mCalled m) && not (mExported m) = Uncalled
  | mLang m == lang && not (mCalled m) && mSourced m && mExported m = Source
  | not (mCalled m) && mExported m = Uncalled
  | mLang m == lang = Cis
  | (mLang m /= lang) && mCalled m = Trans
  | otherwise = Uncalled

filterByManifoldClass :: Lang -> ManifoldClass -> [Manifold] -> [Manifold]
filterByManifoldClass lang mc ms =
  filter (\m -> mc == determineManifoldClass lang m) ms

-- | Is this manifold a called morloc function?
isMorlocCall :: Manifold -> Bool
isMorlocCall m = mDefined m && isNothing (mComposition m)

-- find a packer for each argument passed to a manifold
getUnpackers :: SerialMap -> Manifold -> MorlocMonad [Maybe MDoc]
getUnpackers hash m =
  case mConcreteType m of
    (Just (MFuncType _ ts _)) -> mapM (getUnpacker hash) ts
    (Just t) -> MM.throwError . TypeError $ "Unpackers must be functions, found: " <> MT.show' t
    Nothing ->
      case mAbstractType m of
        (Just (MFuncType _ ts _)) -> mapM (getUnpacker hash) ts
        (Just t) -> MM.throwError . TypeError $ "Unpackers must be functions, found: " <> MT.show' t
        Nothing ->
          MM.throwError . TypeError $
          "No type signature found for this manifold: " <> MT.pretty m
  where
    getUnpacker :: SerialMap -> MType -> MorlocMonad (Maybe MDoc)
    getUnpacker smap t =
      case (findMostSpecificType .
            Map.keys . Map.filterWithKey (\p _ -> childOf t p) $
            (serialUnpacker smap)) >>=
           (flip Map.lookup) (serialUnpacker smap) of
        (Just x) -> return (Just $ pretty x)
        Nothing -> return Nothing

-- | If a language-specific signature is given for the manifold, choose a
-- packer that matches the language-specific output type. Otherwise, search for
-- a packer that matches the morloc type.
-- TODO: make the MorlocMonad
getPacker :: SerialMap -> Manifold -> MDoc
getPacker hash m =
  case packerType of
    (Just t) ->
      case Map.lookup t (serialPacker hash) of
        (Just n) -> pretty n
        Nothing -> error "You should not be reading this"
    Nothing ->
      error . MT.unpack
        $  "No packer found for this serialmap and type: "
        <> MT.pretty hash <> "\n" <> MT.pretty m
  where
    packerType :: Maybe MType
    packerType =
      case cPacker of
        (Just x) -> Just x
        Nothing -> aPacker

    cPacker :: Maybe MType
    cPacker =
      case mConcreteType m of
        (Just (MFuncType _ _ t)) -> findMostSpecificType (packers t)
        (Just _) -> error "Ah shit"
        Nothing -> Nothing

    aPacker :: Maybe MType
    aPacker =
      case mAbstractType m of
        (Just (MFuncType _ _ t)) -> findMostSpecificType (packers t)
        (Just _) -> error "Ah shit"
        Nothing -> Nothing

    packers :: MType -> [MType]
    packers o =
      Map.keys . Map.filterWithKey (\p _ -> childOf o p) $ (serialPacker hash)

-- | Find the manifolds that must be defined in a pool for a given language.
getUsedManifolds :: Lang -> [Manifold] -> [Manifold]
getUsedManifolds lang ms = filter buildIt ms
  where
    buildIt :: Manifold -> Bool
    buildIt m =
      let mc = determineManifoldClass lang m
       in (mc == Cis || mc == Source) && not (mPassed m)

-- | Determine if the first MType object is equal to, or a specialization of,
-- the second MType object.
childOf ::
     MType -- ^ Instance type (e.g. Matrix Num 5 6)
  -> MType -- ^ Parent type (e.g. Matrix a m n)
  -> Bool -- ^ True if arg #1 is equal to or an instance of arg #2
childOf (MConcType _ n1 xs1) (MConcType _ n2 xs2)
  -- I currently don't check out the properties, should I?
 = n1 == n2 && all id (zipWith childOf xs1 xs2) -- child types must be the same
childOf (MConcType _ _ xs1) (MAbstType _ _ xs2) =
  length xs2 == 0 || all id (zipWith childOf xs1 xs2)
childOf (MAbstType _ _ xs1) (MAbstType _ _ xs2) =
  all id (zipWith childOf xs1 xs2)
childOf (MFuncType _ is1 o1) (MFuncType _ is2 o2) =
  all id (zipWith childOf is1 is2) && childOf o1 o2
childOf _ _ = False

-- | Get the most specifc type from a list of compatible types. By compatible I
-- mean types where either (childOf a b) or (childOf b a) is true. That is, one
-- of them must generalize the other.
findMostSpecificType :: [MType] -> Maybe MType
findMostSpecificType [] = Nothing
findMostSpecificType xs = Just (maximum xs)

-- | Find the most general type. Also see @findMostSpecificType@.
findMostGeneralType :: [MType] -> Maybe MType
findMostGeneralType [] = Nothing
findMostGeneralType xs = Just (minimum xs)
