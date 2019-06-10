{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Manifold
Description : Functions for dealing with Manifolds
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Manifold
  ( 
      getManSrcs
    , filterByManifoldClass
    , getUnpackers
    , getPacker
    , getUsedManifolds
  ) where

import Morloc.Global
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Map.Strict as Map
import qualified Morloc.System as MS
import qualified Morloc.TypeHandler as MTH

-- | Get the paths to the sources 
getManSrcs :: Lang -> (MT.Text -> MorlocMonad Doc) -> [Manifold] -> MorlocMonad [Doc]
getManSrcs lang f ms = MM.mapM f . DL.nub . DM.catMaybes . map getManSrc $ ms' where
  getManSrc :: Manifold -> Maybe MT.Text
  getManSrc m = case (mSourcePath m, mModulePath m) of
    (Just srcpath, Just modpath) ->
      case (MT.pack . MS.takeDirectory . MT.unpack $ modpath) of
        "."  -> Just srcpath
        path -> Just $ path <> "/" <> srcpath
    _ -> Nothing

  -- select the manifolds that are in the same language as the grammar 
  ms' = filter (\m -> maybe False ((==) lang) (mLang m)) ms

determineManifoldClass :: Lang -> Manifold -> MorlocMonad ManifoldClass
determineManifoldClass lang m
  | mLang m == Just lang
      && not (mCalled m)
      && mSourced m
      && mExported m = return Source
  | not (mCalled m) && mExported m = return Uncalled
  | mLang m == Just lang = return Cis 
  | (mLang m /= Just lang) && mCalled m = return Trans
  | otherwise = MM.throwError . GeneratorError $ "Unexpected manifold class"

filterByManifoldClass :: Lang -> ManifoldClass -> [Manifold] -> MorlocMonad [Manifold]
filterByManifoldClass lang mc ms = do
  mcs <- mapM (determineManifoldClass lang) ms 
  return . map snd . filter ((==) mc . fst) $ zip mcs ms

-- find a packer for each argument passed to a manifold
getUnpackers :: SerialMap -> Manifold -> MorlocMonad [Doc]
getUnpackers hash m = case mConcreteType m of
  (Just (MFuncType _ ts _)) -> mapM (getUnpacker hash) ts
  (Just _) -> MM.throwError . TypeError $ "Unpackers must be functions"
  Nothing -> case mAbstractType m of
    (Just (MFuncType _ ts _)) -> mapM (getUnpacker hash) ts
    (Just _) -> MM.throwError . TypeError $ "Unpackers must be functions"
    Nothing -> MM.throwError . TypeError $
      "Expected a function for the following manifold: " <> MT.pretty m
  where
    getUnpacker :: SerialMap -> MType -> MorlocMonad Doc
    getUnpacker smap t =
      case (MTH.findMostSpecificType
             . Map.keys
             . Map.filterWithKey (\p _ -> MTH.childOf t p)
             $ (serialUnpacker smap)
           ) >>= (flip Map.lookup) (serialUnpacker smap)
      of
        (Just x) -> return (text' x)
        Nothing -> MM.throwError . GeneratorError $
          (MT.unlines [ "No unpacker found - this is either a bug in the " <>
                        "morloc codebase or incomplete serialization handling " <>
                        "for the given language."
                      , " - SerialMap: " <> MT.show' smap
                      , " - MType: " <> MT.show' t])

-- | If a language-specific signature is given for the manifold, choose a
-- packer that matches the language-specific output type. Otherwise, search for
-- a packer that matches the morloc type.
-- TODO: make the MorlocMonad
getPacker :: SerialMap -> Manifold -> Doc
getPacker hash m = case packerType of
  (Just t) -> case Map.lookup t (serialPacker hash) of
    (Just n) -> text' n
    Nothing -> error "You should not be reading this"
  Nothing -> error "No packer found for this type"
  where
    packerType :: Maybe MType
    packerType = case cPacker of
      (Just x) -> Just x
      Nothing -> aPacker

    cPacker :: Maybe MType
    cPacker = case mConcreteType m of
      (Just (MFuncType _ _ t)) -> MTH.findMostSpecificType (packers t)
      (Just _) -> error "Ah shit"
      Nothing -> Nothing

    aPacker :: Maybe MType
    aPacker = case mAbstractType m of
      (Just (MFuncType _ _ t)) -> MTH.findMostSpecificType (packers t)
      (Just _) -> error "Ah shit"
      Nothing -> Nothing

    packers :: MType -> [MType]
    packers o
      = Map.keys
      . Map.filterWithKey (\p _ -> MTH.childOf o p)
      $ (serialPacker hash)

-- | Who knows what this is for ... it is the hack I used to resolve a worse hack
getUsedManifolds :: Lang -> [Manifold] -> MorlocMonad [Manifold]
getUsedManifolds lang ms = MM.filterM isBuilt ms
  where
    isBuilt :: Manifold -> MorlocMonad Bool
    isBuilt m = do
      mc <- determineManifoldClass lang m
      return $ mc == Cis || mc == Source
