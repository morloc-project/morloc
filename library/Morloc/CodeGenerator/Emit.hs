{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Emit
Description : Group serialized manifolds by language and translate to target source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Emit
  ( pool
  , emit
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R

-- | Sort manifolds into pools. Within pools, group manifolds into call sets.
pool :: [SerialManifold] -> [(Lang, [SerialManifold])]
pool es =
  let (langs, indexedSegments) = unzip . groupSort . map (\x@(SerialManifold i lang _ _ _) -> (lang, (i, x))) $ es
      uniqueSegments = map (Map.elems . Map.fromList) indexedSegments
   in zip langs uniqueSegments

-- | Translate a pool of serialized manifolds to target language source code
emit ::
  Lang ->
  [SerialManifold] ->
  MorlocMonad Script
emit lang xs = do
  srcs' <- findSources xs
  xs' <- mapM (preprocess lang) xs
  translate lang srcs' xs'

findSources :: [SerialManifold] -> MorlocMonad [Source]
findSources ms = unique <$> concatMapM (foldSerialManifoldM fm) ms
  where
    fm =
      defaultValue
        { opSerialExprM = serialExprSrcs
        , opNativeExprM = nativeExprSrcs
        , opNativeManifoldM = nativeManifoldSrcs
        , opSerialManifoldM = nativeSerialSrcs
        }

    nativeExprSrcs (AppExeN_ _ (SrcCallP src) _ xss) = return (src : concat xss)
    nativeExprSrcs (ExeN_ _ (SrcCallP src)) = return [src]
    nativeExprSrcs (DeserializeN_ _ s xs) = return $ serialASTsources s <> xs
    nativeExprSrcs e = return $ foldlNE (<>) [] e

    serialExprSrcs (SerializeS_ s xs) = return $ serialASTsources s <> xs
    serialExprSrcs e = return $ foldlSE (<>) [] e

    serialASTsources :: SerialAST -> [Source]
    serialASTsources (SerialPack _ (p, s)) = [typePackerForward p, typePackerReverse p] <> serialASTsources s
    serialASTsources (SerialList _ s) = serialASTsources s
    serialASTsources (SerialTuple _ ss) = concatMap serialASTsources ss
    serialASTsources (SerialObject _ _ _ (map snd -> ss)) = concatMap serialASTsources ss
    serialASTsources _ = []

    nativeManifoldSrcs (NativeManifold_ m lang _ e) = (<>) e <$> lookupConstructors lang m
    nativeSerialSrcs (SerialManifold_ m lang _ _ e) = (<>) e <$> lookupConstructors lang m

    lookupConstructors :: Lang -> Int -> MorlocMonad [Source]
    lookupConstructors lang i = MM.metaSources i |>> filter ((==) lang . srcLang)

translate :: Lang -> [Source] -> [SerialManifold] -> MorlocMonad Script
translate lang srcs es = do
  case lang of
    CppLang -> Cpp.translate srcs es
    RLang -> R.translate srcs es
    Python3Lang -> Python3.translate srcs es
    x -> MM.throwSystemError $ "Language '" <> viaShow x <> "' has no translator"

preprocess :: Lang -> SerialManifold -> MorlocMonad SerialManifold
preprocess CppLang es = Cpp.preprocess es
preprocess RLang es = R.preprocess es
preprocess Python3Lang es = Python3.preprocess es
preprocess l _ = MM.throwSystemError $ "Language '" <> viaShow l <> "' has no translator"
