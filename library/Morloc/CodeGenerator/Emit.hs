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
  , checkManifoldIds
  , emit
  , TranslateFn
  ) where

import Morloc.CodeGenerator.Grammars.Common (invertSerialManifold)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Map as Map
import qualified Morloc.LangRegistry as LR
import Morloc.Data.Doc (pretty, (<+>))
import Morloc.Monad (runIdentity)
import qualified Morloc.Monad as MM

{- | Callback type for language-specific translation.
The executable provides concrete implementations for each language.
-}
type TranslateFn = Lang -> [Source] -> [SerialManifold] -> MorlocMonad Script

-- | Sort manifolds into pools. Within pools, group manifolds into call sets.
-- Manifolds are grouped by their POOL language ('poolOf'), so a guest member
-- (e.g. futhark) folds into its host's pool (cpp) rather than forming its own.
-- For non-member languages 'poolOf' is identity, so this is unchanged.
pool :: LR.LangRegistry -> [SerialManifold] -> [(Lang, [SerialManifold])]
pool reg es =
  let (langs, indexedSegments) = unzip . groupSort . map (\x@(SerialManifold i lang _ _ _) -> (LR.poolOf reg lang, (i, x))) $ es
      uniqueSegments = map (Map.elems . Map.fromList) indexedSegments
   in zip langs uniqueSegments

-- | A pool defines each manifold, nested ones included, under its id, and
-- 'pool' keeps one segment per id, so two different manifolds with one id in
-- one pool would leave a caller running the other.
checkManifoldIds :: LR.LangRegistry -> [SerialManifold] -> MorlocMonad ()
checkManifoldIds reg ms =
  case [i | ((_, i), ds) <- Map.toList byId, length (nubOrd ds) > 1] of
    [] -> return ()
    (i : _) -> MM.throwCompilerBug $ "two different manifolds in one pool share the id" <+> pretty i
  where
    byId = Map.fromListWith (<>) [((LR.poolOf reg lang, i), [d]) | (i, lang, d) <- concatMap defs ms]
    defs = runIdentity . foldWithSerialManifoldM ops
    ops =
      defaultValue
        { opFoldWithSerialManifoldM = \m full@(SerialManifold_ i lang _ _ _) ->
            return ((i, lang, show m) : foldlSM mappend mempty full)
        , opFoldWithNativeManifoldM = \m full@(NativeManifold_ i lang _ _) ->
            return ((i, lang, show m) : foldlNM mappend mempty full)
        }

-- | Translate a pool of serialized manifolds to target language source code
emit ::
  TranslateFn ->
  Lang ->
  [SerialManifold] ->
  MorlocMonad Script
emit translateFn lang xs = do
  srcs' <- findSources xs
  let xs' = map invertSerialManifold xs
  translateFn lang srcs' xs'

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

    nativeExprSrcs (AppExeN_ _ (SrcCallP src) xss) = return (src : concat xss)
    nativeExprSrcs (ExeN_ _ (SrcCallP src)) = return [src]
    nativeExprSrcs (DeserializeN_ _ s xs) = return $ serialASTsources s <> xs
    nativeExprSrcs e = return $ foldlNE (<>) [] e

    serialExprSrcs (SerializeS_ s xs) = return $ serialASTsources s <> xs
    serialExprSrcs e = return $ foldlSE (<>) [] e

    serialASTsources :: SerialAST -> [Source]
    serialASTsources (SerialPack _ (p, s)) = [typePackerForward p, typePackerReverse p] <> serialASTsources s
    serialASTsources (SerialList _ _ s) = serialASTsources s
    serialASTsources (SerialTuple _ ss) = concatMap serialASTsources ss
    serialASTsources (SerialObject _ _ _ (map snd -> ss)) = concatMap serialASTsources ss
    serialASTsources _ = []

    nativeManifoldSrcs (NativeManifold_ m lang _ e) = (<>) e <$> lookupConstructors lang m
    nativeSerialSrcs (SerialManifold_ m lang _ _ e) = (<>) e <$> lookupConstructors lang m

    lookupConstructors :: Lang -> Int -> MorlocMonad [Source]
    lookupConstructors lang i = MM.metaSources i |>> filter ((==) lang . srcLang)
