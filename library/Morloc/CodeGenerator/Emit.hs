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

import Morloc.Data.Zip (zipSame)
import Morloc.CodeGenerator.Grammars.Common (invertSerialManifold)
import Morloc.CodeGenerator.Serial (serialKids)
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
  let (langs, indexedSegments) = unzip . groupSort . map (\x@(SerialManifold i lang _ hf _) -> (LR.poolOf reg lang, (emittedId i hf, x))) $ es
      uniqueSegments = map (Map.elems . Map.fromList) indexedSegments
   in zipSame langs uniqueSegments

-- | A manifold's name in its pool: its id, and whether it is the job-side
-- entry of a remote call, which is emitted with a suffix beside the local
-- manifold of the same id.
emittedId :: Int -> HeadManifoldForm -> (Int, Bool)
emittedId i HeadManifoldFormRemoteWorker = (i, True)
emittedId i _ = (i, False)

-- | A pool defines each manifold, nested ones included, under its emitted
-- name, and 'pool' keeps one segment per name, so two different manifolds
-- with one name in one pool would leave a caller running the other.
checkManifoldIds :: LR.LangRegistry -> [SerialManifold] -> MorlocMonad ()
checkManifoldIds reg ms =
  case [i | ((_, (i, _)), ds) <- Map.toList byId, length (nubOrd ds) > 1] of
    [] -> return ()
    (i : _) -> MM.throwCompilerBug $ "two different manifolds in one pool share the id" <+> pretty i
  where
    byId = Map.fromListWith (<>) [((LR.poolOf reg lang, i), [d]) | (i, lang, d) <- concatMap defs ms]
    defs = runIdentity . foldWithSerialManifoldM ops
    ops =
      defaultValue
        { opFoldWithSerialManifoldM = \m full@(SerialManifold_ i lang _ hf _) ->
            return ((emittedId i hf, lang, show m) : foldlSM mappend mempty full)
        , opFoldWithNativeManifoldM = \m full@(NativeManifold_ i lang _ _) ->
            return (((i, False), lang, show m) : foldlNM mappend mempty full)
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
  checkDistinctParameters xs'
  translateFn lang srcs' xs'

-- | Each generated function's parameters are distinct variables: one
-- variable bound twice is a function no language accepts.
checkDistinctParameters :: [SerialManifold] -> MorlocMonad ()
checkDistinctParameters ms =
  case [(i, dups) | (i, ids) <- concatMap forms ms, dups@(_ : _) <- [duplicates ids]] of
    [] -> return ()
    ((i, dups) : _) ->
      MM.throwCompilerBug $
        "manifold" <+> pretty i <+> "binds the variables" <+> pretty dups <+> "as more than one parameter"
  where
    forms = runIdentity . foldWithSerialManifoldM ops
    ops =
      defaultValue
        { opFoldWithSerialManifoldM = \_ folded@(SerialManifold_ i _ form _ _) ->
            return ((i, formIds form) : foldlSM mappend mempty folded)
        , opFoldWithNativeManifoldM = \_ folded@(NativeManifold_ i _ form _) ->
            return ((i, formIds form) : foldlNM mappend mempty folded)
        }
    formIds :: ManifoldForm a b -> [Int]
    formIds form = case form of
      ManifoldPass args -> [j | Arg j _ <- args]
      ManifoldFull args -> [j | Arg j _ <- args]
      ManifoldPart ctx bnd -> [j | Arg j _ <- ctx] <> [j | Arg j _ <- bnd]

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

    nativeExprSrcs (AppExeN_ _ (SrcCallP _ src) xss) = return (src : concat xss)
    nativeExprSrcs (ExeN_ _ (SrcCallP _ src)) = return [src]
    nativeExprSrcs (DeserializeN_ _ s xs) = return $ serialASTsources s <> xs
    nativeExprSrcs (WireToNativeN_ _ s _ _ xss) = return $ serialASTsources s <> concat xss
    nativeExprSrcs (NativeToWireN_ _ s xs) = return $ serialASTsources s <> xs
    nativeExprSrcs e = return $ foldlNE (<>) [] e

    serialExprSrcs (SerializeS_ s xs) = return $ serialASTsources s <> xs
    serialExprSrcs e = return $ foldlSE (<>) [] e

    serialASTsources :: SerialAST -> [Source]
    serialASTsources s = here s <> concatMap serialASTsources (serialKids s)
      where
        here (SerialPack _ (p, _)) = [val (typePackerForward p), val (typePackerReverse p)]
        here _ = []

    nativeManifoldSrcs (NativeManifold_ m lang _ e) = (<>) e <$> lookupConstructors lang m
    nativeSerialSrcs (SerialManifold_ m lang _ _ e) = (<>) e <$> lookupConstructors lang m

    lookupConstructors :: Lang -> Int -> MorlocMonad [Source]
    lookupConstructors lang i = MM.metaSources i |>> filter ((==) lang . srcLang)
