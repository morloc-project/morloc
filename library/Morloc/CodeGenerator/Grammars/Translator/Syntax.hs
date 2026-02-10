{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Syntax
Description : Generic translator framework for code generation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides a 'LangSyntax' record that captures per-language syntax decisions and
generic fold handlers that use it. New languages only need to fill in the syntax
record; the fold logic is shared.
-}
module Morloc.CodeGenerator.Grammars.Translator.Syntax
  ( LangSyntax (..)
  , IndexM
  , genericMakeSerialArg
  , genericMakeNativeArg
  , genericMakeSerialExpr
  , genericMakeNativeExpr
  , genericMakeSerialManifold
  , genericMakeNativeManifold
  , genericTranslateSegment
  ) where

import Control.Monad.Identity (Identity)
import qualified Control.Monad.State as CMS
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import Data.Text (Text)
import Morloc.Monad (IndexState, runIndex)

data LangSyntax m = LangSyntax
  { -- Literals
    synTrue        :: MDoc
  , synFalse       :: MDoc
  , synNull        :: MDoc
  , synString      :: Text -> MDoc

    -- Data constructors
  , synList        :: FVar -> TypeF -> [MDoc] -> MDoc
  , synTuple       :: FVar -> [MDoc] -> MDoc
    -- synRecord: recType -> namType -> constructor -> typeParams -> [(key, exprDoc)] -> result
    -- recType is the full TypeF of the record (for C++ type resolution)
  , synRecord      :: TypeF -> NamType -> FVar -> [TypeF] -> [(Key, MDoc)] -> m PoolDocs

    -- Function definition & closures
  , synMakeFunction :: MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> MDoc
  , synMakeLambda   :: MDoc -> [MDoc] -> [MDoc] -> MDoc

    -- Let binding: namer -> index -> Maybe TypeF -> rhs -> body -> result
    -- Maybe TypeF is Nothing for serial lets, Just t for native lets
    -- C++ uses TypeF for type annotation; Py/R ignore it
  , synMakeLet     :: (Int -> MDoc) -> Int -> Maybe TypeF -> PoolDocs -> PoolDocs -> m PoolDocs

    -- Return statement
  , synReturn      :: MDoc -> MDoc

    -- Serialization
  , synSerialize   :: MDoc -> SerialAST -> m PoolDocs
  , synDeserialize :: TypeF -> MDoc -> SerialAST -> m (MDoc, [MDoc])

    -- Source function naming
  , synSrcName     :: Source -> MDoc
  , synTemplateArgs :: [(Text, TypeF)] -> m MDoc

    -- Pattern evaluation
  , synEvalPattern :: TypeF -> Pattern -> [MDoc] -> m MDoc

    -- Foreign/remote calls
  , synForeignCall :: MDoc -> Int -> [MDoc] -> MDoc
  , synRemoteCall  :: MDoc -> Int -> RemoteResources -> [MDoc] -> m PoolDocs
  }

-- Shared across ALL languages (no parameterization needed)
genericMakeSerialArg :: (Monad m) => SerialArg -> SerialArg_ PoolDocs PoolDocs -> m (TypeS, PoolDocs)
genericMakeSerialArg sr (SerialArgManifold_ x) = return (typeSof sr, x)
genericMakeSerialArg sr (SerialArgExpr_ x) = return (typeSof sr, x)

genericMakeNativeArg :: (Monad m) => NativeArg -> NativeArg_ PoolDocs PoolDocs -> m (TypeM, PoolDocs)
genericMakeNativeArg nr (NativeArgManifold_ x) = return (typeMof nr, x)
genericMakeNativeArg nr (NativeArgExpr_ x) = return (typeMof nr, x)

-- Shared expression handler parameterized by LangSyntax
genericMakeSerialExpr ::
  (Monad m) =>
  LangSyntax m ->
  SerialExpr ->
  SerialExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
genericMakeSerialExpr _ _ (ManS_ f) = return f
genericMakeSerialExpr syn _ (AppPoolS_ _ (PoolCall mid (Socket _ _ socketFile) ForeignCall args) _) =
  return $ defaultValue {poolExpr = synForeignCall syn socketFile mid (map argNamer args)}
genericMakeSerialExpr syn _ (AppPoolS_ _ (PoolCall mid (Socket _ _ socketFile) (RemoteCall res) args) _) =
  synRemoteCall syn socketFile mid res (map argNamer args)
genericMakeSerialExpr syn _ (ReturnS_ x) = return $ x {poolExpr = synReturn syn (poolExpr x)}
genericMakeSerialExpr syn _ (SerialLetS_ i e1 e2) = synMakeLet syn svarNamer i Nothing e1 e2
genericMakeSerialExpr syn (NativeLetS _ (typeFof -> t) _) (NativeLetS_ i e1 e2) = synMakeLet syn nvarNamer i (Just t) e1 e2
genericMakeSerialExpr syn _ (NativeLetS_ i e1 e2) = synMakeLet syn nvarNamer i Nothing e1 e2
genericMakeSerialExpr _ _ (LetVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
genericMakeSerialExpr _ _ (BndVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
genericMakeSerialExpr syn _ (SerializeS_ s e) = do
  se <- synSerialize syn (poolExpr e) s
  return $ e {poolExpr = poolExpr se, poolPriorLines = poolPriorLines e <> poolPriorLines se}

genericMakeNativeExpr ::
  (Monad m) =>
  LangSyntax m ->
  NativeExpr ->
  NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
genericMakeNativeExpr syn _ (AppExeN_ _ (SrcCallP src) qs (map snd -> es)) = do
  templateStr <- synTemplateArgs syn qs
  return $ mergePoolDocs (handleFunctionArgs templateStr) es
  where
    handleFunctionArgs templateStr =
      (<>) (synSrcName syn src <> templateStr)
        . hsep
        . map tupled
        . provideClosure src
genericMakeNativeExpr syn _ (AppExeN_ t (PatCallP p) _ xs) = do
  let es = map snd xs
  patResult <- synEvalPattern syn t p (map poolExpr es)
  return $ PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds es
    , poolExpr = patResult
    , poolPriorLines = concatMap poolPriorLines es
    , poolPriorExprs = concatMap poolPriorExprs es
    }
genericMakeNativeExpr syn _ (AppExeN_ _ (LocalCallP idx) qs (map snd -> es)) = do
  templateStr <- synTemplateArgs syn qs
  return $ mergePoolDocs ((<>) (nvarNamer idx <> templateStr) . tupled) es
genericMakeNativeExpr _ _ (ManN_ call) = return call
genericMakeNativeExpr syn _ (ReturnN_ x) =
  return $ x {poolExpr = synReturn syn (poolExpr x)}
genericMakeNativeExpr syn _ (SerialLetN_ i x1 x2) = synMakeLet syn svarNamer i Nothing x1 x2
genericMakeNativeExpr syn (NativeLetN _ (typeFof -> t) _) (NativeLetN_ i x1 x2) = synMakeLet syn nvarNamer i (Just t) x1 x2
genericMakeNativeExpr syn _ (NativeLetN_ i x1 x2) = synMakeLet syn nvarNamer i Nothing x1 x2
genericMakeNativeExpr _ _ (LetVarN_ _ i) = return $ defaultValue {poolExpr = nvarNamer i}
genericMakeNativeExpr _ _ (BndVarN_ _ i) = return $ defaultValue {poolExpr = nvarNamer i}
genericMakeNativeExpr syn _ (DeserializeN_ t s x) = do
  (deserialized, assignments) <- synDeserialize syn t (poolExpr x) s
  return $
    x
      { poolExpr = deserialized
      , poolPriorLines = poolPriorLines x <> assignments
      }
genericMakeNativeExpr syn _ (ExeN_ _ (SrcCallP src)) = return $ defaultValue {poolExpr = synSrcName syn src}
genericMakeNativeExpr _ _ (ExeN_ _ (PatCallP _)) = error "Unreachable: patterns are always used in applications"
genericMakeNativeExpr _ _ (ExeN_ _ (LocalCallP idx)) = return $ defaultValue {poolExpr = nvarNamer idx}
genericMakeNativeExpr syn _ (ListN_ v t xs) = return $ mergePoolDocs (synList syn v t) xs
genericMakeNativeExpr syn _ (TupleN_ v xs) = return $ mergePoolDocs (synTuple syn v) xs
genericMakeNativeExpr syn origExpr (RecordN_ o v ps rs) = do
  let es = map snd rs
      recType = typeFof origExpr
  rec <- synRecord syn recType o v ps (zip (map fst rs) (map poolExpr es))
  return $ rec
    { poolCompleteManifolds = concatMap poolCompleteManifolds es <> poolCompleteManifolds rec
    , poolPriorLines = concatMap poolPriorLines es <> poolPriorLines rec
    , poolPriorExprs = concatMap poolPriorExprs es <> poolPriorExprs rec
    }
genericMakeNativeExpr syn _ (LogN_ _ v) = return $ defaultValue {poolExpr = if v then synTrue syn else synFalse syn}
genericMakeNativeExpr _ _ (RealN_ _ v) = return $ defaultValue {poolExpr = viaShow v}
genericMakeNativeExpr _ _ (IntN_ _ v) = return $ defaultValue {poolExpr = viaShow v}
genericMakeNativeExpr syn _ (StrN_ _ v) = return $ defaultValue {poolExpr = synString syn v}
genericMakeNativeExpr syn _ (NullN_ _) = return $ defaultValue {poolExpr = synNull syn}

-- Shared manifold handlers for simple languages (Python, R)
genericMakeSerialManifold :: (Monad m) => LangSyntax m -> SerialManifold -> SerialManifold_ PoolDocs -> m PoolDocs
genericMakeSerialManifold syn _ (SerialManifold_ m _ form headForm x) =
  return $ translateManifold (synMakeFunction syn) (synMakeLambda syn) m form (Just headForm) x

genericMakeNativeManifold :: (Monad m) => LangSyntax m -> NativeManifold -> NativeManifold_ PoolDocs -> m PoolDocs
genericMakeNativeManifold syn _ (NativeManifold_ m _ form x) =
  return $ translateManifold (synMakeFunction syn) (synMakeLambda syn) m form Nothing x

type IndexM = CMS.StateT IndexState Identity

-- Full generic translateSegment for simple languages (Python, R)
genericTranslateSegment :: LangSyntax IndexM -> SerialManifold -> MDoc
genericTranslateSegment syn m0 =
  let e = runIndex 0 (foldWithSerialManifoldM fm m0)
   in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where
    fm =
      FoldWithManifoldM
        { opFoldWithSerialManifoldM = genericMakeSerialManifold syn
        , opFoldWithNativeManifoldM = genericMakeNativeManifold syn
        , opFoldWithSerialExprM = genericMakeSerialExpr syn
        , opFoldWithNativeExprM = genericMakeNativeExpr syn
        , opFoldWithSerialArgM = genericMakeSerialArg
        , opFoldWithNativeArgM = genericMakeNativeArg
        }
