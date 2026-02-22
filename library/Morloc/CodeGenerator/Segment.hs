{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Segment
Description : Break polymorphic manifold trees at language boundaries
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Segment
  ( segment
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Config as MC
import Morloc.Data.Doc
import qualified Morloc.Monad as MM

segment :: PolyHead -> MorlocMonad [MonoHead]
segment (PolyHead lang m0 args0 e0) = do
  (heads, (_, topExpr)) <- segmentExpr m0 (map ann args0) e0

  MM.sayVVV $
    "segmentation complete"
      <> "\n  topExpr language:" <+> pretty lang
      <> "\n  topExpr: " <+> pretty topExpr
      <> "\n  heads:" <+> list (map pretty heads)

  return (MonoHead lang m0 args0 HeadManifoldFormLocalRoot topExpr : heads)

segmentExpr ::
  Int -> -- manifold index
  [Int] -> -- argument indices
  PolyExpr ->
  MorlocMonad ([MonoHead], (Maybe Lang, MonoExpr))
segmentExpr
  _
  args
  ( PolyRemoteInterface
      lang
      callingType
      cargs
      remoteCall
      e@(PolyManifold _ m (ManifoldFull foreignArgs) _)
    ) = do
    MM.sayVVV $
      "segmentExpr PolyRemoteInterface PolyManifold m"
        <> pretty m
        <> "\n  forced ManifoldFull" <+> pretty foreignArgs
        <> "\n  lang" <+> pretty lang
        <> "\n  args" <+> pretty args
        <> "\n  cargs" <+> pretty cargs
        <> "\n  foreignArgs" <+> pretty (map ann foreignArgs)
    (ms, (_, e')) <- segmentExpr m (map ann foreignArgs) e
    headForm <- case remoteCall of
      ForeignCall -> return HeadManifoldFormLocalForeign
      (RemoteCall _) -> return HeadManifoldFormRemoteWorker
    let foreignHead = MonoHead lang m foreignArgs headForm e'
    config <- MM.ask
    reg <- MM.gets stateLangRegistry
    let socket = MC.setupServerAndSocket config reg lang
    return (foreignHead : ms, (Nothing, MonoPoolCall callingType m socket remoteCall foreignArgs))
segmentExpr m _ (PolyRemoteInterface lang callingType args remoteCall e) = do
  MM.sayVVV $
    "segmentExpr PolyRemoteInterface m"
      <> pretty m
      <> "\n  args" <+> pretty args
      <> "\n  lang" <+> pretty lang
  (ms, (_, e')) <- segmentExpr m args e
  headForm <- case remoteCall of
    ForeignCall -> return HeadManifoldFormLocalForeign
    (RemoteCall _) -> return HeadManifoldFormRemoteWorker
  let foreignHead = MonoHead lang m [Arg i None | i <- args] headForm (MonoReturn e')
      es' = map (MonoBndVar (A None)) args

  config <- MM.ask
  reg <- MM.gets stateLangRegistry
  let socket = MC.setupServerAndSocket config reg lang
      localFun = MonoApp (MonoPoolCall callingType m socket remoteCall [Arg i None | i <- args]) es'

  return (foreignHead : ms, (Nothing, localFun))
segmentExpr _ _ (PolyManifold lang m form e) = do
  (ms, (_, e')) <- segmentExpr m (abilist const const form) e
  return (ms, (Just lang, MonoManifold m form e'))
segmentExpr m args (PolyApp e es) = do
  (ms, (lang, e')) <- segmentExpr m args e
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (ms ++ concat mss, (lang, MonoApp e' (map snd es')))
segmentExpr m args (PolyLet i e1 e2) = do
  MM.sayVVV "segmentExpr PolyLet"
  (ms1, (_, e1')) <- segmentExpr m args e1
  (ms2, (lang2, e2')) <- segmentExpr m args e2
  return (ms1 ++ ms2, (lang2, MonoLet i e1' e2'))
segmentExpr m args (PolyList v t es) = do
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (concat mss, (Nothing, MonoList v t (map snd es')))
segmentExpr m args (PolyTuple v es) = do
  (mss, es') <- mapM (segmentExpr m args . snd) es |>> unzip
  return (concat mss, (Nothing, MonoTuple v (zip (map fst es) (map snd es'))))
segmentExpr m args (PolyRecord o v ps entries) = do
  let entryTypes = map (fst . snd) entries
  (mss, es') <- mapM (segmentExpr m args . snd . snd) entries |>> unzip
  let keys = map fst entries
  return (concat mss, (Nothing, MonoRecord o v ps (zip keys (zip entryTypes (map snd es')))))
segmentExpr m args (PolyReturn e) = do
  (ms, (lang, e')) <- segmentExpr m args e
  return (ms, (lang, MonoReturn e'))
segmentExpr _ _ (PolyLetVar t x) = return ([], (Nothing, MonoLetVar t x))
segmentExpr _ _ (PolyBndVar (A lang) i) = return ([], (Just lang, MonoBndVar (A None) i))
segmentExpr _ _ (PolyBndVar (B t) i) = return ([], (Nothing, MonoBndVar (B t) i))
segmentExpr _ _ (PolyBndVar (C t) i) = return ([], (Nothing, MonoBndVar (C t) i))
segmentExpr _ _ (PolyExe t exe) = return ([], (Nothing, MonoExe t exe))
segmentExpr _ _ (PolyLog v x) = return ([], (Nothing, MonoLog v x))
segmentExpr _ _ (PolyReal v x) = return ([], (Nothing, MonoReal v x))
segmentExpr _ _ (PolyInt v x) = return ([], (Nothing, MonoInt v x))
segmentExpr _ _ (PolyStr v x) = return ([], (Nothing, MonoStr v x))
segmentExpr _ _ (PolyNull v) = return ([], (Nothing, MonoNull v))
segmentExpr m args (PolySuspend t e) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoSuspend t e'))
segmentExpr m args (PolyForce t e) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoForce t e'))
