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
      e@(PolyManifold _ m (ManifoldFull foreignArgs) _ _)
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
    -- An <IO>-forced recursion nested in this cross-pool callee does not inline
    -- (Serialize collapses only a transparent, unforced manifold), so it would
    -- emit as a NATIVE manifold reusing THIS wrapper's id 'm' -- the native form
    -- then overwrites the serial-boundary wrapper in the per-id manifold dedup,
    -- and 'local_dispatch' feeds serial packets to native params. Give any nested
    -- recursion-bearing shadow of 'm' a fresh id so the serial wrapper keeps the
    -- dispatch id. The wrapper (the top manifold at 'm') is left in place.
    e'' <- case e' of
      MonoManifold i mform mkind mbody | i == m ->
        MonoManifold i mform mkind <$> reindexRecShadows m mbody
      _ -> reindexRecShadows m e'
    headForm <- case remoteCall of
      ForeignCall -> return HeadManifoldFormLocalForeign
      (RemoteCall _) -> return HeadManifoldFormRemoteWorker
    let foreignHead = MonoHead lang m foreignArgs headForm e''
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
segmentExpr _ _ (PolyManifold lang m form kind e) = do
  (ms, (_, e')) <- segmentExpr m (abilist const const form) e
  return (ms, (Just lang, MonoManifold m form kind e'))
segmentExpr m args (PolyApp e es) = do
  (ms, (lang, e')) <- segmentExpr m args e
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (ms ++ concat mss, (lang, MonoApp e' (map snd es')))
segmentExpr m args (PolyCacheBody lbl midx cargs e) = do
  (ms, (lang, e')) <- segmentExpr m args e
  return (ms, (lang, MonoCacheBody lbl midx cargs e'))
segmentExpr m args (PolyDebugWrap midx cargs e) = do
  (ms, (lang, e')) <- segmentExpr m args e
  return (ms, (lang, MonoDebugWrap midx cargs e'))
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
segmentExpr m args (PolyDoBlock t e) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoDoBlock t e'))
segmentExpr m args (PolyIf cond thenE elseE) = do
  (ms1, (_, cond')) <- segmentExpr m args cond
  (ms2, (_, thenE')) <- segmentExpr m args thenE
  (ms3, (_, elseE')) <- segmentExpr m args elseE
  return (ms1 ++ ms2 ++ ms3, (Nothing, MonoIf cond' thenE' elseE'))
segmentExpr m args (PolyLoop t ids e) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoLoop t ids e'))
segmentExpr m args (PolyLoopContinue es) = do
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (concat mss, (Nothing, MonoLoopContinue (map snd es')))
segmentExpr m args (PolyEval t e) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoEval t e'))
segmentExpr m args (PolyCoerce c t e) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoCoerce c t e'))
segmentExpr m args (PolyIntrinsic t intr es) = do
  results <- mapM (segmentExpr m args) es
  let (mss, pairs) = unzip results
  return (concat mss, (Nothing, MonoIntrinsic t intr (map snd pairs)))

-- | Whether a 'MonoExpr' subtree contains a recursive back-edge ('RecCallP').
-- Used to gate 'reindexRecShadows' so only recursion-bearing manifolds are
-- reindexed; a non-recursive cross-pool crossing (e.g. a closure return) that
-- legitimately shares the wrapper id and collapses is left untouched.
monoHasRec :: MonoExpr -> Bool
monoHasRec (MonoExe _ (RecCallP _ _)) = True
monoHasRec (MonoManifold _ _ _ e) = monoHasRec e
monoHasRec (MonoLet _ a b) = monoHasRec a || monoHasRec b
monoHasRec (MonoReturn e) = monoHasRec e
monoHasRec (MonoApp f xs) = monoHasRec f || any monoHasRec xs
monoHasRec (MonoCacheBody _ _ _ e) = monoHasRec e
monoHasRec (MonoDebugWrap _ _ e) = monoHasRec e
monoHasRec (MonoRecord _ _ _ es) = any (monoHasRec . snd . snd) es
monoHasRec (MonoList _ _ es) = any monoHasRec es
monoHasRec (MonoTuple _ es) = any (monoHasRec . snd) es
monoHasRec (MonoDoBlock _ e) = monoHasRec e
monoHasRec (MonoEval _ e) = monoHasRec e
monoHasRec (MonoCoerce _ _ e) = monoHasRec e
monoHasRec (MonoIf c t e) = monoHasRec c || monoHasRec t || monoHasRec e
monoHasRec (MonoLoop _ _ e) = monoHasRec e
monoHasRec (MonoLoopContinue es) = any monoHasRec es
monoHasRec (MonoIntrinsic _ _ es) = any monoHasRec es
monoHasRec _ = False

-- | Reindex any nested 'MonoManifold' that reuses the id @s@ AND whose subtree
-- carries a recursive back-edge, giving it a fresh id. This separates an
-- '<IO>'-forced recursion body (which does not inline into the cross-pool
-- serial-boundary wrapper) from the wrapper that owns the dispatch id, so the
-- native body no longer overwrites the serial wrapper in the per-id manifold
-- dedup. Applied to the wrapper's BODY, so the wrapper (the top manifold at @s@)
-- keeps its id.
reindexRecShadows :: Int -> MonoExpr -> MorlocMonad MonoExpr
reindexRecShadows s = go
  where
    go (MonoManifold i form k e)
      | i == s && monoHasRec e = do
          i' <- MM.getCounter
          MonoManifold i' form k <$> go e
      | otherwise = MonoManifold i form k <$> go e
    go (MonoLet i a b) = MonoLet i <$> go a <*> go b
    go (MonoReturn e) = MonoReturn <$> go e
    go (MonoApp f xs) = MonoApp <$> go f <*> mapM go xs
    go (MonoCacheBody l i as e) = MonoCacheBody l i as <$> go e
    go (MonoDebugWrap i as e) = MonoDebugWrap i as <$> go e
    go (MonoRecord n v ts es) =
      MonoRecord n v ts <$> mapM (\(key, (t, e)) -> (\e' -> (key, (t, e'))) <$> go e) es
    go (MonoList v ts es) = MonoList v ts <$> mapM go es
    go (MonoTuple v es) = MonoTuple v <$> mapM (\(t, e) -> (,) t <$> go e) es
    go (MonoDoBlock t e) = MonoDoBlock t <$> go e
    go (MonoEval t e) = MonoEval t <$> go e
    go (MonoCoerce c t e) = MonoCoerce c t <$> go e
    go (MonoIf c t e) = MonoIf <$> go c <*> go t <*> go e
    go (MonoLoop t ids e) = MonoLoop t ids <$> go e
    go (MonoLoopContinue es) = MonoLoopContinue <$> mapM go es
    go (MonoIntrinsic t intr es) = MonoIntrinsic t intr <$> mapM go es
    go e = return e
