{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Reduce
Description : Compile-time reduction of intrinsics
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Walks the SerialManifold tree after serialization and replaces compile-time
intrinsics (@version, @compiled, @lang, @schema, @typeof) with string
literals. Runtime intrinsics (@save, @load, @hash) pass through unchanged
to code generation.
-}
module Morloc.CodeGenerator.Reduce (reduce) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Version as V

reduce :: SerialManifold -> MorlocMonad SerialManifold
reduce sm = do
  timestamp <- liftIO $ do
    now <- getCurrentTime
    return . T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  let ver = T.pack V.versionStr
  reduceManifold ver timestamp sm

reduceManifold :: Text -> Text -> SerialManifold -> MorlocMonad SerialManifold
reduceManifold ver ts (SerialManifold m lang form hf se) =
  SerialManifold m lang form hf <$> reduceSerialExpr ver ts lang se

reduceSerialExpr :: Text -> Text -> Lang -> SerialExpr -> MorlocMonad SerialExpr
reduceSerialExpr ver ts _ (ManS sm) = ManS <$> reduceManifold ver ts sm
reduceSerialExpr ver ts lang (AppPoolS t pc args) =
  AppPoolS t pc <$> mapM (reduceSerialArg ver ts lang) args
reduceSerialExpr ver ts lang (AppRecS t i args) =
  AppRecS t i <$> mapM (reduceSerialExpr ver ts lang) args
reduceSerialExpr ver ts lang (AppForeignRecS t i sock args) =
  AppForeignRecS t i sock <$> mapM (reduceSerialExpr ver ts lang) args
reduceSerialExpr ver ts lang (ReturnS se) = ReturnS <$> reduceSerialExpr ver ts lang se
reduceSerialExpr ver ts lang (SerialLetS i e1 e2) =
  SerialLetS i <$> reduceSerialExpr ver ts lang e1 <*> reduceSerialExpr ver ts lang e2
reduceSerialExpr ver ts lang (NativeLetS i ne se) =
  NativeLetS i <$> reduceNativeExpr ver ts lang ne <*> reduceSerialExpr ver ts lang se
reduceSerialExpr ver ts lang (SerializeS ast ne) =
  SerializeS ast <$> reduceNativeExpr ver ts lang ne
reduceSerialExpr _ _ _ e = return e

reduceSerialArg :: Text -> Text -> Lang -> SerialArg -> MorlocMonad SerialArg
reduceSerialArg ver ts _ (SerialArgManifold sm) = SerialArgManifold <$> reduceManifold ver ts sm
reduceSerialArg ver ts lang (SerialArgExpr se) = SerialArgExpr <$> reduceSerialExpr ver ts lang se

reduceNativeManifold :: Text -> Text -> NativeManifold -> MorlocMonad NativeManifold
reduceNativeManifold ver ts (NativeManifold m lang form ne) =
  NativeManifold m lang form <$> reduceNativeExpr ver ts lang ne

reduceNativeExpr :: Text -> Text -> Lang -> NativeExpr -> MorlocMonad NativeExpr
-- compile-time intrinsics: replace with string literals
reduceNativeExpr ver _ _ (IntrinsicN t IntrVersion _ []) = return $ makeStr t ver
reduceNativeExpr _ ts _ (IntrinsicN t IntrCompiled _ []) = return $ makeStr t ts
reduceNativeExpr _ _ lang (IntrinsicN t IntrLang _ []) = return $ makeStr t (langName lang)
-- runtime intrinsics: recurse into children but keep the intrinsic node
reduceNativeExpr ver ts lang (IntrinsicN t intr msch es) =
  IntrinsicN t intr msch <$> mapM (reduceNativeExpr ver ts lang) es
-- recursive cases
reduceNativeExpr ver ts _ (ManN nm) = ManN <$> reduceNativeManifold ver ts nm
reduceNativeExpr ver ts lang (AppExeN t exe tps args) =
  AppExeN t exe tps <$> mapM (reduceNativeArg ver ts lang) args
reduceNativeExpr ver ts lang (ReturnN ne) = ReturnN <$> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (SerialLetN i se ne) =
  SerialLetN i <$> reduceSerialExpr ver ts lang se <*> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (NativeLetN i ne1 ne2) =
  NativeLetN i <$> reduceNativeExpr ver ts lang ne1 <*> reduceNativeExpr ver ts lang ne2
reduceNativeExpr ver ts lang (DeserializeN t ast se) =
  DeserializeN t ast <$> reduceSerialExpr ver ts lang se
reduceNativeExpr ver ts lang (ListN fv t es) =
  ListN fv t <$> mapM (reduceNativeExpr ver ts lang) es
reduceNativeExpr ver ts lang (TupleN fv es) =
  TupleN fv <$> mapM (reduceNativeExpr ver ts lang) es
reduceNativeExpr ver ts lang (RecordN o fv tps rs) =
  RecordN o fv tps <$> mapM (\(k, ne) -> (,) k <$> reduceNativeExpr ver ts lang ne) rs
reduceNativeExpr ver ts lang (DoBlockN t ne) = DoBlockN t <$> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (EvalN t ne) = EvalN t <$> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (CoerceN c t ne) = CoerceN c t <$> reduceNativeExpr ver ts lang ne
reduceNativeExpr ver ts lang (IfN t c th el) =
  IfN t <$> reduceNativeExpr ver ts lang c <*> reduceNativeExpr ver ts lang th <*> reduceNativeExpr ver ts lang el
-- leaf nodes
reduceNativeExpr _ _ _ e = return e

reduceNativeArg :: Text -> Text -> Lang -> NativeArg -> MorlocMonad NativeArg
reduceNativeArg ver ts _ (NativeArgManifold nm) = NativeArgManifold <$> reduceNativeManifold ver ts nm
reduceNativeArg ver ts lang (NativeArgExpr ne) = NativeArgExpr <$> reduceNativeExpr ver ts lang ne

makeStr :: TypeF -> Text -> NativeExpr
makeStr (VarF fv) x = StrN fv x
makeStr _ x = StrN (FV (TV "Str") (CV "str")) x
