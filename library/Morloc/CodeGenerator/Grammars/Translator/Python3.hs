{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ViewPatterns, TupleSections #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Python3
Description : Python3 translator
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Translator.Python3
  (
    translate
  , preprocess
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial (isSerializable, serialAstToJsonType)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
import qualified Morloc.Config as MC
import Morloc.Monad (asks, gets, Index, newIndex, runIndex)
import qualified Morloc.Data.Text as MT
import qualified System.FilePath as SF
import qualified Data.Char as DC
import qualified Morloc.Language as ML
import Morloc.CodeGenerator.Grammars.Translator.PseudoCode (pseudocodeSerialManifold)

-- tree rewrites
preprocess :: SerialManifold -> MorlocMonad SerialManifold
preprocess = return . invertSerialManifold

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  -- setup library paths
  lib <- pretty <$> asks MC.configLibrary

  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . mapMaybe srcPath $ srcs)

  -- diagnostics
  debugLog (vsep (map pseudocodeSerialManifold es) <> "\n")

  -- translate each manifold tree, rooted on a call from nexus or another pool
  let mDocs = map translateSegment es

  -- make code for dispatching to manifolds
  let dispatch = makeDispatch es

  let code = makePool lib includeDocs mDocs dispatch
  let outfile = ML.makeExecutableName Python3Lang "pool"

  return $ Script
    { scriptBase = "pool"
    , scriptLang = Python3Lang
    , scriptCode = "." :/ File "pool.py" (Code . render $ code)
    , scriptMake = [SysExe outfile]
    }

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

-- FIXME: should definitely use namespaces here, not `import *`
translateSource :: Path -> MorlocMonad MDoc
translateSource s = do
  lib <- MT.pack <$> asks configLibrary
  let moduleStr = pretty
                . MT.liftToText (map DC.toLower)
                . MT.replace "/" "."
                . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
                . MT.stripPrefixIfPresent "./" -- no path if relative to here
                . MT.stripPrefixIfPresent lib  -- make the path relative to the library
                . MT.liftToText SF.dropExtensions
                $ MT.pack s
  return $ "from" <+> moduleStr <+> "import *"

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|#{v}[#{pretty i}]|]

selectAccessor :: NamType -> MT.Text -> (MDoc -> MDoc -> MDoc)
selectAccessor NamTable  "dict" = recordAccess
selectAccessor NamRecord _      = recordAccess
selectAccessor NamTable  _      = objectAccess
selectAccessor NamObject _      = objectAccess

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "[" <> dquotes field <> "]"

objectAccess :: MDoc -> MDoc -> MDoc
objectAccess object field = object <> "." <> field

serialize :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
serialize v0 s0 = do
  (ms, v1) <- serialize' v0 s0
  let schema = typeSchema s0
      v2 = "mlc_serialize" <> tupled [v1, schema]
  return (v2, ms)
  where
    serialize' :: MDoc -> SerialAST -> Index ([MDoc], MDoc)
    serialize' v s
      | isSerializable s = return ([], v)
      | otherwise = construct v s

    construct :: MDoc -> SerialAST -> Index ([MDoc], MDoc)
    construct v (SerialPack _ (p, s)) =
      let unpacker = pretty . srcName . typePackerReverse $ p
      in serialize' [idoc|#{unpacker}(#{v})|] s

    construct v (SerialList _ s) = do
      idx <- newIndex
      let v' = helperNamer idx
          idxStr = pretty idx
      (before, x) <- serialize' [idoc|i#{idxStr}|] s
      let push = [idoc|#{v'}.append(#{x})|]
          lst  = vsep [ [idoc|#{v'} = []|]
                      , nest 4 (vsep ([idoc|for i#{idxStr} in #{v}:|] : before ++ [push]))
                      ]
      return ([lst], v')

    construct v (SerialTuple _ ss) = do
      (befores, ss') <- unzip <$> zipWithM (\i s -> construct (tupleKey i v) s) [0..] ss
      v' <- helperNamer <$> newIndex
      let x = [idoc|#{v'} = #{tupled ss'}|]
      return (concat befores ++ [x], v')

    construct v (SerialObject namType (FV _ constructor) _ rs) = do
      let accessField = selectAccessor namType constructor
      (befores, ss') <- mapAndUnzipM (\(FV _ k,s) -> serialize' (accessField v (pretty k)) s) rs
      v' <- helperNamer <$> newIndex
      let entries = zipWith (\(FV _ key) val -> pretty key <> "=" <> val)
                            (map fst rs) ss'
          decl = [idoc|#{v'} = dict#{tupled (entries)}|]
      return (concat befores ++ [decl], v')

    construct _ _ = error "Told you that branch was reachable"

deserialize :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
deserialize v0 s0
  | isSerializable s0 = do
      let schema = typeSchema s0
          deserializing = [idoc|mlc_deserialize(#{v0}, #{schema})|]
      return (deserializing, [])
  | otherwise = do
      rawvar <- helperNamer <$> newIndex
      let schema = typeSchema s0
          deserializing = [idoc|#{rawvar} = mlc_deserialize(#{v0}, #{schema})|]
      (x, befores) <- check rawvar s0
      return (x, deserializing:befores)
  where
    check :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
    check v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST -> Index (MDoc, [MDoc])
    construct v (SerialPack _ (p, s')) = do
      (x, before) <- check v s'
      let packer = pretty . srcName . typePackerForward $ p
          deserialized = [idoc|#{packer}(#{x})|]
      return (deserialized, before)

    construct v (SerialList _ s) = do
      idx <- newIndex
      let v' = helperNamer idx
          idxStr = pretty idx
      (x, before) <- check [idoc|i#{idxStr}|] s
      let push = [idoc|#{v'}.append(#{x})|]
          lst = vsep [ [idoc|#{v'} = []|]
                     , nest 4 (vsep ([idoc|for i#{idxStr} in #{v}:|] : before ++ [push]))
                     ]
      return (v', [lst])

    construct v (SerialTuple _ ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
      v' <- helperNamer <$> newIndex
      let x = [idoc|#{v'} = #{tupled ss'}|]
      return (v', concat befores ++ [x])

    construct v (SerialObject namType (FV _ constructor) _ rs) = do
      let accessField = selectAccessor namType constructor
      (ss', befores) <- mapAndUnzipM (\(FV _ k,s) -> check (accessField v (pretty k)) s) rs
      v' <- helperNamer <$> newIndex
      let entries = zipWith (\(FV _ key) val -> pretty key <> "=" <> val)
                            (map fst rs) ss'
          decl = [idoc|#{v'} = #{pretty constructor}#{tupled entries}|]
      return (v', concat befores ++ [decl])

    construct _ _ = error "Why is this OK? Well, I see that it never was."


translateSegment :: SerialManifold -> MDoc
translateSegment m0 =
  let e = runIndex 0 (foldWithSerialManifoldM fm m0)
  in vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where
    fm = FoldWithManifoldM
      { opFoldWithSerialManifoldM = makeSerialManifold
      , opFoldWithNativeManifoldM = makeNativeManifold
      , opFoldWithSerialExprM = makeSerialExpr
      , opFoldWithNativeExprM = makeNativeExpr
      , opFoldWithSerialArgM = makeSerialArg
      , opFoldWithNativeArgM = makeNativeArg
      }

    makeSerialManifold :: SerialManifold -> SerialManifold_ (TypeS, PoolDocs) (TypeM, PoolDocs) PoolDocs -> Index PoolDocs
    makeSerialManifold _ (SerialManifold_ m _ form x)
      = return $ translateManifold makeFunction makeLambda m form x

    makeNativeManifold :: NativeManifold -> NativeManifold_ (TypeS, PoolDocs) (TypeM, PoolDocs) PoolDocs -> Index PoolDocs
    makeNativeManifold _ (NativeManifold_ m _ form x)
      = return $ translateManifold makeFunction makeLambda m form x

    makeSerialExpr :: SerialExpr -> SerialExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) -> Index PoolDocs
    makeSerialExpr _ (ManS_ f) = return f
    makeSerialExpr _ (AppPoolS_ _ (PoolCall _ cmds args) _) = do
      let call = "_morloc_foreign_call(" <> list(map dquotes cmds ++ map argNamer args) <> ")"
      return $ defaultValue { poolExpr = call }
    makeSerialExpr _ (ReturnS_ x) = return $ x {poolExpr = "return(" <> poolExpr x <> ")"}
    makeSerialExpr _ (SerialLetS_ i e1 e2) = return $ makeLet svarNamer i e1 e2
    makeSerialExpr _ (NativeLetS_ i e1 e2) = return $ makeLet nvarNamer i e1 e2
    makeSerialExpr _ (LetVarS_ _ i) = return $ PoolDocs [] (svarNamer i) [] []
    makeSerialExpr _ (BndVarS_ _ i) = return $ PoolDocs [] (svarNamer i) [] []
    makeSerialExpr _ (SerializeS_ s e) = do
      (serialized, assignments) <- serialize (poolExpr e) s
      return $ e {poolExpr = serialized, poolPriorLines = poolPriorLines e <> assignments}

    makeNativeExpr :: NativeExpr -> NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) -> Index PoolDocs
    makeNativeExpr _ (AppSrcN_ _ (pretty . srcName -> functionName) xs) =
        return $ mergePoolDocs ((<>) functionName . tupled) (map snd xs)
    makeNativeExpr _ (ManN_ call) = return call
    makeNativeExpr _ (ReturnN_ x) =
        return $ x { poolExpr = "return(" <> poolExpr x <> ")" }
    makeNativeExpr _ (SerialLetN_ i x1 x2) = return $ makeLet svarNamer i x1 x2
    makeNativeExpr _ (NativeLetN_ i x1 x2) = return $ makeLet nvarNamer i x1 x2
    makeNativeExpr _ (LetVarN_ _ i) = return $ PoolDocs [] (nvarNamer i) [] []
    makeNativeExpr _ (BndVarN_ _ i) = return $ PoolDocs [] (nvarNamer i) [] []
    makeNativeExpr _ (DeserializeN_ _ s x) = do
        (deserialized, assignments) <- deserialize (poolExpr x) s
        return $ x
          { poolExpr = deserialized
          , poolPriorLines = poolPriorLines x <> assignments
          }
    makeNativeExpr _ (AccN_ r (FV _ v) x k) =
        return $ x {poolExpr = selectAccessor r v (poolExpr x) (pretty k)}
    makeNativeExpr _ (SrcN_ _ src) = return $ PoolDocs [] (pretty (srcName src)) [] []
    makeNativeExpr _ (ListN_ _ _ xs) = return $ mergePoolDocs list xs
    makeNativeExpr _ (TupleN_ _ xs) = return $ mergePoolDocs tupled xs
    makeNativeExpr _ (RecordN_ _ _ _ rs)
        = return $ mergePoolDocs pyDict (map snd rs)
        where
            pyDict es' =
                let entries' = zipWith (\(FV _ k) v -> pretty k <> "=" <> v) (map fst rs) es'
                in "OrderedDict" <> tupled entries'
    makeNativeExpr _ (LogN_ _ v) = return $ PoolDocs [] (if v then "True" else "False") [] []
    makeNativeExpr _ (RealN_ _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr _ (IntN_ _ v) = return $ PoolDocs [] (viaShow v) [] []
    makeNativeExpr _ (StrN_ _ v) = return $ PoolDocs [] (dquotes $ pretty v) [] []
    makeNativeExpr _ (NullN_ _) = return $ PoolDocs [] "None" [] []

    makeSerialArg :: SerialArg -> SerialArg_ PoolDocs PoolDocs -> Index (TypeS, PoolDocs)
    makeSerialArg sr (SerialArgManifold_ x) = return (typeSof sr, x)
    makeSerialArg sr (SerialArgExpr_ x) = return (typeSof sr, x)

    makeNativeArg :: NativeArg -> NativeArg_ PoolDocs PoolDocs -> Index (TypeM, PoolDocs)
    makeNativeArg nr (NativeArgManifold_ x) = return (typeMof nr, x)
    makeNativeArg nr (NativeArgExpr_ x) = return (typeMof nr, x)

    makeLet :: (Int -> MDoc) -> Int -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet namer i (PoolDocs ms1' e1' rs1 pes1) (PoolDocs ms2' e2' rs2 pes2) =
      let rs = rs1 ++ [ namer i <+> "=" <+> e1' ] ++ rs2
      in PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

    makeFunction :: MDoc -> [Arg TypeM] -> [MDoc] -> MDoc -> MDoc
    makeFunction mname args priorLines body =
      let def = "def" <+> mname <> tupled (map argNamer args) <> ":"
      in nest 4 (vsep $ def:priorLines <> [body])

    makeLambda :: [Arg TypeM] -> MDoc -> MDoc
    makeLambda args body = "lambda" <+> hsep (punctuate "," (map argNamer args)) <> ":" <+> body

makeDispatch :: [SerialManifold] -> MDoc
makeDispatch ms = align . vsep $
  [ align . vsep $ ["dispatch = {", indent 4 (vsep $ map entry ms), "}"]
  , "__mlc_function__ = dispatch[cmdID]"
  ]
  where
    entry :: SerialManifold -> MDoc
    entry (SerialManifold i _ _ _)
      = pretty i <> ":" <+> manNamer i <> ","

typeSchema :: SerialAST -> MDoc
typeSchema = f . serialAstToJsonType
  where
    f :: JsonType -> MDoc
    f (VarJ v) = lst [var v, "None"]
    f (ArrJ v ts) = lst [var v, lst (map f ts)]
    f (NamJ "dict" es) = lst [dquotes "dict", dict (map entry es)]
    f (NamJ "record" es) = lst [dquotes "record", dict (map entry es)]
    f (NamJ v es) = lst [pretty v, dict (map entry es)]

    entry :: (MT.Text, JsonType) -> MDoc
    entry (v, t) = pretty v <> "=" <> f t

    dict :: [MDoc] -> MDoc
    dict xs = "OrderedDict" <> lst xs

    lst :: [MDoc] -> MDoc
    lst xs = encloseSep "(" ")" "," xs

    var :: MT.Text -> MDoc
    var v = dquotes (pretty v)

makePool :: MDoc -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makePool lib includeDocs manifolds dispatch = [idoc|#!/usr/bin/env python

import sys
import subprocess
import json
from pymorlocinternals import (mlc_serialize, mlc_deserialize)
from collections import OrderedDict

sys.path = ["#{lib}"] + sys.path

#{vsep includeDocs}

def _morloc_foreign_call(args):
    try:
        sysObj = subprocess.run(
            args,
            stdout=subprocess.PIPE,
            check=True
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))

    return(sysObj.stdout.decode("ascii"))

#{vsep manifolds}

if __name__ == '__main__':
    try:
        cmdID = int(sys.argv[1])
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(sys.argv[0]))
    except ValueError:
        sys.exit("Internal error in {}: expected integer manifold id".format(sys.argv[0]))
    try:
        #{dispatch}
    except KeyError:
        sys.exit("Internal error in {}: no manifold found with id={}".format(sys.argv[0], cmdID))

    __mlc_result__ = __mlc_function__(*sys.argv[2:])

    if __mlc_result__ != "null":
        print(__mlc_result__)
|]
