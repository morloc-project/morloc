{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Python3
Description : Python3 translator
Copyright   : (c) Zebulun Arendsee, 2020
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
import Morloc.CodeGenerator.Serial (isSerializable, prettySerialOne, serialAstToType, shallowType)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified System.FilePath as SF
import qualified Data.Char as DC

-- tree rewrites
preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad MDoc
translate srcs es = do
  -- setup library paths
  lib <- fmap pretty $ MM.asks MC.configLibrary

  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyExprM es)

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold es

  -- make code for dispatching to manifolds
  let dispatch = makeDispatch es

  return $ makePool lib includeDocs mDocs dispatch

-- create an internal variable based on a unique id
letNamer :: Int -> MDoc
letNamer i = "a" <> viaShow i

-- create namer for manifold positional arguments
bndNamer :: Int -> MDoc
bndNamer i = "x" <> viaShow i

-- create a name for a manifold based on a unique id
manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

-- FIXME: should definitely use namespaces here, not `import *`
translateSource :: Path -> MorlocMonad MDoc
translateSource (Path s) = do
  (Path lib) <- MM.asks configLibrary
  let mod = pretty
          . MT.liftToText (map DC.toLower)
          . MT.replace "/" "."
          . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
          . MT.stripPrefixIfPresent "./" -- no path if relative to here
          . MT.stripPrefixIfPresent lib  -- make the path relative to the library
          . MT.liftToText SF.dropExtensions
          $ s
  return $ "from" <+> mod <+> "import *"

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|#{v}[#{pretty i}]|]

selectAccessor :: NamType -> MT.Text -> MorlocMonad (MDoc -> MDoc -> MDoc)
selectAccessor NamTable  "dict" = return recordAccess
selectAccessor NamRecord _      = return recordAccess
selectAccessor NamTable  _      = return objectAccess
selectAccessor NamObject _      = return objectAccess

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "[" <> dquotes field <> "]"

objectAccess :: MDoc -> MDoc -> MDoc
objectAccess object field = object <> "." <> field

serialize :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
serialize v0 s0 = do
  (ms, v1) <- serialize' v0 s0
  t <- serialAstToType s0
  schema <- typeSchema t
  let v2 = "mlc_serialize" <> tupled [v1, schema]
  return (v2, ms)
  where
    serialize' :: MDoc -> SerialAST One -> MorlocMonad ([MDoc], MDoc)
    serialize' v s
      | isSerializable s = return ([], v)
      | otherwise = construct v s

    construct :: MDoc -> SerialAST One -> MorlocMonad ([MDoc], MDoc)
    construct v (SerialPack _ (One (p, s))) = do
      unpacker <- case typePackerReverse p of
        [] -> MM.throwError . SerializationError $ "No unpacker found"
        (src:_) -> return . pretty . srcName $ src
      serialize' [idoc|#{unpacker}(#{v})|] s

    construct v lst@(SerialList s) = do
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
      (before, x) <- serialize' [idoc|i#{idx}|] s
      let push = [idoc|#{v'}.append(#{x})|]
          lst  = vsep [ [idoc|#{v'} = []|]
                      , nest 4 (vsep ([idoc|for i#{idx} in #{v}:|] : before ++ [push]))
                      ]
      return ([lst], v')

    construct v tup@(SerialTuple ss) = do
      (befores, ss') <- fmap unzip $ zipWithM (\i s -> construct (tupleKey i v) s) [0..] ss
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
          x = [idoc|#{v'} = #{tupled ss'}|]
      return (concat befores ++ [x], v');

    construct v rec@(SerialObject namType (PV _ _ constructor) rs) = do
      accessField <- selectAccessor namType constructor
      (befores, ss') <- fmap unzip $ mapM (\(PV _ _ k,s) -> serialize' (accessField v (pretty k)) s) rs
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
          entries = zipWith (\(PV _ _ k) v -> pretty k <> "=" <> v) (map fst rs) ss'
          decl = [idoc|#{v'} = dict#{tupled (entries)};|]
      return (concat befores ++ [decl], v');

    construct _ s = MM.throwError . SerializationError . render
      $ "construct: " <> prettySerialOne s

deserialize :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
deserialize v0 s0
  | isSerializable s0 = do
      t <- serialAstToType s0
      schema <- typeSchema t
      let deserializing = [idoc|mlc_deserialize(#{v0}, #{schema});|]
      return (deserializing, [])
  | otherwise = do
      idx <- fmap pretty $ MM.getCounter
      t <- serialAstToType s0
      schema <- typeSchema t
      let rawvar = "s" <> idx
          deserializing = [idoc|#{rawvar} = mlc_deserialize(#{v0}, #{schema});|]
      (x, befores) <- check rawvar s0
      return (x, deserializing:befores)
  where
    check :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    check v s
      | isSerializable s = return (v, [])
      | otherwise = construct v s

    construct :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
    construct v (SerialPack _ (One (p, s'))) = do
      packer <- case typePackerForward p of
        [] -> MM.throwError . SerializationError $ "No packer found"
        (x:_) -> return . pretty . srcName $ x
      (x, before) <- check v s'
      let deserialized = [idoc|#{packer}(#{x})|]
      return (deserialized, before)

    construct v lst@(SerialList s) = do
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
      (x, before) <- check [idoc|i#{idx}|] s
      let push = [idoc|#{v'}.append(#{x});|]
          lst = vsep [ [idoc|#{v'} = [];|]
                     , nest 4 (vsep ([idoc|for i#{idx} in #{v}:|] : before ++ [push]))
                     ]
      return (v', [lst])

    construct v tup@(SerialTuple ss) = do
      (ss', befores) <- fmap unzip $ zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
      idx <- fmap pretty $ MM.getCounter
      let v' = "s" <> idx
          x = [idoc|#{v'} = #{tupled ss'};|]
      return (v', concat befores ++ [x]);

    construct v rec@(SerialObject namType (PV _ _ constructor) rs) = do
      idx <- fmap pretty $ MM.getCounter
      accessField <- selectAccessor namType constructor
      (ss', befores) <- fmap unzip $ mapM (\(PV _ _ k,s) -> check (accessField v (pretty k)) s) rs
      let v' = "s" <> idx
          entries = zipWith (\(PV _ _ k) v -> pretty k <> "=" <> v) (map fst rs) ss'
          decl = [idoc|#{v'} = #{pretty constructor}#{tupled entries};|]
      return (v', concat befores ++ [decl]);

    construct _ s = MM.throwError . SerializationError . render
      $ "deserializeDescend: " <> prettySerialOne s



-- break a call tree into manifolds
translateManifold :: ExprM One -> MorlocMonad MDoc
translateManifold m@(ManifoldM _ args _) = do
  MM.startCounter
  (vsep . punctuate line . (\(x,_,_)->x)) <$> f args m
  where

  f :: [Argument]
    -> ExprM One
    -> MorlocMonad
        ( [MDoc] -- completely generated manifolds
        , MDoc   -- a tag for the returned expression
        , [MDoc] -- lines to precede the returned expression
        )
  f pargs m@(ManifoldM (metaId->i) args e) = do
    (ms', body, rs) <- f args e
    let mname = manNamer i
        head = "def" <+> mname <> tupled (map makeArgument args) <> ":"
        mdoc = nest 4 (vsep $ head:rs ++ [body])
    call <- return $ case (splitArgs args pargs, nargsTypeM (typeOfExprM m)) of
      ((rs, []), _) -> mname <> tupled (map makeArgument rs) -- covers #1, #2 and #4
      (([], vs), _) -> mname
      ((rs, vs), _) -> makeLambda vs (mname <> tupled (map makeArgument (rs ++ vs))) -- covers #5
    return (mdoc : ms', call, [])

  f _ (PoolCallM t _ cmds args) = do
    let call = "_morloc_foreign_call(" <> list(map dquotes cmds ++ map makeArgument args) <> ")"
    return ([], call, [])

  f args (ForeignInterfaceM _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"

  f args (LetM i e1 e2) = do
    (ms1', e1', rs1) <- (f args) e1
    (ms2', e2', rs2) <- (f args) e2
    let rs = rs1 ++ [ letNamer i <+> "=" <+> e1' ] ++ rs2
    return (ms1' ++ ms2', e2', rs)

  f args (AppM (SrcM _ src) xs) = do
    (mss', xs', rss') <- mapM (f args) xs |>> unzip3
    return (concat mss', pretty (srcName src) <> tupled xs', concat rss')

  f _ (SrcM t src) = return ([], pretty (srcName src), [])

  f args (LamM lambdaArgs e) = undefined -- FIXME: this is defined in R

  f _ (BndVarM _ i) = return ([], bndNamer i, [])

  f _ (LetVarM _ i) = return ([], letNamer i, [])

  f args (ListM t es) = do
    (mss', es', rss') <- mapM (f args) es |>> unzip3
    return (concat mss', list es', concat rss')

  f args (TupleM _ es) = do
    (mss', es', rss') <- mapM (f args) es |>> unzip3
    return (concat mss', tupled es', concat rss')

  f args (RecordM c entries) = do
    (mss', es', rss') <- mapM (f args . snd) entries |>> unzip3
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return (concat mss', "OrderedDict" <> tupled entries', concat rss')

  f _ (LogM _ x) = return ([], if x then "True" else "False", [])

  f _ (NumM _ x) = return ([], viaShow x, [])

  f _ (StrM _ x) = return ([], dquotes $ pretty x, [])

  f _ (NullM _) = return ([], "None", [])

  f args (SerializeM s e) = do
    (ms, e', rs1) <- f args e
    (serialized, rs2) <- serialize e' s
    return (ms, serialized, rs1 ++ rs2)

  f args (DeserializeM s e) = do
    (ms, e', rs1) <- f args e
    (deserialized, rs2) <- deserialize e' s
    return (ms, deserialized, rs1 ++ rs2)

  f args (ReturnM e) = do
    (ms, e', rs) <- f args e
    return (ms, "return(" <> e' <> ")", rs)


-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map split args1 where
  split :: Argument -> Either Argument Argument
  split r = if elem r args2
            then Left r
            else Right r


makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda args body = "lambda" <+> hsep (punctuate "," (map makeArgument args)) <> ":" <+> body

makeArgument :: Argument -> MDoc
makeArgument (SerialArgument v _) = bndNamer v
makeArgument (NativeArgument v _) = bndNamer v
makeArgument (PassThroughArgument v) = bndNamer v

makeDispatch :: [ExprM One] -> MDoc
makeDispatch ms = align . vsep $
  [ align . vsep $ ["dispatch = {", indent 4 (vsep $ map entry ms), "}"]
  , "f = dispatch[cmdID]"
  ]
  where
    entry :: ExprM One -> MDoc
    entry (ManifoldM (metaId->i) _ _)
      = pretty i <> ":" <+> manNamer i <> ","
    entry _ = error "Expected ManifoldM"

typeSchema :: TypeP -> MorlocMonad MDoc
typeSchema t = f <$> type2jsontype t
  where
    f :: JsonType -> MDoc
    f (VarJ v) = lst [var v, "None"]
    f (ArrJ v ts) = lst [var v, lst (map f ts)]
    f (NamJ v es) = lst [dquotes (pretty v), dict (map entry es)]

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

    result = f(*sys.argv[2:])

    print(result)
|]
