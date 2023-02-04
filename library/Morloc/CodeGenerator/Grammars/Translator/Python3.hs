{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

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
import Morloc.CodeGenerator.Serial (isSerializable, prettySerialOne, serialAstToType)
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified System.FilePath as SF
import qualified Data.Char as DC
import qualified Morloc.Language as ML

-- tree rewrites
preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad Script
translate srcs es = do
  -- setup library paths
  lib <- pretty <$> MM.asks MC.configLibrary

  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . mapMaybe srcPath $ srcs)

  -- diagnostics
  debugLog (vsep (map pretty es))

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold es

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
  verbosity <- MM.gets stateVerbosity
  when (verbosity > 0) $ (liftIO . putDoc) d

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
translateSource s = do
  lib <- MT.pack <$> MM.asks configLibrary
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

    construct v (SerialList s) = do
      idx <- fmap pretty MM.getCounter
      let v' = "s" <> idx
      (before, x) <- serialize' [idoc|i#{idx}|] s
      let push = [idoc|#{v'}.append(#{x})|]
          lst  = vsep [ [idoc|#{v'} = []|]
                      , nest 4 (vsep ([idoc|for i#{idx} in #{v}:|] : before ++ [push]))
                      ]
      return ([lst], v')

    construct v (SerialTuple ss) = do
      (befores, ss') <- unzip <$> zipWithM (\i s -> construct (tupleKey i v) s) [0..] ss
      idx <- fmap pretty MM.getCounter
      let v' = "s" <> idx
          x = [idoc|#{v'} = #{tupled ss'}|]
      return (concat befores ++ [x], v')

    construct v (SerialObject namType (PV _ _ constructor) _ rs) = do
      accessField <- selectAccessor namType constructor
      (befores, ss') <- mapAndUnzipM (\(PV _ _ k,s) -> serialize' (accessField v (pretty k)) s) rs
      idx <- fmap pretty MM.getCounter
      let v' = "s" <> idx
          entries = zipWith (\(PV _ _ key) val -> pretty key <> "=" <> val)
                            (map fst rs) ss'
          decl = [idoc|#{v'} = dict#{tupled (entries)}|]
      return (concat befores ++ [decl], v')

    construct _ s = MM.throwError . SerializationError . render
      $ "construct: " <> prettySerialOne s

deserialize :: MDoc -> SerialAST One -> MorlocMonad (MDoc, [MDoc])
deserialize v0 s0
  | isSerializable s0 = do
      t <- serialAstToType s0
      schema <- typeSchema t
      let deserializing = [idoc|mlc_deserialize(#{v0}, #{schema})|]
      return (deserializing, [])
  | otherwise = do
      idx <- fmap pretty MM.getCounter
      t <- serialAstToType s0
      schema <- typeSchema t
      let rawvar = "s" <> idx
          deserializing = [idoc|#{rawvar} = mlc_deserialize(#{v0}, #{schema})|]
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

    construct v (SerialList s) = do
      idx <- fmap pretty MM.getCounter
      let v' = "s" <> idx
      (x, before) <- check [idoc|i#{idx}|] s
      let push = [idoc|#{v'}.append(#{x})|]
          lst = vsep [ [idoc|#{v'} = []|]
                     , nest 4 (vsep ([idoc|for i#{idx} in #{v}:|] : before ++ [push]))
                     ]
      return (v', [lst])

    construct v (SerialTuple ss) = do
      (ss', befores) <- unzip <$> zipWithM (\i s -> check (tupleKey i v) s) [0..] ss
      idx <- fmap pretty MM.getCounter
      let v' = "s" <> idx
          x = [idoc|#{v'} = #{tupled ss'}|]
      return (v', concat befores ++ [x])

    construct v (SerialObject namType (PV _ _ constructor) _ rs) = do
      idx <- fmap pretty MM.getCounter
      accessField <- selectAccessor namType constructor
      (ss', befores) <- mapAndUnzipM (\(PV _ _ k,s) -> check (accessField v (pretty k)) s) rs
      let v' = "s" <> idx
          entries = zipWith (\(PV _ _ key) val -> pretty key <> "=" <> val)
                            (map fst rs) ss'
          decl = [idoc|#{v'} = #{pretty constructor}#{tupled entries}|]
      return (v', concat befores ++ [decl])

    construct _ s = MM.throwError . SerializationError . render
      $ "deserializeDescend: " <> prettySerialOne s



-- break a call tree into manifolds
translateManifold :: ExprM One -> MorlocMonad MDoc
translateManifold m0@(ManifoldM _ form0 _) = do
  MM.startCounter
  e <- f (manifoldArgs form0) m0
  return . vsep . punctuate line $ poolPriorExprs e <> poolCompleteManifolds e
  where

  f :: [Argument]
    -> ExprM One
    -> MorlocMonad PoolDocs


  f _ (ManifoldM i form e) = do
    let args = manifoldArgs form
    (PoolDocs completeManifolds body priorLines priorExprs) <- f args e
    let mname = manNamer i
        def = "def" <+> mname <> tupled (map argName args) <> ":"
        newManifold = nest 4 (vsep $ def:priorLines <> [body])
        -- call = mname <> tupled (map argName manifoldArgs)
        call = case form of
          (ManifoldFull rs) -> mname <> tupled (map argName rs) -- covers #1, #2 and #4
          (ManifoldPass _) -> mname
          (ManifoldPart rs vs) -> makeLambda vs (mname <> tupled (map argName (rs ++ vs))) -- covers #5
    return $ PoolDocs
        { poolCompleteManifolds = newManifold : completeManifolds
        , poolExpr = call
        , poolPriorLines = []
        , poolPriorExprs = priorExprs
        }

  f _ (PoolCallM _ _ cmds args) = do
    let call = "_morloc_foreign_call(" <> list(map dquotes cmds ++ map argName args) <> ")"
    return $ PoolDocs [] call [] [] 

  f _ (ForeignInterfaceM _ _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"

  f args (LetM i e1 e2) = do
    (PoolDocs ms1' e1' rs1 pes1) <- f args e1
    (PoolDocs ms2' e2' rs2 pes2) <- f args e2
    let rs = rs1 ++ [ letNamer i <+> "=" <+> e1' ] ++ rs2
    return $ PoolDocs (ms1' <> ms2') e2' rs (pes1 <> pes2)

  f args (AppM (SrcM _ src) xs)
    = mergePoolDocs (\xs' -> pretty (srcName src) <> tupled xs')
    <$> mapM (f args) xs

  f args (AppM (PoolCallM _ _ cmds _) xs) = mapM (f args) xs |>> mergePoolDocs makePoolCall where
    makePoolCall xs' = "_morloc_foreign_call(" <> list(map dquotes cmds ++ xs') <> ")"

  f _ (AppM _ _) = error "Can only apply functions"

  f _ (SrcM _ src) = return $ PoolDocs [] (pretty (srcName src)) [] []

  f _ (LamM contextArgs boundArgs e) = do
    p <- f (contextArgs <> boundArgs) e 
    return $ p { poolExpr = makeLambda boundArgs (poolExpr p) }
  -- f args (LamM lambdaArgs body) = do
  --   p <- f args body
  --   i <- MM.getCounter
  --   let vs = map (bndNamer . argId) lambdaArgs
  --       lambdaName = "mlc_lam_" <> pretty i
  --       def = "def" <+> lambdaName <> tupled vs <> ":"
  --       lambdaDef = nest 4 (vsep $ def:poolPriorLines p <> [poolExpr p])
  --       call = lambdaName
  --   return $ p
  --       { poolExpr = call
  --       , poolPriorLines = []
  --       , poolPriorExprs = [lambdaDef]
  --       }

  f _ (BndVarM _ i) = return $ PoolDocs [] (bndNamer i) [] []

  f _ (LetVarM _ i) = return $ PoolDocs [] (letNamer i) [] []

  f args (AccM e k) = do
    p <- f args e
    x <- case typeOfTypeM (typeOfExprM e) of
      (Just (NamP r (PV _ _ v) _ _)) -> selectAccessor r v <*> pure (poolExpr p) <*> pure (pretty k)
      _ -> MM.throwError . CallTheMonkeys $ "Bad record access"
    return $ p {poolExpr = x}

  f args (ListM _ es) = mapM (f args) es |>> mergePoolDocs list

  f args (TupleM _ es) = mapM (f args) es |>> mergePoolDocs tupled

  f args (RecordM _ entries) = mapM (f args . snd) entries |>> mergePoolDocs pyDict
    where
        pyDict es' =
            let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
            in "OrderedDict" <> tupled entries'


  f _ (LogM _ x) = return $ PoolDocs [] (if x then "True" else "False") [] []

  f _ (RealM _ x) = return $ PoolDocs [] (viaShow x) [] []

  f _ (IntM _ x) = return $ PoolDocs [] (viaShow x) [] []

  f _ (StrM _ x) = return $ PoolDocs [] (dquotes $ pretty x) [] []

  f _ (NullM _) = return $ PoolDocs [] "None" [] []

  f args (SerializeM s e) = do
    p <- f args e
    (serialized, assignments) <- serialize (poolExpr p) s
    return $ p {poolExpr = serialized, poolPriorLines = poolPriorLines p <> assignments}

  f args (DeserializeM s e) = do
    p <- f args e
    (deserialized, assignments) <- deserialize (poolExpr p) s
    return $ p {poolExpr = deserialized, poolPriorLines = poolPriorLines p <> assignments}

  f args (ReturnM e) = do
    p <- f args e
    return $ p { poolExpr = "return(" <> poolExpr p <> ")" }
translateManifold _ = error "Every ExprM object must start with a Manifold term"



makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda args body = "lambda" <+> hsep (punctuate "," (map argName args)) <> ":" <+> body

argName :: Argument -> MDoc
argName (SerialArgument v _) = bndNamer v
argName (NativeArgument v _) = bndNamer v
argName (PassThroughArgument v) = bndNamer v

makeDispatch :: [ExprM One] -> MDoc
makeDispatch ms = align . vsep $
  [ align . vsep $ ["dispatch = {", indent 4 (vsep $ map entry ms), "}"]
  , "f = dispatch[cmdID]"
  ]
  where
    entry :: ExprM One -> MDoc
    entry (ManifoldM i _ _)
      = pretty i <> ":" <+> manNamer i <> ","
    entry _ = error "Expected ManifoldM"

typeSchema :: TypeP -> MorlocMonad MDoc
typeSchema t0 = f <$> type2jsontype t0
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

    result = f(*sys.argv[2:])

    print(result)
|]
