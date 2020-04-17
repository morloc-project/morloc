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
  ) where

import Morloc.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified System.FilePath as SF
import qualified Data.Char as DC


translate :: [Source] -> [ExprM] -> MorlocMonad MDoc
translate srcs es = do
  -- setup library paths
  lib <- fmap pretty $ MM.asks MC.configLibrary

  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- tree rewrites
  es' <- mapM (invertExprM varNamer) es

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyExprM es')

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold es'

  -- make code for dispatching to manifolds
  let dispatch = makeDispatch es'

  return $ makePool lib includeDocs mDocs dispatch

-- create an internal variable based on a unique id
varNamer :: Int -> EVar
varNamer i = EVar ("a" <> MT.show' i)

-- create a name for a manifold based on a unique id
manNamer :: Int -> EVar
manNamer i = EVar ("m" <> MT.show' i)

-- FIXME: should definitely use namespaces here, not `import *`
translateSource :: Path -> MorlocMonad MDoc
translateSource (Path s) = do
  (Path lib) <- MM.asks configLibrary
  let mod = pretty
          . MT.liftToText (map DC.toLower)
          . MT.replace "/" "."
          . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
          . MT.stripPrefixIfPresent lib  -- make the path relative to the library
          . MT.liftToText SF.dropExtensions
          $ s
  return $ "from" <+> mod <+> "import *"

-- break a call tree into manifolds
translateManifold :: ExprM -> MorlocMonad MDoc
translateManifold m@(Manifold _ args _ _) = (vsep . punctuate line . fst) <$> f args m where
  f :: [Argument] -> ExprM -> MorlocMonad ([MDoc], MDoc)
  f pargs (Manifold t args i e) = do
    (ms', body) <- f args e
    let head = "def" <+> pretty (manNamer i) <> tupled (map makeArgument args) <> ":"
        mdoc = nest 4 (vsep [head, body])
        mname = pretty (manNamer i)
    call <- return $ case (splitArgs args pargs, nargsTypeM t) of
      ((rs, []), _) -> mname <> tupled (map makeArgument rs) -- covers #1, #2 and #4
      ((rs, vs), _) -> makeLambda (rs ++ vs) (mname <> tupled (map makeArgument (rs ++ vs))) -- covers #5
    return (mdoc : ms', call)
  f args (LetM v e1 e2) = do
    (ms1', e1') <- (f args) e1
    (ms2', e2') <- (f args) e2
    return (ms1' ++ ms2', pretty v <+> "=" <+> e1' <> line <> e2')
  f args (CisAppM c src xs) = do
    (mss', xs') <- mapM (f args) xs |>> unzip
    return (concat mss', pretty (srcName src) <> tupled xs')
  f args (TrsAppM c i lang xs) = return ([], "FOREIGN")
  f args (LamM c mv e) = do
    (ms', e') <- f args e
    let vs = zipWith (\namedVar autoVar -> maybe autoVar (pretty . id) namedVar) mv $
                     (zipWith (<>) (repeat "p") (map viaShow [1..]))
    return (ms', "lambda " <+> hsep (punctuate "," vs) <> ":" <+> e' <> tupled vs)
  f args (ListM _ es) = do
    (mss', es') <- mapM (f args) es |>> unzip
    return (concat mss', list es')
  f args (TupleM _ es) = do
    (mss', es') <- mapM (f args) es |>> unzip
    return (concat mss', tupled es')
  f args (RecordM c entries) = do
    (mss', es') <- mapM (f args . snd) entries |>> unzip
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return (concat mss', "dict" <> tupled entries')
  f _ (VarM c v) = return ([], pretty v)
  f _ (LogM _ x) = return ([], if x then "True" else "False")
  f _ (NumM _ x) = return ([], viaShow x)
  f _ (StrM _ x) = return ([], dquotes $ pretty x)
  f _ (NullM _) = return ([], "None")
  f args (PackM e) = do
    (ms, e') <- f args e
    (Unpacked t) <- typeOfExprM e
    return (ms, "pack" <> tupled [e', typeSchema t])
  f args (UnpackM e) = do
    (ms, e') <- f args e
    (Packed t) <- typeOfExprM e
    return (ms, "unpack" <> tupled [e', typeSchema t])
  f args (ReturnM e) = do
    (ms, e') <- f args e
    return (ms, "return(" <> e' <> ")")

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
makeArgument (PackedArgument v c) = pretty v
makeArgument (UnpackedArgument v c) = pretty v
makeArgument (PassThroughArgument v) = pretty v

makeDispatch :: [ExprM] -> MDoc
makeDispatch ms = align . vsep $
  [ align . vsep $ ["dispatch = {", indent 4 (vsep $ map entry ms), "}"]
  , "result = dispatch[cmdID](*sys.argv[2:])"
  ]
  where
    entry :: ExprM -> MDoc
    entry (Manifold _ _ i _)
      = pretty i <> ":" <+> "m" <> pretty i <> ","
    entry _ = error "Expected Manifold"

typeSchema :: CType -> MDoc
typeSchema c = f (unCType c)
  where
    f (VarT v) = lst [var v, "None"]
    f (ArrT v ps) = lst [var v, lst (map f ps)]
    f (NamT v es) = lst [var v, dict (map entry es)]
    f t = error $ "Cannot serialize this type: " ++ show t

    entry :: (MT.Text, Type) -> MDoc
    entry (v, t) = pretty v <> "=" <> f t

    dict :: [MDoc] -> MDoc
    dict xs = "dict" <> lst xs

    lst :: [MDoc] -> MDoc
    lst xs = encloseSep "(" ")" "," xs

    var :: TVar -> MDoc
    var (TV _ v) = dquotes (pretty v)

makePool :: MDoc -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makePool lib includeDocs manifolds dispatch = [idoc|#!/usr/bin/env python

import sys
import subprocess
import json

sys.path = ["#{lib}"] + sys.path

#{vsep includeDocs}

def _morloc_unpack(unpacker, jsonString, mid, filename):
    try:
        pyObj = unpacker(jsonString)
    except Exception:
        msg = "Error in %s::%s - JSON parse failure" % (filename, mid)
        if(len(jsonString) == 0):
            msg += ": empty document"
        else:
            msg += ", bad document:\n%s" % str(jsonString)
        sys.exit(msg)
    return(pyObj)

def _morloc_foreign_call(interpreter, pool, mid, args):
    try:
        sysObj = subprocess.run(
            [interpreter, pool, mid, *args],
            capture_output=True,
            check=True,
            encoding="ascii"
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))

    jsonString = sysObj.stdout
    jsonLog = sysObj.stderr

    if(len(jsonLog) > 0):
      print(jsonLog, file=sys.stderr)

    return(jsonString)

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

    print(result)
|]
