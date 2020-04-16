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
  es' <- mapM (invertExprM namer) es

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyExprM es')

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM segmentExprM es' |>> concat >>= mapM translateManifold

  -- make code for dispatching to manifolds
  let dispatch = makeDispatch es'

  return $ makePool lib includeDocs mDocs dispatch

namer :: Int -> EVar
namer i = EVar ("a" <> MT.show' i)

-- FIXME: should definitely use namespaces here, not `import *`
translateSource :: Path -> MorlocMonad MDoc
translateSource (Path s) = do
  (Path lib) <- MM.asks configLibrary
  let mod =  pretty
           . MT.liftToText (map DC.toLower)
           . MT.replace "/" "."
           . MT.stripPrefixIfPresent "/" -- strip the leading slash (if present)
           . MT.stripPrefixIfPresent lib  -- make the path relative to the library
           . MT.liftToText SF.dropExtensions
           $ s
  return $ "from" <+> mod <+> "import *"

translateManifold :: ExprM -> MorlocMonad MDoc
translateManifold (Manifold t args i e) = do
  body <- translateExpr e
  let head = "def" <+> "m" <> viaShow i <> tupled (map makeArgument args) <> ":"
  return $ nest 4 (vsep [head, body])
  where
  translateExpr :: ExprM -> MorlocMonad MDoc
  translateExpr (LetM v e1 e2) = do
    e1' <- translateExpr e1
    e2' <- translateExpr e2
    return $ pretty v <+> "=" <+> e1' <> line <> e2'
  translateExpr (LamM c mv e) = do
    e' <- translateExpr e
    let vs = zipWith (\namedVar autoVar -> maybe autoVar (pretty . id) namedVar) mv $
                     (zipWith (<>) (repeat "p") (map viaShow [1..]))
    return $ "lambda " <+> hsep (punctuate "," vs) <> ":" <+> e' <> tupled vs
  translateExpr (VarM c v) = return (pretty v)
  translateExpr (CisAppM c src xs) = do
    xs' <- mapM translateExpr xs
    return $ pretty (srcName src) <> tupled xs'
  translateExpr (TrsAppM c i lang xs) = return "FOREIGN"
  translateExpr (ListM _ es) = do
    es' <- mapM translateExpr es
    return $ list es'
  translateExpr (TupleM _ es) = do
    es' <- mapM translateExpr es
    return $ tupled es'
  translateExpr (RecordM c entries) = do
    es' <- mapM (translateExpr . snd) entries
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return $ "dict" <> tupled entries'
  translateExpr (LogM c x) = return $ if x then "True" else "False"
  translateExpr (NumM c x) = return $ viaShow x
  translateExpr (StrM c x) = return . dquotes $ pretty x
  translateExpr (NullM c) = return "None"
  translateExpr (PackM e) = do
    e' <- translateExpr e
    (Unpacked t) <- typeOfExprM e
    return $ "pack" <> tupled [e', typeSchema t]
  translateExpr (UnpackM e) = do
    e' <- translateExpr e
    (Packed t) <- typeOfExprM e
    return $ "unpack" <> tupled [e', typeSchema t]
  translateExpr (ReturnM e) = do
    e' <- translateExpr e
    return $ "return(" <> e' <> ")"

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
    f _ = error "Cannot serialize this type"

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
