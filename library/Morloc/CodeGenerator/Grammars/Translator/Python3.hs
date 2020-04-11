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
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM 
import qualified Morloc.Data.Text as MT
import qualified System.FilePath as SF
import qualified Data.Char as DC


translate :: [Source] -> [CallTree] -> MorlocMonad MDoc
translate srcs mss = do 
  -- setup library paths
  lib <- fmap pretty $ MM.asks MC.configLibrary

  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- handle serialzation
  mss' <- mapM serializeCallTree mss >>= mapM (extractAssignment namer)

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyCallTree mss')

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold (concat [m:ms | (CallTree m ms) <- mss'])

  -- make code for dispatching to manifolds
  let dispatch = makeDispatch [m | (CallTree m _) <- mss]

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

translateManifold :: Manifold -> MorlocMonad MDoc
translateManifold (Manifold v args es) = do
  let head = "def" <+> returnName v <> tupled (map makeArgument args) <> ":"
  body <- mapM (translateExpr args) es |>> vsep
  return $ vsep [head, indent 4 body]

translateExpr :: [Argument] -> ExprM -> MorlocMonad MDoc
translateExpr args (AssignM v e) = do
  e' <- translateExpr args e
  return $ pretty v <+> "=" <+> e'
translateExpr args (SrcCallM _ (VarM _ v) es) = do
  xs <- mapM (translateExpr args) es
  return $ pretty v <> tupled xs
translateExpr args (ManCallM _ i es) = do
  xs <- mapM (translateExpr args) es
  return $ "m" <> pretty i <> tupled xs
translateExpr args (PartialM _ i (ManCallM c mid es)) = do
  let es' = map (translateExpr args) es
      vs = take i $ zipWith (<>) (repeat "p") (map viaShow [1..])
  return $ "lambda " <+> hsep (punctuate "," vs) <> ":" <+> "m" <> pretty mid <> tupled vs
translateExpr args (ForeignCallM _ i lang vs) = return "FOREIGN"
translateExpr args (ReturnM e) = do
  doc <- translateExpr args e
  return $ "return(" <> doc <> ")"
translateExpr args (VarM _ v) = return $ pretty v
translateExpr args (ListM _ es) = do
  xs <- mapM (translateExpr args) es
  return $ list xs
translateExpr args (TupleM _ es) = do
  xs <- mapM (translateExpr args) es
  return $ tupled xs
translateExpr args (RecordM _ entries) = do
  xs' <- mapM (translateExpr args . snd) entries
  let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) xs'
  return $ "dict" <> tupled entries'
translateExpr args (LogM _ x) = return $ if x then "True" else "False"
translateExpr args (NumM _ x) = return $ viaShow x
translateExpr args (StrM _ x) = return $ dquotes (pretty x)
translateExpr args (NullM _) = return "None"
translateExpr args (PackM e) = do
  e' <- translateExpr args e
  let c = typeOfExprM e
      schema = typeSchema c
  return $ "pack" <> tupled [e', schema]
translateExpr args (UnpackM e) = do
  e' <- translateExpr args e
  let c = typeOfExprM e
      schema = typeSchema c
  return $ "unpack" <> tupled [e', schema]

makeArgument :: Argument -> MDoc
makeArgument (PackedArgument v c) = pretty v
makeArgument (UnpackedArgument v c) = pretty v
makeArgument (PassThroughArgument v) = pretty v

returnName :: ReturnValue -> MDoc
returnName (PackedReturn v _) = "m" <> pretty v
returnName (UnpackedReturn v _) = "m" <> pretty v
returnName (PassThroughReturn v) = "m" <> pretty v

makeDispatch :: [Manifold] -> MDoc
makeDispatch ms = align . vsep $
  [ align . vsep $ ["dispatch = {", indent 4 (vsep $ map entry ms), "}"]
  , "result = dispatch[cmdID](*sys.argv[2:])"
  ]
  where
    entry :: Manifold -> MDoc
    entry (Manifold v _ _)
      = pretty (returnId v) <> ":" <+> "m" <> pretty (returnId v) <> ","

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
