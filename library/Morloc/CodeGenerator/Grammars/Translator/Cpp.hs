{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Cpp
Description : C++ translator
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Translator.Cpp
  ( 
    translate
  ) where

import Morloc.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Data.Doc
import Morloc.Quasi
import qualified Morloc.System as MS
import qualified Morloc.TypeChecker.Macro as MTM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM


translate :: [Source] -> [CallTree] -> MorlocMonad MDoc
translate srcs mss = do 
  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- handle serialzation
  mss' <- mapM serializeCallTree mss >>= mapM (invertTree namer)

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyCallTree mss')

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold (concat [m:ms | (CallTree m ms) <- mss'])

  let dispatch = makeDispatch [m | (CallTree m _) <- mss']
      signatures = map makeSignature (concat [m:ms | (CallTree m ms) <- mss'])

  -- create and return complete pool script
  return $ makeMain includeDocs signatures mDocs dispatch

namer :: Int -> EVar
namer i = EVar ("a" <> MT.show' i)

serialType :: MDoc
serialType = "std::string"

makeSignature :: Manifold -> MDoc
makeSignature (Manifold v args _) =
  (returnType v) <+> "m" <> pretty (returnId v) <> tupled (map makeArg args) <> ";"

makeArg (PackedArgument v c) = serialType <+> pretty v
makeArg (UnpackedArgument v c) = showType c <+> pretty v
makeArg (PassThroughArgument v) = serialType <+> pretty v

-- TLDR: Use `#include "foo.h"` rather than `#include <foo.h>`
-- Include statements in C can be either wrapped in angle brackets (e.g.,
-- `<stdio.h>`) or in quotes (e.g., `"myfile.h"`). The difference between these
-- is implementation specific. I currently use the GCC compiler. For quoted
-- strings, it first searches relative to the working directory and then, if
-- nothing is found, searches system files. For angle brackets, it searches
-- only system files: <https://gcc.gnu.org/onlinedocs/cpp/Search-Path.html>. So
-- quoting seems more reasonable, for now. This might change only if I start
-- loading the morloc libraries into the system directories (which might be
-- reasonable), though still, quotes would work.
--
-- UPDATE: The build system will now read the source paths from the Script
-- object and write an `-I${MORLOC_HOME}/lib/${MORLOC_PACKAGE}` argument for
-- g++. This will tell g++ where to look for headers. So now in the generated
-- source code I can just write the basename. This makes the generated code
-- neater (no hard-coded local paths), but now the g++ compiler will search
-- through all the module paths for each file, which introduces the possibility
-- of name conflicts.
translateSource
  :: Path -- ^ Path to a header (e.g., `$MORLOC_HOME/lib/foo.h`)
  -> MorlocMonad MDoc
translateSource path = return $
  "#include" <+> (dquotes . pretty . MS.takeFileName) path

translateManifold :: Manifold -> MorlocMonad MDoc
translateManifold (Manifold v args e) = do
  let head = returnType v <+> "m" <> returnName v <> tupled (map makeArgument args)
  body <- translateExpr args e
  return $ line <> block 4 head body

translateExpr :: [Argument] -> ExprM -> MorlocMonad MDoc
translateExpr args (LetM v (PackM e1) e2) = do
  e1' <- translateExpr args e1
  e2' <- translateExpr args e2
  let schemaName = pretty v <> "_schema"
      t = showType (typeOfExprM e1)
      schema = t <+> schemaName <> ";"
      unpacking = t <+> pretty v <+> "=" <+> "pack" <> tupled [e1', schemaName] <> ";"
  return (vsep [schema, unpacking, e2'])
translateExpr args (LetM v (UnpackM e1) e2) = do
  e1' <- translateExpr args e1
  e2' <- translateExpr args e2
  let schemaName = pretty v <> "_schema"
      t = showType (typeOfExprM e1)
      schema = t <+> schemaName <> ";"
      packing = t <+> pretty v <+> "=" <+> "unpack" <> tupled [e1', schemaName] <> ";"
  return (vsep [schema, packing, e2'])
translateExpr args (LetM v e1 e2) = do
  e1' <- translateExpr args e1
  e2' <- translateExpr args e2
  return $ pretty v <+> "=" <+> e1' <> line <> e2' 
translateExpr args (AppM c f es) = do
  f' <- translateExpr args f 
  es' <- mapM (translateExpr args) es
  return $ f' <> tupled es'
translateExpr args (LamM c mv e) = do
  e' <- translateExpr args e
  let vs = zipWith (\namedVar autoVar -> maybe autoVar (pretty . id) namedVar) mv $
                   (zipWith (<>) (repeat "p") (map viaShow [1..]))
  return $ "function(" <+> hsep (punctuate "," vs) <> "){" <+> e' <> tupled vs <> "}"
translateExpr args (VarM c v) = return (pretty v)
translateExpr args (CisM c i args') = return $
  "m" <> viaShow i <> tupled (map (pretty . argName) args')
translateExpr args (TrsM c i lang) = return "FOREIGN"
translateExpr args (ListM _ es) = do
  es' <- mapM (translateExpr args) es
  return $ list es'
translateExpr args (TupleM _ es) = do
  es' <- mapM (translateExpr args) es
  return $ tupled es'
translateExpr args (RecordM c entries) = do
  es' <- mapM (translateExpr args . snd) entries
  let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
  return $ "dict" <> tupled entries'
translateExpr args (LogM c x) = return $ if x then "True" else "False"
translateExpr args (NumM c x) = return $ viaShow x
translateExpr args (StrM c x) = return . dquotes $ pretty x
translateExpr args (NullM c) = return "None"
translateExpr args (ReturnM e) = do
  e' <- translateExpr args e
  return $ "return(" <> e' <> ")"

-- translateExpr args (AssignM v (PackM e)) = do
--   e' <- translateExpr args e
--   let schemaName = pretty v <> "_schema"
--       t = showType (typeOfExprM e)
--       schema = t <+> schemaName <> ";"
--       packing = serialType <+> pretty v <+> "=" <+> "pack" <> tupled [e', schemaName] <> ";"
--   return (vsep [schema, packing])
-- translateExpr args (AssignM v (UnpackM e)) = do
--   e' <- translateExpr args e
--   let schemaName = pretty v <> "_schema"
--       t = showType (typeOfExprM e)
--       schema = t <+> schemaName <> ";"
--       packing = t <+> pretty v <+> "=" <+> "unpack" <> tupled [e', schemaName] <> ";"
--   return (vsep [schema, packing])
-- translateExpr args (AssignM v e) = do
--   e' <- translateExpr args e
--   let t = showType (typeOfExprM e)
--   return $ t <+> pretty v <+> "=" <+> e' <> ";"
-- translateExpr args (SrcCallM _ (VarM _ v) es) = do
--   xs <- mapM (translateExpr args) es
--   return $ pretty v <> tupled xs
-- translateExpr args (ManCallM _ i es) = do
--   xs <- mapM (translateExpr args) es
--   return $ "m" <> pretty i <> tupled xs
-- translateExpr args (PartialM _ i (ManCallM c mid es)) = return $ "m" <> viaShow mid
-- translateExpr _ (LamM _ mid) = return $ "m" <> viaShow mid
-- translateExpr args (ForeignCallM _ i lang vs) = return "FOREIGN"
-- translateExpr args (ReturnM e) = do
--   e' <- translateExpr args e
--   return $ "return(" <> e' <> ");"
-- translateExpr args (VarM _ v) = return $ pretty v
-- translateExpr args (ListM _ es) = do
--   xs <- mapM (translateExpr args) es
--   return $ encloseSep "{" "}" "," xs
-- translateExpr args (TupleM _ es) = do
--   xs <- mapM (translateExpr args) es
--   return $ "std::make_tuple" <> tupled xs
-- translateExpr args (RecordM _ entries) = MM.throwError . NotImplemented
--   $ "Records in C++ are not yet supported"
-- translateExpr args (LogM _ x) = return $ if x then "true" else "false"
-- translateExpr args (NumM _ x) = return $ viaShow x
-- translateExpr args (StrM _ x) = return $ dquotes (pretty x)
-- translateExpr args (NullM _) = return "NULL"
-- translateExpr args (PackM e) = MM.throwError . OtherError
--   $ "PackM should only appear in an assignment"
-- translateExpr args (UnpackM e) = MM.throwError . OtherError
--   $ "UnpackM should only appear in an assignment"


makeArgument :: Argument -> MDoc
makeArgument (PackedArgument v c) = serialType <+> pretty v
makeArgument (UnpackedArgument v c) = showType c <+> pretty v
makeArgument (PassThroughArgument v) = serialType <+> pretty v

makeDispatch :: [Manifold] -> MDoc
makeDispatch ms = block 4 "switch(cmdID)" (vsep (map makeCase ms))
  where
    makeCase :: Manifold -> MDoc
    makeCase (Manifold v args es) =
      let mid = pretty (returnId v)
          manifoldName = "m" <> mid
          args' = take (length args) $ map (\i -> "argv[" <> viaShow i <> "]") [2..]
      in
        (nest 4 . vsep)
          [ "case" <+> mid <> ":"
          , "result = " <> manifoldName <> tupled args' <> ";"
          , "break;"
          ]

returnType :: ReturnValue -> MDoc 
returnType (UnpackedReturn _ c) = showType c
returnType _ = serialType

returnName :: ReturnValue -> MDoc
returnName (PackedReturn v _) = pretty v
returnName (UnpackedReturn v _) = pretty v
returnName (PassThroughReturn v) = pretty v

showType :: CType -> MDoc
showType = MTM.buildCType mkfun mkrec where
  mkfun :: MDoc -> [MDoc] -> MDoc
  mkfun _ _ = error "Function type annotations not supported in C++"

  mkrec :: [(MDoc, MDoc)] -> MDoc
  mkrec _ = error "Record type annotations not supported in C++"

makeMain :: [MDoc] -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makeMain includes signatures manifolds dispatch = [idoc|#include <string>
#include <iostream>
#include <functional>

#{vsep includes}

#{vsep signatures}

#{vsep manifolds}

int main(int argc, char * argv[])
{
    int cmdID;
    #{serialType} result;
    cmdID = std::stoi(argv[1]);
    #{dispatch}
    std::cout << result << std::endl;
    return 0;
}
|]
