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


translate :: [Source] -> [ExprM] -> MorlocMonad MDoc
translate srcs es = do
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

  let dispatch = makeDispatch es'
      signatures = map makeSignature es'

  -- create and return complete pool script
  return $ makeMain includeDocs signatures mDocs dispatch

varNamer :: Int -> EVar
varNamer i = EVar ("a" <> MT.show' i)

manNamer :: Int -> EVar
manNamer i = EVar ("m" <> MT.show' i)

serialType :: MDoc
serialType = "std::string"

makeSignature :: ExprM -> MDoc
makeSignature e@(Manifold _ _ _ _) = vsep (f e) where
  f :: ExprM -> [MDoc]
  f (Manifold t args i e) =
    let sig = returnType t <+> "m" <> pretty i <> tupled (map makeArg args) <> ";"
    in sig : f e
  f (LetM _ e1 e2) = f e1 ++ f e2
  f (CisAppM _ _ es) = conmap f es
  f (TrsAppM _ _ _ es) = conmap f es
  f (LamM _ _ e) = f e
  f (ListM _ es) = conmap f es
  f (TupleM _ es) = conmap f es
  f (RecordM _ entries) = conmap f (map snd entries)
  f (PackM e) = f e
  f (UnpackM e) = f e
  f (ReturnM e) = f e
  f _ = []

returnType :: TypeM -> MDoc
returnType (Unpacked c) = showType c
returnType _ = serialType

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

translateManifold :: ExprM -> MorlocMonad MDoc
translateManifold m@(Manifold _ args _ _) = (vsep . punctuate line . fst) <$> f args m where
  f :: [Argument] -> ExprM -> MorlocMonad ([MDoc], MDoc)
  f pargs (Manifold t args i e) = do
    (ms', body) <- f args e
    let head = showTypeM t <+> pretty (manNamer i) <> tupled (map makeArgument args)
        mdoc = block 4 head body
        mname = pretty (manNamer i)
    call <- return $ case (splitArgs args pargs, nargsTypeM t) of
      ((rs, []), _) -> mname <> tupled (map (pretty . argName) rs) -- covers #1, #2 and #4
      (([], vs), _) -> mname
      ((rs, vs), _) -> makeLambda vs (mname <> tupled (map (pretty . argName) (rs ++ vs))) -- covers #5
    return (mdoc : ms', call)

  f args (LetM v (PackM e1) e2) = do
    (ms1, e1') <- f args e1
    (ms2, e2') <- f args e2
    t <- typeOfExprM e1 >>= showUnpackedTypeM
    let schemaName = pretty v <> "_schema"
        schema = t <+> schemaName <> ";"
        packing = serialType <+> pretty v <+> "=" <+> "pack" <> tupled [e1', schemaName] <> ";"
    return (ms1 ++ ms2, vsep [schema, packing, e2'])
  f _ (PackM _) = MM.throwError . OtherError
    $ "PackM should only appear in an assignment"

  f args (LetM v (UnpackM e1) e2) = do
    (ms1, e1') <- f args e1
    (ms2, e2') <- f args e2
    t <- typeOfExprM e1 >>= showUnpackedTypeM
    let schemaName = pretty v <> "_schema"
        schema = t <+> schemaName <> ";"
        unpacking = t <+> pretty v <+> "=" <+> "unpack" <> tupled [e1', schemaName] <> ";"
    return (ms1 ++ ms2, vsep [schema, unpacking, e2'])
  f _ (UnpackM _) = MM.throwError . OtherError
    $ "UnpackM should only appear in an assignment"

  f args (LetM v e1 e2) = do
    (ms1', e1') <- (f args) e1
    (ms2', e2') <- (f args) e2
    t <- showTypeM <$> typeOfExprM e1
    return (ms1' ++ ms2', vsep [t <+> pretty v <+> "=" <+> e1' <> ";", e2'])

  f args (CisAppM c src xs) = do
    (mss', xs') <- mapM (f args) xs |>> unzip
    return (concat mss', pretty (srcName src) <> tupled xs')

  f args (TrsAppM c i lang xs) = return ([], "FOREIGN")

  f args (LamM c mv e) = undefined
    -- (ms', e') <- f args e
    -- let vs = zipWith (\namedVar autoVar -> maybe autoVar (pretty . id) namedVar) mv $
    --                  (zipWith (<>) (repeat "p") (map viaShow [1..]))
    -- return (ms', "function" <> tupled vs <> "{" <+> e' <> tupled vs <> "}")

  f args (ListM t es) = do
    (mss', es') <- mapM (f args) es |>> unzip
    x' <- return $ case t of
      (Unpacked (CType (ArrT _ [VarT et]))) -> case et of       
        (TV _ "numeric") -> "c" <> tupled es'
        (TV _ "logical") -> "c" <> tupled es'
        (TV _ "character") -> "c" <> tupled es'
        _ -> "list" <> tupled es'
      _ -> "list" <> tupled es'
    return (concat mss', x')

  f args (TupleM _ es) = do
    (mss', es') <- mapM (f args) es |>> unzip
    return (concat mss', "list" <> tupled es')

  f args (RecordM c entries) = do
    (mss', es') <- mapM (f args . snd) entries |>> unzip
    let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    return (concat mss', "list" <> tupled entries')

  f _ (VarM c v) = return ([], pretty v)
  f _ (LogM _ x) = return ([], if x then "TRUE" else "FALSE")
  f _ (NumM _ x) = return ([], viaShow x)
  f _ (StrM _ x) = return ([], dquotes $ pretty x)
  f _ (NullM _) = return ([], "NULL")

  f args (ReturnM e) = do
    (ms, e') <- f args e
    return (ms, "return(" <> e' <> ");")

makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda args body = "lambda" <+> hsep (punctuate "," (map makeArgument args)) <> ":" <+> body

-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map split args1 where
  split :: Argument -> Either Argument Argument
  split r = if elem r args2
            then Left r
            else Right r

-- translateExpr args (AppM c f es) = do
--   f' <- translateExpr args f
--   es' <- mapM (translateExpr args) es
--   return $ f' <> tupled es'
-- translateExpr args (LamM c mv e) = do
--   e' <- translateExpr args e
--   let vs = zipWith (\namedVar autoVar -> maybe autoVar (pretty . id) namedVar) mv $
--                    (zipWith (<>) (repeat "p") (map viaShow [1..]))
--   return $ "function(" <+> hsep (punctuate "," vs) <> "){" <+> e' <> tupled vs <> "}"
-- translateExpr args (VarM c v) = return (pretty v)
-- translateExpr args (CisM c i args') = return $
--   "m" <> viaShow i <> tupled (map (pretty . argName) args')
-- translateExpr args (TrsM c i lang) = return "FOREIGN"
-- translateExpr args (ListM _ es) = do
--   es' <- mapM (translateExpr args) es
--   return $ list es'
-- translateExpr args (TupleM _ es) = do
--   es' <- mapM (translateExpr args) es
--   return $ tupled es'
-- translateExpr args (RecordM c entries) = do
--   es' <- mapM (translateExpr args . snd) entries
--   let entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
--   return $ "dict" <> tupled entries'
-- translateExpr args (LogM c x) = return $ if x then "True" else "False"
-- translateExpr args (NumM c x) = return $ viaShow x
-- translateExpr args (StrM c x) = return . dquotes $ pretty x
-- translateExpr args (NullM c) = return "None"
-- translateExpr args (ReturnM e) = do
--   e' <- translateExpr args e
--   return $ "return(" <> e' <> ")"

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

makeDispatch :: [ExprM] -> MDoc
makeDispatch ms = block 4 "switch(cmdID)" (vsep (map makeCase ms))
  where
    makeCase :: ExprM -> MDoc
    makeCase (Manifold _ args i _) =
      let mid = pretty i
          manifoldName = "m" <> mid
          args' = take (length args) $ map (\i -> "argv[" <> viaShow i <> "]") [2..]
      in
        (nest 4 . vsep)
          [ "case" <+> mid <> ":"
          , "result = " <> manifoldName <> tupled args' <> ";"
          , "break;"
          ]

showType :: CType -> MDoc
showType = MTM.buildCType mkfun mkrec where
  mkfun :: MDoc -> [MDoc] -> MDoc
  mkfun _ _ = "FUNCTION_TYPE"

  mkrec :: [(MDoc, MDoc)] -> MDoc
  mkrec _ = "RECORD_TYPE"

showTypeM :: TypeM -> MDoc
showTypeM Null = error "For now, the Null TypeM is not in use, so WTF?"
showTypeM Passthrough = serialType
showTypeM (Packed t) = serialType
showTypeM (Unpacked t) = showType t
showTypeM (Function ts t) = error "Function type annotations not currently supported in C++"

showUnpackedTypeM :: TypeM -> MorlocMonad MDoc
showUnpackedTypeM (Packed t) = return $ showType t
showUnpackedTypeM (Unpacked t) = return $ showType t
showUnpackedTypeM _ = MM.throwError . OtherError $ "Expected packed or unpacked type"

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
