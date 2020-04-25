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
  es' <- mapM invertExprM es

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyExprM es')

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold es'

  let dispatch = makeDispatch es'
      signatures = map makeSignature es'

  -- create and return complete pool script
  return $ makeMain includeDocs signatures mDocs dispatch

letNamer :: Int -> EVar
letNamer i = EVar ("a" <> MT.show' i)

manNamer :: Int -> EVar
manNamer i = EVar ("m" <> MT.show' i)

bndNamer :: Int -> EVar
bndNamer i = EVar ("x" <> MT.show' i)

serialType :: MDoc
serialType = "std::string"

makeSignature :: ExprM -> MDoc
makeSignature e@(ManifoldM _ _ _) = vsep (f e) where
  f :: ExprM -> [MDoc]
  f m@(ManifoldM i args e) =
    let t = typeOfExprM m
        sig = returnType t <+> "m" <> pretty i <> tupled (map makeArg args) <> ";"
    in sig : f e
  f (LetM _ e1 e2) = f e1 ++ f e2
  f (AppM e es) = f e ++ conmap f es
  f (LamM _ e) = f e
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

makeArg :: Argument -> MDoc
makeArg r@(PackedArgument _ _) = serialType <+> argName r
makeArg r@(UnpackedArgument _ c) = showType c <+> argName r
makeArg r@(PassThroughArgument _) = serialType <+> argName r

argName :: Argument -> MDoc
argName (PackedArgument i _) = "x" <> pretty i
argName (UnpackedArgument i _) = "x" <> pretty i
argName (PassThroughArgument i) = "x" <> pretty i

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
translateManifold m@(ManifoldM _ args _) =
  (vsep . punctuate line . (\(x,_,_)->x)) <$> f args m
  where
  f :: [Argument]
    -> ExprM
    -> MorlocMonad
       ( [MDoc] -- the collection of final manifolds
       , MDoc -- a call tag for this expression
       , [MDoc] -- a list of statements that should precede this assignment
       )

  f args (LetM v (PackM e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    t <- showUnpackedTypeM (typeOfExprM e1)
    let schemaName = pretty v <> "_schema"
        schema = t <+> schemaName <> ";"
        packing = serialType <+> pretty v <+> "=" <+> "pack" <> tupled [e1', schemaName] <> ";"
    return (ms1 ++ ms2, vsep $ ps1 ++ ps2 ++ [schema, packing, e2'], [])

  f _ (PackM _) = MM.throwError . OtherError
    $ "PackM should only appear in an assignment"

  f args (LetM i (UnpackM e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    t <- showUnpackedTypeM (typeOfExprM e1)
    let schemaName = pretty (letNamer i) <> "_schema"
        schema = t <+> schemaName <> ";"
        unpacking = t <+> pretty (letNamer i) <+> "=" <+> "unpack" <> tupled [e1', schemaName] <> ";"
    return (ms1 ++ ms2, vsep $ ps1 ++ ps2 ++ [schema, unpacking, e2'], [])
  f _ (UnpackM _) = MM.throwError . OtherError
    $ "UnpackM should only appear in an assignment"

  f args (LetM i e1 e2) = do
    (ms1', e1', ps1) <- (f args) e1
    (ms2', e2', ps2) <- (f args) e2
    let t = showTypeM (typeOfExprM e1)
        ps = ps1 ++ ps2 ++ [t <+> pretty (letNamer i) <+> "=" <+> e1' <> ";", e2']
    return (ms1' ++ ms2', vsep ps, [])

  f args (AppM (SrcM c src) xs) = do
    (mss', xs', pss) <- mapM (f args) xs |>> unzip3
    let inputs = map (showTypeM . unpackTypeM . typeOfExprM') xs
        otype = showTypeM c
        name = pretty $ srcName src
        mangledName = name <> "_fun"
        inputBlock = "(" <> cat (punctuate "," inputs) <> ")"
        lhs = otype <+> "(*" <> mangledName <> ")" <> inputBlock
        rhs = "&" <> name
        sig =  lhs <+> "=" <+> rhs <> ";";
    return (concat mss', mangledName <> tupled xs', sig : concat pss)
    where
      typeOfExprM' :: ExprM -> TypeM
      typeOfExprM' m@(ManifoldM _ args' _) = case splitArgs args' args of
        (_, []) -> typeOfExprM m
        (_, ts) -> Function (map arg2typeM ts) (typeOfExprM m)
      typeOfExprM' e = typeOfExprM e

  f args (AppM f xs) = error "Goddamn it! You just had to ask!"

  f args (SrcM t src) = return ([], pretty $ srcName src, [])

  f pargs m@(ManifoldM i args e) = do
    (ms', body, ps1) <- f args e
    let t = typeOfExprM m
        head = showTypeM t <+> pretty (manNamer i) <> tupled (map makeArgument args)
        mdoc = block 4 head body
        mname = pretty (manNamer i)
    (call, ps2) <- case (splitArgs args pargs, nargsTypeM t) of
      ((rs, []), _) -> return (mname <> tupled (map (pretty . bndNamer . argId) rs), [])
      (([], vs), _) -> return (mname, [])
      ((rs, vs), _) -> do
        let v = mname <> "_fun"
        lhs <- stdFunction t vs |>> (\x -> x <+> v)
        castFunction <- staticCast t args mname
        let vs' = take
                  (length vs)
                  (map (\i -> "std::placeholders::_" <> viaShow i) [1..])
            rs' = map (pretty . bndNamer . argId) rs
            rhs = stdBind $ castFunction : (rs' ++ vs')
            sig = nest 4 (vsep [lhs <+> "=", rhs]) <> ";"
        return (v, [sig])
    return (mdoc : ms', call, ps1 ++ ps2)

  f args (ForeignCallM c i lang foreignArgs) = return ([], "FOREIGN", [])

  f args (LamM lambdaArgs e) = undefined

  f args (ListM t es) = do
    (mss', es', pss) <- mapM (f args) es |>> unzip3
    let x' = encloseSep "{" "}" "," es'
    return (concat mss', x', concat pss)

  f args (TupleM _ es) = do
    (mss', es', pss) <- mapM (f args) es |>> unzip3
    return (concat mss', "std::make_tuple" <> tupled es', concat pss)

  f args (RecordM c entries) = error "C++ records not yet supported"

  f _ (BndVarM c i) = return ([], pretty (bndNamer i), [])
  f _ (LetVarM c i) = return ([], pretty (letNamer i), [])
  f _ (LogM _ x) = return ([], if x then "true" else "false", [])
  f _ (NumM _ x) = return ([], viaShow x, [])
  f _ (StrM _ x) = return ([], dquotes $ pretty x, [])
  f _ (NullM _) = return ([], "null", [])

  f args (ReturnM e) = do
    (ms, e', ps) <- f args e
    return (ms, "return(" <> e' <> ");", ps)


stdFunction :: TypeM -> [Argument] -> MorlocMonad MDoc
stdFunction t args = return $
  "std::function<" <> showTypeM t <> "(" <> cat (punctuate "," (map argTypeM args)) <> ")>"

stdBind :: [MDoc] -> MDoc
stdBind xs = "std::bind" <> "(" <> cat (punctuate "," xs) <> ")"

staticCast :: TypeM -> [Argument] -> MDoc -> MorlocMonad MDoc
staticCast t args name = do
  let output = showTypeM t
      inputs = map argTypeM args
      typedef = output <> "(*)(" <> cat (punctuate "," inputs) <> ")"
  return $ "static_cast<" <> typedef <> ">" <> "(&" <> name <> ")"

argTypeM :: Argument -> MDoc
argTypeM (PackedArgument _ _) = serialType
argTypeM (UnpackedArgument _ c) = showType c
argTypeM (PassThroughArgument _) = serialType

makeLambda :: [Argument] -> MDoc -> MDoc
makeLambda args body = "lambda" <+> hsep (punctuate "," (map makeArgument args)) <> ":" <+> body

-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map split args1 where
  split :: Argument -> Either Argument Argument
  split r = if elem r args2
            then Left r
            else Right r

makeArgument :: Argument -> MDoc
makeArgument (PackedArgument v c) = serialType <+> pretty v
makeArgument (UnpackedArgument v c) = showType c <+> pretty v
makeArgument (PassThroughArgument v) = serialType <+> pretty v

makeDispatch :: [ExprM] -> MDoc
makeDispatch = undefined
-- makeDispatch ms = block 4 "switch(cmdID)" (vsep (map makeCase ms))
--   where
--     makeCase :: ExprM -> MDoc
--     makeCase (ManifoldM _ args i _) =
--       let mid = pretty i
--           manifoldName = "m" <> mid
--           args' = take (length args) $ map (\i -> "argv[" <> viaShow i <> "]") [2..]
--       in
--         (nest 4 . vsep)
--           [ "case" <+> mid <> ":"
--           , "result = " <> manifoldName <> tupled args' <> ";"
--           , "break;"
--           ]

showType :: CType -> MDoc
showType = MTM.buildCType mkfun mkrec where
  mkfun :: MDoc -> [MDoc] -> MDoc
  mkfun _ _ = "FUNCTION_TYPE"

  mkrec :: [(MDoc, MDoc)] -> MDoc
  mkrec _ = "RECORD_TYPE"

showTypeM :: TypeM -> MDoc
showTypeM Passthrough = serialType
showTypeM (Packed t) = serialType
showTypeM (Unpacked t) = showType t
showTypeM (Function ts t) = "std::function<" <> showTypeM t <> "(" <> cat (punctuate "," (map showTypeM ts)) <> ")>"

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
