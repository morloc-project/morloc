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
  , preprocess
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Grammars.Translator.Source.CppInternals as Src
import Morloc.Data.Doc
import Morloc.Quasi
import qualified Morloc.System as MS
import qualified Morloc.Frontend.Macro as MTM
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

-- tree rewrites
preprocess :: ExprM Many -> MorlocMonad (ExprM Many)
preprocess = invertExprM

translate :: [Source] -> [ExprM One] -> MorlocMonad MDoc
translate srcs es = do
  -- translate sources
  includeDocs <- mapM
    translateSource
    (unique . catMaybes . map srcPath $ srcs)

  -- diagnostics
  liftIO . putDoc $ (vsep $ map prettyExprM es)

  -- translate each manifold tree, rooted on a call from nexus or another pool
  mDocs <- mapM translateManifold es

  let dispatch = makeDispatch es
      signatures = map makeSignature es

  -- create and return complete pool script
  return $ makeMain includeDocs signatures mDocs dispatch

letNamer :: Int -> MDoc
letNamer i = "a" <> viaShow i

manNamer :: Int -> MDoc
manNamer i = "m" <> viaShow i

bndNamer :: Int -> MDoc
bndNamer i = "x" <> viaShow i

serialType :: MDoc
serialType = "std::string"

makeSignature :: ExprM One -> MDoc
makeSignature e0@(ManifoldM _ _ _) = vsep (f e0) where
  f :: ExprM One -> [MDoc]
  f (ManifoldM (metaId->i) args e) =
    let t = typeOfExprM e
        sig = showTypeM t <+> manNamer i <> tupled (map makeArg args) <> ";"
    in sig : f e
  f (LetM _ e1 e2) = f e1 ++ f e2
  f (AppM e es) = f e ++ conmap f es
  f (LamM _ e) = f e
  f (ListM _ es) = conmap f es
  f (TupleM _ es) = conmap f es
  f (RecordM _ entries) = conmap f (map snd entries)
  f (SerializeM _ e) = f e
  f (DeserializeM _ e) = f e
  f (ReturnM e) = f e
  f _ = []

makeArg :: Argument -> MDoc
makeArg (SerialArgument i _) = serialType <+> bndNamer i
makeArg (NativeArgument i c) = showType c <+> bndNamer i
makeArg (PassThroughArgument i) = serialType <+> bndNamer i

argName :: Argument -> MDoc
argName (SerialArgument i _) = bndNamer i
argName (NativeArgument i _) = bndNamer i
argName (PassThroughArgument i) = bndNamer i

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

translateManifold :: ExprM One -> MorlocMonad MDoc
translateManifold m@(ManifoldM _ args _) =
  (vsep . punctuate line . (\(x,_,_)->x)) <$> f args m
  where
  f :: [Argument]
    -> ExprM One
    -> MorlocMonad
       ( [MDoc] -- the collection of final manifolds
       , MDoc -- a call tag for this expression
       , [MDoc] -- a list of statements that should precede this assignment
       )

  f args (LetM i (SerializeM _ e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    t <- showNativeTypeM (typeOfExprM e1)
    let schemaName = letNamer i <> "_schema"
        schema = [idoc|#{t} #{schemaName};|]
        serializing = [idoc|#{serialType} #{letNamer i} = serialize(#{e1'}, #{schemaName});|]
    return (ms1 ++ ms2, vsep $ ps1 ++ ps2 ++ [schema, serializing, e2'], [])

  f _ (SerializeM _ _) = MM.throwError . OtherError
    $ "SerializeM should only appear in an assignment"

  f args (LetM i (DeserializeM _ e1) e2) = do
    (ms1, e1', ps1) <- f args e1
    (ms2, e2', ps2) <- f args e2
    t <- showNativeTypeM (typeOfExprM e1)
    let schemaName = letNamer i <> "_schema"
        schema = [idoc|#{t} #{schemaName};|]
        deserializeing = [idoc|#{t} #{letNamer i} = deserialize(#{e1'}, #{schemaName});|]
    return (ms1 ++ ms2, vsep $ ps1 ++ ps2 ++ [schema, deserializeing, e2'], [])
  f _ (DeserializeM _ _) = MM.throwError . OtherError
    $ "DeserializeM should only appear in an assignment"

  f args (LetM i e1 e2) = do
    (ms1', e1', ps1) <- (f args) e1
    (ms2', e2', ps2) <- (f args) e2
    let t = showTypeM (typeOfExprM e1)
        ps = ps1 ++ ps2 ++ [[idoc|#{t} #{letNamer i} = #{e1'};|], e2']
    return (ms1' ++ ms2', vsep ps, [])

  f args (AppM (SrcM (Function inputs output) src) xs) = do
    (mss', xs', pss) <- mapM (f args) xs |>> unzip3
    let
        name = pretty $ srcName src
        mangledName = name <> "_fun"
        inputBlock = cat (punctuate "," (map showTypeM inputs))
        sig = [idoc|#{showTypeM output}(*#{mangledName})(#{inputBlock}) = &#{name};|]
    return (concat mss', mangledName <> tupled xs', sig : concat pss)
    where
      typeOfExprM' :: ExprM One -> TypeM
      typeOfExprM' m@(ManifoldM _ args' _) = case splitArgs args' args of
        (_, []) -> typeOfExprM m
        (_, ts) -> Function (map arg2typeM ts) (typeOfExprM m)
      typeOfExprM' e = typeOfExprM e

  f args (AppM f xs) = error "Goddamn it! You just had to ask!"

  f args (SrcM t src) = return ([], pretty $ srcName src, [])

  f pargs m@(ManifoldM (metaId->i) args e) = do
    (ms', body, ps1) <- f args e
    let t = typeOfExprM e
        head = showTypeM t <+> manNamer i <> tupled (map makeArg args)
        mdoc = block 4 head body
        mname = manNamer i
    (call, ps2) <- case (splitArgs args pargs, nargsTypeM t) of
      ((rs, []), _) -> return (mname <> tupled (map (bndNamer . argId) rs), [])
      (([], vs), _) -> return (mname, [])
      ((rs, vs), _) -> do
        let v = mname <> "_fun"
        lhs <- stdFunction t vs |>> (\x -> x <+> v)
        castFunction <- staticCast t args mname
        let vs' = take
                  (length vs)
                  (map (\i -> "std::placeholders::_" <> viaShow i) [1..])
            rs' = map (bndNamer . argId) rs
            rhs = stdBind $ castFunction : (rs' ++ vs')
            sig = nest 4 (vsep [lhs <+> "=", rhs]) <> ";"
        return (v, [sig])
    return (mdoc : ms', call, ps1 ++ ps2)

  f _ (PoolCallM t _ cmds args) = do
    let bufDef = "std::ostringstream s;"
        callArgs = map dquotes cmds ++ map argName args
        cmd = "s << " <> cat (punctuate " << \" \" << " callArgs) <> ";"
        call = [idoc|foreign_call(s.str())|] 
    return ([], call, [bufDef, cmd])

  f args (ForeignInterfaceM _ _) = MM.throwError . CallTheMonkeys $
    "Foreign interfaces should have been resolved before passed to the translators"

  f args (LamM lambdaArgs e) = undefined

  f args (ListM t es) = do
    (mss', es', pss) <- mapM (f args) es |>> unzip3
    let x' = encloseSep "{" "}" "," es'
    return (concat mss', x', concat pss)

  f args (TupleM _ es) = do
    (mss', es', pss) <- mapM (f args) es |>> unzip3
    return (concat mss', "std::make_tuple" <> tupled es', concat pss)

  f args (RecordM c entries) = error "C++ records not yet supported"

  f _ (BndVarM c i) = return ([], bndNamer i, [])
  f _ (LetVarM c i) = return ([], letNamer i, [])
  f _ (LogM _ x) = return ([], if x then "true" else "false", [])
  f _ (NumM _ x) = return ([], viaShow x, [])
  f _ (StrM _ x) = return ([], dquotes $ pretty x, [])
  f _ (NullM _) = return ([], "null", [])

  f args (ReturnM e) = do
    (ms, e', ps) <- f args e
    return (ms, "return(" <> e' <> ");", ps)


stdFunction :: TypeM -> [Argument] -> MorlocMonad MDoc
stdFunction t args = 
  let argList = cat (punctuate "," (map argTypeM args))
  in return [idoc|std::function<#{showTypeM t}(#{argList})>|]

stdBind :: [MDoc] -> MDoc
stdBind xs = [idoc|std::bind(#{args})|] where
  args = cat (punctuate "," xs)

staticCast :: TypeM -> [Argument] -> MDoc -> MorlocMonad MDoc
staticCast t args name = do
  let output = showTypeM t
      inputs = map argTypeM args
      argList = cat (punctuate "," inputs)
  return $ [idoc|static_cast<#{output}(*)(#{argList})>(&#{name})|]

argTypeM :: Argument -> MDoc
argTypeM (SerialArgument _ _) = serialType
argTypeM (NativeArgument _ c) = showType c
argTypeM (PassThroughArgument _) = serialType

-- divide a list of arguments based on wheither they are in a second list
splitArgs :: [Argument] -> [Argument] -> ([Argument], [Argument])
splitArgs args1 args2 = partitionEithers $ map split args1 where
  split :: Argument -> Either Argument Argument
  split r = if elem r args2
            then Left r
            else Right r

makeDispatch :: [ExprM One] -> MDoc
makeDispatch ms = block 4 "switch(cmdID)" (vsep (map makeCase ms))
  where
    makeCase :: ExprM One -> MDoc
    makeCase (ManifoldM (metaId->i) args _) =
      let args' = take (length args) $ map (\i -> "argv[" <> viaShow i <> "]") [2..]
      in
        (nest 4 . vsep)
          [ "case" <+> viaShow i <> ":"
          , "result = " <> manNamer i <> tupled args' <> ";"
          , "break;"
          ]

showType :: CType -> MDoc
showType = MTM.buildCType mkfun mkrec where
  mkfun :: MDoc -> [MDoc] -> MDoc
  mkfun _ _ = "FUNCTION_TYPE"

  mkrec :: [(MDoc, MDoc)] -> MDoc
  mkrec _ = "RECORD_TYPE"

showTypeM :: TypeM -> MDoc
showTypeM Passthrough = serialType
showTypeM (Serial t) = serialType
showTypeM (Native t) = showType t
showTypeM (Function ts t) = "std::function<" <> showTypeM t <> "(" <> cat (punctuate "," (map showTypeM ts)) <> ")>"

-- for use in making schema, where the native type is needed
showNativeTypeM :: TypeM -> MorlocMonad MDoc
showNativeTypeM (Serial t) = return $ showType t
showNativeTypeM (Native t) = return $ showType t
showNativeTypeM _ = MM.throwError . OtherError $ "Expected a native or serialized type"

makeMain :: [MDoc] -> [MDoc] -> [MDoc] -> MDoc -> MDoc
makeMain includes signatures manifolds dispatch = [idoc|#include <string>
#include <iostream>
#include <sstream>
#include <functional>

#{Src.foreignCallFunction}

#{Src.serializationHandling}

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
