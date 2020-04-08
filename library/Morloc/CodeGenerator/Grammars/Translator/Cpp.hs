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


translate :: [Source] -> [CallTree] -> MorlocMonad MDoc
translate srcs mss = do 
  let includes = unique . catMaybes . map srcPath $ srcs

  includeDocs <- mapM translateSource includes
  mDocs <- mapM translateManifold (concat [m:ms | (CallTree m ms) <- mss])
  main <- translateMain [m | (CallTree m _) <- mss]
  return . vsep $ includeDocs ++ mDocs ++ [main]

serialType :: MDoc
serialType = "std::string"

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
translateManifold (Manifold v args es) = return $ line <> block 4 head body where
  head = returnType v <+> returnName v <> tupled (map makeArgument args)
  body = "BODY"

makeArgument :: Argument -> MDoc
makeArgument (PackedArgument v c) = serialType <+> pretty v
makeArgument (UnpackedArgument v c) = showType c <+> pretty v
makeArgument (PassThroughArgument v) = serialType <+> pretty v

returnType :: ReturnValue -> MDoc
returnType (PackedReturn _ _) = serialType
returnType (UnpackedReturn _ c) = showType c
returnType (PassThroughReturn _) = serialType

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

-- -- The type schema is simply an initialized variable for the proper type
-- -- This is all that is needed to ensure proper resolution of the C++ templates
-- gTypeSchema' :: CType -> Int -> GeneralAssignment
-- gTypeSchema' c i =
--   GeneralAssignment
--     { gaType = Just (gShowType' c)
--     , gaName = "t" <> pretty i
--     , gaValue = Nothing
--     , gaArg = Nothing
--     }

translateMain :: [Manifold] -> MorlocMonad MDoc
translateMain ms = return [idoc|
int main(int argc, char * argv[])
{
    int cmdID;
    std::string result;
    cmdID = std::stoi(argv[1]);
    #{schema}
    #{dispatch}
    std::cout << result << std::endl;
    return 0;
}
|]
  where
    schema = "SCHEMA" 
    dispatch = "DISPATCH" 
