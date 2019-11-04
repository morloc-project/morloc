{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : C
Description : Build a Cpp program given a file
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pools.Template.Cpp
( 
  generate
) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.Pools.Common
import Morloc.Quasi
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import qualified Morloc.TypeChecker.Macro as MTM

generate :: [Manifold] -> SerialMap -> MorlocMonad Script
generate = defaultCodeGenerator g wrapIncludeString

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
wrapIncludeString
  :: Monad m
  => MT.Text -- ^ Path to a header (e.g., `$MORLOC_HOME/lib/foo.h`)
  -> m MDoc
wrapIncludeString = return . dquotes . pretty . MS.takeFileName

g = Grammar {
      gLang        = gLang'
    , gSerialType  = gSerialType'
    , gAssign      = gAssign'
    , gCall        = gCall'
    , gFunction    = gFunction'
    , gId2Function = gId2Function'
    , gComment     = gComment'
    , gReturn      = gReturn'
    , gQuote       = gQuote'
    , gImport      = gImport'
    , gTrue        = gTrue'
    , gFalse       = gFalse'
    , gList        = gList'
    , gTuple       = gTuple'
    , gRecord      = gRecord'
    , gIndent      = gIndent'
    , gTry         = gTry'
    , gUnpacker    = gUnpacker'
    , gForeignCall = gForeignCall'
    , gSignature   = gSignature'
    , gSwitch      = gSwitch'
    , gCmdArgs     = gCmdArgs'
    , gShowType    = gShowType'
    , gMain        = gMain'
  }

fromMaybeType :: Maybe MDoc -> MDoc
fromMaybeType = maybe "void*" id

gLang' :: Lang
gLang' = CppLang

gSerialType' :: MType
gSerialType' = MConcType (MTypeMeta Nothing [] Nothing) "std::string" []

gAssign' :: GeneralAssignment -> MDoc
gAssign' ga = case (gaArg ga, gaType ga) of
  -- need to call constructors
  (Just (ArgData (Lst' _)), Just t) -> t <+> gaName ga <> gaValue ga <> ";"
  (Just (ArgData (Tup' _)), Just t) -> t <+> gaName ga <> gaValue ga <> ";"
  (Just (ArgData (Rec' _)), Just _) -> undefined
  -- simple '=' assignment
  (_, Nothing) -> gaName ga <+> "=" <+> gaValue ga <> ";" 
  (_, Just t) -> t <+> gaName ga <+> "=" <+> gaValue ga <> ";"

gCall' :: MDoc -> [MDoc] -> MDoc
gCall' f args = f <> tupled args

gFunction' :: GeneralFunction -> MDoc
gFunction' gf = comments <> head' <> braces (line <> gIndent' (gfBody gf) <> line) where
  targs = tupled (map (\(t, x) -> (fromMaybeType t) <+> x) (gfArgs gf))
  -- -- do I not need this?
  -- rargs = tupled (map snd (gfArgs gf))
  head' = (fromMaybeType (gfReturnType gf)) <+> gfName gf <> targs
  comments = gfComments gf

gSignature' :: GeneralFunction -> MDoc
gSignature' gf =  (fromMaybeType (gfReturnType gf)) <+> (gfName gf)
               <> encloseSep "(" ")" "," (map (\(t, v) -> (fromMaybeType t) <+> v) (gfArgs gf))
               <> ";"

gId2Function' :: Integer -> MDoc
gId2Function' i = "m" <> integer i

gComment' :: MDoc -> MDoc
gComment' d = "/* " <> d <> " */"

gReturn' :: MDoc -> MDoc
gReturn' x = "return" <+> x <> ";"

gQuote' :: MDoc -> MDoc
gQuote' = dquotes

-- | The first argment is the directory, this is added later?
gImport' :: MDoc -> MDoc -> MDoc
gImport' _ s = "#include" <+> s 

gList' :: [MDoc] -> MDoc
gList' xs = encloseSep "{" "}" "," xs

gTuple' :: [MDoc] -> MDoc
gTuple' xs = encloseSep "{" "}" "," xs

gRecord' :: [(MDoc, MDoc)] -> MDoc
gRecord' _ = undefined 

gTrue' :: MDoc
gTrue' = integer 1

gFalse' :: MDoc
gFalse' = integer 0

gIndent' :: MDoc -> MDoc
gIndent' = indent 4

gUnpacker' :: UnpackerDoc -> MDoc
gUnpacker' ud = gCall' (udUnpacker ud) [udValue ud] 

-- There is no try, only do
gTry' :: TryDoc -> MDoc
gTry' td = gCall' (tryCmd td) (tryArgs td) 

-- "foreign_call" is defined in "cbase.h"
gForeignCall' :: ForeignCallDoc -> MDoc
gForeignCall' fc = gCall' "foreign_call" [hsep $ punctuate " + " (joinStr (fcdCall fc) : fcdArgs fc)]
  where
    joinStr xs
      = "\"" <> (hsep $ map (pretty . MT.undquote . render) xs) <+> "\""

gSwitch' :: (Manifold -> MDoc) -> (Manifold -> MDoc) -> [Manifold] -> MDoc -> MDoc -> MDoc
gSwitch' l r ms x var = switchC x (map (\m -> (l m, r m)) ms)
  where
    switchC i cases = gCall' "switch" [i] <> blockC caseBlock where
      caseBlock = vsep (map asCase cases) <> line
      asCase (v, body) = ("case" <+> v <> ":") <> line <> (indent 2 $ caseC body)

    blockC :: MDoc -> MDoc
    blockC x = "{" <> line <> "  " <> indent 2 x <> line <> "}"

    caseC :: MDoc -> MDoc
    caseC body = var <> " = " <> body <> ";" <> line <> "break;"

gCmdArgs' :: [MDoc]
gCmdArgs' = map (\i -> "argv[" <> integer i <> "]") [2..]

gShowType' :: MType -> MDoc
gShowType' t = pretty $ MTM.showMType f t
  where
    f :: [Name] -> Name -> Name
    f inputs output = render $ pretty output <> "(*f)" <> tupled (map pretty inputs)  

gMain' :: PoolMain -> MorlocMonad MDoc
gMain' pm = return [idoc|#include <string>

#include <iostream>

#{vsep (pmSources pm)}

#{vsep $ map (gSignature g) (pmPoolManifolds pm)}

#{vsep $ map (gFunction g) (pmPoolManifolds pm)}

int main(int argc, char * argv[]){
  int cmdID;
  std::string result;
  cmdID = std::stoi(argv[1]);
  #{(pmDispatchManifold pm) "cmdID" "result"}
  std::cout << result << std::endl;
  return 0;
}
|]
