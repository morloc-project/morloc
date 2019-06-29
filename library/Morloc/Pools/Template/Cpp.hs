{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

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

import Morloc.Global
import Morloc.Manifold as Man
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import Morloc.Data.Doc hiding ((<$>))
import Morloc.Quasi
import Morloc.Pools.Common
import qualified Data.Map.Strict as Map

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
wrapIncludeString
  :: Monad m
  => MT.Text -- ^ Path to a header (e.g., `$MORLOC_HOME/lib/foo.h`)
  -> m Doc
wrapIncludeString = return . dquotes . text'

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

fromMaybeType :: Maybe Doc -> Doc
fromMaybeType = maybe "void*" id

gLang' :: Lang
gLang' = CppLang

gSerialType' :: MType
gSerialType' = MConcType (MTypeMeta Nothing [] Nothing) "char*" []

gAssign' :: GeneralAssignment -> Doc
gAssign' ga = case gaType ga of
  Nothing -> gaName ga <+> "=" <+> gaValue ga <> ";" 
  (Just t) -> t <+> gaName ga <+> "=" <+> gaValue ga <> ";"

gCall' :: Doc -> [Doc] -> Doc
gCall' f args = f <> tupled args

gFunction' :: GeneralFunction -> Doc
gFunction' gf = comments <> head <> braces (line <> gIndent' (gfBody gf) <> line) where
  targs = tupled (map (\(t, x) -> (fromMaybeType t) <+> x) (gfArgs gf))
  rargs = tupled (map snd (gfArgs gf))
  head = (fromMaybeType (gfReturnType gf)) <+> gfName gf <> targs
  comments = gfComments gf

gSignature' :: GeneralFunction -> Doc
gSignature' gf =  (fromMaybeType (gfReturnType gf)) <+> (gfName gf)
               <> encloseSep "(" ")" "," (map (\(t, v) -> (fromMaybeType t) <+> v) (gfArgs gf))
               <> ";"

gId2Function' :: Integer -> Doc
gId2Function' i = "m" <> integer i

gComment' :: Doc -> Doc
gComment' d = "/* " <> d <> " */"

gReturn' :: Doc -> Doc
gReturn' x = "return" <+> x <> ";"

gQuote' :: Doc -> Doc
gQuote' = dquotes

-- | The first argment is the directory, this is added later?
gImport' :: Doc -> Doc -> Doc
gImport' _ s = "#include" <+> s 

gList' :: [Doc] -> Doc
gList' _ = undefined

gTuple' :: [Doc] -> Doc
gTuple' _ = undefined

gRecord' :: [(Doc,Doc)] -> Doc
gRecord' _ = undefined 

gTrue' :: Doc
gTrue' = integer 1

gFalse' :: Doc
gFalse' = integer 0

gIndent' :: Doc -> Doc
gIndent' = indent 4

gUnpacker' :: UnpackerDoc -> Doc
gUnpacker' ud = gCall' (udUnpacker ud) [udValue ud] 

-- There is no try, only do
gTry' :: TryDoc -> Doc
gTry' td = gCall' (tryCmd td) (tryArgs td) 

-- "foreign_call" is defined in "cbase.h"
gForeignCall' :: ForeignCallDoc -> Doc
gForeignCall' fc = gCall' "foreign_call" (fcdCall fc ++ fcdArgs fc)

gSwitch' :: (Manifold -> Doc) -> (Manifold -> Doc) -> [Manifold] -> Doc -> Doc -> Doc
gSwitch' l r ms x var = switchC x (map (\m -> (l m, r m)) ms)
  where
    switchC x cases = gCall' "switch" [x] <> blockC caseBlock where
      caseBlock = vsep (map asCase cases) <> line
      asCase (v, body) = ("case" <+> v <> ":") <> line <> (indent 2 $ caseC body)

    blockC :: Doc -> Doc
    blockC x = "{" <> line <> "  " <> indent 2 x <> line <> "}"

    caseC :: Doc -> Doc
    caseC body = var <> " = " <> body <> ";" <> line <> "break;"

gCmdArgs' :: [Doc]
gCmdArgs' = map (\i -> "argv[" <> integer i <> "]") [2..]

gShowType' :: MType -> Doc
gShowType' = mshow

gMain' :: PoolMain -> MorlocMonad Doc
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
