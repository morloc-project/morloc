{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : C
Description : Build a C program given a file
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : totally experimental

The build process for C differs from that used in R and python since a
compilation step is needed. This code currently is wildly experimental.
-}

module Morloc.Pools.Template.C
( 
  generate
) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import Morloc.Data.Doc
import Morloc.Quasi
import Morloc.Pools.Common

generate :: [Manifold] -> SerialMap -> MorlocMonad Script
generate = defaultCodeGenerator g wrapIncludeString

-- See comments above the homologous Cpp.hs function
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
gLang' = CLang

gSerialType' :: MType
gSerialType' = MConcType (MTypeMeta Nothing [] Nothing) "char*" []

gAssign' :: GeneralAssignment -> MDoc
gAssign' ga = case gaType ga of
  Nothing -> gaName ga <+> "=" <+> gaValue ga <> ";" 
  (Just t) -> t <+> gaName ga <+> "=" <+> gaValue ga <> ";"

gCall' :: MDoc -> [MDoc] -> MDoc
gCall' f args = f <> tupled args

gFunction' :: GeneralFunction -> MDoc
gFunction' gf = comments <> head <> braces (line <> gIndent' (gfBody gf) <> line) where
  targs = tupled (map (\(t, x) -> (fromMaybeType t) <+> x) (gfArgs gf))
  rargs = tupled (map snd (gfArgs gf))
  head = (fromMaybeType (gfReturnType gf)) <+> gfName gf <> targs
  comments = gfComments gf

gSignature' :: GeneralFunction -> MDoc
gSignature' gf =  (fromMaybeType (gfReturnType gf)) <+> (gfName gf)
               <> encloseSep "(" ")" "," (map (\(t, v) -> (fromMaybeType t) <+> v) (gfArgs gf))
               <> ";"

gId2Function' :: Integer -> MDoc
gId2Function' i = "m" <> pretty i

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
gList' _ = undefined

gTuple' :: [MDoc] -> MDoc
gTuple' _ = undefined

gRecord' :: [(MDoc,MDoc)] -> MDoc
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
gForeignCall' fc = gCall' "foreign_call" (fcdCall fc ++ fcdArgs fc)

gSwitch' :: (Manifold -> MDoc) -> (Manifold -> MDoc) -> [Manifold] -> MDoc -> MDoc -> MDoc
gSwitch' l r ms x var = switchC x (map (\m -> (l m, r m)) ms)
  where
    switchC x cases = gCall' "switch" [x] <> blockC caseBlock where
      caseBlock = vsep (map asCase cases) <> line
      asCase (v, body) = ("case" <+> v <> ":") <> line <> (indent 2 $ caseC body)

    blockC :: MDoc -> MDoc
    blockC x = "{" <> line <> "  " <> indent 2 x <> line <> "}"

    caseC :: MDoc -> MDoc
    caseC body = var <> " = " <> body <> ";" <> line <> "break;"

gCmdArgs' :: [MDoc]
gCmdArgs' = map (\i -> "argv[" <> integer i <> "]") [2..]

gShowType' :: MType -> MDoc
gShowType' = pretty 

gMain' :: PoolMain -> MorlocMonad MDoc
gMain' pm = return [idoc|#include <string.h>

#include <stdio.h>

#{vsep (pmSources pm)}

#{vsep $ map (gSignature g) (pmPoolManifolds pm)}

#{vsep $ map (gFunction g) (pmPoolManifolds pm)}

int main(int argc, char * argv[]){
  int cmdID;
  char* result;
  cmdID = atoi(argv[1]);
  #{(pmDispatchManifold pm) "cmdID" "result"}
  printf("%s\n", result);
  return 0;
}
|]
