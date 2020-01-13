{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Template.C
Description : Build a C program given a file
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : totally experimental

The build process for C differs from that used in R and python since a
compilation step is needed. This code currently is wildly experimental.
-}

module Morloc.CodeGenerator.Grammars.Template.C
(
  grammar
) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import qualified Morloc.TypeChecker.Macro as MTM

grammar = Grammar {
      gLang        = gLang'
    , gSerialType  = gSerialType'
    , gAssign      = gAssign'
    , gCall        = gCall'
    , gFunction    = gFunction'
    , gId2Function = gId2Function'
    , gCurry       = gCurry'
    , gComment     = gComment'
    , gReturn      = gReturn'
    , gQuote       = gQuote'
    , gImport      = gImport'
    , gPrepImport  = gPrepImport'
    , gNull        = gNull'
    , gBool        = gBool'
    , gReal        = viaShow
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

gSerialType' :: Type
gSerialType' = VarT (TV (Just CLang) "char*")

gAssign' :: GeneralAssignment -> MDoc
gAssign' ga = case gaType ga of
  Nothing -> gaName ga <+> "=" <+> gaValue ga <> ";"
  (Just t) -> t <+> gaName ga <+> "=" <+> gaValue ga <> ";"

gCall' :: MDoc -> [MDoc] -> MDoc
gCall' f args = f <> tupled args

gFunction' :: GeneralFunction -> MDoc
gFunction' gf
  = gComment' (gfComments gf)
  <> head' <> braces (line <> gIndent' (gfBody gf) <> line)
  where
    targs = tupled (map (\(t, x) -> (fromMaybeType t) <+> x) (gfArgs gf))
    -- FIXME: do I really not need this?
    -- rargs = tupled (map snd (gfArgs gf))
    head' = (fromMaybeType (gfReturnType gf)) <+> gfName gf <> targs
    comments = maybe "" 

gSignature' :: GeneralFunction -> MDoc
gSignature' gf =  (fromMaybeType (gfReturnType gf)) <+> (gfName gf)
               <> encloseSep "(" ")" "," (map (\(t, v) -> (fromMaybeType t) <+> v) (gfArgs gf))
               <> ";"

gId2Function' :: Integer -> MDoc
gId2Function' i = "m" <> pretty i

gCurry' :: MDoc -> [MDoc] -> Int -> MDoc
gCurry' _ _ _ = error "Currying is not supported in C"

gComment' :: [MDoc] -> MDoc
gComment' ds = "/* " <> vsep ds <> " */" <> line

gReturn' :: MDoc -> MDoc
gReturn' x = "return" <+> x <> ";"

gQuote' :: MDoc -> MDoc
gQuote' = dquotes

-- | The first argment is the directory, this is added later?
gImport' :: MDoc -> MDoc -> MDoc
gImport' _ s = "#include" <+> s

-- See comments above the homologous Cpp.hs function
gPrepImport'
  :: Path -- ^ Path to a header (e.g., `$MORLOC_HOME/lib/foo.h`)
  -> MorlocMonad MDoc
gPrepImport' = return . dquotes . pretty . MS.takeFileName

gList' :: [MDoc] -> MDoc
gList' _ = undefined

gTuple' :: [MDoc] -> MDoc
gTuple' _ = undefined

gRecord' :: [(MDoc,MDoc)] -> MDoc
gRecord' _ = undefined

gNull' :: MDoc
gNull' = "NULL"

gBool' :: Bool -> MDoc
gBool' x = if x then integer 1 else integer 0

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

gSwitch' :: (a -> MDoc) -> (a -> MDoc) -> [a] -> MDoc -> MDoc -> MDoc
gSwitch' l r ms x var = switchC x (map (\m -> (l m, r m)) ms)
  where
    switchC i cases = block 2 (gCall' "switch" [i]) caseBlock where
      caseBlock = vsep (map asCase cases) <> line
      asCase (v, body) = ("case" <+> v <> ":") <> line <> (indent 2 $ caseC body)

    caseC :: MDoc -> MDoc
    caseC body = var <> " = " <> body <> ";" <> line <> "break;"

gCmdArgs' :: [MDoc]
gCmdArgs' = map (\i -> "argv[" <> integer i <> "]") [2..]

gShowType' :: Type -> MDoc
gShowType' = MTM.buildConcreteType mkfun mkrec where
  mkfun :: MDoc -> [MDoc] -> MDoc 
  mkfun _ _ = "FUNCTION!!!" -- FIXME: stub

  mkrec :: [(MDoc, MDoc)] -> MDoc
  mkrec _ = "RECORD!!!" -- FIXME: stub

gMain' :: PoolMain -> MorlocMonad MDoc
gMain' pm = return [idoc|#include <string.h>

#include <stdio.h>

#{vsep (pmSources pm)}

#{vsep (pmSignatures pm)}

#{vsep (pmPoolManifolds pm)}

int main(int argc, char * argv[]){
  int cmdID;
  char* result;
  cmdID = atoi(argv[1]);
  #{(pmDispatchManifold pm) "cmdID" "result"}
  printf("%s\n", result);
  return 0;
}
|]
