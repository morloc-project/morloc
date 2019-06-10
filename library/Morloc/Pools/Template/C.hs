{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

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

import Morloc.Global
import Morloc.Manifold as Man
import qualified Morloc.Data.Text as MT
import qualified Morloc.Component.Serializer as Serializer
import qualified Morloc.Component.Manifold as Manifold
import qualified Morloc.Monad as MM
import Morloc.Data.Doc hiding ((<$>))
import Morloc.Quasi
import Morloc.Pools.Common

type CType = Doc
type CVar = Doc
type CVal = Doc
type CExpr = Doc
type CStatement = Doc

-- | this must contain all the information needed to build a C program
data CGlobal = CProg {
    includesC :: [Doc] -- ^ list of include statements
  , globalC :: [Doc] -- ^ any global variables
  , functionsC :: [CFunction] -- ^ list of functions and their type info
  , allocatedC :: [CVar] -- ^ things to free when leaving main
  , mainC :: Doc
}

data CFunction = CFunction {
    cFReturnType :: Doc -- ^ return type
  , cFName :: Doc -- ^ function name
  , cFArgs :: [(CType, CVar)] -- /
  , cFBody :: Doc 
}

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate db = pure Script <*> pure "pool" <*> pure CLang <*> generateC db

generateC :: SparqlDatabaseLike db => db -> MorlocMonad MT.Text
generateC db = do
  manifolds <- Manifold.fromSparqlDb db
  packMap <- Serializer.fromSparqlDb CLang db
  paksrcs <- mapM nameSource (serialSources packMap)
  mansrcs <- Man.getManSrcs CLang nameSource manifolds
  fmap render $ main sources' cismanifolds' switch'
  where
    nameSource :: MT.Text -> MorlocMonad Doc
    nameSource = return . dquotes . text'

initializeC :: CType -> CVar -> CExpr
initializeC t v = t <+> v <> ";"

assign :: Maybe CType -> CVar -> CVal -> Doc
assign Nothing v x = v <+> "=" <+> x <> ";"
assign (Just t) v x = t <+> v <+> "=" <+> x <> ";"

callC :: CVar -> CExpr -> CExpr
callC f x = f <> "(" <> x <> ")"

blockC :: CStatement -> Doc
blockC = braces . nest 2

-- FIXME: this is all a dirty hack. The type strings for a given language MUST
-- NOT be specified in the Haskell code. You should be able to implement
-- handling for a language without having to touch the Haskell core. In the
-- dynamic languages (R and Python), the problem is easier because I don't have
-- to explicitly state the data types.
toCType :: MType -> MorlocMonad Doc
toCType (MConcType MTypeMeta{metaLang = Just CLang} "Double" []) = return "double"
toCType (MConcType MTypeMeta{metaLang = Just CLang} "String" []) = return "char*"
toCType (MConcType MTypeMeta{metaLang = Just CLang} "Int"    []) = return "int"
toCType _ = MM.throwError (TypeError "Cannot cast as a C type")

-- | Generate a switch statement
switchC
  :: CVar
  -- ^ The variable the switch statement dispatches upon
  -> [(CVal, [CStatement])]
  -- ^ Pairs of values and statements to put in the block (@break@ will automatically be added)
  -> [CStatement]
  -- ^ Statements that go in the @default@ block
  -> Doc
switchC x cases defs = callC "switch" x <> blockC caseBlock where
  caseBlock = vsep (map asCase cases) <> line <> def'
  asCase (v, xs) = ("case" <+> v <> ":") <> line <> (nest 2 . vsep $ xs ++ ["break;"])
  def' = "default:" <> line <> nest 2 (vsep defs <> line <> "break;")

-- | Single line comment
commentC :: Doc -> Doc
commentC x = enclose "/*" "*/" x

-- | Multi-line comment
multicommentC :: Doc -> Doc
multicommentC x = enclose "/*" "*/" (nest 2 x)

-- | Create if else
conditionalC :: [(Doc, Doc)] -> Maybe Doc -> Doc
conditionalC [] (Just _) = error "else without if"
conditionalC ((c, b):xs) els = callC "if" c <> blockC b <> conditionalC' xs els where
  conditionalC' [] Nothing = ""
  conditionalC' [] (Just x) = "else" <> blockC x
  conditionalC' ((c', b'):xs) els
    =   callC "else if" c' <> blockC b' 
    <> line <> conditionalC' xs els

-- | Create the prototype of a function
prototypeC :: CFunction -> Doc
prototypeC r =  (cFReturnType r) <+> (cFName r)
             <> encloseSep "(" ")" "," (map (\(t, v) -> t <+> v) (cFArgs r))

-- | Create a function
functionC :: CFunction -> Doc
functionC r = prototypeC r <> enclose "{" "}" (indent 2 (cFBody r))

switch' = [idoc| 
    switch(mid){
        case 1:
            char* json = packDouble(m0(unpackDouble(argv[2])));
            break;
        case 2:
            char* json = packDouble(m1(unpackDouble(argv[2])));
            break;
        case 3:
            char* json = packDouble(m2(unpackDouble(argv[2])));
            break;
        default:
            break;
    }
|]

cismanifolds' = [idoc|
double m0(double x){
  return sin(x)
}

double m1(double x){
  return cos(x)
}

double m2(double x){
  return tan(x)
}
|]

sources' = [idoc|
#include "/home/z/.morloc/lib/math/c_math.h"

#include "/home/z/.morloc/lib/cbase/cbase.h"
|]

main :: Doc -> Doc -> Doc -> MorlocMonad Doc
main sources cismanifolds switch = do
  return [idoc|
#include <string.h>

#include <stdio.h>

#{sources}

#{cismanifolds}

int main(int argc, char * argv[]){
    int mid = atoi(argv[1]);

    #{switch}

    printf("%s\n", json);

    return 0;
}
|]
