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
import qualified Morloc.Data.Text as MT
import Morloc.Data.Doc hiding ((<$>))
import Morloc.Quasi
import Morloc.Pools.Common

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate db = makeGenerator g (defaultCodeGenerator g asImport) db

asImport :: MT.Text -> MorlocMonad Doc
asImport s = return . text' $ s

-- generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
-- generate _ = Script <$> pure "pool" <*> pure CLang <*> pure stubCode

g = Grammar {
      gLang        = CLang
    , gAssign      = assign' 
    , gCall        = call'
    , gFunction    = function'
    , gId2Function = id2function'
    , gComment     = comment'
    , gReturn      = return'
    , gQuote       = dquotes
    , gImport      = include'
    , gTrue        = "1" 
    , gFalse       = "0"
    , gList        = array'
    , gTuple       = array'
    , gRecord      = struct'
    , gIndent      = indent'
    , gTry         = tryCmd -- do or do not, there is no try
    , gUnpacker    = unpacker'
    , gForeignCall = foreignCall'
    , gHash        = hash'
    , gMain        = main'
  } where

    indent' = indent 4

    -- FIXME: need to specify type
    -- FIXME: l is dependent on r, e.g.:
    --   int xs[5] = {1,2,3,4,5};
    -- FIXME: memory management
    assign' :: Doc -> Doc -> Doc
    assign' l r = l <> " = " <> r

    call' :: Doc -> [Doc] -> Doc
    call' f args = f <> tupled args

    -- FIXME: need to specity return type
    -- FIXME: need to specify each argument type
    function' :: Doc -> [Doc] -> Doc -> Doc
    function' name args body
      = "<type> " <> name <> tupled args <> braces (line <> indent' body <> line)

    id2function' :: Integer -> Doc
    id2function' i = "m" <> (text' (MT.show' i))

    comment' :: Doc -> Doc
    comment' d = "// " <> d

    return' :: Doc -> Doc
    return' x = call' "return" [x]

    -- FIXME: needs to allocate memory
    -- FIXME: needs type
    array' :: [Doc] -> Doc
    array' xs = encloseSep "{" "}" ", " xs 

    -- FIXME: needs declaration
    -- FIXME: needs memory management
    -- FIXME: needs type for assignment
    struct' :: [(Doc,Doc)] -> Doc
    struct' xs = encloseSep "{" "}" ", " (map (\(k,v) -> "." <> k <> "=" <> v) xs)

    -- FIXME: distinguish between `<lib.h>` and `"lib.h"` imports
    include' :: Doc -> Doc -> Doc
    include' _ s = "#include " <> dquotes s

    -- FIXME: add error handling
    unpacker' :: UnpackerDoc -> Doc
    unpacker' u = call' (udUnpacker u) [udValue u]

    -- FIXME: implement `_morloc_foreign_call`
    foreignCall' :: ForeignCallDoc -> Doc
    foreignCall' f = call' "_morloc_foreign_call" (map dquotes (fcdCliArgs f))

    -- FIXME: I am not sure we even need this in C, the purpose for which it is
    -- used in R/Python is served in C with a switch statement.
    hash' :: (Manifold -> Doc) -> (Manifold -> Doc) -> [Manifold] -> Doc
    hash' _ _ _ = ""

    main' :: [Doc] -> Doc -> Doc -> Doc -> Doc -> MorlocMonad Doc
    main' _ _ _ _ _ = return $ [idoc|

#include <string.h>

#include <stdio.h>

#include "/home/z/.morloc/lib/math/c_math.h"

#include "/home/z/.morloc/lib/cbase/cbase.h"

int main(int argc, char * argv[]){
    char* json = (char*)malloc(50 * sizeof(char));

    int mid = atoi(argv[1]);

    switch(mid){
        case 1:
            strcpy(json, packDouble(sin(unpackDouble(argv[2]))));
            break;
        case 2:
            strcpy(json, packDouble(cos(unpackDouble(argv[2]))));
            break;
        case 3:
            strcpy(json, packDouble(tan(unpackDouble(argv[2]))));
            break;
        default:
            break;
    }
    printf("%s\n", json);
    return 0;
}
|]
