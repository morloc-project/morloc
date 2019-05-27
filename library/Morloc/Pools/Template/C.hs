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

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate _ = Script <$> pure "pool" <*> pure "c" <*> pure stubCode

stubCode :: MT.Text
stubCode = render [idoc|
#include <string.h>

#include <stdio.h>

#include "/home/z/.morloc/lib/math/c_math.h"

#include "/home/z/.morloc/lib/cbase/cbase.h"

int main(int argc, char * argv[]){
    char* json = (char*)malloc(50 * sizeof(char));
    if(strcmp(argv[1], "sin") == 0){
        strcpy(json, packDouble(sin(unpackDouble(argv[2]))));
    } else if(strcmp(argv[1], "cos") == 0){
        strcpy(json, packDouble(cos(unpackDouble(argv[2]))));
    } else if(strcmp(argv[1], "tan") == 0){
        strcpy(json, packDouble(tan(unpackDouble(argv[2]))));
    } else {
        strcpy(json, "other");
    }
    printf("%s\n", json);
    return 0;
}
|]
