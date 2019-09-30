{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Nexus.Template.TerminalNexus
Description : A manifold nexus for the command line
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus.Template.TerminalNexus (generate) where

import Morloc.TypeChecker.API
import Morloc.Global
import Morloc.Quasi
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT

generate :: [Module] -> MorlocMonad Script 
generate mods = return $ Script {
    scriptBase = "voodoo"
  , scriptLang = CLang
  , scriptCode = render $ makeBody mods
  , scriptCompilerFlags = []
  }


  -- map () (concat . map moduleExports) (rootModules ms)

makeBody :: [Module] -> MDoc
makeBody _ = [idoc|// CLI manifold nexus
#include <string.h>
#include <stdio.h>

void usage () {
    printf("yolo\n"); 
}

int main(int argc, char* argv[]){
    if(argc == 1){
        usage();
        return 1;
    }

    if(strcmp(argv[1], "foo") == 0){
        printf("foo ");
        for(int i = 2; i < argc; i++){
            printf(argv[i]);
            printf(" ");
        }
        printf("\n");
        return 0;
    }

    printf("Unexpected argument, choose a command from the list below:\n");
    usage();
    return 1;
}
|]
