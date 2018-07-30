{-# LANGUAGE OverloadedStrings #-}

module Morloc.Language (
    CodeGenerator(..)
  , rCodeGenerator
) where

import qualified Data.Text as DT

import qualified Morloc.Util as U

data CodeGenerator = CodeGenerator {
      makePool
        :: [DT.Text] -- any required global declarations
        -> [DT.Text] -- any input source code
        -> [DT.Text] -- the node function declarations
        -> DT.Text   -- entire pool script
  }

rCodeGenerator :: CodeGenerator
rCodeGenerator = CodeGenerator {
    makePool = makePool'
  }
  where

    makePool' :: [DT.Text] -> [DT.Text] -> [DT.Text] -> DT.Text
    makePool' gs is fs = DT.unlines . concat $ [begin', gs, is, fs, end']

    begin' = ["#!/usr/bin/env Rscript"]

    end'   =
      [ "args <- commandArgs(trailingOnly=TRUE)"
      , "if(length(args) == 0){"
      , "  stop(\"Expected 1 or more arguments\")"
      , "} else if(exists(args[[1]])){"
      , "  x <- get(args[[1]])"
      , "  result <- if(class(x) == \"function\"){"
      , "    par <- lapply(args[-1], function(s) eval(parse(text=s)))"
      , "    do.call(get(args[[1]]), par)"
      , "  } else {"
      , "    x"
      , "  }"
      , "  cat(result, \"\\n\")"
      , "} else {"
      , "  stop(\"Could not find function '\", f, \"'\")"
      , "}"
      ]
