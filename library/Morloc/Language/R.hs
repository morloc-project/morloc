{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.R
Description : R language generation
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Language.R (generate) where

import Morloc.Quasi
import Morloc.Types
import qualified Morloc.Vortex as V
import qualified Morloc.System as MS
import qualified Morloc.Util as MU
import qualified Morloc.Query as Q

import qualified Data.Text as DT 
import qualified Data.List as DL
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "pool"
  <*> pure "R"
  <*> generateCode e

generateCode :: SparqlEndPoint -> IO DT.Text
generateCode e = do
  manifolds <- V.buildManifolds e
  packHash <- V.buildPackHash e
  let srcs = map text' (V.sources packHash)
  (return . render) $ main srcs [] []

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate ", "

nameArgs :: [a] -> [Doc]
nameArgs xs = map ((<>) "x") (map int [1 .. length xs])

iArgs :: Int -> [Doc]
iArgs i = map ((<>) "x") (map int [1 .. i])

main
  :: [Doc] 
  -- ^ Pass to retrieve sources
  -> [((Doc, Doc, Doc, Doc), [Maybe (Doc, Doc)])]
  -- ^ Pass to exported functions
  -> [(Doc, Doc, Int, Maybe (Doc, Doc))]
  -- ^ Pass to cis functions
  -> Doc
main srcs exps ciss = [idoc|#!/usr/bin/env Rscript

${vsep (map sourceT srcs)}

# ------------------
# Exported functions
# ------------------

${vsep (map exportedT exps)}

# --------------
# Internal calls
# --------------

${vsep (map cisT ciss)}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else if(exists(args[[1]])){
  x <- get(args[[1]])
  result <- if(class(x) == "function"){
    do.call(get(args[[1]]), as.list(args[-1, drop=FALSE]))
  } else {
    x
  }
  cat(packGeneric(result), "\n")
} else {
  stop("Could not find function '", args[[1]], "'")
}
|]

sourceT s = [idoc|source("${s}")|]

exportedT ((alias, fname, uid, generic), args) = [idoc|
# ${alias}

${uid} <- function(${commaSep (nameArgs args)}){
  ${fname}(${commaSep (castArgs (generic, args))})
}
|]
  where
    castArgs :: (Doc, [Maybe (Doc, Doc)]) -> [Doc]
    castArgs (generic, xss) = zipWith
      (\w i -> [idoc|${w}(x${i})|])
      (map (wrapper generic) xss)
      (map int [1..])

    wrapper :: Doc -> (Maybe (Doc, Doc)) -> Doc
    wrapper _ (Just (specific, _)) = specific
    wrapper generic _ = generic


cisT (callid, fid, nargs, Just (falias, fname)) = [idoc|
# ${falias}

${callid} <- function(${commaSep (iArgs nargs)}){
  ${fname}(${commaSep (iArgs nargs)})
}
|]

cisT (callid, fid, nargs, Nothing) = [idoc|
# ${callid}

${callid} <- function(${commaSep (iArgs nargs)}){
  ${fid}(${commaSep (iArgs nargs)})
}
|]
