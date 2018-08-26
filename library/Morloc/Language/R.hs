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
import qualified Morloc.System as MS
import qualified Morloc.Util as MU
import qualified Morloc.Query as Q

import qualified Data.Text as DT 
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "pool"
  <*> pure "R"
  <*> generateCode e

expsQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
expsQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?fname ?typedec (count (distinct ?element) as ?nargs)
  WHERE {
    ?typedec rdf:type mlc:typeDeclaration ;
       mlc:lang "Morloc" ;
       mlc:lhs ?fname ;
       mlc:rhs ?type .

    ?type rdf:type mlc:functionType .

    ?arg ?element ?type .
    FILTER(regex(str(?element), "_[0-9]+$", "i"))

    ?src rdf:type mlc:source ;
         mlc:lang "R" ;
         mlc:import ?i .

    ?i mlc:alias ?fname .

    ?e rdf:type mlc:export ;
       rdf:value ?fname .
  }
  GROUP BY ?typedec ?fname
|]

generateCode :: SparqlEndPoint -> IO DT.Text
generateCode e = fmap render (main <$> srcs' <*> exps')
  where
    srcs' :: IO [Doc]
    srcs' = fmap (map toOne) (Q.sourcesByLangQ (dquotes "R") e)

    toOne :: [Maybe DT.Text] -> Doc
    toOne [Just x] = text' x
    toOne _ = error "Bad SPARQL return"

    exps' :: IO [(Doc, Doc, [Doc])]
    exps' = fmap (map toExps) (expsQ e)

    toExps :: [Maybe DT.Text] -> (Doc, Doc, [Doc])
    toExps [Just fname, Just uid, Just nargs]
      = ( text' fname
        , text' (MS.makeManifoldName uid)
        , map (<> "x") (map int [1 .. (read (DT.unpack nargs) :: Int)])
        )


main :: [Doc] -> [(Doc, Doc, [Doc])] -> Doc
main srcs exps = [idoc|#!/usr/bin/env Rscript

${vsep (map sourceT srcs)}

${vsep (map exportedT exps)}

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
  cat(packGeneric(result))
} else {
  stop("Could not find function '", args[[1]], "'")
}
|]

sourceT s = [idoc|source("${s}")|]


exportedT (fname, uid, args) = [idoc|
# ${fname}
m${uid} <- function(${hcat (punctuate ", " args)}){
  mXX(${hcat (punctuate ", " args)})
}
|]
