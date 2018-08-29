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
import qualified Data.List as DL
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
SELECT ?alias ?fname ?typedec ?generic ?element ?unpackerName ?langtype
WHERE {
  # ---- find Morloc function ------------------------------
  ?typedec rdf:type mlc:typeDeclaration ;
     mlc:lang "Morloc" ;
     mlc:lhs ?alias ;
     mlc:rhs ?type .
  ?type rdf:type mlc:functionType .
  ?arg ?element ?type .
  FILTER(regex(str(?element), "_[0-9]+$", "i"))
  ?e rdf:type mlc:export ;
     rdf:value ?alias .
  # ---- get source name, if this was a sourced file
  OPTIONAL {
    ?src rdf:type mlc:source ;
         mlc:lang "R" ;
         mlc:import ?i .
    ?i mlc:alias ?alias ;
       mlc:name ?fname .
  }
  # ---- find generic unpacker -----------------------------
  ?genericDec rdf:type mlc:typeDeclaration ;
              mlc:lang "R" ;
              mlc:lhs ?generic ;
              mlc:rhs ?genericFunc .
  ?genericFunc rdf:type mlc:functionType ;
               mlc:output ?genericOut ;
               mlc:property ?genericProperty .
  ?genericProperty rdf:type mlc:name ;
                   rdf:value "unpacks" .
  ?genericOut rdf:type mlc:atomicGeneric .
  ?genericIn rdf:_0 ?genericFunc ;
             rdf:type mlc:atomicType ;
             rdf:value "JSON" .
  # ---- find optional specific unpacker -------------------
  OPTIONAL {
   ?langdec rdf:type mlc:typeDeclaration ;
            mlc:lang "R" ;
            mlc:lhs ?alias ;
            mlc:rhs ?langfunc .
   ?langfunc rdf:type mlc:functionType .
   ?ltype ?element ?langfunc ;
          rdf:type mlc:atomicType ;
          rdf:value ?langtype .
   ?unpackDec rdf:type mlc:typeDeclaration ;
              mlc:lang "R" ;
              mlc:lhs ?unpackerName ;
              mlc:rhs ?unpacker .
   ?unpacker rdf:type mlc:functionType ;
             mlc:property ?property ;
             mlc:output ?packerOutput .
   ?property rdf:type mlc:name ;
             rdf:value "unpacks" .
   ?packerInput rdf:_0 ?unpacker ;
                rdf:type mlc:atomicType ;
                rdf:value "JSON" .
   ?packerOutput rdf:type mlc:atomicType ;
                 rdf:value ?langtype .
  }
}
|]

generateCode :: SparqlEndPoint -> IO DT.Text
generateCode e = fmap render (main <$> srcs' <*> exps')
  where
    srcs' :: IO [Doc]
    srcs' = fmap (map toOne) (Q.sourcesByLangQ (dquotes "R") e)

    toOne :: [Maybe DT.Text] -> Doc
    toOne [Just x] = text' x
    toOne _ = error "Bad SPARQL return"

    -- FIXME: damn the following abomination
    exps' :: IO [((Doc, Doc, Doc, Doc), [Maybe (Doc, Doc)])]
    exps' = fmap toExps (expsQ e)

    toExps :: [[Maybe DT.Text]] -> [((Doc, Doc, Doc, Doc), [Maybe (Doc, Doc)])]
    toExps xss
      = map (\(x, ys) -> (x, map genericGrp ys))
      . map (\((w,x,y,z), xs) -> ((text' w, x, y, z),xs)) 
      . map concatA
      . DL.groupBy (\((x,_,_,_),_) ((y,_,_,_),_) -> x == y)
      . DL.sortBy (\((x,_,_,_),(i,_,_)) ((y,_,_,_),(j,_,_)) -> compare (x,i) (y,j))
      . map toExp
      $ xss

    concatA :: [(a, b)] -> (a, [b])
    concatA [] = error "There must be at least 1 element"
    concatA ((x,y):xs) = (x, y:(map snd xs))

    genericGrp :: (DT.Text, Maybe Doc, Maybe Doc) -> Maybe (Doc, Doc)
    genericGrp (_, Just x, Just y) = Just (x,y)
    genericGrp _ = Nothing

    toExp :: [Maybe DT.Text] -> ((DT.Text, Doc, Doc, Doc), (DT.Text, Maybe Doc, Maybe Doc))
    toExp [Just alias, fname, Just typedec, Just generic, Just el, unpacker, ltype] =
      ( ( alias
        , text' (maybe alias id fname)
        , text' (MS.makeManifoldName typedec)
        , text' generic
        )
      , ( el
        , fmap text' unpacker
        , fmap text' ltype
        )
      )
    toExp e = error (
      "Bad SPARQL, expected: " ++
      "[Just alias, Just fname, Just typedec, Just generic, Just el, unpacker, ltype]\n" ++
      "got: " ++ show e)


main :: [Doc] -> [((Doc, Doc, Doc, Doc), [Maybe (Doc, Doc)])] -> Doc
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
  cat(packGeneric(result), "\n")
} else {
  stop("Could not find function '", args[[1]], "'")
}
|]

sourceT s = [idoc|source("${s}")|]

exportedT ((alias, fname, uid, generic), args) = [idoc|
# ${alias}

${uid} <- function(${hcat (punctuate ", " (nameArgs args))}){
  ${fname}(${hcat (punctuate ", " (castArgs (generic, args)))})
}
|]
  where
    nameArgs :: [a] -> [Doc]
    nameArgs xs = map ((<>) "x") (map int [1 .. length xs])

    castArgs :: (Doc, [Maybe (Doc, Doc)]) -> [Doc]
    castArgs (generic, xss) = zipWith
      (\w i -> [idoc|${w}(x${i})|])
      (map (wrapper generic) xss)
      (map int [1..])

    wrapper :: Doc -> (Maybe (Doc, Doc)) -> Doc
    wrapper _ (Just (specific, _)) = specific
    wrapper generic _ = generic
