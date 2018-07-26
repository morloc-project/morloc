{-# LANGUAGE OverloadedStrings #-}

module Morloc.RdfSchema
(
  rdfSchema
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT

sss :: DT.Text -> DT.Text -> DT.Text -> DR.Triple
sss s p o = DR.triple (DR.UNode s) (DR.UNode p) (DR.UNode o)

rdfSchema =
  [ sss "morloc:list"                 "rdfs:subClassOf" "rdfs:Seq"
  , sss "morloc:tuple"                "rdfs:subClassOf" "rdfs:Seq"
  , sss "morloc:record"               "rdfs:subClassOf" "rdfs:Bag"
  , sss "morloc:script"               "rdfs:subClassOf" "rdfs:Bag"
  , sss "morloc:call"                 "rdfs:subClassOf" "rdfs:Seq"
  , sss "morloc:input"                "rdfs:subClassOf" "rdfs:Seq"
  , sss "morloc:parameterizedType"    "rdfs:subClassOf" "rdfs:Seq"
  , sss "morloc:parameterizedGeneric" "rdfs:subClassOf" "rdfs:Seq"
  ]
