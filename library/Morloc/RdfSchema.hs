{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.RdfSchema
Description : Contains the OWL schema for a Morloc scripts
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

Currently nothing is done with this schema. Eventually all the logic needed for
typechecking a Morloc script will be declared here using OWL. Then a standard
reasoner can be used to validate the program and infer missing information
(such as calls that need a conversion). 
-}

module Morloc.RdfSchema
(
  rdfSchema
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT

sss :: DT.Text -> DT.Text -> DT.Text -> DR.Triple
sss s p o = DR.triple (DR.UNode s) (DR.UNode p) (DR.UNode o)

rdfSchema :: [DR.Triple]
rdfSchema =
  [
  -- collections
    sss "morloc:list"                 "rdfs:subClassOf" "rdfs:Seq" -- elements
  , sss "morloc:tuple"                "rdfs:subClassOf" "rdfs:Seq" -- elements
  , sss "morloc:record"               "rdfs:subClassOf" "rdfs:Bag" -- tag/value pairs
  , sss "morloc:script"               "rdfs:subClassOf" "rdfs:Bag" -- statements
  , sss "morloc:call"                 "rdfs:subClassOf" "rdfs:Seq" -- arguments
  , sss "morloc:parameterizedType"    "rdfs:subClassOf" "rdfs:Seq" -- type parameters
  , sss "morloc:parameterizedGeneric" "rdfs:subClassOf" "rdfs:Seq" -- type parameters
  , sss "morloc:restricted_import"    "rdfs:subClassOf" "rdfs:Bag" -- imports
  , sss "morloc:import"               "rdfs:subClassOf" "rdfs:Bag" -- imports
  , sss "morloc:source"               "rdfs:subClassOf" "rdfs:Bag" -- imports
  , sss "morloc:functionType"         "rdfs:subClassOf" "rdfs:Seq" -- inputs
  -- primitives
  , sss "morloc:alias"         "rdf:type" "xsd:string"
  , sss "morloc:atomicGeneric" "rdf:type" "xsd:string"
  , sss "morloc:atomicType"    "rdf:type" "xsd:string"
  , sss "morloc:integer"       "rdf:type" "xsd:integer"
  , sss "morloc:boolean"       "rdf:type" "xsd:boolean"
  , sss "morloc:number"        "rdf:type" "xsd:float"
  , sss "morloc:key"           "rdf:type" "xsd:string"
  , sss "morloc:label"         "rdf:type" "xsd:string"
  , sss "morloc:lang"          "rdf:type" "xsd:string"
  , sss "morloc:name"          "rdf:type" "xsd:string"
  , sss "morloc:string"        "rdf:type" "xsd:string"
  , sss "morloc:namespace"     "rdf:type" "xsd:string"
  -- generic value tags
  , sss "morloc:value"  "rdf:type" "rdf:value"
  , sss "morloc:output" "rdf:type" "rdf:value"
  , sss "morloc:lhs"    "rdf:type" "rdf:value"
  , sss "morloc:rhs"    "rdf:type" "rdf:value"
  -- -- things with a left and right hand side (binary operators)
  -- "morloc:binop"
  -- "morloc:composition"
  -- "morloc:dataDeclaration"
  -- "morloc:namedType"
  -- "morloc:recordEntry"
  -- "morloc:typeDeclaration"
  --
  -- -- unary operators
  -- "morloc:unaryOp"
  ]
