{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Component.MType
Description : Build manifolds for code generation from a SPARQL endpoint.
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Component.MType (fromSparqlDb) where

import Morloc.Types
import Morloc.Operators
import Morloc.Quasi
import qualified Morloc.Triple as M3
import qualified Morloc.Component.Util as MCU

import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
import Morloc.Database.HSparql.Connection
import qualified Data.Map.Strict as Map
import qualified Data.List.Extra as DLE
import qualified Data.Foldable as DF
import qualified Data.Text as DT

fromSparqlDb :: SparqlEndPoint -> IO (Map.Map Key MType)
fromSparqlDb = MCU.simpleGraph toMType getParentData id sparqlQuery

getParentData :: [Maybe DT.Text] -> (DT.Text, Maybe DT.Text, Maybe Key, Maybe Lang, [Name]) 
getParentData [Just t, v, o, l, ps] = (t, v, o, l, properties) where
  properties = DF.concat . fmap (DT.splitOn ",") $ ps
getParentData x = error ("Unexpected SPARQL result: " ++ show x)

toMType :: Map.Map Key ((DT.Text, Maybe DT.Text, Maybe Key, Maybe Lang, [Name]), [Key]) -> Key -> MType
toMType h k = toMType' (Map.lookup k h) where
  toMType' (Just ((_, Just v, _, Just l, ps), [f])) = MDeclType v ps l (toMType h f)
  toMType' (Just ((_, Just v, _, _, _), xs)) = MDataType v (map (toMType h) xs)
  toMType' (Just ((_, _, Just o, _, _), xs)) = MFuncType (map (toMType h) xs) (toMType h o)

instance MShow MType where
  mshow (MDataType n []) = text' n
  mshow (MDataType n ts) = parens $ hsep (text' n:(map mshow ts))
  mshow (MFuncType ts o) = parens $
    (hcat . punctuate ", ") (map mshow ts) <> " -> " <> mshow o
  mshow (MDeclType name props lang t) = hsep [ text' name
                                             , text' lang
                                             , "::"
                                             , tupled (map text' props)
                                             , " => "
                                             , mshow t
                                             ] 

sparqlQuery :: SparqlEndPoint -> IO [[Maybe DT.Text]]
sparqlQuery = [sparql|
PREFIX mlc: <http://www.morloc.io/ontology/000/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX mid: <http://www.morloc.io/xxx/mid/>
SELECT ?id ?element ?child ?type ?v ?output ?lang
       (GROUP_CONCAT(?property; separator=",") AS ?properties)
WHERE {
    {
        ?id rdf:type mlc:type ;
            rdf:type ?type .
        FILTER(?type != mlc:type)
        OPTIONAL {
            ?id rdf:value ?v
        }
        OPTIONAL {
            ?id ?element ?child
            FILTER(REGEX(STR(?element), "_[0-9]+$", "i"))
        }
        OPTIONAL {
            ?id mlc:output ?output
        }
    } UNION {
        ?id rdf:type mlc:typeDeclaration ;
            mlc:lhs ?v ;
            mlc:lang ?lang ;
            mlc:rhs ?child .
        OPTIONAL { ?child mlc:property/rdf:value ?property }
        BIND(mlc:typeDeclaration AS ?type)
        BIND(rdf:_0 AS ?element) # type declarations have only one "child"
    }
}
GROUP BY ?id ?element ?child ?type ?v ?output ?lang
ORDER BY ?id ?element
|] 
