{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Json
Description : Translate Morloc data to JSON
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Json (
    mdata2aeson
  , aeson2mdata
  , mtype2aeson
  , aeson2mtype
) where

import qualified Data.Aeson as DA
import qualified Data.RDF as DR
import qualified Text.ParserCombinators.ReadP as TPR
import qualified Data.Scientific as DS
import qualified Data.Text as DT
import qualified Data.HashMap.Strict as DHS
import qualified Data.Vector as DV

import qualified Morloc.Walker as MW
import Morloc.Operators

-- | Convert Morloc data to aeson, a Haskell data type that maps directly to JSON
mdata2aeson :: DR.Rdf a => DR.RDF a -> DR.Node -> DA.Value
mdata2aeson rdf x = case MW.down rdf (MW.p "rdf:type") x of
  [] -> error "mdata2aeson error: the given node does not represent a type"
  [DR.LNode (DR.TypedL "morloc:integer" s)] -> toScientific s
  [DR.LNode (DR.TypedL "morloc:number"  s)] -> toScientific s
  [DR.LNode (DR.TypedL "morloc:string"  s)] -> DA.String s
  [DR.LNode (DR.TypedL "morloc:boolean" s)] -> toBool s
  [DR.UNode "morloc:list"]   -> toArray  "List"
  [DR.UNode "morloc:tuple"]  -> toArray  "Tuple"
  [DR.UNode "morloc:record"] -> toObject "Record"
  _ -> error "Data type not supported"
  where
    toScientific s' = case TPR.readP_to_S DS.scientificP (DT.unpack s') of
      [(i, "")] -> DA.Number i
      _ -> error "Failed to parse scientific number"

    toBool s'
      | s' == "True"  = DA.Bool True
      | s' == "False" = DA.Bool False
      | otherwise = error "Failed to parse boolean"

    toArray typename = DA.Object $
      DHS.singleton
        (DT.pack typename)
        (DA.Array $ DV.map (mdata2aeson rdf) (DV.fromList $ MW.elements rdf x))

    toObject typename = DA.Object $
      DHS.singleton
        (DT.pack typename)
        (DA.Object . DHS.fromList $
            zip (concat $ MW.elements rdf x >>= MW.lhs rdf |>> MW.valueOf)
                (MW.elements rdf x >>= MW.rhs rdf |>> mdata2aeson rdf)
          )

-- not something I need yet, but included for completeness
aeson2mdata :: DR.Rdf a => DA.Value -> DR.RDF a
aeson2mdata = undefined

typeDeclaration2aeson :: DR.Rdf a => DR.RDF a -> DR.Node -> DA.Value
typeDeclaration2aeson rdf x =
  case
    ( MW.lhs rdf x >>= MW.down rdf (MW.p "rdf:type") >>= MW.valueOf
    , MW.rhs rdf x)
  of
    ([name], [t]) -> DA.Object $ DHS.singleton name (mtype2aeson rdf t)
    _ -> error "Cannot convert type declaration to JSON"

-- | build a type template from a type (used in nexus and pool generators)
mtype2aeson :: DR.Rdf a => DR.RDF a -> DR.Node -> DA.Value
mtype2aeson rdf x = case MW.down rdf (MW.p "rdf:type") x of
  [DR.UNode "morloc:functionType"] -> toArray (DT.pack "Function")
  [DR.LNode (DR.TypedL "morloc:parameterizedType" s)] -> toArray s
  [DR.LNode (DR.TypedL "morloc:atomicType" s)] -> DA.String s
  _ -> error "Cannot parse this type"
  where
    toArray typename = DA.Object $
      DHS.singleton
        typename
        (DA.Array $ DV.map (mtype2aeson rdf) (DV.fromList $ MW.elements rdf x))

-- | infer the type of a JSON
aeson2mtype :: DR.Rdf a => DR.RDF a -> DA.Value -> DR.Node
aeson2mtype = undefined
