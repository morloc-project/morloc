{-# LANGUAGE OverloadedStrings #-}

module Morloc.Walker
(
    down
  , downOn
  , has
  , hasThese
  , isElement
  , position
  , elements
  , countElements
  , getType
  , getConstraints
  , getSources
  , getDataDeclarations
  , imports
  , value
  , lhs
  , rhs
  , p
  , o
  , v
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Char as DC
import qualified Control.Monad as CM
import qualified Data.Text.Read as DTR

import qualified Morloc.Error as ME
import qualified Morloc.Util as MU
import Morloc.Operators ((|>>))

-- Convenience functions
p :: DT.Text -> DR.Predicate
p s = DR.UNode s

o :: DT.Text -> DR.Object
o s = DR.UNode s

v :: Maybe DT.Text -> DT.Text -> DR.Object
v (Just t) s = DR.LNode (DR.TypedL t s)
v Nothing  s = DR.LNode (DR.PlainL s)

-- Operations
--
-- End :: [Node] -> a
maybeValue :: DR.Node -> Maybe DT.Text
maybeValue (DR.LNode (DR.TypedL _ s)) = Just s
maybeValue (DR.LNode (DR.PlainL s)) = Just s
maybeValue _ = Nothing 

value :: DR.Node -> [DT.Text]
value (DR.LNode (DR.TypedL _ s)) = [s]
value (DR.LNode (DR.PlainL s)) = [s]
value _ = []

-- Down :: Subject -> [Object]
down :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Subject    -- -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Object]   -- /  chain function, allows searching in parallel
down rdf p s = map DR.objectOf (DR.query rdf (Just s) (Just p) Nothing)

downOn :: DR.Rdf a
  => DR.RDF a
  -> (DR.Predicate -> Bool)
  -> DR.Subject
  -> [DR.Object]
downOn rdf pf s = map DR.objectOf
  (DR.select rdf (Just $ (==) s) (Just pf) Nothing)

-- Require :: Subject:x -> xs:[Subject]
-- -- Where (i == xs[0] and len xs == 1) or (len xs == 0)
has :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Object
  -> DR.Subject
  -> [DR.Subject]
has rdf p o s = case DR.query rdf (Just s) (Just p) (Just o) of 
  [] -> [ ] -- if nothing is found, the subject is filtered out 
  _  -> [s] -- if anything is found, the subject is kept


-- Filter :: (Node -> [Node]) -> Subject -> [Subject]
hasThese :: DR.Rdf a
  => DR.RDF a
  -> (DR.RDF a -> DR.Node -> [DR.Node])
  -> DR.Subject
  -> [DR.Subject]
hasThese rdf f x = case f rdf x of
  [] -> []
  _ -> [x]


isElement :: DR.Node -> Bool
isElement (DR.UNode s)
  = maybe False id             -- Bool
  . fmap (DT.all DC.isNumber)  -- Maybe Bool
  . DT.stripPrefix "rdf:_"     -- Maybe Text
  $ s
isElement _ = False

toIndex :: DR.Node -> Maybe Int
toIndex (DR.UNode n) = DT.stripPrefix "rdf:_" n >>= decimal' where
  decimal' :: DT.Text -> Maybe Int
  decimal' s = case DTR.decimal s of
    (Right (i, "")) -> Just i
    _ -> Nothing

position :: DR.Rdf a => DR.RDF a -> DR.Node -> Maybe Int
position rdf sbj
  = CM.join
  . MU.maybeOne 
  . map toIndex
  . map DR.predicateOf
  $ DR.select rdf (Just $ (==) sbj) (Just isElement) Nothing

countElements :: DR.Rdf a => DR.RDF a -> DR.Node -> Int
countElements rdf sbj = length $ downOn rdf isElement sbj

elements :: DR.Rdf a => DR.RDF a -> DR.Object -> [DR.Node]
elements rdf obj
  = map DR.subjectOf
  . DR.uordered
  $ DR.select
      rdf
      Nothing
      (Just isElement)
      (Just ((==) obj))

-- Morloc specific functions ----------------------------------------

lhs :: DR.Rdf a => DR.RDF a -> DR.Subject -> [DR.Object]
lhs rdf s = down rdf (p "morloc:lhs") s

rhs :: DR.Rdf a => DR.RDF a -> DR.Subject -> [DR.Object]
rhs rdf s = down rdf (p "morloc:rhs") s

lang :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Object]
lang rdf s = down rdf (p "morloc:lang") s

imports :: DR.Rdf a => DR.RDF a -> DR.Subject -> [(DT.Text, Maybe DT.Text)]
imports rdf s = down rdf (p "morloc:import") s >>= names'
  where
    names' :: DR.Subject -> [(DT.Text, Maybe DT.Text)]
    names' i =
      case
        ( MU.maybeOne (down rdf (p "morloc:name" ) i >>= value)
        , MU.maybeOne (down rdf (p "morloc:alias") i >>= value)
        )
      of
        (Just name, alias) -> [(name, alias)]
        _ -> []

-- -- TODO add getters for all of these
-- "morloc:list"                 "rdfs:subClassOf" "rdfs:Seq" -- elements
-- "morloc:tuple"                "rdfs:subClassOf" "rdfs:Seq" -- elements
-- "morloc:record"               "rdfs:subClassOf" "rdfs:Bag" -- tag/value pairs
-- "morloc:script"               "rdfs:subClassOf" "rdfs:Bag" -- statements
-- "morloc:call"                 "rdfs:subClassOf" "rdfs:Seq" -- arguments
-- "morloc:parameterizedType"    "rdfs:subClassOf" "rdfs:Seq" -- type parameters
-- "morloc:parameterizedGeneric" "rdfs:subClassOf" "rdfs:Seq" -- type parameters
-- "morloc:restricted_import"    "rdfs:subClassOf" "rdfs:Bag" -- imports
-- "morloc:import"               "rdfs:subClassOf" "rdfs:Bag" -- imports
-- "morloc:source"               "rdfs:subClassOf" "rdfs:Bag" -- imports
-- "morloc:functionType"         "rdfs:subClassOf" "rdfs:Seq" -- inputs
--   -- primitives
-- "morloc:alias"         "rdf:type" "xsd:string"
-- "morloc:atomicGeneric" "rdf:type" "xsd:string"
-- "morloc:atomicType"    "rdf:type" "xsd:string"
-- "morloc:integer"       "rdf:type" "xsd:integer"
-- "morloc:boolean"       "rdf:type" "xsd:boolean"
-- "morloc:number"        "rdf:type" "xsd:float"
-- "morloc:key"           "rdf:type" "xsd:string"
-- "morloc:label"         "rdf:type" "xsd:string"
-- "morloc:lang"          "rdf:type" "xsd:string"
-- "morloc:name"          "rdf:type" "xsd:string"
-- "morloc:string"        "rdf:type" "xsd:string"
-- "morloc:namespace"     "rdf:type" "xsd:string"
--   -- generic value tags
-- "morloc:value"  "rdf:type" "rdf:value"
-- "morloc:output" "rdf:type" "rdf:value"
-- "morloc:lhs"    "rdf:type" "rdf:value"
-- "morloc:rhs"    "rdf:type" "rdf:value"


getType :: DR.Rdf a => DR.RDF a -> DT.Text -> [DR.Node]
getType rdf name
  -- get Triples for all type declarations
  =   DR.query rdf
        Nothing
        (Just $ p "rdf:type")
        (Just $ o "morloc:typeDeclaration")
  -- Get the subject node from each triple
  |>> DR.subjectOf
  -- remove any IDs that do not have the appropriate lhs name
  >>= hasThese rdf (\r' x ->
            down r' (p "morloc:lhs") x
        >>= has r' (p "rdf:type") (v (Just "morloc:name") name)
      )
  -- get the rhs
  >>= down rdf (p "morloc:rhs")

-- x = (\(Right z) -> z) $ morlocScript "foo :: x:A, y:B -> z:C where (x > y);"
-- getConstraints x "foo"
getConstraints :: DR.Rdf a => DR.RDF a -> DT.Text -> [DR.Node]
getConstraints rdf n = getType rdf n >>= down rdf (p "morloc:constraint")

getDataDeclarations :: DR.Rdf a => DR.RDF a -> [DR.Node]
getDataDeclarations rdf
  = DR.query rdf
    Nothing 
    (Just $ p "rdf:type")
    (Just $ o "morloc:dataDeclaration")
  |>> DR.subjectOf

getSources :: DR.Rdf a => DR.RDF a -> [DR.Node]
getSources rdf
  = DR.query rdf
    Nothing
    (Just $ p "rdf:type")
    (Just $ o "morloc:source")
  |>> DR.subjectOf
