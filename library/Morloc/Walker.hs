{-# LANGUAGE OverloadedStrings #-}

module Morloc.Walker
(
  -- general RDF access functions
    down
  , downOn
  , up
  , has
  , hasThese
  , isElement
  , position
  , elements
  , countElements
  , rdftype
  , value
  , valueOf
  , idOf
  -- node builder convenience functions
  , p
  , o
  , v
  -- morloc specific query functions
  , getType
  , getConstraints
  , getSources
  , getDataDeclarations
  , getCalls
  , getScope
  -- morloc specific step functions
  , imports
  , lang
  , path
  , name
  , lhs
  , rhs
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

valueOf :: DR.Node -> [DT.Text]
valueOf (DR.LNode (DR.TypedL _ s)) = [s]
valueOf (DR.LNode (DR.PlainL s)) = [s]
valueOf _ = []

idOf :: DR.Node -> [DT.Text]
idOf (DR.UNode s) = [s]
idOr _ = []

-- Down :: Subject -> [Object]
down :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Subject    -- -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Object]   -- /  chain function, allows searching in parallel
down rdf p s = map DR.objectOf (DR.query rdf (Just s) (Just p) Nothing)

up :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Object    -- -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Subject]   -- /  chain function, allows searching in parallel
up rdf p o = map DR.subjectOf (DR.query rdf Nothing (Just p) (Just o))

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

parent :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Node]
parent rdf kid = map DR.subjectOf $
  DR.select
    rdf
    (Just $ (==) kid)
    (Just isElement)
    Nothing

-- Morloc specific functions ----------------------------------------

lhs :: DR.Rdf a => DR.RDF a -> DR.Subject -> [DR.Object]
lhs rdf s = down rdf (p "morloc:lhs") s

rhs :: DR.Rdf a => DR.RDF a -> DR.Subject -> [DR.Object]
rhs rdf s = down rdf (p "morloc:rhs") s

lang :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Object]
lang rdf s = down rdf (p "morloc:lang") s

path :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Object]
path rdf s = down rdf (p "morloc:path") s

name :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Object]
name rdf s = down rdf (p "morloc:name") s

value :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Object]
value rdf s = down rdf (p "morloc:value") s

rdftype :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Object]
rdftype rdf s = down rdf (p "rdf:type") s

imports :: DR.Rdf a => DR.RDF a -> DR.Subject -> [(DT.Text, Maybe DT.Text)]
imports rdf s = down rdf (p "morloc:import") s >>= names'
  where
    names' :: DR.Subject -> [(DT.Text, Maybe DT.Text)]
    names' i =
      case
        ( MU.maybeOne (down rdf (p "morloc:name" ) i >>= valueOf)
        , MU.maybeOne (down rdf (p "morloc:alias") i >>= valueOf)
        )
      of
        (Just name, alias) -> [(name, alias)]
        _ -> []

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

-- for an element in a DataDeclaration or TypeDeclaration, get the top node
getScope :: DR.Rdf a => DR.RDF a -> DR.Node -> [DR.Node]
getScope rdf n = getScope' rdf n
  where 
    getScope' rdf n
      | (rdftype rdf n >>= valueOf) == ["morloc:dataDeclaration"] = [n]
      | (rdftype rdf n >>= valueOf) == ["morloc:typeDeclaration"] = [n]
      | length (rhsOf rdf n) == 1 = rhsOf rdf n 
      | otherwise = parent rdf n >>= has rdf (p "rdf:type") (v Nothing "morloc:call")

rhsOf :: DR.Rdf a => DR.RDF a -> DR.Object -> [DR.Subject]
rhsOf rdf o = up rdf (p "morloc:rhs") o
  

getCalls :: DR.Rdf a => DR.RDF a -> [DR.Node]
getCalls rdf
  = DR.query rdf
    Nothing 
    (Just $ p "rdf:type")
    (Just $ o "morloc:call")
  |>> DR.subjectOf

getSources :: DR.Rdf a => DR.RDF a -> [DR.Node]
getSources rdf
  = DR.query rdf
    Nothing
    (Just $ p "rdf:type")
    (Just $ o "morloc:source")
  |>> DR.subjectOf
