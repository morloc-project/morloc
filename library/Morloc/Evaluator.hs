{-# LANGUAGE OverloadedStrings #-}

module Morloc.Evaluator
(
    down
  , downOn
  , has
  , hasThese
  , isElement
  , position
  , elements
  , fetchType
  , p
  , o
  , v
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Char as DC
import qualified Control.Monad as CM

import qualified Morloc.Error as ME
import qualified Morloc.Util as MU

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
valueOf :: DR.Node -> Maybe DT.Text
valueOf (DR.LNode (DR.TypedL _ s)) = Just s
valueOf (DR.LNode (DR.PlainL s)) = Just s
valueOf _ = Nothing 

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


-- Morloc specific functions ----------------------------------------

isElement :: DR.Node -> Bool
isElement (DR.UNode s)
  = maybe False id             -- Bool
  . fmap (DT.all DC.isNumber)  -- Maybe Bool
  . DT.stripPrefix "rdf:_"     -- Maybe Text
  $ s
isElement _ = False

position :: DR.Node -> Maybe Int
position (DR.UNode s) = case DT.stripPrefix "rdf:_" s of
  Just str_int -> case (reads (show str_int) :: [(Int, String)]) of
    [(i, "")] -> Just i
    _ -> Nothing
  _ -> Nothing
position _ = Nothing

elements :: DR.Rdf a => DR.RDF a -> DR.Object -> [DR.Node]
elements rdf obj
  = map DR.subjectOf
  . DR.uordered
  $ DR.select
      rdf
      Nothing
      (Just isElement)
      (Just ((==) obj))

fetchType :: DR.Rdf a => DR.RDF a -> DT.Text -> Maybe DR.Node
fetchType rdf name
  -- require the final result contain only a single node
  = MU.maybeOne
  -- get the IDs for all type declarations
  $ (map DR.subjectOf)
    (DR.query
      rdf
      Nothing
      (Just $ p "rdf:type")
      (Just $ o "morloc:typeDeclaration")
    )
  -- remove any IDs that do not have the appropriate lhs name
  >>= hasThese rdf (\r' x ->
            down r' (p "morloc:lhs") x
        >>= has r' (p "rdf:type") (v (Just "morloc:name") name)
      )
  -- get the rhs
  >>= down rdf (p "morloc:rhs")
