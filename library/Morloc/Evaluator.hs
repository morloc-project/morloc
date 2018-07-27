{-# LANGUAGE OverloadedStrings #-}

module Morloc.Evaluator
(
    isElement
  , position
  , elements
  , fetchType
  , getKids
  , haveKid
  , hasThese
  -- probably not worth exporting
  , p
  , o
  , v
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Char as DC
import qualified Control.Monad as CM

import qualified Morloc.Error as ME

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

requireOne :: [a] -> Maybe a
requireOne [x] = Just x
requireOne _  = Nothing

p :: DT.Text -> DR.Predicate
p s = DR.UNode s

o :: DT.Text -> DR.Object
o s = DR.UNode s

v :: Maybe DT.Text -> DT.Text -> DR.Object
v (Just t) s = DR.LNode (DR.TypedL t s)
v Nothing  s = DR.LNode (DR.PlainL s)
  
valueOf :: DR.Node -> Maybe DT.Text
valueOf (DR.LNode (DR.TypedL _ s)) = Just s
valueOf (DR.LNode (DR.PlainL s)) = Just s
valueOf _ = Nothing 

maybe2bool :: Maybe a -> Bool
maybe2bool (Just _) = True
maybe2bool Nothing = False


getKids :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Subject    -- -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Object]   -- /  chain function, allows searching in parallel
getKids rdf p s = map DR.objectOf (DR.query rdf (Just s) (Just p) Nothing)

haveKid :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Object
  -> DR.Subject
  -> [DR.Subject]
haveKid rdf p o s = case DR.query rdf (Just s) (Just p) (Just o) of 
  [] -> [ ] -- if nothing is found, the subject is filtered out 
  _  -> [s] -- if anything is found, the subject is kept

hasThese :: DR.Rdf a
  => DR.RDF a
  -> (DR.RDF a -> DR.Node -> [DR.Node])
  -> DR.Subject
  -> [DR.Subject]
hasThese rdf f x = case f rdf x of
  [] -> []
  _ -> [x]

fetchType :: DR.Rdf a => DR.RDF a -> DT.Text -> Maybe DR.Node
fetchType rdf name
  -- require the final result contain only a single node
  = requireOne
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
            getKids r' (p "morloc:lhs") x
        >>= haveKid r' (p "rdf:type") (v (Just "morloc:name") name)
      )
  -- get the rhs value
  >>= getKids rdf (p "morloc:rhs")
