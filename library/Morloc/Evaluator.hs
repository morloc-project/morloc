{-# LANGUAGE OverloadedStrings #-}

module Morloc.Evaluator
(
    isElement
  , position
  , elements
  , fetchType
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

-- get a type declaration by name
-- TODO: this is an insane amount of work for something so simple
fetchType :: DR.Rdf a => DR.RDF a -> DT.Text -> Maybe DR.Node
fetchType rdf name
  = requireOne
  . map DR.objectOf
  . concat
  . map (\o -> DR.query rdf (Just o) (Just $ p "morloc:rhs") Nothing)
  . filter (lhsName rdf (v (Just "morloc:name") name))
  . map DR.subjectOf
  $ DR.query rdf Nothing (Just $ p "rdf:type") (Just $ o "morloc:typeDeclaration")
  where
    lhsName :: DR.Rdf a => DR.RDF a -> DR.Object -> DR.Subject -> Bool 
    lhsName rdf' exp' s' =
      case
        DR.query rdf' (Just s') (Just $ p "morloc:lhs") Nothing 
      of
        [DR.Triple _ _ lhs'] ->
          case
            DR.query rdf' (Just lhs') (Just $ p "rdf:type") Nothing 
          of
            [DR.Triple _ _ obs'] -> obs' == exp'
            _ -> False
        _ -> False
