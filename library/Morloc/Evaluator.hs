{-# LANGUAGE OverloadedStrings #-}

module Morloc.Evaluator
(
    isElement
  , position
  , elements
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Char as DC

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

byType :: DR.Rdf a => DR.RDF a -> DT.Text -> [DR.Node]
byType rdf s = map DR.subjectOf $ DR.query
  rdf
  Nothing
  (Just $ DR.UNode "rdf:type")
  (Just $ DR.UNode s) 

objRelation :: DR.Rdf a => DR.RDF a -> DT.Text -> DR.Node -> [DR.Node]
objRelation r s o = map DR.subjectOf $ DR.query r (Just o) (Just $ DR.UNode s) Nothing 

-- get a type declaration by name
fetchType :: DR.Rdf a => DR.RDF a -> DT.Text -> ME.ThrowsError DR.Node
fetchType rdf name =
    requireOne'
  . concat
  . map (objRelation rdf "morloc:rhs")
  . filter (leftValue' rdf name)
  . byType rdf
  $ "morloc:typeDeclaration" 
  where
    requireOne' :: [a] -> ME.ThrowsError a
    requireOne' []  = Left $ ME.MissingType (show name)
    requireOne' [x] = Right x
    requireOne' _   = Left $ ME.NameConflict (show name)

    -- it was for times like these that monads where invented ... 
    leftValue' :: DR.Rdf a => DR.RDF a -> DT.Text -> DR.Node -> Bool
    leftValue' r' n' s' = case objRelation r' "morloc:lhs" s' of
      [o'] -> case objRelation r' "rdf:type" o' of
        [DR.LNode (DR.TypedL "morloc:name" _)] -> True
        _ -> False
      _ -> False
