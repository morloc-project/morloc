{-# LANGUAGE OverloadedStrings #-}

module Morloc.Evaluator
(
    rdf2manifolds
  , isElement
  , position
  , qSeq
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Char as DC
import qualified Morloc.Data as MD

rdf2manifolds :: DR.RDF a -> [MD.Manifold]
rdf2manifolds = undefined

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

qSeq :: DR.Rdf a => DR.Object -> DR.RDF a -> [DR.Triple]
qSeq obj rel = DR.select
  rel
  Nothing
  (Just isElement)
  (Just ((==) obj))
