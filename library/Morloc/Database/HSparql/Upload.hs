{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Database.Upload
Description : Upload an RDF graph to a SPARQL endpoin
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The material in this module is adapted from the rdf4h and hsparql packages
maintained by Rob Steward.
-}

module Morloc.Database.HSparql.Upload (
    uploadRDF
  , uploadTriples
) where

import Data.RDF
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.PrettyPrint.Leijen.Text as G

import qualified Morloc.Database.HSparql.Connection as Conn
import Morloc.Operators

type EndPoint = String

uploadRDF :: Rdf a => EndPoint -> RDF a -> IO Bool
uploadRDF ep rdf = uploadTriples ep (triplesOf rdf)

uploadTriples :: EndPoint -> Triples -> IO Bool
uploadTriples ep xs = Conn.updateQuery' ep (_render . makeSparql $ xs) where
  -- TODO: convert the whole Connection module to Text
  _render :: G.Doc -> String 
  _render = LT.unpack . G.displayT . G.renderPretty 0.5 70

makeSparql :: [Triple] -> G.Doc
makeSparql xs =
  "INSERT DATA" <> G.line <> G.braces (G.indent 4 $ makeTriples xs)

makeTriples :: Triples -> G.Doc
makeTriples xs = G.vsep (map makeTriple xs) 

makeTriple :: Triple -> G.Doc
makeTriple (Triple s o p) = G.hsep [makeNode s, makeNode o, makeNode p, "."]

text' :: T.Text -> G.Doc
text' t = G.text (LT.fromStrict t)

makeNode :: Node -> G.Doc
makeNode (UNode s) = G.angles (text' s) 
makeNode (BNode gId) = "_:" <> text' gId
makeNode (BNodeGen i) = "_:genid" <> G.int i
makeNode (LNode (PlainL lit)) = makeLiteral lit
makeNode (LNode (PlainLL lit lang)) = makeLiteral lit <> "@" <> text' lang
makeNode (LNode (TypedL lit dtype)) = makeLiteral lit <> "^^" <> G.angles (text' dtype)

makeLiteral :: T.Text -> G.Doc
makeLiteral lit = (G.dquotes . G.string . LT.fromStrict) $ T.concatMap escapeChar lit where
  escapeChar '\n' = "\\n"
  escapeChar '\t' = "\\t"
  escapeChar '\r' = "\\r"
  escapeChar '"'  = "\\\""
  escapeChar '\\' = "\\\\"
  escapeChar c    = T.singleton c
