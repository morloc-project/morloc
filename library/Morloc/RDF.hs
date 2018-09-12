{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.RDF
Description : Convenience functions for working with triples
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.RDF (
    TopRDF(..)
  , RDF
  , mtriple
  -- ** Re-exports from Data.RDF
  , DR.Triple
  , DR.Node(..)
  , DR.LValue(..)
  , DR.triplesOf
  -- ** RDF Prefixes
  , prefixMap
  , mlcPre
  , midPre
  , rdfPre
  , xsdPre
  -- ** RDF access
  , getImportedFiles
  -- ** TopRDF Utilities
  , makeTopRDF
  , idUri
  , rdfId
  , adoptAs
  , adopt
  , showTopRDF
  , rdfAppend
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS
import qualified Data.Maybe as DM
import qualified Safe
import qualified Data.Scientific as DS

import Morloc.Types
-- TODO: remove this import
import Morloc.Operators
-- TODO: remove this import
import Morloc.Util (show')

type RDF = DR.RDF DR.TList

data TopRDF = TopRDF DR.Node RDF deriving(Show)

-- | Join an RDF prefix and base creating a URI node
infix 9 .:.
(.:.) :: DT.Text -> DT.Text -> DR.Node
prefix .:. base = DR.UNode (prefix <> base)

mlcPre :: DT.Text
mlcPre = "http://www.morloc.io/ontology/000/"

midPre :: DT.Text
midPre = "http://www.morloc.io/XXX/mid/"

rdfPre :: DT.Text
rdfPre = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

xsdPre :: DT.Text
xsdPre = "http://www.w3.org/2001/XMLSchema#"

prefixMap :: DR.PrefixMappings
prefixMap = DR.PrefixMappings $ DMS.fromList
  [("mlc", mlcPre),
   ("mid", midPre),
   ("rdf", rdfPre),
   ("xsd", xsdPre)
  ]

instance MorlocNodeLike DR.Node where
  asRdfNode = id
  fromRdfNode = id

instance MorlocNodeLike DT.Text where
  asRdfNode s = DR.LNode (DR.PlainL s)
  fromRdfNode (DR.LNode (DR.PlainL s)) = s
  fromRdfNode (DR.LNode (DR.PlainLL s _)) = s
  fromRdfNode (DR.LNode (DR.TypedL s _)) = s
  fromRdfNode (DR.UNode s) = s
  fromRdfNode (DR.BNode s) = s
  -- TODO: match to what rdf4h actually generates
  fromRdfNode (DR.BNodeGen i) = DT.pack ("auto_" ++ show i)

instance MorlocNodeLike DS.Scientific where
  asRdfNode x = DR.LNode (DR.TypedL (show' x) (xsdPre <> "decimal"))
  fromRdfNode (DR.LNode (DR.TypedL x t))
    | t == (xsdPre <> "decimal") = read (DT.unpack x)
    | otherwise = error ("Cannot read number from node of type: " ++ show t) 
  fromRdfNode n = error ("Cannot read number from node: " ++ show n)


instance MorlocNodeLike Bool where
  asRdfNode True  = DR.LNode (DR.TypedL "true"  (xsdPre <> "boolean"))
  asRdfNode False = DR.LNode (DR.TypedL "false" (xsdPre <> "boolean"))
  fromRdfNode (DR.LNode (DR.TypedL x t))
    | t == (xsdPre <> "boolean") && x == "false" = False
    | t == (xsdPre <> "boolean") && x == "true" = True
    | t == (xsdPre <> "boolean") = error ("Expected RDF boolean to be true/false, found: " ++ show x)
    | otherwise = error ("Expected boolean node, found: " ++ show x ++ "^^" ++ show t)  
  fromRdfNode x = error ("Could not derive Bool from node: " ++ show x) 

instance MorlocNodeLike GraphPredicate where
  asRdfNode (PElem i)  = rdfPre .:. ("_" <> show' i)
  asRdfNode PLabel     = rdfPre .:. "label"
  asRdfNode PType      = rdfPre .:. "type"
  asRdfNode PValue     = rdfPre .:. "value"
  asRdfNode PAlias     = mlcPre .:. "alias"
  asRdfNode PConstraint = mlcPre .:. "constraint"
  asRdfNode PLang      = mlcPre .:. "lang"
  asRdfNode PLeft      = mlcPre .:. "left"
  asRdfNode PNamespace = mlcPre .:. "namespace"
  asRdfNode POutput    = mlcPre .:. "output"
  asRdfNode PPath      = mlcPre .:. "path"
  asRdfNode PProperty  = mlcPre .:. "property"
  asRdfNode PRight     = mlcPre .:. "right"
  asRdfNode PKey       = mlcPre .:. "key"
  asRdfNode PNot       = mlcPre .:. "not"
  asRdfNode PName      = mlcPre .:. "name"
  asRdfNode PImport    = mlcPre .:. "import"

  fromRdfNode n
    | n == ( rdfPre .:. "label"      ) = PLabel
    | n == ( rdfPre .:. "type"       ) = PType
    | n == ( rdfPre .:. "value"      ) = PValue
    | n == ( mlcPre .:. "alias"      ) = PAlias
    | n == ( mlcPre .:. "constraint" ) = PConstraint
    | n == ( mlcPre .:. "lang"       ) = PLang
    | n == ( mlcPre .:. "left"       ) = PLeft
    | n == ( mlcPre .:. "namespace"  ) = PNamespace
    | n == ( mlcPre .:. "output"     ) = POutput
    | n == ( mlcPre .:. "path"       ) = PPath
    | n == ( mlcPre .:. "property"   ) = PProperty
    | n == ( mlcPre .:. "right"      ) = PRight
    | n == ( mlcPre .:. "key"        ) = PKey
    | n == ( mlcPre .:. "not"        ) = PNot
    | n == ( mlcPre .:. "name"       ) = PName
    | n == ( mlcPre .:. "import"     ) = PImport
    | otherwise =
        case
          (DT.stripPrefix (rdfPre <> "_") (fromRdfNode n)) >>= (Safe.readMay . DT.unpack)
        of
          (Just i) -> PElem i
          Nothing -> error ("Unsupported predicate: " ++ show n)

instance MorlocNodeLike GraphObject where
  asRdfNode (OLiteral s)              = (DR.LNode (DR.PlainL s))
  asRdfNode OAccess                   = mlcPre .:. "access"
  asRdfNode OAtomicGenericType        = mlcPre .:. "atomicGeneric"
  asRdfNode OAtomicType               = mlcPre .:. "atomicType"
  asRdfNode OBinaryOp                 = mlcPre .:. "binaryOp"
  asRdfNode OBoolean                  = mlcPre .:. "boolean"
  asRdfNode OCall                     = mlcPre .:. "call"
  asRdfNode OData                     = mlcPre .:. "data"
  asRdfNode ODataDeclaration          = mlcPre .:. "dataDeclaration"
  asRdfNode OEmptyType                = mlcPre .:. "emptyType"
  asRdfNode OExport                   = mlcPre .:. "export"
  asRdfNode OFunctionType             = mlcPre .:. "functionType"
  asRdfNode OImport                   = mlcPre .:. "import"
  asRdfNode OList                     = mlcPre .:. "list"
  asRdfNode OName                     = mlcPre .:. "name"
  asRdfNode ONamedType                = mlcPre .:. "namedType"
  asRdfNode ONumber                   = mlcPre .:. "number"
  asRdfNode OParameterizedGenericType = mlcPre .:. "parameterizedGeneric"
  asRdfNode OParameterizedType        = mlcPre .:. "parameterizedType"
  asRdfNode ORecord                   = mlcPre .:. "record"
  asRdfNode ORecordEntry              = mlcPre .:. "recordEntry"
  asRdfNode ORestrictedImport         = mlcPre .:. "restrictedImport"
  asRdfNode OScript                   = mlcPre .:. "script"
  asRdfNode OSource                   = mlcPre .:. "source"
  asRdfNode OString                   = mlcPre .:. "string"
  asRdfNode OTuple                    = mlcPre .:. "tuple"
  asRdfNode OType                     = mlcPre .:. "type"
  asRdfNode OTypeDeclaration          = mlcPre .:. "typeDeclaration"
  asRdfNode OUnaryOp                  = mlcPre .:. "unaryOp"
  asRdfNode OEmpty                    = mlcPre .:. "empty"
  asRdfNode OBinOp                    = mlcPre .:. "binop"
  fromRdfNode (DR.LNode (DR.PlainL s)) = OLiteral s
  fromRdfNode n
    | n == ( mlcPre .:. "access"               ) = OAccess
    | n == ( mlcPre .:. "atomicGeneric"        ) = OAtomicGenericType
    | n == ( mlcPre .:. "atomicType"           ) = OAtomicType
    | n == ( mlcPre .:. "binaryOp"             ) = OBinaryOp
    | n == ( mlcPre .:. "boolean"              ) = OBoolean
    | n == ( mlcPre .:. "call"                 ) = OCall
    | n == ( mlcPre .:. "data"                 ) = OData
    | n == ( mlcPre .:. "dataDeclaration"      ) = ODataDeclaration
    | n == ( mlcPre .:. "emptyType"            ) = OEmptyType
    | n == ( mlcPre .:. "export"               ) = OExport
    | n == ( mlcPre .:. "functionType"         ) = OFunctionType
    | n == ( mlcPre .:. "import"               ) = OImport
    | n == ( mlcPre .:. "list"                 ) = OList
    | n == ( mlcPre .:. "name"                 ) = OName
    | n == ( mlcPre .:. "namedType"            ) = ONamedType
    | n == ( mlcPre .:. "number"               ) = ONumber
    | n == ( mlcPre .:. "parameterizedGeneric" ) = OParameterizedGenericType
    | n == ( mlcPre .:. "parameterizedType"    ) = OParameterizedType
    | n == ( mlcPre .:. "record"               ) = ORecord
    | n == ( mlcPre .:. "recordEntry"          ) = ORecordEntry
    | n == ( mlcPre .:. "restrictedImport"     ) = ORestrictedImport
    | n == ( mlcPre .:. "script"               ) = OScript
    | n == ( mlcPre .:. "source"               ) = OSource
    | n == ( mlcPre .:. "string"               ) = OString
    | n == ( mlcPre .:. "tuple"                ) = OTuple
    | n == ( mlcPre .:. "type"                 ) = OType
    | n == ( mlcPre .:. "typeDeclaration"      ) = OTypeDeclaration
    | n == ( mlcPre .:. "unaryOp"              ) = OUnaryOp
    | n == ( mlcPre .:. "empty"                ) = OEmpty
    | n == ( mlcPre .:. "binOp"                ) = OBinOp

-- | Build a triple from Morloc node-like objects
mtriple
  :: ( MorlocNodeLike s, MorlocNodeLike p, MorlocNodeLike o)
  => s -> p -> o -> DR.Triple
mtriple s p o = DR.triple (asRdfNode s) (asRdfNode p) (asRdfNode o)

makeTopRDF :: DR.Node -> [DR.Triple] -> TopRDF
makeTopRDF i ts = TopRDF i (makeRDF ts)

makeRDF :: [DR.Triple] -> RDF
makeRDF xs = DR.mkRdf xs Nothing prefixMap

rdfAppend :: RDF -> RDF -> RDF
rdfAppend x y = makeRDF (DR.triplesOf x ++ DR.triplesOf y)

adoptAs
  :: (MorlocNodeLike s, MorlocNodeLike p)
  => s -> p -> [TopRDF] -> [DR.Triple]
adoptAs rel sbj objs =
       map (link rel sbj) objs
    ++ concat (map (\(TopRDF _ obj) -> DR.triplesOf obj) objs)
  where
    link rel' sbj' (TopRDF obj' _) = mtriple sbj' rel' obj'

adopt :: DR.Node -> [TopRDF] -> [DR.Triple]
adopt sbj objs =
       zipWith (link sbj) [0..] objs
    ++ concat (map (\(TopRDF _ obj) -> DR.triplesOf obj) objs)
  where
    link :: DR.Node -> Int -> TopRDF -> DR.Triple
    link sbj' index (TopRDF obj' _)
      -- TODO: straighten this out, it is an artefact of my element reversal
      = mtriple sbj' (PElem index) obj'

showTopRDF :: TopRDF -> DT.Text
showTopRDF (TopRDF _ rdf) = DT.pack $ DR.showGraph rdf

-- | Make a UNode from a number, optionall with a prefix. This is used by
-- Morloc.State to create unique ids.
idUri :: Maybe DT.Text -> Int -> DR.Node
idUri Nothing  i = midPre .:. show' i
idUri (Just s) i = midPre .:. (s <> "_" <> show' i)

rdfId :: TopRDF -> DR.Node
rdfId (TopRDF i _) = i


-- The last survivors of the Walker module
-- =======================================

valueOf :: DR.Node -> [DT.Text]
valueOf (DR.LNode (DR.TypedL s _)) = [s]
valueOf (DR.LNode (DR.PlainL s)) = [s]
valueOf _ = []

-- Down :: Subject -> [Object]
down :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Subject    -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Object]   -- chain function, allows searching in parallel
down rdf p' s' = DR.query rdf (Just s') (Just p') Nothing |>> DR.objectOf

up :: DR.Rdf a
  => DR.RDF a
  -> DR.Predicate
  -> DR.Object      -- (Dr.Subject -> [Dr.Object]) is the monadic
  -> [DR.Subject]   -- chain function, allows searching in parallel
up rdf p' o' = DR.query rdf Nothing (Just p') (Just o') |>> DR.subjectOf

getImportedFiles :: DR.Rdf a => DR.RDF a -> [DT.Text]
getImportedFiles rdf
  =   up rdf (rdfPre .:. "type") (mlcPre .:. "import")
  >>= down rdf (mlcPre .:. "name")
  >>= valueOf
