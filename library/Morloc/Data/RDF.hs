{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : Morloc.Data.RDF
Description : Convenience functions for working with triples
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.RDF (
    TopRDF(..)
  , RDF
  , mtriple
  , makeRDF
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
  , getImports
  -- ** TopRDF Utilities
  , makeTopRDF
  , idUri
  , rdfId
  , adoptAs
  , adopt
  , showTopRDF
  , rdfAppend
) where

import Morloc.Types
import Morloc.Operators
import qualified Morloc.Error     as ME
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.Doc  as G
import qualified Morloc.Monad     as MM

import qualified Data.RDF         as DR
import qualified Data.Map.Strict  as DMS
import qualified Data.Scientific  as DS
import qualified System.IO        as SIO
import qualified System.Process   as SP
import qualified System.Exit      as SE
import qualified System.Directory as SD

type RDF = DR.RDF DR.TList

data TopRDF = TopRDF DR.Node RDF deriving(Show)

-- | Join an RDF prefix and base creating a URI node
infix 9 .:.
(.:.) :: MT.Text -> MT.Text -> DR.Node
prefix .:. base = DR.UNode (prefix <> base)

mlcPre :: MT.Text
mlcPre = "http://www.morloc.io/ontology/000/"

midPre :: MT.Text
midPre = "http://www.morloc.io/XXX/mid/"

rdfPre :: MT.Text
rdfPre = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

xsdPre :: MT.Text
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

instance MorlocNodeLike MT.Text where
  asRdfNode s = DR.LNode (DR.PlainL s)
  fromRdfNode (DR.LNode (DR.PlainL s)) = s
  fromRdfNode (DR.LNode (DR.PlainLL s _)) = s
  fromRdfNode (DR.LNode (DR.TypedL s _)) = s
  fromRdfNode (DR.UNode s) = s
  fromRdfNode (DR.BNode s) = s
  -- TODO: match to what rdf4h actually generates
  fromRdfNode (DR.BNodeGen i) = MT.pack ("auto_" ++ show i)

instance MorlocNodeLike DS.Scientific where
  asRdfNode x = DR.LNode (DR.TypedL (MT.show' x) (xsdPre <> "decimal"))
  fromRdfNode (DR.LNode (DR.TypedL x t))
    | t == (xsdPre <> "decimal") = MT.read' x
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
  asRdfNode (PElem i)   = rdfPre .:. ("_" <> MT.show' i)
  asRdfNode PType       = rdfPre .:. "type"
  asRdfNode PValue      = rdfPre .:. "value"
  asRdfNode PAlias      = mlcPre .:. "alias"
  asRdfNode PConstraint = mlcPre .:. "constraint"
  asRdfNode PLabel      = mlcPre .:. "label" -- NOT the same as the rdf:label
  asRdfNode PLang       = mlcPre .:. "lang"
  asRdfNode PLeft       = mlcPre .:. "lhs"
  asRdfNode PNamespace  = mlcPre .:. "namespace"
  asRdfNode POutput     = mlcPre .:. "output"
  asRdfNode PPath       = mlcPre .:. "path"
  asRdfNode PProperty   = mlcPre .:. "property"
  asRdfNode PRight      = mlcPre .:. "rhs"
  asRdfNode PKey        = mlcPre .:. "key"
  asRdfNode PNot        = mlcPre .:. "not"
  asRdfNode PName       = mlcPre .:. "name"
  asRdfNode PImport     = mlcPre .:. "import"

  fromRdfNode n
    | n == ( rdfPre .:. "type"       ) = PType
    | n == ( rdfPre .:. "value"      ) = PValue
    | n == ( mlcPre .:. "alias"      ) = PAlias
    | n == ( mlcPre .:. "constraint" ) = PConstraint
    | n == ( mlcPre .:. "label"      ) = PLabel
    | n == ( mlcPre .:. "lang"       ) = PLang
    | n == ( mlcPre .:. "lhs"        ) = PLeft
    | n == ( mlcPre .:. "namespace"  ) = PNamespace
    | n == ( mlcPre .:. "output"     ) = POutput
    | n == ( mlcPre .:. "path"       ) = PPath
    | n == ( mlcPre .:. "property"   ) = PProperty
    | n == ( mlcPre .:. "rhs"        ) = PRight
    | n == ( mlcPre .:. "key"        ) = PKey
    | n == ( mlcPre .:. "not"        ) = PNot
    | n == ( mlcPre .:. "name"       ) = PName
    | n == ( mlcPre .:. "import"     ) = PImport
    | otherwise =
        case
          (MT.stripPrefix (rdfPre <> "_") (fromRdfNode n)) >>= MT.readMay'
        of
          (Just i) -> PElem i
          Nothing -> error ("Unsupported predicate: " ++ show n)


instance MorlocNodeLike GraphObject where
  asRdfNode (OLiteral s)              = (DR.LNode (DR.PlainL s))
  asRdfNode OAccess                   = mlcPre .:. "access"
  asRdfNode OAtomicGenericType        = mlcPre .:. "atomicGeneric"
  asRdfNode OAtomicType               = mlcPre .:. "atomicType"
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
  asRdfNode OBinOp                    = mlcPre .:. "binOp"

  fromRdfNode (DR.LNode (DR.PlainL  s  )) = OLiteral s
  fromRdfNode (DR.LNode (DR.TypedL  s _)) = OLiteral s
  fromRdfNode (DR.LNode (DR.PlainLL s _)) = OLiteral s
  fromRdfNode (DR.BNode x) = error ("illegal RDF node: " ++ show (DR.BNode x))
  fromRdfNode (DR.BNodeGen x) = error ("illegal RDF node: " ++ show (DR.BNodeGen x))
  fromRdfNode n
    | n == ( mlcPre .:. "access"               ) = OAccess
    | n == ( mlcPre .:. "atomicGeneric"        ) = OAtomicGenericType
    | n == ( mlcPre .:. "atomicType"           ) = OAtomicType
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
    | n == ( mlcPre .:. "binOp"                ) = OBinOp
    | otherwise = error ("illegal RDF object: " ++ show n)

instance DocLike DR.Triple where 
  toDoc (DR.Triple s o p) = G.hsep [toDoc s, toDoc o, toDoc p, "."]

instance DocLike [DR.Triple] where
  toDoc xs = G.vsep (map toDoc xs)

instance DocLike DR.Node where
  toDoc (DR.UNode s) = G.angles (G.text' s) 
  toDoc (DR.BNode gId) = "_:" <> G.text' gId
  toDoc (DR.BNodeGen i) = "_:genid" <> G.int i
  toDoc (DR.LNode (DR.PlainL lit)) = G.textEsc' lit
  toDoc (DR.LNode (DR.PlainLL lit lang)) = G.textEsc' lit <> "@" <> G.text' lang
  toDoc (DR.LNode (DR.TypedL lit dtype)) = G.textEsc' lit <> "^^" <> G.angles (G.text' dtype)

-- | Build a triple from Morloc node-like objects
mtriple
  :: ( MorlocNodeLike s, MorlocNodeLike p, MorlocNodeLike o)
  => s -> p -> o -> DR.Triple
mtriple s p o = DR.triple (asRdfNode s) (asRdfNode p) (asRdfNode o)

instance RdfLike RDF where
  writeTurtle p x = do
    handle <- SIO.openFile (MT.unpack p) SIO.WriteMode
    let serializer = DR.TurtleSerializer Nothing prefixMap
    DR.hWriteRdf serializer handle x
      <* SIO.hClose handle

  asTriples = DR.triplesOf

instance SparqlDatabaseLike RDF where
  -- sparqlUpload :: (RdfLike r) => a -> r -> MorlocMonad a
  sparqlUpload x r = return $ makeRDF (asTriples r ++ asTriples x)

  sparqlSelect t q x = do
    -- * find the temporary directory
    tmpdir <- MM.asks configTmpDir
    -- * create it if needed
    MM.liftIO $ SD.createDirectoryIfMissing True (MT.unpack tmpdir)
    -- * write the RDF and query to it, using the given prefix
    let turtlePath = tmpdir <> "/" <> t <> ".ttl"
        sparqlPath = tmpdir <> "/" <> t <> ".rq"
    MM.liftIO $ writeTurtle turtlePath x
    MM.liftIO $ writeSparql sparqlPath q
    -- the system command that queries against a SPARQL database
    let cmd = "arq --data=" <> turtlePath <> " --query=" <> sparqlPath <> " --results=TSV"
    MM.runCommandWith "sparqlSelect" MT.parseTSV cmd

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

showTopRDF :: TopRDF -> MT.Text
showTopRDF (TopRDF _ rdf) = MT.pack $ DR.showGraph rdf

-- | Make a UNode from a number, optionally with a prefix. This is used by
-- Morloc.State to create unique ids.
idUri :: Maybe MT.Text -> Int -> DR.Node
idUri Nothing  i = midPre .:. MT.show' i
idUri (Just s) i = midPre .:. (s <> "_" <> MT.show' i)

rdfId :: TopRDF -> DR.Node
rdfId (TopRDF i _) = i

-- The last survivors of the Walker module
-- =======================================

valueOf :: DR.Node -> [MT.Text]
valueOf (DR.LNode (DR.TypedL s _)) = [s]
valueOf (DR.LNode (DR.PlainL s)) = [s]
valueOf (DR.LNode (DR.PlainLL s _)) = [s]
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

getImports :: DR.Rdf a => DR.RDF a -> [MT.Text]
getImports rdf
  =   up rdf (rdfPre .:. "type") (mlcPre .:. "import")
  >>= down rdf (mlcPre .:. "name")
  >>= valueOf
