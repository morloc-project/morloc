{-# LANGUAGE OverloadedStrings #-}

module Morloc.Pool (generatePoolCode) where

import qualified Data.RDF as DR
import qualified Data.Text as DT

import Morloc.Walker
import Morloc.Operators
import qualified Morloc.Error as ME
import qualified Morloc.Language as ML
import qualified Morloc.Util as MU

-- RDF node representing a source import
type Source = DR.Node 
-- RDF node representing a function call
type Call = DR.Node
-- RDF node representing a an imported function
type Import = DR.Node

generatePoolCode :: DR.Rdf a => DR.RDF a -> Source -> ME.ThrowsError DT.Text
generatePoolCode rdf src =
  case
    (
        MU.maybeOne $ lang rdf src >>= valueOf
      , MU.maybeOne $ path rdf src >>= valueOf
    )
  of
  (Just "R", Nothing) -> generateWith rdf src ML.rCodeGenerator
  (_, Just _) -> Left $ ME.NotImplemented "cannot yet read source"
  (Just s, _) -> Left $ ME.NotSupported ("no support for '" ++ DT.unpack s ++ "'")
  (Nothing, _) -> Left $ ME.InvalidRDF "No language specified for source"

generateWith
  :: DR.Rdf a
  => DR.RDF a
  -> Source
  -> ML.CodeGenerator
  -> ME.ThrowsError DT.Text
generateWith rdf src g = Right $
  (ML.makePool g)
    (generateGlobal rdf src g)
    (generateSource rdf src g)
    ((generateSourceFunctions rdf src g) ++ (generateFunctions rdf src g))

generateGlobal :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateGlobal _ _ _ = []

generateSource :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateSource rdf src g = path rdf src >>= valueOf |>> ML.makeSource g

generateFunctions :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateFunctions rdf src g = map (generateFunction rdf src g) (getCalls rdf)

generateSourceFunctions :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateSourceFunctions rdf src g = concat $ map (generateSourceFunction rdf g) (sourceExports rdf src) 
generateSourceFunction
  :: DR.Rdf a
  => DR.RDF a
  -> ML.CodeGenerator
  -> Import
  -> [DT.Text]
generateSourceFunction rdf g imp = case (importAlias rdf imp, importName rdf imp) of
  ([alias'], [name']) -> case (
        idOf imp
      , asPositional (getType rdf alias' >>= elements rdf)
    ) of
      ([mid'], args') ->
        [(ML.makeFunction g)
           ((ML.makeManifoldName g) mid')
           args'
           ((ML.makeCall g) name' args')
        ]
      _ -> []
  _ -> []
  where
    asPositional :: [a] -> [DT.Text]
    asPositional xs = map (\(_,i) -> "x" <> (DT.pack $ show i)) (zip xs [1..])

generateFunction :: DR.Rdf a
  => DR.RDF a
  -> Source -- needed to decide whether this is an internal or external function
  -> ML.CodeGenerator
  -> Call 
  -> DT.Text 
-- TODO: use src to separate internal/external functions
generateFunction rdf _ g n =
  case
    (
        value rdf n >>= rdftype rdf >>= valueOf
        -- FIXME: this only works for primitives
      , elements rdf n >>= rdftype rdf >>= valueOf
      , getScope rdf n >>= elements rdf >>= rdftype rdf >>= valueOf 
      , idOf n
    )
  of
  ([name'], args', bndvars', [mid']) ->
    (ML.makeFunction g)
      ((ML.makeManifoldName g) mid')
      bndvars'
      ((ML.makeCall g) name' args')
  _ -> "XXX"
