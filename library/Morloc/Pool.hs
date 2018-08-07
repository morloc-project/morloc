{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Pool
Description : Short description
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

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

generatePoolCode :: DR.Rdf a => DR.RDF a -> [Source] -> ME.ThrowsError DT.Text
generatePoolCode rdf (s:ss) = case (MU.maybeOne $ lang rdf s >>= valueOf) of
  (Just "R") -> generateWith rdf (s:ss) ML.rCodeGenerator
  (Just l) -> Left $ ME.NotSupported ("No support for " ++ DT.unpack l)
  (Nothing) -> Left $ ME.InvalidRDF "No language specified for source"
generatePoolCode _ [] = Left $ ME.InvalidRDF "No source given for the pool"

generateWith
  :: DR.Rdf a
  => DR.RDF a
  -> [Source]
  -> ML.CodeGenerator
  -> ME.ThrowsError DT.Text
generateWith rdf srcs g = Right $
  (ML.makePool g)
    (generateGlobal rdf srcs g)
    (generateSource rdf srcs g)
    ((generateSourceFunctions rdf srcs g) ++ (generateFunctions rdf srcs g))

generateGlobal :: DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateGlobal _ _ _ = []

generateSource :: DR.Rdf a => DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateSource rdf srcs g = srcs >>= path rdf >>= valueOf |>> ML.makeSource g

generateFunctions :: DR.Rdf a => DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateFunctions rdf srcs g = map (generateFunction rdf srcs g) (getCalls rdf)

generateSourceFunctions :: DR.Rdf a => DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateSourceFunctions rdf srcs g
  = concat
  $ map (generateSourceFunction rdf g)
        (srcs >>= sourceExports rdf)

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
  -> [Source] -- needed to decide whether this is an internal or external function
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
  ([], _, _, _) -> error "Invalid RDF: no name for function"
  (_, _, _, []) -> error "Invalid RDF: no id for function"
  _ -> error "Invalid RDF: failed to generate function"
