{-# LANGUAGE OverloadedStrings #-}

module Morloc.Pool (generatePoolCode) where

import qualified Data.RDF as DR
import qualified Data.Text as DT

import Morloc.Walker
import qualified Morloc.Error as ME
import qualified Morloc.Language as ML
import qualified Morloc.Util as MU

-- A RDF node which is assumed to be for a source import
type Source = DR.Node 

generatePoolCode :: DR.Rdf a => DR.RDF a -> Source -> ME.ThrowsError DT.Text
generatePoolCode rdf src =
  case
    (
        MU.maybeOne $ lang rdf src >>= value
      , MU.maybeOne $ path rdf src >>= value
    )
  of
  (Just "R", Nothing) -> generateWith rdf src ML.rCodeGenerator
  (_, Just _) -> Left $ ME.NotImplemented "cannot yet read source"
  (Just s, _) -> Left $ ME.NotSupported ("no support for '" ++ DT.unpack s ++ "'")

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
    (generateFunctions rdf src g)

generateGlobal :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateGlobal _ _ _ = ["<stub global>"]

generateSource :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateSource _ _ _ = ["<stub source>"]

generateFunctions :: DR.Rdf a => DR.RDF a -> Source -> ML.CodeGenerator -> [DT.Text]
generateFunctions _ _ _ = ["<stub functions>"]
