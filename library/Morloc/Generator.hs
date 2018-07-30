{-# LANGUAGE OverloadedStrings #-}

module Morloc.Generator
(
    Script(..)
  , Nexus
  , Pool
  , generate  
) where

import Morloc.Walker
import Morloc.Operators
import qualified Morloc.Error as ME
import qualified Morloc.Nexus as MN

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Maybe as DM

data Script = Script {
      scriptBase :: String -- script basename (no extension)
    , scriptLang :: String -- script language
    , scriptCode :: DT.Text -- full script source code
  }
  deriving(Ord, Eq)

type Nexus = Script
type Pool = Script

generate :: DR.Rdf a => DR.RDF a -> ME.ThrowsError (Nexus, [Pool]) 
generate r = (,) <$> generateNexus r <*> generatePools r

generateNexus :: DR.Rdf a => DR.RDF a -> ME.ThrowsError Nexus
generateNexus rdf = pure $ Script {
      scriptBase = "nexus"
    , scriptLang = lang
    , scriptCode = nexusCode'
  }
  where
    -- TODO allow user to choose a generator
    -- Eventually these will include, for example, a CWL generator
    g = MN.perlCliNexusGenerator
    lang = "perl"

    nexusCode' = DT.unlines
      [ (MN.nexusPrologue g)
      , (MN.nexusPrint g) ""
      , (MN.nexusDispatch g) (getDataDeclarations rdf >>= lhs rdf >>= value)
      , (MN.nexusHelp g) []
      ]
      <> DT.unlines (map ((MN.nexusCall g) "Rscript" "pool.R") [])
      <> MN.nexusEpilogue g

generatePools :: DR.Rdf a => DR.RDF a -> ME.ThrowsError [Pool]
generatePools r = sequence $ map (generatePool r) (getSources r)

generatePool :: DR.Rdf a => DR.RDF a -> DR.Node -> ME.ThrowsError Pool
generatePool rdf n = pure $ Script {
      scriptBase = "pool"
    , scriptLang = "French"
    , scriptCode = "yolo"
  }
