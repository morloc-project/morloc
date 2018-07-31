{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Generator
Description : Generate code from the RDF representation of a Morloc script 
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

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
import qualified Morloc.Pool as MP
import qualified Morloc.Util as MU

import qualified Data.RDF as DR
import qualified Data.Text as DT

data Script = Script {
      scriptBase :: String -- ^ script basename (no extension)
    , scriptLang :: String -- ^ script language
    , scriptCode :: DT.Text -- ^ full script source code
  }
  deriving(Ord, Eq)

type Nexus = Script
type Pool = Script

generate :: DR.Rdf a => DR.RDF a -> ME.ThrowsError (Nexus, [Pool]) 
generate r = (,) <$> generateNexus r <*> generatePools r

generateNexus :: DR.Rdf a => DR.RDF a -> ME.ThrowsError Nexus
generateNexus rdf = pure $ Script {
      scriptBase = "nexus"
    , scriptLang = lang'
    , scriptCode = nexusCode'
  }
  where
    -- TODO allow user to choose a generator
    -- Eventually these will include, for example, a CWL generator
    g = MN.perlCliNexusGenerator
    lang' = "perl"
    nexusCode' = case exports rdf of
      exports' -> DT.unlines
        [ (MN.nexusPrologue g)
        , (MN.nexusPrint g) ""
        , (MN.nexusDispatch g) exports'
        , (MN.nexusHelp g) []
        , DT.unlines (map (generateNexusCall rdf g) exports')
        , MN.nexusEpilogue g
        ]

generateNexusCall :: DR.Rdf a => DR.RDF a -> MN.NexusGenerator -> DT.Text -> DT.Text
generateNexusCall rdf g exp' = case (
      getType rdf exp' >>= elements rdf -- inputs
    , getImportByName rdf exp'
    , getDataDeclarationByName rdf exp'
  ) of
    (inputs', [import'], []) -> (MN.nexusCall g)
      (lang2prog (importLang rdf import'))
      (poolName
        (MU.maybeOne $ importLang rdf import')
        (MU.maybeOne $ importPath rdf import'))
      exp'
      (makeManifoldName (idOf import'))
      (length inputs')

    (inputs', [], [decl]) -> (MN.nexusCall g)
      -- FIXME: I need to find the first sourced call and use the source info
      -- from that to choose a command and pool name.
      "Rscript"
      "pool.R"
      exp'
      (makeManifoldName (return decl >>= rhs rdf >>= idOf))
      (length inputs')

    _ -> "XXX"

-- FIXME: Obviously need something more sophisticated here. Eventually, I need
-- to load a config file. Dependency handling will be a pain in the future.
lang2prog :: [DT.Text] -> DT.Text
lang2prog _ = "Rscript"

poolName :: Maybe DT.Text -> Maybe DT.Text -> DT.Text
poolName (Just lang') Nothing = "pool." <> lang'
poolName _ (Just path') = path'
poolName _ _ = "pool.WTF"

makeManifoldName :: [DT.Text] -> DT.Text
makeManifoldName [t] = case DT.splitOn ":" t of
  [_, i] -> "m" <> i
  _ -> "XXX"
makeManifoldName _ = "XXX"

generatePools :: DR.Rdf a => DR.RDF a -> ME.ThrowsError [Pool]
generatePools r = sequence $ map (generatePool r) (getSources r)

generatePool :: DR.Rdf a => DR.RDF a -> DR.Node -> ME.ThrowsError Pool
generatePool rdf n = Script
  <$> pure "pool"
  <*> getLang rdf n
  <*> MP.generatePoolCode rdf n
  where
    getLang :: DR.Rdf a => DR.RDF a -> DR.Node -> ME.ThrowsError String
    getLang rdf' n' = case lang rdf' n' >>= valueOf of
      [x] -> Right (DT.unpack x)
      _   -> Left $ ME.InvalidRDF "A source must have exactly one language"
