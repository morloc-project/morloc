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
        , nexusHelp rdf g
        , DT.unlines (map (generateNexusCall rdf g) exports')
        , MN.nexusEpilogue g
        ]

nexusHelp :: DR.Rdf a => DR.RDF a -> MN.NexusGenerator -> DT.Text
nexusHelp rdf g = (MN.nexusHelp g) prologue' exports' epilogue' where
  prologue' = ["The following commands are exported:"]
  exports' = map (\s -> "    " <> s) (exports rdf)
  epilogue' = []

generateNexusCall :: DR.Rdf a => DR.RDF a -> MN.NexusGenerator -> DT.Text -> DT.Text
generateNexusCall rdf g exp' = case (
      getType rdf exp' >>= elements rdf -- inputs
    , getImportByName rdf exp'
    , getDataDeclarationByName rdf exp'
  ) of
    (inputs', [import'], []) -> (MN.nexusCall g)
      (lang2prog (importLang rdf import'))
      (poolName . MU.maybeOne . importLang rdf $ import')
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

    ([],  _,  _) -> error $ errorMsg "missing type signature"
    (_ , [], []) -> error $ errorMsg "no info"
    (_ ,  _,  _) -> error $ errorMsg "duplicated name or type"
    where
      errorMsg :: String -> String
      errorMsg msg = "Bad RDF for export '" ++ DT.unpack exp' ++ "': " ++ msg

-- FIXME: Obviously need something more sophisticated here. Eventually, I need
-- to load a config file. Dependency handling will be a pain in the future.
lang2prog :: [DT.Text] -> DT.Text
lang2prog ["R"] = "Rscript"
lang2prog _ = "echo" -- TODO: handle error

poolName :: Maybe DT.Text -> DT.Text
poolName (Just lang') = "pool." <> lang'
poolName _ = "pool.WTF" -- TODO: handle error

makeManifoldName :: [DT.Text] -> DT.Text
makeManifoldName [t] = case DT.splitOn ":" t of
  [_, i] -> "m" <> i
  _ -> error "Bad RDF: expected manifold id of form 'mid:<int>'"
makeManifoldName _ = error "Bug: expected `makeManifoldName` input length of 1"

generatePools :: DR.Rdf a => DR.RDF a -> ME.ThrowsError [Pool]
generatePools r = sequence $ map (generatePool r) (getGroupedSources r)

generatePool :: DR.Rdf a => DR.RDF a -> [DR.Node] -> ME.ThrowsError Pool
generatePool rdf ns = Script
  <$> pure "pool"
  <*> getLang rdf ns
  <*> MP.generatePoolCode rdf ns
  where
    getLang :: DR.Rdf a => DR.RDF a -> [DR.Node] -> ME.ThrowsError String
    -- TODO: might want to ensure that all the inputs are indeed of the same language
    -- they *should* be ...
    getLang rdf' n' = case n' >>= lang rdf' >>= valueOf of
      (x:_) -> Right (DT.unpack x)
      []  -> Left $ ME.InvalidRDF "A source must have exactly one language"
