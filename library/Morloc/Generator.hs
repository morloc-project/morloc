module Morloc.Generator (
    generate
  , Nexus
  , Pool
) where

import qualified Morloc.Data as MD
import qualified Morloc.Error as ME
import qualified Morloc.Language as ML
import qualified Morloc.Nexus as MN

type Nexus = MD.Script
type Pool = MD.Script

generate :: MD.Program -> ME.ThrowsError (Nexus, [Pool])
generate p = (,) <$> generateNexus p <*> generatePools p

generateNexus :: MD.Program -> ME.ThrowsError Nexus
generateNexus p = pure $ MD.Script {
      MD.scriptBase = "nexus"
    , MD.scriptLang = lang
    , MD.scriptCode = nexusCode' p
  }
  where
    -- TODO allow user to choose a generator
    -- Eventually these will include, for example, a CWL generator
    g = MN.perlCliNexusGenerator
    lang = "perl"

    nexusCode' p = unlines
      [ (MN.nexusPrologue g)
      , (MN.nexusPrint g) ""
      , (MN.nexusDispatch g) [n | (MD.DataDecl n _ _) <- MD.programData p]
      , (MN.nexusHelp g) []
      ]
      ++ unlines (map ((MN.nexusCall g) "Rscript" "pool.R") [])
      ++ MN.nexusEpilogue g

generatePools :: MD.Program -> ME.ThrowsError [Pool]
generatePools p = sequence $ map (generatePool p) (MD.programSources p)

generatePool :: MD.Program -> MD.Source -> ME.ThrowsError Pool
generatePool p src
  =   MD.Script
  <$> pure "pool"
  <*> pure "R"
  <*> generatePoolCode p src

-- complete code for the pool
generatePoolCode :: MD.Program -> MD.Source -> ME.ThrowsError String
generatePoolCode p (MD.Source "R" Nothing funs)
  = Right $
    (ML.makePool g)
    (generateGlobal g)
    (generateSource g src)
    (generateFunctions g src p)
  where
    g   = ML.rCodeGenerator
    src = MD.Source "R" Nothing funs
generatePoolCode _  (MD.Source _ (Just _) _ )
  = Left $ ME.NotImplemented "cannot yet read source"
generatePoolCode _  (MD.Source lang _ _)
  = Left $ ME.NotSupported ("ERROR: the language '" ++ lang ++ "' is not yet supported")

generateGlobal :: ML.CodeGenerator -> [String]
generateGlobal _ = ["<stub global>"]

generateSource :: ML.CodeGenerator -> MD.Source -> [String]
generateSource _ _ = ["<stub source>"]

generateFunctions :: ML.CodeGenerator -> MD.Source -> MD.Program -> [String]
generateFunctions _ _ _ = ["<stub functions>"]
