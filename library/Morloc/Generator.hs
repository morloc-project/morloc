module Morloc.Generator (
    generate
  , Nexus
  , Pool
) where

import Morloc.Data
import Morloc.Error
import Morloc.Language
import Morloc.Nexus

type Nexus = Script
type Pool = Script

generate :: Program -> ThrowsError (Nexus, [Pool])
generate p = (,) <$> generateNexus p <*> generatePools p

generateNexus :: Program -> ThrowsError Nexus
generateNexus p = pure $ Script {
      scriptBase = "nexus"
    , scriptLang = lang
    , scriptCode = nexusCode' p
  }
  where
    -- TODO allow user to choose a generator
    -- Eventually these will include, for example, a CWL generator
    g = perlCliNexusGenerator
    lang = "perl"

    nexusCode' p = unlines
      [ (nexusPrologue g)
      , (nexusPrint g) ""
      , (nexusDispatch g) [n | (DataDecl n _ _) <- programData p]
      , (nexusHelp g) []
      ]
      ++ unlines (map ((nexusCall g) "Rscript" "pool.R") [])
      ++ nexusEpilogue g

generatePools :: Program -> ThrowsError [Pool]
generatePools p = sequence $ map (generatePool p) (programSources p)

generatePool :: Program -> Source -> ThrowsError Pool
generatePool p src
  =   Script
  <$> pure "pool"
  <*> pure "R"
  <*> generatePoolCode p src

-- complete code for the pool
generatePoolCode :: Program -> Source -> ThrowsError String
generatePoolCode p (Source "R" Nothing funs)
  = Right $
    (makePool g)
    (generateGlobal g)
    (generateSource g src)
    (generateFunctions g src p)
  where
    g   = rCodeGenerator
    src = Source "R" Nothing funs
generatePoolCode _  (Source _ (Just _) _ )
  = Left $ NotImplemented "cannot yet read source"
generatePoolCode _  (Source lang _ _)
  = Left $ NotSupported ("ERROR: the language '" ++ lang ++ "' is not yet supported")

generateGlobal :: CodeGenerator -> [String]
generateGlobal _ = ["<stub global>"]

generateSource :: CodeGenerator -> Source -> [String]
generateSource _ _ = ["<stub source>"]

generateFunctions :: CodeGenerator -> Source -> Program -> [String]
generateFunctions _ _ _ = ["<stub functions>"]
