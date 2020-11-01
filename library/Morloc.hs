module Morloc
  ( writeProgram
  , typecheck
  ) where

import Morloc.Namespace
import Morloc.Frontend.Namespace (TypedDag)

import qualified Morloc.Frontend.API as F
import Morloc.Frontend.Desugar (desugar) 
import Morloc.CodeGenerator.Generate (generate)
import Morloc.ProgramBuilder.Build (buildProgram)
import Morloc.Frontend.Treeify (treeify)

typecheck :: Maybe Path -> Code -> MorlocMonad TypedDag
typecheck path code
  -- Maybe Path -> Text -> [Module]
  -- parse code into unannotated modules
  = F.parse path code
  -- [Module] -> [Module]
  -- resolve type aliases and such
  >>= desugar
  -- [Module] -> [Module]
  -- add type annotations to sub-expressions and raise type errors
  >>= F.typecheck

-- | Build a program as a local executable
writeProgram ::
     Maybe Path -- ^ source code filename (for debugging messages)
  -> Code       -- ^ source code text
  -> MorlocMonad ()
writeProgram path code
  = typecheck path code
  -- [Module] -> SAnno GMeta Many [CType]
  >>= treeify
  -- [SAnno GMeta Many [CType]] -> (Script, [Script])
  -- translate mtree into nexus and pool source code
  >>= generate
  -- (Script, [Script]) -> IO ()
  -- write the code and compile as needed
  >>= buildProgram
