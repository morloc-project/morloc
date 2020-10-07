module Morloc
  ( writeProgram
  , typecheck
  ) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

import qualified Morloc.Parser.API as P
import qualified Morloc.TypeChecker.API as T
import Morloc.Parser.Desugar (desugar) 
import Morloc.CodeGenerator.Generate (generate)
import Morloc.ProgramBuilder.Build (buildProgram)
import Morloc.TypeChecker.Treeify (treeify)

typecheck :: Maybe Path -> Code -> MorlocMonad TypedDag
typecheck path code
  -- Maybe Path -> MT.Text -> [Module]
  -- parse code into unannotated modules
  = P.parse path code
  -- [Module] -> [Module]
  -- resolve type aliases and such
  >>= desugar
  -- [Module] -> [Module]
  -- add type annotations to sub-expressions and raise type errors
  >>= T.typecheck

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
