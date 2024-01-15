module Morloc
  ( writeProgram
  , typecheck
  , typecheckFrontend
  ) where

import Morloc.Namespace

import qualified Morloc.Frontend.API as F
import Morloc.Frontend.Restructure (restructure)
import Morloc.CodeGenerator.Generate (generate, generatePools, realityCheck)
import Morloc.CodeGenerator.Namespace (SerialManifold)
import Morloc.ProgramBuilder.Build (buildProgram)
import Morloc.Frontend.Treeify (treeify)

typecheckFrontend
  :: Maybe Path
  -> Code
  -> MorlocMonad [SAnno (Indexed TypeU) Many Int]
typecheckFrontend path code
  -- Maybe Path -> Text -> [Module]
  -- parse code into unannotated modules
  = F.parse path code
  -- resolve type aliases and such
  >>= restructure
  -- convert to Sanno
  >>= treeify
  -- add type annotations to sub-expressions and raise type errors
  >>= F.typecheck

typecheck
  :: Maybe Path
  -> Code
  -> MorlocMonad [(Lang, [SerialManifold])]
typecheck path code
  = typecheckFrontend path code
  -- resolve all TypeU types to Type
  |>> map F.resolveTypes
  -- resolve all TypeU types to Type
  >>= realityCheck
  -- generate the language specific pools
  >>= (generatePools . snd)

-- | Build a program as a local executable
writeProgram ::
     Maybe Path -- ^ source code filename (for debugging messages)
  -> Code       -- ^ source code text
  -> MorlocMonad ()
writeProgram path code
  = typecheckFrontend path code
  -- resolve all TypeU types to Type
  |>> map F.resolveTypes
  -- resolve all TypeU types to Type
  >>= realityCheck
  -- prepare scripts
  >>= uncurry generate
  -- (Script, [Script]) -> IO ()
  -- write the code and compile as needed
  >>= buildProgram
