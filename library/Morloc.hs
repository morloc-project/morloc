module Morloc
  ( writeProgram
  , typecheck
  , P.cute
  , P.ugly
  ) where

import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM

import qualified Morloc.Parser.API as P
import qualified Morloc.TypeChecker.API as T
import Morloc.CodeGenerator.Generate (generate)
import Morloc.ProgramBuilder.Build (buildProgram)

typecheck :: Maybe Path -> Code -> MorlocMonad [T.Module]
typecheck path code = P.parse path code >>= T.typecheck

-- | Build a program as a local executable
writeProgram ::
     Maybe Path -- ^ source code filename (for debugging messages)
  -> Code       -- ^ source code text
  -> MorlocMonad ()
writeProgram path code
  -- Maybe Path -> MT.Text -> [Module]
  -- parse code into unannotated modules
  =   P.parse path code
  -- [Module] -> [Module]
  -- add type annotations to sub-expressions and raise type errors
  >>= T.typecheck
  -- [Module] -> (Script, [Script])
  -- translate mtree into nexus and pool source code
  >>= generate
  -- (Script, [Script]) -> IO ()
  -- write the code and compile as needed
  >>= buildProgram
