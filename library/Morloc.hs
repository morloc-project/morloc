module Morloc
  ( writeProgram
  , typecheck
  , P.cute
  , P.ugly
  ) where

import Morloc.Build (buildProgram)
import Morloc.Connect (connect)
import qualified Morloc.Data.Text as MT
import Morloc.Generate (generate)
import Morloc.Namespace
import qualified Morloc.Parser.API as P
import Morloc.Realize (realize)
import Morloc.Serialize (serialize)
import qualified Morloc.TypeChecker.API as T

typecheck :: Maybe Path -> MT.Text -> MorlocMonad [T.Module]
typecheck path code = P.parse path code >>= T.typecheck

-- | Build a program as a local executable
writeProgram ::
     Maybe Path -- ^ source code filename (for debugging messages)
  -> MT.Text    -- ^ source code text
  -> MorlocMonad ()
writeProgram path code
  -- Maybe Path -> MT.Text -> [Module]
  -- parse code into unannotated modules
  =   P.parse path code
  -- [Module] -> [Module]
  -- add type annotations to sub-expressions and raise type errors
  >>= T.typecheck
  -- [Module] -> [Manifold]
  -- dissassemble modules into a graph of linked manifolds
  -- associate each realization with serialization functions
  >>= connect
  -- [Manifold] -> [Manifold]
  -- Connect serialization functions as needed
  >>= serialize
  -- [Manifold] -> [Manifold]
  -- select a single realization for each manifold
  >>= realize
  -- [Manifold] -> (Script, [Script])
  -- translate modules into manifolds and manifolds into scripts of source code
  >>= generate
  -- (Script, [Script]) -> IO ()
  -- write the code and compile as needed
  >>= buildProgram
