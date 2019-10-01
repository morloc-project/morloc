module Morloc (
    writeProgram
  , typecheck
  , P.cute
  , P.ugly
) where

import Morloc.Namespace
import qualified Morloc.TypeChecker.API as T
import qualified Morloc.Parser.API as P
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Generator as MG
import qualified Morloc.Build as MB

typecheck :: Maybe Path -> MT.Text -> MorlocMonad [T.Module]
typecheck path code = P.parse path code >>= T.typecheck

-- | Build a program as a local executable
writeProgram
  :: Maybe Path -- ^ source code filename (for debugging messages)
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
  >>= MG.generate
  -- (Script, [Script]) -> IO ()
  -- write the code and compile as needed
  >>= MB.buildProgram

-- STUB
connect = return
realize = return
serialize = return
