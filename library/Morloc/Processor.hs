module Morloc.Processor (process) where

import Morloc.Error
import Morloc.Data

process :: Program -> ThrowsError Program
process p
  =   return p
  >>= functionsHaveSource -- ensure all functions come from somewhere
  >>= functionsHaveTypes  -- ensure all functions have assigned types
  >>= uniqueDeclarations  -- ensure that there are no reassignments
  >>= typecheckAll        -- typecheck with resolutions

functionsHaveSource :: Program -> ThrowsError Program
functionsHaveSource = undefined

functionsHaveTypes :: Program -> ThrowsError Program
functionsHaveTypes = undefined

uniqueDeclarations :: Program -> ThrowsError Program
uniqueDeclarations = undefined

typecheckAll :: Program -> ThrowsError Program
typecheckAll = undefined

resolve :: MType -> MType -> ThrowsError (MType -> MType)
resolve _ _ = Left (TypeError "Unresolvable mismatch")
