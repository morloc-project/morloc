module Morloc.Check (typecheck) where

import Morloc.Error
import Morloc.Data

typecheck :: Program -> ThrowsError Program
typecheck p = Right $ p
