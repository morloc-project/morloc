module Morloc.Generator (
    generate
  , Nexus
  , Pool
) where

import Morloc.Data
import Morloc.Error

-- stub
type Nexus = String
-- stub
type Pool  = String

generate :: Program -> ThrowsError (Nexus, [Pool])
generate = undefined
