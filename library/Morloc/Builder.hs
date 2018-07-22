module Morloc.Builder
(
  build
) where

import Morloc.Generator
import Morloc.Error

-- builds the Morloc program and returns the name of the executable
build :: (Nexus, [Pool]) -> ThrowsError (IO String)
build = undefined
