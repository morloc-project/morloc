module Morloc.Builder
(
  build
) where

import Morloc.Generator
import Morloc.Error
import Morloc.Data

writeScript :: Script -> IO ()
writeScript (Script base lang code) =
  writeFile (base ++ "." ++ lang) code

-- builds the Morloc program and returns the name of the executable
build :: (Nexus, [Pool]) -> IO ()
build (nexus, pools) = do
  writeScript nexus
  mapM_ writeScript pools 
