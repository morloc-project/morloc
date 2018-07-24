module Morloc.Builder
(
  build
) where

import qualified Morloc.Generator as MG
import qualified Morloc.Error as ME
import qualified Morloc.Data as MD

writeScript :: MD.Script -> IO ()
writeScript (MD.Script base lang code) =
  writeFile (base ++ "." ++ lang) code

-- builds the Morloc program and returns the name of the executable
build :: (MG.Nexus, [MG.Pool]) -> IO ()
build (nexus, pools) = do
  writeScript nexus
  mapM_ writeScript pools 
