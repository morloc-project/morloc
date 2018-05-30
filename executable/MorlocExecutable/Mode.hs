module MorlocExecutable.Mode (asCode, asResult) where

import System.Directory
import System.Process

import Morloc.Graph
import Morloc.Data
import Morloc.Generator (generate)

asCode :: Graph MData -> IO ()
asCode g = case generate g of
  (nexus, pools) -> putStr $ nexusCode ++ poolCode where
    nexusCode = unlines ["NEXUS", indent nexus]
    poolCode = concatMap writePool pools

    indent :: String -> String
    indent = unlines . map (\s -> "  " ++ s) . lines

    writePool :: Show a => (a, String) -> String
    writePool (l,c) = unlines [show l, indent c]


asResult :: Graph MData -> IO ()
asResult g = case generate g of
  (nexus, pools) -> do

    -- write nexus to a file
    writeExeFile ("nexus.sh", nexus)

    mapM_ writeExeFile pools

    -- execute nexus, recording STDOUT to string
    _ <- rawSystem "bash" ["nexus.sh"]

    -- cleanup
    removeFile "nexus.sh"
    mapM_ (removeFile . fst) pools

setExecutable :: FilePath -> IO ()
setExecutable f = do
  p <- getPermissions f
  setPermissions f (p {executable = True})

writeExeFile :: (String,String) -> IO ()
writeExeFile (name,code) = do
  -- write pools to files
  writeFile name code
  -- make poosl executable
  setExecutable name
