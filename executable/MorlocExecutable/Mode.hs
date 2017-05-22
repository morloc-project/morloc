module MorlocExecutable.Mode (asCode, asResult) where

import Data.List (intercalate)
import System.IO
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
    writeFile "nexus.sh" nexus

    -- make nexus executable
    setExecutable "nexus.sh"

    mapM_ writePool pools

    -- execute nexus, recording STDOUT to string
    rawSystem "./nexus.sh" []

    -- cleanup
    removeFile "nexus.sh"
    mapM_ (removeFile . fst) pools

setExecutable :: FilePath -> IO ()
setExecutable f = do
  p <- getPermissions f
  setPermissions f (p {executable = True})

writePool :: (String,String) -> IO ()
writePool (name,code) = do
  -- write pools to files
  writeFile name code
  -- make poosl executable
  setExecutable name
