module MorlocExecutable.Mode
(
    asLIL
  , asCode
) where

import Data.List (intercalate)
import Morloc.Graph
import Morloc.NodeAttribute
import Morloc.Generator (generate)


asLIL :: Graph NodeAttr -> IO ()
asLIL = putStr . unlines . concat . toList . parentChildMapI link where
  link :: NodeAttr -> (Int, NodeAttr) -> String
  link parent (position, child) = 
    intercalate "\t"
    [ 
        showNodeValue parent
      , showNodeID    parent
      , show          position
      , showNodeType  child
      , showNodeValue child
    ]

asCode :: Graph NodeAttr -> IO ()
asCode g = case generate g of
  (nexus, pools) -> putStr $ nexusCode ++ poolCode where
    nexusCode = unlines ["NEXUS", indent nexus]
    poolCode = concatMap writePool pools

    indent :: String -> String
    indent = unlines . map (\s -> "  " ++ s) . lines

    writePool :: Show a => (a, String) -> String
    writePool (l,c) = unlines [show l, indent c]


{- asResult :: Graph NodeAttr -> IO GHC.IO.Exception.ExitCode -}
{- asResult g = case generate g of                            -}
{-   (nexus, pools) -> do                                     -}
{-     -- write nexus to a file                               -}
{-     -- make nexus executable                               -}
{-     -- write pools to file                                 -}
{-     -- make pools executable                               -}
{-     -- execute nexus, recording STDOUT to string           -}
{-     System.Process.rawSystem "nexus.sh" []                 -}
