import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.Golden as Golden
import qualified System.FilePath as SF
import qualified Data.Text.IO as DTIO

import Morloc.Operators
import Morloc (writeTurtleTo)

main :: IO ()
main = defaultMain =<< goldenTests

-- add a dot to a base filename
hideFile :: FilePath -> FilePath
hideFile f = SF.replaceBaseName f ("." ++ SF.takeBaseName f)

writeTurtleTo' :: FilePath -> FilePath -> IO ()
writeTurtleTo' loc ttl = do
  locFile <- DTIO.readFile loc
  writeTurtleTo locFile ttl

goldenTests :: IO TestTree
goldenTests = do
  locFiles <- Golden.findByExtension [".loc"] ("test-suite" </> "loc2rdf-cases")
  return $ testGroup "morloc to RDF golden tests"
    [ Golden.goldenVsFile
          (SF.takeBaseName locFile) -- test name
          ttlFile -- golden file path
          obsFile
          (writeTurtleTo' locFile obsFile)
    | locFile <- locFiles
    , let obsFile = SF.replaceExtension (hideFile locFile) ".obs.ttl"
    , let ttlFile = SF.replaceExtension locFile ".ttl"
    ]
