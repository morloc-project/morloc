import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.Golden as Golden
import qualified System.FilePath as SF
import qualified Data.Text.IO as DTIO

import Morloc.Operators
import Morloc (writeTurtleTo)
import qualified Morloc.Config as MC
import qualified Morloc.Monad as MM

main :: IO ()
main = defaultMain =<< goldenTests

-- add a dot to a base filename
hideFile :: FilePath -> FilePath
hideFile f = SF.replaceBaseName f ("." ++ SF.takeBaseName f)

writeTurtleTo' :: MC.Config -> FilePath -> FilePath -> IO ()
writeTurtleTo' config loc ttl = do
  locFile <- DTIO.readFile loc
  MM.runMorlocMonad config Nothing (writeTurtleTo Nothing locFile ttl) >>=
    MM.writeMorlocReturn

goldenTests :: IO TestTree
goldenTests = do
  config <- MC.loadDefaultMorlocConfig
  locFiles <- Golden.findByExtension [".loc"] ("test-suite" </> "loc2rdf-cases")
  return $ testGroup "morloc to RDF golden tests"
    [ Golden.goldenVsFile
          (SF.takeBaseName locFile) -- test name
          ttlFile -- golden file path
          obsFile
          (writeTurtleTo' config locFile obsFile)
    | locFile <- locFiles
    , let obsFile = SF.replaceExtension (hideFile locFile) ".obs.ttl"
    , let ttlFile = SF.replaceExtension locFile ".ttl"
    ]
