import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Test.Tasty.Golden as Golden
import qualified System.FilePath as SF
import qualified System.IO as SIO
import qualified System.Directory as SD
import qualified Data.Text.IO as DTI
import qualified Data.RDF as DR
import qualified Data.Map.Strict as DMS

import qualified Morloc.Error as ME
import qualified Morloc.Parser as MP

main :: IO ()
main = defaultMain =<< goldenTests

loc2ttl' :: FilePath -> FilePath -> IO ()
loc2ttl' loc ttl = do
  rdf <- DTI.readFile loc >>= MP.parse Nothing >>= doOrDie
  handle <- SIO.openFile ttl SIO.WriteMode
  DR.hWriteRdf (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty)) handle rdf
    <* SIO.hClose handle

doOrDie :: ME.ThrowsError a -> IO a
doOrDie (Right x) = return x
doOrDie (Left err) = fail $ show err ++ "\n"

goldenTests :: IO TestTree
goldenTests = do
  locFiles <- Golden.findByExtension [".loc"] "test-suite/loc2rdf-cases"
  return $ testGroup "morloc to RDF golden tests"
    [ Golden.goldenVsFile
          (SF.takeBaseName locFile) -- test name
          ttlFile -- golden file path
          obsFile
          (loc2ttl' locFile obsFile)
    | locFile <- locFiles
    , let obsFile = SF.replaceExtension locFile ".obs.ttl"
    , let ttlFile = SF.replaceExtension locFile ".ttl"
    ]
