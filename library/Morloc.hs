module Morloc (
    writeTurtle
  , writeTriple
) where

import qualified Data.RDF as DR
import qualified Data.Map.Strict as DMS
import qualified Morloc.Error as ME
import qualified Morloc.Triple as M3
import qualified Morloc.Parser as MP
import qualified Morloc.Data as MD

write' :: DR.RdfSerializer s => s -> String -> IO ()
write' serializer code = case MP.morlocScript code of
  Left err -> putStr $ show err ++ "\n"
  Right rdfOutput -> DR.writeRdf serializer rdfOutput

writeTurtle :: String -> IO ()
writeTurtle = write' (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

writeTriple :: String -> IO ()
writeTriple = write' DR.NTriplesSerializer
