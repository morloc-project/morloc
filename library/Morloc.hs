module Morloc (
    writeTurtle
  , writeTriple
  , writeProgram
) where

import qualified Data.RDF as DR
import qualified Data.Map.Strict as DMS
import qualified Data.Text.IO as DTIO

import Morloc.Operators
import qualified Morloc.Error as ME
import qualified Morloc.Parser as MP
import qualified Morloc.Generator as MG

writeProgram :: String -> IO ()
writeProgram s = writeProgram' (MP.morlocScript s >>= MG.generate) where
  writeProgram' :: ME.ThrowsError (MG.Script, [MG.Script]) -> IO ()
  writeProgram' (Right (n, ps)) = do
    writeScript' n
    mapM_ writeScript' ps
  writeProgram' (Left err) = putStr (show err)
    
  writeScript' :: MG.Script -> IO ()
  writeScript' (MG.Script base lang code) =
    DTIO.writeFile (base <> "." <> lang) code

writeRDF' :: DR.RdfSerializer s => s -> String -> IO ()
writeRDF' serializer code = case MP.morlocScript code of
  Left err -> putStr $ show err ++ "\n"
  Right rdfOutput -> DR.writeRdf serializer rdfOutput

writeTurtle :: String -> IO ()
writeTurtle = writeRDF' (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

writeTriple :: String -> IO ()
writeTriple = writeRDF' DR.NTriplesSerializer
